rasterPCA <- function(img, nSamples = NULL, nComp = nlayers(img), spca = FALSE,  maskCheck = TRUE, ...){      
  
  if(nlayers(img) <= 1) stop("Need at least two layers to calculate PCA.")   
  ellip <- list(...)
  
  ## Deprecate norm, as it has the same effect as spca
  if("norm" %in% names(ellip)) {
    warning("Argument 'norm' has been deprecated. Use argument 'spca' instead.\nFormer 'norm=TRUE' corresponds to 'spca=TRUE'.", call. = FALSE)
    ellip[["norm"]] <- NULL
  }
  
  if(nComp > nlayers(img)) nComp <- nlayers(img)
  
  if(!is.null(nSamples)){    
    trainData <- sampleRandom(img, size = nSamples, na.rm = TRUE)
    if(nrow(trainData) < nlayers(img)) stop("nSamples too small or img contains a layer with NAs only")
    model <- princomp(trainData, scores = FALSE, cor = spca)
  } else {
    if(maskCheck) {
      totalMask <- !sum(calc(img, is.na))
      if(cellStats(totalMask, sum) == 0) stop("img contains either a layer with NAs only or no single pixel with valid values across all layers")
      img <- mask(img, totalMask , maskvalue = 0) ## NA areas must be masked from all layers, otherwise the covariance matrix is not non-negative definite   
    }
    covMat <- layerStats(img, stat = "cov", na.rm = TRUE)
    model  <- princomp(covmat = covMat[[1]], cor=spca)
    model$center <- covMat$mean
    model$n.obs  <- ncell(img)
    if(spca) {    
      ## Calculate scale as population sd like in in princomp
      S <- diag(covMat$covariance)
      model$scale <- sqrt(S * (model$n.obs-1)/model$n.obs)
    }
  }
  ## Predict
  out   <- .paraRasterFun(img, rasterFun=raster::predict, args = list(model = model, na.rm = TRUE, index = 1:nComp), wrArgs = ellip)  
  names(out) <- paste0("PC", 1:nComp)
  structure(list(call = match.call(), model = model, map = out), class = c("rasterPCA", "RStoolbox"))  
  
}

.paraRasterFun <- function(raster, rasterFun, args = list(), wrArgs = list()){
  if (isTRUE( getOption('rasterCluster'))) {
    do.call("clusterR", args = c(list(x = raster, fun = rasterFun, args=args), wrArgs))
  } else {
    do.call("rasterFun", args=c(raster, args, wrArgs))
  }
}

ggRGB <- function(img, r = 3, g = 2, b = 1, scale, maxpixels = 500000, stretch = "none", ext = NULL,  limits = NULL,
                  clipValues  = "limits", quantiles = c(0.02,0.98), ggObj = TRUE, ggLayer = FALSE, 
                  alpha = 1, coord_equal = TRUE, geom_raster = FALSE, nullValue = 0) { 
  
  ## TODO: handle single value rasters (e.g. masks)
  
  # RGB processing originally forked from raster::plotRGB (Author: Robert J. Hijmans) GPL3 
  verbose <- getOption("RStoolbox.verbose")
  annotation <- !geom_raster
  ## Subsample raster        
  rgb <- unlist(.numBand(raster=img,r,g,b))
  nComps <- length(rgb)
  if(inherits(img, "RasterLayer")) img <- brick(img)
  rr     <- sampleRegular(img[[rgb]], maxpixels, ext=ext, asRaster=TRUE)
  RGB    <- getValues(rr)
  if(!is.matrix(RGB)) RGB <- as.matrix(RGB)
  
  ## Clip to limits
  if (!is.null(limits)) {
    ## Tidy limits
    if (!is.matrix(limits)) {
      limits <- matrix(limits, ncol = 2, nrow = nComps, byrow = TRUE)        
    }         
    ## Tidy clip values
    if(!is.matrix(clipValues)){
      if(!anyNA(clipValues) && clipValues[1] == "limits") {
        clipValues <- limits
      } else {
        clipValues <- matrix(clipValues, ncol = 2, nrow = nComps, byrow = TRUE)                            
      } 
    }
    ## Do clipping
    for (i in 1:nComps) {    
      if(verbose){
        message("Number of pixels clipped in ", c("red", "green", "blue")[i], " band:\n",
                "below limit: ", sum(RGB[,i] < limits[i,1], na.rm = TRUE), " | above limit: ", sum(RGB[,i] > limits[i,2], na.rm = TRUE))
      }
      RGB[RGB[,i] < limits[i,1], i] <- clipValues[i,1]
      RGB[RGB[,i] > limits[i,2], i] <- clipValues[i,2]            
    }
  }   
  rangeRGB <- range(RGB, na.rm = TRUE)
  
  if(missing('scale')){ scale <- rangeRGB[2] }
  
  if(rangeRGB[1] < 0){
    RGB      <- RGB - rangeRGB[1]
    scale    <- scale - rangeRGB[1] 
    rangeRGB <- rangeRGB - rangeRGB[1]
  }   
  
  if(scale < rangeRGB[2]) {
    warning("Scale < max value. Resetting scale to max.", call.=FALSE)
    scale <- rangeRGB[2]
  }
  RGB <- na.omit(RGB)
  
  
  ## Perform data stretch
  if (stretch != "none") {
    stretch <- tolower(stretch)
    for(i in seq_along(rgb)){
      RGB[,i] <- .stretch(RGB[,i], method = stretch, quantiles=quantiles, band = i)
    }
    scale <- 1        
  }
  
  ## Assemble colors
  naind <- as.vector( attr(RGB, "na.action") ) 
  nullbands <- sapply(list(r,g,b), is.null)       
  
  
  if(any(nullbands)) {
    RGBm <- matrix(nullValue, ncol = 3, nrow = NROW(RGB))
    RGBm[,!nullbands] <- RGB
    RGB <- RGBm      
  }
  
  
  if (!is.null(naind)) {
    z <- rep( NA, times=ncell(rr))
    z[-naind] <- rgb(RGB[,1], RGB[,2], RGB[,3],  max = scale, alpha = alpha*scale)
  } else {
    z <- rgb(RGB[,1], RGB[,2], RGB[,3], max = scale, alpha = alpha*scale)
  }
  df_raster <- data.frame(coordinates(rr), fill = z, stringsAsFactors = FALSE)
  
  x <- y <- fill <- NULL ## workaround for a R CMD check 'note' about non-visible global variable in call to ggplot (variables are column names created earlier within 'data' and hence not visible to check). This does not in any way affect ggRGB,
  if(ggObj){ 
    
    ## We need to set up ggplot with at least the minimum aestetics x and y
    exe <- as.vector(extent(rr))
    df <- data.frame(x=exe[1:2],y=exe[3:4])
    
    ## Set-up plot       
    ## I prefer annotate_raster instead of geom_raster or tile to keep the fill scale free for additional rasters        
    if(annotation) {           
      dz <- matrix(z, nrow=nrow(rr), ncol=ncol(rr), byrow = TRUE)  
      p <- annotation_raster(raster = dz, xmin = exe[1], xmax = exe[2], ymin = exe[3], ymax = exe[4], interpolate = FALSE)
      if(!ggLayer) {
        p <- ggplot() + p + geom_blank(data = df, aes(x = x,y = y))
      }
    } else {
      p <- geom_raster(data = df_raster, aes(x = x, y = y, fill = fill), alpha = alpha)  
      if(!ggLayer) {
        p <- ggplot() + p + scale_fill_identity() 
      }
    }   
    
    if(coord_equal & !ggLayer) p <- p + coord_equal()
    
    return(p)
    
  } else {
    return(df_raster)
  }
  
  
}


## Perform histogram, sqrt log and 98% linear stretching
.stretch <- function (x, method = "lin", quantiles = c(0.02,0.98), band = NULL) {
  
  if(!method %in% c("lin", "hist", "log", "sqrt")) stop("Stretch method must be 'lin', 'hist', 'sqrt' or 'log'", call. = FALSE)
  if(!length(x)) return(x)
  if(all(is.na(x))) {
    warning("All values are NA. Can't compute color values -> plot will appear empty.", call. = FALSE)
    return(NA)
  } 
  ra <- range(x, na.rm = TRUE)
  if(diff(ra) == 0 & method %in% c("lin", "log", "sqrt")){ 
    if(ra[1] > 1 | ra [1] < 0) {
      warning("Only one unique value in band ", band," (", c("red","green","blue")[band], 
              "). Stretch not possible -> assigning a value of 1 for rgb color calculation.", call. = FALSE)  
      return( rep( 1, length(x)))
    } else {
      return(x)
    }
  }
  if(method == "lin"){
    if(length(quantiles) == 1) quantiles <- c(0,1) + c(quantiles, -quantiles)/100
    v <- quantile(x, quantiles, na.rm = TRUE)
    if(diff(v)==0) {
      ## sometimes lowr and upr quantile can be identical, which breaks the color calculation --> enforce a minimal distance by adding ~0
      v[2] <- v[2] + 1e-9
    }
    temp <-  (x - v[1])/(v[2] - v[1])
    temp[temp < 0] <- 0
    temp[temp > 1] <- 1 
    return(temp)
  } 
  if(method == "hist"){
    ecdfun <- ecdf(x)
    return(ecdfun(x))
  } 
  if(method == "log"){
    x <- log(x + 1)
    x <- x - min(x)
    return(x / max(x))         
  }
  if(method == "sqrt"){
    x <- sqrt(x)
    x <- x - min(x)
    return(x /max(x))
  }
}

