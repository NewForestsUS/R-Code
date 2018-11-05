###To make raster stack into DF
#1) run 64bit R so it will have more memory and can change something like that
#2) when doing as.data.frame() add xy = TRUE this will return the spatial coordinates so you should
#be able to merge the raster stack and the raster with the values in it
#3) merg the two dataframes together based on the coordinates

library(sp)
library(raster)
library(maptools)
library(rgdal)
library(fields)
library(dplyr)
library(odbc)
library(DBI)
library(RSQLite)
library(caret)
library(corrplot)
library(gdalUtils)
library(ggplot2)
library(reshape2)
library(rgeos)
library(randomForest)
library(randomForestExplainer)
library(classInt)

#####Bring in the shapefile of plots and all of the different raster layers that you will use

plots_EBC <- readOGR("../FVS QAQC/shapefiles/EBC COMPLETED PLOTS UTMZ5.shp")
plots_EBC_short <- plots_EBC[-c(2:36)]
plot_fnf_EBC <-  merge(plots_EBC_short, Grow, by.x = "ID", by.y = "StandID", all.x = FALSE)


plots_StrataFNF_EBC <- plot_fnf_EBC[-c(2,3)]

##Bring in the shapefile used to mask everything with

EBC_Area <- readOGR("../EBC/EBC_Project_Forest_Area_CYE_WGS84_Dis.shp")

plot(EBC_Area, col = "RED")


###Create a forloop to crop and mask everythig to the proper outline and try to add adding them into a raster stack as well

path <- "../Earth Explorer Downloads/Raster_PCA_EBC/"
file.names <- dir(path, pattern = ".TIF", ignore.case = T)
out.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names)){
  Rast_i <- raster(paste0("../Earth Explorer Downloads/Raster_PCA_EBC/",file.names[i]))
  #extent(Rast_i) <- extent(SNA_Area_Buf)
  crop_i <- crop(Rast_i, EBC_Area_Buf)
  mask_i <- mask(crop_i, EBC_Area_Buf)
  out.file <- stack(out.file, mask_i)
}

end <- Sys.time()
tot.t <- end - start
tot.t ##Time difference should be about 7.8 min
###Make sure all the rasters made it through the stack
nlayers(out.file)
##Looks like it worked. try the PCAnow

PCA_Raster <- rasterPCA(out.file, nSamples = NULL, nComp = nlayers(out.file), spca = TRUE, maskCheck = TRUE)

summary(PCA_Raster$model)

##From the summary we know we want the first 12 PCs so I am seeing if limiting the PCA to the first
#18PCs if it returns the same result

PCA_Ras_12<- rasterPCA(out.file, nSamples = NULL, nComp = 12, spca = TRUE, maskCheck = TRUE,  "../Earth Explorer Downloads/Raster_PCA_EBC/Results/PCA_PC12_EBC_24Sep18.tif", format = "GTiff")
PC1 <- subset(PCA_Ras_12,1)
summary(PCA_Ras_12$model)
plot(PCA_Ras_12$map)

####Use ARCGIS to seperate teh PCA results into the different components 

#####FOREST NONFOREST RANDOM FOREST TEST
##OK SO now we have the PC that we want (12) now we need to stack the NDVIs and NBRs to the PC layers 
##and extract the plots to get the values to determine forest non forest

path.Rando <- "../Earth Explorer Downloads/Raster_RandomForest_All_EBC/"
file.names.rando <- dir(path.Rando, pattern = ".TIF", ignore.case = T)
RandFor.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names.rando)){
  Rast_i <- raster(paste0("../Earth Explorer Downloads/Raster_RandomForest_All_EBC/",file.names.rando[i]))
  crop_i <- crop(Rast_i, EBC_Area_Buf)
  mask_i <- mask(crop_i, EBC_Area_Buf)
  RandFor.file <- stack(RandFor.file, mask_i)
}

end <- Sys.time()
tot.t <- end - start
tot.t ##Time difference should be about 7.8 min
###Make sure all the rasters made it through the stack
nlayers(RandFor.file)

Rando_Forest_PCA_Ras <- extract(RandFor.file, plots_StrataFNF_EBC, small = TRUE, sp=TRUE)

df.RF <- as.data.frame(Rando_Forest_PCA_Ras)
df.RF$Forest <- as.factor(df.RF$Forest)
df.RF.short <- df.RF[-c(1:2,27:28)]
##Use this file in your Random Forest 

metric <- "Accuracy"
control <- trainControl(method = "LGOCV", number = 80, p = 0.7)

model_rf <- train(Forest ~ ., data = df.RF.short, method = "rf", metric = metric, 
                  trControl = control)

print(model_rf)

prediction_rf <- predict(model_rf, newdata =df.RF.short)
confusionMatrix(prediction_rf, df.RF.short$Forest)

model_rf$pred

model_rf$finalModel

model_rf$results

model_rf$coefnames

model_rf$bestTune

model_rf$yLimits

model_rf$times


Ras.FNF <- predict(RandFor.file, model_rf)
plot(Ras.FNF)

writeRaster(Ras.FNF, "../Earth Explorer Downloads/Raster_RandomForest_All_EBC/Results/FNF_PCA_RF_EBC_10MG.tif", format = "GTiff")
