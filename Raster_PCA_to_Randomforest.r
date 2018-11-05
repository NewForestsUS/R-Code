#####This script is going to run a kmeans seperation to determine strata for later down the road
####this will also create a large raster stack to run a PCA on
####Finally this scipt will take the output of the PCA and run it in a Random forest
####Then use that output to predict Forest/Nonforest and Strataon PCA Raster Brick

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

#Connect to the .db database and make sure the files in it are the ones you want to use
con_SNA <- dbConnect(RSQLite::SQLite(), "../../../New Forests/Forest Carbon Partners - FCP Seldovia/seldovia_ak_analysis/seldovia_ak_fvs/SNA_V0.4/result.db")     
con_SNA
alltables_SNA = dbListTables(con_SNA)
alltables_SNA

#Extract the TreeInIt file and the lost of plot names from the PlotInIt file
##Take a look at them to see if they make sense 
OnsiteSNA <- dbGetQuery(con_SNA, 'select * from onsiteCarbon')
head(OnsiteSNA)

dbDisconnect(con_SNA)

Grow <- subset.data.frame(OnsiteSNA, OnsiteSNA$RX == "NF_SNA_GROW")
Grow <- subset.data.frame(Grow, Grow$Year == 2018)
Grow2 <- subset(Grow,Grow$LIVE_AG_CO2 >= 84.9)
Grow
length(Grow$StandID)
#hist(Grow$LIVE_AG_CO2, xlim = c(0,40), breaks = 200)
Grow$Forest <- 1
Grow$Forest[Grow$LIVE_AG_CO2<20] <- 0
Grow
table(Grow$Forest)

#####Bring in the shapefile of plots and all of the different raster layers that you will use

plots_SNA <- readOGR("../FVS QAQC/shapefiles/SNA_Completed_Plots_UTMZ5.shp")
plots_SNA_short <- plots_SNA[-c(2:36)]
plot_fnf_sna <-  merge(plots_SNA_short, Grow, by.x = "ID", by.y = "StandID", all.x = FALSE)
plots_StrataFNF_SNA <- subset(plot_fnf_sna, plot_fnf_sna@data$ID != 1707)

plots_StrataFNF_SNA <- plots_StrataFNF_SNA[-c(2,3)]

##Bring in the shapefile used to mask everything with

SNA_Area <- readOGR("../SNA/SNA_Project_Area_Boundary_UTMZ5.shp")
SNA_Area_Buf <- buffer(SNA_Area, 60, dissolve = TRUE)

plot(SNA_Area_Buf, col = "BLUE")
plot(SNA_Area, add = T, col = "RED")

##YOU NEED TO MANIPULATE THE DSM AND DTM FILES SO THEY HAVE THE SAME NUMBER OF ROWS, COLUMNS AND SAME EXTENTS AS THE OTHER RASTERS
###YOU ONLY NEED OT RUN THIS CODE ONCE
b4 <- raster("../Earth Explorer Downloads/EBC_EE/Tentitive_Use/LC08_L1TP_070018_20161007_20170220_01_T1.tar/LC08_L1TP_070018_20161007_20170220_01_T1/LC08_L1TP_070018_20161007_20170220_01_T1_B4.tif")
DSM <- raster("../Earth Explorer Downloads/CHM/DSM_SNA_EBC_UTMZ5_30M.tif")
DTM <- raster("../Earth Explorer Downloads/CHM/DTM_SNA_EBC_UTMZ5_30M.tif")

DSM <- resample(DSM, b4)
DTM <- resample(DTM, b4)

writeRaster(DSM, "../Earth Explorer Downloads/Raster_PCA/DSM_SNA_EBC_UTMZ5_30M.tif", format = "GTiff", overwrite = TRUE)
writeRaster(DTM, "../Earth Explorer Downloads/Raster_PCA/DTM_SNA_EBC_UTMZ5_30M.tif", format = "GTiff", overwrite = TRUE)
###Create a forloop to crop and mask everythig to the proper outline and try to add adding them into a raster stack as well

path <- "../Earth Explorer Downloads/Raster_PCA/"
file.names <- dir(path, pattern = ".TIF", ignore.case = T)
out.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names)){
  Rast_i <- raster(paste0("../Earth Explorer Downloads/Raster_PCA/",file.names[i]))
  #extent(Rast_i) <- extent(SNA_Area_Buf)
  crop_i <- crop(Rast_i, SNA_Area_Buf)
  mask_i <- mask(crop_i, SNA_Area_Buf)
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

##From the summary we know we want the first 18 PCs so I am seeing if limiting the PCA to the first
#18PCs if it returns the same result

PCA_Ras_15<- rasterPCA(out.file, nSamples = NULL, nComp = 15, spca = TRUE, maskCheck = TRUE,  "../Earth Explorer Downloads/Raster_PCA/Results/PCA_PC15_SNA_18Sep18.tif", format = "GTiff")
PC1 <- subset(PCA_Ras_15,1)
summary(PCA_Ras_15$model)
plot(PCA_Ras_15$map)
##writeRaster(PCA_Ras_15, "../Earth Explorer Downloads/Raster_RandomForest/PCA_PC15_SNA.tif", format = "GTiff")
ras.stack <- raster("../Earth Explorer Downloads/Raster_RandomForest/PCA_PC15_SNA.tif", layer = 1:15)
nbands(ras.stack)

#####FOREST NONFOREST RANDOM FOREST TEST
##OK SO now we have the PC that we want (15) now we need to stack the NDVIs and NBRs to the PC layers 
##and extract the plots to get the values to determine forest non forest

path.Rando <- "../Earth Explorer Downloads/Raster_RandomForest/"
file.names.rando <- dir(path.Rando, pattern = ".TIF", ignore.case = T)
RandFor.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names.rando)){
  Rast_i <- raster(paste0("../Earth Explorer Downloads/Raster_RandomForest/",file.names.rando[i]))
  crop_i <- crop(Rast_i, SNA_Area)
  mask_i <- mask(crop_i, SNA_Area)
  RandFor.file <- stack(RandFor.file, mask_i)
}

end <- Sys.time()
tot.t <- end - start
tot.t ##Time difference should be about 7.8 min
###Make sure all the rasters made it through the stack
nlayers(RandFor.file)

Rando_Forest_PCA_Ras <- extract(RandFor.file, plots_StrataFNF_SNA, small = TRUE, sp=TRUE)

df.RF <- as.data.frame(Rando_Forest_PCA_Ras)
df.RF$Forest <- as.factor(df.RF$Forest)
df.RF.short <- df.RF[-c(1:2,33,34)]
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

writeRaster(Ras.FNF, "../Earth Explorer Downloads/Raster_RandomForest/Results/FNF_PCA_RF_20MG.tif", format = "GTiff")


#########STRATA RANDOM FOREST TEST
###NOW create different strata using kemans clustering and applying it ot the new "Forest" layer create from above
###Run a Kmeans clustering analysis to determine Strata. However we only want plots that are "Forest"
##SO subset the data set to only plots where Forest == 1
#
##Add the new plot list here that is completed plots that fall with the CC6m forest layer
##Also add the CC6m layer and buffer 60meters to insure you get all pixels
plots_SNA_ELCF <- readOGR("../Canopy_Cover/SNA_CanopyCov/SNA_Completed_Plots_CC6_UTMZ5_2.shp")
plots_SNAELCF_short <- plots_SNA_ELCF[-c(2:36)]
plot_ELCfnf_sna <-  merge(plots_SNAELCF_short, Grow, by.x = "ID", by.y = "StandID", all.x = FALSE)
plots_StrataFNFELC_SNA <- subset(plot_ELCfnf_sna, plot_ELCfnf_sna@data$ID != 1707)

plots_StrataFNF_SNA <- plot_ELCfnf_sna[-c(2,3)]

###Make a data frame to sue in the clustering exercise
df.plots <- as.data.frame(plots_StrataFNF_SNA)
##Bring in the shapefile used to mask everything with

CC6_Area <- readOGR("../Canopy_Cover/SNA_CanopyCov/CanopyCover_6_UTMZ5.shp")
CC6_Area_Buf <- buffer(CC6_Area, 60, dissolve = TRUE)
plot(CC6_Area_Buf)
hist(df.plots$LIVE_AG_CO2, breaks = 2000)
##Merge the plot info and the Grow info before running the Clustering so you run clustering on correct plots

##CO2_Greater <- subset(Grow, Forest == 1)
CO2_Greater <- Grow2$LIVE_AG_CO2[Grow2$LIVE_AG_CO2 < 550]
CO2_Greater <- data.frame(CO2_Greater)

jenk.grow <- classIntervals(CO2_Greater$CO2_Greater, 4, style = "jenks", rtimes = 80)
jenk.grow$brks
jenk.grow$var
#######
kmean.grow <- kmeans(CO2_Greater$CO2_Greater, centers = 5, iter.max = 25, nstart = 25, 
                     algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

kmean.grow$centers
kmean.grow$cluster
kmean.grow$size
##df.plots$Strata <- kmean.grow$cluster

##FOr some reason the clusters are not in numeric order. Create another row that orders the strata in ascending numerical order
##KEY TO CONVERT
## Old Strata      Kmeans Center   New Strata
##     1                 414           4
##     2                 30            1
##     3                 264           3
##     4                 1058          5
##     5                 143           2

df.plots$New_Strata <- 5
df.plots$New_Strata[df.plots$LIVE_AG_CO2 < 372.6191] <- 4
df.plots$New_Strata[df.plots$LIVE_AG_CO2 < 263.1009] <- 3
df.plots$New_Strata[df.plots$LIVE_AG_CO2 < 170.4385] <- 2
df.plots$New_Strata[df.plots$LIVE_AG_CO2 < 85.0479] <- 1
###Now merge your plots shapefile that you brought in earlier with the new stratfied data 

plot_strata_sna <-  merge(plots_StrataFNF_SNA, df.plots, by.x = "ID", by.y = "ID", all.x = FALSE)
#plots_Strata_SNA <- subset(plot_strata_sna, plot_strata_sna@data$ID != 1707)

plots_Strata_SNA <- plot_strata_sna[-c(2:7)]

##MANIPULATE THE CHM FILES THE SAME AS YOU DID TO THE DTM AND DSM FILES. 
##ONCE AGAIN THIS ONLY NEEDS TO BE DONE ONCE

CHM <- raster("../Earth Explorer Downloads/CHM/CHM_SNA_EBC_UTMZ5_30M.tif")
CHM <- resample(CHM, b4)

writeRaster(CHM, "../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/CHM_SNA_EBC_UTMZ5_30M.tif", format = "GTiff", overwrite = TRUE)

##OK SO now we have the PC that we want (15) now we need to stack the NDVIs and NBRs to the PC layers 
##and extract the plots to get the values to determine the different strata of the new "Forest" area

path.Rando <- "../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/"
file.names.rando <- dir(path.Rando, pattern = ".TIF", ignore.case = T)
RandFor.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names.rando)){
  Rast_i <- raster(paste0("../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/",file.names.rando[i]))
  crop_i <- crop(Rast_i, CC6_Area_Buf)
  mask_i <- mask(crop_i, CC6_Area_Buf)
  RandFor.file <- stack(RandFor.file, mask_i)
}

end <- Sys.time()
tot.t <- end - start
tot.t ##Time difference should be about 7.8 min
###Make sure all the rasters made it through the stack
nlayers(RandFor.file)

Rando_Forest_PCA_Ras <- extract(RandFor.file, plots_Strata_SNA, small = TRUE, sp=TRUE)

df.RF <- as.data.frame(Rando_Forest_PCA_Ras)
df.RF$New_Strata <- as.factor(df.RF$New_Strata)
df.RF.short <- df.RF[-c(1, 33,34)]
##Use this file in your Random Forest 

metric <- "Accuracy"
control <- trainControl(method = "LGOCV", number = 80, p = 0.7)

model_rf <- train(New_Strata ~ ., data = df.RF.short, method = "rf", metric = metric, 
                  trControl = control)

print(model_rf)

prediction_rf <- predict(model_rf, newdata =df.RF.short)
confusionMatrix(prediction_rf, df.RF.short$New_Strata)

model_rf$pred

model_rf$finalModel

model_rf$results

model_rf$coefnames

model_rf$bestTune

model_rf$yLimits

model_rf$times


Ras.Strata <- predict(RandFor.file, model_rf_new)
plot(Ras.Strata)

writeRaster(Ras.Strata, "../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/Results/Strata_PCA_RF_FNFCC6_NewTC_Abovebase_17Oct18.tif", format = "GTiff", overwrite=TRUE)

###TRY NEW RANDOMFOREST Function
model_rf_new <- randomForest(New_Strata~ ., data = df.RF.short)
model_rf_new
importance(model_rf_new)
varImpPlot(model_rf_new)
plot(model_rf_new)

###Use RF Model to restratify TVSF
##First bring in TVSF sampling frame and buffer to not exclude pixels
#Then you will need to pass the layers through it jsut so you have a raster stack to use with the RF
TVSF_Area <- readOGR("../SNA/Sample_Frame_UTMZ5.shp")
TVSF_Area_Buf <- buffer(TVSF_Area, 60, dissolve = TRUE)
plot(TVSF_Area)

path.Rando <- "../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/"
file.names.rando <- dir(path.Rando, pattern = ".TIF", ignore.case = T)
RandFor.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names.rando)){
  Rast_i <- raster(paste0("../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/",file.names.rando[i]))
  crop_i <- crop(Rast_i, TVSF_Area_Buf)
  mask_i <- mask(crop_i, TVSF_Area_Buf)
  RandFor.file <- stack(RandFor.file, mask_i)
}

end <- Sys.time()
tot.t <- end - start
tot.t ##Time difference should be about 7.8 min
###Make sure all the rasters made it through the stack
nlayers(RandFor.file)

Ras.Strata <- predict(RandFor.file, model_rf_new)
plot(Ras.Strata)

writeRaster(Ras.Strata, "../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/Results/Strata_PCA_RF_TVSF_NewTC_Strata_AboveCP.tif", format = "GTiff")

###Use RF Model to restratify CC7m
##First bring in CC7m sampling frame and buffer to not exclude pixels
#Then you will need to pass the layers through it jsut so you have a raster stack to use with the RF
CC7m_Area <- readOGR("../Canopy_Cover/SNA_CanopyCover/Canopy_Cover_7m_PAB_UTMZ5.shp")
CC7m_Area_Buf <- buffer(CC7m_Area, 60, dissolve = TRUE)
plot(CC7m_Area)

path.Rando <- "../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/"
file.names.rando <- dir(path.Rando, pattern = ".TIF", ignore.case = T)
RandFor.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names.rando)){
  Rast_i <- raster(paste0("../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/",file.names.rando[i]))
  crop_i <- crop(Rast_i, CC7m_Area_Buf)
  mask_i <- mask(crop_i, CC7m_Area_Buf)
  RandFor.file <- stack(RandFor.file, mask_i)
}

end <- Sys.time()
tot.t <- end - start
tot.t ##Time difference should be about 7.8 min
###Make sure all the rasters made it through the stack
nlayers(RandFor.file)

Ras.Strata <- predict(RandFor.file, model_rf_new)
plot(Ras.Strata)

writeRaster(Ras.Strata, "../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/Results/Strata_PCA_RF_CC7m_NewTC.tif", format = "GTiff")

###Use RF Model to restratify TVSF
##First bring in TVSF sampling frame and buffer to not exclude pixels
#Then you will need to pass the layers through it jsut so you have a raster stack to use with the RF
CC4.5m_Area <- readOGR("../Canopy_Cover/SNA_CanopyCover/Canopy_Cover_15ft_PAB_UTMZ5.shp")
CC4.5m_Area_Buf <- buffer(CC4.5m_Area, 60, dissolve = TRUE)
plot(CC4.5m_Area)

path.Rando <- "../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/"
file.names.rando <- dir(path.Rando, pattern = ".TIF", ignore.case = T)
RandFor.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names.rando)){
  Rast_i <- raster(paste0("../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/",file.names.rando[i]))
  crop_i <- crop(Rast_i, CC4.5m_Area_Buf)
  mask_i <- mask(crop_i, CC4.5m_Area_Buf)
  RandFor.file <- stack(RandFor.file, mask_i)
}

end <- Sys.time()
tot.t <- end - start
tot.t ##Time difference should be about 7.8 min
###Make sure all the rasters made it through the stack
nlayers(RandFor.file)

Ras.Strata <- predict(RandFor.file, model_rf_new)
plot(Ras.Strata)

writeRaster(Ras.Strata, "../Earth Explorer Downloads/Raster_RandomForest_DSM_DTM_PCA/Results/Strata_PCA_RF_CC4_5m_NewTC.tif", format = "GTiff")
