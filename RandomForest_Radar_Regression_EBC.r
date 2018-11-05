###THis script will create a .tif file of aboveground Biomass after using a random forest approach 
##and Principle Compenents that have been predetermined
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

###Bring in Plots with spatial data and carbon quantification 
EBCPlots <- readOGR(dsn = "./EBC_Plots_CarbonQuant_ELCS_GCSWGS84.shp")
##Bring in the Shapefile to clip everything by
EBCArea <- readOGR(dsn = "../../Virtual_Machine/shapefiles/EBC_Project_Area_Boundary_WGS84_Buf.shp")

##Create our raster stack

path.Rando <- "../../Virtual_Machine/resampled17mInput/"
file.names.rando <- dir(path.Rando, pattern = ".TIF", ignore.case = T)
RandFor.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names.rando)){
  Rast_i <- raster(paste0("../../Virtual_Machine/resampled17mInput/",file.names.rando[i]))
  crop_i <- crop(Rast_i, EBCArea)
  mask_i <- mask(crop_i, EBCArea)
  RandFor.file <- stack(RandFor.file, mask_i)
}

end <- Sys.time()
tot.t <- end - start
tot.t ##Time difference should be about 7.8 min
###Make sure all the rasters made it through the stack
nlayers(RandFor.file)

Rando_Forest_PCA_Ras <- extract(RandFor.file, EBCPlots, small = TRUE, sp=TRUE)

df.RF <- as.data.frame(Rando_Forest_PCA_Ras)
df.RF.cut <- subset(df.RF, df.RF$ID != "2268")
df.RF.cut$New_Strata <- as.factor(df.RF.cut$New_Strata)
df.RF.short <- df.RF.cut[-c(1,2, 29:30)]
##Use this file in your Random Forest 

model_rf_new <- randomForest(LIVE_AG~ ., data = df.RF.short, importantance = TRUE, nodesize = 5)
model_rf_new
importance(model_rf_new)
varImpPlot(model_rf_new)
plot(model_rf_new)
tuneRF(LIVE_AG~ ., data = df.RF.short)

Ras.Strata <- predict(RandFor.file, model_rf_new)
plot(Ras.Strata)

writeRaster(Ras.Strata, "../EBC_Regression_RF/EBC_AGBiomass_Map_RF_Radar.tif", format = "GTiff", overwrite = TRUE)

inTrain <- createDataPartition(df.RF.short$LIVE_AG, p = 0.75, list = FALSE)
trainDF <- df.RF.short[inTrain,]
testDF <- df.RF.short[-inTrain,]

mtry_def <- floor(sqrt(ncol(trainDF))*.75) 
t_grid <- expand.grid(mtry= c(mtry_def))

model_rf <- train(LIVE_AG ~ ., data = trainDF, method = "rf", ntree = 500)

print(model_rf)

predictions <- predict(model_rf, testDF[,2:26])
RMSE <- sqrt(sum((predictions - testDF$LIVE_AG)^2)/length(predictions))
print(RMSE)

print(RMSE/mean(testDF$LIVE_AG))
