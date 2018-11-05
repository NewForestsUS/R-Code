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
library(Cubist)
###Bring in Plots with spatial data and carbon quantification 
AllPlots <- readOGR(dsn = "../Kenai_Regression/All_PlotsMeas_Keanai_LiveAGB_WGS84.shp")
##Bring in the Shapefile to clip everything by
KenaiArea <- readOGR(dsn = "../Kenai_Regression/Kenai_Boundary_WGS84.shp")

##Create our raster stack

path.Rando <- "../Kenai_Regression/Raster_RF_Regression_Radar/"
file.names.rando <- dir(path.Rando, pattern = ".TIF", ignore.case = T)
RandFor.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names.rando)){
  Rast_i <- raster(paste0("../Kenai_Regression/Raster_RF_Regression_Radar/",file.names.rando[i]))
  crop_i <- crop(Rast_i, KenaiArea)
  mask_i <- mask(crop_i, KenaiArea)
  RandFor.file <- stack(RandFor.file, mask_i)
}

end <- Sys.time()
tot.t <- end - start
tot.t ##Time difference should be about 7.8 min
###Make sure all the rasters made it through the stack
nlayers(RandFor.file)

Rando_Forest_PCA_Ras <- extract(RandFor.file, AllPlots, small = TRUE, sp=TRUE)

df.RF <- as.data.frame(Rando_Forest_PCA_Ras)
#df.RF.cut <- subset(df.RF, df.RF$ID != "2268")
#df.RF.cut$New_Strata <- as.factor(df.RF.cut$New_Strata)
df.RF.short <- df.RF[-c(1,3:5, 27:28)]
##Use this file in your Random Forest 

inTrain <- createDataPartition(df.RF.short$LIVE_AG, p = 0.8, list = FALSE)
trainDF <- df.RF.short[inTrain,]
testDF <- df.RF.short[-inTrain,]

model_rf_new <- randomForest(LIVE_AG~ ., data = trainDF, importantance = TRUE)
model_rf_new
importance(model_rf_new)
varImpPlot(model_rf_new)
plot(model_rf_new)
RF_Predict <- predict(model_rf_new, newdata = testDF)
RMSE_RF <- sqrt(sum((RF_Predict - testDF$LIVE_AG)^2)/length(RF_Predict))
print(RMSE_RF/mean(testDF$LIVE_AG))

Ras.Strata <- predict(RandFor.file, model_rf_new)
plot(Ras.Strata)

writeRaster(Ras.Strata, "../EBC_Regression_RF/EBC_AGBiomass_Map_RF_Radar.tif", format = "GTiff", overwrite = TRUE)

mtry_def <- floor(sqrt(ncol(trainDF))*.8) 
t_grid <- expand.grid(mtry= c(mtry_def))

model_rf <- train(LIVE_AG ~ ., data = trainDF, method = "rf", ntree = 500)

print(model_rf)

predictions <- predict(model_rf, testDF[,2:22])
RMSE <- sqrt(sum((predictions - testDF$LIVE_AG)^2)/length(predictions))
print(RMSE)

print(RMSE/mean(testDF$LIVE_AG))


#####Lets try a cubist approach
inTrain2 <- sample(1:nrow(df.RF.short), floor(.8*nrow(df.RF.short)))
trainDF_Pred <- df.RF.short[inTrain2,-1]
testDF_Pred <- df.RF.short[-inTrain2,-1]

trainDF_Resp <- df.RF.short$LIVE_AG[ inTrain2]
testDF_Resp <- df.RF.short$LIVE_AG[-inTrain2]

mod_cube <- cubist(x = trainDF_Pred, y = trainDF_Resp)
summary(mod_cube)

mod_tree_pred <- predict(mod_cube, testDF_Pred)
sqrt(mean((mod_tree_pred - testDF_Resp)^2))
cor(mod_tree_pred, testDF_Resp)^2

Ras.Strata2 <- predict(RandFor.file,mod_cube)
plot(Ras.Strata2)


mod_cube_point_extract <- extract(Ras.Strata2, AllPlots,small = TRUE, sp=TRUE)

plot(mod_cube_point_extract@data$LIVE_AG ~ mod_cube_point_extract@data$layer, 
     xlim = c(0,800), ylim = c(0,800))
abline(0,1)
