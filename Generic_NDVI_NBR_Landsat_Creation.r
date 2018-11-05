####Pull out the onsiteCarbon spreadsheet from results.db and determine what is "forested/nonforested"

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


##To do that you may need to create the NDVI and NBR files before.
#(make sure you bring in the 30m pixel DEM)
####Make the NDVI and NBR bands here. Also bring in the DEM and make the exten the same as every other raster you are using in the file
##Then write out the DEM NEVI and NBR to the folder to be able to run the forloop
##You will need to cahge the names each iteration

b4 <- raster("../../Virtual_Machine/Kenai_Regression_Unzip/Kenai_Regression_Landsat_CHM/resampled10mInput/LC08_L1TP_070018_20161007_20170220_01_T1_B4_pgc_res.tif")
b5 <- raster("../../Virtual_Machine/Kenai_Regression_Unzip/Kenai_Regression_Landsat_CHM/resampled10mInput/LC08_L1TP_070018_20161007_20170220_01_T1_B5_pgc_res.tif")
b6 <- raster("../../Virtual_Machine/Kenai_Regression_Unzip/Kenai_Regression_Landsat_CHM/resampled10mInput/LC08_L1TP_070018_20161007_20170220_01_T1_B6_pgc_res.tif")


NDVI <- (b5-b4)/(b5+b4)
NBR <- (b5-b6)/(b5+b6)

##Check them values should be between -1 and 1 and water should be 0
plot(NDVI)
plot(NBR)

##Write the raster back out to your folder to use for your forloop

writeRaster(NDVI, "../../Virtual_Machine/Kenai_Regression_Unzip/RF_Input/NDVI_07Oct15.tif", format = "GTiff", overwrite = TRUE)
writeRaster(NBR, "../../Virtual_Machine/Kenai_Regression_Unzip/RF_Input/NBR_07Oct15.tif", format = "GTiff", overwrite = TRUE)
