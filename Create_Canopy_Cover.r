###Creating a %Canopy Cover layer from a CHM
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

###Bring in the CHM

CHM <- raster("../DTM_EBC_SNA/CHM_EBC_SNA_3.tif")
plot(CHM)

###Now create a layer designating everything above 2m as one
##and below 2m as 0m

CHMgr2 <- CHM
CHMgr2[CHM >= 7.5] <- 1
CHMgr2[CHM<7.5] <- 0

plot(CHMgr2)

CHMgr2_30 <- aggregate(CHMgr2,fact = 6, fun = sum) 

res(CHMgr2_30)

plot(CHMgr2_30)

Canopy_Cover <- (CHMgr2_30/36)*100

plot(Canopy_Cover)

Canopy_Cover_10 <- Canopy_Cover
Canopy_Cover_10[Canopy_Cover <= 10] <- NA

plot(Canopy_Cover_10)

writeRaster(Canopy_Cover_10, "../Canopy_Cover/Canopy_Cover_GR10_7.5m.tif", format = "GTiff")



###Look at a hist of the heghts to see what YOU think is the best cutoff
con <- dbConnect(RSQLite::SQLite(), "../FVS QAQC/databases/SNA_FVS_DATA_8_08_18.db")     
con
alltables = dbListTables(con)
alltables

#Extract the TreeInIt file and the lost of plot names from the PlotInIt file
##Take a look at them to see if they make sense 
Tree <- dbGetQuery(con, 'select * from FVS_TreeInIt')
head(Tree)

dbDisconnect(con)

hist(Tree$Ht, breaks = 200)
summary(Tree$Ht)
sd(Tree$Ht)


#####We can use CHM to create Forest Nonforest doesnt have to be used with RF or PCA Model