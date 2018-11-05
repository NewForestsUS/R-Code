##This is a "scrap" script to determine the best model to get different strata from

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
con_SNA <- dbConnect(RSQLite::SQLite(), "../FVS Runs/SNA_Take2_CFVS/result.db")     
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
Grow
length(Grow$StandID)

(tail(sort(Grow$LIVE_AG_CO2), 5))
(head(sort(Grow$LIVE_AG_CO2), 30))

CO2_Greater <- Grow$LIVE_AG_CO2[Grow$LIVE_AG_CO2 < 700]
kmean.grow <- kmeans(CO2_Greater, centers = 5, iter.max = 25, nstart = 25, trace=FALSE,
                     algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"))

kmean.grow$centers
kmean.grow$cluster
kmean.grow$size
CO2_Greater$Strata <- kmean.grow$cluster

###TRY A DIFFERENT WAY TO CREATE STRATA
jenk.grow <- classIntervals(CO2_Greater, 5, style = "jenks", rtimes = 80)
jenk.grow$brks
jenk.grow$var

CO2_Greater <- data.frame(CO2_Greater)
CO2_Greater$New_Strata <- 5
CO2_Greater$New_Strata[CO2_Greater$CO2_Greater < 381.38556] <- 4
CO2_Greater$New_Strata[CO2_Greater$CO2_Greater < 253.88244] <- 3
CO2_Greater$New_Strata[CO2_Greater$CO2_Greater < 154.57754] <- 2
CO2_Greater$New_Strata[CO2_Greater$CO2_Greater < 64.45175] <- 1
CO2_Greater$New_Strata[CO2_Greater$CO2_Greater < 49.67212] <- 1

table(CO2_Greater$New_Strata)


###OUTCOME: Dropping plots is ok as long as you have good reason.
##Using Kmeans or jenks doesnt really matter and they return pretty much the same results
#Use 5 strata with dropping the top two plots (add them back in as strata 5)
