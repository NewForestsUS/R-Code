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

### Bring in files we will need

##GIS Files
CHM <- raster("../Kenai_Regression/CHM/CHM_EBC_SNA_3_UTMZ5.tif")
plot(CHM)
CHMgr6 <- raster("../Kenai_Regression/CHM/CHM_EBC_SNA_3_GT6_UTMZ5.tif")
plot(CHMgr6)

plots_SNA <- readOGR("../FVS QAQC/shapefiles/SNA_Completed_Plots_UTMZ5.shp")
plots_EBC <- readOGR("../FVS QAQC/shapefiles/EBC COMPLETED PLOTS UTMZ5.shp")
plots_PGC <- readOGR("../Kenai_Regression/Port_Graham/PG_Plots_InCHM_UTMZ5.shp")

plot(CHM)
plot(plots_SNA, col = "red", add = TRUE, pch = 19, cex = 0.25)
plot(plots_EBC, col = "blue", add = TRUE, pch = 19, cex = 0.25)
plot(plots_PGC, col = "green", add = TRUE, pch = 19, cex = 0.25)

####Now the data that corresponds with the plot info
##EBC
con_EBC <- dbConnect(RSQLite::SQLite(), "../../../New Forests/Forest Carbon Partners - nwkNanwalekEBC_analysis/nwkNanwalekEBC_fvs/EBC_V0.13_FVS_Output/result.db")     
con_EBC
alltables_EBC = dbListTables(con_EBC)
alltables_EBC

#Extract the TreeInIt file and the lost of plot names from the PlotInIt file
##Take a look at them to see if they make sense 
TreeC <- dbGetQuery(con_EBC, 'select * from massTree')
head(TreeC)

dbDisconnect(con_EBC)
Grow_EBC <- subset.data.frame(TreeC, TreeC$Year == 2018)
Grow_EBC
length(unique(Grow_EBC$StandID))
Grow_EBC <- Grow_EBC[-c(1, 3, 5, 9:12, 14)]
Grow_EBC$TreeId <- as.numeric(as.character(Grow_EBC$TreeId))
Grow_EBC$StandID <- as.numeric(as.character(Grow_EBC$StandID))
EBC_Plot_kg <- aggregate(Grow_EBC, by = Grow_EBC$StandID, FUN = "sum")
EBC_Plot_kg$StandID <- EBC_Plot_kg$Group.1
EBC_Plot_kg <- EBC_Plot_kg[-c(1:2, 4:6)]

EBC_Plot_kg_test <- aggregate(Grow_EBC, by = list(Grow_EBC$StandID), FUN = "mean")
##SNA
con_SNA <- dbConnect(RSQLite::SQLite(), "../../../New Forests/Forest Carbon Partners - FCP Seldovia/seldovia_ak_analysis/seldovia_ak_fvs/SNA_v0.05/result.db")     
con_SNA
alltables_SNA = dbListTables(con_SNA)
alltables_SNA

#Extract the TreeInIt file and the lost of plot names from the PlotInIt file
##Take a look at them to see if they make sense 
TreeSNA <- dbGetQuery(con_SNA, 'select * from massTree')
head(TreeSNA)

dbDisconnect(con_SNA)
Grow_SNA <- subset.data.frame(TreeSNA, TreeSNA$Year == 2018)
length(unique(Grow_SNA$StandID))
Grow_SNA <- Grow_SNA[-c(1, 3, 5, 9:12, 14)]
Grow_SNA$TreeId <- as.numeric(as.character(Grow_SNA$TreeId))
Grow_SNA$StandID <- as.numeric(as.character(Grow_SNA$StandID))
SNA_Plot_kg <- aggregate(Grow_SNA, by = list(Grow_SNA$StandID), FUN = "sum")
SNA_Plot_kg$StandID <- SNA_Plot_kg$Group.1
SNA_Plot_kg <- SNA_Plot_kg[-c(1:2, 4:6)]
##PORT GRAHAM
con_PGC <- dbConnect(RSQLite::SQLite(), "../Kenai_Regression/Port_Graham/bundle_PGC FVS 1_2_20180626/result.db")     
con_PGC
alltables_PGC = dbListTables(con_PGC)
alltables_PGC

#Extract the TreeInIt file and the lost of plot names from the PlotInIt file
##Take a look at them to see if they make sense 
TreePG <- dbGetQuery(con_PGC, 'select * from massTree')
head(TreePG)

dbDisconnect(con_PGC)
Grow_PGC <- subset.data.frame(TreePG, TreePG$Year == 2016)
length(unique(Grow_PGC$StandID))
Grow_PGC$ID <- gsub("NFPG_", "", Grow_PGC$StandID)
Grow_PGC <- Grow_PGC[-c(1, 3:5, 9:12, 14)]
Grow_PGC$TreeId <- as.numeric(as.character(Grow_PGC$TreeId))
Grow_PGC$ID <- as.numeric(as.character(Grow_PGC$ID))
PG_Plot_kg <- aggregate(Grow_PGC, by = list(Grow_PGC$ID), FUN = "sum")
PG_Plot_kg$ID <- PG_Plot_kg$Group.1
PG_Plot_kg <- PG_Plot_kg[-c(1:5)]
###Now combine the AGCO2 to the plot locations
##EBC
plots_EBC_short <- plots_EBC[-c(2:36)]
plot_AGB_EBC <-  merge(plots_EBC_short, EBC_Plot_kg, by.x = "ID", by.y = "StandID", all.x = FALSE)
##SNA
plots_SNA_short <- plots_SNA[-c(2:36)]
plot_AGB_SNA <-  merge(plots_SNA_short, SNA_Plot_kg, by.x = "ID", by.y = "StandID", all.x = FALSE)
##PGC
plots_PGC_short <- plots_PGC[-c(1:2, 4:24)]
plot_AGB_PGC <-  merge(plots_PGC_short, PG_Plot_kg, by.x = "plotid", by.y = "ID", all.x = FALSE)



###Extract the points from the CHM and the CHMgr6 using a buffer of 8.01624m
EBC_CHM <- extract(CHM, plot_AGB_EBC, buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)
EBC_CHMgr6 <- extract(CHMgr6, plot_AGB_EBC,buffer = 8.01624, small = TRUE, fun = sum, sp=TRUE)
SNA_CHM <- extract(CHM, plot_AGB_SNA,buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)
SNA_CHMgr6 <- extract(CHMgr6, plot_AGB_SNA, buffer = 8.01624, small = TRUE, fun = sum, sp=TRUE)

EBC_CC6m <- (EBC_CHMgr6/9)*100
SNA_CC6m <- (SNA_CHMgr6/9)*100

EBC_CHM_DF <- as.data.frame(EBC_CHM)
EBC_CC6m_DF <- as.data.frame(EBC_CC6m)
SNA_CHM_DF <- as.data.frame(SNA_CHM)
SNA_CC6m_DF <- as.data.frame(SNA_CC6m)

CHM_DF <- rbind(EBC_CHM_DF, SNA_CHM_DF)
CC6_DF <- rbind(EBC_CC6m_DF, SNA_CC6m_DF)

df.rg <- merge(CHM_DF,CC6_DF, by.x = "ID", by.y = "ID")
df.rg <- df.rg[-c(4:6)]

plot(kg_dry_total ~ CHM_EBC_SNA_3_UTMZ5, data = CHM_DF)
plot(LIVE_AG_CO2.x ~ CHM_EBC_SNA_3_GT6_UTMZ5, data = df.rg)
mod1 <- lm(kg_dry_total ~ CHM_EBC_SNA_3_UTMZ5, data = CHM_DF)
summary(mod1)

mod2 <- lm(LIVE_AG_CO2.x ~ CHM_EBC_SNA_3_UTMZ5 + CHM_EBC_SNA_3_GT6_UTMZ5, data = df.rg)
summary(mod2)
mod3 <- lm(LIVE_AG_CO2.x ~ CHM_EBC_SNA_3_UTMZ5 * CHM_EBC_SNA_3_GT6_UTMZ5, data = df.rg)
summary(mod3)

df.test <- df.rg
df.test <- subset.data.frame(df.test, df.test$LIVE_AG_CO2.x !=0)
plot(log(kg_dry_total) ~ (CHM_EBC_SNA_3_UTMZ5), data = CHM_DF)

round(cor(CHM_DF[,-c(1, 7:8)]),2)





###Now aggregate the CHM layers to 15m pixels rather than 5m pixels.###################################################################################################### 
##we want the mean for CHM and sum for CHMgr6 

CHMagg <- aggregate(CHM, fact = 3, fun = mean)
plot(CHMagg)
CHMgr6agg <- aggregate(CHMgr6, fact = 3, fun = sum)
plot(CHMgr6agg)

CC6m <- (CHMgr6agg/9)*100
plot(CC6m)

##Now we need to extract values from the CHMagg and CC6m of EBC and SNA plots

EBC_CHM_agg <- extract(CHMagg, plot_AGB_EBC, buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)
EBC_CHMgr6_Agg <- extract(CC6m, plot_AGB_EBC,buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)
SNA_CHM_agg <- extract(CHMagg, plot_AGB_SNA,buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)
SNA_CHMgr6_agg <- extract(CC6m, plot_AGB_SNA, buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)

EBC_CHM_DF2 <- as.data.frame(EBC_CHM_agg)
EBC_CC6m_DF2 <- as.data.frame(EBC_CHMgr6_Agg)
SNA_CHM_DF2 <- as.data.frame(SNA_CHM_agg)
SNA_CC6m_DF2 <- as.data.frame(SNA_CHMgr6_agg)

CHM_DF2 <- rbind(EBC_CHM_DF2, SNA_CHM_DF2)
CC6_DF2 <- rbind(EBC_CC6m_DF2, SNA_CC6m_DF2)

df.rg.agg <- merge(CHM_DF2,CC6_DF2, by.x = "ID", by.y = "ID")
df.rg.agg <- df.rg.agg[-c(7:12, 14:15)]

plot(kg_dry_total.x ~ CHM_EBC_SNA_3_UTMZ5, data = df.rg.agg)
plot(kg_dry_total.x ~ CHM_EBC_SNA_3_GT6_UTMZ5, data = df.rg.agg)

mod.agg.1 <- lm(kg_dry_total.x ~ CHM_EBC_SNA_3_UTMZ5, data = df.rg.agg)
summary(mod.agg.1)

mod.agg.2 <- lm(kg_dry_total.x ~ CHM_EBC_SNA_3_GT6_UTMZ5, data = df.rg.agg)
summary(mod.agg.2)

mod.agg.3 <- lm(kg_dry_total.x ~ CHM_EBC_SNA_3_UTMZ5*CHM_EBC_SNA_3_GT6_UTMZ5, data = df.rg.agg)
summary(mod.agg.3)

mod.agg.4 <- lm(log(kg_dry_total.x) ~ (CHM_EBC_SNA_3_UTMZ5), data = df.rg.agg)
summary(mod.agg.4)

###Try with the max tree height####################################################################################
CHMagg.max <- aggregate(CHM, fact = 3, fun = max)
plot(CHMagg.max)


##Now we need to extract values from the CHMagg and CC6m of EBC and SNA plots

EBC_CHM_agg.max <- extract(CHMagg.max, plot_AGB_EBC, buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)
SNA_CHM_agg.max <- extract(CHMagg.max, plot_AGB_SNA,buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)

EBC_CHM_DF.max <- as.data.frame(EBC_CHM_agg.max)
SNA_CHM_DF.max <- as.data.frame(SNA_CHM_agg.max)

CHM_DF.max <- rbind(EBC_CHM_DF.max, SNA_CHM_DF.max)

plot(kg_dry_total ~ CHM_EBC_SNA_3_UTMZ5, CHM_DF.max)

mod.agg.max1 <- lm(kg_dry_total ~ CHM_EBC_SNA_3_UTMZ5, data = CHM_DF.max)
summary(mod.agg.max1)

mod.agg.max2 <- lm(log(kg_dry_total) ~ (CHM_EBC_SNA_3_UTMZ5), data = CHM_DF.max)
summary(mod.agg.max2)

###Try making it mean tree height with acres#############################################################
CHMagg.acre <- aggregate(CHM, fact = 12.72298, fun = mean)
plot(CHMagg.acre)

##Now we need to extract values from the CHMagg and CC6m of EBC and SNA plots

EBC_CHM_agg.acre <- extract(CHMagg.acre, plot_AGB_EBC, buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)
SNA_CHM_agg.acre <- extract(CHMagg.acre, plot_AGB_SNA,buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)

EBC_CHM_DF.acre <- as.data.frame(EBC_CHM_agg.acre)

SNA_CHM_DF.acre <- as.data.frame(SNA_CHM_agg.acre)

CHM_DF.acre <- rbind(EBC_CHM_DF.acre, SNA_CHM_DF.acre)

plot(kg_acre ~ CHM_EBC_SNA_3_UTMZ5, data = CHM_DF.acre)

mod.agg.acre1 <- lm(kg_acre ~ CHM_EBC_SNA_3_UTMZ5, data = CHM_DF.acre)
summary(mod.agg.acre1)

mod.agg.acre2 <- lm(log(kg_acre) ~ (CHM_EBC_SNA_3_UTMZ5), data = CHM_DF.acre)
summary(mod.agg.acre2)

###Try max tree height with acres##############################################################
CHMagg.acremax <- aggregate(CHM, fact = 12.72298, fun = max)
plot(CHMagg.acremax)
CHMgr6agg.acremax <- aggregate(CHMgr6, fact = 12.72298, fun = sum)
plot(CHMgr6agg.acremax)

CC6m.acremax <- (CHMgr6agg.acremax/169)*100
plot(CC6m.acremax)

##Now we need to extract values from the CHMagg and CC6m of EBC and SNA plots

EBC_CHM_agg.acremax <- extract(CHMagg.acremax, plot_AGB_EBC, buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)
SNA_CHM_agg.acremax <- extract(CHMagg.acremax, plot_AGB_SNA,buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)

EBC_CC6m.acremax <- extract(CC6m.acremax, plot_AGB_EBC, buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)
SNA_CC6m.acremax <- extract(CC6m.acremax, plot_AGB_SNA,buffer = 8.01624, small = TRUE, fun = mean, sp=TRUE)

EBC_CHM_DF.acremax <- as.data.frame(EBC_CHM_agg.acremax)
SNA_CHM_DF.acremax <- as.data.frame(SNA_CHM_agg.acremax)
EBC_CC6m.acremax <- as.data.frame(EBC_CC6m.acremax)
SNA_CC6m.acremax <- as.data.frame(SNA_CC6m.acremax)


CHM_DF.acremax <- rbind(EBC_CHM_DF.acremax, SNA_CHM_DF.acremax)
CC6m_DF.acremax <- rbind(EBC_CC6m.acremax, SNA_CC6m.acremax)

df.rg.agg.acremax <- merge(CHM_DF.acremax,CC6m_DF.acremax, by.x = "ID", by.y = "ID")
df.rg.agg.acremax <- df.rg.agg.acremax[-c(7:12, 14:15)]

plot(kg_acre ~ CHM_EBC_SNA_3_UTMZ5, data = CHM_DF.acremax)
plot(kg_acre ~ CHM_EBC_SNA_3_GT6_UTMZ5, data = CC6m_DF.acremax)

mod.agg.acremax1 <- lm(kg_acre ~ CHM_EBC_SNA_3_UTMZ5, data = CHM_DF.acremax)
summary(mod.agg.acremax1)

mod.agg.acremax2 <- lm(kg_acre ~ CHM_EBC_SNA_3_GT6_UTMZ5, data = CC6m_DF.acremax)
summary(mod.agg.acremax2)

mod.agg.acremax3 <- lm(kg_acre.x ~ CHM_EBC_SNA_3_UTMZ5*CHM_EBC_SNA_3_GT6_UTMZ5, data = df.rg.agg.acremax)
summary(mod.agg.acremax3)

mod.agg.acremax4 <- lm(log(kg_acre) ~ (CHM_EBC_SNA_3_UTMZ5), data = CHM_DF.acremax)
summary(mod.agg.acremax4)

#################################################################################################################
###Simple linear reggression doesnt seem to work. I am going to run a new PCA with the scenes that 
##cover both SNA and EBC, then along with the mean height and max height layers I will run a random forest
#and see if i can get anyhting better using another random forest model
##################################################################################################

Kenai_Boundary <- readOGR(dsn = "../Kenai_Regression/Kenai_Boundary.shp")


path <- "../Kenai_Regression/LandSat_Both/"
file.names <- dir(path, pattern = ".TIF", ignore.case = T)
out.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names)){
  Rast_i <- raster(paste0("../Kenai_Regression/LandSat_Both/",file.names[i]))
  crop_i <- crop(Rast_i, Kenai_Boundary)
  mask_i <- mask(crop_i, Kenai_Boundary)
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

PCA_Ras_10<- rasterPCA(out.file, nSamples = NULL, nComp = 10, spca = TRUE, maskCheck = TRUE,  "../Kenai_Regression/LandSat_Both/Results/PCA_PC10_all_11Oct18.tif", format = "GTiff")
summary(PCA_Ras_10$model)
plot(PCA_Ras_10$map)

####Use ARCGIS to seperate teh PCA results into the different components 
##MANIPULATE THE CHM FILES THE SAME AS YOU DID TO THE DTM AND DSM FILES. 
##ONCE AGAIN THIS ONLY NEEDS TO BE DONE ONCE

CHM <- raster("../Earth Explorer Downloads/CHM/CHM_SNA_EBC_UTMZ5_30M.tif")
PC1 <- raster("../Kenai_Regression/Raster_RF_Regression/PCA_PC10_all_11Oct18_PC1.tif")
CHM <- resample(CHM, PC1)

writeRaster(CHM, "../Kenai_Regression/Raster_RF_Regression/CHM_SNA_EBC_UTMZ5_30M.tif", format = "GTiff", overwrite = TRUE)

path.Rando <- "../Kenai_Regression/Raster_RF_Regression/"
file.names.rando <- dir(path.Rando, pattern = ".TIF", ignore.case = T)
RandFor.file <- stack()

start <- Sys.time()

for (i in 1:length(file.names.rando)){
  Rast_i <- raster(paste0("../Kenai_Regression/Raster_RF_Regression/",file.names.rando[i]))
  crop_i <- crop(Rast_i, Kenai_Boundary)
  mask_i <- mask(crop_i, Kenai_Boundary)
  RandFor.file <- stack(RandFor.file, mask_i)
}

end <- Sys.time()
tot.t <- end - start
tot.t ##Time difference should be about 7.8 min
###Make sure all the rasters made it through the stack
nlayers(RandFor.file)

plots_all <- spRbind(plot_AGB_EBC, plot_AGB_SNA)
plot(plots_all)

Rando_Forest_PCA_Ras <- extract(RandFor.file, plots_all, small = TRUE, sp=TRUE)

df.RF <- as.data.frame(Rando_Forest_PCA_Ras)
df.RF.short <- df.RF[-c(1, 3:5,25:26)]

write.csv(df.RF.short, "../Kenai_Regression/RandFor_DF.csv")
writeRaster(RandFor.file, "../Kenai_Regression/RandFor_Raster.tif", format = "GTiff")
###writeOGR(plots_all, "../Kenai_regression/all_plots_EBC_SNA.shp", format = "ESRI")
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

####Check that this isnt better especially with adding PG plots to test

model_rf_new <- randomForest(kg_dry_total~ ., data = df.RF.short, importantance = TRUE, nodesize = 5)
model_rf_new
importance(model_rf_new)
varImpPlot(model_rf_new)
plot(model_rf_new)
varUsed(model_rf_new)

