library(sp)
library(raster)
library(maptools)
library(rgdal)
library(fields)
library(dplyr)
library(odbc)
library(DBI)
library(RSQLite)
library(rgeos)
library(RODBC)
library(excel.link)

##Open up the geodatabase to extract the layers you need
EBC_gdb <- "../EBC/EBC Strata/Kenai_03142018.gdb"

subset(ogrDrivers(), grepl("GDB", name))
ebc_list <- ogrListLayers(EBC_gdb)
print(ebc_list)

##Extract the vegitation (strata) layer and the completed plots
EBC_veg <- readOGR(dsn = EBC_gdb, layer = "EB_veg")
EBC_plots <- readOGR(dsn = "../FVS QAQC/shapefiles/EBC COMPLETED PLOTS.shp")

EBC_plots2 <- readOGR(dsn = "./EBC_Plots_CarbonQuant_TVS.shp")
EBC_plots3 <- readOGR(dsn = "./EBC_Plots_CarbonQuant_AboveCP_TVS.shp")

##Check that the extrations wroked and that they make sense
summary(EBC_veg)
head(EBC_veg@data)
head(EBC_plots@data)
##Get both shapefiles inot the same projection 
EBC_veg_project <- spTransform(EBC_veg, crs(EBC_plots))
##Graph to make sure that this all makes sense
plot(EBC_veg_project)
plot(EBC_plots, col = "red", pch = 19, cex = 0.25, add = TRUE)
#Write out a .shp of the strata to use in the correction projection
writeOGR(EBC_veg_project, "../FVS QAQC/shapefiles", "EBC_Strata_use", driver = "ESRI Shapefile")

##DO the above step for SNA project
SNA_veg <- readOGR(dsn = EBC_gdb, layer = "SEL_veg")
SNA_plots <- readOGR(dsn = "../FVS QAQC/shapefiles/SNA COMPLETED PLOTS.shp")

SNA_plots2 <- readOGR(dsn = "./SNA_Plots_CarbonQuant_TVS.shp")
SNA_plots3 <- readOGR(dsn = "./SNA_Plots_CarbonQuant_AboveCP_TVS.shp")

SNA_veg_project <- spTransform(SNA_veg, crs(SNA_plots))
plot(SNA_veg)
summary(SNA_veg)
head(SNA_veg@data)
plot(SNA_veg_project)
plot(SNA_plots, col = "red", pch = 19, cex = 0.25, add = TRUE)
writeOGR(SNA_veg_project, "../FVS QAQC/shapefiles", "SNA_Strata_use", driver = "ESRI Shapefile")

#####OK NOW WE HAVE THE STRATA AND THE PLOTS,
######LETS intersect the plots by the project area to get which strata the plots fall in

EBC_strataplots <- intersect(EBC_plots, EBC_veg_project)
##Plot to make sure things till make sense after the intersect
plot(EBC_veg_project)
plot(EBC_strataplots, add=T)
##Look at the top of the intersected layer to make sure you have all the attributes you want
head(EBC_strataplots)
#Create a dataframe of the data from the intersected shapefile
EBC_strata_df <- EBC_strataplots@data
EBC_strata_df <- EBC_plots2@data
EBC_strata_df <- EBC_plots3@data
##Check it to make sure it makes sense (should look very similar to head(EBC_strataplots))
head(EBC_strata_df)
##Create a list of the headers you want to keep so you can drop all the other columns 
keep <- c("ID", "BUCKET")
##Drop the columns that aren't necessary 
EBC_strata_df_usseful <- EBC_strata_df[,keep]

##Repeat for SNA Project
SNA_strataplots <- intersect(SNA_plots, SNA_veg_project)
head(SNA_strataplots)
SNA_strata_df <- SNA_strataplots@data
SNA_strata_df <- SNA_plots2@data
SNA_strata_df <- SNA_plots3@data

head(SNA_strata_df)
SNA_strata_df_usseful <- SNA_strata_df[,keep]

### Now, open the siteindex provided by Dan Tumenis. 
## This is found in the "Current Master" .xlsb document
##However for ease of acces we have saved it as a .csv file 
#the file from the .xlsb document that you want to save is "Site Trees" or some similar iteration 
EBC_Master <- read.csv("../data/EBC_Master_SiteTrees.csv")
head(EBC_Master)
EBC_Siteindex <- merge(EBC_Master, EBC_strata_df_usseful, by.x = "Plot..", by.y = "ID", all.y = TRUE, all.x = FALSE)

###This for loop will create a summary table of the averge Site Index of each Strata
##And the number of plots that fall within each strata
Summary.Strata <- NULL
for(i in unique(EBC_Siteindex$BUCKET)){
  BUCKET_i <- subset.data.frame(EBC_Siteindex, EBC_Siteindex$BUCKET == i)
  mean.strata <- mean(BUCKET_i$Site.Index, na.rm = TRUE)
  ListBUCKET <- as.character(droplevels(BUCKET_i$BUCKET))
  LengBucket <- length(BUCKET_i$Strata)
  test.i <- data.frame("Strata" = unique(ListBUCKET), "Site.Index" = unique(mean.strata),
                       "Number_Plots" = unique(LengBucket))
  Summary.Strata <- rbind(Summary.Strata,test.i)
}
Summary.Strata

###This for loop applies the average siteindex to each plot and shows the site index designation in the output
plot_SI <- NULL
for(i in unique(EBC_Siteindex$BUCKET)){
  BUCKET_i <- subset.data.frame(EBC_Siteindex, EBC_Siteindex$BUCKET == i)
  mean.strata <- mean(BUCKET_i$Site.Index, na.rm = TRUE)
  BUCKET_i$Site.Index[is.na(BUCKET_i$Site.Index)] <- mean.strata
  SIndex_i <- BUCKET_i$Site.Index
  ListBUCKET <- as.character(droplevels(BUCKET_i$BUCKET))
  plotID <- BUCKET_i$Plot..
  Site_Species <- BUCKET_i$S.T.SPP.
  test.i <- data.frame("Plot_ID" = plotID, "Strata" = ListBUCKET, "Site.Index" = SIndex_i)
  plot_SI <- rbind(plot_SI,test.i)
}
plot_SI

###This for loop creates the plot_SiteIndex tab to be entered into the FVS input database  
plot_SiteIndex <- NULL
for(i in unique(EBC_Siteindex$BUCKET)){
  BUCKET_i <- subset.data.frame(EBC_Siteindex, EBC_Siteindex$BUCKET == i)
  mean.strata <- mean(BUCKET_i$Site.Index, na.rm = TRUE)
  BUCKET_i$Site.Index[is.na(BUCKET_i$Site.Index)] <- mean.strata
  SIndex_i <- BUCKET_i$Site.Index
  ListBUCKET <- as.character(droplevels(BUCKET_i$BUCKET))
  plotID <- BUCKET_i$Plot..
  StandID <- BUCKET_i$Plot..
  Site_Species <- BUCKET_i$S.T.SPP.
  Location <- 1004
  index <- 0:length(BUCKET_i$Plot..)
  test.i <- data.frame("Stand ID" = StandID, "Plot ID" = plotID, "Site.Index" = SIndex_i)
  plot_SiteIndex <- rbind(plot_SiteIndex,test.i)
}
plot_SiteIndex

###After exporting you will need to add the location and site species to the exported list. 
##The index column will be created as you export to a .csv you will need to open the .csv and name the first column "id"

write.csv(plot_SiteIndex, "../data/EBC_Plot_SiteIndex_30Aug19.csv")
write.csv(plot_SI, "../data/EBC_Plot_Strata_AboveCP_24Oct18.csv")

#####SNA
SNA_Master <- read.csv("../data/SNA_Master_SiteTrees.csv")
head(SNA_Master)
SNA_Siteindex <- merge(SNA_Master, SNA_strata_df_usseful, by.x = "Plot..", by.y = "ID", all.y = TRUE, all.x = FALSE)

###This for loop will create a summary table of the averge Site Index of each Strata
##And the number of plots that fall within each strata
Summary.Strata <- NULL
for(i in unique(SNA_Siteindex$BUCKET)){
  BUCKET_i <- subset.data.frame(SNA_Siteindex, SNA_Siteindex$BUCKET == i)
  mean.strata <- mean(BUCKET_i$Site.Index, na.rm = TRUE)
  ListBUCKET <- as.character(droplevels(BUCKET_i$BUCKET))
  LengBucket <- length(BUCKET_i$Strata)
  test.i <- data.frame("Strata" = unique(ListBUCKET), "Site.Index" = unique(mean.strata),
                       "Number_Plots" = unique(LengBucket))
  Summary.Strata <- rbind(Summary.Strata,test.i)
}
Summary.Strata

###This for loop applies the average siteindex to each plot and shows the site index designation in the output
plot_SI <- NULL
for(i in unique(SNA_Siteindex$BUCKET)){
  BUCKET_i <- subset.data.frame(SNA_Siteindex, SNA_Siteindex$BUCKET == i)
  mean.strata <- mean(BUCKET_i$Site.Index, na.rm = TRUE)
  BUCKET_i$Site.Index[is.na(BUCKET_i$Site.Index)] <- mean.strata
  SIndex_i <- BUCKET_i$Site.Index
  ListBUCKET <- as.character(droplevels(BUCKET_i$BUCKET))
  plotID <- BUCKET_i$Plot..
  Site_Species <- BUCKET_i$S.T.SPP.
  test.i <- data.frame("Plot_ID" = plotID, "Strata" = ListBUCKET, "Site.Index" = SIndex_i)
  plot_SI <- rbind(plot_SI,test.i)
}
plot_SI

###This for loop creates the plot_SiteIndex tab to be entered into the FVS input database  
plot_SiteIndex <- NULL
for(i in unique(SNA_Siteindex$BUCKET)){
  BUCKET_i <- subset.data.frame(SNA_Siteindex, SNA_Siteindex$BUCKET == i)
  mean.strata <- mean(BUCKET_i$Site.Index, na.rm = TRUE)
  BUCKET_i$Site.Index[is.na(BUCKET_i$Site.Index)] <- mean.strata
  SIndex_i <- BUCKET_i$Site.Index
  ListBUCKET <- as.character(droplevels(BUCKET_i$BUCKET))
  plotID <- BUCKET_i$Plot..
  StandID <- BUCKET_i$Plot..
  Site_Species <- BUCKET_i$S.T.SPP.
  Location <- 1004
  index <- 0:length(BUCKET_i$Plot..)
  test.i <- data.frame("Stand ID" = StandID, "Plot ID" = plotID, "Site.Index" = SIndex_i)
  plot_SiteIndex <- rbind(plot_SiteIndex,test.i)
}
plot_SiteIndex

###After exporting you will need to add the location and site species to the exported list. 
##The index column will be created as you export to a .csv you will need to open the .csv and name the first column "id"

write.csv(plot_SiteIndex, "../data/SNA_Plot_SiteIndex_30Aug19.csv")
write.csv(plot_SI, "../data/SNA_Plot_Strata_TVS_AboveCP_24Oct18_PlotsOnlyAboceCP.csv")

