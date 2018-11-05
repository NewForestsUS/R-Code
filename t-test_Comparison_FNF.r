###Test to see if ELC method for forest nonforest will actually bring the credit yeild up

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

plots_SNA_TV <- readOGR("../FVS QAQC/shapefiles/SNA_Completed_Plots_UTMZ5_Strata2.shp")
plots_EBC_TV <- readOGR("../FVS QAQC/shapefiles/EBC COMPLETED PLOTS UTMZ5 STRATA2.shp")
plots_SNA_ELC <- readOGR("../Earth Explorer Downloads/Analysis_SNA_Strata_CC6_NewTC/SNA_Strata_PCA_RF_FNFCC6_NewTC_inTVSF_PlotsMeas.shp")
plots_EBC_ELC <- readOGR("../Earth Explorer Downloads/Analysis_EBC_Strata_CC6_NewTC/Strata_PCA_RF_FNFCC6F_NewTC_PlotsMeas.shp")
plots_EBC_15ft_ELC <- readOGR("../Canopy_Cover/EBC_CanopyCover/EBC_Completed_Plots_CC15ft_UTMZ5_2.shp")
plots_EBC_7_ELC <- readOGR("../Canopy_Cover/EBC_CanopyCover/EBC_Completed_Plots_CC7_UTMZ5_2.shp")
plots_SNA_15ft_ELC <- readOGR("../Canopy_Cover/SNA_CanopyCover/SNA_Completed_Plots_CC15ft_UTMZ5.shp")
plots_SNA_7_ELC <- readOGR("../Canopy_Cover/SNA_CanopyCover/SNA_Completed_Plots_CC7_UTMZ5_2.shp")


####Now the data that corresponds with the plot info
##EBC
con_EBC <- dbConnect(RSQLite::SQLite(), "../../../New Forests/Forest Carbon Partners - nwkNanwalekEBC_analysis/nwkNanwalekEBC_fvs/EBC_V0.13_FVS_Output/result.db")     
con_EBC
alltables_EBC = dbListTables(con_EBC)
alltables_EBC

#Extract the TreeInIt file and the lost of plot names from the PlotInIt file
##Take a look at them to see if they make sense 
OnsiteEBC <- dbGetQuery(con_EBC, 'select * from onsiteCarbon')
head(OnsiteEBC)

dbDisconnect(con_EBC)
Grow_EBC <- subset.data.frame(OnsiteEBC, OnsiteEBC$RX == "NF_EBC_GROW_8_17_18")
Grow_EBC <- subset.data.frame(Grow_EBC, Grow_EBC$Year == 2018)
Grow_EBC
length(Grow_EBC$StandID)

##SNA
con_SNA <- dbConnect(RSQLite::SQLite(), "../../../New Forests/Forest Carbon Partners - FCP Seldovia/seldovia_ak_analysis/seldovia_ak_fvs/SNA_V0.05/result.db")     
con_SNA
alltables_SNA = dbListTables(con_SNA)
alltables_SNA

#Extract the TreeInIt file and the lost of plot names from the PlotInIt file
##Take a look at them to see if they make sense 
OnsiteSNA <- dbGetQuery(con_SNA, 'select * from onsiteCarbon')
head(OnsiteSNA)

dbDisconnect(con_SNA)
Grow_SNA <- subset.data.frame(OnsiteSNA, OnsiteSNA$RX == "NF_SNA_GROW")
Grow_SNA <- subset.data.frame(Grow_SNA, Grow_SNA$Year == 2018)
Grow_SNA
length(Grow_SNA$StandID)

###Now combine the AGCO2 to the plot locations
##EBC
plots_EBC_TV_short <- plots_EBC_TV[-c(2:44)]
plot_TV_EBC <-  merge(plots_EBC_TV_short, Grow_EBC, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_TV_EBC <- plot_TV_EBC[-c(2,3)]
##SNA
plots_SNA_TV_short <- plots_SNA_TV[-c(2:44)]
plot_TV_SNA <-  merge(plots_SNA_TV_short, Grow_SNA, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_TV_SNA <- plot_TV_SNA[-c(2,3)]
##EBC ELC 6m
plots_EBC_ELC_short <- plots_EBC_ELC[-c(1, 3:41)]
plot_ELC_EBC <-  merge(plots_EBC_ELC_short, Grow_EBC, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_ELC_EBC <- plot_ELC_EBC[-c(2,3)]
##SNA ELC 6m
plots_SNA_ELC_short <- plots_SNA_ELC[-c(1, 3:41)]
plot_ELC_SNA <-  merge(plots_SNA_ELC_short, Grow_SNA, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_ELC_SNA <- plot_ELC_SNA[-c(2,3)]
##EBC ELC 15ft
plots_EBC_15ft_ELC_short <- plots_EBC_15ft_ELC[-c(2:36)]
plot_ELC_EBC_15ft <-  merge(plots_EBC_15ft_ELC_short, Grow_EBC, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_ELC_EBC_15ft <- plot_ELC_EBC_15ft[-c(2,3)]
##EBC ELC 7m
plots_EBC_7_ELC_short <- plots_EBC_7_ELC[-c(2:36)]
plot_ELC_EBC_7 <-  merge(plots_EBC_7_ELC_short, Grow_EBC, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_ELC_EBC_7 <- plot_ELC_EBC_7[-c(2,3)]
##SNA ELC 15ft
plots_SNA_15ft_ELC_short <- plots_SNA_15ft_ELC[-c(2:36)]
plot_ELC_SNA_15ft <-  merge(plots_SNA_15ft_ELC_short, Grow_SNA, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_ELC_SNA_15ft <- plot_ELC_SNA_15ft[-c(2,3)]
##SNA ELC 7m
plots_SNA_7_ELC_short <- plots_SNA_7_ELC[-c(2:36)]
plot_ELC_SNA_7 <-  merge(plots_SNA_7_ELC_short, Grow_SNA, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_ELC_SNA_7 <- plot_ELC_SNA_7[-c(2,3)]

####Run some T-tests...
EBC_Ttest <- t.test(plot_ELC_EBC$LIVE_AG_CO2, plot_TV_EBC$LIVE_AG_CO2)
EBC_Ttest
boxplot(plot_ELC_EBC$LIVE_AG_CO2, plot_TV_EBC$LIVE_AG_CO2)
(mean(plot_ELC_EBC$LIVE_AG_CO2))
(cv(plot_ELC_EBC$LIVE_AG_CO2))
(sd(plot_ELC_EBC$LIVE_AG_CO2))
(length(plot_ELC_EBC$LIVE_AG_CO2))

(mean(plot_TV_EBC$LIVE_AG_CO2))
(cv(plot_TV_EBC$LIVE_AG_CO2))
(sd(plot_TV_EBC$LIVE_AG_CO2))
(length(plot_TV_EBC$LIVE_AG_CO2))

EBC_Ttest_15ft <- t.test(plot_ELC_EBC_15ft$LIVE_AG_CO2, plot_TV_EBC$LIVE_AG_CO2)
EBC_Ttest_15ft
boxplot(plot_ELC_EBC_15ft$LIVE_AG_CO2, plot_TV_EBC$LIVE_AG_CO2)
(mean(plot_ELC_EBC_15ft$LIVE_AG_CO2))
(cv(plot_ELC_EBC_15ft$LIVE_AG_CO2))
(sd(plot_ELC_EBC_15ft$LIVE_AG_CO2))
(length(plot_ELC_EBC_15ft$LIVE_AG_CO2))

EBC_Ttest_7 <- t.test(plot_ELC_EBC_7$LIVE_AG_CO2, plot_TV_EBC$LIVE_AG_CO2)
EBC_Ttest_7
boxplot(plot_ELC_EBC_7$LIVE_AG_CO2, plot_TV_EBC$LIVE_AG_CO2)
(mean(plot_ELC_EBC_7$LIVE_AG_CO2))
(cv(plot_ELC_EBC_7$LIVE_AG_CO2))
(sd(plot_ELC_EBC_7$LIVE_AG_CO2))
(length(plot_ELC_EBC_7$LIVE_AG_CO2))

#####SNA T-tests
SNA_Ttest <- t.test(plot_ELC_SNA$LIVE_AG_CO2, plot_TV_SNA$LIVE_AG_CO2)
SNA_Ttest
boxplot(plot_ELC_SNA$LIVE_AG_CO2, plot_TV_SNA$LIVE_AG_CO2)
(mean(plot_ELC_SNA$LIVE_AG_CO2))
(cv(plot_ELC_SNA$LIVE_AG_CO2))
(sd(plot_ELC_SNA$LIVE_AG_CO2))
(length(plot_ELC_SNA$LIVE_AG_CO2))

(mean(plot_TV_SNA$LIVE_AG_CO2))
(cv(plot_TV_SNA$LIVE_AG_CO2))
(sd(plot_TV_SNA$LIVE_AG_CO2))
(length(plot_TV_SNA$LIVE_AG_CO2))

SNA_Ttest_15ft <- t.test(plot_ELC_SNA_15ft$LIVE_AG_CO2, plot_TV_SNA$LIVE_AG_CO2)
SNA_Ttest_15ft
boxplot(plot_ELC_SNA_15ft$LIVE_AG_CO2, plot_TV_SNA$LIVE_AG_CO2)
(mean(plot_ELC_SNA_15ft$LIVE_AG_CO2))
(cv(plot_ELC_SNA_15ft$LIVE_AG_CO2))
(sd(plot_ELC_SNA_15ft$LIVE_AG_CO2))
(length(plot_ELC_SNA_15ft$LIVE_AG_CO2))

SNA_Ttest_7 <- t.test(plot_ELC_SNA_7$LIVE_AG_CO2, plot_TV_SNA$LIVE_AG_CO2)
SNA_Ttest_7
boxplot(plot_ELC_SNA_7$LIVE_AG_CO2, plot_TV_SNA$LIVE_AG_CO2)
(mean(plot_ELC_SNA_7$LIVE_AG_CO2))
(cv(plot_ELC_SNA_7$LIVE_AG_CO2))
(sd(plot_ELC_SNA_7$LIVE_AG_CO2))
(length(plot_ELC_SNA_7$LIVE_AG_CO2))

###Get the average loading for each strata for ELC sample frame
##EBC ELC
plots_EBC_ELC_Strata <- plots_EBC_ELC[-c(1, 3:38, 40:41)]
plot_ELC_EBC_Strata <-  merge(plots_EBC_ELC_Strata, Grow_EBC, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_ELC_EBC_Strata <- plot_ELC_EBC_Strata[-c(3,4)]

plot_ELC_EBC_Strata_x <- as.data.frame(plot_ELC_EBC_Strata)
plot_ELC_EBC_Strata_1 <- subset.data.frame(plot_ELC_EBC_Strata_x, plot_ELC_EBC_Strata_x$DN== 1)
plot_ELC_EBC_Strata_2 <- subset.data.frame(plot_ELC_EBC_Strata_x, plot_ELC_EBC_Strata_x$DN== 2)
plot_ELC_EBC_Strata_3 <- subset.data.frame(plot_ELC_EBC_Strata_x, plot_ELC_EBC_Strata_x$DN== 3)
plot_ELC_EBC_Strata_4 <- subset.data.frame(plot_ELC_EBC_Strata_x, plot_ELC_EBC_Strata_x$DN== 4)
plot_ELC_EBC_Strata_5 <- subset.data.frame(plot_ELC_EBC_Strata_x, plot_ELC_EBC_Strata_x$DN== 5)

(mean(plot_ELC_EBC_Strata_1$LIVE_AG_CO2))
(cv(plot_ELC_EBC_Strata_1$LIVE_AG_CO2))
(length(plot_ELC_EBC_Strata_1$LIVE_AG_CO2))

(mean(plot_ELC_EBC_Strata_2$LIVE_AG_CO2))
(cv(plot_ELC_EBC_Strata_2$LIVE_AG_CO2))
(length(plot_ELC_EBC_Strata_2$LIVE_AG_CO2))

(mean(plot_ELC_EBC_Strata_3$LIVE_AG_CO2))
(cv(plot_ELC_EBC_Strata_3$LIVE_AG_CO2))
(length(plot_ELC_EBC_Strata_3$LIVE_AG_CO2))

(mean(plot_ELC_EBC_Strata_4$LIVE_AG_CO2))
(cv(plot_ELC_EBC_Strata_4$LIVE_AG_CO2))
(length(plot_ELC_EBC_Strata_4$LIVE_AG_CO2))

(mean(plot_ELC_EBC_Strata_5$LIVE_AG_CO2))
(cv(plot_ELC_EBC_Strata_5$LIVE_AG_CO2))
(length(plot_ELC_EBC_Strata_5$LIVE_AG_CO2))
##SNA ELC
plots_SNA_ELC_Strata <- plots_SNA_ELC[-c(1, 3:38, 40:41)]
plot_ELC_SNA_Strata <-  merge(plots_SNA_ELC_Strata, Grow_SNA, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_ELC_SNA_Strata <- plot_ELC_SNA_Strata[-c(3,4)]

plot_ELC_SNA_Strata_x <- as.data.frame(plot_ELC_SNA_Strata)
plot_ELC_SNA_Strata_1 <- subset.data.frame(plot_ELC_SNA_Strata_x, plot_ELC_SNA_Strata_x$DN== 1)
plot_ELC_SNA_Strata_2 <- subset.data.frame(plot_ELC_SNA_Strata_x, plot_ELC_SNA_Strata_x$DN== 2)
plot_ELC_SNA_Strata_3 <- subset.data.frame(plot_ELC_SNA_Strata_x, plot_ELC_SNA_Strata_x$DN== 3)
plot_ELC_SNA_Strata_4 <- subset.data.frame(plot_ELC_SNA_Strata_x, plot_ELC_SNA_Strata_x$DN== 4)
plot_ELC_SNA_Strata_5 <- subset.data.frame(plot_ELC_SNA_Strata_x, plot_ELC_SNA_Strata_x$DN== 5)

(mean(plot_ELC_SNA_Strata_1$LIVE_AG_CO2))
(cv(plot_ELC_SNA_Strata_1$LIVE_AG_CO2))
(length(plot_ELC_SNA_Strata_1$LIVE_AG_CO2))

(mean(plot_ELC_SNA_Strata_2$LIVE_AG_CO2))
(cv(plot_ELC_SNA_Strata_2$LIVE_AG_CO2))
(length(plot_ELC_SNA_Strata_2$LIVE_AG_CO2))

(mean(plot_ELC_SNA_Strata_3$LIVE_AG_CO2))
(cv(plot_ELC_SNA_Strata_3$LIVE_AG_CO2))
(length(plot_ELC_SNA_Strata_3$LIVE_AG_CO2))

(mean(plot_ELC_SNA_Strata_4$LIVE_AG_CO2))
(cv(plot_ELC_SNA_Strata_4$LIVE_AG_CO2))
(length(plot_ELC_SNA_Strata_4$LIVE_AG_CO2))

(mean(plot_ELC_SNA_Strata_5$LIVE_AG_CO2))
(cv(plot_ELC_SNA_Strata_5$LIVE_AG_CO2))
(length(plot_ELC_SNA_Strata_5$LIVE_AG_CO2))

##EBC
plots_EBC_TV_Strata <- plots_EBC_TV[-c(2:38, 40:44)]
plot_TV_EBC_Strata <-  merge(plots_EBC_TV_Strata, Grow_EBC, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_TV_EBC_Strata <- plot_TV_EBC_Strata[-c(3,4)]

plot_TV_EBC_Strata_x <- as.data.frame(plot_TV_EBC_Strata)
plot_TV_EBC_Strata_NF <- subset.data.frame(plot_TV_EBC_Strata_x, plot_TV_EBC_Strata_x$BUCKET == "NF")
plot_TV_EBC_Strata_RP <- subset.data.frame(plot_TV_EBC_Strata_x, plot_TV_EBC_Strata_x$BUCKET == "RP")
plot_TV_EBC_Strata_SS <- subset.data.frame(plot_TV_EBC_Strata_x, plot_TV_EBC_Strata_x$BUCKET == "SS")
plot_TV_EBC_Strata_SD <- subset.data.frame(plot_TV_EBC_Strata_x, plot_TV_EBC_Strata_x$BUCKET == "SD")
plot_TV_EBC_Strata_LS <- subset.data.frame(plot_TV_EBC_Strata_x, plot_TV_EBC_Strata_x$BUCKET == "LS")
plot_TV_EBC_Strata_LD <- subset.data.frame(plot_TV_EBC_Strata_x, plot_TV_EBC_Strata_x$BUCKET == "LD")

(mean(plot_TV_EBC_Strata_NF$LIVE_AG_CO2))
(cv(plot_TV_EBC_Strata_NF$LIVE_AG_CO2))
(length(plot_TV_EBC_Strata_NF$LIVE_AG_CO2))
(range(plot_TV_EBC_Strata_NF$LIVE_AG_CO2))

(mean(plot_TV_EBC_Strata_RP$LIVE_AG_CO2))
(cv(plot_TV_EBC_Strata_RP$LIVE_AG_CO2))
(length(plot_TV_EBC_Strata_RP$LIVE_AG_CO2))
(range(plot_TV_EBC_Strata_RP$LIVE_AG_CO2))

(mean(plot_TV_EBC_Strata_SS$LIVE_AG_CO2))
(cv(plot_TV_EBC_Strata_SS$LIVE_AG_CO2))
(length(plot_TV_EBC_Strata_SS$LIVE_AG_CO2))
(range(plot_TV_EBC_Strata_SS$LIVE_AG_CO2))

(mean(plot_TV_EBC_Strata_SD$LIVE_AG_CO2))
(cv(plot_TV_EBC_Strata_SD$LIVE_AG_CO2))
(length(plot_TV_EBC_Strata_SD$LIVE_AG_CO2))
(range(plot_TV_EBC_Strata_SD$LIVE_AG_CO2))

(mean(plot_TV_EBC_Strata_LS$LIVE_AG_CO2))
(cv(plot_TV_EBC_Strata_LS$LIVE_AG_CO2))
(length(plot_TV_EBC_Strata_LS$LIVE_AG_CO2))
(range(plot_TV_EBC_Strata_LS$LIVE_AG_CO2))

(mean(plot_TV_EBC_Strata_LD$LIVE_AG_CO2))
(cv(plot_TV_EBC_Strata_LD$LIVE_AG_CO2))
(length(plot_TV_EBC_Strata_LD$LIVE_AG_CO2))
(range(plot_TV_EBC_Strata_LD$LIVE_AG_CO2))

##SNA
plots_SNA_TV_Strata <- plots_SNA_TV[-c(2:36, 38:44)]
plot_TV_SNA_Strata <-  merge(plots_SNA_TV_Strata, Grow_SNA, by.x = "ID", by.y = "StandID", all.x = FALSE)
plot_TV_SNA_Strata <- plot_TV_SNA_Strata[-c(3,4)]

plot_TV_SNA_Strata_x <- as.data.frame(plot_TV_SNA_Strata)

plot_TV_SNA_Strata_RP <- subset.data.frame(plot_TV_SNA_Strata_x, plot_TV_SNA_Strata_x$BUCKET == "RP")
plot_TV_SNA_Strata_SS <- subset.data.frame(plot_TV_SNA_Strata_x, plot_TV_SNA_Strata_x$BUCKET == "SS")
plot_TV_SNA_Strata_SD <- subset.data.frame(plot_TV_SNA_Strata_x, plot_TV_SNA_Strata_x$BUCKET == "SD")
plot_TV_SNA_Strata_LS <- subset.data.frame(plot_TV_SNA_Strata_x, plot_TV_SNA_Strata_x$BUCKET == "LS")
plot_TV_SNA_Strata_LD <- subset.data.frame(plot_TV_SNA_Strata_x, plot_TV_SNA_Strata_x$BUCKET == "LD")

(mean(plot_TV_SNA_Strata_RP$LIVE_AG_CO2))
(cv(plot_TV_SNA_Strata_RP$LIVE_AG_CO2))
(length(plot_TV_SNA_Strata_RP$LIVE_AG_CO2))
(range(plot_TV_SNA_Strata_RP$LIVE_AG_CO2))

(mean(plot_TV_SNA_Strata_SS$LIVE_AG_CO2))
(cv(plot_TV_SNA_Strata_SS$LIVE_AG_CO2))
(length(plot_TV_SNA_Strata_SS$LIVE_AG_CO2))
(range(plot_TV_SNA_Strata_SS$LIVE_AG_CO2))

(mean(plot_TV_SNA_Strata_SD$LIVE_AG_CO2))
(cv(plot_TV_SNA_Strata_SD$LIVE_AG_CO2))
(length(plot_TV_SNA_Strata_SD$LIVE_AG_CO2))
(range(plot_TV_SNA_Strata_SD$LIVE_AG_CO2))

(mean(plot_TV_SNA_Strata_LS$LIVE_AG_CO2))
(cv(plot_TV_SNA_Strata_LS$LIVE_AG_CO2))
(length(plot_TV_SNA_Strata_LS$LIVE_AG_CO2))
(range(plot_TV_SNA_Strata_LS$LIVE_AG_CO2))

(mean(plot_TV_SNA_Strata_LD$LIVE_AG_CO2))
(cv(plot_TV_SNA_Strata_LD$LIVE_AG_CO2))
(length(plot_TV_SNA_Strata_LD$LIVE_AG_CO2))
(range(plot_TV_SNA_Strata_LD$LIVE_AG_CO2))

