library(sp)
library(raster)
library(maptools)
library(rgdal)
library(fields)
library(plyr)
library(dplyr)
library(odbc)
library(DBI)
library(RSQLite)
library(caret)
library(corrplot)
library(gdalUtils)
library(RPostgres)
library(RPostgreSQL)
library(readxl)
library(postGIStools)
library(dbplyr)

AOI <- readOGR("../Fossil_Project/Harvest_Areas_Kinz_5070.shp")
LEMMA_Ras <- raster("../LEMMA/LEMMA_Layer_5070.tif")

plot(LEMMA_Ras)
plot(AOI, add = T)

pixel_Value <- read.csv("../LEMMA/SPPSZ_ATTR_LIVE.csv")
meta_codes <- read.csv("../LEMMA/Metadata_codes.csv")
meta_fields <- read.csv("../LEMMA/Metadata_fields.csv")
pixel_dead <- read.csv("../LEMMA/SPPSZ_ATTR_DEAD.csv")


AOI_LEMMA_crop <- crop(LEMMA_Ras, AOI)
AOI_LEMMA <- mask(AOI_LEMMA_crop, AOI)
plot(AOI_LEMMA)
plot(AOI)

pix <- select(pixel_Value, "BPH_GE_3_CRM", "BPH_GE_3_JENK", "VPH_GE_3", "FORTYPBA",
              "FORTYPCOV", "FORTYPIV", "VEGCLASS", "VEGCLASSR","VALUE")
pix_dead <- select(pixel_dead, "SBPH_GE_25", "VPH_REMS", "VALUE")
AOI_LEMMA_df <- as.data.frame(AOI_LEMMA)
AOI_LEMMA_df <- na.omit(AOI_LEMMA_df)
AOI_pix <- merge.data.frame(AOI_LEMMA_df, pix, by.x = 'LEMMA_Layer_5070', by.y = "VALUE")
AOI_pix <- droplevels(AOI_pix)
AOI_pix_tree <- AOI_pix[AOI_pix$VEGCLASS != 1,]
AOI_pix_tree <- na.omit(AOI_pix_tree)
AOI_pix_tree <- droplevels(AOI_pix_tree)
AOI_pix_tree$LowCP <- 40.76
AOI_pix_tree$HighCP <- 64.98
write.csv(AOI_pix_tree, "../Fossil_Project/AOI_Pixel.csv")
CYE <- read.csv("../Fossil_Project/AOI_Pixel.csv")
CYE$Biomass_CRM_kgacr <- CYE$BPH_GE_3_CRM * 0.404686
CYE$BIomass_Jenk_hgacr <- CYE$BPH_GE_3_JENK * 0.404686
CYE$Biomass_CRM_mtacr <- CYE$Biomass_CRM_kgacr/1000
CYE$Biomass_JENK_mtacr <- CYE$BIomass_Jenk_hgacr/1000
CYE$CO2e_CRM_mtacr <- CYE$Biomass_CRM_mtacr * 1.8335
CYE$CO2e_JENK_mtacr <- CYE$Biomass_JENK_mtacr * 1.8335
CYE$lowCP_CRM <- CYE$CO2e_CRM_mtacr - CYE$LowCP
CYE$highCP_CRM <- CYE$CO2e_CRM_mtacr - CYE$HighCP
CYE$lowCP_Jenk <- CYE$CO2e_JENK_mtacr - CYE$LowCP
CYE$highCP_Jenk <- CYE$CO2e_JENK_mtacr - CYE$HighCP
CYE$Above_lowCP_CRM <- CYE$lowCP_CRM
CYE$Above_lowCP_CRM[CYE$Above_lowCP_CRM <= 0] <- NA
CYE$Above_highCP_CRM <- CYE$highCP_CRM
CYE$Above_highCP_CRM[CYE$Above_highCP_CRM <= 0] <- NA
CYE$Above_lowCP_Jenk <- CYE$lowCP_Jenk
CYE$Above_lowCP_Jenk[CYE$Above_lowCP_Jenk <= 0] <- NA
CYE$Above_highCP_Jenk <- CYE$highCP_Jenk
CYE$Above_highCP_Jenk[CYE$Above_highCP_Jenk <= 0] <- NA
###BElowground calculations
CYE$Biomass_CRM_Mgha <- CYE$BPH_GE_3_CRM /1000
CYE$BIomass_Jenk_Mgha <- CYE$BPH_GE_3_JENK / 1000
CYE$BelowGround_Biomass_CRM_Mgha <- exp(-1.085 + 0.9256 * log(CYE$Biomass_CRM_Mgha))
CYE$BelowGround_Biomass_Jenk_Mgha <- exp(-1.085 + 0.9256 * log(CYE$BIomass_Jenk_Mgha))
CYE$Ratio_Above_All_LowCP_Jenk <- CYE$Above_lowCP_Jenk/CYE$CO2e_JENK_mtacr
CYE$Ratio_Above_All_HighCP_Jenk <- CYE$Above_highCP_Jenk/CYE$CO2e_JENK_mtacr
CYE$Ratio_Above_All_LowCP_CRM <- CYE$Above_lowCP_CRM/CYE$CO2e_CRM_mtacr
CYE$Ratio_Above_All_HighCP_CRM <- CYE$Above_highCP_CRM/CYE$CO2e_CRM_mtacr
CYE$BelowGround_Biomass_CRM_Mgacr <- CYE$BelowGround_Biomass_CRM_Mgha * 0.404686
CYE$BelowGround_Biomass_Jenk_Mgacr <- CYE$BelowGround_Biomass_Jenk_Mgha * 0.404686
CYE$CO2e_CRM_BG_MGacr <- CYE$BelowGround_Biomass_CRM_Mgacr * 1.8335
CYE$CO2e_Jenk_BG_Mgacr <- CYE$BelowGround_Biomass_Jenk_Mgacr * 1.8335
CYE$CO2e_HighCP_CRM_BG_Mgacr_Ratio <- CYE$CO2e_CRM_BG_MGacr * CYE$Ratio_Above_All_HighCP_CRM
CYE$CO2e_lowCP_CRM_BG_Mgacr_Ratio <- CYE$CO2e_CRM_BG_MGacr * CYE$Ratio_Above_All_LowCP_CRM
CYE$CO2e_HighCP_Jenk_BG_Mgacr_Ratio <- CYE$CO2e_Jenk_BG_Mgacr * CYE$Ratio_Above_All_HighCP_Jenk
CYE$CO2e_lowCP_Jenk_BG_Mgacr_Ratio <- CYE$CO2e_Jenk_BG_Mgacr * CYE$Ratio_Above_All_LowCP_Jenk
####LETS ADD EVERYTHING TOGETHER And dont forget to multiply everything from 900m2 to get total
CYE_final <- select(CYE, "LEMMA_Layer_5070", "FORTYPBA", "FORTYPCOV", "FORTYPIV", "VEGCLASS", "VEGCLASSR",
                    "Above_lowCP_CRM", "Above_highCP_CRM", "Above_lowCP_Jenk", "Above_highCP_Jenk",
                    "CO2e_HighCP_CRM_BG_Mgacr_Ratio", "CO2e_lowCP_CRM_BG_Mgacr_Ratio",
                    "CO2e_HighCP_Jenk_BG_Mgacr_Ratio", "CO2e_lowCP_Jenk_BG_Mgacr_Ratio")
CYE_final$CRM_HighCP_AllLive <- CYE_final$Above_highCP_CRM + CYE_final$CO2e_HighCP_CRM_BG_Mgacr_Ratio
CYE_final$CRM_lowCP_AllLive <- CYE_final$Above_lowCP_CRM + CYE_final$CO2e_lowCP_CRM_BG_Mgacr_Ratio
CYE_final$Jenk_HighCP_AllLive <- CYE_final$Above_highCP_Jenk + CYE_final$CO2e_HighCP_Jenk_BG_Mgacr_Ratio
CYE_final$Jenk_lowCP_AllLive <- CYE_final$Above_lowCP_Jenk + CYE_final$CO2e_lowCP_Jenk_BG_Mgacr_Ratio
CYE_final$CRM_HighCP_Total <- CYE_final$CRM_HighCP_AllLive * 0.22239501
CYE_final$CRM_lowCP_Total <- CYE_final$CRM_lowCP_AllLive * 0.22239501
CYE_final$Jenk_HighCP_Total <- CYE_final$Jenk_HighCP_AllLive * 0.22239501
CYE_final$Jenk_lowCP_Total <- CYE_final$Jenk_lowCP_AllLive * 0.22239501

write.csv(CYE_final, "../Fossil_Project/CYE_Final.csv", row.names = FALSE)
