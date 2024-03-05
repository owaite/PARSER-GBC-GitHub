## calulcating indcies with NIR shadow mask


library(terra)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
library(exactextractr)

## Ran on 2024-02-02 for Canoe

site = "Fdc_PR_Canoe" #"Cw_PR_Rainbow_SiteA"
#dir_crowns <- "D:\\Sync\\Sync\\Fdc_PR_Canoe\\CROWNS_done_reviewed_missing_added\\Data\\CROWNS\\Edited\\CHM_.06_masked_RGB_H20T_mosaic_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss.sig.01w3_z75_crowns_EDITED.shp"  #"K:\\Rainbow_A_Crowns\\Edited_as_of_2023_09_22_notDone\\Edited_CHM_.06_masked_RGB_H20T_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss.sig.01w3_z75_crowns_2_AbsIncluded.shp"  #pols_spat = st_read(paste0("K:\\Rainbow_B_Crowns\\Data\\Crowns\\Edited_Spencer\\CHM_.06_masked_RGB_H20T_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss.sig.01w3_z75_crowns_2_EDITED.shp")) %>%
dir_crowns <- "T:\\OliviaW\\BackUp_Canoe_Crowns\\CROWNS_done_reviewed_missing_added - Copy - sam segmentation on these\\Data\\CROWNS\\Edited\\Edited_added\\CHM_.06_masked_RGB_H20T_mosaic_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss.sig.01w3_z75_crowns_EDITED_NEWmissAdded.shp"
# reading in crown polygons
pols_read = st_read(paste0(dir_crowns), crs = 26910) %>% 
  filter(!st_is_empty(.)) 
#calculating area and adding a buffer
pols_buf <- pols_read %>%
  mutate(Area = st_area(pols_read)) %>%
  st_buffer(dist = -.05)
  #vect()
#calculating area of buffered crown
pols_spat <- pols_buf %>%
  mutate(Area_buffer5cm = st_area(pols_buf))

colnames(pols_spat)

plot(pols_spat)
class(pols_spat)

# c_or_m <- "_polyMask" ## getting nir from folder that used mask function for the crowns and not crop
# Nir_shadow_folder_baseName <- "NIR_shadow_mask"
# 
# Nir_shadow_folder <- paste0(Nir_shadow_folder_baseName, c_or_m)

Nir_shadow_folder <- "NIR_shadow_mask_localMinOrMax_polyCrop"

date_list <- c( "2022_04_23",
                 "2022_05_08",
                 "2022_05_27",
                 "2022_06_08",
                 "2022_06_23",# here and above have mean and median for
                "2022_07_06",
                "2022_07_22",
                "2022_07_27",
                "2022_08_05",
                "2022_08_24",
                "2022_09_08",
                "2022_09_20",
                "2022_10_05",
                "2022_10_20",
                 "2022_11_28",
                 "2023_02_17",
                 "2023_03_22",
                "2023_04_05",
                 "2023_04_26",
                "2023_05_10",
                "2023_05_24",
                "2023_06_07",
                "2023_06_21",
                "2023_08_17"
            )
                
date_list              

# date_list <- ("2022_05_08")

# pols_spat = st_read(paste0(dir_crowns)) %>% 
#   filter(!st_is_empty(.)) %>%
#   mutate(Area = st_area()) %>%
#   st_buffer(dist = -.05)%>%
#   mutate(Area_buffer5cm = st_area())

pols <- pols_spat

updated_metrics_folder <- "CSV_Index_NIR_Shadow_updated"

for (x in 1:length(date_list)){
  print(date_list[x])
  
  if (site %in% c("Cw_PR_Rainbow_SiteA", "Cw_PR_Rainbow_SiteB")){
    dir = paste0("K:\\",site,"\\Flights\\", date_list[x], "\\2_Inputs\\")
    dir_D = paste0("K:\\",site,"\\Flights\\", date_list[x])
  }
  if (site == "Fdc_PR_Canoe"){
    dir = paste0("D:\\Sync\\Sync\\",site,"\\Flights\\", date_list[x], "\\2_Inputs\\")
    dir_D = paste0("D:\\Sync\\Sync\\",site,"\\Flights\\", date_list[x])
  }
  
  print(dir)
  #ms_ortho
  ms_ortho = rast(list.files(paste0(dir, "metashape\\3_MS_ORTHO\\"), pattern = ".*MS_Calibrated.*_bestPanel.tif$", full.names = TRUE))
  ms_ortho_name = names(ms_ortho)[1]
  ms_ortho_name_root <- substr(ms_ortho_name, 1, nchar(ms_ortho_name) - 2)

  # ratio = ratios[x,]
  #nir shadow mask
  shadow_mask = rast(list.files(paste0(dir, Nir_shadow_folder,"\\"),  pattern = ".*_NIR_shadow.*_thresh.*_mask2.tif$", full.names = TRUE))## NIr shadow mask, uing the mask from the NIr of the crowns only (ie mask NOT crop)
  
  #mask NIR shadow to pols:
  shadow_mask <- terra::mask(shadow_mask, pols)
  
  #masking ms ortho by shadow mask
  ms_mask <- terra::mask(ms_ortho, shadow_mask, maskvalues = 1, updatevalue = NA)
  terra::writeRaster(ms_mask, paste0(dir, Nir_shadow_folder,"\\",ms_ortho_name_root,"_NirShadow_masked.tif"),
                     overwrite = TRUE)
  # ms_mask <- rast(paste0(dir, Nir_shadow_folder,"\\",ms_ortho_name_root,"_NirShadow_masked.tif"))
  ## mask raster to pols
  ms_mask <- terra::mask(ms_mask, pols)
  terra::writeRaster(ms_mask, paste0(dir, Nir_shadow_folder,"\\",ms_ortho_name_root,"_NirShadow_ms_mask_to_pols.tif"),
                     overwrite = TRUE)
  ms_mask <- rast(paste0(dir, Nir_shadow_folder,"\\",ms_ortho_name_root,"_NirShadow_ms_mask_to_pols.tif"))
  rast_list = list(# reflectance values
    R444 = ms_mask[[1]],
    R475 = ms_mask[[2]],
    R531 = ms_mask[[3]],
    R560 = ms_mask[[4]],               
    R650 = ms_mask[[5]],               
    R668 = ms_mask[[6]],               
    R705 = ms_mask[[7]],               
    R717 = ms_mask[[8]],               
    R740 = ms_mask[[9]],               
    R842 = ms_mask[[10]],                              
    
    #NIR greeness
    mDatt = (ms_mask[[10]] - ms_mask[[8]]) / (ms_mask[[10]] - ms_mask[[6]]),
    NDVI = (ms_mask[[10]] - ms_mask[[6]]) / (ms_mask[[10]] + ms_mask[[6]]), 
    
    # chlorophyll
    NIRv = ((ms_mask[[10]] - ms_mask[[6]]) / (ms_mask[[10]] + ms_mask[[6]])) * ms_mask[[10]],                          
    NDRE1 = (ms_mask[[10]] - ms_mask[[7]]) / (ms_mask[[10]] + ms_mask[[7]]),     
    NDRE2 = (ms_mask[[10]] - ms_mask[[8]]) / (ms_mask[[10]] + ms_mask[[8]]), 
    NDRE3 = (ms_mask[[10]] - ms_mask[[9]]) / (ms_mask[[10]] + ms_mask[[9]]), 
    EVI = (2.5 * (ms_mask[[10]] - ms_mask[[6]])) / (ms_mask[[10]] + (6 * ms_mask[[6]]) - ( 7.5 * ms_mask[[1]] + 1)),             
    # Gcc = ms_mask[[4]] / (ms_mask[[2]] + ms_mask[[4]] + ms_mask[[5]]), 
    GCC_Full = (ms_mask[[4]]+ ms_mask[[3]]) / (ms_mask[[1]] + ms_mask[[2]] + ms_mask[[3]]+ms_mask[[4]] + ms_mask[[5]] + ms_mask[[6]]),             
    BCC = (ms_mask[[1]] + ms_mask[[2]]) / (ms_mask[[1]]  + ms_mask[[2]] + ms_mask[[3]] + ms_mask[[4]] + ms_mask[[5]] + ms_mask[[6]]), 
    
    # carotenoids, waxes
    ARI = (1 / ms_mask[[4]]) - (1 / ms_mask[[7]]),    
    EWI9 = (ms_mask[[6]] - ms_mask[[8]]) / (ms_mask[[6]] + ms_mask[[8]]),
    
    # carotenoids             
    SIPI = (ms_mask[[10]] - ms_mask[[1]]) / (ms_mask[[10]] - ms_mask[[6]]), 
    SIPI_2 = (ms_mask[[10]] - ms_mask[[2]]) / (ms_mask[[10]] - ms_mask[[6]]),  
    PRI = (ms_mask[[3]] - ms_mask[[4]]) / (ms_mask[[3]] + ms_mask[[4]]),             
    CCI = (ms_mask[[3]] - ms_mask[[5]]) / (ms_mask[[3]] + ms_mask[[5]]),             
    
    # Red edge             
    RE_upper = (ms_mask[[9]] - ms_mask[[8]]) / 23,             
    RE_lower = (ms_mask[[8]] - ms_mask[[7]]) / 12,             
    RE_total = (ms_mask[[9]] - ms_mask[[7]]) / 35
  )
  
  rast_all = rast(rast_list)
  
  # # Mean Indices
  # print("calculating mean indices")
  # df_spectral_mean = exact_extract(rast_all, pols, fun = "mean", append_cols = c("treeID","Edited","tag__","rep","blk","fam","fem"))
  # print("calculating cell count")
  df_count = exact_extract(ms_mask[[1]], pols, fun = "count", progress = TRUE, append_cols = c("treeID","Edited","tag__","rep","blk","fam","fem"))
  # 
  # df_spectral_mean_count <- merge(df_spectral_mean,df_count, by = c("treeID","Edited","tag__","rep","blk","fam","fem"))
  #                                
  # if(!dir.exists(paste0(dir_D,"\\",updated_metrics_folder,"\\"))){ #creating CSV folders
  #   dir.create(paste0(dir_D,"\\",updated_metrics_folder,"\\"))}
  # print("calculated indices")
  # saveRDS(df_spectral_mean_count, paste0(dir_D,"\\",updated_metrics_folder,"\\", date_list[x], "_NIRshadowMask_MeanCrownSpectralIndices.rds"))
  # 
  #Median Indices
  print("calculating median indices")
  df_spectral_median = exact_extract(rast_all, pols, fun = "median", append_cols = c("treeID","Edited","tag__","rep","blk","fam","fem"))
  df_spectral_median_count <- merge(df_spectral_median,df_count, by = c("treeID","Edited","tag__","rep","blk","fam","fem"))

  if(!dir.exists(paste0(dir_D,"\\",updated_metrics_folder,"\\"))){ #creating CSV folders
    dir.create(paste0(dir_D,"\\",updated_metrics_folder,"\\"))}
  print("calculated indices")
  saveRDS(df_spectral_median_count, paste0(dir_D,"\\",updated_metrics_folder,"\\", date_list[x], "_NIRshadowMask_MedianCrownSpectralIndices.rds"))

}


### adding them all to one, for median first:

for (i in 1:length(date_list)){
  print(date_list[i])

  file <- readRDS(paste0("D:\\Sync\\Sync\\Fdc_PR_Canoe\\Flights\\",date_list[i],"\\",updated_metrics_folder,"\\", date_list[i], "_NIRshadowMask_MeanCrownSpectralIndices.rds"))%>%
    as.data.frame()%>%
    mutate(Date = ymd(date_list[i]))
  
  if(i ==1){
    df_to_join <- as.data.frame(file)
  }else{
    df_to_join <- rbind(df_to_join,file)
  }
}

saveRDS(df_to_join, paste0("D:\\Sync\\Sync\\Fdc_PR_Canoe\\Flights\\AllDates_NIRshadowMask_MeanCrownSpectralIndices.rds"))

unique(df_to_join$Date)
