## calulcating indcies with NIR shadow mask


library(terra)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
library(exactextractr)

## Ran on 2024-02-02 for Canoe

site = "Cw_PR_Rainbow_SiteA"

# RB_crowns <-A:\WaiteOlivia_2\Rainbow_L1\pols_B

RA_crowns <- "A:\\WaiteOlivia_2\\Rainbow_L1\\2023_12_17_A\\05_ALIGNED_ALS\\pols\\Edited_CHM__06_masked_RGB_H20T_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss_sig_01w3_z75_crowns_2_AbsIncluded_ND.shp"
# reading in crown polygons
RA_pols_read = st_read(RA_crowns, crs = 26910) %>% 
  filter(!st_is_empty(.)) 

unique(sf::st_dimension(RA_pols_read))
#calculating area and adding a buffer
RA_pols_buf <- RA_pols_read %>%
  mutate(Area = st_area(RA_pols_read)) %>%
  st_buffer(dist = -.05)%>%
  filter(!st_is_empty(.)) 
  #vect()

unique(sf::st_dimension(RA_pols_buf))

#calculating area of buffered crown
RA_pols_spat <- RA_pols_buf %>%
  mutate(Area_buffer5cm = st_area(RA_pols_buf))

colnames(RA_pols_spat)
class(RA_pols_spat)

#name of folder with shadow masks
Nir_shadow_folder <- "NIR_shadow_mask_localMinOrMax_polyCrop"

date_list <- c( 
                "2022_04_22",
                 "2022_05_07",
                 "2022_05_26",
                 "2022_06_07",
                 "2022_06_22",
                "2022_07_05",
                "2022_07_21",
               "2022_07_28",
                "2022_08_04",
                "2022_08_23",
                "2022_09_07",
                "2022_09_19",
                "2022_10_04",
                "2022_10_19",
                 "2022_11_20",
                 "2023_01_25",
                 "2023_03_21",
                "2023_04_04",
                 "2023_04_25",
                "2023_05_11",
                "2023_05_23",
                "2023_06_06",
                "2023_06_22",
                "2023_08_18"
            )
                
date_list              

# date_list <- ("2022_05_08")

# pols_spat = st_read(paste0(dir_crowns)) %>% 
#   filter(!st_is_empty(.)) %>%
#   mutate(Area = st_area()) %>%
#   st_buffer(dist = -.05)%>%
#   mutate(Area_buffer5cm = st_area())

RA_pols <- RA_pols_spat
class(RA_pols)
updated_metrics_folder <- "Updated_CSV_Index_NIR_Shadow"

RA_pols <- RA_pols
RA_pols$tag = paste(RA_pols$ROW, RA_pols$COL, sep = "") #making tag for rainbow
RA_pols
unique(sf::st_dimension(RA_pols))
colnames(RA_pols)
for (x in 1:length(date_list)){
  print(date_list[x])
  dir <- paste0("F:\\Cw_PR_Rainbow_SiteA\\2_Processed_Work\\Flights\\",date_list[x],"\\2_Inputs\\")
  print(dir)
  dir_D <- paste0("F:\\Cw_PR_Rainbow_SiteA\\2_Processed_Work\\Flights\\",date_list[x])
  
  #ms_ortho
  ms_ortho = rast(list.files(paste0(dir, "metashape\\3_MS_ORTHO\\"), pattern = ".*MS_Calibrated.*_bestPanel.tif$", full.names = TRUE))
  ms_ortho_name = names(ms_ortho)[1]
  ms_ortho_name_root <- substr(ms_ortho_name, 1, nchar(ms_ortho_name) - 2)

  # ratio = ratios[x,]
  #nir shadow mask
  shadow_mask = rast(list.files(paste0(dir, Nir_shadow_folder,"\\"),  pattern = ".*_NIR_shadow.*_thresh.*_mask2.tif$", full.names = TRUE))## NIr shadow mask, uing the mask from the NIr of the crowns only (ie mask NOT crop)
  
  #mask NIR shadow to RA_pols:
  shadow_mask <- terra::mask(shadow_mask, RA_pols)
  
  #masking ms ortho by shadow mask
  ms_mask <- terra::mask(ms_ortho, shadow_mask, maskvalues = 1, updatevalue = NA)
  terra::writeRaster(ms_mask, paste0(dir, Nir_shadow_folder,"\\",ms_ortho_name_root,"_NirShadow_masked.tif"),
                     overwrite = TRUE)
  # ms_mask <- rast(paste0(dir, Nir_shadow_folder,"\\",ms_ortho_name_root,"_NirShadow_masked.tif"))
  ## mask raster to RA_pols
  ms_mask <- terra::mask(ms_mask, RA_pols)
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
    # mDatt = (ms_mask[[10]] - ms_mask[[8]]) / (ms_mask[[10]] - ms_mask[[6]]),
    # NDVI = (ms_mask[[10]] - ms_mask[[6]]) / (ms_mask[[10]] + ms_mask[[6]]), 
    # 
    # chlorophyll
    NIRv = ((ms_mask[[10]] - ms_mask[[6]]) / (ms_mask[[10]] + ms_mask[[6]])) * ms_mask[[10]],                          
    NDRE1 = (ms_mask[[10]] - ms_mask[[7]]) / (ms_mask[[10]] + ms_mask[[7]]),     
    NDRE2 = (ms_mask[[10]] - ms_mask[[8]]) / (ms_mask[[10]] + ms_mask[[8]]), 
    NDRE3 = (ms_mask[[10]] - ms_mask[[9]]) / (ms_mask[[10]] + ms_mask[[9]]), 
    # EVI = (2.5 * (ms_mask[[10]] - ms_mask[[6]])) / (ms_mask[[10]] + (6 * ms_mask[[6]]) - ( 7.5 * ms_mask[[1]] + 1)),             
    ## Gcc = ms_mask[[4]] / (ms_mask[[2]] + ms_mask[[4]] + ms_mask[[5]]), 
    GCC_Full = (ms_mask[[4]]+ ms_mask[[3]]) / (ms_mask[[1]] + ms_mask[[2]] + ms_mask[[3]]+ms_mask[[4]] + ms_mask[[5]] + ms_mask[[6]]),             
    # BCC = (ms_mask[[1]] + ms_mask[[2]]) / (ms_mask[[1]]  + ms_mask[[2]] + ms_mask[[3]] + ms_mask[[4]] + ms_mask[[5]] + ms_mask[[6]]), 
    
    # carotenoids, waxes
    # ARI = (1 / ms_mask[[4]]) - (1 / ms_mask[[7]]),    
    # EWI9 = (ms_mask[[6]] - ms_mask[[8]]) / (ms_mask[[6]] + ms_mask[[8]]),
    
    # carotenoids             
    # SIPI = (ms_mask[[10]] - ms_mask[[1]]) / (ms_mask[[10]] - ms_mask[[6]]), 
    # SIPI_2 = (ms_mask[[10]] - ms_mask[[2]]) / (ms_mask[[10]] - ms_mask[[6]]),  
    PRI = (ms_mask[[3]] - ms_mask[[4]]) / (ms_mask[[3]] + ms_mask[[4]]),             
    CCI = (ms_mask[[3]] - ms_mask[[5]]) / (ms_mask[[3]] + ms_mask[[5]]),             
    
    # Red edge             
    RE_upper = (ms_mask[[9]] - ms_mask[[8]]) / 23,             
    RE_lower = (ms_mask[[8]] - ms_mask[[7]]) / 12,             
    RE_total = (ms_mask[[9]] - ms_mask[[7]]) / 35
  )
  
  rast_all = rast(rast_list)
  
  # # Mean Indices
  print("calculating mean indices")
  df_spectral_mean = exact_extract(rast_all, RA_pols, fun = "mean", append_cols = c("treeID","EditedCrwn","tag","REP","COL","ROW","GenSam","Area_buffer5cm","Area"))
  print("calculating cell count")
  df_count = exact_extract(ms_mask[[1]], RA_pols, fun = "count", progress = TRUE, append_cols = c("treeID","EditedCrwn","tag","REP","COL","ROW","GenSam","Area_buffer5cm","Area"))

  df_spectral_mean_count <- merge(df_spectral_mean,df_count, by = c("treeID","EditedCrwn","tag","REP","COL","ROW","GenSam","Area_buffer5cm","Area"))

  if(!dir.exists(paste0(dir_D,"\\",updated_metrics_folder,"\\"))){ #creating CSV folders
    dir.create(paste0(dir_D,"\\",updated_metrics_folder,"\\"))}
  
  print("calculated indices")
  saveRDS(df_spectral_mean_count, paste0(dir_D,"\\",updated_metrics_folder,"\\", date_list[x], "_NIRshadowMask_MeanCrownSpectralIndices_GeneticSubset.rds"))

  #Median Indices
  print("calculating median indices")
  df_spectral_median = exact_extract(rast_all, RA_pols, fun = "median", append_cols = c("treeID","EditedCrwn","tag","REP","COL","ROW","GenSam","Area_buffer5cm","Area"))
  df_spectral_median_count <- merge(df_spectral_median,df_count, by = c("treeID","EditedCrwn","tag","REP","COL","ROW","GenSam","Area_buffer5cm","Area"))

  if(!dir.exists(paste0(dir_D,"\\",updated_metrics_folder,"\\"))){ #creating CSV folders
    dir.create(paste0(dir_D,"\\",updated_metrics_folder,"\\"))}
  print("calculated indices")
  saveRDS(df_spectral_median_count, paste0(dir_D,"\\",updated_metrics_folder,"\\", date_list[x], "_NIRshadowMask_MedianCrownSpectralIndices_GeneticSubset.rds"))

}


### adding them all to one, for median first:

for (i in 1:length(date_list)){
  print(date_list[i])

  file <- readRDS(paste0("D:\\Sync\\Sync\\Fdc_PR_Canoe\\Flights\\",date_list[i],"\\",updated_metrics_folder,"\\", date_list[i], "_NIRshadowMask_MeanCrownSpectralIndices_GeneticSubset.rds"))%>%
    as.data.frame()%>%
    mutate(Date = ymd(date_list[i]))
  
  if(i ==1){
    df_to_join <- as.data.frame(file)
  }else{
    df_to_join <- rbind(df_to_join,file)
  }
}

saveRDS(df_to_join, paste0("D:\\Sync\\Sync\\Fdc_PR_Canoe\\Flights\\AllDates_NIRshadowMask_MeanCrownSpectralIndices_GeneticSubset.rds"))

unique(df_to_join$Date)
