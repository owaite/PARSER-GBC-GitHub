## calulcating indcies with NIR shadow mask


library(terra)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
library(exactextractr)


##############################################################################
# (1) Read in crown .shp, add a 5cm inward buffer, and write an area attribute for both buffered and unbuffered geometries
##############################################################################

dir_crowns <- "D:\\Sync\\Sync\\Fdc_PR_Canoe\\Crowns.shp" #path to crowns shapefile

# reading in crown polygons
pols_read = st_read(paste0(dir_crowns), crs = 26910) %>% 
  filter(!st_is_empty(.)) 

#calculating area of crowns and adding a buffer
pols_buf <- pols_read %>%
  mutate(Area = st_area(pols_read)) %>%
  st_buffer(dist = -.05)
#vect()

#calculating area of buffered crown
pols_spat <- pols_buf %>%
  mutate(Area_buffer5cm = st_area(pols_buf))

#checking the crowns
colnames(pols_spat)
# plot(pols_spat)
# class(pols_spat)


##############################################################################
# (2) Masking multispectral orthos and calculating indices 
##############################################################################

site = "Fdc_PR_Canoe" #Site name
Nir_shadow_folder <- "NIR_shadow_mask_localMinOrMax" #name of the folder shadow masks are in (this folder should already exist with masks in it)
updated_metrics_folder <- "CSV_Index_NIR_Shadow_updated" #name of the folder that rds of indices will be written out to, does not need to exist yet, will be created in the loop below 

# Our folder structure has a "Flights" folder with sub folders within for each flight that are labelled with the acquisition
# date in the form "YYYY_mm_dd"

# List of dates of flight acquisitions that are also the names of the folders containing folders of processed multispectral data for the flight 
date_list <- c( "2022_04_23",
                "2023_04_05",
                "2022_05_08")
date_list

pols <- pols_spat # setting the pols variable used in the below loop to the pols_spat created above

for (x in 1:length(date_list)){
  print(date_list[x])
  
  #choosing a directory based on the site
  #directory locations are the paths to the folder that contains the Nir_shadow_folder and willcontain the updated_metrics_folder
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
  ms_ortho = rast(list.files(paste0(dir, "metashape\\3_MS_ORTHO\\"), pattern = ".*MS_Calibrated.*_bestPanel.tif$", full.names = TRUE))#path to multispectral ortho
  ms_ortho_name = names(ms_ortho)[1]
  ms_ortho_name_root <- substr(ms_ortho_name, 1, nchar(ms_ortho_name) - 2)#getting the root name of the multispectral ortho
  # ratio = ratios[x,]
  
  #reading in the NIR shadow mask
  shadow_mask = rast(list.files(paste0(dir, Nir_shadow_folder,"\\"),  pattern = ".*_NIR_shadow.*_thresh.*_mask2.tif$", full.names = TRUE))## NIr shadow mask, uing the mask from the NIr of the crowns only (ie mask NOT crop)
  
  #mask NIR shadow to pols:
  shadow_mask <- terra::mask(shadow_mask, pols)
  
  #masking multispectral ortho by shadow mask
  ms_mask <- terra::mask(ms_ortho, shadow_mask, maskvalues = 1, updatevalue = NA)
  #writing out the shadow masked multispectral ortho
  terra::writeRaster(ms_mask, paste0(dir, Nir_shadow_folder,"\\",ms_ortho_name_root,"_NirShadow_masked.tif"),
                     overwrite = TRUE)
  # ms_mask <- rast(paste0(dir, Nir_shadow_folder,"\\",ms_ortho_name_root,"_NirShadow_masked.tif"))
  
  ## mask shadow masked raster to pols
  ms_mask <- terra::mask(ms_mask, pols)
  #write out the masked raster
  terra::writeRaster(ms_mask, paste0(dir, Nir_shadow_folder,"\\",ms_ortho_name_root,"_NirShadow_ms_mask_to_pols.tif"),
                     overwrite = TRUE)
  #read in the masked ms raster
  ms_mask <- rast(paste0(dir, Nir_shadow_folder,"\\",ms_ortho_name_root,"_NirShadow_ms_mask_to_pols.tif"))
  
  #list of reflectance and index values that will be calculated per tree crown
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
  
  #Creating the folder that the rds will be written out to 
  if(!dir.exists(paste0(dir_D,"\\",updated_metrics_folder,"\\"))){ 
    dir.create(paste0(dir_D,"\\",updated_metrics_folder,"\\"))}
  
  # Calculating Mean Index values per crown
  print("calculating mean indices")
  df_spectral_mean = exact_extract(rast_all, pols, fun = "mean", append_cols = c("treeID","Edited","tag__","rep","blk","fam","fem"))
  print("calculating cell count")
  # Calculating the number of non-masked pixels used for index calculation
  df_count = exact_extract(ms_mask[[1]], pols, fun = "count", progress = TRUE, append_cols = c("treeID","Edited","tag__","rep","blk","fam","fem"))
  # Joining the mean index values df and the count values df
  df_spectral_mean_count <- merge(df_spectral_mean,df_count, by = c("treeID","Edited","tag__","rep","blk","fam","fem"))

 
  print("calculated mean indices")
  saveRDS(df_spectral_mean_count, paste0(dir_D,"\\",updated_metrics_folder,"\\", date_list[x], "_NIRshadowMask_MeanCrownSpectralIndices.rds"))

  # Calculating Median Index values per crown
  print("calculating median indices")
  df_spectral_median = exact_extract(rast_all, pols, fun = "median", append_cols = c("treeID","Edited","tag__","rep","blk","fam","fem"))
  df_spectral_median_count <- merge(df_spectral_median,df_count, by = c("treeID","Edited","tag__","rep","blk","fam","fem"))
  
  print("calculated median indices")
  saveRDS(df_spectral_median_count, paste0(dir_D,"\\",updated_metrics_folder,"\\", date_list[x], "_NIRshadowMask_MedianCrownSpectralIndices.rds"))

}


## Now we have .rds files for mean and medium values per crown, however each file is more one data aquisition
# below adds multiple data aquisitions into one large .rds file

for (i in 1:length(date_list)){
  print(date_list[i])

  #read in .rds file with index values per crown
  file <- readRDS(paste0("D:\\Sync\\Sync\\Fdc_PR_Canoe\\Flights\\",date_list[i],"\\",updated_metrics_folder,"\\", date_list[i], "_NIRshadowMask_MeanCrownSpectralIndices.rds"))%>%
    as.data.frame()%>%
    mutate(Date = ymd(date_list[i]))#Making a column named Date that is a lubridate date object in the format of year-month-day
  
  if(i ==1){
    df_to_join <- as.data.frame(file)
  }else{
    df_to_join <- rbind(df_to_join,file) #joining .rds files together, one date at a time as it goes through the for loop
  }
}

#saving out the joined df
saveRDS(df_to_join, paste0("D:\\Sync\\Sync\\Fdc_PR_Canoe\\Flights\\AllDates_NIRshadowMask_MeanCrownSpectralIndices.rds"))

#checking that all required data aquisitions are present
unique(df_to_join$Date)
