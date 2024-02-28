

library(terra)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
#install.packages('pracma')
library(pracma) # for gradient/ derivative function
library(LaplacesDemon) # is.multimodal function


### NIR SHADOW MASKING ###
## IF LOCAL MIN - LOCAL MIN IS SHADOW MASKED VALUE : paper 1: https://onlinelibrary.wiley.com/doi/10.1111/pce.14177 paper 2: https://www.mdpi.com/2504-446X/3/4/80
## IF NO LOCAL MIN< MAX IS THE THRESHOLD VALUE : paper 3: https://www.tandfonline.com/doi/full/10.1080/01431160600954621  :  https://doi.org/10.1080/01431160600954621


site = "Fdc_PR_Canoe" #"Cw_PR_Rainbow_SiteA"

# NIR mask for shadow masking 
date_list <- c( "2022_04_23")
                # "2023_04_05",
                
 #  "2022_05_08",
 #  "2022_05_27",
 #  "2022_06_08",
 #  "2022_06_23",
 #  "2022_07_06",
 # "2022_07_22",
 #  "2022_07_27",
 #  "2022_08_05",
 #  "2022_08_24",
 #  "2022_09_08",
 #  "2022_09_20",
 #  "2022_10_05",
 #  "2022_10_20",
 #  "2022_11_28",
 #  "2023_02_17",
 #  "2023_03_22",
 #  "2023_04_26",
 # "2023_05_10",
 # "2023_05_24",
 # "2023_06_07",
 # "2023_06_21",
 # "2023_08_17"
  
)

#For Rainbow:
# date_list <- c( "2022_06_07", ##### NEED TO ALIGN and export ORTHO for SiteB
#                 "2022_06_22", 
#                 "2022_07_05",
#                 "2022_07_21",
#                 "2022_08_04",
#                 "2022_08_23",
#                 "2022_09_07",
#                 "2022_09_19",
#                 "2022_10_04",
#                 "2022_10_19",
#                 "2023_01_25"
# )

date_list

##Assign Dir and varaibles
dir_crowns <- "D:\\Sync\\Sync\\Fdc_PR_Canoe\\CROWNS_done_reviewed_missing_added\\Data\\CROWNS\\Edited\\CHM_.06_masked_RGB_H20T_mosaic_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss.sig.01w3_z75_crowns_EDITED.shp"

#"K:\\Rainbow_A_Crowns\\Edited_as_of_2023_09_22_notDone\\Edited_CHM_.06_masked_RGB_H20T_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss.sig.01w3_z75_crowns_2_AbsIncluded.shp"  #pols_spat = st_read(paste0("K:\\Rainbow_B_Crowns\\Data\\Crowns\\Edited_Spencer\\CHM_.06_masked_RGB_H20T_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss.sig.01w3_z75_crowns_2_EDITED.shp")) %>%


#Nir_shadow_folder_baseName <- "NIR_shadow_mask"
Nir_shadow_folder_baseName <- "NIR_shadow_mask_localMinOrMax"

# reading in corwn polygons
pols_spat = st_read(paste0(dir_crowns)) %>% 
  filter(!st_is_empty(.)) %>%
  st_buffer(dist = -.05) %>%
  vect()

plot(pols_spat)
class(pols_spat)


## Function to find index values where derivative switches from neg to post (aka local min)----------:
find_local_min <- function(values) {
  neg_slope <- numeric()
  pos_slope <- numeric()
  index_of_closest <- numeric()
  definition = numeric()
  neg_sum = numeric()
  pos_sum = numeric()
  
  k <- 1
  
  for (i in 2:length(values)) {
    #print(i)
    if (values[i - 1] < 0 & values[i] >= 0 & i > 15) { #i>15 because local min will not be in first 15 and this stops an error occuring where a local min is found in teh first 15 values and the def_positive indexing doe snot work
      # Check if the absolute value of the previous and current values is closer to zero
      # if (abs(values[i - 1]) < abs(values[i])) {
      #   closest_to_zero[k] <- values[i - 1]
      #   index_of_closest[k] <- i - 1
      # } else {
      #   closest_to_zero[k] <- values[i]
      #   index_of_closest[k] <- i
      # }
      def_positive <- c(values[i:i+15]) # vector of next 7 gradient values
      def_pos <- def_positive[def_positive > 0] #only taking positives
      pos_sum[k] <- sum(def_pos)
      
      def_negative <- c(values[(i - 10):i-15]) # vector of next 7 gradient values # 7 was used and found first min
      def_neg <- def_negative[def_negative < 0] #only taking negatives
      neg_sum[k] <- sum(def_neg)
      
      neg_slope[k] <- values[i - 1]
      pos_slope[k] <- values[i]
      index_of_closest[k] <- i - 1
      definition[k] <- sum(abs(def_neg), abs(def_pos)) # higher the value, more pronounced the local min
      k <- k + 1
    }
  }
  
  return(data.frame(Neg_Value = neg_slope, Pos_Value = pos_slope, Index = index_of_closest, definition = definition, neg_def = neg_sum, pos_def = pos_sum))
}


## For loop to create NIR shadow masks for MS orthos and save out the masks, and density plots
for (x in seq_along(date_list)) {
  print(date_list[x])
  
  if (site %in% c("Cw_PR_Rainbow_SiteA", "Cw_PR_Rainbow_SiteB")){
    dir = paste0("K:\\",site,"\\Flights\\", date_list[x], "\\2_Inputs\\")
  }
  if (site == "Fdc_PR_Canoe"){
    dir = paste0("D:\\Sync\\Sync\\",site,"\\Flights\\", date_list[x], "\\2_Inputs\\")
  }
  
  print(dir)
  
  ms_temp = rast(list.files(paste0(dir, "metashape\\3_MS_ORTHO\\"), pattern = ".*MS_Calibrated.*_bestPanel.tif$", full.names = TRUE))

  
  # USING CROP FOR EXTEND OF CROWN SHP: make a shadow mask from NIR reflectance values: --------------
  #making a folder for shadow masks
  c_or_m <- "_polyCrop"
  
  Nir_shadow_folder <- paste0(Nir_shadow_folder_baseName, "_polyCrop")
  
  if (!dir.exists(paste0(dir, Nir_shadow_folder,"\\"))) {
    dir.create(paste0(dir,Nir_shadow_folder,"\\"))
  }
  
  NIR = ms_temp[[10]] %>% 
    crop(pols_spat) %>% 
    #mask(pols_spat) %>% 
    clamp(upper = 50000, values = FALSE) %>% 
    as.vector()
  
  NIR_na <- na.omit(NIR)
  density_values <- density(NIR_na)
  plot(density_values)
  
  if(date_list[x] == "2022_04_23"){
    max_density = max(density_values$y, na.rm = TRUE)
    threshold = density_values$x[which(density_values$y == max_density)]
    threshold
    thresh_name <- "LocalMax"
    mode <- "Unimodal"
    
    }else if (is.multimodal(NIR_na)){
    
    dy_dt <- pracma::gradient(density_values$y) #list of first derivatives
    
    zeros <- find_local_min(dy_dt) #list of values and their indicies where slope switches from neg to post (local min)
    zeros_filtered <- zeros[(zeros$pos_def > 0),]#filters rows with pos_def > 0, filtering out small minimums on negative slopes
    
    if(nrow(zeros_filtered)==0){#if empty, then detected a false multimodal distribution and defaulting to max as threshold value
      print("Is is false multimodal")
      max_density = max(density_values$y, na.rm = TRUE)
      threshold = density_values$x[which(density_values$y == max_density)]
      mode <- "Unimodal (False Multi)"
      thresh_name <- "LocalMax"
      
      }else{
      zeros_local_min <- zeros_filtered[which.max(zeros_filtered$definition), ]
      
      x_zeros <- density_values$x[zeros_local_min$Index] #selecting NIR (aka x_mid) values that correspond to index values where the slope switches from neg to positive (indicating a local min)
      threshold <- max(x_zeros[x_zeros<0.6])
      # Again catching "inf" returns:
      if (threshold <= 0.7 & threshold > 0){
        threshold
        mode <- "Multimodal"
        thresh_name <- "LocalMin"
        
        }else{
        print("Multimodal with org thresh > 0.7")
        max_density = max(density_values$y, na.rm = TRUE)
        threshold = density_values$x[which(density_values$y == max_density)]
        mode <- "Unimodal (False Multi with org thresh > 0.7)"
        thresh_name <- "LocalMax"
      }
      
    }
  }else{
    max_density = max(density_values$y, na.rm = TRUE)
    threshold = density_values$x[which(density_values$y == max_density)]
    threshold
    thresh_name <- "LocalMax"
    mode <- "Unimodal"
  }
  # plot(density_values$x, density_values$y)
  
  
  
  width = 8
  height = 8
  
  (hist = NIR %>% 
      as_tibble() %>% 
      ggplot() +
      geom_histogram(aes(x = NIR), bins = 150) +
      geom_vline(xintercept = threshold, color = "red3") +
      labs(title = paste0(mode, " , threshold: ",round(threshold, digits = 2)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 13,hjust = 0.75, vjust = -28)))
  
  
  ggsave(hist, 
         filename = paste0(dir, Nir_shadow_folder,"\\", date_list[x], "_NIR_shadow_hist_localMin_orMax.jpeg"),
         device = jpeg,
         width = 8,
         height = 8)
  
  #hist(NIR, breaks = 100) + abline(v = threshold, col='red', lwd = 3)
  
  shadow_mask = ms_temp[[10]]
  shadow_mask[shadow_mask > threshold] = NA
  shadow_mask[shadow_mask <= threshold] = 1
  
  terra::writeRaster(shadow_mask, paste0(dir, Nir_shadow_folder,"\\",  date_list[x], "_NIR_shadow",c_or_m,"_thresh",threshold ,"_",thresh_name, ".tif"),
                     overwrite = TRUE)
  
  
  shadow_patches = raster::clump(raster::raster(shadow_mask), directions = 4) %>%
    rast()
  
  clumps = data.frame(freq(shadow_patches))
  # threshold 200 cm^2
  num_pix = 0.02 / (res(shadow_patches)[1]^2)
  flecks = clumps[clumps$count > num_pix,] #remove clump observations with frequency smaller than 9
  flecks = as.vector(flecks$value) # record IDs from clumps which met the criteria in previous step
  
  new_mask = shadow_patches %in% flecks
  new_mask[new_mask == 0] = NA
  
  terra::writeRaster(new_mask, paste0(dir, Nir_shadow_folder,"\\",  date_list[x], "_NIR_shadow",c_or_m,"_thresh",threshold ,"_",thresh_name, "_mask2.tif"),
                     overwrite = TRUE)
  
}

#-- Troubleshooting strange outputs ----------------
#NIR rds from Sam that gives odd outputs:
#NIR_sam <- readRDS("C:\\Users\\owaite\\Downloads\\Skimikin_2020_07_02_NIR.rds")
