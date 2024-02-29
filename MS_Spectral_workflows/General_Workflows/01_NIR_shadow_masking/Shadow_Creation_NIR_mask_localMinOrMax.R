
### NIR SHADOW MASKING ###

#Required Packages
library(terra)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
#install.packages('pracma')
library(pracma) # for gradient/ derivative function
library(LaplacesDemon) # is.multimodal function


## If a local minimum exists -> the local minimum is the shadow threshold value : source paper 1: https://onlinelibrary.wiley.com/doi/10.1111/pce.14177 paper 2: https://www.mdpi.com/2504-446X/3/4/80
## If local min does not exist, the max is the threshold value : source paper 3: https://www.tandfonline.com/doi/full/10.1080/01431160600954621  :  https://doi.org/10.1080/01431160600954621

## -(1)- LOAD CROWN SHP AND BUFFER -###############################################################################
dir_crowns <- "D:\\Sync\\Sync\\Fdc_PR_Canoe\\Crowns.shp" #path to crowns shapefile

pols_spat = st_read(paste0(dir_crowns)) %>% # reading in crown shp
  filter(!st_is_empty(.)) %>% #removing empty
  st_buffer(dist = -.05) %>% #buffering inward by 5cm
  vect()

## -(2)- FUNCTION:  Finds index values where derivative switches from neg to post (aka local min)###################
#this function finds local minimums and "ranks" how defined they are by the 'definition' attribute
find_local_min <- function(values) {#input values is a list of 1st derivatives 
  neg_slope <- numeric() #initializing variables
  pos_slope <- numeric()
  index_of_closest <- numeric()
  definition = numeric()
  neg_sum = numeric()
  pos_sum = numeric()
  
  k <- 1 #initializing iterator 
  
  for (i in 2:length(values)) { #skip index 1 so following "values[i-1]..." can properly index
    #print(i)
    if (values[i - 1] < 0 & values[i] >= 0 & i > 15) { 
      #finds a change from negative to positive 1st derivative (local min)
      #i>15 because local min will not be in first 15 and this stops an error occurring where a local min is found in the first 15 values and the def_positive indexing does not work
      def_positive <- c(values[i:(i+15)]) # vector of next 15 gradient values
      def_pos <- def_positive[def_positive > 0] #only taking positive 1st derivative values
      pos_sum[k] <- sum(def_pos) #adding up the positive 1st derivative values
      
      def_negative <- c(values[(i - 15):i]) # vector of the 15 values to the left of the local min (negative 1st dervs)
      def_neg <- def_negative[def_negative < 0] #only taking negatives
      neg_sum[k] <- sum(def_neg) #adding up negative values
      
      neg_slope[k] <- values[i - 1] #1st derivative value at i-1 (right before sign change from neg to pos)
      pos_slope[k] <- values[i] #1st derivative at i
      index_of_closest[k] <- i - 1 #index value of the last neg 1st derivative before the local min
      definition[k] <- sum(abs(def_neg), abs(def_pos)) # higher the value, more pronounced the local min
      k <- k + 1
    }
  }
  
  return(data.frame(Neg_Value = neg_slope, Pos_Value = pos_slope, Index = index_of_closest, definition = definition, neg_def = neg_sum, pos_def = pos_sum))
}

## -(3)- NIR shadow mask #######################################################
# Creates NIR shadow masks for multispectral orthos (from the MicaSense)
#Saves out the masks and the NIR density plots with a vertical line indicating the theshold value

site = "Fdc_PR_Canoe" #Site name, this name will be used 
Nir_shadow_folder_baseName <- "NIR_shadow_mask_localMinOrMax" #name of the folder shadow masks will be written to

#list of dates of flight aquisitions
date_list <- c( "2022_04_23",
                "2023_04_05",
                "2022_05_08"
)
date_list

for (x in seq_along(date_list)) {
  print(date_list[x])
  
  dir = paste0("D:\\Sync\\Sync\\",site,"\\Flights\\", date_list[x], "\\2_Inputs\\")#directory where shadow mask folder will be made
  ms_temp = rast(list.files(paste0(dir, "metashape\\3_MS_ORTHO\\"), pattern = ".*MS_Calibrated.*_bestPanel.tif$", full.names = TRUE)) #path to multispectral orthomosaic
  
  #making a folder for shadow masks
  if (!dir.exists(paste0(dir, Nir_shadow_folder_baseName,"\\"))) {
    dir.create(paste0(dir,Nir_shadow_folder_baseName,"\\"))
  }
  
  NIR = ms_temp[[10]] %>% #isolating the NIR (842nm) band
    crop(pols_spat) %>% #cropping to the extent of the crown polygon shp
    clamp(upper = 50000, values = FALSE) %>% 
    as.vector()
  
  NIR_na <- na.omit(NIR) 
  density_values <- density(NIR_na)
  # plot(density_values) # check to see the distribution of NIR values (ie a visual check for whether or not a local min exists)
  
  if(is.multimodal(NIR_na)){ #if vector of NIR values has more than one peak/local max
    
    dy_dt <- pracma::gradient(density_values$y) #list of first derivatives
    
    zeros <- find_local_min(dy_dt) #Finds index locations where slope switches from neg to post (local min) and ranks the intensity of each local min
    zeros_filtered <- zeros[(zeros$pos_def > 0),]#filters rows with pos_def > 0, filtering out small minimums on negative slopes
    
    if(nrow(zeros_filtered)==0){#if empty, then detected a false multimodal distribution and defaulting to max as threshold value
      print("Is is false multimodal")
      max_density = max(density_values$y, na.rm = TRUE)
      threshold = density_values$x[which(density_values$y == max_density)]
      mode <- "Unimodal (False Multi)"
      thresh_name <- "LocalMax"
      
      }else{
      zeros_local_min <- zeros_filtered[which.max(zeros_filtered$definition), ]
      
      x_zeros <- density_values$x[zeros_local_min$Index] #selecting NIR (aka x_mid) values that correspond to index values where the slope switches from neg to positive 
      # Again catching "inf" returns:
      if (threshold <= 0.7 & threshold > 0){#setting limits on the threshold to remove any thresholds from the tails of the distribution
        threshold
        mode <- "Multimodal"
        thresh_name <- "LocalMin"
        
        }else{
        print("Multimodal with org thresh > 0.7") #This output usually indicates an error in the function, make sure to look at the NIR distribution to confirm this is in fact correct or if the code needs modification for a special case
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

  (hist = NIR %>% #plotting a histogram of NIR values with a vertical red line for the defined threshold value
      as_tibble() %>% 
      ggplot() +
      geom_histogram(aes(x = NIR), bins = 150) +
      geom_vline(xintercept = threshold, color = "red3") +
      labs(title = paste0(mode, " , threshold: ",round(threshold, digits = 2)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 13,hjust = 0.75, vjust = -28)))
  
  
  ggsave(hist, #saving out the plot
         filename = paste0(dir, Nir_shadow_folder,"\\", date_list[x], "_NIR_shadow_hist_localMin_orMax.jpeg"),
         device = jpeg,
         width = 8,
         height = 8)
  
  shadow_mask = ms_temp[[10]] #isolating the NIR band of the multispectral ortho
  shadow_mask[shadow_mask > threshold] = NA #for NIR values > threshold, make them NA
  shadow_mask[shadow_mask <= threshold] = 1 #for NIR values < threshold, make them 1
  #writing out the shadow mask that has values of 1 for all pixels that will be masked out
  terra::writeRaster(shadow_mask, paste0(dir, Nir_shadow_folder,"\\",  date_list[x], "_NIR_shadow_thresh",threshold ,"_",thresh_name, ".tif"),
                     overwrite = TRUE)
  
  #Grouping adjacent pixels with the same value (in this case representing shadowed areas) into distinct patches or clumps
  shadow_patches = raster::clump(raster::raster(shadow_mask), directions = 4) %>%
    rast()
  
  #Summarizing the clumps obtained from the shadow patches raster. It contains two columns: 
  #1. value: representing the unique ID of each clump:
  #2. count: representing the number of pixels within each clump
  clumps = data.frame(freq(shadow_patches))
  
  
  
  #Calculating the threshold for the number of pixels that a clump must contain to be considered significant
  #It is calculated based on the desired area threshold (200 cm²) divided by the area of a single pixel
  num_pix = 0.02 / (res(shadow_patches)[1]^2)# threshold 200 cm^2
  flecks = clumps[clumps$count > num_pix,] #remove clump observations with frequency smaller than the threshold
  flecks = as.vector(flecks$value) # record IDs from clumps which met the criteria in previous step
  
  new_mask = shadow_patches %in% flecks #keep clumps that have IDS in flecks
  new_mask[new_mask == 0] = NA #make clumps that are zero, NA
  
  #writing out a 'cleaned' shadow mask
  terra::writeRaster(new_mask, paste0(dir, Nir_shadow_folder,"\\",  date_list[x], "_NIR_shadow_thresh",threshold ,"_",thresh_name, "_mask2.tif"),
                     overwrite = TRUE)
  
}
