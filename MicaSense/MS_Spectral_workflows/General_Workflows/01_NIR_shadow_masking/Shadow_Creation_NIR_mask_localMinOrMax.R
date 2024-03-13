

#Required Packages
library(terra)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(sf)
#install.packages('pracma')
library(pracma) # for gradient/ derivative function
library(LaplacesDemon) # is.multimodal function

### NIR SHADOW MASKING ###
# If a local minimum exists -> the local minimum is the shadow threshold value : source paper 1: https://onlinelibrary.wiley.com/doi/10.1111/pce.14177 paper 2: https://www.mdpi.com/2504-446X/3/4/80
# If local min does not exist, the max is the threshold value : source paper 3: https://www.tandfonline.com/doi/full/10.1080/01431160600954621  :  https://doi.org/10.1080/01431160600954621


##############################################################################
# (1) LOAD CROWN SHP AND BUFFER 
##############################################################################
dir_crowns <- "D:\\Sync\\Sync\\Fdc_PR_Canoe\\Crowns.shp" #path to crowns shapefile

pols_spat = st_read(paste0(dir_crowns)) %>% # reading in crown shp
  filter(!st_is_empty(.)) %>% #removing empty
  st_buffer(dist = -.05) %>% #buffering inward by 5cm
  vect()



##############################################################################
# (2) Function find_local_min: finds index values where derivative switches from neg to post (aka local min)
##############################################################################

#this function finds local minimums and "ranks" how defined they are by the 'definition' attribute
find_local_min <- function(values) { #input "values" is a list of 1st derivatives of NIR values
  #initializing variables
  neg_slope <- numeric() 
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
      #i>15 because local min will not be in first 15 values and this stops an error occurring where a local min is found in the first 15 values and the def_positive indexing does not work
      def_positive <- c(values[i:(i+15)]) # vector of next 15 gradient values
      def_pos <- def_positive[def_positive > 0] #only taking positive 1st derivative values (aka positive slopes)
      pos_sum[k] <- sum(def_pos) #adding up the positive 1st derivative values
      
      def_negative <- c(values[(i - 15):i]) # vector of the 15 values to the left of the local min (negative 1st derivatives)
      def_neg <- def_negative[def_negative < 0] #only taking the negative slopes in the list
      neg_sum[k] <- sum(def_neg) #adding up negative values
      
      neg_slope[k] <- values[i - 1] #1st derivative value at i-1 (right before sign change from neg to pos)
      pos_slope[k] <- values[i] #1st derivative at i (at the switch from neg to positive)
      index_of_closest[k] <- i - 1 #index value of the last neg 1st derivative before the local min
      definition[k] <- sum(abs(def_neg), abs(def_pos)) # higher the value, more pronounced the local min
      k <- k + 1
    }
  }
  
  return(data.frame(Neg_Value = neg_slope, Pos_Value = pos_slope, Index = index_of_closest, definition = definition, neg_def = neg_sum, pos_def = pos_sum))
}

##############################################################################
# (3) Creating NIR shadow masks (for loop iterates over data acquisition dates)
##############################################################################
# Creates NIR shadow masks for multispectral orthos (from the MicaSense)
# Saves out the masks and the NIR density plots with a vertical line indicating the threshold value

site = "Fdc_PR_Canoe" #Site name, is used to create the proper path name in the below loop
Nir_shadow_folder_baseName <- "NIR_shadow_mask_localMinOrMax" #name of the folder shadow masks will be written to

# Our folder structure has a "Flights" folder with sub folders within for each flight that are labelled with the acquisition
# date in the form "YYYY_mm_dd"

# List of dates of flight acquisitions that are also the names of the folders containing folders of processed multispectral data for the flight 
date_list <- c( "2022_04_23",
                "2023_04_05",
                "2022_05_08"
)

# date_list # checking dates

# Below for loop: 
# ()

for (x in seq_along(date_list)) {
  print(date_list[x])
  
  dir = paste0("D:\\Sync\\Sync\\",site,"\\Flights\\", date_list[x], "\\2_Inputs\\")#directory where shadow mask folder will be made
  ms_temp = rast(list.files(paste0(dir, "metashape\\3_MS_ORTHO\\"), pattern = ".*MS_Calibrated.*_bestPanel.tif$", full.names = TRUE)) #path to multispectral orthomosaic
  
  #making a folder for shadow masks
  if (!dir.exists(paste0(dir, Nir_shadow_folder_baseName,"\\"))) {
    dir.create(paste0(dir,Nir_shadow_folder_baseName,"\\"))
  }
  
  
  ##############################################################################
  # (3.1) Defining the NIR threshold value 
  ##############################################################################
  
  #Create a vector of near-infrared (NIR) values from the 10th multispectral band (842nm)
  NIR = ms_temp[[10]] %>% #isolating the NIR (842nm) band
    crop(pols_spat) %>% #cropping to the extent of the crown polygon shp
    clamp(upper = 50000, values = FALSE) %>% 
    as.vector()
  
  NIR_na <- na.omit(NIR) #remove NA values from the NIR vector
  density_values <- density(NIR_na) # caluclates density of NIR values, used to plot below to check the shape of the distribution
  # plot(density_values) # check to see the distribution of NIR values (ie a visual check for whether or not a local min exists)
  
  # If the vector of NIR values has more than one peak/local max and is therefore multimodal: 
  if(is.multimodal(NIR_na)){ 
    
    dy_dt <- pracma::gradient(density_values$y) #list of first derivatives of NIR vector
    
    zeros <- find_local_min(dy_dt) # Finds index locations where slope switches from neg to post (local min) and ranks the intensity of each local min
    zeros_filtered <- zeros[(zeros$pos_def > 0),] # Filters rows with pos_def > 0, filtering out small minimums on negative slopes (not true local mins)
    
    if(nrow(zeros_filtered)==0){#if empty, then detected a false multimodal distribution and defaulting to max as threshold value
      print("It is a false multimodal")
      max_density = max(density_values$y, na.rm = TRUE)
      threshold = density_values$x[which(density_values$y == max_density)]
      mode <- "Unimodal (False Multi)"
      thresh_name <- "LocalMax"
      
      }else{ 
      zeros_local_min <- zeros_filtered[which.max(zeros_filtered$definition), ] #isolating the largest local min
      x_zeros <- density_values$x[zeros_local_min$Index] #selecting NIR (aka x_mid) value that corresponds to the index value of the most defined local min 
     
      # Catching cases of "inf" returns:
      if (threshold <= 0.7 & threshold > 0){#setting limits on the threshold to remove any thresholds from the tails of the distribution
        print(threshold)
        mode <- "Multimodal"
        thresh_name <- "LocalMin"
        
        }else{
        print("Multimodal with org thresh > 0.7") #This output usually indicates an error in the function, make sure to look at the NIR distribution to confirm this is in fact correct or if the code needs modification for a special case
        #This often indicates a false multimodal as well, and therefore the NIR value with the max frequency in the NIR distribution with be used as the threshold
        max_density = max(density_values$y, na.rm = TRUE)
        threshold = density_values$x[which(density_values$y == max_density)]
        mode <- "Unimodal (False Multi with org thresh > 0.7)"
        thresh_name <- "LocalMax"
      }
      
    }
  }else{ #if the NIR vector is NOT multimodal (determined by the is.multimodal function)
    #Finding NIR value with the greatest frequency in the distribution - this max value will be the threshold for unimodal distributions 
    max_density = max(density_values$y, na.rm = TRUE)
    threshold = density_values$x[which(density_values$y == max_density)]
    print(threshold)
    thresh_name <- "LocalMax"
    mode <- "Unimodal"
  }
  # plot(density_values$x, density_values$y) #a check

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
  
  
  ##############################################################################
  # (3.2) Creating the shadow mask using the above threshold and the associated multispectral ortho 
  ##############################################################################
  
  shadow_mask = ms_temp[[10]] #isolating the NIR band of the multispectral ortho
  shadow_mask[shadow_mask > threshold] = NA #for NIR values > threshold, make them NA
  shadow_mask[shadow_mask <= threshold] = 1 #for NIR values < or = to the threshold value, make them 1
  
  # Writing out the shadow mask that has values of 1 for all pixels that will be masked out
  terra::writeRaster(shadow_mask, paste0(dir, Nir_shadow_folder,"\\",  date_list[x], "_NIR_shadow_thresh",threshold ,"_",thresh_name, ".tif"),
                     overwrite = TRUE)
  
  # Grouping adjacent pixels with the same value (in this case representing shadowed areas) into distinct patches or clumps
  shadow_patches = raster::clump(raster::raster(shadow_mask), directions = 4) %>%
    rast()
  
  # Summarizing the clumps obtained from the shadow patches raster. It contains two columns: 
  #1. value: representing the unique ID of each clump:
  #2. count: representing the number of pixels within each clump
  clumps = data.frame(freq(shadow_patches))
  
  # Calculating the threshold for the number of pixels that a clump must contain to be considered significant
  # It is calculated based on the desired area threshold (200 cm²) divided by the area of a single pixel
  num_pix = 0.02 / (res(shadow_patches)[1]^2)# 0.02 represents a threshold of 200 cm^2
  flecks = clumps[clumps$count > num_pix,] # remove clump observations with frequency smaller than the threshold
  flecks = as.vector(flecks$value) # record IDs from clumps which met the criteria in previous step
  
  new_mask = shadow_patches %in% flecks #keep clumps that have IDS in flecks
  new_mask[new_mask == 0] = NA # make clumps that are zero, NA
  
  #writing out a 'cleaned' shadow mask
  terra::writeRaster(new_mask, paste0(dir, Nir_shadow_folder,"\\",  date_list[x], "_NIR_shadow_thresh",threshold ,"_",thresh_name, "_mask2.tif"),
                     overwrite = TRUE)
  
}
