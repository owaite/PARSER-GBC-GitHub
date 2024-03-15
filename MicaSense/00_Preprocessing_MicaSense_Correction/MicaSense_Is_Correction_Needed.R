# 
# # Identifying if correction if needed for MicaSense Imagery
# library(terra)
# library(exifr)
# library(tidyverse)
# library(ggplot2)
# library(raster)
# library(lubridate)
# library(exifr)
# library(pracma)
# library(RcppCNPy)
# library(zoo)
# library(forecast)
# library(fable)
# library(ggpmisc)
# library(broom)
# library(tidyfit)
# library(rsample)
# 
# #libraries to parallize:
# # Load required packages
# library(foreach)
# library(snow)
# library(parallel)
# library(iterators)
# library(magrittr)  # For the pipe operator %>%
# 

library(future)

################################################################################

# assumes that image directories have been copied with "_save" for originals
### IE you will have a "MicaSense_Cleaned" folder and a "MicaSense_Cleaned_save" folder
### that ahve identical contents and contains the subfolders with images 
### throughout this code the "MicaSense_Cleaned" exif data will be edited and the "MicaSense_Cleaned_save" 
### will be an unedited backup

# Assumes that masks have been exported from agisoft metashape for all panels to '\\MASKS'

# Folder pattern: dir \\DAP\\MS\\ flight_date_save \\ PANELS \\ panel_img.tif
dir <- "Q:\\SNC\\Data\\Fdc_East_GCA\\Flights\\"
dir_masks <-  "Q:\\SNC\\Data\\Fdc_East_GCA\\Flights\\"
MS_folder_name <- "Micasense"
#Flight aquisitions that are also the names of folders containing the data per flight
date_list <-  c(
  "2023_03_13"
)
date_list               

date_range <- "2023_03_13" #"2022_04_23-2022_06_23" # This is used in the below code as a suffix to name the rds of corrected exif data

ext_save <- "_save" 
ext_save <- "" 
#In the ...\\Flights\\Date\\1_Data\\ directory there should be two folders: (1) MicaSense_Cleaned (just original MS data)
# (2) MicaSense_Cleaned_save (a copy of MicaSense_Cleaned)
# Images in MicaSense_Cleaned will have their metadata overwritten with corrected values and images in MicaSense_Cleaned_save will not be edited and remain as original backups
# change the string "_save" in ext_save to whatever is the suffix of your naming convention to match the above format (ie could do ext_save <- "_Origianl, for MicaSense_Cleaned_Original)

MS_folder_name <- paste0("Micasense", ext_save)

################################################################################
# (1) read XMP data into R format and save as RDS
################################################################################

# future:::ClusterRegistry("stop")
plan(multisession, workers = 2L) # Initiating 2 workers to speed up processing

for (x in seq_along(date_list)){
  
  for (j in 1:10){ # bands 1 through 10 
    
    pics = list.files(paste0(dir, date_list[x],"\\1_Data\\MS_folder_name"), #path the Micasense_Cleaned_save that is the original copied that will NOT be written over
                      pattern = c(paste0("IMG_...._", j, ".tif"), paste0("IMG_...._", j, "_", ".tif")), recursive = TRUE, 
                      full.names = TRUE) # list of directories to all images
    
    # # Getting names of masks
    # mask_images <- list.files(paste0("D:\\Sync\\Sync\\Fdc_PR_Canoe\\Flights\\",date_list[x],"\\2_Inputs\\metashape\\MASKS\\")) #path to folder with masks that are exported from metashape 
    # mask_img_names <- gsub("_mask\\.png$", "", mask_images) # removes the suffix "_mask.png" from each element in the 'mask_images' list, ie : IMG_0027_1_mask.png becomes : IMG_0027_1 
    # # IMG_0027_1 will match an image name in the MicaSense_Cleaned_save folder, therefore the list of mask_img_names will be used to identify panel images
    # 
    for (i in 1:length(pics)){ #for each image in Micasense_Cleaned_save, one at a time
      pic = pics[i]
      pic_root <- gsub(".*/(IMG_.*)(\\.tif)$", "\\1", pic)# The pattern captures the filename starting with "IMG_" and ending with ".tif".Ie: IMG_0027_1.tiff becomes IMG_0027_1
      print(pic_root)
      
      if (substr(pic_root,11,11) == "0") { # Distinguishes between _1 (band 1) and _10 (band 10), ie IMG_0027_1 verse IMG_0027_10
        band = substr(pic_root,10,11) # for _10
      } else { 
        band = substr(pic_root,10,10) # for _1, _2, _3, ... _9 (bands 1-9)
      } 
      
      img_exif = read_exif(pic) # read the XMP data 
      print(paste0(date_list[x], " band ", j, " ", i, "/", length(pics))) # keep track of progress
      
      # Creating df with same column names as exif data
      if (i == 1){ # If it's the first image, make new df from XMP data 
        exif_df = as.data.frame(img_exif) %>% 
          mutate(panel_flag = ifelse( gsub(".*/(IMG_.*)(\\.tif)$", "\\1", SourceFile) %in% mask_img_names, 1, 0)) # Give a value of 1 if the pic root name is in the list of mask root names and is therefore a calibration panel, give 0 otherwise (ie not a panel) 
      } else {   # Add each new image exif to dataframe
        exif_df = merge(exif_df, img_exif, by = intersect(names(exif_df), names(img_exif)), all = TRUE)%>%
          mutate(panel_flag = ifelse( gsub(".*/(IMG_.*)(\\.tif)$", "\\1", SourceFile) %in% mask_img_names, 1, 0)) # Give a value of 1 if the pic root name is in the list of mask root names and is therefore a calibration panel, give 0 otherwise (ie not a panel) 
      }
    }
    if(!dir.exists(paste0(dir,date_list[x],"\\1_Data\\CSV\\"))){ #creating CSV folders
      dir.create(paste0(dir,date_list[x],"\\1_Data\\CSV\\"))
    }
    if(!dir.exists(paste0(dir,date_list[x],"\\1_Data\\CSV\\Corrected_values\\"))){ #Creating Corrected_values folder to save the XMP data as rds files
      dir.create(paste0(dir,date_list[x],"\\1_Data\\CSV\\Corrected_values\\"))
    }
    saveRDS(exif_df, paste0(dir,date_list[x], "\\1_Data\\CSV\\Corrected_values\\df_", date_list[x], "_", j, ".rds")) # save output for each band, set path
  } 
}





























### Ensuring that panels are correctly flagged is important for the rest of the code
###  Here we are checking that the panel_flag is correct:
x = 2 # isolating a date (2022_05_08 here)
j = 1 # isolating a band (band 1 here)

test1 <- read_rds(paste0(dir,date_list[x], "\\1_Data\\CSV\\Corrected_values\\df_", date_list[x], "_", j, ".rds")) 
unique(test1$panel_flag) # should give 0 & 1
test1[test1$panel_flag == 1, ] # check that the img names here that are flagged as panels are in fact panels in your folders 


### Joining all bands and date's XMP data to one dataframe to speed up the editing process

for (x in 1:length(date_list)){
  for (j in c(1:10)){
    
    exif_df = read_rds(paste0(dir,date_list[x], "\\1_Data\\CSV\\Corrected_values\\df_", date_list[x], "_", j, ".rds")) # SET PATH
    
    if (x == 1 & j == 1){ # if it's the first image, make new df from XMP data 
      exif_df_all = as.data.frame(exif_df)
    } else {   # otherwise bind to previous df 
      exif_df_all = bind(exif_df_all, exif_df, by = intersect(names(exif_df_all), names(exif_df)), all = TRUE)
    }
    
    xmp_all = exif_df_all 
  }
}

# Creating a Date column that is a lubridate object
xmp_all$Date <- as.Date(xmp_all$CreateDate, format = "%Y:%m:%d")

missing_imgs <- xmp_all[is.na(xmp_all$Date),]$FileName # Extracts the FileName for rows that have a Date of NA
length(missing_imgs) #check if there are any images missing Dates
missing_imgs_roots <- sub("_[^_]*$", "", missing_imgs) # gets root image names of missing images (In my experience there are very few of these cases, if any, 
# and it happens when the img is corrupt and cant be opened, we fly at 87-89% overlap, 
# so I delete these images since deleting ~2 images in ~ 1200 does not affect the output)
missing_imgs_roots #print img roots 

# Create a new df "xmp_all_new" that filters out images that were determined to be corrupt and have missing information above
xmp_all_new <- xmp_all %>%
  mutate(img_name = str_split_i(FileName, ".tif", i = 1),
         img_root = sub("_(\\d+)$", "", img_name))%>%
  filter(!img_root %in% missing_imgs_roots)%>%
  dplyr::select(!c(img_name, img_root))

unique(xmp_all_new$Date) #check all dates are present and if any NA remain
dim(xmp_all) # Compare dimension of xmp_all to dimension of xmp_all_new below to make sure you only remove the intended photos
dim(xmp_all_new)
colnames(xmp_all_new) # Check column names 

saveRDS(xmp_all_new, paste0(dir, "CSV\\Corrected_values\\XMP_", date_range,".rds")) # Save out the joined df that now has a Date column to distinguish between flights, and has 

unique(xmp_all_new$panel_flag)
xmp_all_new[xmp_all_new$panel_flag == 1,]$FileName # Again just checking that the proper images are labelled as panels
# I'll take a few min here and just check a few of the images in their respective folders that are labelled panels her and make sure they are panel imgs
# (our panels are at the beginning, at battery swaps and the end so doing this manual check is quick)
