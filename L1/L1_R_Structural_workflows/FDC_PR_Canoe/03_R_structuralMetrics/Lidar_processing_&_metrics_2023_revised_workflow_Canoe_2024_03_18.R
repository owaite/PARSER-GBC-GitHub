library(tidyverse)
library(sf)
library(sp)
library(spatial)
# library(rgdal)###
# library(rgeos)###
library(raster)
library(terra)
library(lidR)
library(sp)
library(nngeo)
library(future)
library(rmapshaper)
library(concaveman)
library(parallel)
library(foreach)
library(smoothr)
library(ForestTools)
library(gdalUtilities)
library(exactextractr)
library(alphashape3d)
# library(aRchi)###
library(lwgeom)
library(dplyr)

# LIDAR WORKFLOW: 2023-11-30

## (0) processed in terra v3.9
## (1) projected to nad83 and offset in lastools to ease initial manual alignemnet in CC
## (1) Align unthinned cloud in CC
## (2) In lastools: rescale, index and tile
## In R below: (3) thin with voxels, create 10cm DTM, normalize cloud, isolate individual crown point clouds, calculate metrics


name = "Canoe"
site = "Fdc_PR_Canoe"
L1_folder = "L1_metrics" #Canoe_L1"
drive = "E"
#"2022_04_23",
# done: "2022_04_23"
date_list = c("2022_04_23","2022_11_28","2023_06_21","2023_12_17")#,"2023_06_21")

for (i in 1:length(date_list)){
  
  date = date_list[i]
  print(date)
  dir = paste0("E:\\Canoe\\L1_metrics\\",date)
  
  # Create output file directories if they don't exist
  
  # if (!dir.exists(paste0(dir, "\\04_RASTER"))) {
  #   dir.create(paste0(dir, "\\04_RASTER"),recursive = TRUE)
  # }
  # if (!dir.exists(paste0(dir, "\\05_NORM"))) {
  #   dir.create(paste0(dir, "\\05_NORM"),recursive = TRUE)
  # }
  # if (!dir.exists(paste0(dir, "\\06_NORM_clean"))) {
  #   dir.create(paste0(dir, "\\06_NORM_clean"),recursive = TRUE)
  #}
  if (!dir.exists(paste0(dir, "\\07_SEGMENTED"))) {
    dir.create(paste0(dir, "\\07_SEGMENTED"),recursive = TRUE)
  }
  if (!dir.exists(paste0(dir, "\\08_MERGED"))) {
    dir.create(paste0(dir, "\\08_MERGED"),recursive = TRUE)
  }
  if (!dir.exists(paste0(dir,"\\09_CROWNS"))) {
    dir.create(paste0(dir, "\\09_CROWNS"), recursive = TRUE)
  }
  if (!dir.exists(paste0(dir,"\\10_CROWNS_clean"))) {
    dir.create(paste0(dir, "\\10_CROWNS_clean"), recursive = TRUE)
  }
  
  #making a folder to put the individual crown .rds metric files in
  
  if (!dir.exists(paste0(dir,"\\10_CROWNS_clean\\Structural_metrics\\"))) {
    dir.create(paste0(dir,"\\10_CROWNS_clean\\Structural_metrics\\"),recursive = TRUE)
  }
  
}## Remove this, just to get folderss because Jake already ran the below for Canoe 2023_12_17

##############################################################################
#(1)# Create 10cm DTMs - ground identified in DJI terra - thinned to 1cm below
##############################################################################
GROUND = readLAScatalog(folder = paste0(dir, "\\03_tile\\"), filter = "drop_withheld")
opt_filter(GROUND) <- "-drop_withheld"
opt_filter(GROUND) = "-thin_with_voxel 0.01"
opt_chunk_buffer(GROUND) = .5
opt_laz_compression(GROUND) = TRUE
opt_output_files(GROUND) = ""
opt_progress(GROUND) = TRUE


# Create a DTM from the ground points
DTM = grid_terrain(GROUND, res = 0.1, tin(), full_raster = FALSE) %>% 
  focal(w = matrix(1, 25, 25),
        fun = mean,
        na.rm = TRUE,
        pad = TRUE)

crs(DTM) <- CRS("+proj=utm +zone=10 +datum=NAD83")
writeRaster(DTM, paste0(dir, "\\04_RASTER\\", name, "_DTM_0.1m.tif"), overwrite = TRUE)

##############################################################################
#(2)# Normalize Cloud - thinned to 1cm below
##############################################################################

# read the tiles again, but with high-res parameters
CTG = readLAScatalog(folder = paste0(dir, "\\03_tile\\"))
# small buffer; we're just normalizing
opt_chunk_buffer(CTG) = .5
opt_laz_compression(CTG) = TRUE
# 1cm voxelization
opt_filter(CTG) = "-thin_with_voxel 0.01"
opt_output_files(CTG) = paste0(dir, "\\05_NORM\\{*}_NORM")
opt_progress(CTG) = TRUE

# normalize the heights
NORM = normalize_height(CTG, DTM, na.rm = TRUE)

# now read them in again and filter out points below -0.5
NORM = readLAScatalog(paste0(dir, "\\05_NORM\\"))
# no buffer, just filtering
opt_chunk_buffer(NORM) = 0
opt_laz_compression(NORM) = TRUE
opt_filter(NORM) = "-drop_z_below -.25"
opt_output_files(NORM) = paste0(dir, "\\06_NORM_clean\\{*}")
opt_progress(NORM) = TRUE

# now write over the NORM files but filter points less than -.5
NORM_clean = catalog_retile(NORM)

#### Making CHMS
CHM_max = grid_metrics(NORM_clean,
                       res = 0.04,
                       func = ~max(Z))

crs(CHM_max) <- CRS("+proj=utm +zone=10 +datum=NAD83")
writeRaster(CHM_max, paste0(dir, "\\04_RASTER\\", name, "_CHM_max_0.04m.tif"), overwrite = TRUE)

}

##############################################################################
#(3)# Read in edited polygons for metric calculation: 
##############################################################################
## Canoe polygon section: -------------------------------------------------------------
#Canoe crowns, EDITED_NEW: 
#2c: doesnt exiist, apart of another tree'
#2g: reviewed and determined to be good
#2w: unsure
#3 uncorrected
#4 doestn exist

Q:\GBC\Data\Cw_PR_Rainbow_A\Flights\2023_12_17\2_Inputs\L1\05_ALIGNED_ALS\pols
pols = st_read("E:\\Canoe\\Crowns\\Edited_added\\CHM_.06_masked_RGB_H20T_mosaic_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss.sig.01w3_z75_crowns_EDITED_NEWmissAdded.shp", crs = 26910) %>% 
  filter(!st_is_empty(.)) %>% 
  # filter(Edited != "4") #%>%
  st_make_valid() %>% 
  st_cast("POLYGON")


#ensure polygon has a"fam", "rep", "treeID" and "tag" column for point cloud labeling later on
colnames(pols)
pols$treeID = pols$treeID
# pols$tag = paste(pols$ROW, pols$COL, sep = "") #making tag for rainbow
pols$tag <- pols$tag__
pols$fam
pols$rep

## Rainbow A polygons: ------------------------------------------------------------------
## Rainbow B polygons: ------------------------------------------------------------------

##############################################################################
#(4)# Segment NORM_clean cloud - goes tile by tile and attaches treeID info to points and filters based on height
##############################################################################
# las <- readLAS("G:\\PARSER_Ext\\Fdc_PR_Canoe\\Canoe_L1\\2022_04_23\\06_NORM_clean\\405300_5516055_NORM.laz")
# st_crs(las) <- crs(NORM)

date_list <- c("2022_04_23","2022_11_28","2023_06_21")
date_list <- c("2022_11_28","2023_06_21","2023_12_17") #

for (i in 1:length(date_list)){
  
  date = date_list[i]
  print(date)
  dir = paste0("E:\\Canoe\\L1_metrics\\",date)
  
  if(date != "2023_12_17"){
    # read in norm cloud
    NORM = readLAScatalog(paste0(dir, "\\06_NORM_clean\\"))
    crs(NORM) <- st_crs(26910)
    opt_chunk_buffer(NORM) = 0
    opt_laz_compression(NORM) = TRUE
    opt_output_files(NORM) = paste0(dir, "\\07_SEGMENTED\\{*}_SEGMENTED")
    opt_progress(NORM) = TRUE
    
    
    zq = 0.75 # height percentile, here is going to take the top 25%
    
    polys_to_las = function(chunk, zq = 0.75, polygons = pols) {
      
      las = readLAS(chunk)                  
      if (lidR::is.empty(las)) {
        return(NULL) }
      
      #polys_crop = raster::crop(polys, extent(las))
      las2 = merge_spatial(las, polygons, "treeID") #changed Obs to treeID
      
      las_df = las2@data %>%
        dplyr::group_by(treeID) %>%
        dplyr::mutate(Zq999 = quantile(Z, 0.999), na.rm = TRUE) %>%
        # dplyr::ungroup() %>% 
        dplyr::mutate(treeID = if_else(Z > quantile(Z, 0.999,na.rm = TRUE) * zq, as.numeric(treeID), 0))
      # dplyr::mutate(treeID = if_else(!is.na(Z) && Z > (Zq999 * zq), as.numeric(treeID), 0))
      
      
      las3 = las2 %>% 
        add_lasattribute(las_df$treeID, name = "treeID", desc = "treeID") %>% 
        filter_poi(treeID > 0) %>% 
        filter_poi(buffer == 0)
      
      if (lidR::is.empty(las3)) {
        return(NULL)
      } else {
        return(las3)
      }
      
    }
    
    
    # new chunks have only segmented portions of tree crowns
    SEGMENTED = catalog_apply(NORM, polys_to_las)
    
    print("segmented")
    ##############################################################################
    #(5)# Merge all segmented tiles (will only have points left that are attributed to top 25% of each tree, need to merge because some trees have been split in the tiling process
    ##############################################################################
    
    SEGMENTED = readLAScatalog(paste0(dir, "\\07_SEGMENTED\\"))
    opt_chunk_buffer(SEGMENTED) = 0
    opt_chunk_size(SEGMENTED) = 10000
    opt_laz_compression(SEGMENTED) = TRUE
    opt_progress(SEGMENTED) = TRUE
    opt_output_files(SEGMENTED) = paste0(dir, "\\08_MERGED\\", name, "_HULLS_merged")
    
    # merge all the segmented trees into a single point cloud 
    MERGED = catalog_retile(SEGMENTED)
    print("merged")
  }
  
  
  
  ##############################################################################
  #(6)# Clip merged cloud to individual trees 
  ##############################################################################
  # save each individual tree as a point cloud
  zq = 0.75 
  print(dir)
  MERGED <- readLAScatalog(paste0(dir, "\\08_MERGED\\",name,"_HULLS_merged.laz"))
  print("making crowns")
  opt_output_files(MERGED) = paste0(dir, "\\09_CROWNS\\", name, "_fam{fam}_rep{rep}_tag{tag}_treeID{treeID}") #changed Seedlot to fam, changed tree to treeID
  CROWNS = clip_roi(MERGED, pols) 
  
  ## CLEANING crowns
  CROWNS = readLAScatalog(paste0(dir, "\\09_CROWNS\\"), filter = "-drop_withheld")
  opt_chunk_size(CROWNS) = 0 # processing by files
  opt_laz_compression(CROWNS) = TRUE
  opt_chunk_buffer(CROWNS) = 0 # no buffer
  opt_wall_to_wall(CROWNS) = TRUE # disable internal checks to ensure a valid output. Free wheel mode
  opt_output_files(CROWNS) = paste0(dir, "\\10_CROWNS_clean\\{*}")
  print("cleaning crowns")
  CROWNS_clean = catalog_apply(CROWNS, clean_crowns)
}


#HERE down changes from original code
#Make sure you have a unique tree identifier from the field-based data, below that is "tag" for Canoe
# 
# for (i in 1:length(date_list)){
#   date = date_list[i]
#   print(date)
#   dir = paste0(drive,":\\PARSER_Ext\\",site,"\\",L1_folder,"\\",date)
#   
#   MERGED = readLAScatalog(paste0(dir, "\\08_MERGED\\"))
#   opt_chunk_buffer(MERGED) = 0
#   opt_laz_compression(MERGED) = TRUE
#   opt_progress(MERGED) = TRUE
#   
#   ##############################################################################
#   #(6)# Clip merged cloud to individual trees 
#   ##############################################################################
#   # save each individual tree as a point cloud
#   opt_output_files(MERGED) = paste0(dir, "\\09_CROWNS\\", name, "_fam{fam}_rep{rep}_tag{tag}_treeID{treeID}") #changed Seedlot to fam, changed tree to treeID
#   CROWNS = clip_roi(MERGED, pols) 
# 
# }

##############################################################################
#(6)# Cleaning indv crowns (points with other treeIDs, and outliers)
##############################################################################
# Now we have to make sure that each tree cloud has only one treeID value;
# it seems that clip_roi does not assure this

#testing clean function below
#chunk <- "D:\\Fdc_PR_Canoe\\Flights\\2022_04_23\\2_Inputs\\lidar\\R\\output\\CROWNS_clean\\Fdc_PR_Canoe_p1_r22_t1865.laz"
future:::ClusterRegistry("stop")
plan(multisession, workers = 4L)

clean_crowns = function(chunk) {
  #removing empty
  las = readLAS(chunk)                  
  if(lidR::is.empty(las)) return(NULL) 
  
  #ensuring all points in the cloud have the same/right tree ID
  treeID_true = as.numeric(names(sort(table(las@data$treeID), decreasing = TRUE))[1])
  #print(treeID_true)
  las2 = filter_poi(las, treeID == treeID_true)
  
  #added below lines onto sams code to filter points below the 75% threshold that were not dropped preivously
  las_df = las2@data %>%
    dplyr::group_by(treeID) %>%
    dplyr::mutate(Zq99 = quantile(Z, 0.99,na.rm = TRUE)) %>%
    dplyr::mutate(Zq999 = quantile(Z, 0.999,na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  #filtering points below 75% theshold again because after looking at the crown clouds there are some left
  las3 = filter_poi(las2, Z >= quantile(Z, 0.99,na.rm = TRUE)*zq) #only keeping points above 99%*zq height percentile
  las4 = filter_poi(las3, Z <= quantile(Z, 0.999,na.rm = TRUE)) #removing points too high
}

for (i in 1:length(date_list)){
  date = date_list[i]
  print(date)
  dir = paste0(drive,":\\PARSER_Ext\\",site,"\\",L1_folder,"\\",date)
  
  CROWNS = readLAScatalog(paste0(dir, "\\09_CROWNS\\"), filter = "-drop_withheld")
  opt_chunk_size(CROWNS) = 0 # processing by files
  opt_laz_compression(CROWNS) = TRUE
  opt_chunk_buffer(CROWNS) = 0 # no buffer
  opt_wall_to_wall(CROWNS) = TRUE # disable internal checks to ensure a valid output. Free wheel mode
  opt_output_files(CROWNS) = paste0(dir, "\\10_CROWNS_clean\\{*}")
  
  CROWNS_clean = catalog_apply(CROWNS, clean_crowns)
  
}

##############################################################################
#(6)# Key Metrics function: looping through indv las files to add tag
##############################################################################
future:::ClusterRegistry("stop")
plan(multisession, workers = 2L)

## the tag number is written into the file names but not into the las df, below will extract the tag names from the file names and make a tag variable in the las file:
# Function to extract tag value from file name
extract_tag <- function(file_path) {
  # Extract the file name without extension
  file_name <- tools::file_path_sans_ext(file_path)
  
  # Split the file name by underscores
  name_parts <- unlist(strsplit(file_name, "_"))
  
  # Find and extract the tag value
  tag_index <- grep("tag", name_parts)
  if (length(tag_index) > 0) {
    tag_value <- as.numeric(gsub("tag", "", name_parts[tag_index]))
    return(tag_value)
  } else {
    return(NA)  # Return NA if tag is not found in the file name
  }
}


#date <- "2022_04_23"
# date <- "2022_11_28"
date <- "2023_06_21"
date <- "2023_12_17" # puased for comp restart at [1] 2247

date
#las_files <- "G:\\PARSER_Ext\\Fdc_PR_Canoe\\Canoe_L1\\2022_04_23\\10_CROWNS_clean_treeID_noNULL\\Canoe_fam61_rep28_tag2753_treeID2355.laz"
las_files <- list.files(paste0("E:\\Canoe\\L1_metrics\\",date,"\\10_CROWNS_clean"), pattern = "\\.laz$", full.names = TRUE)
#above folder is 10_CROWNS_clean_treeID_noNULL for 2022_04_24
#trees that have thrown alpha shape errors: 

error_tags <- c(2753, 631, 863) # 2022_04_23 Canoe old script, before remove na in vol calcs
error_tags <-c(956, 1267, 631, 863) #2022_11_28 Canoe old script, before remove na in vol calcs
error_tags <- c(2753, 631, 863) #2023_06_21 Canoe old script, before remove na in vol calcs
error_tags <- c(1855, 1321, 656, 1890, 1436)#
error_tags <- c(1792, 840,1971)#2022_11_28 Canoe: #1] 1792
                              # Error in alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE, eps = 1e-09) : 
                              # NA/NaN/Inf in foreign function call (arg 2)
                              # In addition: There were 50 or more warnings (use warnings() to see the first 50)
                              # [1] 840
                                #Error in alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE, eps = 1e-09) : 
                                # NA/NaN/Inf in foreign function call (arg 2)

                              # 1971
                              # Error in alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE, eps = 1e-09) : 
                              #   NA/NaN/Inf in foreign function call (arg 2)

error_tags <- c(1792, 840, 1971)#2023_06_21 Canoe
                  #1792
                  # Error in alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE, eps = 1e-09) : 
                  # NA/NaN/Inf in foreign function call (arg 2)
                  # [1] 840
                  # Error in alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE, eps = 1e-09) : 
                  # NA/NaN/Inf in foreign function call (arg 2)
                  # [1] 1971
                  #Error in alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE, eps = 1e-09) : 
                  # NA/NaN/Inf in foreign function call (arg 2)

error_tags <- c(1792, 840, 1971)#2023_12_17 Canoe
                        # [1] 1792
                        # Error in alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE, eps = 1e-09) : 
                        #   NA/NaN/Inf in foreign function call (arg 2)
                        # [1] 840
                        # Error in alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE, eps = 1e-09) : 
                        #   NA/NaN/Inf in foreign function call (arg 2)
                        # [1] 1971
                        # Error in alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE, eps = 1e-09) : 
                        #   NA/NaN/Inf in foreign function call (arg 2)

num =0
#stopwatch started at 5.5%
# Process each LAS file and add a "tag" column
for (las_file in las_files) {
  num = num + 1
  print(paste0(date,", percent done: ", num*100/length(las_files)))
  las <- readLAS(las_file)
  # Extract tag value from the file name
  tag_value <- extract_tag(las_file)
  print(tag_value)
  
  dir <- paste0("E:\\Canoe\\L1_metrics\\",date)
  metrics_path <- paste0(dir,"\\10_CROWNS_clean\\Structural_metrics\\",name,"_", date,"_structuralMetrics_tag",tag_value,".rds") #USED PERT= TRUE : https://cran.r-project.org/web/packages/alphashape3d/alphashape3d.pdf
  
  if(!file.exists(metrics_path) & !tag_value %in% c(error_tags)){ #making sure metrics have not already been calculated and the known error trees are removed for alter troubleshooting
    # Read the LAS file
    n_points = length(las@data$Z)
    
    if (n_points <= 15){
      
      structural_metrics_df <- data.frame(
        treeID = unique(las@data$treeID), #added by Olivia
        tag = tag_value, #added by Olivia
        #### Crown size
        n_points = length(las@data$Z),
        
        Zq99 = as.numeric(quantile(las@data$Z, 0.990,na.rm = TRUE)),#### Crown height
        Zq975 = as.numeric(quantile(las@data$Z, 0.975,na.rm = TRUE)),
        Zq95 = as.numeric(quantile(las@data$Z, 0.95,na.rm = TRUE)),
        Zq925 = as.numeric(quantile(las@data$Z, 0.925,na.rm = TRUE)),
        Z_mean = mean(las@data$Z,na.rm = TRUE),
        
        
        
        vol_convex = NA,
        vol_concave = NA,
        vol_a05= NA,
        
        #apex_angle = ang$fn1,
        #apex_sd = ang$fn2,
        
        #### Crown complexity
        CV_Z = NA,
        rumple = NA, #rumple: ratio of canopy outer surface area to ground surface area() as measured by the CHM and DTM
        CRR =NA
      )
    }else{
      Z = las@data$Z
      
      chm = grid_metrics(las, func = ~max(Z,na.rm = TRUE), res = 0.05) #grid_metrics, replaced by pixel_metrics :https://github.com/r-lidar/lidR/releases
      chm_mean = grid_metrics(las, func = ~mean(Z,na.rm = TRUE), res = 0.2)
      chm_mean[chm_mean < (maxValue(chm_mean))] = NA 
      chm_mean_trim = raster::trim(chm_mean)
      
      #chm_mean_trim = terra::trim(chm_mean) #using terra instead of trim from raster
      
      #apex angles are a measure of concality
      apex = clip_roi(las, extent(chm_mean_trim))@data %>% #ext instead of extent since we now have a SpatRaster from terra
        dplyr::filter(Z == max(.$Z))
      
      origin = c(apex$X, apex$Y, apex$Z)
      a_point = c(apex$X, apex$Y, 0)
      
      # myangle3d = function(b1, b2, b3) {
      #   aRchi::angle3d(o = c(origin[1], origin[2], origin[3]),
      #                  a = c(a_point[1], a_point[2], a_point[3]),
      #                  b = c(b1, b2, b3))
      # }
      
      library(dplyr) #had to add here for vars() to work
      # ang = las@data %>%
      #   dplyr::select(X, Y, Z) %>%
      #   rowwise() %>% 
      #   mutate(angle = myangle3d(b1 = X, b2 = Y, b3 = Z)) %>% 
      #   dplyr::ungroup() %>% 
      #   dplyr::summarise_at(vars(angle), list <- c(mean, sd), na.rm = TRUE) #vars() has been updated to across() starting with dplyr version 1.0.0
      # 
      # # alphashadep3d
      a3d <-  cbind(las@data$X, las@data$Y, las@data$Z)
      
      #a3d <- as.matrix(a3d)
      a3d[,1] = a3d[,1] - mean(a3d[,1],na.rm = TRUE) #center points around 0,0,0
      a3d[,2] = a3d[,2] - mean(a3d[,2],na.rm = TRUE) #center points around 0,0,0
      a3d[,3] = a3d[,3] - mean(a3d[,3],na.rm = TRUE) # sams orginal code did not center the Z values, but that is the only way we found to not get an error thrown in the ashape3d function.
      shape_convex = alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE,eps = 1e-09)
      shape_concave = alphashape3d::ashape3d(x = a3d, alpha = 1, pert = TRUE,eps = 1e-09)
      shape_a05 = alphashape3d::ashape3d(x = a3d, alpha = 0.5, pert = TRUE,eps = 1e-09)
      
      structural_metrics_df <- data.frame(
        treeID = unique(las@data$treeID), #added by Olivia
        tag = tag_value, #added by Olivia
        #### Crown size
        n_points = length(las@data$Z),
        
        Zq99 = as.numeric(quantile(Z, 0.990,na.rm = TRUE)),#### Crown height
        Zq975 = as.numeric(quantile(Z, 0.975,na.rm = TRUE)),
        Zq95 = as.numeric(quantile(Z, 0.95,na.rm = TRUE)),
        Zq925 = as.numeric(quantile(Z, 0.925,na.rm = TRUE)),
        Z_mean = mean(Z,na.rm = TRUE),
        
        
        
        vol_convex = alphashape3d::volume_ashape3d(shape_convex), #was inf changed to "all"
        vol_concave = alphashape3d::volume_ashape3d(shape_concave),
        vol_a05= alphashape3d::volume_ashape3d(shape_a05),
        
        #apex_angle = ang$fn1,
        #apex_sd = ang$fn2,
        
        #### Crown complexity
        CV_Z = sd(Z,na.rm = TRUE) / mean(Z,na.rm = TRUE),
        rumple = lidR::rumple_index(chm), #rumple: ratio of canopy outer surface area to ground surface area() as measured by the CHM and DTM
        CRR = (mean(Z,na.rm = TRUE) - min(Z,na.rm = TRUE)) / (max(Z,na.rm = TRUE) - min(Z,na.rm = TRUE))
      )
      
    }
    
    if (!dir.exists(paste0(dir,"\\10_CROWNS_clean\\Structural_metrics\\"))) {
      dir.create(paste0(dir,"\\10_CROWNS_clean\\Structural_metrics\\"), recursive = TRUE)
    }
    #df_structural = structural_metrics@data
    saveRDS(structural_metrics_df, paste0(dir,"\\10_CROWNS_clean\\Structural_metrics\\",name,"_", date,"_structuralMetrics_tag",tag_value,".rds")) #USED PERT= TRUE : https://cran.r-project.org/web/packages/alphashape3d/alphashape3d.pdf
    
  }
  
}
