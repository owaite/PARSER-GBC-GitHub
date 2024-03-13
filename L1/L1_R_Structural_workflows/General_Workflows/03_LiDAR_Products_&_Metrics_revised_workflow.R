# packages:
library(tidyverse)
library(sf)
library(sp)
library(spatial)
library(rgdal)###
library(rgeos)###
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
library(aRchi)###
library(lwgeom)
library(dplyr)

## (0) processed in terra v3.9
## (1) In LAStools: projected to nad83 and offset to ease initial manual alignment in CC
## (1) Align unthinned cloud in CC
## (2) In LAStools: rescale, index and tile
## (3) in R:  thin with voxels, create 10cm DTM, normalize cloud, isolate individual crown point clouds, calculate metrics-##############


# The folder structure throughout this code contains folders per date of data acquisition
# IE: paste0(drive,":\\",site,"\\",L1_folder,"\\",date,"\\04_RASTER") where date is the name of a folder that is the data aquisition date in the form "YYYY_mm_dd"
# Hence, for loops loop through the dates, change the format to match your folder structure

# Initiate multiple workers to speed up processing
future:::ClusterRegistry("stop")
plan(multisession, workers = 4L)

##############################################################################
# (1) Read in edited crown polygons for metric calculation: 
##############################################################################
# Canoe polygon section: 
# ie of a path : to Canoe: "E:\\Canoe\\Crowns\\Edited_added\\CHM_.06_masked_RGB_H20T_mosaic_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss.sig.01w3_z75_crowns_EDITED_NEWmissAdded.shp"
pols = st_read("path_to_crown_polygon_shp", crs = 26910) %>% # setting crs to EPSG 26910
  filter(!st_is_empty(.)) %>% #remove polygons with an area of zero
  st_make_valid() %>%  #make the polygons valid
  st_cast("POLYGON") #cast the geometries to polygons

# check to make sure that there are no multiple geometries
# the df pols_dup should come out empty if there are no duplicates
pols_dup = pols %>% 
  group_by(treeID) %>% #group_by treeID, there should only be one tree for every treeID 
  filter(n()>1) #filter for cases where there is more than one tree with the same treeID
pols_dup #print results

unique(pols_dup$treeID)#if not empty, find trees with multiple polygons
## Manually delete secondary polygons in arcgis, usually they are very small remnants of the editing process

#read in pols with duplicated edited out and ensure there is a treeID, fam, rep, and tag column
pols = st_read("path to poly _duplicatesRemoved") %>% 
  filter(!st_is_empty(.)) %>% 
  st_make_valid() %>% 
  st_cast("POLYGON")%>%
  st_crs(26910) #assign crs of EPSG 26910

#double check no duplicates remaining:
pols_dup = pols %>% 
  group_by(treeID) %>% 
  filter(n()>1)
pols_dup #this df should be empty, if it is not, then the remaining trees have duplicate treeIDs

#ensure polygon has a"fam", "rep", "treeID" and "tag" column for point cloud labeling later on
colnames(pols)
pols$treeID 
pols$tag = pols$tag__
pols$fam 
pols$rep


##############################################################################
# (2.1) Define function for section 3 to attach treeID info to points and filter based on height
##############################################################################
#zq set the % height to filter by, ie zq = 0.75 removes the bottom 75% of the tree and keeps the top 25%
polys_to_las = function(chunk, zq = 0.75, polygons = pols) {
  
  las = readLAS(chunk)                  
  if (lidR::is.empty(las)) {
    return(NULL) }# If the LAS file is empty, return NULL
  
  #polys_crop = raster::crop(polys, extent(las))
  las2 = merge_spatial(las, polygons, "treeID") # Merge the LAS points with the polygons based on the treeID attribute
  
  
  las_df = las2@data %>%
    dplyr::group_by(treeID) %>%
    dplyr::mutate(Zq999 = quantile(Z, 0.999)) %>% # compute Zq999 (the 99.9th percentile of Z) for each tree 
    dplyr::mutate(treeID = if_else(Z > quantile(Z, 0.999) * zq, as.numeric(treeID), 0))# assign points below the top zq% of the tree height a treeID of zero to filter them out later
  # dplyr::mutate(treeID = if_else(!is.na(Z) && Z > (Zq999 * zq), as.numeric(treeID), 0))
  
  las3 = las2 %>% 
    add_lasattribute(las_df$treeID, name = "treeID", desc = "treeID") %>% #add treeID las a lasattribute
    filter_poi(treeID > 0) #filter for points with a treeID greater than 0 
  
  if (lidR::is.empty(las3)) {
    return(NULL)#return NULL is the las3 is empty
  } else {
    return(las3)#return the las file that has a treeID associated with it and only contains points in the top zq%
  }
  
}

##############################################################################
# (2.2) Define function for section 3 to :
# (a) only keep points labelled with the correct treeID per tree 
# (b) ensure that only top 25% of tree remains (change this % by changing the zq value in the for loop in (3.3) below
##############################################################################
clean_crowns = function(chunk) {
  las = readLAS(chunk) #reading in las file                 
  if(lidR::is.empty(las)) return(NULL) #removing empty
  
  #ensuring all points in the cloud have the same/right tree ID
  treeID_true = as.numeric(names(sort(table(las@data$treeID), decreasing = TRUE))[1])
  las2 = filter_poi(las, treeID == treeID_true)
  
  #(added to Sam's code) filter points below the 75% threshold that were not dropped previously
  las_df = las2@data %>%
    dplyr::group_by(treeID) %>%
    dplyr::mutate(Zq99 = quantile(Z, 0.99)) %>%
    dplyr::mutate(Zq999 = quantile(Z, 0.999)) %>%
    dplyr::ungroup()
  
  las3 = filter_poi(las2, Z >= quantile(Z, 0.99)*zq) #only keeping points above 99%*zq height percentile
  las4 = filter_poi(las3, Z <= quantile(Z, 0.999)) #removing points too high
}


##############################################################################
# (3) For loop that outputs individual tree point clouds that can be input into the key_metrics function
##############################################################################
# list of dates of data acquisitions that match date folder names
date_list = c("2022_11_28","2023_06_21","2023_12_17")

# Key_metrics are run outside the loop to avoid stops in processing due to crown point clouds throwing errors in metric calculation
# Iterating over dates to:
# (3.1) create sub folders for following steps
# (3.2) create DTMs
# (3.3) normalize point clouds
# (3.4) segment point cloud and label points with treeIDs
# (3.5) merge all tiles (that now have points labelled with treeIDs)
# (3.6) clip individual tree point clouds and "clean" to ensure they only contain the correct points (based on height and treeID)

for (i in 1:length(date_list)){
  
  date = date_list[i]
  
  site = "Fdc_PR_Canoe" 
  L1_folder = "L1_metrics" #name of the folder containing sub folder of Lidar data per acquisition
  dir = paste0(drive,":\\",site,"\\",L1_folder,"\\",date) #path to lidar data per date
  
  ##############################################################################
  # (3.1) Create output file directories if they don't exist
  ##############################################################################
  if (!dir.exists(paste0(dir, "\\04_RASTER"))) {
    dir.create(paste0(dir, "\\04_RASTER"),recursive = TRUE)
  }
  if (!dir.exists(paste0(dir, "\\05_NORM"))) {
    dir.create(paste0(dir, "\\05_NORM"),recursive = TRUE)
  }
  if (!dir.exists(paste0(dir, "\\06_NORM_clean"))) {
    dir.create(paste0(dir, "\\06_NORM_clean"),recursive = TRUE)
  }
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
  
  ##############################################################################
  #(3.2) Create 10cm DTMs (Ditgital Terrain Model) - ground identified in DJI terra - thinned to 1cm below
  # for future - don't think the point cloud for the ground, keep all possible points
  ##############################################################################
  GROUND = readLAScatalog(folder = paste0(dir, "\\03_tile\\"), filter = "drop_withheld")
  opt_filter(GROUND) <- "-drop_withheld"   # set filtering options for the LAScatalog object to drop withheld points
  opt_filter(GROUND) = "-thin_with_voxel 0.01" # thin points with a voxel size of 1cm
  opt_chunk_buffer(GROUND) = .5   # set the buffer size for chunks in the LAScatalog object to 0.5m
  opt_laz_compression(GROUND) = TRUE   # enable LAZ compression for the LAScatalog object
  opt_output_files(GROUND) = ""   # set output file options for the LAScatalog object to empty
  opt_progress(GROUND) = TRUE # enable progress tracking for processing


  # Create a DTM from the ground points
  DTM = grid_terrain(GROUND, res = 0.1, tin(), full_raster = FALSE) %>% 
    # applying a focal operation to the DTM raster.
    # computing the mean value within a moving window defined by a matrix.
    focal(w = matrix(1, 25, 25),  # define a 25x25 window with all values as 1
          fun = mean,             # use the mean function to compute the focal statistic
          na.rm = TRUE,           # remove NA values from computation
          pad = TRUE)             # pad the edges of the raster with NAs to maintain the original extent
  
  crs(DTM) <- CRS("+proj=utm +zone=10 +datum=NAD83") # assign a CRS to the raster using the proj4 string representation

  writeRaster(DTM, paste0(dir, "\\04_RASTER\\", site, "_DTM_0.1m.tif"), overwrite = TRUE) #save the DTM
  
  ##############################################################################
  #(3.3) Normalize Cloud - thinned to 1cm below
  ##############################################################################
  
  # read the tiles again, but with high-res parameters
  CTG = readLAScatalog(folder = paste0(dir, "\\03_tile\\"))
  opt_chunk_buffer(CTG) = .5 # small buffer; we're just normalizing
  opt_laz_compression(CTG) = TRUE
  opt_filter(CTG) = "-thin_with_voxel 0.01" # 1cm voxel thinning
  opt_output_files(CTG) = paste0(dir, "\\05_NORM\\{*}_NORM") #saving out normalized individual laz files with an extension of _NORM to the 05_NORM folder
  opt_progress(CTG) = TRUE
  
  # normalize 
  NORM = normalize_height(CTG, DTM, na.rm = TRUE)#normalizing the laz files in the CTG (las catalog) to the previously made DTM
  
  # read them in normalized laz files and filter out points below -0.25
  NORM = readLAScatalog(paste0(dir, "\\05_NORM\\"))
  opt_chunk_buffer(NORM) = 0  # no buffer, just filtering
  opt_laz_compression(NORM) = TRUE
  opt_filter(NORM) = "-drop_z_below -.25" #drop points below -25cm
  opt_output_files(NORM) = paste0(dir, "\\06_NORM_clean\\{*}") #save the filtered normalized filed to the "06..." folder
  opt_progress(NORM) = TRUE #show the progress
  
  # now write over the NORM files but filter points less than -.5
  NORM_clean = catalog_retile(NORM) # applying the filter (of dropping points below -0.25) to all files in the catalog
  
  #### Making CHMS
  CHM_max = grid_metrics(NORM_clean, #NORM_clean is the catalog the CHM is being made from
                         res = 0.04, #4cm resolution
                         func = ~max(Z)) #using max Z values
  
  crs(CHM_max) <- CRS("+proj=utm +zone=10 +datum=NAD83") # assign a CRS to the raster using the proj4 string representation
  writeRaster(CHM_max, paste0(dir, "\\04_RASTER\\", site, "_CHM_max_0.04m.tif"), overwrite = TRUE) #saving out the CHM
  
  ##############################################################################
  #(3.4) Segment NORM_clean cloud - goes tile by tile and attaches treeID info to points and filters based on height
  ##############################################################################
  
  # read in norm cloud
  NORM = readLAScatalog(paste0(dir, "\\06_NORM_clean\\"))
  crs(NORM) <- st_crs(26910) #setting CRS
  opt_chunk_buffer(NORM) = 0 #no buffer
  opt_laz_compression(NORM) = TRUE #compress output files to .laz objects
  opt_output_files(NORM) = paste0(dir, "\\07_SEGMENTED\\{*}_SEGMENTED") #write output files to "07_SEG..." folder and name with suffix _SEGMENTED
  opt_progress(NORM) = TRUE #show progress
  
  
  zq = 0.75 # height percentile, here we will be filtering for the top 25% of the tree 
  
  
  
  # new chunks have only segmented portions of tree crowns
  SEGMENTED = catalog_apply(NORM, polys_to_las)#apply the poly_to_las function to the NORM catalog
  
  print("segmented")#print out "segmented" so you know where the code is at
  
  ##############################################################################
  #(3.5) Merge all segmented tiles (will only have points left that are attributed to top 25% of each tree, need to merge because some trees have been split in the tiling process
  ##############################################################################
  
  SEGMENTED = readLAScatalog(paste0(dir, "\\07_SEGMENTED\\"))
  opt_chunk_buffer(SEGMENTED) = 0 #zero buffer
  opt_chunk_size(SEGMENTED) = 10000 #set the chunk size for the LAScatalog object to 10000 points so that all laz files are merged into one large laz file
  opt_laz_compression(SEGMENTED) = TRUE
  opt_progress(SEGMENTED) = TRUE
  opt_output_files(SEGMENTED) = paste0(dir, "\\08_MERGED\\", site, "_HULLS_merged") #write output files to "08_M..." with the suffix "_HULLS_merged"
  
  # merge all the segmented trees into a single point cloud 
  MERGED = catalog_retile(SEGMENTED) #apply the above opt_ commands
  print("merged") #print statement to show where the code is at
  
  ##############################################################################
  #(3.6) Clip merged cloud to individual trees and "clean" the tree point clouds
  ##############################################################################
  # save each individual tree as a point cloud
  print("making crowns")#print statement to show where the code is at
  opt_output_files(MERGED) = paste0(dir, "\\09_CROWNS\\", site, "_fam{fam}_rep{rep}_tag{tag}_treeID{treeID}") #where the indivdual crown point clouds will be written to and the suffix they will have. Note values assoaciated with the laz file will replace the names in {}
  CROWNS = clip_roi(MERGED, pols) #clip the MERGED laz file to individual crowns using the crown polygons 
  
  ## CLEANING crowns
  CROWNS = readLAScatalog(paste0(dir, "\\09_CROWNS\\"), filter = "-drop_withheld") #read in the clipped crown point clouds
  opt_chunk_size(CROWNS) = 0 # processing by files
  opt_laz_compression(CROWNS) = TRUE
  opt_chunk_buffer(CROWNS) = 0 # no buffer
  opt_wall_to_wall(CROWNS) = TRUE # disable internal checks to ensure a valid output. Free wheel mode
  opt_output_files(CROWNS) = paste0(dir, "\\10_CROWNS_clean\\{*}") #location for output files
  print("cleaning crowns")
  CROWNS_clean = catalog_apply(CROWNS, clean_crowns) #applying the clean_crowns function on the CROWNS
}


################################################################################
#(4) Key Metrics Calculation:
# we ran this step as a loop to write a rds metric file per tree .laz file to not have 
# to re run all trees if an error was thrown by a single tree 
# the code was also written to extract the "tag" value that had been written into
# the .laz files names in step 3.6 above and add it to the df 
################################################################################
future:::ClusterRegistry("stop") #stopping current multisession workers
plan(multisession, workers = 2L) # initiating 2 workers for below process

#getting a list of the laz file names in the 10_CROWNS_clean folder
las_files <- list.files(paste0("E:\\Canoe\\L1_metrics\\",date,"\\10_CROWNS_clean"), pattern = "\\.laz$", full.names = TRUE)


# The tag number is written into the .laz file names but is not an attribute in the file
# We decided after the fact that we wanted the tag value as an attribute, so created the
# extract_tag function below 
# the extract_tag function was built to extract the tag value from a filename in this fomat:
#"E:\\Canoe\\L1_metrics\\2022_11_28\\10_CROWNS_clean/Canoe_fam75_rep17_tag1642_treeID1173.laz"

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


date <- "2023_12_17" # Date you would like to run the LiDAR metrics for

# As the below for loop runs it may encounter an error when calculating the metrics for certain trees. 
# Keep a note of the error and the tag ID (below 3 lines is an example of the two lines i copy from the console and paste into a word doc to keep track of:
## i.e.: tag: 1971
## Error in alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE, eps = 1e-09) : 
## NA/NaN/Inf in foreign function call (arg 2)

# Add the tag number into the error_tags vector and rerun the loop after initiating num = 0 again
# This will skip the tree that threw the error so you can go back later and troubleshoot the specific point clouds that threw errors (there should only be a few if any)

error_tags <- c()#initiate this empty vector, and fill this vector with tag values printed in the console as error trees are encountered


num =0 #initiating an iterator that helps show progress in below for loop

# For loop to process metrics for each laz file and add a "tag" column to the metrics df
# This loop processes each tree separately and writes out rds files per tree
# this was done to make it easy to rerun if a tree threw an error and stopped the loop, without having to rerun already calculated metrics for non-problem trees
for (las_file in las_files){
  num = num + 1
  print(paste0(date,", percent done: ", num*100/length(las_files)))#prints progress
  las <- readLAS(las_file)#read in the .laz file for an individual crown
  tag_value <- extract_tag(las_file)# extract tag value from the file name
  print(tag_value)
  
  dir <- paste0("E:\\Canoe\\L1_metrics\\",date) #directory to crowns folder
  metrics_path <- paste0(dir,"\\10_CROWNS_clean\\Structural_metrics\\",name,"_", date,"_structuralMetrics_tag",tag_value,".rds") # location where metrics will be written to 
  
  if(!file.exists(metrics_path) & !tag_value %in% c(error_tags)){ #making sure metrics have not already been calculated and the known error trees are removed for later troubleshooting
    n_points = length(las@data$Z) # Read the LAS file
    if (n_points <= 15){#if the laz file has less than 15 points
      structural_metrics_df <- data.frame(
        treeID = unique(las@data$treeID), 
        tag = tag_value, 
        #### Crown size
        n_points = length(las@data$Z),
        Zq99 = as.numeric(quantile(las@data$Z, 0.990,na.rm = TRUE)),# 99th percentile
        Zq975 = as.numeric(quantile(las@data$Z, 0.975,na.rm = TRUE)), # 97.5th percentile
        Zq95 = as.numeric(quantile(las@data$Z, 0.95,na.rm = TRUE)), # 95th percentile
        Zq925 = as.numeric(quantile(las@data$Z, 0.925,na.rm = TRUE)), # 92.5th percentile
        Z_mean = mean(las@data$Z,na.rm = TRUE), # mean z value
        
        # Not calculating any volume metrics for clouds with < 15 points
        vol_convex = NA,
        vol_concave = NA,
        vol_a05= NA,
        
        # Crown complexity, not calculating for clouds with < 15 points
        CV_Z = NA,
        rumple = NA, #rumple: ratio of canopy outer surface area to ground surface area() as measured by the CHM and DTM
        CRR =NA
      )
    }else{
      Z = las@data$Z # Z values of each point in the .laz cloud
      
      chm = grid_metrics(las, func = ~max(Z,na.rm = TRUE), res = 0.05) # 5cm CHM with max Z values #grid_metrics, replaced by pixel_metrics :https://github.com/r-lidar/lidR/releases
      chm_mean = grid_metrics(las, func = ~mean(Z,na.rm = TRUE), res = 0.2) # 20cm CHM with mean Z values
      chm_mean[chm_mean < (maxValue(chm_mean))] = NA # pixels of chm_mean that are less than the max value of chm_mean are set to NA
      chm_mean_trim = raster::trim(chm_mean) # creating a new raster (chm_mean_raster) with teh extent of chm_mean
      #chm_mean_trim = terra::trim(chm_mean) #using terra instead of trim from raster
      
      #apex angles are a measure of concality
      # Extract points from the las that fall within the extent of 'chm_mean_trim',
      apex = clip_roi(las, extent(chm_mean_trim))@data %>% #clipping #ext instead of extent if using a SpatRaster from terra
        dplyr::filter(Z == max(.$Z))# filter the extracted points to retain only those with Z (height) equal to the maximum height value in the dataset.

      origin = c(apex$X, apex$Y, apex$Z)# Define the coordinates of the apex point extracted from the LAS data
      a_point = c(apex$X, apex$Y, 0)# Define the coordinates of a point lying on the same XY plane as the apex but at a Z coordinate of 0

      # function 'myangle3d' to calculate the 3D angle between vectors defined by three points
      myangle3d = function(b1, b2, b3) {
        # Use the 'angle3d' function from the 'aRchi' package to calculate the angle in 3D space
        # between the vectors formed by points 'origin' (apex), 'a_point', and 'b1', 'b2', 'b3'
        aRchi::angle3d(o = c(origin[1], origin[2], origin[3]),
                       a = c(a_point[1], a_point[2], a_point[3]),
                       b = c(b1, b2, b3))
      }
      
      library(dplyr) #needed to re read in the package in the loop here for vars() to work
      # Extract the X, Y, and Z coordinates from the LAS data and calculate the angle for each point
      # using the custom function 'myangle3d'. Then summarize the angle values.
      ang = las@data %>%
        dplyr::select(X, Y, Z) %>%
        rowwise() %>%
        mutate(angle = myangle3d(b1 = X, b2 = Y, b3 = Z)) %>%
        dplyr::ungroup() %>%
        dplyr::summarise_at(vars(angle), list <- c(mean, sd), na.rm = TRUE) #vars() has been updated to across() starting with dplyr version 1.0.0

      # Extract X, Y, and Z coordinates from the LAS data and create a matrix 'a3d'
      a3d <-  cbind(las@data$X, las@data$Y, las@data$Z)
      
      # Center the points around the origin (0,0,0) by subtracting the mean of each dimension
      a3d[,1] = a3d[,1] - mean(a3d[,1],na.rm = TRUE) #center x values
      a3d[,2] = a3d[,2] - mean(a3d[,2],na.rm = TRUE) #center y values
      a3d[,3] = a3d[,3] - mean(a3d[,3],na.rm = TRUE) #center z values # sams orginal code did not center the Z values, but that is the only way we found to not get an error thrown in the ashape3d function.
      
      # Generate different shapes using the alphashape3d package
      shape_convex = alphashape3d::ashape3d(x = a3d, alpha = Inf, pert = TRUE,eps = 1e-09)# Compute a convex hull using alpha = Inf (convex hull) #USED PERT= TRUE : https://cran.r-project.org/web/packages/alphashape3d/alphashape3d.pdf
      shape_concave = alphashape3d::ashape3d(x = a3d, alpha = 1, pert = TRUE,eps = 1e-09)# Compute a concave hull using alpha = 1 (concave hull)
      shape_a05 = alphashape3d::ashape3d(x = a3d, alpha = 0.5, pert = TRUE,eps = 1e-09)# Compute a shape for alpha = 0.5 (balanced between convex and concave)
      
      structural_metrics_df <- data.frame(
        treeID = unique(las@data$treeID), 
        tag = tag_value,
        n_points = length(las@data$Z),
        
        #### Crown height
        Zq99 = as.numeric(quantile(Z, 0.990,na.rm = TRUE)),# 99th percentile
        Zq975 = as.numeric(quantile(Z, 0.975,na.rm = TRUE)), # 97.5th percentile
        Zq95 = as.numeric(quantile(Z, 0.95,na.rm = TRUE)),# 95th percentile
        Zq925 = as.numeric(quantile(Z, 0.925,na.rm = TRUE)), # 92.5th percentile
        Z_mean = mean(Z,na.rm = TRUE), #mean crown height
        
        ### Crown volume
        vol_convex = alphashape3d::volume_ashape3d(shape_convex), # convex volume
        vol_concave = alphashape3d::volume_ashape3d(shape_concave), # concave volume
        vol_a05= alphashape3d::volume_ashape3d(shape_a05), #volume with alpha = 0.5 (balance between concave and convex)
        
        #apex_angle = ang$fn1,
        #apex_sd = ang$fn2,
        
        #### Crown complexity
        CV_Z = sd(Z,na.rm = TRUE) / mean(Z,na.rm = TRUE),# Compute the coefficient of variation (CV) of Z values, representing the variability of heights relative to the mean height
        rumple = lidR::rumple_index(chm), #rumple: ratio of canopy outer surface area to ground surface area as measured by the CHM and DTM
        CRR = (mean(Z,na.rm = TRUE) - min(Z,na.rm = TRUE)) / (max(Z,na.rm = TRUE) - min(Z,na.rm = TRUE))# Compute the Canopy Rugosity Ratio (CRR), 
        # representing the ruggedness or roughness of the canopy surface
      )
      
    }
    #Create structural metrics folder
    if (!dir.exists(paste0(dir,"\\10_CROWNS_clean\\Structural_metrics\\"))) {
      dir.create(paste0(dir,"\\10_CROWNS_clean\\Structural_metrics\\"), recursive = TRUE)
    }
    #writing out .rds files
    saveRDS(structural_metrics_df, paste0(dir,"\\10_CROWNS_clean\\Structural_metrics\\",name,"_", date,"_structuralMetrics_tag",tag_value,".rds")) #USED PERT= TRUE : https://cran.r-project.org/web/packages/alphashape3d/alphashape3d.pdf
    
  }
  
}