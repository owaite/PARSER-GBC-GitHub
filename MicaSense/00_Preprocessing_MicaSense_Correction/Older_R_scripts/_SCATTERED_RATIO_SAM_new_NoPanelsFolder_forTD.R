# Pre-processing of Micasense images before calibration and orthophoto
# generation in Metashape

# (1) read XMP data into R format and save as RDS
# (2) compute sun-sensor angle for all photos 
# (3) estimate scattered:direct ratio for each flight
# (4) compute horizontal (corrected) irradiance for the DLS for all photos
# (5) select the best calibration panel to use for each flight


library(terra)
library(exifr)
library(tidyverse)
library(ggplot2)
library(raster)
library(lubridate)
library(exifr)
library(pracma)
library(RcppCNPy)
library(zoo)
library(forecast)
library(fable)
library(ggpmisc)
library(broom)
library(tidyfit)
library(rsample)

#libraries to parallize:
# Load required packages
library(foreach)
library(snow)
library(parallel)
library(iterators)
library(magrittr)  # For the pipe operator %>%


################################################################################

# assumes that image directories have been copied with "_save" for originals
# assumes that panels are in a separate subdirectory called '\\PANELS'
# assumes that masks have been exported for all panels to '\\MASKS'

# folder pattern: dir \\ DAP\\MS\\ flight_date_save \\ PANELS \\ panel_img.tif
dir <- "G:\\PARSER_Ext\\Fdc_PR_Canoe\\Flights\\"#"I:\\PARSER_Ext\\Fdc_PR_Canoe\\Flights\\"
dir_masks <- "D:\\Sync\\Sync\\Fdc_PR_Canoe\\Flights\\"

#date_list <- c("2022_07_06")#,

# done = done exif copying (1st step)
date_list <-  c(
  "2022_04_23", #done_exif #corrected
  "2022_05_08", #done_exif #corrected
)
date_list               
date_range <- "2022_09_08-2022_10_20" #a string that will be used for naming purposes

ext_save <- "_save" #suffix of uncorrected original folder of images 


################################################################################
# (1) read XMP data into R format and save as RDS

# make an empty dataframe to add XMP data to 
df_all = data.frame()

for (x in seq_along(date_list)){
  
  for (j in 1:10){ # bands 1 through 10 
    
    pics = list.files(paste0(dir, date_list[x],"\\1_Data\\Micasense_Cleaned",ext_save), #path the MS_original that is copied
                      pattern = c(paste0("IMG_...._", j, ".tif"), paste0("IMG_...._", j, "_", ".tif")), recursive = TRUE, 
                      full.names = TRUE) # list of directories to images
    
    #getting names of masks
    mask_images <- list.files(paste0("D:\\Sync\\Sync\\Fdc_PR_Canoe\\Flights\\",date_list[x],"\\2_Inputs\\metashape\\MASKS\\")) #path to folder with masks
    mask_img_names <- gsub("_mask\\.png$", "", mask_images)
    
    for (i in 1:length(pics)){
      pic = pics[i]
      pic_root <- gsub(".*/(IMG_.*)(\\.tif)$", "\\1", pic)
      print(pic_root)
      
      if (substr(pic_root,11,11) == "0") { #distinguishes between _1 and _10
        band = substr(pic_root,10,11) # for _10
      } else {
        band = substr(pic_root,10,10)
      } # for _1
      
      img_exif = read_exif(pic) # read the XMP data 
      print(paste0(date_list[x], " band ", j, " ", i, "/", length(pics))) # keep track of progress
      
      #creating df with same column names as exif data
      if (i == 1){ # if it's the first image, make new df from XMP data 
        exif_df = as.data.frame(img_exif) %>% 
          mutate(panel_flag = ifelse( gsub(".*/(IMG_.*)(\\.tif)$", "\\1", SourceFile) %in% mask_img_names, 1, 0))
      } else {   # otherwise add each new image exif to dataframe
        exif_df = merge(exif_df, img_exif, by = intersect(names(exif_df), names(img_exif)), all = TRUE)%>%
          mutate(panel_flag = ifelse( gsub(".*/(IMG_.*)(\\.tif)$", "\\1", SourceFile) %in% mask_img_names, 1, 0))
      }
    }
    if(!dir.exists(paste0(dir,date_list[x],"\\1_Data\\CSV\\"))){ #creating CSV folders
      dir.create(paste0(dir,date_list[x],"\\1_Data\\CSV\\"))
    }
    if(!dir.exists(paste0(dir,date_list[x],"\\1_Data\\CSV\\Corrected_values\\"))){ #Creating Corrected_Values folder
      dir.create(paste0(dir,date_list[x],"\\1_Data\\CSV\\Corrected_values\\"))
    }
    saveRDS(exif_df, paste0(dir,date_list[x], "\\1_Data\\CSV\\Corrected_values\\df_", date_list[x], "_", j, ".rds")) # save output for each band, set path
  } 
}

#############checking panels were flagged properly and if not, fixing them: ---------------------------------------------------------
# chekcing panle_flag:
x = 2
j = 1
test1 <- read_rds(paste0(dir,date_list[x], "\\1_Data\\CSV\\Corrected_values\\df_", date_list[x], "_", j, ".rds")) 
unique(test1$panel_flag)

test1[test1$panel_flag == 1, ]

for (x in 1:length(date_list)){
  for (j in c(1:10)){
    print(date_list[x])
    exif_df = read_rds(paste0(dir,date_list[x], "\\1_Data\\CSV\\Corrected_values\\df_", date_list[x], "_", j, ".rds")) # SET PATH
    
    #getting list of mask images names:
    mask_images <- list.files(paste0("D:\\Sync\\Sync\\Fdc_PR_Canoe\\Flights\\",date_list[x],"\\2_Inputs\\metashape\\MASKS\\")) #path to folder with masks
    mask_img_names <- gsub("_mask\\.png$", "", mask_images)
    
    exif_df_new <-  exif_df %>%
      mutate(panel_flag = ifelse( gsub(".*/(IMG_.*)(\\.tif)$", "\\1", SourceFile) %in% mask_img_names, 1, 0)) %>%
      saveRDS(paste0(dir,date_list[x], "\\1_Data\\CSV\\Corrected_values\\df_", date_list[x], "_", j, ".rds"))
  }
}
##### Read them in from the rds files now -----

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

saveRDS(xmp_all, paste0(dir, "CSV\\Corrected_values\\XMP_", date_range,".rds"))

unique(xmp_all_new$panel_flag)
xmp_all_new[xmp_all_new$panel_flag == 1,]$FileName
#testing panel_flag works 
# tail(xmp_all_new)
################################################################################
# (2) compute sun-sensor angle for all photos 

# Define a function to compute the DLS-Sun angle in R
# from the position of the sun,
# the roll, pitch, and yaw of the drone,
# and the orientation vector of the DLS
compute_sun_angle = function(SolarElevation, SolarAzimuth, Roll, Pitch, Yaw) {
  
  # Define the orientation vector of the DLS in body coordinates
  # DLS pointing down?
  ori = c(0, 0, -1)
  
  # Convert to numeric
  SolarElevation = as.numeric(SolarElevation)
  SolarAzimuth = as.numeric(SolarAzimuth)
  Roll = as.numeric(Roll)
  Pitch = as.numeric(Pitch)
  Yaw = as.numeric(Yaw)
  
  # Convert sun azimuth and elevation to NED (north east down) coordinates
  elements = c(
    cos(SolarAzimuth) * cos(SolarElevation),
    sin(SolarAzimuth) * cos(SolarElevation),
    -sin(SolarElevation))
  
  # transpose vector to matrix
  nSun = t(matrix(elements, ncol = 3))
  
  # Convert the sensor orientation angles (Roll, Pitch, Yaw) to a 3x3 rotation matrix
  c1 = cos(-Yaw)
  s1 = sin(-Yaw)
  c2 = cos(-Pitch)
  s2 = sin(-Pitch)
  c3 = cos(-Roll)
  s3 = sin(-Roll)
  Ryaw = matrix(c(c1, s1, 0, -s1, c1, 0, 0, 0, 1), ncol = 3, byrow = TRUE)
  Rpitch = matrix(c(c2, 0, -s2, 0, 1, 0, s2, 0, c2), ncol = 3, byrow = TRUE)
  Rroll = matrix(c(1, 0, 0, 0, c3, s3, 0, -s3, c3), ncol = 3, byrow = TRUE)
  # take the dot product of the three matrices
  R_sensor = Ryaw %*% Rpitch %*% Rroll
  
  # Compute the orientation vector of the sensor in NED coordinates
  # as the dot product of sensor orientation and sensor position relative to the drone
  nSensor = R_sensor %*% ori
  
  # Compute the angle between the sensor and the sun
  angle = acos(sum(nSun * nSensor))
  return(angle) # in radians
}


read_rds(paste0(dir, "CSV\\Corrected_values\\XMP_", date_range,".rds")) %>% # SET PATH
  rowwise() %>% 
  mutate(SunSensorAngle = compute_sun_angle(SolarElevation, SolarAzimuth, Roll, Pitch, Yaw)) %>% 
  saveRDS(paste0(dir, "CSV\\Corrected_values\\XMP_", date_range,"_with_SSA.rds"))

################################################################################
# (3) estimate scattered:direct ratio for each flight by relating cosine of the sun-sensor angle to 
# spectral irradiance measured by the DLS. This relationship, in a perfect world,
# should give you the scattered irradiance as the intercept, which is independent of angle,
# and the direct, which is the slope, and perfecntly proportional to sun angle. In reality, 
# these relatiopnships are extremely messy and most data need to be discarded.
# First, generate a rolling regression of this linear relationship over a certain time window.
# Second, drop any models with negative slopes or intercepts (physically impossible).
# Third, eliminate all models with poor fits (R^2).

# Parameters to set: the R^2 threshold (here .4), the time window for the rolling regression (here 30 seconds)

xmp_all_ssa = read_rds(paste0(dir, "CSV\\Corrected_values\\XMP_", date_range,"_with_SSA.rds")) %>% # SET PATH
  
  # EDIT THIS PART
  
  # mutate(Date = sub(".+Flights\\\\(.+?)\\\\1_Data.+", "\\1", Directory) # the Date from the directory
  #        # #panel_flag already exists subdirectory = grepl("Panels", Directory), # folder where the images are (all previous slashes are backslashes, first /)
  #        # panel_flag = if_else(subdirectory == TRUE, 1, 0)
  #        ) %>% # the only use of subdirectory is to check if images are in PANELS
  mutate(Yaw_deg = rad2deg(as.numeric(Yaw)),
         Roll_deg = rad2deg(as.numeric(Roll)),
         Pitch_deg = rad2deg(as.numeric(Pitch))) %>% 
  group_by(Date, BandName) %>% 
  arrange(ymd_hms(DateTimeOriginal)) %>% 
  mutate(GPSLatitude_plot = scale(as.numeric(GPSLatitude)), # these are for clean ggplotting, no other reason to scale
         GPSLongitude_plot = scale(as.numeric(GPSLongitude)),
         cos_SSA = cos(SunSensorAngle),
         Irradiance = as.numeric(Irradiance),
         Date2 = ymd_hms(DateTimeOriginal)) # this is the date/time we will use moving forward 

#checking panels:
head(xmp_all_ssa$Date)[1]
head(xmp_all_ssa$Date2)[1]
# check that the sun-sensor angles are within a reasonable range
# should range from 30ish degrees mid summer to 80 ish degrees mid winter
(SSA_plot <- xmp_all_ssa %>%
    filter(BandName == "Blue") %>% # identical across bands in a rig, just check one 
    group_by(Date, BandName) %>% 
    ggplot(aes(x = Date2, y = rad2deg(SunSensorAngle))) +
    geom_point(color = "red4", alpha = .5) +
    theme_bw(base_size = 16) +
    facet_wrap(. ~ Date, 
               scales = "free_x"))

ggsave(paste0(date_range,"_SSA_plots.pdf"), SSA_plot, path = paste0(dir, "CSV\\PLOTS\\"))

mod_frame = xmp_all_ssa %>% 
  drop_na(Date2) %>% 
  drop_na(cos_SSA) %>% 
  drop_na(Irradiance) %>% 
  # fir a rolling regression; for each image, fit a linear model of all images (of the same band) 
  # within 30 seconds of the image
  tidyfit::regress(Irradiance ~ cos_SSA, m("lm"),
                   .cv = "sliding_index", .cv_args = list(lookback = lubridate::seconds(30), index = "Date2"),
                   .force_cv = TRUE, .return_slices = TRUE)



df = mod_frame %>% 
  # get a summary of each model, extract the r squared
  mutate(R2 = map(model_object, function(obj) summary(obj)$adj.r.squared)) %>% 
  # extract the slope and intercept
  coef() %>% 
  unnest(model_info) %>% 
  mutate(Date2 = ymd_hms(slice_id)) 

saveRDS(df, paste0(dir, "CSV\\Corrected_values\\", date_range,"_rolling_regression.rds")) #SET PATH
#here
df = read_rds(paste0(dir, "CSV\\Corrected_values\\", date_range,"_rolling_regression.rds"))
#here
df_params = df %>%
  dplyr::select(Date:estimate, Date2) %>% 
  # we will have to go from long, with 2 observations per model, to wide
  pivot_wider(names_from = term, values_from = estimate, values_fn = {first}) %>% 
  dplyr::rename("Intercept" = `(Intercept)`,
                "Slope" = "cos_SSA")

df_p = df %>%
  filter(term == "cos_SSA") %>% 
  dplyr::select(Date:model, R2, p.value, Date2)

df_filtered = df_params %>% 
  left_join(df_p) %>% 
  left_join(xmp_all_ssa) %>% 
  mutate(percent_scattered = Intercept / (Slope + Intercept),
         dir_diff = Intercept/Slope)

df_filtered$SourceFile
df_to_use = df_filtered %>% 
  #filter(BandName %in% c("Blue","Red-650", "NIR")) %>% 
  mutate(R2 = as.numeric(R2)) %>% 
  filter(R2 > .4 
         & Slope > 0 & Intercept > 0) %>% 
  group_by(Date) %>% 
  mutate(mean_scattered = mean(percent_scattered),
         dir_diff_ratio = mean(dir_diff))

saveRDS(df_to_use, paste0(dir, "CSV\\Corrected_values\\", date_range,"_rolling_regression_used.rds")) # SET PATH

df_to_use = read_rds(paste0(dir, "CSV\\Corrected_values\\", date_range,"_rolling_regression_used.rds"))



# plot the pattern in the rolling regression, make sure you're keeping enough models
# use this plot to check parameters 
(RR_params <- df_to_use %>%
    group_by(Date, BandName) %>% 
    ggplot(aes(x = Date2, y = percent_scattered, color = R2)) +
    geom_point(data = df_filtered, color = "grey") +
    geom_point(#data = filter(df_filtered, Slope > 0)
    ) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_hline(aes(yintercept = mean_scattered), color = "red4", linewidth = 1) +
    scale_y_continuous(breaks = seq(0, 1, .2), limits = c(0,1)) +
    ggnewscale::new_scale_color() +
    theme_bw(base_size = 16) +
    facet_wrap(. ~ Date, 
               scales = "free"))

ggsave(paste0(date_range,"_checkRollingRegressionParams_plots.pdf"), RR_params, path = paste0(dir, "CSV\\PLOTS\\"))

# check that the linear relationships you're keeping look realistic,
# should be steeper for sunny days, shallower for cloudy
(linear_plots <- df_to_use %>%
    filter(BandName == "Blue") %>% 
    ggplot(aes(x = cos_SSA, y = Irradiance, color = R2)) +
    geom_point(data = filter(df_filtered, BandName == "Blue"), color = "grey30", alpha = .4) +
    geom_point(data = filter(df_filtered, BandName == "Blue" & R2 > .4), aes(color = as.numeric(R2))) +
    geom_smooth(method = "lm", se = FALSE, aes(group = BandName)) +
    lims(x = c(0, 1),
         y = c(0, max(df_to_use$Irradiance))) +
    geom_abline(aes(slope = Slope, intercept = Intercept, color = R2), alpha = .3) +
    theme_bw(base_size = 16) +
    scale_color_viridis_c() +
    facet_wrap(. ~ Date, 
               scales = "free_y"))

ggsave(paste0(date_range,"_LinearRegression_plots.pdf"), linear_plots, path = paste0(dir, "CSV\\PLOTS\\"))

df_filtered <- df_filtered %>%
  filter(GPSLongitude != 0 & GPSLatitude != 0)

# check that there is no excessive spatial pattern in the data 
(photos_kept <- df_to_use %>%
    filter(GPSLongitude != 0 & GPSLatitude != 0) %>% 
    ggplot(aes(x = GPSLongitude, y = GPSLatitude, color = rad2deg(SunSensorAngle))) +
    geom_point(data = df_filtered, color = "grey60") +
    geom_point(size = 3) +
    theme_bw() +
    scale_color_viridis_c() +
    facet_wrap(. ~ Date, scales = "free"))

ggsave(paste0(date_range,"_GPSphotosKept_plots.pdf"), photos_kept, path = paste0(dir, "CSV\\PLOTS\\"))

(ratios = df_to_use %>% 
    dplyr::select(Date, mean_scattered, dir_diff_ratio,
                  GPSLatitude, GPSLongitude, Date2) %>% 
    group_by(Date) %>% 
    mutate(Lat_mean = mean(GPSLatitude),
           Long_mean = mean(GPSLongitude),
           Date_mean = mean(Date2)) %>% 
    distinct(Date, mean_scattered, dir_diff_ratio, Lat_mean, Long_mean, Date_mean))

saveRDS(ratios, paste0(dir, "CSV\\Corrected_values\\", date_range,"_ratios.rds")) # SET PATH
ratios <- readRDS(paste0(dir, "CSV\\Corrected_values\\", date_range,"_ratios.rds"))

# print the values to use in the calibration
round(ratios$dir_diff_ratio, 2)

################################################################################
# (4) compute horizontal (corrected) irradiance for the DLS for all photos

# test = xmp_all_ssa[50,]
# phi = test$SunSensorAngle

# this is the Fresnel correction, which adjusts for the DLS reflecting, rather than 
# measuring, some of the irradiance that hits it
# this is mostly taken directly from the micasense github 
fresnel_transmission = function(phi, n1, n2, polarization) {
  f1 = cos(phi)
  f2 = sqrt(1 - (n1 / n2 * sin(phi))^2)
  Rs = ((n1 * f1 - n2 * f2) / (n1 * f1 + n2 * f2))^2
  Rp = ((n1 * f2 - n2 * f1) / (n1 * f2 + n2 * f1))^2
  T = 1 - polarization[1] * Rs - polarization[2] * Rp
  T = pmin(pmax(T, 0), 1)  # Clamp the value between 0 and 1
  return(T)
}

multilayer_transmission = function(phi, n, polarization) {
  T = 1.0
  phi_eff = phi
  for (i in 1:(length(n) - 1)) {
    n1 = n[i]
    n2 = n[i + 1]
    phi_eff = asin(sin(phi_eff) / n1)
    T = T * fresnel_transmission(phi_eff, n1, n2, polarization)
  }
  return(T)
}

fresnel_correction = function(x) {
  
  Irradiance = x$Irradiance
  SunSensorAngle = x$SunSensorAngle
  n1=1.000277
  n2=1.38
  polarization=c(0.5, 0.5)
  
  # Convert sun-sensor angle from radians to degrees
  SunSensorAngle_deg <- SunSensorAngle * (180 / pi)
  
  # Perform the multilayer Fresnel correction
  Fresnel <- multilayer_transmission(SunSensorAngle, c(n1, n2), polarization)
  return(Fresnel)
}

################################################################################
# now put it all together to compute the horizontal irradiance, the irradiance,
# which can be thought of as a corrected value for the irradiance
# reaching a point on the flat ground directly underneath the drone 

xmp_corrected = xmp_all_ssa %>% 
  group_by(Date, BandName) %>% 
  #filter(BandName == "Red") %>% 
  #slice_head(n = 900) %>% 
  nest(data = c(Irradiance, SunSensorAngle)) %>% 
  mutate(Fresnel = as.numeric(map(.x = data, .f = fresnel_correction))) %>% 
  unnest(data) %>% 
  # now join the ratios
  left_join(ratios, by = "Date") %>% 
  #mutate(HorizontalIrradiance_new = CorrectedIrradiance*(cos((90-as.numeric(SolarElevation))*pi/180)+dir_diff_ratio)/(cos(SunSensorAngle*pi/180)+dir_diff_ratio))
  mutate(SensorIrradiance = as.numeric(SpectralIrradiance) / Fresnel, # irradiance adjusted for some reflected light from the DLS diffuser
         DirectIrradiance_new = SensorIrradiance / (dir_diff_ratio + cos(as.numeric(SunSensorAngle))), # adjusted for sun angle, 
         #the DIRECT portion of irradiance if the DLS were pointing straight up
         HorizontalIrradiance_new = DirectIrradiance_new * (dir_diff_ratio + sin(as.numeric(SolarElevation))), 
         ScatteredIrradiance_new = HorizontalIrradiance_new - DirectIrradiance_new)

(Sensor_irr <- xmp_corrected %>% 
    filter(BandName == "Blue") %>% 
    ggplot(aes(x = Date2, y = SensorIrradiance)) +
    geom_point(size = 1) +
    geom_point(aes(y = HorizontalIrradiance_new), color = "red4", size = 1) +
    #geom_point(aes(y = DirectIrradiance_new), color = "orange4", size = 1) +
    #geom_point(aes(y = ScatteredIrradiance_new), color = "purple4", size = 1) +
    geom_hline(yintercept = 0) +
    #lims(y = c(50, 150)) +
    theme_bw() +
    facet_wrap(. ~ Date, scales = "free",
               ncol = 3))

ggsave(paste0(date_range,"_SensorIrradiance_plots.pdf"), Sensor_irr, path = paste0(dir, "CSV\\PLOTS\\"))


xmp_corrected %>%
  filter(BandName == "NIR") %>% 
  filter(GPSLongitude != 0 & GPSLatitude != 0)%>%
  ggplot(aes(x = GPSLongitude, y = GPSLatitude, color = rad2deg(SunSensorAngle))) +
  #geom_point(data = df_filtered, color = "grey60") +
  geom_point(size = 4#data = filter(df_filtered, Slope > 0)
  ) +
  theme_bw() +
  scale_color_viridis_c() +
  facet_wrap(. ~ Date, scales = "free")

saveRDS(xmp_corrected, paste0(dir, "CSV\\Corrected_values\\", date_range,"_xmp_corrected.rds"))

################################################################################
# (4) compute horizontal (corrected) irradiance for the DLS for all photos
# correct the photos not in the "_save" folder

xmp_corrected = readRDS(paste0(dir, "CSV\\Corrected_values\\", date_range,"_xmp_corrected.rds")) %>% 
  mutate(TargetFile = str_replace(SourceFile, ext_save, ""))

# as vectors
targets = xmp_corrected$TargetFile
SSA = xmp_corrected$SunSensorAngle
horirrorig = xmp_corrected$HorizontalIrradiance
horirr = xmp_corrected$HorizontalIrradiance_new
dirirr = xmp_corrected$DirectIrradiance_new
scairr = xmp_corrected$ScatteredIrradiance_new


## 2022_07_06 updated first 6 images --- want to check its updating right ones first

for (i in seq_along(targets)) {
  
  # given a micasense config file, overwrite tags with computed values
  # using exiftool_call from the exifr package
  call = paste0("-config C:/Users/owaite/Documents/Scripts/MicaSense_Correction/MicaSense.config", # SET PATH
                " -overwrite_original_in_place",
                " -SunSensorAngle=", SSA[i],
                " -HorizontalIrradiance=", horirr[i],
                " -HorizontalIrradianceDLS2=", horirrorig[i],
                " -DirectIrradiance=", dirirr[i],
                " -ScatteredIrradiance=", scairr[i], " ",
                targets[i])
  
  exiftool_call(call, quiet = TRUE)
  print(paste0(i, "/", length(targets), " updated"))
}

################################################################################
# (5) select the best calibration panel to use for each flight,
# requires 'PANELS/' and 'MASKS/' directories with matching images 


# select the best panels to use 
# currently this script requires masks to have been generated already 
xmp_corrected = read_rds(paste0(dir, "CSV\\Corrected_values\\", date_range,"_xmp_corrected.rds")) 


xmp_panels = xmp_corrected %>% 
  filter(panel_flag == 1) %>% 
  mutate(#root = str_split_i(Directory, "\\/", i = 1),
    img_name = str_split_i(FileName, ".tif", i = 1),
    #root_new = str_split_i(root, "_save", i = 1),
    mask_path = paste0("D:\\Sync\\Sync\\Fdc_PR_Canoe\\Flights\\",gsub("-", "_", Date),"\\2_Inputs\\metashape\\MASKS\\",img_name, "_mask.png"))

xmp_panels$img_name


unique(xmp_panels$Date)

get_panel_irr = function(x) {
  SourceFile = x$SourceFile
  mask_path = x$mask_path
  BlackLevel = x$BlackLevel 
  RadiometricCalibration = x$RadiometricCalibration
  VignettingCenter = x$VignettingCenter
  VignettingPolynomial = x$VignettingPolynomial
  ExposureTime = x$ExposureTime
  ISOSpeed = x$ISOSpeed
  BitsPerSample = x$BitsPerSample
  
  # read in the image, set its CRS
  rast = rast(SourceFile)
  crs(rast) = "epsg:26910" 
  
  #mask to raster
  mask = rast(mask_path)
  mask[mask == 0] <- NA
  crs(mask) = "epsg:26910"
  
  darkLevel = BlackLevel %>% 
    str_split(" ") %>% 
    lapply(as.numeric) %>% 
    unlist() %>% 
    mean(na.rm = TRUE)
  
  cal = RadiometricCalibration 
  a1 = cal[[1]][1] %>% as.numeric()
  a2 = cal[[1]][2] %>% as.numeric()
  a3 = cal[[1]][3] %>% as.numeric()
  
  #distance from vignette center
  cent = VignettingCenter
  vpoly = VignettingPolynomial %>% 
    lapply(as.numeric) %>% 
    unlist()
  
  cent_vect = data.frame(x = cent[[1]][1],
                         y = cent[[1]][2]) %>% 
    vect(geom = c("x", "y"), crs = "epsg:26910")
  
  # vignetting correction raster
  dist_rast = distance(rast, cent_vect)
  poly_rast = dist_rast^6 * vpoly[6] + 
    dist_rast^5 * vpoly[5] +
    dist_rast^4 * vpoly[4] +
    dist_rast^3 * vpoly[3] +
    dist_rast^2 * vpoly[2] +
    dist_rast * vpoly[1] +
    1
  
  V = 1 / poly_rast
  
  # row gradient correction
  y = rast
  values(y) = rep(seq(1, nrow(rast), 1), 
                  each = ncol(rast))
  
  exposureTime = ExposureTime
  gain = ISOSpeed/100.0
  
  R = 1.0 / (1.0 + a2 * y / exposureTime - a3 * y)
  
  L = V * R * (rast - darkLevel)
  
  L[L < 0] = 0
  
  
  # apply the radiometric calibration - 
  # scale by the gain-exposure product and 
  #multiply with the radiometric calibration coefficient
  bitsPerPixel = BitsPerSample
  dnMax = 2^bitsPerPixel
  radianceImage = L/(gain * exposureTime)*a1/dnMax
  
  #masking out panel
  panel = mask(radianceImage, mask)
  
  #getting mean reflectance value of panel
  val = values(panel, na.rm = TRUE) #get raster values
  mean_ref = mean(val)
  # and standard deviation of panel irr
  cv_ref = sd(val) / mean_ref
  
  #print(paste0(str_split_i(SourceFile, "/", i = 3)))
  print(SourceFile)
  
  return(list(irr_mean = mean_ref, irr_cv = cv_ref))
}

xmp_vals = xmp_panels %>% 
  ungroup() %>% 
  nest(data = c(SourceFile_G, 
                #SourceFile, ##### usually this is uncommented, just needed _G for one where im changing the drive
                mask_path, 
                BlackLevel, 
                RadiometricCalibration,
                VignettingCenter,
                VignettingPolynomial,
                ExposureTime,
                ISOSpeed,
                BitsPerSample)) %>% 
  mutate(panel_vals = map(.x = data, .f = get_panel_irr)) %>% 
  unnest(c(data)) %>% 
  unnest_wider(panel_vals)

saveRDS(xmp_vals, paste0(dir, "CSV\\Corrected_values\\", date_range,"_xmp_to_choose_panels.rds")) #SET PATH

################################################################################

# the following values need to be set for each individual calibration panel used
# thic could be coded to be done automatically 
xmp_vals_choose = read_rds(paste0(dir, "CSV\\Corrected_values\\", date_range,"_xmp_to_choose_panels.rds"))
unique(xmp_vals_choose$Date)

xmp_vals_choose = read_rds(paste0(dir, "CSV\\Corrected_values\\", date_range,"_xmp_to_choose_panels.rds")) %>% #SET PATH
  ## added below section to filter for cameras that have all 10 bands ie all 10 images have a panel mask
  mutate(img_root = sub("_(\\d+)$", "", img_name))%>%
  group_by(Date, img_root)%>%
  filter(n_distinct(BandName) == 10) %>%
  ungroup()%>%
  # UBC PANEL
  mutate(panel_val = case_when(BandName == "Blue-444" ~ 0.487607,
                               BandName == "Blue" ~ 0.488051,
                               BandName == "Green-531" ~ 0.489004,
                               BandName == "Green" ~ 0.489448,
                               BandName == "Red-650" ~ 0.489518,
                               BandName == "Red" ~ 0.489372,
                               BandName == "Red edge-705" ~ 0.488794,
                               BandName == "Red edge" ~ 0.48845,
                               BandName == "Red edge-740" ~ 0.488047,
                               BandName == "NIR"~ 0.486897)) %>%
  ## JAKES CFS PANEL
  # mutate(panel_val = case_when( BandName == "Blue-444" ~ 0.471234,
  #                               BandName == "Blue" ~ 0.471417,
  #                               BandName == "Green-531" ~ 0.472238,
  #                               BandName == "Green" ~ 0.472404,
  #                               BandName == "Red-650" ~ 0.472016,
  #                               BandName == "Red" ~ 0.471893,
  #                               BandName == "Red edge-705" ~ 0.471425,
  #                               BandName == "Red edge" ~ 0.471099,
  #                               BandName == "Red edge-740" ~ 0.470721,
  #                               BandName == "NIR"~ 0.469514,)) %>%

mutate(panel_irradiance = (irr_mean * pi) /
         ((HorizontalIrradiance_new) * .01)) %>% # percent of DLS-measured irradiance reflected by panel
  mutate(irr_diff = panel_val - panel_irradiance) %>% 
  group_by(ModifyDate) %>% # group by rig, compute average and max CV as selection criterion
  mutate(mean_panel_irr = mean(panel_irradiance),
         irr_cv_max = max(irr_cv),
         irr_cv_mean = mean(irr_cv),
         irr_diff_max = max(abs(irr_diff)),
         irr_diff_mean = mean(abs(irr_diff)),
         irr_rank = irr_cv_mean + irr_cv_max) %>% # choose panel with consistently uniform values
  group_by(Date) %>% 
  mutate(irr_rank_min = min(irr_rank),
         choose_flag = if_else(
           irr_rank == irr_rank_min,
           1, 0))

unique(xmp_vals_choose$Date)

#direct irradiance of panels verse time
(xmp_vals_choose %>% 
    #filter(BandName %in% c("Blue", "NIR")) %>% 
    #mutate(Mod = if_else(grepl("_save", FileName), "Original", "Modified")) %>% 
    ggplot(aes(x = DateTimeOriginal, y = panel_irradiance, group = BandName, color = BandName)) +
    geom_point(size = 1, alpha = .4) +
    geom_line(size = 1, alpha = .5) +
    geom_line(aes(y = mean_panel_irr), color = "black", size = 1) +
    #geom_line(aes(y = mean_ratio_horiz), color = "black", size = 1) +
    geom_hline(aes(yintercept = panel_val), color = "grey10") +
    geom_point(data = filter(xmp_vals_choose, choose_flag == 1), 
               size = 3) +
    # Add text labels for img_name
    geom_text(data = filter(xmp_vals_choose, choose_flag == 1,BandName == "Red edge-740"), 
              aes(label = img_name), size = 3, nudge_x = 0.05, nudge_y = 0.55) +
    
    theme_bw(base_size = 8) + 
    #lims(y = c(.2,.8)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_color_manual(values = c("Blue-444" = "royalblue4",
                                  "Blue" = "steelblue",
                                  "Green-531" = "springgreen3",
                                  "Green" = "forestgreen",
                                  "Red-650" = "firebrick2",
                                  "Red" = "red3",
                                  "Red edge-705" = "indianred",
                                  "Red edge" = "lightpink3",
                                  "Red edge-740" = "pink4",
                                  "NIR" = "thistle4")) +
    facet_wrap(. ~ Date, scales = "free"))

#direct irradiance of panels verse time
(xmp_vals_choose %>% 
    filter(BandName %in% c("Blue")) %>% 
    ggplot(aes(x = irr_diff_max, y = irr_cv_mean, group = BandName, color = BandName)) +
    geom_point(size = 1.5) +
    geom_point(data = filter(xmp_vals_choose, choose_flag == 1), 
               size = 2,
               color = "red4") +
    theme_bw(base_size = 12) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x = "max difference between panel target and DLS calculated reflectance (%)",
         y = "mean coefficient of variation of panel irradiance") +
    scale_color_manual(values = c("Blue-444" = "royalblue4",
                                  "Blue" = "steelblue",
                                  "Green-531" = "springgreen3",
                                  "Green" = "forestgreen",
                                  "Red-650" = "firebrick2",
                                  "Red" = "red3",
                                  "Red edge-705" = "indianred",
                                  "Red edge" = "lightpink3",
                                  "Red edge-740" = "pink4",
                                  "NIR" = "thistle4")) +
    facet_grid(. ~ Date))

#saveRDS(xmp_vals_choose, paste0(dir, "CSV\\Corrected_values\\", date_range,"_xmp_chosen_panels.rds"))  # SET PATH


# these are the panels to use
xmp_vals_choose %>%
  filter(BandName == "Blue" & choose_flag == 1) %>% 
  distinct(img_name, Date, choose_flag) #%>% 
# write_csv(paste0(dir, "CSV\\Corrected_values\\", date_range,"_xmp_chosen_panels.rds"))

chosen_p <- read_csv(paste0(dir, "CSV\\Corrected_values\\", date_range,"_xmp_chosen_panels.rds"))
chosen_p