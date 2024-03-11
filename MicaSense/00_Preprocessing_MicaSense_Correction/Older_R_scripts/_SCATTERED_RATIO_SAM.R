
# Pre-processing of Micasense images before calibration and orthophoto
# generation in Metashape

# (1) compute sun-sensor angle for all photos 



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


################################################################################

# dir = "D:\\Sync\\_Sites\\Skimikin_spectral"
dir <- "I:\\PARSER_Ext\\Fdc_PR_Canoe\\Flights\\"
dir_panels_to_move <- "D:\\Fdc_PR_Canoe\\Flights\\"

date_list <- c("2022_07_06",
               "2022_07_22",
               "2022_07_27",
               "2022_08_05",
               "2022_08_24") #2022_07_06, 2022_07_22, 2022_07_27, 2022_08_05, 2022_08_24 done for "Micasense_cleaned_org_save" and "Micasense_cleaned"

ext_save <- "_org_save" #make either "" for "Micasense_Cleaned" or "_org_save" for "Micasense_Cleaned_org_save"

#################################################################################
# Transferring Panels folder to the main directory with MS original images
################################################################################
for (x in seq_along(date_list)){ 
  print(date_list[x])
  if (!dir.exists(paste0(dir,date_list[x],"\\1_Data\\Micasense_Cleaned",ext_save,"\\Panels"))) {
    dir.create(paste0(dir,date_list[x],"\\1_Data\\Micasense_Cleaned",ext_save,"\\Panels"),recursive = TRUE)
    
    #Names of panel images
    panel_names_roots <- list.files(paste0(dir_panels_to_move, date_list[x],"\\2_Inputs\\metashape\\PANELS"), 
                                    pattern = c(".tif$"), recursive = TRUE, 
                                    full.names = FALSE)
    #All image names
    pics_all = list.files(paste0(dir, date_list[x],"\\1_Data\\Micasense_Cleaned",ext_save), #path the MS_original that is copied
                          pattern = c("IMG_....", ".tif$"), recursive = TRUE, 
                          full.names = TRUE)
    
    # Extract Just the Image name
    pics_all_root <- gsub(".*/(IMG_.*\\.tif)$", "\\1", pics_all)
    
    #For each image, if the root is the same as a panel, add it to the panel folder and remove from main folders
    for (i_img in 1:length(pics_all)){
      single_pic <- pics_all[i_img]
      single_pic_root <- pics_all_root[i_img]
      
      if(single_pic_root %in% panel_names_roots){#if pic name is in the vector of panel names
        file.copy(from = single_pic,
                  to = paste0(dir, date_list[x], "\\1_Data\\Micasense_Cleaned",ext_save,"\\Panels/",single_pic_root), overwrite = FALSE) 
        file.remove(single_pic)
      }
    }
  }
}
################################################################################
# XMP data directly

df_all = data.frame()
date_list = c("2022_07_06")
x = 1
j = 10
i = 46
for (x in seq_along(date_list)){ 
  for (j in 1:10){

    pics = list.files(paste0(dir, date_list[x],"\\1_Data\\Micasense_Cleaned",ext_save), #path the MS_original that is copied
                      pattern = c(paste0("IMG_...._", j, ".tif"), paste0("IMG_...._", j, "_", ".tif")), recursive = TRUE, 
                      full.names = TRUE)
   
    pic_root_panels = list.files(paste0(dir, date_list[x], "\\1_Data\\Micasense_Cleaned",ext_save,"\\Panels"), #path the panels folder
                      pattern = c("IMG_...",".tif$"), recursive = TRUE, 
                      full.names = FALSE)
    # Extract just image names
    pics_root <- gsub(".*/(IMG_.*\\.tif)$", "\\1", pics)
    
    #finding directories from the panels folder:
    keyword = "Panel"
    panel_dirs <- pics[grep(keyword, pics)]
    
    for (i in 1:length(pics)){
      pic = pics[i]
      #pic_root = tail(unlist(strsplit(pics_root[i], "Panels/")), n = 1)
      pic_root = pics_root[i]
      
      #tag for if a panel image:
      if(pic %in% panel_dirs){
        #pan_flag = length(unlist(strsplit(pics_root[i], "Panels/"))) - 1
        pan_flag = 1 
      }else{
        pan_flag = 0
      }
      
      if (substr(pic_root,11,11) == "0") {
        band = substr(pic_root[i],10,11)
      } else {
        band = substr(pic_root,10,10)}
      
      img_exif = read_exif(pic)
      print(paste0(date_list[x], " band ", j, " ", i, "/", length(pics)))
      
      #creating df with same column names as exif data
      if (i == 1){
        exif_df = as.data.frame(img_exif) %>% 
          mutate(panel_flag = pan_flag)
      } else {   #adding each new image exif to dataframe
        exif_df = merge(exif_df, img_exif, by = intersect(names(exif_df), names(img_exif)), all = TRUE)
      }
      #j = j + 1
    }
    if(!dir.exists(paste0(dir,date_list[x],"\\1_Data\\CSV\\"))){ #creating CSV folders
      dir.create(paste0(dir,date_list[x],"\\1_Data\\CSV\\"))
    }
    if(!dir.exists(paste0(dir,date_list[x],"\\1_Data\\CSV\\Corrected_values\\"))){ #Creating Corrected_Values folder
      dir.create(paste0(dir,date_list[x],"\\1_Data\\CSV\\Corrected_values\\"))
    }
    saveRDS(exif_df, paste0(dir,date_list[x],"\\1_Data\\CSV\\Corrected_values\\df_", date_list[x], "_", j, ".rds"))
  } 
}

# read them in from the rds files now 

for (x in 1:length(date_list)){
  for (j in c(1:10)){#1:10){
    
    exif_df = read_rds(paste0(dir, "\\CSV\\Corrected_values\\df_", date_list[x], "_", j, ".rds"))
    
    if (x == 1 & j == 1){
      exif_df_all = as.data.frame(exif_df)
    } else {   #adding each new image exif to dataframe
      exif_df_all = bind(exif_df_all, exif_df, by = intersect(names(exif_df_all), names(exif_df)), all = TRUE)
    }
    
    xmp_all = exif_df_all 
    # %>% 
    #   filter(FileType == "TIFF")
    # some are DNG for some reason
  }
}

xmp_all %>% 
  saveRDS(paste0(dir, "\\CSV\\Corrected_values\\XMP_all.rds"))


################################################################################
# compute the sun-sensor angle for all photos

# Define a function to compute the DLS-Sun angle in R
compute_sun_angle = function(SolarElevation, SolarAzimuth, Roll, Pitch, Yaw) {
  
  # Convert to numeric
  SolarElevation = as.numeric(SolarElevation)
  SolarAzimuth = as.numeric(SolarAzimuth)
  Roll = as.numeric(Roll)
  Pitch = as.numeric(Pitch)
  Yaw = as.numeric(Yaw)
  
  # Convert sun azimuth and elevation to NED coordinates
  elements <- c(
    cos(SolarAzimuth) * cos(SolarElevation),
    sin(SolarAzimuth) * cos(SolarElevation),
    -sin(SolarElevation))
  
  nSun = t(matrix(elements, ncol = 3))
  
  # Define the orientation vector of the DLS in body coordinates
  ori = c(0, 0, -1)
  
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
  R_sensor = Ryaw %*% Rpitch %*% Rroll
  
  # Compute the orientation vector of the sensor in NED coordinates
  nSensor = R_sensor %*% ori
  
  # Compute the angle between the sensor and the sun
  angle = acos(sum(nSun * nSensor))
  return(angle)
  #return(rad2deg(angle))
}


read_rds(paste0(dir, "\\CSV\\Corrected_values\\XMP_all.rds")) %>% 
  rowwise() %>% 
  mutate(SunSensorAngle = compute_sun_angle(SolarElevation, SolarAzimuth, Roll, Pitch, Yaw)) %>% 
  saveRDS(paste0(dir, "\\CSV\\Corrected_values\\XMP_with_SSA.rds"))

################################################################################

xmp_all_ssa = read_rds(paste0(dir, "\\CSV\\Corrected_values\\XMP_with_SSA.rds")) %>% 
  mutate(Date = str_split_i(Directory, "\\\\", i = 7),
         Date = str_split_i(Date, "Skimikin_", i = 2),
         Date = str_split_i(Date, "_save", i = 1),
         subdirectory = str_split_i(Directory, "/", i = 2),
         panel_flag = if_else(subdirectory == "Panels", 1, 0)) %>% 
  mutate(Yaw_deg = rad2deg(as.numeric(Yaw)),
         Roll_deg = rad2deg(as.numeric(Roll)),
         Pitch_deg = rad2deg(as.numeric(Pitch))) %>% 
  group_by(Date, BandName) %>% 
  arrange(ymd_hms(DateTimeOriginal)) %>% 
  mutate(GPSLatitude_plot = scale(as.numeric(GPSLatitude)),
         GPSLongitude_plot = scale(as.numeric(GPSLongitude)),
         cos_SSA = cos(SunSensorAngle),
         Irradiance = as.numeric(Irradiance),
         Date2 = ymd_hms(DateTimeOriginal))

# check that the sun-sensor angles are within a reasonable range
xmp_all_ssa %>%
  filter(BandName == "Blue") %>% 
  group_by(Date, BandName) %>% 
  ggplot(aes(x = Date2, y = rad2deg(SunSensorAngle))) +
  geom_point(color = "red4", alpha = .5) +
  theme_bw(base_size = 16) +
  facet_wrap(. ~ Date, 
             scales = "free_x")

mod_frame = xmp_all_ssa %>% 
  drop_na(Date2) %>% 
  drop_na(cos_SSA) %>% 
  drop_na(Irradiance) %>% 
  tidyfit::regress(Irradiance ~ cos_SSA, m("lm"),
                   .cv = "sliding_index", .cv_args = list(lookback = lubridate::seconds(30), index = "Date2"),
                   .force_cv = TRUE, .return_slices = TRUE)

df = mod_frame %>% 
  mutate(R2 = map(model_object, function(obj) summary(obj)$adj.r.squared)) %>% 
  coef() %>% 
  unnest(model_info) %>% 
  mutate(Date2 = ymd_hms(slice_id)) 

saveRDS(df, paste0(dir, "\\CSV\\Corrected_values//rolling_regression.rds"))
df = read_rds(paste0(dir, "\\CSV\\Corrected_values//rolling_regression.rds"))

df_params = df %>%
  dplyr::select(Date:estimate, Date2) %>% 
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


df_to_use = df_filtered %>% 
  #filter(BandName %in% c("Blue","Red-650", "NIR")) %>% 
  mutate(R2 = as.numeric(R2)) %>% 
  filter(R2 > .4 
         & Slope > 0 & Intercept > 0) %>% 
  group_by(Date) %>% 
  mutate(mean_scattered = mean(percent_scattered),
         dir_diff_ratio = mean(dir_diff))

saveRDS(df_to_use, paste0(dir, "\\CSV\\Corrected_values//rolling_regression_used.rds"))

df_to_use = read_rds(paste0(dir, "\\CSV\\Corrected_values//rolling_regression_used.rds"))

df_to_use %>%
  #filter(R2 > .4 & Slope > 0) %>% 
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
  #geom_abline(aes(slope = Slope, intercept = Intercept, color = Date2)) +
  # scale_color_manual(values = c("Blue-444" = "royalblue4",
  #                               "Blue" = "steelblue",
  #                               "Green-531" = "springgreen3",
  #                               "Green" = "forestgreen",
  #                               "Red-650" = "firebrick2",
  #                               "Red" = "red3",
  #                               "Red edge-705" = "indianred",
  #                               "Red edge" = "lightpink3",
  #                               "Red edge-740" = "pink4",
  #                               "NIR" = "thistle4")) +
theme_bw(base_size = 16) +
  facet_wrap(. ~ Date, 
             scales = "free")

df_to_use %>%
  filter(BandName == "Blue") %>% 
  ggplot(aes(x = cos_SSA, y = Irradiance, color = R2)) +
  geom_point(data = filter(df_filtered, BandName == "Blue"), color = "grey30", alpha = .4) +
  geom_point(data = filter(df_filtered, BandName == "Blue" & R2 > .4), aes(color = as.numeric(R2))) +
  geom_smooth(method = "lm", se = FALSE, aes(group = BandName)) +
  lims(x = c(0, 1),
       y = c(0, max(df_to_use$Irradiance))) +
  geom_abline(aes(slope = Slope, intercept = Intercept, color = R2), alpha = .3) +
  theme_bw(base_size = 16) +
  facet_wrap(. ~ Date, 
             scales = "free_y")

# check that there is no excessive spatial pattern in the data 
df_to_use %>%
  #filter(abs(Irr_change_perc) < .005) %>% 
  ggplot(aes(x = GPSLongitude, y = GPSLatitude, color = SunSensorAngle)) +
  geom_point(data = df_filtered, color = "grey60") +
  geom_point(size = 3#data = filter(df_filtered, Slope > 0)
  ) +
  #geom_smooth(method = "lm", se = FALSE) +
  #labs(title = "1%") +
  #geom_hline(yintercept = 1, linetype = 2) +
  #lims(y = c(0,1.5)) +
  #geom_abline(aes(slope = Slope, intercept = Intercept, color = p.value), alpha = .3) +
  theme_bw() +
  facet_wrap(. ~ Date, scales = "free")


(ratios = df_to_use %>% 
    dplyr::select(Date, mean_scattered, dir_diff_ratio,
                  GPSLatitude, GPSLongitude, Date2) %>% 
    group_by(Date) %>% 
    mutate(Lat_mean = mean(GPSLatitude),
           Long_mean = mean(GPSLongitude),
           Date_mean = mean(Date2)) %>% 
    distinct(Date, mean_scattered, dir_diff_ratio, Lat_mean, Long_mean, Date_mean))

saveRDS(ratios, paste0(dir, "\\CSV\\Corrected_values//ratios.rds"))

# print the values to use in the calibration
round(ratios$dir_diff_ratio, 2)

################################################################################
# okay, now calculate the correct irradiance values
# this is the Fresnel correction

test = xmp_all_ssa[50,]
phi = test$SunSensorAngle

fresnel_transmission <- function(phi, n1, n2, polarization) {
  f1 <- cos(phi)
  f2 <- sqrt(1 - (n1 / n2 * sin(phi))^2)
  Rs <- ((n1 * f1 - n2 * f2) / (n1 * f1 + n2 * f2))^2
  Rp <- ((n1 * f2 - n2 * f1) / (n1 * f2 + n2 * f1))^2
  T <- 1 - polarization[1] * Rs - polarization[2] * Rp
  T <- pmin(pmax(T, 0), 1)  # Clamp the value between 0 and 1
  return(T)
}

multilayer_transmission <- function(phi, n, polarization) {
  T <- 1.0
  phi_eff <- phi
  for (i in 1:(length(n) - 1)) {
    n1 <- n[i]
    n2 <- n[i + 1]
    phi_eff <- asin(sin(phi_eff) / n1)
    T <- T * fresnel_transmission(phi_eff, n1, n2, polarization)
  }
  return(T)
}

fresnel_correction <- function(x) {
  
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
# now put it all together to compute the horizontal irradiance 

xmp_corrected = xmp_all_ssa %>% 
  group_by(Date, BandName) %>% 
  #filter(BandName == "Red") %>% 
  #slice_head(n = 900) %>% 
  nest(data = c(Irradiance, SunSensorAngle)) %>% 
  mutate(Fresnel = as.numeric(map(.x = data, .f = fresnel_correction))) %>% 
  unnest(data) %>% 
  #mutate(CorrectedIrradiance = Irradiance/Fresnel) %>% 
  # now join the ratios
  left_join(ratios, by = "Date") %>% 
  #mutate(HorizontalIrradiance_new = CorrectedIrradiance*(cos((90-as.numeric(SolarElevation))*pi/180)+dir_diff_ratio)/(cos(SunSensorAngle*pi/180)+dir_diff_ratio))
  mutate(SensorIrradiance = as.numeric(SpectralIrradiance) / Fresnel,
         DirectIrradiance_new = SensorIrradiance / (dir_diff_ratio + cos(as.numeric(SunSensorAngle))),
         HorizontalIrradiance_new = DirectIrradiance_new * (dir_diff_ratio + sin(as.numeric(SolarElevation))),
         ScatteredIrradiance_new = HorizontalIrradiance_new - DirectIrradiance_new)

xmp_corrected %>% 
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
             ncol = 3)

xmp_corrected %>%
  filter(BandName == "NIR") %>% 
  ggplot(aes(x = GPSLongitude, y = GPSLatitude, color = rad2deg(SunSensorAngle))) +
  #geom_point(data = df_filtered, color = "grey60") +
  geom_point(size = 4#data = filter(df_filtered, Slope > 0)
  ) +
  theme_bw() +
  scale_color_viridis_c() +
  facet_wrap(. ~ Date, scales = "free")

saveRDS(xmp_corrected, paste0(dir, "\\CSV\\Corrected_values//xmp_corrected.rds"))

################################################################################

# select the best panels to use 
# currently this script requires masks to have been generated already 
xmp_corrected = read_rds(paste0(dir, "\\CSV\\Corrected_values//xmp_corrected.rds"))

xmp_panels = xmp_corrected %>% 
  filter(panel_flag == 1) %>% 
  mutate(root = str_split_i(Directory, "\\/", i = 1),
         img_name = str_split_i(FileName, ".tif", i = 1),
         root_new = str_split_i(root, "_save", i = 1),
         mask_path = paste0(root_new, "/Masks/", img_name, "_mask.png"))

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
  
  # and standard deviation of panel irr
  
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
  cv_ref = sd(val) / mean_ref
  
  #print(paste0(str_split_i(SourceFile, "/", i = 3)))
  print(SourceFile)
  # print(mean_ref)
  # print(cv_ref)
  
  return(list(irr_mean = mean_ref, irr_cv = cv_ref))
}

xmp_vals = xmp_panels %>% 
  ungroup() %>% 
  nest(data = c(SourceFile, 
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

saveRDS(xmp_vals, paste0(dir, "\\CSV\\Corrected_values//xmp_to_choose_panels.rds"))

################################################################################

xmp_vals_choose = read_rds(paste0(dir, "\\CSV\\Corrected_values//xmp_to_choose_panels.rds")) %>% 
  mutate(panel_val = case_when(Date != "2023_02_24" & BandName == "Blue-444" ~ 0.538593,
                               Date != "2023_02_24" & BandName == "Blue" ~ 0.538759,
                               Date != "2023_02_24" & BandName == "Green-531" ~ 0.539475,
                               Date != "2023_02_24" & BandName == "Green" ~ 0.539453,
                               Date != "2023_02_24" & BandName == "Red-650" ~ 0.538375,
                               Date != "2023_02_24" & BandName == "Red" ~ 0.538064,
                               Date != "2023_02_24" & BandName == "Red edge-705" ~ 0.537183,
                               Date != "2023_02_24" & BandName == "Red edge" ~ 0.537055,
                               Date != "2023_02_24" & BandName == "Red edge-740" ~ 0.536402,
                               Date != "2023_02_24" & BandName == "NIR"~ 0.533745,
                               Date == "2023_02_24" & BandName == "Blue-444" ~ 0.473,
                               Date == "2023_02_24" & BandName == "Blue" ~ 0.472,
                               Date == "2023_02_24" & BandName == "Green-531" ~ 0.472,
                               Date == "2023_02_24" & BandName == "Green" ~ 0.472,
                               Date == "2023_02_24" & BandName == "Red-650" ~ 0.471,
                               Date == "2023_02_24" & BandName == "Red" ~ 0.471,
                               Date == "2023_02_24" & BandName == "Red edge-705" ~ 0.47,
                               Date == "2023_02_24" & BandName == "Red edge" ~ 0.47,
                               Date == "2023_02_24" & BandName == "Red edge-740" ~ 0.47,
                               Date == "2023_02_24" & BandName == "NIR"~ 0.469)) %>% 
  mutate(panel_irradiance = (irr_mean * pi) /
           ((HorizontalIrradiance_new) * .01)) %>% # this is technically reflectance I think 
  mutate(irr_diff = panel_val - panel_irradiance) %>% 
  group_by(ModifyDate) %>% 
  mutate(mean_panel_irr = mean(panel_irradiance),
         irr_cv_max = max(irr_cv),
         irr_cv_mean = mean(irr_cv),
         irr_diff_max = max(abs(irr_diff)),
         irr_diff_mean = mean(abs(irr_diff)),
         irr_rank = irr_cv_mean + irr_cv_max) %>% 
  group_by(Date) %>% 
  # mutate(choose_flag = if_else(
  #   irr_diff_max < .24 &
  #     irr_diff_mean < .12 &
  #     irr_cv_max < .1,
  #   1, 0))
  mutate(irr_rank_min = min(irr_rank),
         choose_flag = if_else(
    irr_rank == irr_rank_min,
    1, 0))
  # choose a panel with less than 10% coefficient of variation of irradiance 
  # values,
  # mean dustance from target value less than 12%, 
  # max value less than 24%



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
               size = 1.5,
               color = "red4") +
    theme_bw(base_size = 12) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    # labs(x = "difference between panel target and DLS calculated reflectance (%)",
    #      y = "coefficient of variation of panel irradiance") +
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

saveRDS(xmp_vals_choose, paste0(dir, "\\CSV\\Corrected_values//xmp_chosen_panels.rds"))

# these are the panels to use
# these are the panels to use
xmp_vals_choose %>%
  filter(BandName == "Blue" & choose_flag == 1) %>% 
  distinct(img_name, Date, choose_flag) %>% 
  write_csv(paste0(dir, "\\CSV\\Corrected_values//xmp_chosen_panels.csv"))


################################################################################
# correct the photos not in the "_save" folder

xmp_corrected = readRDS(paste0(dir, "\\CSV\\Corrected_values\\xmp_corrected.rds")) %>% 
  #filter(subdirectory == "Panels" & Site == "LangBay") %>% 
  mutate(TargetFile = str_replace(SourceFile, "_save", "")
           #SourceFile
  )

targets = xmp_corrected$TargetFile
SSA = xmp_corrected$SunSensorAngle
horirrorig = xmp_corrected$HorizontalIrradiance
horirr = xmp_corrected$HorizontalIrradiance_new
dirirr = xmp_corrected$DirectIrradiance_new
scairr = xmp_corrected$ScatteredIrradiance_new

for (i in seq_along(targets)) {
  
  call = paste0("-config D:/Sync/imageprocessing/MicaSense.config",
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
