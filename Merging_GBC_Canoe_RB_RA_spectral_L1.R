
library(sf)
library(dplyr)

### Canoe -----------------------------------------------------------
#census data:
canoe_dir <- "E:\\Canoe\\Crowns\\Edited_added\\CHM_.06_masked_RGB_H20T_mosaic_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss.sig.01w3_z75_crowns_EDITED_NEWmissAdded.shp"
canoe_pols_col_remove <- c("geometry", "crownArea","tag__", "Z","a99_2yr", "a01_3yr" ,"c03_5yr" ,"ht03_5y" ,"c05_7yr" ,"d05_7yr", "ht05_7y" ,"cr08_10" ,"rsst_10" ,"c08_10y",
                      "ht08_10" ,"d08_10y" ,"vl08_10" ,"a11_13y" ,"a13_15y" ,"com22","ht22","dbh","prrty22", "chck_ht", "chck_db",
                       "chck_sr", "x_coord", "Absent")

canoe_pols = st_read(canoe_dir, crs = 26910) %>% 
  filter(!st_is_empty(.)) %>% 
  st_make_valid() %>% 
  st_cast("POLYGON")%>%
  as.data.frame()%>%
  mutate(tag = tag__,
         ht2003_age5 = ht03_5y,
         ht2005_age7 = ht05_7y,
         ht2008_age10 = ht08_10,
         ht2022_age24 = ht22)%>%
  dplyr::select(!c(canoe_pols_col_remove))

colnames(canoe_pols)
length(unique(canoe_pols$tag))
length(canoe_pols$tag)

length(unique(canoe_pols$treeID))
length(canoe_pols$treeID)

colnames(canoe_pols)
#L1 Metrics
#Canoe: 3006 trees planted in 1999, 93 fams, 32 reps
L1_date <- "2022_04_23"
can_L1_files <- list.files(paste0("E:\\Canoe\\L1_metrics\\",L1_date,"\\10_CROWNS_clean\\Structural_metrics\\"), pattern = ".rds$",  full.names= TRUE)

for(i in 1:length(can_L1_files)){
  file <- readRDS(can_L1_files[i])%>%
    as.data.frame()%>%
    mutate(Date = ymd(L1_date))
  
  if(i==1){
    can_L1_df <- file
  }else{
    can_L1_df <- rbind(can_L1_df, file)
  }
}
can_L1_df_renamed_2022_04_23 <- can_L1_df %>%
  mutate(ALS.Zq99.2022_04_23 = Zq99,
         ALS.Zq975.2022_04_23 = Zq975,
         ALS.n_points.2022_04_23 = n_points)%>%
  select(c("tag","ALS.Zq99.2022_04_23","ALS.Zq975.2022_04_23","ALS.n_points.2022_04_23"))
colnames(can_L1_df_renamed_2022_04_23)
can_L1_df_renamed_2022_04_23
length(unique(can_L1_df_renamed_2022_04_23$tag))


##dec ALS:
L1_date_dec <- "2023_12_17"
can_L1_files_dec <- list.files(paste0("Q:\\GBC\\Data\\Fdc_PR_Canoe\\Flights\\2023_12_17\\2_Inputs\\L1\\05_ALIGNED_ALS\\output\\CROWNS_clean\\Structural_metrics\\"), pattern = ".rds$",  full.names= TRUE)

for(i in 1:length(can_L1_files_dec)){
  file <- readRDS(can_L1_files_dec[i])%>%
    as.data.frame()%>%
    mutate(Date = ymd(L1_date_dec))
  
  if(i==1){
    can_L1_df_dec <- file
  }else{
    can_L1_df_dec <- rbind(can_L1_df_dec, file)
  }
}
can_L1_df_renamed_2023_12_17 <- can_L1_df_dec %>%
  mutate(ALS.Zq99.2023_12_17 = Zq99,
         ALS.Zq975.2023_12_17 = Zq975,
         ALS.n_points.2023_12_17 = n_points)%>%
  select(c("tag","ALS.Zq99.2023_12_17","ALS.Zq975.2023_12_17","ALS.n_points.2023_12_17"))
colnames(can_L1_df_renamed_2023_12_17)
can_L1_df_renamed_2023_12_17
length(unique(can_L1_df_renamed_2023_12_17$tag))

can_L1_df_renamed_2023_12_17_tag <- can_L1_df_renamed_2023_12_17%>%
  filter(tag %in% c(can_L1_df_renamed_2022_04_23$tag))
length(can_L1_df_renamed_2023_12_17_tag$tag)

tags_not_in_df1 <- setdiff(can_L1_df_renamed_2023_12_17$tag, can_L1_df_renamed_2022_04_23$tag)

print(tags_not_in_df1)

#Spectral metrics
#"tag"                               "COL"                               "ROW"                              
#[4] "Area_buffer5cm"                    "Area"                              "site"   
# spec.median.index.Month_dd_YYYY

canoe_spec <- readRDS("E:\\Canoe\\AllDates_NIRshadowMask_MedianCrownSpectralIndices.rds")
colnames(canoe_spec)
class(canoe_spec$Date)

#below gives spec.re_total.July_04_2022
colnames(canoe_spec)

canoe_spectral_df <- canoe_spec %>%
  as.data.frame() %>%
  mutate(tag = tag__)%>%
  mutate(Date = gsub("-", "_", as.character(Date)))%>%
  dplyr::select(matches(paste0("^median\\.", paste(GBC_indicies, collapse = "|"))), c("tag","count","Date"))%>%
  mutate(site = "Fdc_PR_Canoe")%>%
  mutate(n_pixels = count)%>%
  # mutate(Date_M = format(canoe_spec$Date, "%B_%d_%Y"))%>%
  dplyr::select(!c(count))%>%
  pivot_wider(names_from = Date, values_from = c(starts_with("median."), starts_with("n_pixels"))) %>%
  rename_with(~ gsub("_(?=\\d{4})", ".", ., perl = TRUE), c(starts_with("median."), starts_with("n_pixels")))%>%
  # rename_with(~ gsub("_(?=[A-Za-z]+_\\d{2}_\\d{4})", ".", ., perl = TRUE), c(starts_with("median."),starts_with("n_pixels")))%>%
  rename_with(~ if_else(. == "tag" | . == "site", ., paste0("spec.", .)), -c(tag, site))%>%
  select(!c(site))
  # rename_with(~ if_else(. == "tag" | . == "site" |. == "COL"|. == "ROW"|. == "Area"|. == "Area_buffer5cm"|. == "treeID", ., paste0("spec.", .)), -c(tag, site))

colnames(canoe_spectral_df)
# canoe_spectral_df <- canoe_spec %>%
#   as.data.frame() %>%
#   mutate(tag = tag__) %>%
#   dplyr::select(matches(paste0("^median\\.", paste(GBC_indicies, collapse = "|"))), c("tag","count", "Date")) %>%
#   mutate(site = "Fdc_PR_Canoe") %>%
#   mutate(Date = gsub("-", "_", as.character(Date)))%>%
#   mutate(n_pixels = count) %>%
#   dplyr::select(-count) %>%
#   pivot_wider(names_from = Date, values_from = c(starts_with("median."), starts_with("n_pixels"))) %>%
#   rename_with(~ gsub("_(?=\\d{4})", ".", ., perl = TRUE), c(starts_with("median."), starts_with("n_pixels")))%>%
#   rename_with(~ paste("spec", ., sep = "."), starts_with("median.")) %>%
#   rename_with(~ paste("spec", ., sep = "."), starts_with("n_pixels"))

colnames(canoe_spectral_df)
duplicates <- canoe_spectral_df[duplicated(canoe_spectral_df[, c("tag")]), ]%>%
  arrange(tag)


## adding all together:
length(unique(canoe_spectral_df$tag))
length(canoe_spectral_df$tag)

length(unique(can_L1_df$tag))
length(can_L1_df$tag)

length(unique(canoe_pols$tag))
length(canoe_pols$tag)

colnames(canoe_spectral_df)

Canoe_data <- full_join(canoe_pols,canoe_spectral_df, by = c("tag"))
Canoe_data <- full_join(Canoe_data,can_L1_df_renamed_2022_04_23, by = c("tag"))
# Canoe_data <- full_join(Canoe_data,can_L1_df_renamed_2023_12_17, by = c("tag"))

#edit EDITED
write.csv(Canoe_data,"E:\\Canoe\\Fdc_PR_Canoe_GBC_metrics.csv" )

# Calculate Pearson correlation coefficient
correlation <- cor(Canoe_data$ht2022_age24, Canoe_data$ALS.Zq99.2022_04_23,use = "pairwise.complete.obs")
correlation

# Plot the scatter plot
plot(Canoe_data$ht2022_age24, Canoe_data$ALS.Zq99.2022_04_23, main = "Scatter Plot of Variables", 
     xlab = "Variable 1", ylab = "Variable 2")
# Add the Pearson correlation coefficient to the plot
text(x = max(variable1), y = min(variable2), 
     labels = paste("Pearson correlation =", round(correlation, 2)))

Canoe_data$Al
colnames(Canoe_data)

### Rainbow A -----------------------------------------------------------
# pols with census info:
RA_pols_dir <- "A:\\WaiteOlivia_2\\Rainbow_L1\\2023_12_17_A\\05_ALIGNED_ALS\\pols\\Edited_CHM__06_masked_RGB_H20T_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss_sig_01w3_z75_crowns_2_AbsIncluded_ND.shp"
RA_pols = st_read(RA_pols_dir, crs = 26910) %>% 
  filter(!st_is_empty(.)) %>% 
  # filter(Edited != "4") #%>%
  st_make_valid() %>% 
  st_cast("POLYGON")%>%
  as.data.frame()%>%
  dplyr::select(!c("geometry"))%>%
  mutate(tag = paste(ROW, COL, sep = ""))
  
# Show the rows with duplicated tag numbers
duplicated_rows <- RA_pols %>%
  filter(tag %in% duplicated_tags$tag)
colnames(RA_pols)
dim(RA_pols)

RA_pols_filtered <- RA_pols%>%
  filter(!(tag == 3420 & EditedCrwn == 5))%>% #removing a duplicate that is bad
  arrange(tag, desc(crownArea)) %>%
  distinct(tag, .keep_all = TRUE)%>%
  mutate(ht2013_age15 = HT13,
         ht2021_age23 = Ht21)%>%
  select(c("tag","treeID","crownArea","COL","ROW","TRIAL","REP","SET","GenSam","FAM","LABEL","Msr_T_S","EditedCrwn","ht2013_age15","ht2021_age23"))

dim(RA_pols_filtered)
length(unique(RA_pols_filtered$tag))
colnames(RA_pols_filtered)

#L1 Metrics
RA_L1_date <- "2022_04_22"
RA_L1_files <- list.files(paste0("A:\\WaiteOlivia_2\\Rainbow_L1\\",RA_L1_date,"\\PlotA\\10_CROWNS_clean\\Structural_metrics\\"), pattern = ".rds$",  full.names= TRUE)
for(i in 1:length(RA_L1_files)){
  file <- readRDS(RA_L1_files[i])%>%
    as.data.frame()%>%
    mutate(Date = RA_L1_date)
  if(i==1){
    RA_L1_df <- file
  }else{
    RA_L1_df <- rbind(RA_L1_df, file)
  }
}
RA_L1_df
colnames(RA_L1_df)
#dec ALS
RA_L1_files_dec <- list.files("Q:\\GBC\\Data\\Cw_PR_Rainbow_A\\Flights\\2023_12_17\\2_Inputs\\L1\\05_ALIGNED_ALS\\output\\CROWNS_clean\\Structural_metrics\\", pattern = ".rds$",  full.names= TRUE)
# RA_L1_files <- list.files(paste0("A:\\WaiteOlivia_2\\Rainbow_L1\\",RA_L1_date,"\\PlotA\\10_CROWNS_clean\\Structural_metrics\\"), pattern = ".rds$",  full.names= TRUE)
for(i in 1:length(RA_L1_files_dec)){
  file <- readRDS(RA_L1_files_dec[i])%>%
    as.data.frame()%>%
    mutate(Date = RA_L1_date)
  if(i==1){
    RA_L1_df_dec <- file
  }else{
    RA_L1_df_dec <- rbind(RA_L1_df_dec, file)
  }
}
RA_L1_df
colnames(RA_L1_df)

#renaming L1 metrics
#Good to include the last 2 field heights that we have
#ALS: Best to select the flights that we have the most precise data for
RA_L1_df_renamed_2022_04_22 <- RA_L1_df %>%
  mutate(ALS.Zq99.2022_04_22 = Zq99,
         ALS.Zq975.2022_04_22 = Zq975,
         ALS.n_points.2022_04_22 = n_points,
         tag = as.character(tag))%>%
  select(c("tag","treeID","ALS.Zq99.2022_04_22","ALS.Zq975.2022_04_22","ALS.n_points.2022_04_22"))
colnames(RA_L1_df_renamed_2022_04_22)
RA_L1_df_renamed_2022_04_22
length(unique(RA_L1_df_renamed_2022_04_22$tag))

##december:
RA_L1_df_renamed_2023_12_17 <- RA_L1_df_dec %>%
  mutate(ALS.Zq99.2023_12_17 = Zq99,
         ALS.Zq975.2023_12_17 = Zq975,
         ALS.n_points.2023_12_17 = n_points,
         tag = as.character(tag))%>%
  select(c("tag","treeID","ALS.Zq99.2023_12_17","ALS.Zq975.2023_12_17","ALS.n_points.2023_12_17"))
colnames(RA_L1_df_renamed_2023_12_17)
RA_L1_df_renamed_2023_12_17
length(unique(RA_L1_df_renamed_2023_12_17$tag))




##Spectral metrics
RA_dates <- c(
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
GBC_indicies <- c("PRI","GCC","NDRE3","RE_total","CCI")

for(d in 1:length(RA_dates)){
  date <- RA_dates[d]
  print(date)
  RA_spectral <- readRDS(paste0("F:\\Cw_PR_Rainbow_SiteA\\2_Processed_Work\\Flights\\", date, "\\Updated_CSV_Index_NIR_Shadow\\", date, "_NIRshadowMask_MedianCrownSpectralIndices_GeneticSubset.rds")) %>%
    as.data.frame() %>%
    dplyr::select(matches(paste0("^median\\.", paste(GBC_indicies, collapse = "|"))), c("tag","treeID" ,"COL", "ROW","Area_buffer5cm","Area", "count"))%>%
    mutate(site = "Rainbow_A")%>%
    mutate(Date = date)%>%
    mutate(n_pixels = count)%>%
    # mutate(Date_M = format(Date, "%B_%d_%Y"))%>%
    dplyr::select(!c(count))%>%
    pivot_wider(names_from = Date, values_from = c(starts_with("median."), starts_with("n_pixels"))) %>%
    rename_with(~ gsub("_(?=\\d{4})", ".", ., perl = TRUE), c(starts_with("median."), starts_with("n_pixels")))%>%
    rename_with(~ if_else(. == "tag" | . == "site" |. == "COL"|. == "ROW"|. == "Area"|. == "Area_buffer5cm"|. == "treeID", ., paste0("spec.", .)), -c(tag, site))
  

  if(d==1){
    RA_spectral_df <- RA_spectral
  }else{
    RA_spectral_df <- full_join(RA_spectral_df, RA_spectral, by = c("tag","treeID","site", "COL", "ROW","Area_buffer5cm","Area"))
  }
}
# test <- RA_spectral %>%
#   filter(tag %in% c("465"))
# 465
colnames(RA_spectral_df)
# length(unique(RA_spectral_df$tag))

#tag 3424 (both dont exist, remove smaller)| tag 3420 remove edited 5, tag 3218 remove smaller
RA_dup_remove_smaller <- c("3424","3218")

RA_spectral_df_filtered <- RA_spectral_df%>%
  mutate(EditedCrownArea = Area,
         EditedCrownArea_MSbufferIn5cm = Area_buffer5cm)%>%
  group_by(tag) %>%
  slice_min(Area) %>%
  ungroup() %>%  
  select(!c("Area_buffer5cm","Area"))%>%
  distinct()

colnames(RA_spectral_df_filtered)
dim(RA_spectral_df_filtered)
dim(RA_pols)
dim(RA_L1_df)

length(unique(RA_spectral_df_filtered$tag))
length(unique(RA_pols$tag))
length(unique(RA_L1_df$tag))
# renaming spectral metrics: 
# heat:  PRI, RE_total and NDRE3
# drought: CCI, GCC, RE_total
#PRI.July_27_2022 or maybe median.PRI.date so we dont forget we took the median


#looking for duplicates:
duplicates <- RA_L1_df_renamed_2023_12_17$tag[duplicated(RA_L1_df_renamed_2023_12_17$tag)]
RA_dup <- RA_L1_df_renamed_2023_12_17 %>%
  filter(tag %in% duplicates)%>%
  dplyr::select(c("tag"))
RA_dup

dim(RA_pols_filtered)#no duplicates, tags: 228 2324 not
dim(RA_spectral_df_filtered) #no duplicates
dim(RA_L1_df_renamed_2022_04_22) #no duplicates
dim(RA_L1_df_renamed_2023_12_17)

## adding all together:
colnames(RA_pols_filtered)#no duplicates
colnames(RA_spectral_df_filtered) #no duplicates
colnames(RA_L1_df_renamed_2022_04_22)
colnames(RA_L1_df_renamed_2023_12_17)

Rainbow_PlotA_data <- full_join(RA_pols_filtered,RA_spectral_df_filtered,by = c("treeID","tag", "ROW","COL"))
Rainbow_PlotA_data <- full_join(Rainbow_PlotA_data, RA_L1_df_renamed_2022_04_22, by = c("tag","treeID"))
# Rainbow_PlotA_data <- full_join(Rainbow_PlotA_data, RA_L1_df_renamed_2023_12_17,by = c("tag","treeID"))
Rainbow_PlotA_data$tag <- paste0(Rainbow_PlotA_data$ROW,1,Rainbow_PlotA_data$COL)

colnames(Rainbow_PlotA_data)
dim(Rainbow_PlotA_data)

duplicates <- Rainbow_PlotA_data$tag[duplicated(Rainbow_PlotA_data$tag)]
RA_dup <- Rainbow_PlotA_data %>%
  filter(tag %in% duplicates)%>%
  dplyr::select(c("tag","ROW","COL","EditedCrwn" ))
RA_dup$tag

remove_RA <- c("3424")
### Rainbow B -----------------------------------------------------------
# census info:
RB_pols_dir <- "A:\\WaiteOlivia_2\\Rainbow_L1\\pols_B\\CHM_06_masked_RGB_H20T_thres45percent_2022_06_07_NAfilledminw3_medw3_gauss_sig_01w3_z75_crowns_2_EDITED_legend_ND.shp"
RB_pols = st_read(RB_pols_dir, crs = 26910) %>% 
  filter(!st_is_empty(.)) %>% 
  st_make_valid() %>% 
  st_cast("POLYGON")%>%
  as.data.frame()%>%
  mutate(ht2013_age14 = HT13,
         ht2021_age22 = HT21)%>%
  dplyr::select(!c("Z","geometry", "crownArea","HT13","DBH13","HT21","DBH21","Absent", "Edited","random_num", "Edited_str"))

colnames(RB_pols)
length(unique(RB_pols$tag))
length(RB_pols$tag)


#L1 Metrics
#Zq99.Month_0d_YYYY
RB_L1_date <- c("2022_04_22")
RB_L1_files <- list.files(paste0("A:\\WaiteOlivia_2\\Rainbow_L1\\",RB_L1_date,"\\PlotB\\10_CROWNS_clean\\Structural_metrics\\"), pattern = ".rds$",  full.names= TRUE)

for(i in 1:length(RB_L1_files)){
  file <- readRDS(RB_L1_files[i])%>%
    as.data.frame()%>%
    mutate(Date = RB_L1_date)
  if(i==1){
    RB_L1_df <- file
  }else{
    RB_L1_df <- rbind(RB_L1_df, file)
  }
}

colnames(RB_L1_df)

RB_L1_df_renamed_2022_04_22 <- RB_L1_df %>%
  mutate(Als.2022_04_22.Zq99 = Zq99,
         Als.2022_04_22.Zq975 = Zq975,
         Als.2022_04_22.n_points = n_points,
         tag = as.character(tag))%>%
  select(c("tag","treeID","Als.2022_04_22.n_points","Als.2022_04_22.Zq975","Als.2022_04_22.Zq99"))
colnames(RB_L1_df_renamed_2022_04_22)
RB_L1_df_renamed_2022_04_22
length(unique(RB_L1_df_renamed_2022_04_22$tag))
length(unique(RB_L1_df_renamed_2022_04_22$treeID))



RB_L1_df
colnames(RB_L1_df)

#Spectral metrics
RB_dates <- c(
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

for(d in 1:length(RB_dates)){
  date <- RB_dates[d]
  print(date)
  RB_spectral <- readRDS(paste0("A:\\WaiteOlivia_2\\Cw_PR_Rainbow_SiteB\\2_Processed_Work\\Flights\\",date,"\\Updated_CSV_Index_NIR_Shadow\\",date,"_NIRshadowMask_MedianCrownSpectralIndices_GeneticSubset.rds"))%>%
    as.data.frame() %>%
    dplyr::select(matches(paste0("^median\\.", paste(GBC_indicies, collapse = "|"))), c("tag", "COL", "ROW","Area_buffer5cm","Area", "count"))%>%
    mutate(site = "Rainbow_B")%>%
    mutate(Date = ymd(date))%>%
    mutate(Date_M = format(Date, "%B_%d_%Y"))%>%
    dplyr::select(!c(Date))%>%
    pivot_wider(names_from = Date_M, values_from = starts_with("median.")) %>%
    rename_with(~ gsub("_(?=[A-Za-z]+_\\d{2}_\\d{4})", ".", ., perl = TRUE), starts_with("median."))
  
  if(d==1){
    RB_spectral_df <- RB_spectral
  }else{
    RB_spectral_df <- full_join(RB_spectral_df, RB_spectral, by = c("tag","site", "COL", "ROW","Area_buffer5cm","Area", "count"))
  }
}

colnames(RB_spectral_df)

RB_spectral_df <- RB_spectral_df%>%
  mutate(n_pixels = count,
         EditedCrownArea = Area,
         EditedCrownArea_MSbufferIn5cm = Area_buffer5cm)%>%
  dplyr::select(!c(Area, count, Area_buffer5cm))

colnames(RB_spectral_df)
RB_spectral_df$site

### all all RB toegther
colnames(RB_pols)
colnames(RB_L1_df_renamed_2022_04_22)
RB_Data  <- full_join(RB_pols, RB_L1_df_renamed_2022_04_22, by = c("treeID"))

colnames(RB_Data)
length(RB_Data$treeID)
length(unique(RB_Data$treeID))

colnames(Rainbow_PlotA_data)
[1] "tag"                               "crownArea"                         "COL"                              
[4] "ROW"                               "TRIAL"                             "REP"                              
[7] "SET"                               "GenSam"                            "FAM"                              
[10] "LABEL"                             "Msr_T_S"                           "EditedCrwn"                       
[13] "ht2013_age15"                      "ht2021_age23"                      "site"       
