::2021 lastools processing tutorial
::Tristan Goodbody -- goodbody.t@alumni.ubc.ca

:: Set global variables
set f_in=D:\Fdc_PR_Canoe\Flights\2022_04_23\2_Inputs\lidar\~_DJI\Fdc_PR_Canoe_2022_04_23\lidars\terra_las
set f_out=D:\Fdc_PR_Canoe\Flights\2022_04_23\2_Inputs\lidar

set cores=1

:: processing stream
:: if a folder doesnt already exist -> make that folder
IF NOT EXIST %f_out% MKDIR %f_out%
IF NOT EXIST %f_out%\00_reports MKDIR %f_out%\00_reports
IF NOT EXIST %f_out%\01_tile MKDIR %f_out%\01_tile
REM IF NOT EXIST %f_out%\02_optimized MKDIR %f_out%\02_optimized
IF NOT EXIST %f_out%\02_class MKDIR %f_out%\02_class
IF NOT EXIST %f_out%\03_norm MKDIR %f_out%\03_norm
IF NOT EXIST %f_out%\04_filtered MKDIR %f_out%\04_filtered
IF NOT EXIST %f_out%\05_raster\dtm MKDIR %f_out%\05_raster\dtm
IF NOT EXIST %f_out%\05_raster\dsm MKDIR %f_out%\05_raster\dsm
IF NOT EXIST %f_out%\05_raster\chm MKDIR %f_out%\05_raster\chm
IF NOT EXIST %f_out%\05_raster\den MKDIR %f_out%\05_raster\den

REM pause
REM ::Info about data
REM lasinfo -i %f_in%\*.las ^
    REM -cd ^
    REM -stdout ^
    REM -odir %f_out%\00_reports ^
    REM -otxt ^
    REM -cores %cores% 
REM pause
REM lasindex -i %f_in%\*.las -cores %cores%
REM pause 
REM ::Tile 100m - flag buffered points as withheld for simple drop later
REM lastile -i %f_in%\*.las ^
    REM -tile_size 100 ^
    REM -buffer 10 ^
    REM -flag_as_withheld ^
    REM -odir %f_out%\01_tile ^
    REM -olaz ^
    REM -cores %cores%
REM pause
REM :: Optimize las
REM lasoptimize -i %f_out%\01_tile\*.laz ^
    REM -odir %f_out%\02_optimized ^
    REM -cores %cores%
	
REM pause
::Generate boundary shapefile
REM lasboundary -i %f_out%\01_tile\*.laz -drop_withheld -merged -o %f_out%\boundary.shp

::Denoise
REM lasnoise -i %f_out%\01_tile\*.laz ^
    REM -step 2 ^
    REM -isolated 3 ^
    REM -odix _denoised ^
    REM -olaz ^
    REM -cores %cores% 
REM pause
REM ::Classify ground
REM lasground -i %f_out%\01_tile\*_denoised.laz ^
    REM -odir %f_out%\02_class ^
    REM -olaz ^
    REM -cores %cores%  
REM pause
REM ::Normalize height
REM lasheight -i %f_out%\02_class\*_denoised.laz ^
    REM -replace_z ^
    REM -odir %f_out%\03_norm ^
    REM -olaz ^
    REM -cores %cores%
REM pause

::filter height
lasheight -i %f_out%\03_norm\*_denoised.laz ^
    -drop_above 55^
	-drop_below 0^
    -odir %f_out%\04_filtered ^
    -olaz ^
    -cores %cores%



:: make DTM
blast2dem -i %f_out%\02_class\*_denoised.laz ^
    -keep_class 2 ^
    -step 0.05 ^
    -use_tile_bb ^
    -odix _dtm ^
    -obil ^
    -odir %f_out%\05_raster\dtm ^
    -kill 75 ^
    -cores %cores%
pause
::merge DTM .bil files
lasgrid -i %f_out%\05_raster\dtm\*.bil -merged ^
    -step 0.05 ^
    -false ^
    -o %f_out%\05_raster\DTM_.png   
pause
:: make DSM
blast2dem -i %f_out%\02_class\*_denoised.laz ^
    -step 0.05 ^
    -kill 75 ^
    -use_tile_bb ^
    -odix _dsm  ^
    -obil ^
    -odir %f_out%\05_raster\dsm ^
    -cores %cores%
REM pause
::merge DSM .bil files
lasgrid -i %f_out%\05_raster\dsm\*.bil -merged ^
    -step 0.05 ^
    -false ^
    -o %f_out%\05_raster\DSM_.png
pause
:: make CHM
lasgrid -i %f_out%\03_norm\*.laz ^
    -max ^
    -first_only ^
    -step 0.05 ^
    -odir %f_out%\05_raster\chm ^
    -odix _chm ^
    -obil ^
    -cores %cores%
pause
lasgrid -i %f_out%\05_raster\chm\*.bil -merged ^
    -step 0.05 ^
    -false ^
    -o %f_out%\05_raster\CHM_.png

pause
:: Compute density image
lasgrid -i %f_out%\04_filtered\*.laz ^
    -step 0.05 ^
    -point_density_16bit ^
    -odir %f_out%\05_raster\den ^
    -odix _den  -obil ^
    -cores %cores%
pause
:: tif
lasgrid -i %f_out%\05_raster\den\*.bil -merged ^
    -step 0.05 ^
    -o %f_out%\05_raster\density_allreturns_.tif
pause
:: png      
lasgrid -i %f_out%\05_raster\den\*.bil -merged ^
    -step 0.05 ^
    -false -set_min_max 0 5 ^
    -o %f_out%\05_raster\density_allreturns_0_5_.png
pause
:: Compute structural metrics
lascanopy -i %f_out%\04_filtered\*.laz ^
    -merged ^
    -p 10 25 50 75 99 ^
    -step 20 ^
    -o %f_out%\05_raster\struc.tif