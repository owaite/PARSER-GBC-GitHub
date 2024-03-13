::2021 lastools processing tutorial
::Tristan Goodbody -- goodbody.t@alumni.ubc.ca

:: Set global variables
set f_in=C:\Users\owaite\Documents\DJI\DJITerra\olivia.jm.waite@gmail.com\2\lidars\terra_las
set f_out=D:\Cw_PR_Rainbow\Flights\2022_04_22\2_Inputs\lidar

set cores=4

:: processing stream
:: if a folder doesnt already exist -> make that folder
IF NOT EXIST %f_out% MKDIR %f_out%
IF NOT EXIST %f_out%\~_reports MKDIR %f_out%\~_reports
IF NOT EXIST %f_out%\01_tile MKDIR %f_out%\01_tile
IF NOT EXIST %f_out%\02_optimized MKDIR %f_out%\02_optimized
IF NOT EXIST %f_out%\03_class MKDIR %f_out%\03_class
IF NOT EXIST %f_out%\04_norm MKDIR %f_out%\04_norm
IF NOT EXIST %f_out%\05_raster\dtm MKDIR %f_out%\05_raster\dtm
IF NOT EXIST %f_out%\05_raster\dsm MKDIR %f_out%\05_raster\dsm
IF NOT EXIST %f_out%\05_raster\chm MKDIR %f_out%\05_raster\chm
IF NOT EXIST %f_out%\05_raster\den MKDIR %f_out%\05_raster\den

pause
::Info about data
lasinfo -i %f_in%\*.las ^
    -cd ^
    -stdout ^
    -odir %f_out%\~_reports ^
    -otxt ^
    -cores %cores% 
pause 
::Tile 100m - flag buffered points as withheld for simple drop later
lastile -i %f_in%\*.las ^
    -tile_size 100 ^
    -buffer 10 ^
    -flag_as_withheld ^
    -odir %f_out%\01_tile ^
    -olaz ^
    -cores %cores%
pause
:: Optimize las
lasoptimize -i %f_out%\01_tile\*.laz ^
    -odir %f_out%\02_optimized ^
    -cores %cores%
pause
::Generate boundary shapefile
REM lasboundary -i %f_out%\02_optimized\*.laz -drop_withheld -merged -o %f_out%\boundary.shp

::Denoise
lasnoise -i %f_out%\02_optimized\*.laz ^
    -step 2 ^
    -isolated 3 ^
    -odix _denoised ^
    -olaz ^
    -cores %cores% 
pause
::Classify ground
lasground -i %f_out%\02_optimized\*_denoised.laz ^
    -odir %f_out%\03_class ^
    -olaz ^
    -cores %cores%  
pause
::Normalize height
lasheight -i %f_out%\03_class\*_denoised.laz ^
    -replace_z ^
    -odir %f_out%\04_norm ^
    -olaz ^
    -cores %cores%
pause
:: make DTM
blast2dem -i %f_out%\03_class\*_denoised.laz ^
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
    -step 1 ^
    -false ^
    -o %f_out%\05_raster\DTM_.png   
pause
:: make DSM
blast2dem -i %f_out%\03_class\*_denoised.laz ^
    -step 0.01 ^
    -kill 75 ^
    -use_tile_bb ^
    -odix _dsm  ^
    -obil ^
    -odir %f_out%\05_raster\dsm ^
    -cores %cores%
pause
::merge DSM .bil files
lasgrid -i %f_out%\05_raster\dsm\*.bil -merged ^
    -step 1 ^
    -false ^
    -o %f_out%\05_raster\DSM_.png
pause
:: make CHM
lasgrid -i %f_out%\04_norm\*.laz ^
    -max ^
    -first_only ^
    -step 1 ^
    -odir %f_out%\05_raster\chm ^
    -odix _chm ^
    -obil ^
    -cores %cores%
pause
lasgrid -i %f_out%\05_raster\chm\*.bil -merged ^
    -step 1 ^
    -false ^
    -o %f_out%\05_raster\CHM_.png
pause
:: Compute density image
lasgrid -i %f_out%\04_norm\*.laz ^
    -step 1 ^
    -point_density_16bit ^
    -odir %f_out%\05_raster\den ^
    -odix _den  -obil ^
    -cores %cores%
pause
:: tif
lasgrid -i %f_out%\05_raster\den\*.bil -merged ^
    -step 1 ^
    -o %f_out%\05_raster\density_allreturns_.tif
pause
:: png      
lasgrid -i %f_out%\05_raster\den\*.bil -merged ^
    -step 1 ^
    -false -set_min_max 0 5 ^
    -o %f_out%\05_raster\density_allreturns_0_5_.png
pause
:: Compute structural metrics
lascanopy -i %f_out%\04_norm\*.laz ^
    -merged ^
    -p 10 25 50 75 99 ^
    -step 20 ^
    -o %f_out%\05_raster\struc.tif