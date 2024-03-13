set f_out=E:\Cw_PR_Rainbow\Flights\2022_04_22\2_Inputs\lidar\LAStools\PlotA\
set f_in=E:\Cw_PR_Rainbow\Flights\2022_04_22\2_Inputs\lidar\R\output\PlotB_Red\TILES_Thinned\CloudCompare

IF NOT EXIST %f_out%\00_rescaled MKDIR %f_out%00_rescaled
IF NOT EXIST %f_out%\00_reports MKDIR %f_out%\00_reports
IF NOT EXIST %f_out%\01_tile MKDIR %f_out%\01_tile
IF NOT EXIST %f_out%\02_ground\01_thin_25cmGrid MKDIR %f_out%\02_ground\01_thin_25cmGrid
IF NOT EXIST %f_out%\02_ground\02_ground_25cmGrid MKDIR %f_out%\02_ground\02_ground_25cmGrid
IF NOT EXIST %f_out%\02_ground\03_ground_thick MKDIR %f_out%\02_ground\03_ground_thick
IF NOT EXIST %f_out%\02_ground\04_median MKDIR %f_out%\02_ground\04_median
IF NOT EXIST %f_out%\02_ground\05_ground_median MKDIR %f_out%\02_ground\05_ground_median


set cores=4


REM ::Coming out of cloud compare once aligned to DAP cloud
las2las -i %f_in%\*.las ^
		-rescale 0.01 0.01 0.01 ^
		-cpu64 ^
		-utm 10north ^
		-v ^
		-odir %f_out%\00_rescaled
	
lasinfo -i %f_out%\00_rescaled\*.las ^
		-cd ^
		-stdout ^
		-odir %f_out%\00_reports ^
		-otxt ^
		-cores %cores% 
	
lasindex -i %f_out%\00_rescaled\*.las -cores %cores%

REM ::Tile 15m - flag buffered points as withheld for simple drop later
lastile -i %f_out%\00_rescaled\*.las ^
	-tile_size 15 ^
	-buffer 4 ^
	-flag_as_withheld ^
	-odir %f_out%\01_tile ^
	-olaz ^
	-cores %cores%
	
REM class 7 is teh noise class, set classification is putting all other classes into class 1, step 0.25 is creating a 25cm by 25cm grid, 
REM percentile 5 10 is reclassificying the points closest to the 5th percentile as class 8
lasthin -i %f_out%\01_tile\*.laz -ignore_class 7 -set_classification 1 -step 0.25 -percentile 5 10 -classify_as 8 -odir %f_out%\02_ground\01_thin_25cmGrid -olaz -cores %cores%

REM :: Asked Liam and he thinks below chunk gets over written by the next one anyway so comment out		 
REM lasground -i %f_out%\02_ground\01_thin_25\*.laz ^
          REM -ignore_class 1 7 ^ :: ignoring class 1 and 7
		  REM -bulge 1.5 ^
		  REM -spike 0.2 ^ :: removes spikes above 20cm
          REM -extra_fine ^ :: ideal for hills/slopes but also detailed flat
          REM -odir %f_out%\02_ground\02_thin_25 -olaz ^
          REM -cores %cores%
		  
REM ::>> lasground -i lidar.las -o lidar_with_bare_earth.las -city

REM ultra_fine is the highest setting
lasground -i %f_out%\02_ground\01_thin_25cmGrid\*.laz ^
-ignore_class 1 ^
-ultra_fine ^
-ground_class 2 ^
-odir %f_out%\02_ground\02_ground_25cmGrid -olaz ^
-cores %cores%

REM ::>> lasheight -i lidar_with_bare_earth.las -o lidar_with_heights.las
REM :: classify between: creating a thick ground with points 5cm below to "low ground' derived and 15cm above it and temporarily using class 6 to capture the thickened ground
lasheight -i %f_out%\02_ground\02_ground_25cmGrid\*.laz ^
-classify_between -0.05 0.15 6 ^
-odir %f_out%\02_ground\03_ground_thick -olaz ^
-cores %cores%

REM :: calcuating the median ground values
lasthin -i %f_out%\02_ground\03_ground_thick\*.laz ^
-ignore_class 1 ^
-step 0.5 -percentile 50 -classify_as 8 ^
-odir %f_out%\02_ground\04_median -olaz ^
-cores %cores%

REM ::lasclassify -i lidar_with_heights.las -o lidar_classified.las
REM rugged 0.3 says grid cell points above this standard deviation are potential vegetation
REM ground_offset says at maximum points 1.5m above the current ground estimate will be incluced
lasclassify -i %f_out%\02_ground\04_median\*.laz ^
-change_classification_from_to 6 2 ^
-rugged 0.3 ^-ground_offset 1.5 ^
-odir %f_out%\02_ground\05_ground_median -olaz ^
-cores %cores%
	

REM Liams original code belowL:
REM lasthin -i %f_out%\02_optimized\*.laz ^
        REM -ignore_class 7 ^
		REM -set_classification 1 ^ :: putting all other classes into class 1
        REM -step 0.25 ^ :: in every 25cm by 25cm grid
		REM -percentile 5 10 ^ :: reclassify points closest to the 5th percentile as class 8
		REM -classify_as 8 ^
        REM -odir %f_out%\02_optimized\thin_20 -olaz ^
        REM -cores %cores%
		 
REM lasground -i %f_out%\02_optimized\thin_20\*.laz ^
          REM -ignore_class 1 7 ^ :: ignoring class 1 and 7
		  REM -bulge 1.5 ^
		  REM -spike 0.2 ^ :: removes spikes above 20cm
          REM -extra_fine ^ :: ideal for hills/slopes but also detailed flat
          REM -odir %f_out%\03_class\thin_20 -olaz ^
          REM -cores %cores%
		 
REM lasground -i %f_out%\02_optimized\thin_20\*.laz ^
          REM -ignore_class 1 ^
          REM -ultra_fine ^ ::highest setting
		  REM -ground_class 2 ^
          REM -odir %f_out%\03_class\thin_20 -olaz ^
          REM -cores %cores%
		  
REM lasheight -i %f_out%\03_class\thin_20\*.laz ^
          REM -classify_between -0.05 0.15 6 ^ :: creating a thick ground with points 5cm below to "low ground' derived and 15cm above it and temporarily using class 6 to capture the thickened ground
          REM -odir %f_out%\03_class\thick -olaz ^
          REM -cores %cores%
		  
REM lasthin -i %f_out%\03_class\thick\*.laz ^
        REM -ignore_class 1 ^
        REM -step 0.5 -percentile 50 -classify_as 8 ^
        REM -odir %f_out%\03_class\median -olaz ^ :: calcuating the median ground values
        REM -cores %cores%
		
REM lasclassify ^
	REM -i %f_out%\03_class\median\*.laz ^
	REM -change_classification_from_to 6 2 ^
	REM -rugged 0.3 ^
	REM -ground_offset 1.5 ^ :: at maximum points 1.5m above the current ground estimate will be incluced
	REM -odir %f_out%\03_class\median\ground -olaz ^
	REM -cores %cores%