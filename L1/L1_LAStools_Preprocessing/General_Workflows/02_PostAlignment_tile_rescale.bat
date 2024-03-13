REM Script to use after alignment of lidar to DAP in CloudCompare
REM (1) ensures the lidar cloud is in the right projection (UTM 10N)
REM (2) rescales to be read into Rainbow_L1\2023_06_22\PlotB
REM (3) writes 

set cores=4
set date=2023_06_22

REM plot B 
set f_wd=O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotB   
REM projecting aligned Lidar to NAD 83 UTM 10N
REM change "path_to_aligned_lidar" to the path to the aligned L1 data exported from CC
REM \*.las takes the las file in that folder, change to the file name if you want to specify the file and have more than one .las in the folder
REM change "path_to_write\01_proj_NAD83" to the path you would like the new .laz file to be written to 
REM previous "path_to_write" was O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotB

las2las -i path_to_aligned_lidar\*.las ^
    -odir path_to_write\01_proj_NAD83 ^
    -odix _nad83 ^
    -olaz ^
    -nad83 ^
    -utm 10north ^
    -cpu64 ^
    -v
    
REM rescale 
REM aligned L1 is exported in CC at the highest resolution which changes the scale of the data
REM necessary to rescale to x,y,z of 0.01 to avoid warnings/erros in R
REM below the "path_to_write" is in the input dir and the output dir because its the main folder we are now working in
REM change poth the input (-i) and output (-odir)to match your data structure
las2las -i path_to_write\01_proj_NAD83\*.laz ^
        -rescale 0.01 0.01 0.01 ^
        -cpu64 ^
        -utm 10north ^
        -v ^
        -odir path_to_write\02_rescaled ^
        -olaz
		
REM reports
REM generating las reports
REM lasinfo: Reports the contents of the header and a short summary of thepoints
REM cd : compute point density
REM stdout : 
lasinfo -i path_to_write\02_rescaled\*.laz ^
        -cd ^
        -stdout ^
        -odir path_to_write\01_las_reports ^
        -otxt ^
        -cores %cores% 
        
REM lasindex : Creates a *.lax file for a given *.las or *.laz file that contains spatial indexing information. 
REM When this LAX file is present it will be used to speed up access to the relevant areas of the LAS/LAZ file 
REM whenever a spatial queries of the type
lasindex -i path_to_write\02_rescaled\*.laz
        -cores %cores%
		
		
REM Tile 15m with 4m buffers- flag buffered points as withheld for simple drop later in R
REM output in 03_tile will be the input in the next step in R
lastile -i path_to_write\02_rescaled\*.laz ^
    -tile_size 15 ^
    -buffer 4 ^
    -flag_as_withheld ^
    -odir path_to_write\03_tile ^
    -olaz ^
    -cores %cores% 
