REM for aligned out of CC ALS clouds
REM for working dir
REM Rainbow dates:


set cores=4
set date=2023_06_22

REM plot B 
set f_wd=O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotB   
REM projecting Lidar to NAD 83
las2las -i O:\PARSER_Ext\Rainbow_L1\2023_06_22\Aligned_Clouds\lidars\terra_las\projected_to_NAD83\PlotB\CC_alignedToRGBdap_50overlap\*.las ^
    -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotB\01_proj_NAD83 ^
    -odix _nad83 ^
    -olaz ^
    -nad83 ^
    -utm 10north ^
    -cpu64 ^
    -v
    
REM rescale 
las2las -i O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotB\01_proj_NAD83\*.laz ^
        -rescale 0.01 0.01 0.01 ^
        -cpu64 ^
        -utm 10north ^
        -v ^
        -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotB\02_rescaled ^
        -olaz
REM reports
lasinfo -i O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotB\02_rescaled\*.laz ^
        -cd ^
        -stdout ^
        -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotB\01_las_reports ^
        -otxt ^
        -cores %cores% 
        
REM index
lasindex -i O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotB\02_rescaled\*.laz
        -cores %cores%
REM ::Tile 15m - flag buffered points as withheld for simple drop later
lastile -i O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotB\02_rescaled\*.laz ^
    -tile_size 15 ^
    -buffer 4 ^
    -flag_as_withheld ^
    -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotB\03_tile ^
    -olaz ^
    -cores %cores% 

REM
REM
REM plot A  
REM
REM

set f_wd=O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotA  
REM projecting Lidar to NAD 83
las2las -i O:\PARSER_Ext\Rainbow_L1\2023_06_22\Aligned_Clouds\lidars\terra_las\projected_to_NAD83\PlotA\CC_alignedToRGBdap_50overlap\*.las ^
    -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotA\01_proj_NAD83 ^
    -odix _nad83 ^
    -olaz ^
    -nad83 ^
    -utm 10north ^
    -cpu64 ^
    -v
    
REM rescale 
las2las -i O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotA\01_proj_NAD83\*.laz ^
        -rescale 0.01 0.01 0.01 ^
        -cpu64 ^
        -utm 10north ^
        -v ^
        -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotA\02_rescaled ^
        -olaz
REM reports
lasinfo -i O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotA\02_rescaled\*.laz ^
        -cd ^
        -stdout ^
        -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotA\01_las_reports ^
        -otxt ^
        -cores %cores% 
        
REM index
lasindex -i O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotA\02_rescaled\*.laz
        -cores %cores%
REM ::Tile 15m - flag buffered points as withheld for simple drop later
lastile -i O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotA\02_rescaled\*.laz ^
    -tile_size 15 ^
    -buffer 4 ^
    -flag_as_withheld ^
    -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22\PlotA\03_tile ^
    -olaz ^
    -cores %cores% 



REM
REM
REM Canoe
REM 
REM 2022_04_23 : G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2022_04_23\Aligned_Clouds\lidars\terra_las\projected_to_NAD83\CC_alignedToRGBdap_50overlap\*.las
REM 2022_11_28: G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2023_11_28\Aligned_Clouds\lidars\terra_las\projected_to_NAD83\CC_alignedToRGBdap_50overlap\*.laz

REM projecting Lidar to NAD 83
las2las -i G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2022_11_28\Aligned_Clouds\lidars\terra_las\projected_to_NAD83\CC_alignedToRGBdap_50overlap\*.laz ^
    -odir G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2022_11_28\01_proj_NAD83 ^
    -odix _nad83 ^
    -olaz ^
    -nad83 ^
    -utm 10north ^
    -cpu64 ^
    -v
    
REM rescale 
las2las -i G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2022_11_28\01_proj_NAD83\*.laz ^
        -rescale 0.01 0.01 0.01 ^
        -cpu64 ^
        -utm 10north ^
        -v ^
        -odir G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2022_11_28\02_rescaled ^
        -olaz
REM reports
lasinfo -i G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2022_11_28\02_rescaled\*.laz ^
        -cd ^
        -stdout ^
        -odir G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2022_11_28\01_las_reports ^
        -otxt ^
        -cores %cores% 
        
REM index
lasindex -i G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2022_11_28\02_rescaled\*.laz
        -cores %cores%
REM ::Tile 15m - flag buffered points as withheld for simple drop later
lastile -i G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2022_11_28\02_rescaled\*.laz ^
    -tile_size 15 ^
    -buffer 4 ^
    -flag_as_withheld ^
    -odir G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2022_11_28\03_tile ^
    -olaz ^
    -cores %cores% 
 
 
REM 2023_06_21

REM projecting Lidar to NAD 83
las2las -i G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2023_06_21\Aligned_Clouds\lidars\terra_las\projected_to_NAD83\CC_alignedToRGBdap_50overlap\*.laz ^
    -odir G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2023_06_21\01_proj_NAD83 ^
    -odix _nad83 ^
    -olaz ^
    -nad83 ^
    -utm 10north ^
    -cpu64 ^
    -v
    
REM rescale 
las2las -i G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2023_06_21\01_proj_NAD83\*.laz ^
        -rescale 0.01 0.01 0.01 ^
        -cpu64 ^
        -utm 10north ^
        -v ^
        -odir G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2023_06_21\02_rescaled ^
        -olaz
REM reports
lasinfo -i G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2023_06_21\02_rescaled\*.laz ^
        -cd ^
        -stdout ^
        -odir G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2023_06_21\01_las_reports ^
        -otxt ^
        -cores %cores% 
        
REM index
lasindex -i G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2023_06_21\02_rescaled\*.laz
        -cores %cores%
REM ::Tile 15m - flag buffered points as withheld for simple drop later
lastile -i G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2023_06_21\02_rescaled\*.laz ^
    -tile_size 15 ^
    -buffer 4 ^
    -flag_as_withheld ^
    -odir G:\PARSER_Ext\Fdc_PR_Canoe\Canoe_L1\2023_06_21\03_tile ^
    -olaz ^
    -cores %cores% 
 