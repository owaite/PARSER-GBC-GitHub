REM set date_folder = 2022_04_22_v39
REM date: 2022_11_20_v393
REM date: 2023_06_22_v39

pause

REM projecting DAP to NAD 83
las2las -i L:\Cw_PR_Rainbow_SiteA\Flights\2022_06_07\2_Inputs\metashape\1_TILES\RGB_DenseCloud\Clipped\RGB_DenseCloud_PlotA.laz ^
	-odir L:\Cw_PR_Rainbow_SiteA\Flights\2022_06_07\2_Inputs\metashape\1_TILES\RGB_DenseCloud\Clipped\ ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v
REM projecting Lidar to NAD 83
las2las -i O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\cloud_merged.las ^
	-odir O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83 ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v
REM dropping poitns - odd flight lines
las2las -i O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\cloud_merged_nad83.laz ^
	-odir O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83 ^
	-odix _droppedPtsAbove160m ^
	-olaz ^
	-drop_z_above 160 ^
	-cpu64 ^
	-v

pause
REM clipping RA lidar to shp, used larger RA shp for 2022_11_20
set RA_poly = O:\PARSER_Ext\Rainbow_L1\PlotA\Rainbow_A_shp_nad83\Larger_shp\Rainbow_PlotA_large.shp
lasclip -i O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\cloud_merged_nad83_droppedPtsAbove160m.laz -merged -poly O:\PARSER_Ext\Rainbow_L1\PlotA\Rainbow_A_shp_nad83\Larger_shp\Rainbow_PlotA_large.shp -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\PlotA\ -odix _plotA -olaz
	

REM clipping RB lidar to shp
REM DO PLOT B FOR 2022_11_20_v393
set RB_poly = O:\PARSER_Ext\Rainbow_L1\PlotB\Rainbow_B_shp_nad83\Rainbow_B_Project.shp
lasclip -i O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\cloud_merged_nad83_droppedPtsAbove160m.laz -merged -poly O:\PARSER_Ext\Rainbow_L1\PlotB\Rainbow_B_shp_nad83\Rainbow_B_Project.shp -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\PlotB\ -odix _plotB -olaz


REM reading in clipped DAP cloud and saving from a las to a laz for CC
REM RA
las2las -i L:\Cw_PR_Rainbow_SiteA\Flights\2022_06_07\2_Inputs\metashape\1_TILES\RGB_DenseCloud\Clipped\RGB_DenseCloud_PlotA.las ^
	-odir L:\Cw_PR_Rainbow_SiteA\Flights\2022_06_07\2_Inputs\metashape\1_TILES\RGB_DenseCloud\Clipped\ ^
	-olaz ^
	-cpu64 ^
	-v

REM RB
las2las -i L:\Cw_PR_Rainbow_SiteB\Flights\2022_06_07\2_Inputs\metashape\1_TILES\DenseCloud_filtered\Clipped\DenseCloud_NoNormals_NAD83_offset.las ^
	-odir L:\Cw_PR_Rainbow_SiteB\Flights\2022_06_07\2_Inputs\metashape\1_TILES\DenseCloud_filtered\Clipped\ ^
	-olaz ^
	-cpu64 ^
	-v



REM CANOE
REM projecting Lidar to NAD 83
las2las -i O:\PARSER_Ext\Canoe_L1\2022_04_23_v39\lidars\terra_las\cloud_merged.las ^
	-odir O:\PARSER_Ext\Canoe_L1\2022_04_23_v39\lidars\terra_las\projected_to_NAD83 ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v

REM 2022_11_28_v39
las2las -i O:\PARSER_Ext\Canoe_L1\2022_11_28_v39\lidars\terra_las\cloud_merged.las ^
	-odir O:\PARSER_Ext\Canoe_L1\2022_11_28_v39\lidars\terra_las\projected_to_NAD83 ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v

REM 2023_06_21_v39
las2las -i O:\PARSER_Ext\Canoe_L1\2023_06_21_v39\lidars\terra_las\cloud_merged.las ^
	-odir O:\PARSER_Ext\Canoe_L1\2023_06_21_v39\lidars\terra_las\projected_to_NAD83 ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v


































REM did the above and CC still froze, so voxelling all clouds to 1cm
REM By adding '-compute_mean_xyz' to the command line lasvoxel uses the average x, y, z coordinate of all points falling into a voxel instead of the center of the voxel.
REM RA DAP

lastile -i O:\PARSER_Ext\Rainbow_L1\2022_04_22_v39\lidars\terra_las\projected_to_NAD83\PlotA\cloud_merged_nad83_droppedPtsAbove160m_plotA.laz ^
	-tile_size 20 ^
	-buffer 4 ^
	-flag_as_withheld ^
	-odir O:\PARSER_Ext\Rainbow_L1\2022_04_22_v39\lidars\terra_las\projected_to_NAD83\PlotA\tiled_20m ^
	-olaz ^
	-cores %cores%
	
lasvoxel -v ^
    -i L:\Cw_PR_Rainbow_SiteA\Flights\2022_06_07\2_Inputs\metashape\1_TILES\RGB_DenseCloud\Clipped\RGB_DenseCloud_PlotA.laz ^
    -step 0.01 ^
	-compute_mean_xyz ^ 
    -odix _1cmVox ^
	-olaz ^
	-cores 4

:: RA 2022_04_22_v39
lasvoxel -v ^
    -i O:\PARSER_Ext\Rainbow_L1\2022_04_22_v39\lidars\terra_las\projected_to_NAD83\PlotA\cloud_merged_nad83_droppedPtsAbove160m_plotA.laz ^
    -step 0.01 ^
	-compute_mean_xyz ^ 
    -odix _1cmVox ^
	-olaz ^
	-cores 4
	
:: RA 2022_11_20_v393
lasvoxel -v ^
    -i O:\PARSER_Ext\Rainbow_L1\2022_11_20_v393\lidars\terra_las\projected_to_NAD83\PlotA\cloud_merged_nad83_droppedPtsAbove160m_plotA.laz ^
    -step 0.01 ^
	-compute_mean_xyz ^ 
    -odix _1cmVox ^
	-olaz ^
	-cores 4
	
:: RA 2023_06_22_v39
lasvoxel -v ^
    -i O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\PlotA\cloud_merged_nad83_droppedPtsAbove160m_plotA.laz ^
    -step 0.01 ^
	-compute_mean_xyz ^ 
    -odix _1cmVox ^
	-olaz ^
	-cores 4
	
REM RB DAP
lasvoxel -v ^
    -i L:\Cw_PR_Rainbow_SiteB\Flights\2022_06_07\2_Inputs\metashape\1_TILES\DenseCloud_filtered\Clipped\DenseCloud_NoNormals_NAD83_offset.laz ^
    -step 0.01 ^
	-compute_mean_xyz ^ 
    -odix _1cmVox ^
	-olaz ^
	-cores 4

:: RB 2022_04_22_v39
lasvoxel -v ^
    -i O:\PARSER_Ext\Rainbow_L1\2022_04_22_v39\lidars\terra_las\projected_to_NAD83\PlotB\cloud_merged_nad83_droppedPtsAbove160m_plotB.laz ^
    -step 0.01 ^
	-compute_mean_xyz ^ 
    -odix _1cmVox ^
	-olaz ^
	-cores 4
	
:: RB 2022_11_20_v393
lasvoxel -v ^
    -i O:\PARSER_Ext\Rainbow_L1\2022_11_20_v393\lidars\terra_las\projected_to_NAD83\PlotB\cloud_merged_nad83_droppedPtsAbove160m_plotB.laz ^
    -step 0.01 ^
	-compute_mean_xyz ^ 
    -odix _1cmVox ^
	-olaz ^
	-cores 4
	
:: RB 2023_06_22_v39
lasvoxel -v ^
    -i O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\PlotB\cloud_merged_nad83_droppedPtsAbove160m_plotB.laz ^
	-odir O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\PlotB\ ^
    -step 0.01 ^
	-compute_mean_xyz ^ 
    -odix _1cmVox ^
	-olaz ^
	-cores 4














lastile -i O:\PARSER_Ext\Rainbow_L1\2022_04_22_v39\lidars\terra_las\projected_to_NAD83\PlotA\cloud_merged_nad83_droppedPtsAbove160m_plotA.laz ^
	-tile_size 20 ^
	-buffer 4 ^
	-flag_as_withheld ^
	-odir O:\PARSER_Ext\Rainbow_L1\2022_04_22_v39\lidars\terra_las\projected_to_NAD83\PlotA\tiled_20m ^
	-olaz ^
	-cores %cores%
	
las2las -i E:\Fdc_PR_Canoe\Flights\2022_06_08\2_Inputs\metashape\1_TILES\RGB_DenseCloud\Clipped\Offset\Tiled\*.laz ^
	-odir E:\Fdc_PR_Canoe\Flights\2022_06_08\2_Inputs\metashape\1_TILES\RGB_DenseCloud\Clipped\Offset\Tiled\las2las_tiles\ 












REM OLLLLLLLLD BELOW

REM clipping to Rainbow B DTM area to create CHMs from DAP with DTM from L1
set polygon=D:\Cw_PR_Rainbow\Flights\2022_04_22\2_Inputs\lidar\LAStools\PlotB\DTM_outline.shp
lasclip -i D:\Cw_PR_Rainbow_SiteB\Flights\2022_06_07\2_Inputs\metashape\1_TILES\DenseCloud_filtered\DenseCloud_NoNormals_NAD83_offset.laz -merged -poly D:\Cw_PR_Rainbow\Flights\2022_04_22\2_Inputs\lidar\LAStools\PlotB\DTM_outline.shp -odir D:\Cw_PR_Rainbow_SiteB\Flights\2022_06_07\2_Inputs\metashape\1_TILES\DenseCloud_filtered\Clipped

REM retiling

set path=D:\Cw_PR_Rainbow_SiteB\Flights\2022_06_07\2_Inputs\metashape\1_TILES\DenseCloud_filtered\Clipped

set cores=4 

lasindex -i D:\Cw_PR_Rainbow_SiteB\Flights\2022_06_07\2_Inputs\metashape\1_TILES\DenseCloud_filtered\Clipped\*.las 
lastile -i D:\Cw_PR_Rainbow_SiteB\Flights\2022_06_07\2_Inputs\metashape\1_TILES\DenseCloud_filtered\Clipped\*.las -tile_size 20 -buffer -flag_as_withheld -odir D:\Cw_PR_Rainbow_SiteB\Flights\2022_06_07\2_Inputs\metashape\1_TILES\DenseCloud_filtered\Clipped\Tiled -olaz
	
REM put back into R as a CTG, normalize with DTM from L1, and create CHMs and delineated crowns to test with

pause