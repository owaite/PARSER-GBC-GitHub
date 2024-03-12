

REM The following code:
REM (1) reads in exported dense clouds from agisoft from the micasense, as well as the L1 points clouds and reprojects to nad 83 UTM 10N 
REM (2) drops L1 points above a threshold hold that represents the max height tree points would be (works to remove noise)
REM (3) Clips L1 clouds to site shapefiles to decrease the size of the point cloud for alignment in cloud compare
REM (4) Saves clipped DAP clouds as laz files to save storage space

REM explanations for path placeholders
REM path_to_DAP_laz_file -> path without "" and with \ between folders to the DAP laz files
REM output_path_to_store_the_projected_DAP_laz_file -> path to location to write the projected DAP laz

REM General workflow 
las2las -i path_to_DAP_laz_file ^
	-odir output_path_to_store_the_projected_DAP_laz_file ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v
REM projecting L1 Lidar to NAD 83
REM path_to_L1_las -> path to the L1 .las file
REM path_to_write_projected_L1_to -> where the projected L1 will be written to 
las2las -i path_to_L1_las ^
	-odir path_to_write_projected_L1_to ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v
REM dropping points above 160m (noise points, 160m was the value for this dataset)
REM suggestion: plot a histogram of Z values to see where the upper noise is to set this threshold should be, you might also not need to do this step if you dont have upper or lower noise
las2las -i O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\cloud_merged_nad83.laz ^
	-odir O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83 ^
	-odix _droppedPtsAbove160m ^
	-olaz ^
	-drop_z_above 160 ^
	-cpu64 ^
	-v

pause
REM clipping RA lidar to shp, used larger RA shp for 2022_11_20
REM this step is done to decrease the storage size so that loading into cloud compare goes smoothly and the alignment process does not crash
set RA_poly = O:\PARSER_Ext\Rainbow_L1\PlotA\Rainbow_A_shp_nad83\Larger_shp\Rainbow_PlotA_large.shp
lasclip -i O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\cloud_merged_nad83_droppedPtsAbove160m.laz -merged -poly O:\PARSER_Ext\Rainbow_L1\PlotA\Rainbow_A_shp_nad83\Larger_shp\Rainbow_PlotA_large.shp -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\PlotA\ -odix _plotA -olaz
	

REM clipping RB lidar to shp
REM DO PLOT B FOR 2022_11_20_v393
set RB_poly = O:\PARSER_Ext\Rainbow_L1\PlotB\Rainbow_B_shp_nad83\Rainbow_B_Project.shp
lasclip -i O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\cloud_merged_nad83_droppedPtsAbove160m.laz -merged -poly O:\PARSER_Ext\Rainbow_L1\PlotB\Rainbow_B_shp_nad83\Rainbow_B_Project.shp -odir O:\PARSER_Ext\Rainbow_L1\2023_06_22_v39\lidars\terra_las\projected_to_NAD83\PlotB\ -odix _plotB -olaz


REM reading in clipped DAP cloud and saving from a las to a laz for CC
REM site: RA
las2las -i L:\Cw_PR_Rainbow_SiteA\Flights\2022_06_07\2_Inputs\metashape\1_TILES\RGB_DenseCloud\Clipped\RGB_DenseCloud_PlotA.las ^
	-odir L:\Cw_PR_Rainbow_SiteA\Flights\2022_06_07\2_Inputs\metashape\1_TILES\RGB_DenseCloud\Clipped\ ^
	-olaz ^
	-cpu64 ^
	-v

REM site: RB
las2las -i L:\Cw_PR_Rainbow_SiteB\Flights\2022_06_07\2_Inputs\metashape\1_TILES\DenseCloud_filtered\Clipped\DenseCloud_NoNormals_NAD83_offset.las ^
	-odir L:\Cw_PR_Rainbow_SiteB\Flights\2022_06_07\2_Inputs\metashape\1_TILES\DenseCloud_filtered\Clipped\ ^
	-olaz ^
	-cpu64 ^
	-v


REM site: CANOE -----------------------------------------------------------------------------------------------
REM projecting Lidar to NAD 83 for 2022_04_23_v39
las2las -i O:\PARSER_Ext\Canoe_L1\2022_04_23_v39\lidars\terra_las\cloud_merged.las ^
	-odir O:\PARSER_Ext\Canoe_L1\2022_04_23_v39\lidars\terra_las\projected_to_NAD83 ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v

REM projecting Lidar to NAD 83 for 2022_11_28_v39
las2las -i O:\PARSER_Ext\Canoe_L1\2022_11_28_v39\lidars\terra_las\cloud_merged.las ^
	-odir O:\PARSER_Ext\Canoe_L1\2022_11_28_v39\lidars\terra_las\projected_to_NAD83 ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v

REM projecting Lidar to NAD 83 for 2023_06_21_v39
las2las -i O:\PARSER_Ext\Canoe_L1\2023_06_21_v39\lidars\terra_las\cloud_merged.las ^
	-odir O:\PARSER_Ext\Canoe_L1\2023_06_21_v39\lidars\terra_las\projected_to_NAD83 ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v
