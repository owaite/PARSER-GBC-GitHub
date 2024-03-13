

REM The following code:
REM (1) reads in exported dense clouds from agisoft from the micasense, as well as the L1 points clouds and reprojects to nad 83 UTM 10N 
REM (2) drops L1 points above a threshold hold that represents the max height tree points would be (works to remove noise)
REM (3) Clips L1 clouds to site shapefiles to decrease the size of the point cloud for alignment in cloud compare
REM (4) Saves clipped DAP clouds as laz files to save storage space

REM explanations for path placeholders
REM path_to_DAP_laz_file -> path without "" and with \ between folders to the DAP laz files
REM path_to_write -> path to location to write the projected DAP laz

REM pause below is functioning as a safety, in case this .bat file is double clicked by mistake and runs, the pause will allow the chance to cancel it before it overwrites data
pause

REM General workflow 
las2las -i path_to_DAP_las_file ^
	-odir path_to_write ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v
REM projecting L1 Lidar to NAD 83
REM path_to_L1_las -> path to the L1 .las file from DJI terra
REM path_to_write -> where the projected L1 will be written to 
las2las -i path_to_L1_las ^
	-odir path_to_write ^
	-odix _nad83 ^
	-olaz ^
	-nad83 ^
	-utm 10north ^
	-cpu64 ^
	-v
REM dropping points above 160m (noise points, 160m was the value for this dataset)
REM suggestion: plot a histogram of Z values (in R) to see where the upper noise is to set this threshold, you might also not need to do this step if you dont have upper or lower noise
REM : path_to_projected_L1 -> path to projected .laz file from above
las2las -i path_to_projected_L1 ^
	-odir path_to_write ^
	-odix _droppedPtsAbove160m ^
	-olaz ^
	-drop_z_above 160 ^
	-cpu64 ^
	-v

pause

REM clipping lidar to shp
REM this step is done to decrease the storage size so that loading into cloud compare goes smoothly and the alignment process does not crash

REM path_to_projected_below160m_L1 <- path to .laz file that has been clipped to the site shp and filtered for points above a set threshold
REM path_to_shp -> path to the site shapefile
REM "-odix _plotA" will save the new laz file with the same name as the input file with "_plotA" attached to the end, change the "_plotA" to work with your naming system
lasclip -i path_to_projected_below160m_L1 -merged -path_to_shp -odir path_to_write -odix _plotA -olaz
	
REM Next step: Align the L1 data to the DAP data in Cloud compare
REM Note, convention is to align the DAP to the L1 however for this specific project it made more since to align the lidar to the DAP because:
REM a) we had many more DAP datasets then L1
REM b) we could not load the lidar point cloud into agisoft metashape for alignment, and had been aligning all DAP clouds to a reference DAP cloud in agisoft 