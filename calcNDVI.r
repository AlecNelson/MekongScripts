# calcNDVI
# calculate NDVI from surface reflectance bands
# 
# input: string of the directories of the two bands, RED and NIR respectively
# output: list of 2 RasterLayer objects

calcNDVI <- function(RED_dir, NIR_dir)
{
	require(raster)
	RED <- raster(RED_dir)
	NIR <- raster(NIR_dir)

	ndvi <- (NIR - RED) / (NIR + RED)
	return(ndvi)
}