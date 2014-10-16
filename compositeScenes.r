# compositeScenes.r
# 
# Intended for compositing both terra and aqua scenes by taking the 
# greater or lesser value'd pixel of both scenes
# input: rasters 
# output: a composited RasterLayer object

compositeScenes <- function(scene1, scene2, method)
{
	require(raster)

	scene1_mat <- as.matrix(scene1)
	scene2_mat <- as.matrix(scene2)

	if(method=="max"){
		# take the max values 
		# NAs are not taken if another value available.
		# if a pixel is NA in both scenes, leave it for now
		composite <- pmax(scene1_mat, scene2_mat, na.rm=TRUE)
	}else if(method=="min"){
		composite <- pmin(scene1_mat, scene2_mat, na.rm=TRUE)
	}

	# ready for output, use scene1's georef
	proj <- scene1@crs@projargs
	composite_raster <- raster(composite)
	projection(composite_raster) <- CRS(proj)
	ex_xmin <- scene1@extent@xmin
	ex_xmax <- scene1@extent@xmax
	ex_ymin <- scene1@extent@ymin
	ex_ymax <- scene1@extent@ymax
	extent(composite_raster) <- c(ex_xmin, ex_xmax, ex_ymin, ex_ymax)

	return(composite_raster)
}