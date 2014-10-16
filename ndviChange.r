# ndviChange.r
#
# Calculate Dr. Joe Spruce's ndvi change method
# input: rasterLayer object

# Note there's a warning about extent not matching (suppressed in the call_8day.r)
# but when attempting to correct that with extent(flood) <- extent(dry), it complains about origin
# so I suppressed the warning for now, but later on it should be good to look into this issue.
# perhaps trying alignExtent{raster}

ndviChange <- function(flood, dry)
{
	require(raster)
	#origin(flood) <- origin(dry)
	ndviChange_product <- ((flood - dry)/ dry) * 100
	return(ndviChange_product)
}

