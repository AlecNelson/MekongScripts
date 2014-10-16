# DEALING WITH CLOUDS!
# Last minute game plan to incorporate cloud mask into the process.
#
# OUTPUT: An R list containing
#		1. Logical value of whever it matches dateNDVI
#
#		2. Rasterlayer of the binary cloud mask, resampled to 250m, 
# 		   if available
#
# This function:
# 1. Downloads MYD09A1 and extracts the blue (third) band. It is at 500m. 
# 	 Reproject to WGS84 with mrt
# 2. Resample to 250m
# 3. Set threshold value and creaete 

createCloudMask <- function(dateNDVI){
	source("grabMODISdates.r")
	source("grabMODIS.r")
	threshold <- 1350

	latestDatesCLOUD <- grabMODISdates(latest=TRUE, freq="8day", cloudMask=TRUE)
	# Make 'YYYY.MM.DD' into 'YYYY-MM-DD'
	# for terra
	terraDate <- sub(pattern=".", replacement="-", latestDates$terra, fixed=TRUE)
	terraDate <- sub(pattern=".", replacement="-", terraDate, fixed=TRUE)
	# for aqua
	aquaDate <- sub(pattern=".", replacement="-", latestDates$aqua, fixed=TRUE)
	aquaDate <- sub(pattern=".", replacement="-", aquaDate, fixed=TRUE)
	
	allPairDatesMatch <- all(latestDatesCLOUD$aqua == dateNDVI,latestDatesCLOUD$terra == dateNDVI)
	
  if(allPairDatesMatch){
		# Composite both terra and aqua to create cloud mask
		grabMODIS(c(latestDates$terra, latestDates$aqua), freq="8day", cloudMask=TRUE)

		# 	Make dir names, dates for 8day often the same, but dates 
		# 	gathered separately for robustness.
		terraBLUE_dir <- paste0("misc/", "MOD09A1_", terraDate, ".sur_refl_b03.tif")
		aquaBLUE_dir  <- paste0("misc/", "MYD09A1_", aquaDate, ".sur_refl_b03.tif")
		terraBLUE <- raster(terraBLUE_dir)
		aquaBLUE  <- raster(aquaBLUE_dir)

		BLUE <- compositeScenes(aquaBLUE, terraBLUE, method="min")
		available <- TRUE

	}else if(!allPairDatesMatch && any(latestDatesCLOUD == dateNDVI)){
		# Does at least one match with dateNDVI? Get whichever one.
		oneMatchDate <- as.character(latestDatesCLOUD[which(latestDatesCLOUD == dateNDVI)])
		oneMatchsat <-latestDatesCLOUD[which(latestDatesCLOUD == dateNDVI)]
		sat <- names(oneMatchsat)
		grabMODIS(oneMatchDate, freq="8day", sat=sat, cloudMask=TRUE)

		# 	Make dir names, dates for 8day often the same, but dates 
		# 	gathered separately for robustness.
		if(sat == "terra"){
			terraBLUE_dir <- paste0("misc/", "MOD09A1_", terraDate, ".sur_refl_b03.tif")
			BLUE  <- raster(terraBLUE_dir)
		}else if(sat == "aqua"){
			aquaBLUE_dir  <- paste0("misc/", "MYD09A1_", aquaDate, ".sur_refl_b03.tif")
			BLUE  <- raster(aquaBLUE_dir)
		}
		available <- TRUE
	}else{
		# No cloud Mask
		available <- FALSE
		return( list(available=available, cloudMask=NULL) )
	}

	BLUE[BLUE < threshold]  <- 1
	BLUE[BLUE >= threshold] <- NA
	return( list(available=available, cloudMask=BLUE) )
}



