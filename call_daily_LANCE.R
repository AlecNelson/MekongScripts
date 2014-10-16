# call_daily_LANCE.r
# 
# Tie everything together for creating MODIS 8-day product.
# DEPENDENCIES: RCurl, raster, XML

getwd()

MODIS_frequency <- "1day"
# *************************************
# 		   Manually enter Dates?	  #
# *************************************
# Set NULL for grabbing latest dates.
# Otherwise, enter a date in YYYY.MM.DD format (a string) and it will grab that from the server.
manualDate <- NULL
# -------------------------------------

if(MODIS_frequency != "1day"){
	stop("Please check MODIS_frequency variable. It should be '1day'.")
}

library(raster)
library(XML)
library(RCurl)
library(raster)
library(gdalUtils)

source("createCloudMask.r")
source("grabMODIS.r")
source("grabMODISdates.r")
source("calcNDVI.r")
source("compositeScenes.r")
source("ndviChange.r")
source("grabMODISdates_LANCE.R")
source("grabMODIS_LANCE.r")

# Get dry scene for baseline, get NDVI change.
# Assume same extent and projection as the freshly downloaded modis tiles
# NAs exist to mask out perennial water and high elevation. Will turn them into 0's later
dryScene <- raster("dryBaseline.tif") 

if( is.null(manualDate) ){
	# Get new dates and download
	latestDates<-grabMODISdates_LANCE(latest=TRUE, freq=MODIS_frequency)
	# --------------------------------------------------------------------
	# read last line in log file, if the same as current latestDates$terra 
	# then there is no new data
	lastLineLog <- tail(readLines("log.txt", warn=FALSE), n=1)
	lastLineLog_date <- substr(lastLineLog, 1, 10)

  if(lastLineLog_date == latestDates$terra){
		newData <- FALSE
	}else if(lastLineLog_date != latestDates$terra){
		newData <- TRUE
	}
	# --------------------------------------------------------------------
  
	if(newData){
		# to Date class for comparisons
		latestDates.dateClass <- lapply(latestDates, function(x) as.Date(x, format="%Y.%m.%d"))
		bothDatesMatch <- latestDates.dateClass$terra == latestDates.dateClass$aqua
    
		if( bothDatesMatch ){
			# grab both
		  grabMODIS_LANCE(c(latestDates$terra, latestDates$aqua), freq=MODIS_frequency)
      
		}else{
			# take later date
			if(latestDates.dateClass$terra > latestDates.dateClass$aqua){
				laterDate <- latestDates$terra
				sat       <- names(latestDates.dateClass)[1] 
			}else if(latestDates.dateClass$aqua > latestDates.dateClass$terra){
				laterDate <- latestDates$aqua
				sat       <- names(latestDates.dateClass)[2]
			}
			grabMODIS_LANCE(laterDate, freq=MODIS_frequency, sat=sat)
		}
    
		# Calculate ndvi and composite for flood scene
		# 	Make 'YYYY.MM.DD' into 'YYYY-MM-DD'
		# 	for terra
		terraDate <- sub(pattern=".", replacement="-", latestDates$terra, fixed=TRUE)
		terraDate <- sub(pattern=".", replacement="-", terraDate, fixed=TRUE)
		# 		for aqua
		aquaDate <- sub(pattern=".", replacement="-", latestDates$aqua, fixed=TRUE)
		aquaDate <- sub(pattern=".", replacement="-", aquaDate, fixed=TRUE)
	}

}else if( !is.null(manualDate) ){
	# Use manually entered dates

  grabMODIS_LANCE(manualDate, freq=MODIS_frequency)
  
	terraDate <- sub(pattern=".", replacement="-", manualDate, fixed=TRUE)
	terraDate <- sub(pattern=".", replacement="-", terraDate, fixed=TRUE)
	aquaDate  <- sub(pattern=".", replacement="-", manualDate, fixed=TRUE)
	aquaDate  <- sub(pattern=".", replacement="-", aquaDate, fixed=TRUE)
	
	#defaults to TRUE, this variable only relevant when grabbing latest data
	newData <- TRUE
}else{
	stop("Check main.r date vaiables.")
}


if(newData){

	if( bothDatesMatch || !is.null(manualDate) ){
		# Composite
		# Make dir names, dates for 8day often the same, but dates 
		# gathered separately for robustness.
		terraRED_dir <- paste0("misc/", "MOD09GQ_", terraDate, ".sur_refl_b01_1.tif")
		terraNIR_dir <- paste0("misc/", "MOD09GQ_", terraDate, ".sur_refl_b02_1.tif")

		aquaRED_dir  <- paste0("misc/", "MYD09GQ_", aquaDate,  ".sur_refl_b01_1.tif")
		aquaNIR_dir  <- paste0("misc/", "MYD09GQ_", aquaDate,  ".sur_refl_b02_1.tif")

		terra_ndvi <- calcNDVI(terraRED_dir, terraNIR_dir)
		aqua_ndvi  <- calcNDVI(aquaRED_dir, aquaNIR_dir)
		floodScene <- compositeScenes(aqua_ndvi, terra_ndvi, method="max")
		product    <- suppressWarnings(ndviChange(floodScene, dryScene))
		cloudMask.list <- createCloudMask(latestDates$terra)
	}else{
		# No composite, Only one date for NDVI exist, so use later date
		if(sat=="terra"){
			terraRED_dir <- paste0("misc/", "MOD09GQ_", terraDate, ".sur_refl_b01_1.tif")
			terraNIR_dir <- paste0("misc/", "MOD09GQ_", terraDate, ".sur_refl_b02_1.tif")
			floodScene   <- calcNDVI(terraRED_dir, terraNIR_dir)
			product      <- suppressWarnings(ndviChange(floodScene, dryScene))
		}else if(sat=="aqua"){
			aquaRED_dir  <- paste0("misc/", "MYD09GQ_", aquaDate,  ".sur_refl_b01_1.tif")
			aquaNIR_dir  <- paste0("misc/", "MYD09GQ_", aquaDate,  ".sur_refl_b02_1.tif")
			floodScene   <- calcNDVI(aquaRED_dir, aquaNIR_dir)
			product      <- suppressWarnings(ndviChange(floodScene, dryScene))		
		}
		cloudMask.list    <- createCloudMask(laterDate)
	}
	# 1. Remove NA within Baseline, corresponding DEM and perennial water
	product[is.na(product)] <- 0
	# for making tiles load faster, convert pixels more lightweight. 
	# anything over 100 will be turned to 100. anything below 0 will be turned to 0
	product[product > 0] <- 0
	product[product < -100] <- -100
	product <- product * -1 # turn negatives to positives -- to correspond to "% decrease"

  #DEM-based mask of potential flood height
	demMask <- raster("demmask/FinalProductMask.tif")
	product   <- suppressWarnings(demMask * product)
  
	# 2. Remove NA corresponding to the clouds within cloudMask
	if(cloudMask.list$available){
# 		origin.product <- origin(product) # needed for the workaround for next line
# 		origin(cloudMask.list$cloudMask) <- origin.product
# 		origin(raster.test)[1] <- origin.product[1]
    
		product   <- suppressWarnings(cloudMask.list$cloudMask * product) # workaround hack for extent/origin issue
		product[is.na(product)] <- 105 #105 is the cloudValue

		# mask out any clouds over water with perennial water mask
		waterMask <- raster("waterMask.tif")
# 	origin(waterMask) <- origin(product) # needed for the workaround for next line
		product   <- suppressWarnings(waterMask * product) # workaround hack for extent/origin issue
	}
  
  
	#savePath <- paste0(getwd(),"/product_TEST_10_15.tif")
  
	savePath <- "/home/ubuntu/geoserver-2.5.1/data_dir/data/DEVELOP/NDVIChangeProduct.tif"
	# savePath <- 
	writeRaster(product, savePath, format="GTiff", datatype="INT1U", overwrite=TRUE)
	# ----- Write to log -----
	if( is.null(manualDate) ){
		successLog <- paste(latestDates$terra, "success on", as.character(Sys.time()))
		cat(successLog, file="log.txt", append=TRUE)
	}
	# ------------------------
	cat("Done.")
	gc()
}else{
	# there is no new Data
	noNewData <- paste0("\n", latestDates$terra, " -- NO NEW DATA. Checked on ", as.character(Sys.time()))
	cat(noNewData, file="log.txt", append=TRUE)
	cat("No New Data.")
}


