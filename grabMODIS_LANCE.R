# grabMODIS.r
# 
# simply download MODIS of terra and aqua off server
# using the ModisDownload tool
# ------------------------------------------

grabMODIS_LANCE <- function(Dates, freq="1day", sat=NULL, cloudMask=FALSE )
{
	MRTpath <- "/Users/ahnelson/Downloads/MRT_download_Win/MRT/bin"
	#MRTpath <- "/home/ubuntu/mrt/bin"
	source("ModisDownload_LANCE.R")
	# First, check frequency, dates, and sat
	if(freq=="1day"){
		if(cloudMask){
			terra_13bandSR <- "MOD09A1"
			aqua_13bandSR  <- "MYD09A1"
		}else{
		  terra_product <- "MOD09GQ"
		  aqua_product  <- "MYD09GQ"
		}
	}else{
		stop("Incorrect argument")
	}
	numDates <- length(Dates)
	if(numDates > 2) stop("No more than 2 dates. If two, input Terra date first.")
	if(!is.null(sat)){
		if( sat != "terra" && sat != "aqua" ) stop("sat argument either 'terra' or 'aqua'.")
	}
	
  UL_dryScene<-c(xmin(dryScene),ymax(dryScene))
	LR_dryScene<-c(xmax(dryScene),ymin(dryScene))
  
  
  #Ensure the date format of YYYY.MM.DD
  
  #--------------------------------------------

	if(numDates == 1 && sat=="terra" && !cloudMask){

		ModisDownload(x = terra_product,
		        h = c(27, 28),
		        v = c(7, 8),
					  dates = Dates[1],
					  MRTpath = MRTpath,
					  mosaic = TRUE,
					  bands_subset="0 1 1",
					  proj = TRUE,
					  proj_type = "GEO",
					  delete=TRUE,
					  datum="WGS84",
					  UL=UL_dryScene,
					  LR=LR_dryScene,
					  pixel_size=0.0021059669)

	}else if(numDates == 1 && sat=="aqua" && !cloudMask){

		ModisDownload(x = aqua_product,
		        h = c(27, 28),
		        v = c(7, 8),
					  dates = Dates[1],
					  MRTpath = MRTpath,
					  mosaic = TRUE,
					  bands_subset="0 1 1",
					  proj = TRUE,
					  proj_type = "GEO",
					  delete=TRUE,
					  datum="WGS84",
					  UL=UL_dryScene,
					  LR=LR_dryScene,
					  pixel_size=0.0021059669)

	}else if(numDates == 2 && freq=="1day" && !cloudMask ){

	#}else if(numDates == 2 && freq=="8day" && is.null(sat) && !cloudMask ){ 
    
		# Terra, first argument of dates
		ModisDownload(x = terra_product,
		        h = c(27, 28),
		        v = c(7, 8),
					  dates = Dates[1],
					  MRTpath = MRTpath,
					  mosaic = TRUE,
					  bands_subset="0 1 1",
					  proj = TRUE,
					  proj_type = "GEO",
					  delete=TRUE,
					  datum="WGS84",
					  UL=UL_dryScene,
					  LR=LR_dryScene,
					  pixel_size=0.0021059669)

		# Aqua, second argument of dates
		ModisDownload(x = aqua_product,
		        h = c(27, 28),
		        v = c(7, 8),
					  dates = Dates[2],
					  MRTpath = MRTpath,
					  mosaic = TRUE,
					  bands_subset="0 1 1",
					  proj = TRUE,
					  proj_type = "GEO",
					  delete=TRUE,
					  datum="WGS84",
					  UL=UL_dryScene,
					  LR=LR_dryScene,
					  pixel_size=0.0021059669)
		return()

	}else if(freq=="8day" && cloudMask ){
	  
	#}else if(numDates == 2 && freq=="8day" && is.null(sat) && cloudMask ){ 

		# get 3rd band (BLUE) from 09A1 product, resample to 250m
		ModisDownload(x = terra_13bandSR,
		        h = c(27, 28),
		        v = c(7, 8),
					  dates = Dates[1],
					  MRTpath = MRTpath,
					  mosaic = TRUE,
					  bands_subset="0 0 1 0 0 0 0 0 0 0 0 0 0",
					  proj = TRUE,
					  proj_type = "GEO",
					  delete=TRUE,
					  datum="WGS84",
					  UL=UL_dryScene,
					  LR=LR_dryScene,
					  pixel_size=0.0021059669)

		ModisDownload(x = aqua_13bandSR,
					  h = c(27, 28),
					  v = c(7, 8),
					  dates = Dates[1],
					  MRTpath = MRTpath,
					  mosaic = TRUE,
					  bands_subset="0 0 1 0 0 0 0 0 0 0 0 0 0",
					  proj = TRUE,
					  proj_type = "GEO",
					  delete=TRUE,
					  datum="WGS84",
					  UL=UL_dryScene,
					  LR=LR_dryScene,
					  pixel_size=0.0021059669)

	}
	else{
		stop("Invalid Arguments.")
	}
}

