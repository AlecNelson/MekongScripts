# Michael Gao 
# NASA DEVELOP 2014 
# SE Asia Disasters Project
#
# DEPENDENCIES: XML
# grabMODISdates retrieves dates from respective Aqua/Terra
# products: MYD09Q1.005/MOD09Q1.005 respectively.
# if cloudMask is TRUE, then it will grab dates for the createCloudMask.r function 

# Arguments:
# 	1. latest=TRUE will return a list of the latest dates available
# 			  (Default) latest=FALSE, it will print out most recent 10 dates
#	2. freq="8day" or "16day"
#	3. cloudMask=FALSE default, if TRUE then it will grab the dates from the 
#	500m 8-day surface reflectance product 09A1, which will be used for cloud mask	
# ------------------------------------------------------

grabMODISdates_LANCE <- function(latest=FALSE, freq="1day", cloudMask=FALSE)
{
	require(XML)
	# 1. Get the most recent MODIS products for our region
	#	 Aqua and Terra
	#	 from the server http://e4ftl01.cr.usgs.gov/
	
  
	############################################################################  
	############################################################################
  
	if(freq=="1day")
	{
	  #	 8-day Surface Relflectance products version .005
	  if(cloudMask){
	    terra_prod <- "MOD09A.005/"
	    aqua_prod  <- "MYD09A.005/"
	  }else{
	    terra_prod <- "MOD09GQ"
	    aqua_prod  <- "MYD09GQ"
	  }	
	}else
	{
	  stop("Invalid freq argument. Enter '1day' for daily product.")
	}
  
  
	#Log-in and direct to correct FTP server
	server = "ftp://ahnelson:Md$20#14!@nrt1.modaps.eosdis.nasa.gov/allData/1/"
	year = format(Sys.time(), "%Y")
  
	#	 the URL
	terra_url <- paste0(server, terra_prod,"/",year,"/")
	aqua_url  <- paste0(server, aqua_prod,"/",year,"/")
  
############################################################################
#For Terra:
  #Aquire the filenames of the current available files
	filenames = getURL(terra_url, ftp.use.epsv = FALSE, dirlistonly = TRUE) 
	filenames = paste(server, strsplit(filenames, "\r*\n")[[1]], sep = "") 
	con = getCurlHandle( ftp.use.epsv = FALSE) 
	try(getURL(server, curl = con))
  
  
  #Substring the filenames to Date
for(i in 1:length(filenames)){ 
  listDates.i <- substring(filenames[i],nchar(server)+1,nchar(server)+3)
  listDates.i<-paste0(year,".",format(strptime(listDates.i, format="%j"), format="%m.%d"))
  if(i==1){listDates<-listDates.i} else{listDates<-c(listDates,listDates.i)} 
} 

latest_terra_date <- listDates[length(listDates)]

############################################################################
#For Aqua:
#Aquire the filenames of the current available files
filenames = getURL(aqua_url, ftp.use.epsv = FALSE, dirlistonly = TRUE) 
filenames = paste(server, strsplit(filenames, "\r*\n")[[1]], sep = "") 
con = getCurlHandle( ftp.use.epsv = FALSE) 
try(getURL(server, curl = con))


#Substring the filenames to Date
for(i in 1:length(filenames)){ 
  listDates.i <- substring(filenames[i],nchar(server)+1,nchar(server)+3)
  listDates.i<-paste0(year,".",format(strptime(listDates.i, format="%j"), format="%m.%d"))
  if(i==1){listDates<-listDates.i} else{listDates<-c(listDates,listDates.i)} 
} 

latest_aqua_date <- listDates[length(listDates)]

############################################################################

if(latest){

latestDates <- list(latest_terra_date, latest_aqua_date)
names(latestDates) <- c("terra", "aqua")
# gives a list of the latest dates
return(latestDates)

}else{
  listDates <- list(listDates)
  return(listDates)
}
}


############################################################################
############################################################################
#