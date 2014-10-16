# Title:  ModisDownload 
# Version: 3.2, 13th Jan. 2014
# Author: Babak Naimi (naimi@r-gis.net)
# Major changes have been made on this version comparing to the 2.x. Since the FTP does not support anymore,
# the functions have been adjusted to support HTTP in this version!

# (Thanks Tomislav Hengl as his script made the main core of this function [spatial-analyst.net])
# Description: This is the source for two functions (ModisDownload and MRTproj) in R language that assist you to download, mosaic, subset and reproject the MODIS image products.
# Dependent library: RCurl
# Reference: http://www.r-gis.net

# MRTproj function is a wrapper to MRT tools, can be used to resample and subset an HDF image and convert to GeoTiff

library(raster)

modisProducts <- function() {
  #load(system.file("external/ModisLP.RData", package="rts"))
  load('ModisLP.RData')
  return(.ModisLPxxx)
  rm(.ModisLPxxx)
}

#-----------------------

.modisHTTP <- function(x,v='005',dates) {
  #print(".modisHTTP")
  
  if (!require(RCurl)) stop("Package RCurl is not installed")
  mp <- modisProducts()
  if (is.numeric(x)) {
    if (x > nrow(mp)) stop("The product code is out of subscription!")
    x <- as.character(mp[x,1])
 }
  x <- trim(x)
    year<-substr(dates[1], start=1, stop=4)
    doy<-format(strptime(dates[1], format="%Y.%m.%d"), format="%j")
print(c("year:",year))
print(c("doy:",doy))
    x <- paste("ftp://ahnelson:Md$20#14!@nrt1.modaps.eosdis.nasa.gov/allData/1/",x,"/",year,"/",doy,"/",sep="")
    if (!url.exists(x)) stop("the http address does not exist! Version may be incorrect OR Server is down!")
return(x)

}

#######################################################
# ADD ERROR MESSAGE FOR THE DATE NOT BEING AVAILABLE
#######################################################

# extract the selected dates
# if (length(dates) == 2) {
#   dirs <- dirs[which(d >= dates[1] & d <= dates[2])]
# } else dirs <- dirs[unlist(lapply(dates,function(x){which(d == x)}))]
# 
# if (length(dirs) < 1) stop("No available data for the selected dates")
# if (length(dates) > 2 & length(dirs) < length(dates)) warning("The images are not available for some specified dates!")
#######################################################

#-----------------
.getModisList <- function(x,h,v,dates) {
  
  #print(".getModisList")
  #if (inherits(dates,"character")) dates <- as.Date(dates,format='%Y.%m.%d')
  #dates <- na.omit(as.Date(dates))
  #print("dates:")
  #print(dates)
  #x<-"ftp://ahnelson:Md$20#14!@nrt1.modaps.eosdis.nasa.gov/allData/1/MOD09GQ/2014/277/"
  if (length(dates) == 0) stop("dates is not appropriately selected!")
#   dates <- sort(dates)
#   try.nr <- 3
#   items <- 0
#   class(items) <- "try-error"
#   ce <- 0
  
  #Unclear what this section does:
#   while(class(items) == "try-error") { 
    items <- try(strsplit(getURL(x), "\r*\n")[[1]],silent=TRUE)
#     if (class(items) == "try-error") {
#       Sys.sleep(5)
#       ce <- ce + 1
#       if (ce == (try.nr+1)) stop("Download error: Server does not response!")
#     }
#   }

#Unclear what this command does:
  items <- items[-c(1:19)]
  
items <- as.character(items)

for(i in 1:length(items)){
  tiles.i<-unlist(strsplit(as.character(items[i]), split = '[[:punct:]]'))[10]
  if(i==1){
    tiles<-tiles.i
  } else {
    tiles<-c(tiles,tiles.i)
  }
}

h.list<-suppressWarnings(tiles[which(as.numeric(substr(tiles,start=2,stop=3))==as.numeric(h))])
v.list<-suppressWarnings(tiles[which(as.numeric(substr(tiles,start=5,stop=6))==as.numeric(v))])
tile.list<-intersect(h.list, v.list)

  # creating a vector of available Modis images in selected tiles and dates
  Modislist <- items
      
  #Grab HDF files in list

tile.list <- as.character(tile.list)
#print(class(tile.list))
#print("tile.list:")
#print(tile.list)

for(k in 1:length(tile.list)){
  files.k<- grep(as.character(tile.list[k]), Modislist,value=TRUE,fixed=TRUE)
  ModisName.k <- grep(".hdf$",files.k,value=TRUE,ignore.case = TRUE)
  if(k==1){
    ModisName<-ModisName.k
  } else {
    ModisName<-c(ModisName,ModisName.k)
  }
}        

#Convert ModisName into Modis List with whatever the file manager needs to download the file

for(j in 1:length(ModisName)){
  pattern <- "[Mod][^f]*f"
  filename.j<-regmatches(ModisName[j], gregexpr(pattern, ModisName[1]))[[1]]
  filename.j<-paste0(x,filename.j)
  if(j==1){
    Modislist<-filename.j
  } else {
    Modislist<-c(Modislist,filename.j)
  }
}

Modislist<-list('1'=Modislist)

#print(class(Modislist))
#print("Modislist:")
#print(Modislist)

return(Modislist)
}


####################################################
####################################################
#--------

.downloadHTTP <- function(x,filename) {
  #print(".downloadHTTP")
  success <- FALSE
  er <- try( writeBin(getBinaryURL(x),con=filename),silent=TRUE)
  if (class(er) == "try-error") print("Download Error: Server does not response!!")
  else success <- TRUE
  return(success)
}

.getMODIS <- function(x, h, v, dates, version='005') {
  #print(".getMODIS")
  xx <- .modisHTTP(x,v=version,dates)
  Modislist <- .getModisList(xx,h=h,v=v,dates=dates)
  if (length(Modislist) == 0) stop("There is NO available images for the specified product!")
  out <- data.frame(matrix(nrow=0,ncol=2))
  names(out) <- c("Date","Name")
  dirs <- dates[1]
  #print(class(dirs))
  #print(c("dirs:",dirs))
  for (d in dirs) {
    cnt <- 1
    for (ModisName in Modislist[[1]]) {
      n <- strsplit(ModisName,"/")[[1]]
      n <- n[length(n)]
      if (.downloadHTTP(ModisName,n)) {
        out <- rbind(out,data.frame(Date=d,Name=n))
      }
      cnt <- cnt + 1
    }
    
  }
  #print(class(out))
  #print("out:")
  #print(out)
  out
}

#---------


#------------



if (!isGeneric("getMODIS")) {
  setGeneric("getMODIS", function(x,h,v,dates, ...)
    standardGeneric("getMODIS"))
}

if (!isGeneric("mosaicHDF")) {
  setGeneric("mosaicHDF", function(hdfNames,filename,MRTpath, ...)
    standardGeneric("mosaicHDF"))
}

if (!isGeneric("reprojectHDF")) {
  setGeneric("reprojectHDF", function(hdfName,filename,MRTpath, ...)
    standardGeneric("reprojectHDF"))
}

if (!isGeneric("ModisDownload")) {
  setGeneric("ModisDownload", function(x,h,v,dates, ...)
    standardGeneric("ModisDownload"))
}

setMethod("mosaicHDF", "character",
          function(hdfNames,filename,MRTpath,bands_subset,delete=FALSE) {
            if (missing(MRTpath)) stop("MRTpath argument should be specified...")
            if (length(hdfNames) < 2) stop("mosaic cannot be called for ONE image!")
            if (missing(bands_subset))  bands_subset <- ''
            if (missing(delete)) delete <- FALSE
            
            mosaicname = file(paste(MRTpath, "/TmpMosaic.prm", sep=""), open="wt")
            write(paste(getwd(),"/",hdfNames[1], sep=""), mosaicname)
            for (j in 2:length(hdfNames)) write(paste(getwd(),"/",hdfNames[j], sep=""),mosaicname,append=T)
            close(mosaicname)
            # generate mosaic:
            
            if (bands_subset != '') {
              e <- system(paste(MRTpath, '/mrtmosaic -i ', MRTpath, '/TmpMosaic.prm -s "',bands_subset,'" -o ',getwd(), '/',filename, sep=""))
              if (e != 0) warning ("Mosaic failed! 'bands_subset' may has incorrect structure!")
            } else {
              e <- system(paste(MRTpath, '/mrtmosaic -i ', MRTpath, '/TmpMosaic.prm -o ',getwd(), '/',filename, sep=""))
              if (e != 0) warning ("Mosaic failed!")
            }
            if (delete & e == 0) for (ModisName in hdfNames) unlink(paste(getwd(), '/', ModisName, sep=""))
            if (e == 0) return (TRUE)
            else return (FALSE)
          }
)


setMethod("reprojectHDF", "character",
          function(hdfName,filename,MRTpath,UL="",LR="",resample_type='NEAREST_NEIGHBOR',proj_type='UTM',
                   bands_subset='',proj_params='0 0 0 0 0 0 0 0 0 0 0 0',datum='WGS84',utm_zone=NA,pixel_size=1000) {
            
            fname = file('tmp.prm', open="wt")
            write(paste('INPUT_FILENAME = ', getwd(), '/',hdfName, sep=""), fname) 
            if (bands_subset != '') {
              write(paste('SPECTRAL_SUBSET = ( ',bands_subset,' )',sep=''),fname,append=TRUE)
            }
            if (UL[1] != '' & LR[1] != '') {
              write('SPATIAL_SUBSET_TYPE = OUTPUT_PROJ_COORDS', fname, append=TRUE)
              write(paste('SPATIAL_SUBSET_UL_CORNER = ( ', as.character(UL[1]),' ',as.character(UL[2]),' )',sep=''), fname, append=TRUE)
              write(paste('SPATIAL_SUBSET_LR_CORNER = ( ', as.character(LR[1]),' ',as.character(LR[2]),' )',sep=''), fname, append=TRUE)
            }
            write(paste('OUTPUT_FILENAME = ', filename, sep=""), fname, append=TRUE)
            write(paste('RESAMPLING_TYPE = ',resample_type,sep=''), fname, append=TRUE)
            write(paste('OUTPUT_PROJECTION_TYPE = ',proj_type,sep=''), fname, append=TRUE)
            write(paste('OUTPUT_PROJECTION_PARAMETERS = ( ',proj_params,' )',sep=''), fname, append=TRUE)
            write(paste('DATUM = ',datum,sep=''), fname, append=TRUE)
            if (proj_type == 'UTM') write(paste('UTM_ZONE = ',utm_zone,sep=''), fname, append=TRUE)
            write(paste('OUTPUT_PIXEL_SIZE = ',as.character(pixel_size),sep=''), fname, append=TRUE)
            close(fname)
            e <- system(paste(MRTpath, '/resample -p ',getwd(),'/','tmp.prm', sep=''))
            if (e == 0) return (TRUE)
            else return(FALSE)
          }
          
)


setMethod("getMODIS", "character",
          function(x,h,v,dates,version='005') {
            xx <- .modisHTTP(x,v=version)
            Modislist <- .getModisList(xx,h=h,v=v,dates=dates)
            if (length(Modislist) == 0) stop("There is NO available images for the specified product!")
            
            dirs <- names(Modislist)
            out <- data.frame(matrix(nrow=length(dirs),ncol=3))
            out[,1] <- dirs
            dc <- 1
            for (d in dirs) {
              dwnld <- rep(FALSE,length(Modislist[[d]]))
              cnt <- 1
              out[dc,2] <- length(dwnld)
              for (ModisName in Modislist[[1]]) {
                n <- strsplit(ModisName,"/")[[1]]
                n <- n[length(n)]
                if (.downloadHTTP(ModisName,n)) dwnld[cnt] <- TRUE
                cnt <- cnt + 1
              }
              out[dc,3] <- length(which(dwnld))
              dc <- dc+1
            }
            if (sum(out[,3]) > 0) {
              cat(paste('from ', sum(out[,2]),' available images, ',sum(out[,3]),' images are successfully downloaded.',sep=''))
            } else cat('Download is failed!')
            
          }
)


setMethod("getMODIS", "numeric",
          function(x,h,v,dates,version='005') {
            xx <- .modisHTTP(x,v=version)
            Modislist <- .getModisList(xx,h=h,v=v,dates=dates)
            if (length(Modislist) == 0) stop("There is NO available images for the specified product!")
            
            dirs <- names(Modislist)
            out <- data.frame(matrix(nrow=length(dirs),ncol=3))
            out[,1] <- dirs
            dc <- 1
            for (d in dirs) {
              dwnld <- rep(FALSE,length(Modislist[[d]]))
              cnt <- 1
              out[dc,2] <- length(dwnld)
              for (ModisName in Modislist[[1]]) {
                n <- strsplit(ModisName,"/")[[1]]
                n <- n[length(n)]
                if (.downloadHTTP(ModisName,n)) dwnld[cnt] <- TRUE
                cnt <- cnt + 1
              }
              out[dc,3] <- length(which(dwnld))
              dc <- dc+1
            }
            if (sum(out[,3]) > 0) {
              cat(paste('from ', sum(out[,2]),' available images, ',sum(out[,3]),' images are successfully downloaded.',sep=''))
            } else cat('Download is failed!')
            
          }
)


setMethod("ModisDownload", "character",
          function(x,h,v,dates,version='005',MRTpath,mosaic=FALSE,bands_subset='',delete=FALSE,proj=FALSE,UL="",LR="",resample_type='NEAREST_NEIGHBOR',proj_type='UTM', proj_params='0 0 0 0 0 0 0 0 0 0 0 0',datum='WGS84',utm_zone=NA,pixel_size) {
            dHDF <- .getMODIS(x,h,v,dates,version)
            dHDF$Date <- as.character(dHDF$Date)
            dHDF$Name <- as.character(dHDF$Name)
            if (nrow(dHDF) < 2) mosaic <- FALSE
            
            if (mosaic) {
              
              du <- unique(dHDF$Date)
              
              for (d in du) {
                dw <- dHDF[which(dHDF$Date == d),]
                if (nrow(dw) > 1){
                  date_name <- sub(sub(pattern="\\.", replacement="-", d), pattern="\\.", replacement="-", d)
                  name <- paste("Mosaic_",date_name,".hdf",sep='')
                  Mosaic.success <- mosaicHDF(dw[,2],name,MRTpath=MRTpath,bands_subset=bands_subset,delete=delete)
                  if (Mosaic.success) {
                    if (delete) for (ModisName in dw[,2]) unlink(paste(getwd(), '/', ModisName, sep=""))
                    if (proj) {
                      pref <- strsplit(dw[1,2],'\\.')[[1]][1]
                      e <- reprojectHDF(name,filename=paste('misc/',pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                      if (e & delete) unlink(paste(name))
                      if (!e) warning (paste("The procedure has failed to REPROJECT the mosaic image for date ",d,"!",sep=""))
                    }
                  } else {
                    warning(paste("The procedure has failed to MOSAIC the images for date ",d,"!",sep=""))
                    if (proj) {
                      warning ("Since the mosaic is failed, the individual hdf images are reprojected...")
                      pref <- strsplit(dw[1,2],'\\.')[[1]]
                      pref <- paste(pref[1],"_",pref[3],sep="")
                      for (ModisName in dw[,2]) {
                        e <- reprojectHDF(ModisName,filename=paste('misc/',pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,bands_subset=bands_subset,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                        if (e & delete) unlink(paste(ModisName))
                        if (!e) warning (paste("The procedure has failed to REPROJECT the individual HDF image ",ModisName,"!",sep=""))
                      }
                    }
                  } 
                }
              }   
            } else {
              if (proj) {
                for (i in 1:nrow(dHDF)) {
                  ModisName <- dHDF[i,2]
                  pref <- strsplit(ModisName,'\\.')[[1]]
                  pref <- paste(pref[1],"_",pref[3],sep="")
                  d <- dHDF[i,1]
                  date_name <- sub(sub(pattern="\\.", replacement="-", d), pattern="\\.", replacement="-", d)
                  e <- reprojectHDF(ModisName,filename=paste('misc/',pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,bands_subset=bands_subset,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                  if (e & delete) unlink(paste(ModisName))
                  if (!e) warning (paste("The procedure has failed to REPROJECT the individual HDF image ",ModisName,"!",sep=""))
                }
              }
            }
            
          }
)


setMethod("ModisDownload", "numeric",
          function(x,h,v,dates,version='005',MRTpath,mosaic=FALSE,bands_subset='',delete=FALSE,proj=FALSE,UL="",LR="",resample_type='NEAREST_NEIGHBOR',proj_type='UTM', proj_params='0 0 0 0 0 0 0 0 0 0 0 0',datum='WGS84',utm_zone=NA,pixel_size) {
            dHDF <- .getMODIS(x,h,v,dates,version)
            dHDF$Date <- as.character(dHDF$Date)
            dHDF$Name <- as.character(dHDF$Name)
            if (nrow(dHDF) < 2) mosaic <- FALSE
            if (mosaic) {
              
              du <- unique(dHDF$Date)
              
              for (d in du) {
                dw <- dHDF[which(dHDF$Date == d),]
                if (nrow(dw) > 1){
                  date_name <- sub(sub(pattern="\\.", replacement="-", d), pattern="\\.", replacement="-", d)
                  name <- paste("Mosaic_",date_name,".hdf",sep='')
                  Mosaic.success <- mosaicHDF(dw[,2],name,MRTpath=MRTpath,bands_subset=bands_subset,delete=delete)
                  if (Mosaic.success) {
                    if (delete) for (ModisName in dw[,2]) unlink(paste(getwd(), '/', ModisName, sep=""))
                    if (proj) {
                      pref <- strsplit(dw[1,2],'\\.')[[1]][1]
                      e <- reprojectHDF(name,filename=paste('misc/',pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                      if (e & delete) unlink(paste(name))
                      if (!e) warning (paste("The procedure has failed to REPROJECT the mosaic image for date ",d,"!",sep=""))
                    }
                  } else {
                    warning(paste("The procedure has failed to MOSAIC the images for date ",d,"!",sep=""))
                    if (proj) {
                      warning ("Since the mosaic is failed, the individual hdf images are reprojected...")
                      pref <- strsplit(dw[1,2],'\\.')[[1]]
                      pref <- paste(pref[1],"_",pref[3],sep="")
                      for (ModisName in dw[,2]) {
                        e <- reprojectHDF(ModisName,filename=paste('misc/',pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,bands_subset=bands_subset,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                        if (e & delete) unlink(paste(ModisName))
                        if (!e) warning (paste("The procedure has failed to REPROJECT the individual HDF image ",ModisName,"!",sep=""))
                      }
                    }
                  } 
                }
              }   
            } else {
              if (proj) {
                for (i in 1:nrow(dHDF)) {
                  ModisName <- dHDF[i,2]
                  pref <- strsplit(ModisName,'\\.')[[1]]
                  pref <- paste(pref[1],"_",pref[3],sep="")
                  d <- dHDF[i,1]
                  date_name <- sub(sub(pattern="\\.", replacement="-", d), pattern="\\.", replacement="-", d)
                  e <- reprojectHDF(ModisName,filename=paste('misc/',pref,'_',date_name,'.tif',sep=''),MRTpath=MRTpath,UL=UL,LR=LR,bands_subset=bands_subset,proj_type=proj_type,proj_params=proj_params,utm_zone=utm_zone,pixel_size=pixel_size)
                  if (e & delete) unlink(paste(ModisName))
                  if (!e) warning (paste("The procedure has failed to REPROJECT the individual HDF image ",ModisName,"!",sep=""))
                }
              }
            }
            
          }
)
