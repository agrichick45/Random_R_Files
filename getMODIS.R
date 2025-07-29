#' Download and load MOD17A3 NPP product
#' 
#' Download and load the MOD17A3 NPP product from UMT archives.
#' File download here can be finicky, you may need to manually go to [http://files.ntsg.umt.edu/data/NTSG_Products/MOD17/GeoTIFF/MOD17A3/GeoTIFF_30arcsec](http://files.ntsg.umt.edu/data/NTSG_Products/MOD17/GeoTIFF/MOD17A3/GeoTIFF_30arcsec) and download the file.
#' Total file size is about 385 MB and the geotiff NPP file is optionally loaded as a raster object.
#' NPP raster object is 30 seconds resolution or 0.008333 degrees.
#' Additional ReadMe and download guides are also fetched here.
#'
#' @param dir download directory
#' @param crs optional projection, default matches GeoTIFF specifications
#' @param load logical flag to load the data as a raster object
#'
#' @return list containing 1) a data frame with the download urls, and local filenames as well as 2) the NPP raster object
#' @export
#' 
#' @importFrom raster raster crs NAvalue
#'
getMODIS <- function(dir = NULL,
                     given_crs = "+proj=longlat +datum=WGS84 +no_defs", 
                     load = FALSE){
  #dir <- '~/Documents/Datasets/MOD17_MODIS_250_NPP'
  
  # check for valid download folder specification
  if (is.null(dir)) {
    dir <- tempdir()
  } else {
    if(!dir.exists(dir)){
      stop('Need target for download files.')
    }
  }
  
  download.df <- data.frame(url_downloads = c('http://files.ntsg.umt.edu/data/NTSG_Products/MOD17/GeoTIFF/MOD17A3/GeoTIFF_30arcsec/MOD17A3_Science_NPP_mean_00_15.tif',
  'http://files.ntsg.umt.edu/data/NTSG_Products/MOD17/GeoTIFF/MOD17A3/readme.txt',
   'http://files.ntsg.umt.edu/readme.md', 
  'http://files.ntsg.umt.edu/NTSG-Data-Repository_Downloading-Data-Guide.pdf'),
  filenames = file.path(dir, c('MOD17A3_Science_NPP_mean_00_15.tif', 'MOD17A3_readme.txt',
                  'files_ntsg_umt_edu_readme.md', 'NTSG-Data-Repository_Downloading-Data-Guide.pdf')))
  
  for(ii in (1:nrow(download.df))){
    if(!file.exists(download.df$filenames[ii]))
      download.file(url = download.df$url_downloads[ii], 
                    destfile = download.df$filenames[ii])
  }
  
  if(load){
    #resolution: 0.008333333, 0.008333333  (x, y)
    temp <- raster::raster(download.df$filenames[1])
    raster::NAvalue(temp) <- 65535
    # manually reset the projection
    raster::crs(temp) <- given_crs
    return(list(npp = temp, downloads=download.df))
  }else{
    return(list(downlaods = download.df))
  }
}

