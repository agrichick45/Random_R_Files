#' WorldClim average temperature
#'
#' This historical climate data covers 1970-2000 and is delivered at the 30 second resolution. 
#' You can go manually download the 30 sec average monthly temperature files from 
#' https://www.worldclim.org/data/worldclim21.html, specifically https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tavg.zip.
#' This is a large file and sometimes the download times out.
#' The GeoTiff file is a stack of annual temperatures that are averaged in this function and saved as a mean value.
#' The first time this runs might take longer then subsequent due to this averaging.
#' 
#' Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial resolution climate surfaces for global land areas. International Journal of Climatology 37 (12): 4302-4315.
#' 
#' @param dir local download directory
#' @param crs proscribed projection
#' @param load flag for loading
#' @param verbose print out debugging messages
#'
#' @return a list with the download filenames and the averaged annual values
#' @export
#' 
#' @importFrom raster calc stack writeRaster raster
#'
getWorldClimate <- function(dir = NULL,
             crs = "+proj=longlat +datum=WGS84 +no_defs", 
             load = FALSE,
             verbose = FALSE){

  #Fick, S.E. and R.J. Hijmans, 2017. WorldClim 2: new 1km spatial resolution climate surfaces for global land areas. International Journal of Climatology 37 (12): 4302-4315.
  #https://www.worldclim.org/data/worldclim21.html
  #https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tavg.zip
  
  # dir <- '~/Documents/Datasets/WorldClim'
  # check for valid download folder specification
  if (is.null(dir)) {
    dir <- tempdir()
  } else {
    if(!dir.exists(dir)){
      stop('Need target for download files.')
    }
  }
  
    #this is part of the raster library but it's unclear how to get the 30s resolution
  #temp <- getData('worldclim', var = 'tmin', res = 0.5, lon = 5, lat = 45)
  
  download.df <- data.frame(
    data_url = c('https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_30s_tavg.zip'),
    files = file.path(dir, c('wc2.1_30s_tavg.zip')),
    unzip_dir = file.path(dir, c('wc2.1_30s_tavg'))
  )
  
  if(verbose) print('Downloading data and unzipping... first time is a few minute runtime...')
  for(ii in 1:nrow(download.df)){
    if(!file.exists(download.df$files[ii])){
      download.file(url = download.df$data_url[ii],
                    destfile = download.df$files[ii], method = 'libcurl')
    }
    if(!file.exists(download.df$unzip_dir[ii])){
      unzip(download.df$files[ii], exdir = download.df$unzip_dir)
    }
  }
  if(verbose) print('...done!')
  
  if (load) {
    if(!file.exists(file.path(dir, c('wc2.1_30s_tavg_annual.tif')))){
      list.files(download.df$unzip_dir)
      if(verbose) print('Calculating stack mean, this takes a few minute runtime...')
      
     
      monthlyMeans <- raster::stack(list.files(download.df$unzip_dir, full.names = TRUE))
      
      annualMeans <- writeStart(raster::raster(list.files(download.df$unzip_dir, full.names = TRUE)[1]),
                                filename = file.path(dir, c('wc2.1_30s_tavg_annual.tif')))
      
      #blockSize(monthlyMeans)
      
      chunks <- blockSize(monthlyMeans)
      
      for(ii in seq_along(chunks$row)){
        if(ii %% 10 == 1){
          print(paste(ii, 'of',chunks$n, ':', Sys.time()))
        }
        temp <- getValues(monthlyMeans, row = chunks$row[ii], nrows = chunks$nrows)
        temp <- rowMeans(temp)
        annualMeans <- writeValues(annualMeans, temp, chunks$row[ii])
      }
      
      annualMeans <- writeStop(annualMeans)
      #annualMean <- raster::calc(monthlyMeans, fun = mean)
      
      temp <- raster::raster(file.path(dir, c('wc2.1_30s_tavg_annual.tif')))
      
      #writeRaster(x = annualMean, 
      #            filename = file.path(dir, c('wc2.1_30s_tavg_annual.tif')), 
      #            driver = "GeoTiff")
      
      if(verbose) print('...done!')
    }
    
    temp2 <- raster::raster(file.path(dir, c('wc2.1_30s_tavg_annual.tif')))
    
  }
  
  return(list(download.df, raster=temp2))
  
}