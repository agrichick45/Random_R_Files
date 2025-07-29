#' Read in GLDAS data
#'
#' @param dir data directory where the file is stored
#'
#' @return a list of filenames and raster with moisture information
#' @export
#'
#' @examples
getGLDAS <- function(dir = NULL){
  
  #files must be downloaded from: https://drive.google.com/drive/folders/1jZjwY9ohQhhbb4A1ojqSZKk-4K3YhNTQ?usp=sharing
  #TODO need a better way to link this up to an archive.
  #dir <- "~/Dropbox (UFL)/Research/Datasets/GLDAS_soil_moisture/GLDAS_SM_statistics"
  #in units
  download.df <- data.frame(
    data_url = NA,
    files = NA,
    unzip_dir = file.path(dir, c('GLDAS_SM1mean.tif', 'GLDAS_SM1max.tif'))
  )
  
  temp <- raster::raster(file.path(dir, "GLDAS_SM1mean.tif"))
  
  #anything that is 100% water (ice) is removed
  raster::NAvalue(temp) <- 1
  
  return( list(files = download.df, moisture = temp ) )

}