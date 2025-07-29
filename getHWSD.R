#' Download and load HWSD
#'
#' This function prioritizes a HWSDa an amended version of the HWSDv1.2 provided by the European Soil Data Centre with reference R. The provided citation: Hiederer, M. Kochy 2012. Global Soil Organic Carbon Estimates and the Harmonized World Soil Database. EUR Scientific and Technical Research series ISSN 1831-9424 (online), ISSN 1018-5593 (print), ISBN 978-92-79-23108-7, doi:10.2788/1326
#' appears to be incorrect https://esdac.jrc.ec.europa.eu/content/global-soil-organic-carbon-estimates-and-harmonized-world-soil-database 
#' with the following citation: Hiederer, R. and M. Köchy (2011) Global Soil Organic Carbon Estimates and the Harmonized World Soil Database. EUR 25225 EN. Publications Office of the European Union. 79pp. https://esdac.jrc.ec.europa.eu/ESDB_Archive/eusoils_docs/other/EUR25225.pdf 
#' Unfortunately this updated amendment requires registration and has use-case restrictions. Please see the ESDAC website and fill out their data request form.
#' These files are in the https://gdal.org/drivers/raster/Idrisi.html format
#' 
#' If HWSDa datafiles are found then the function loads and processes these files.#' 
#' Otherwise this function downloads the HWSD v1.2 to a target folder from the project webpage, and unzips it.
#' It then loads the map of the mapping units as a raster object and the MS Access Database as a list of tables.
#' You will need to install mdbtools on your system to use Hmisc::mdb.get
#' Most correctly we should ratify the raster and append the data frame.
#' However, the raster is too large to efficently ratify, so we are returning this as two objects.
#' This function was taken from the following repository [https://github.com/ktoddbrown/GlobalRegrid_RHWSD] please refer here for further comment on the usage of the data base to construct soil carbon stock estimates.
#' 
#' Please refer to the HWSD website [http://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v12/en/] for further information.
#'
#' @param dir location to download the files to. Defaults to `dir = tempdir()`.
#' @param crs string defining the crs projection. Defaults to `crs = "+proj=longlat +datum=WGS84"`.
#' @param load Defaults to `load = FALSE`.
#'
#' @return a list contining the raster of the mapping units, a list of the data tables supporting this raster, and the file name for the HWSD manual
#' @export
#'
#' @importFrom Hmisc mdb.get
#' @importFrom raster raster crs
#' @importFrom utils download.file unzip
#'
#' @examples 
#' \dontest{
#' hwsd <- getHWSD(load = TRUE)
#'}
getHWSD <- function(dir = NULL, given_crs = "+proj=longlat +datum=WGS84", load = FALSE) {
  
  #dir <- '~/Documents/Datasets/HWSD'
  # check for valid download folder specification
  if (is.null(dir)) {
    dir <- tempdir()
  } else {
    if(!dir.exists(dir)){
      stop('Need target for download files.')
    }
  }
  
  if(file.exists(file.path(dir, 'HWSDa_OC_Dens_30SEC.zip'))){
    if(!file.exists(file.path(dir, 'HWSDa_OC_Dens_30SEC'))){
      unzip(file.path(dir, 'HWSDa_OC_Dens_30SEC.zip'), exdir = file.path(dir, 'HWSDa_OC_Dens_30SEC'))
    }
    
    temp <- list(
      sub_east_raster = raster::raster(file.path(dir, 'HWSDa_OC_Dens_30SEC/HWSDa_OC_Dens_SUB_30SEC_East.rst')),
      sub_west_raster = raster::raster(file.path(dir, 'HWSDa_OC_Dens_30SEC/HWSDa_OC_Dens_SUB_30SEC_West.rst')),
      top_east_raster = raster::raster(file.path(dir, 'HWSDa_OC_Dens_30SEC/HWSDa_OC_Dens_TOP_30SEC_East.rst')),
      top_west_raster = raster::raster(file.path(dir, 'HWSDa_OC_Dens_30SEC/HWSDa_OC_Dens_TOP_30SEC_West.rst')),
      units = c('t-C ha-1', '0.1 kg m-2'),
      directory = dir)
    return(temp)
  }
  
  # Check for previous downloads so we don't repeat if we don't need to
  if(!file.exists(file.path(dir, 'HWSD.zip')) |
     !file.exists(file.path(dir, 'HWSD_RASTER.zip'))){
    #downloads
    download.file(
      url = 'http://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/HWSD.zip', 
      destfile = file.path(dir, 'HWSD.zip')) #mdb
    download.file(
      url = 'http://www.fao.org/fileadmin/user_upload/soils/HWSD%20Viewer/HWSD_RASTER.zip', 
      destfile = file.path(dir, 'HWSD_RASTER.zip')) #Raster
    download.file(
      # url = 'http://www.fao.org/docrep/018/aq361e/aq361e.pdf', # this is version 1.1
      url = "https://esdac.jrc.ec.europa.eu/ESDB_Archive/Soil_Data/Docs_GlobalData/Harmonized_World_Soi_Database_v1.2.pdf",
      destfile = file.path(dir, 'HWSD_Techicanl_Report_and_Instructions.pdf'))
  }
  
  # Should the downloaded files be loaded into the current R session?
  if (load) {
    # unzip downloaded files with data in it
    unzip(file.path(dir, 'HWSD.zip'), exdir = dir)
    unzip(file.path(dir, 'HWSD_RASTER.zip'), exdir = file.path(dir, 'HWSD_RASTER'))
    
    # read in the raster
    HWSD_raster <- raster::raster(file.path(dir, 'HWSD_RASTER', 'hwsd.bil'))
    
    # manually set the projection
    raster::crs(HWSD_raster) <- given_crs
    
    # read in the MS Access database
    HWSD_mdb <- Hmisc::mdb.get(file.path(dir, 'HWSD.mdb'))
    
    # put them both together with a pointer to the pdf file
    return(list(
      raster = HWSD_raster, 
      db = HWSD_mdb,
      manual_filename = file.path(dir, 'HWSD_Techicanl_Report_and_Instructions.pdf')))
  }
}
