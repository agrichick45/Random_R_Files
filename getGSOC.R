#' Download GLOSIS - GSOCmap
#'
#' This function downloads the GLOSIS - GSOCmap to a target folder from the project webpage.
#' It then can load the map as a raster object.
#'  
#' @param dir locatin to download the files to. Defaults to `dir = tempdir()`.
#' @param version character string setting the GLOSIS - GSOCmap version with options `"1.1.0"`,
#' `"1.2.0"` and `"1.5.0"` (default).
#' @param load logical indicating if the downloaded map should be loaded to the current R session.
#' @param timeout positive integer setting the timeout for the download operation, in seconds.
#' Defaults to `timeout = 1000` (seconds).
#' 
#' @details
#' \section{From the FAO soils portal}{
#' GSOCmap is the first global soil organic carbon map ever produced through a consultative and
#' participatory process involving member countries, which makes this map totally new and unique.
#' In fact, the map was prepared by member countries, under the guidance of the Intergovernmental
#' Technical Panel on Soils and the Global Soil Partnership Secretariat.
#' 
#' Countries agreed on the methodology to produce the map and were trained on modern tools and
#' methodologies to develop national maps. The Global Soil Partnership then gathered all national
#' maps to produce the final product, ensuring a thorough harmonization process.
#' 
#' GSOCmap data is provided under Creative commons CC BY licence.
#' 
#' The technical report is a companion report to the GSOCmap V1.2.0.
#' It presents methodologies and process of compiling the Global Soil organic Carbon Map.
#' }
#' 
#' Please refer to the GLOSIS - GSOCmap website
#' [http://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/global-soil-organic-carbon-map-gsocmap/en/]
#' for further information.
#' 
#' @author Alessandro Samuel-Rosa <alessandrosamuelrosa@@gmail.com>
#' 
#' @return A list contining the raster map with crs +proj=longlat +datum=WGS84 +no_defs and the
#' file name for the GLOSIS - GSOCmap manual.
#' 
#' @importFrom raster raster
#' @importFrom utils download.file
#' 
#' @export
#' @examples 
#' \dontest{
#' gsoc_map <- getGSOC(dir = "tmp/")
#' }
####################################################################################################
getGSOC <- 
  function(dir = NULL, version = "1.5.0", load = FALSE, timeout = 1000) {
    # check for valid download folder specification
    if (is.null(dir)) {
      dir <- tempdir()
    } else {
      if(!dir.exists(dir)){
        stop("Need target for download files.")
      }
    }
    # Check GLOSIS - GSOCmap version
    if(!version %in% c("1.1.0", "1.2.0", "1.5.0")) {
      stop(paste0("Unknown GLOSIS - GSOCmap version ", version))
    }
    version <- ifelse(version == "1.5.0", version, paste0("V", version))
    # Avoid "Downloads fail after 60 seconds"
    if (!is.null(timeout) & is.numeric(timeout)) {
      options(timeout = max(timeout, getOption("timeout")))
    }
    # Check for previous downloads so we don't repeat if we don't need to
    if(!file.exists(file.path(dir, paste0("GSOCmap", version, ".tif")))) {
      utils::download.file(
        url = paste0("http://54.229.242.119/GSOCmap/downloads/GSOCmap", version, ".tif"),
        destfile = file.path(dir, paste0("GSOCmap", version, ".tif")))
      utils::download.file(
        url = "http://www.fao.org/3/I8891EN/i8891en.pdf",
        destfile = file.path(dir, "GSOC_Techicanl_Report_and_Instructions.pdf"))
    }
    # Should the raster be loaded to the current R session?
    if (load) {
      return(list(
        raster = raster::raster(file.path(dir, paste0("GSOCmap", version, ".tif"))),
        manual_filename = file.path(dir, "GSOC_Techicanl_Report_and_Instructions.pdf")))
    }
  }
