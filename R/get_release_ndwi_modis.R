#Code for extracting NDWI data from google earth engine

#' @author Brian Maitner
#' @description This function will download the most recent MODIS NDWI layer 
#' @import rgee piggyback sf
#' @param directory directory to save data in. Defaults to "data/temp/raw_data/NDWI_MODIS/"
#' @param domain domain (spatialpolygons* object) used for masking
get_release_ndwi_modis <- function(temp_directory = "data/temp/raw_data/NDWI_MODIS/",
                                          tag = "current"){
  
  # Make a directory if one doesn't exist yet
  
    if(!dir.exists(temp_directory)){
      dir.create(temp_directory,recursive = TRUE)
    }
  
  
  #Initialize rgee (if not done within the function won't work with targets for some reason)
    ee_Initialize()
  
  # Load the image
    ndwi <- ee$ImageCollection("MODIS/MCD43A4_006_NDWI")
  
  #Get the domain
  
    #env_files <- pb_list("AdamWilsonLab/emma_envdata")
  
    pb_download(file = "domain.gpkg",
                dest = temp_directory,
                repo = "AdamWilsonLab/emma_envdata",
                tag = "raw_static")
    
    domain <- st_read(dsn = file.path(temp_directory,"domain.gpkg"))
  
  
  #Format the domain
    domain <- sf_as_ee(x = domain)
    domain <- domain$geometry()
    
    
  #Examing md
      
    #ee_print(ndwi)
    #cut down to the most recent year to work with it
    info <-     
      ndwi$
      filterDate(start = as.character(Sys.Date()-365),
                      opt_end = as.character(Sys.Date()))$
      select("NDWI")$
      getInfo()
    
    
  #Cut down to the most recent layer
    
    most_recent <- 
    lapply(X = info$features,FUN = function(x){x$properties$`system:index`})%>%
      unlist()
  
    most_recent <- most_recent[length(most_recent)]    
    
    most_recent_ee  <- ndwi$filter(ee$Filter$eq("system:index",most_recent))$first()    
    
    # Map$addLayer(most_recent_ee) #for checking

  #Download the raster
    ee_as_raster(image = most_recent_ee,
                 region = domain,
                 #scale = 100, #used to adjust the scale. commenting out uses the default
                 dsn = file.path(temp_directory, "ndwi.tif"),
                 maxPixels = 10000000000)
    
  # Load raster
    
    ndwi_raster <- terra::rast(file.path(temp_directory, "ndwi.tif"))

  # Release file

    pb_upload(repo = "AdamWilsonLab/emma_report",
              file = file.path(temp_directory,"ndwi.tif"),
              tag = tag,
              name = "nasadem.tif")
  
  #Remove file
    unlink(temp_directory,recursive = TRUE,force = TRUE)
  
  # End  
    message("NDWI download finished")
    return(Sys.Date())
  
  
  
}#end fx



