library(terra)
#current_month <- month(Sys.Date())

#' This function calculates the mean ndvi for the current month from the NDVI releases
get_monthly_mean_ndvi <- function(temp_directory = "data/temp/",
                                  current_month = current_month){
  
  
  # Create directory if needed
  
    if(!dir.exists(temp_directory)){
      dir.create(temp_directory, recursive = TRUE)
    }
  
  # Download list of available files

  files <- pb_list(repo = "AdamWilsonLab/emma_envdata",
                   tag = "current")
  
  # Filter to NDVI records from the relevant month
  
    files %>%
      filter(grepl(pattern = "parquet-ndvi",
                   x=file_name)) -> files
    
  # Convert numeric to date
    
    files %>%
      mutate(unix_date = gsub(pattern = "-dynamic_parquet-ndvi-",
                              replacement = "",
                              x=file_name))%>%
      mutate(unix_date = gsub(pattern = ".gz.parquet",
                              replacement = "",
                              x=unix_date))->files
    
    files %>%
      mutate(month = month(as_date(as.numeric(unix_date)))) -> files
    

    # Filter to relevant month
    
      files %>%
        filter(month == current_month)-> files
      
    # Download files
        
      robust_pb_download(file = files$file_name,
                         tag = "current",
                         repo = "AdamWilsonLab/emma_envdata",
                         dest = file.path(temp_directory),
                         max_attempts = 10,
                         sleep_time = 10)
      
    # Open Dataset and calculate mean
      
      open_dataset(file.path(temp_directory,files$file_name)) %>%
        group_by(cellID) %>%
        summarize(value = mean(value))%>% collect()-> mean_values
  
    # Get template
      
      robust_pb_download(file = "template.tif",
                         tag = "processed_static",
                         repo = "AdamWilsonLab/emma_envdata",
                         dest = file.path(temp_directory),
                         max_attempts = 10,
                         sleep_time = 10)
      
      template <- rast(file.path(temp_directory,"template.tif"))
      
    # Make mean raster from template
      
      values(template) <- NA
            
      values(template)[mean_values$cellID] <- mean_values$value 
  
      plot(template)      
      
    # Fix values
      
      
      template <- (template/100)-1
      template %>%
        terra::mask(mask = template,
                    maskvalue = 0) -> template
      
    # write file  
      
      template %>%      
      writeRaster(filename = file.path(temp_directory,paste(current_month,"_monthly_mean_ndvi.tif",sep = "")),
                  overwrite=TRUE)
      
    # Upload

      pb_upload(file = file.path(temp_directory,paste(current_month,"_monthly_mean_ndvi.tif",sep = "")),
                repo = "AdamWilsonLab/emma_report",
                tag = "current")      
      
      
    #cleanup
      
      unlink(file.path(temp_directory),recursive = TRUE,force = TRUE)
      
    # return metadata
      
      output <- data.frame(filename =paste(current_month,"_monthly_mean_ndvi.tif",sep = ""),
                 repo = "AdamWilsonLab/emma_report",
                 tag = "current")
    
      
      return(output)
      
      
      
}
