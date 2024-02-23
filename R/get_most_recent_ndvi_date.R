# Function to get the most recent date of ndvi record
get_most_recent_ndvi_date <- function(){
  
  
  # Get list of available env data files
  
    env_files <- pb_list(repo = "AdamWilsonLab/emma_envdata")
  
  
  # get most recent NDVI data
  
  env_files %>%
    filter(tag == "clean_ndvi_modis") %>%
    filter(grepl(pattern = ".tif",x = file_name)) %>%
    mutate(file_date = gsub(pattern = ".tif",replacement = "",x = file_name)) %>%
    mutate(file_date = gsub(pattern = "_",replacement = "-",x = file_date)) %>%
    slice(which.max(as_date(file_date))) -> most_recent_ndvi_file
  
  
  # Get NDVI date
  
  most_recent_ndvi_file %>%
    mutate(file_name = gsub(pattern = ".tif",replacement="",x=file_name))%>%
    pull(file_name)%>%
    as_date() -> most_recent_ndvi_date
  

  return(most_recent_ndvi_date)  
  
}
