#' @description Function to scrape our envdata repo and download the remnant data layers
get_remnants_raster <- function(){

  robust_pb_download(file = "remnants.tif",
                     dest = "data/misc/",
                     repo = "AdamWilsonLab/emma_envdata",
                     tag = "processed_static")

  return(invisible(NULL))


}


################################################################
