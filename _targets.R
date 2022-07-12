library(targets)
library(tarchetypes)
library(tidyverse)
library(arrow)
library(piggyback)
library(plotly)
library(leaflet)
library(rnoaa)
#remotes::install_github("ropensci/stantargets")
# if(!"basemapR" %in% rownames(installed.packages())){
#   devtools::install_github('Chrisjb/basemapR')
# }
#library(stantargets)
source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")
# source all files in R folder
lapply(list.files("R",pattern="[.]R",full.names = T)[-4], source)

options(tidyverse.quiet = TRUE)
options(clustermq.scheduler = "multicore")

tar_option_set(packages = c("piggyback","cmdstanr", "posterior", "bayesplot", "tidyverse",
                            "stringr","knitr","sf","stars","units","arrow","lubridate","stantargets",
                            "doParallel","raster"),
               deployment="main")

Sys.setenv(HOME="/home/rstudio")

# tar_destroy(ask = F)
## Download the most recent data release

list(

  # Load the model data

    tar_target(name = model_results,
               command = get_model_data(file = "model_results.rds")
    ),

    tar_target(name = spatial_outputs,
                 command = get_model_data(file = "spatial_outputs.rds")
    ),

    tar_target(name = model_prediction,
                 command = get_model_data(file = "model_prediction.rds")
    ),


  tar_target(name = parks,
             command = get_park_polygons(temp_directory = "data/temp/",
                                         sacad_filename = "data/manual_downloads/protected_areas/SACAD_OR_2021_Q4.shp",
                                         sapad_filename = "data/manual_downloads/protected_areas/SAPAD_OR_2021_Q4.shp",
                                         cape_nature_filename = "data/manual_downloads/protected_areas/Provincial_Nature_Reserves/CapeNature_Reserves_gw.shp")
             ),

  tar_target(name = ndwi,
             command = get_release_ndwi_modis(temp_directory = "data/temp/raw_data/NDWI_MODIS/",
                                              tag = "current"))
  ,


  tar_target(name = noaa_data,
             command = update_climate_data(parks = parks,
                                           temp_directory = "data/temp/noaa",
                                           sleep_time = 30,
                                           max_attempts = 10,
                                           reset_all = FALSE) #set this to TRUE to re-download everything, rather than only updating
             ),

 # tar_target(name = reports,
 #            command = generate_reports(output_directory = "reports/",
 #                                       temp_directory = "data/temp/reports/",
 #                                       report_location = "report_prototype.rmd",
 #                                       time_window_days = 365,
 #                                       n_stations = 3,
 #                                       parks = parks,
 #                                       ... = noaa_data,
 #                                       ... = ndwi)),

 tar_target(name = update_index,
            command = knitr::knit("index.Rmd"))
)
