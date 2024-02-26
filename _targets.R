library(targets)
library(tarchetypes)
library(tidyverse)
library(arrow)
library(piggyback)
library(plotly)
library(leaflet)
#library(rnoaa)
library(gt)
library(dygraphs)

#remotes::install_github("ropensci/stantargets")
# if(!"basemapR" %in% rownames(installed.packages())){
#   devtools::install_github('Chrisjb/basemapR')
# }

#library(stantargets)
source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")
source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_upload.R")
# source all files in R folder
lapply(list.files("R",pattern="[.]R",full.names = T)[-4], source)

options(tidyverse.quiet = TRUE)
options(clustermq.scheduler = "multicore")

tar_option_set(packages = c("piggyback","cmdstanr", "posterior", "bayesplot", "tidyverse",
                            "stringr","knitr","sf","stars","units","arrow","lubridate","stantargets",
                            "doParallel","raster"),
               deployment="main")

#tar_load(c(envdata, stan_data, model_results, spatial_outputs,model_prediction,parks))
Sys.setenv(HOME="/home/rstudio")

# tar_destroy(ask = F)

list(

  # Load the model data

    tar_target(name = model_results,
               command = get_model_data(file = "model_results.rds")
    )
    ,

    tar_target(name = spatial_outputs,
                 command = get_model_data(file = "spatial_outputs.rds")
    ),

    tar_target(name = model_prediction,
                 command = get_model_data(file = "model_prediction.rds")
    )
    ,

    tar_target(name = parks,
               command = get_park_polygons(temp_directory = "data/temp/",
                                           sacad_filename = "data/manual_downloads/protected_areas/SACAD_OR_2021_Q4.shp",
                                           sapad_filename = "data/manual_downloads/protected_areas/SAPAD_OR_2021_Q4.shp",
                                           cape_nature_filename = "data/manual_downloads/protected_areas/Provincial_Nature_Reserves/CapeNature_Reserves_gw.shp")
               )
   ,

   tar_target(name = remnants,
              command = get_remnants_raster()
   )
   ,


    # tar_age(name = noaa_data,
    #         command = update_climate_data(parks = parks,
    #                                       temp_directory = "data/temp/noaa",
    #                                       sleep_time = 30,
    #                                       max_attempts = 10,
    #                                       reset_all = FALSE, #set this to TRUE to re-download everything, rather than only updating
    #                                       batch = TRUE, #if batch = TRUE, the argument 'batches' will be used to decide what fraction of records to update that day (e.g., 2 batches will update half of the records each day)
    #                                       batches = 2),
    #         age = as.difftime(7, units = "days")
    #         #age = as.difftime(0, units = "hours") #will update whenever run
    # ),

   # tar_age(name = gsod_data,
   #         command = update_climate_data_gsod(parks,
   #                                       temp_directory = "data/temp/gsod",
   #                                       sleep_time = 10,
   #                                       max_attempts = 10),
   #         #age = as.difftime(7, units = "days") #weekly updates
   #         age = as.difftime(1, units = "days") #daily updates
   #         # age = as.difftime(0, units = "hours") #will update whenever run
   # ),


   tar_age(name = most_recent_ndvi_date,
           command = get_most_recent_ndvi_date(),
           #age = as.difftime(7, units = "days") #weekly updates
           age = as.difftime(1, units = "days") #daily updates
           # age = as.difftime(0, units = "hours") #will update whenever run
           ),

   tar_age(name = current_month,
           command = lubridate::month(most_recent_ndvi_date),
           age = as.difftime(1, units = "days") #daily updates
           ),

   tar_target(name = monthly_mean_ndvi,
              command = get_monthly_mean_ndvi(temp_directory = "data/temp/",
                                              current_month = current_month)),

   tar_age(name = inat_data,
              command = get_inat_data(inat_data_location = "data/manual_downloads/inat_project/observations-405358.csv",
                                      temp_directory = "data/temp/inat/",
                                      max_attemps = 10,
                                      sleep_time=10,
                                      oldest_date = "2020-01-01",
                                      sa_parks = parks,
                                      park_buffer = 10000,
                                      verbose=TRUE),
           # age = as.difftime(7, units = "days") #weekly updates
           # age = as.difftime(1, units = "days") #daily updates
            age = as.difftime(28, units = "days") #will update monthly
           ),

   # the target below is used so that things are re-run if the qmd changes
   tar_target(name = report_location,
              command = "report_prototype.qmd",
              format = "file"),

   tar_target(name = reports,
              command = generate_reports(output_directory = "reports/",
                                         temp_directory = "data/temp/reports/",
                                         temp_directory_ndvi = "data/temp/ndvi",
                                         report_location = report_location,
                                         monthly_mean_ndvi = monthly_mean_ndvi,
                                         most_recent_ndvi_date = most_recent_ndvi_date,
                                         tag = "current",
                                         park_data_tag = "park_data",
                                         time_window_days = 365,
                                         min_date = "2010-01-01",
                                         n_stations = 3,
                                         parks = parks,
                                         sleep_time = 10,
                                         max_attempts = 10,
                                         ... = ndwi,
                                         ... = gsod_data))
   ,

   tar_target(name = model_summary,
              command = workflow(call = rmarkdown::render(input = "model_summary.qmd"),
                                 ... = model_results,
                                 ... = model_prediction,
                                 ... = spatial_outputs)),

   tar_target(name = index,
              command = workflow(call = rmarkdown::render(input = "index.qmd"),
                                 ... = parks,
                                 ... = reports,
                                 ... = model_summary))




)
