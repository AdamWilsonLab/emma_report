library(rmarkdown)
library(stars)
library(tidyverse)
library(lubridate)
library(arrow)
library(dygraphs)
library(rgee)
library(SPEI)
library(ggplot2)
#webshot::install_phantomjs()
source("R/get_park_polygons.R")

#tar_load(monthly_mean_ndvi)
#tar_load(most_recent_ndvi_date)
#tar_load(parks)
#tar_load(remnants)
#source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_download.R")
#source("https://raw.githubusercontent.com/AdamWilsonLab/emma_envdata/main/R/robust_pb_upload.R")

generate_reports <- function(output_directory = "reports/",
                             temp_directory = "data/temp/reports",
                             temp_directory_ndvi = "data/temp/ndvi",
                             report_location = "report_prototype.qmd",
                             monthly_mean_ndvi = monthly_mean_ndvi,
                             most_recent_ndvi_date= most_recent_ndvi_date,
                             park_data_tag = "park_data",
                             time_window_days = 120,
                             n_stations = 3,
                             parks,
                             remnants,
                             sleep_time = 10,
                             max_attempts = 10,
                             tag = "current",
                             min_date = "1970-01-01",
                             verbose=FALSE,
                             ...
){


  #ensure directories are empty

  if(dir.exists(file.path(output_directory))){

    unlink(file.path(output_directory),recursive = TRUE,force = TRUE)

  }

  if(dir.exists(file.path(temp_directory))){

    unlink(file.path(temp_directory),recursive = TRUE,force = TRUE)

  }

  if(dir.exists(file.path(temp_directory_ndvi))){

    unlink(file.path(temp_directory_ndvi),recursive = TRUE,force = TRUE)

  }


  #create directories if needed

    if(!dir.exists(file.path(output_directory))){

      dir.create(file.path(output_directory), recursive = TRUE)

    }

  #Create temp directory (needs to come after get_park_polygons if using the same temp_directory, since the temp folder is deleted)

    if(!dir.exists(file.path(temp_directory))){

      dir.create(file.path(temp_directory),recursive = TRUE)

    }


  if(!dir.exists(file.path(temp_directory_ndvi))){

    dir.create(file.path(temp_directory_ndvi),recursive = TRUE)

  }


  # Get outputs from model

    #model_results <- tar_load(model_results)

    #model_prediction <- tar_load(model_prediction)

    #spatial_outputs <- tar_load(spatial_outputs)


  # Get list of available env data files

    env_files <- pb_list(repo = "AdamWilsonLab/emma_envdata")

  # Get list of available report files

    report_files <- pb_list(repo = "AdamWilsonLab/emma_report")

  # create output release if needed

    if(!park_data_tag %in% report_files$tag){

      pb_release_create(repo = "AdamWilsonLab/emma_report",
                        tag = park_data_tag)

    }


  #get most recent fire data

    env_files %>%
      filter(tag == "processed_most_recent_burn_dates") %>%
      mutate(file_date = gsub(pattern = ".tif",replacement = "",x = file_name)) %>%
      mutate(file_date = gsub(pattern = "_",replacement = "-",x = file_date)) %>%
      slice(which.max(as_date(file_date))) -> most_recent_fire_file

    robust_pb_download(file = most_recent_fire_file$file_name,
                  dest = file.path(temp_directory),
                  repo = "AdamWilsonLab/emma_envdata",
                  tag = most_recent_fire_file$tag,
                  max_attempts = max_attempts,
                  sleep_time = sleep_time)

    most_recent_fire_raster <- terra::rast(file.path(temp_directory,
                                                     most_recent_fire_file$file_name))

    most_recent_fire_raster[most_recent_fire_raster == 0] <- NA #toss NAs

  # convert from date of fire to years since fire

    years_since_fire_raster <-
      terra::app(x = most_recent_fire_raster,
                 fun = function(x){
                   return( time_length(Sys.Date() - as_date(x,origin = lubridate::origin),unit = "years"))
                 })

  # release the years since fire raster

    years_since_fire_raster %>%
      writeRaster(filename = file.path(temp_directory,"years_since_fire.tif"))

    robust_pb_upload(file = file.path(temp_directory,"years_since_fire.tif"),
                     repo = "AdamWilsonLab/emma_report",
                     tag = tag,
                     max_attempts = 10,
                     sleep_time = 10,
                     temp_directory = temp_directory,
                     overwrite = TRUE)

  # crop years since fire raster to the remnants

    remnants <- terra::rast("data/misc/remnants.tif")

    years_since_fire_raster %>%
      terra::mask(remnants) -> years_since_fire_raster

  # make a polygon version and convert to WGS84 (for plotting ease)

    fires_wgs <- terra::as.polygons(x = years_since_fire_raster) %>%
      st_as_sf() %>%
      rename(Years = lyr.1) %>%
      st_transform(crs = st_crs(4326))

  # get most recent NDVI data

    env_files %>%
      filter(tag == "clean_ndvi_modis") %>%
      filter(grepl(pattern = ".tif",x = file_name)) %>%
      mutate(file_date = gsub(pattern = ".tif",replacement = "",x = file_name)) %>%
      mutate(file_date = gsub(pattern = "_",replacement = "-",x = file_date)) %>%
      slice(which.max(as_date(file_date))) -> most_recent_ndvi_file

    robust_pb_download(file = most_recent_ndvi_file$file_name,
                dest = file.path(temp_directory_ndvi),
                repo = "AdamWilsonLab/emma_envdata",
                tag = most_recent_ndvi_file$tag,
                max_attempts = max_attempts,
                sleep_time = sleep_time)

  # Load the NDVI raster

    most_recent_ndvi_raster <- terra::rast(file.path(temp_directory_ndvi,
                                                     most_recent_ndvi_file$file_name))

  # Fix the NDVI values

    most_recent_ndvi_raster <- (most_recent_ndvi_raster/100)-1

    most_recent_ndvi_raster %>%
      terra::mask(mask = most_recent_ndvi_raster,
                  maskvalue = 0) -> most_recent_ndvi_raster

  # Get NDVI date

    most_recent_ndvi_file %>%
      mutate(file_name = gsub(pattern = ".tif",replacement="",x=file_name))%>%
      pull(file_name)%>%
      as_date() -> most_recent_ndvi_date

  # Monthly mean NDVI

    robust_pb_download(file = monthly_mean_ndvi$filename,
                       dest = file.path(temp_directory_ndvi),
                       repo = monthly_mean_ndvi$repo,
                       tag = monthly_mean_ndvi$tag,
                       overwrite = TRUE,
                       max_attempts = max_attempts,
                       sleep_time = sleep_time)

    monthly_mean_ndvi_raster <- terra::rast(file.path(temp_directory_ndvi,monthly_mean_ndvi$filename))


    # Create delta NDVI raster

    monthly_delta_ndvi_raster <- (most_recent_ndvi_raster - monthly_mean_ndvi_raster)

    if(crs(most_recent_ndvi_raster,proj=TRUE) != crs(monthly_mean_ndvi_raster,proj=TRUE)){

      stop("NDVI CRS mismatch")

    }

    if(terra::ext(most_recent_ndvi_raster) != terra::ext(monthly_mean_ndvi_raster)){

      stop("NDVI extent mismatch")

    }


    if(crs(monthly_delta_ndvi_raster,proj=TRUE) != crs(most_recent_ndvi_raster,proj=TRUE)){

      crs(monthly_delta_ndvi_raster) <- crs(most_recent_ndvi_raster)
    }

    # Double check projections

    if(crs(most_recent_ndvi_raster, proj = TRUE) !=
       crs(monthly_mean_ndvi_raster, proj = TRUE)){
      stop("NDVI layers have different projections")
    }

    if(crs(most_recent_ndvi_raster, proj = TRUE) !=
       crs(monthly_delta_ndvi_raster, proj = TRUE)){
      stop("NDVI layers have different projections")
    }

    # Write monthly delta NDVI layer

    monthly_delta_ndvi_raster %>%
      writeRaster(filename = file.path(temp_directory_ndvi,"monthly_delta_NDVI.tif"),
                  overwrite=TRUE)

    # Upload delta NDVI in case anyone wants it

    robust_pb_upload(file = file.path(temp_directory_ndvi,"monthly_delta_NDVI.tif"),
                     repo = "AdamWilsonLab/emma_report",
                     tag = "current",
                     max_attempts = max_attempts,
                     sleep_time = sleep_time,
                     temp_directory = temp_directory,
                     overwrite = TRUE)

  # Long term mean NDVI

        # Get mean NDVI

          robust_pb_download(file = "mean_ndvi.tif",
                             dest = file.path(temp_directory_ndvi),
                             repo = "AdamWilsonLab/emma_envdata",
                             tag = "current",
                             max_attempts = max_attempts,
                             sleep_time = sleep_time)

        # Load the mean NDVI raster

          mean_ndvi_raster <- terra::rast(file.path(temp_directory_ndvi,"mean_ndvi.tif"))

        # Fix the mean NDVI values

          mean_ndvi_raster <- (mean_ndvi_raster/100)-1

          mean_ndvi_raster %>%
            terra::mask(mask = mean_ndvi_raster,
                        maskvalue = 0) -> mean_ndvi_raster

        # Create delta NDVI raster

           delta_ndvi_raster <- (most_recent_ndvi_raster - mean_ndvi_raster)

           if(crs(most_recent_ndvi_raster,proj=TRUE) != crs(mean_ndvi_raster,proj=TRUE)){

             stop("NDVI CRS mismatch")

           }

           if(terra::ext(most_recent_ndvi_raster) != terra::ext(mean_ndvi_raster)){

             stop("NDVI extent mismatch")

           }


        if(crs(delta_ndvi_raster,proj=TRUE) != crs(most_recent_ndvi_raster,proj=TRUE)){

          crs(delta_ndvi_raster) <- crs(most_recent_ndvi_raster)
        }

        # Double check projections

           if(crs(most_recent_ndvi_raster, proj = TRUE) !=
             crs(mean_ndvi_raster, proj = TRUE)){
             stop("NDVI layers have different projections")
           }

           if(crs(most_recent_ndvi_raster, proj = TRUE) !=
              crs(delta_ndvi_raster, proj = TRUE)){
             stop("NDVI layers have different projections")
           }

        # Write delta NDVI layer

           delta_ndvi_raster %>%
           writeRaster(filename = file.path(temp_directory_ndvi,"delta_NDVI.tif"),
                       overwrite=TRUE)

        # Upload delta NDVI in case anyone wants it

          robust_pb_upload(file = file.path(temp_directory_ndvi,"delta_NDVI.tif"),
                           repo = "AdamWilsonLab/emma_report",
                           tag = "current",
                           max_attempts = max_attempts,
                            sleep_time = sleep_time,
                            temp_directory = temp_directory,
                            overwrite = TRUE)

  # 3 month mean NDVI

          # get most recent NDVI data

          env_files %>%
            filter(tag == "clean_ndvi_modis") %>%
            filter(grepl(pattern = ".tif",x = file_name)) %>%
            mutate(file_date = gsub(pattern = ".tif",replacement = "",x = file_name)) %>%
            mutate(file_date = gsub(pattern = "_",replacement = "-",x = file_date)) %>%
            filter(Sys.Date()-as_date(file_date) < 90) %>%
            filter(!file_name %in% most_recent_ndvi_file$file_name) -> most_recent_quarter_ndvi_file

          robust_pb_download(file = most_recent_quarter_ndvi_file$file_name,
                             dest = file.path(temp_directory_ndvi),
                             repo = "AdamWilsonLab/emma_envdata",
                             tag = most_recent_ndvi_file$tag,
                             max_attempts = max_attempts,
                             sleep_time = sleep_time)

          # Load the NDVI raster

          most_recent_quarter_ndvi_raster <- terra::rast(file.path(temp_directory_ndvi,
                                                           most_recent_quarter_ndvi_file$file_name))

          # Fix the NDVI values

            most_recent_quarter_ndvi_raster <- (most_recent_quarter_ndvi_raster/100)-1

            most_recent_quarter_ndvi_raster %>%
              terra::mask(mask = most_recent_quarter_ndvi_raster,
                          maskvalue = 0) -> most_recent_quarter_ndvi_raster

          # Take mean value

            most_recent_quarter_ndvi_raster <-
              terra::app(x = most_recent_quarter_ndvi_raster,
                fun = function(x){mean(x, na.rm = TRUE)}
                )

            # Create quarterly delta NDVI raster

            quarterly_delta_ndvi_raster <- (most_recent_ndvi_raster - most_recent_quarter_ndvi_raster)

            if(crs(most_recent_ndvi_raster,proj=TRUE) != crs(quarterly_delta_ndvi_raster,proj=TRUE)){

              stop("NDVI CRS mismatch")

            }

            if(terra::ext(most_recent_ndvi_raster) != terra::ext(quarterly_delta_ndvi_raster)){

              stop("NDVI extent mismatch")

            }


            if(crs(quarterly_delta_ndvi_raster,proj=TRUE) != crs(most_recent_ndvi_raster,proj=TRUE)){

              crs(quarterly_delta_ndvi_raster) <- crs(most_recent_ndvi_raster)
            }

            # Double check projections

            if(crs(most_recent_ndvi_raster, proj = TRUE) !=
               crs(quarterly_delta_ndvi_raster, proj = TRUE)){
              stop("NDVI layers have different projections")
            }


            # Write delta NDVI layer

            quarterly_delta_ndvi_raster %>%
              writeRaster(filename = file.path(temp_directory_ndvi,"quarterly_delta_NDVI.tif"),
                          overwrite = TRUE)


            # Upload delta NDVI in case anyone wants it

            robust_pb_upload(file = file.path(temp_directory_ndvi,"quarterly_delta_NDVI.tif"),
                             repo = "AdamWilsonLab/emma_report",
                             tag = "current",max_attempts = 10,
                             sleep_time = 10,
                             temp_directory = temp_directory,
                             overwrite = TRUE)


  # MODIS NDWI
            ## Commenting NDWI out for now, as gee hasn't updated to the new version yet.
            ## uncomment below and adjust the corresponding bits of report_prototype.qmd if this changes

      # robust_pb_download(file = "ndwi.tif",
      #                    tag = tag,
      #                    dest = file.path(temp_directory),
      #                    repo = "AdamWilsonLab/emma_envdata",
      #                    max_attempts = max_attempts,
      #                    sleep_time = sleep_time)
      #
      # ndwi_rast <- raster::raster(terra::rast(file.path(temp_directory,"ndwi.tif"))) %>%
      #   round(digits = 2) #round to save memory

    # MODIS NDWI date

      # env_files %>%
      #   filter(file_name == "ndwi.tif")%>%
      #   pull(timestamp) %>%
      #   as_date() -> most_recent_ndwi_date

  # Other drought layers?


  # Get the weather station metadata

    # robust_pb_download(file = "noaa_stations.gz.parquet",
    #             dest = file.path(temp_directory),
    #             repo = "AdamWilsonLab/emma_report",
    #             tag = "NOAA",
    #             max_attempts = max_attempts,
    #             sleep_time = sleep_time)


    robust_pb_download(file = "gsod_stations.gz.parquet",
                       dest = file.path(temp_directory),
                       repo = "AdamWilsonLab/emma_report",
                       tag = "GSOD",
                       max_attempts = max_attempts,
                       sleep_time = sleep_time)

    # stations <- arrow::read_parquet(file.path(temp_directory,"noaa_stations.gz.parquet"))
    stations <- arrow::read_parquet(file.path(temp_directory,"gsod_stations.gz.parquet"))
    stations_sf <- st_as_sf(stations,coords = c("LON","LAT"))
    st_crs(stations_sf) <- st_crs("EPSG:4326")
    stations_sf <- st_transform(stations_sf, crs = st_crs(parks$cape_nature))

  # Get invasive species data

    robust_pb_download(file = "current_inat_records.gpkg",
                       dest = file.path(temp_directory),
                       repo = "AdamWilsonLab/emma_report",
                       tag = "current",
                       max_attempts = max_attempts,
                       sleep_time = sleep_time)

    invasives <- st_read(file.path(temp_directory,"current_inat_records.gpkg"))

  # Generate the National Park reports via a for loop

    #park_name <- unique(parks$national_parks$CUR_NME)[1]
    #park_name <- unique(parks$national_parks$CUR_NME)[2]
    #park_name <- unique(parks$national_parks$CUR_NME)[4] #Algulhas

  for (park_name in unique(parks$national_parks$CUR_NME)){

    gc()

    focal_park <- parks$national_parks %>%
      filter(CUR_NME == park_name)

    debug <- tryCatch(expr =
      render(input = report_location,
               output_file = gsub(pattern = " ",replacement = "_",
                                            x = paste0('report.', park_name, '.html')),
               output_dir = output_directory
        ),
      error = function(e){message("Error processing ", park_name);e}
      )

    if(inherits(debug,"error")){stop("Error processing ", park_name)}

  }# end for loop


    # Generate the Cape Nature reports via a for loop
      #park_name <- unique(parks$cape_nature$COMPLEX)[33]

    for (park_name in unique(parks$cape_nature$COMPLEX)){

      gc()

      focal_park <- parks$cape_nature %>%
        filter(COMPLEX == park_name)

      debug <- tryCatch(expr =
                render(input = report_location,
                       output_file = gsub(pattern = " ",replacement = "_",
                                          x = paste0('report.', park_name, '.html')),
                       output_dir = output_directory),
              error = function(e){message("Error processing ", park_name); e}
      )

      #if(inherits(debug,"error")){stop()}

    }# end for loop


    #clean up temp directory
      unlink(file.path(temp_directory), recursive = TRUE, force = TRUE)
      unlink(file.path(temp_directory_ndvi), recursive = TRUE, force = TRUE)



}#end fx

