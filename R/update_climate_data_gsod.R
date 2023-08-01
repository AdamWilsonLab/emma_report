#tar_load(parks)
library(GSODR)
library(tidyverse)
library(lubridate)

source("https://raw.githubusercontent.com/AdamWilsonLab/emma_model/medecos/R/robust_pb_upload.R")
update_climate_data_gsod <- function(parks,
                                     temp_directory = "data/temp/gsod",
                                     sleep_time = 1,
                                     max_attempts = 100){

    #create release if needed

      releases <- pb_list(repo = "AdamWilsonLab/emma_report")

      if(!"GSOD" %in% releases$tag){

        pb_new_release(repo = "AdamWilsonLab/emma_report",
                       tag = "GSOD")

      }

    #Create temp directory if needed

      if(!dir.exists(temp_directory)){dir.create(temp_directory,recursive = TRUE)}

    #create subdir within the temp dir

      if(!dir.exists(file.path(temp_directory,"/upload_check/"))){
        dir.create(file.path(temp_directory,"/upload_check/"),recursive = TRUE)}

    # Get list of stations

    load(system.file("extdata", "isd_history.rda", package = "GSODR"))

    # only take stations with data within South Africa

      isd_history %>%
        filter(COUNTRY_NAME == "SOUTH AFRICA")-> isd_history

    # only take stations with relatively recent data

      isd_history %>%
        separate(col = END,
                 into = c("end_year","end_month","end_day"),
                 sep = c(4,6,8),remove = FALSE) %>%
        separate(col = BEGIN,
                 into = c("start_year","start_month","start_day"),
                 sep = c(4,6,8),remove = FALSE) %>%
        filter(end_year >= year(Sys.Date())-1) -> isd_history

    # filter by stations within 100 km of our parks

      isd_history %>%
        st_as_sf(coords = c("LON","LAT"),
                 remove = FALSE )-> isd_history

      st_crs(isd_history) <- st_crs("WGS84")

      parks$cape_nature %>%
        st_buffer(dist = 100000) %>%
        st_union()-> cn_buffer

      parks$national_parks %>%
        st_buffer(dist = 100000) %>%
        st_union()-> np_buffer

      st_union(np_buffer,cn_buffer) -> park_buffer

      isd_history %>%
      st_filter(y = park_buffer %>%
                  st_transform(st_crs(isd_history))) -> isd_history

      rm(np_buffer,cn_buffer,park_buffer)


    #update station metadata

      isd_history %>%
        st_drop_geometry() %>%
        write_parquet(sink = file.path(temp_directory,"gsod_stations.gz.parquet"),
                      compression = "gzip")


      robust_pb_upload(file = file.path(temp_directory,"gsod_stations.gz.parquet"),
                repo = "AdamWilsonLab/emma_report/",
                tag = "GSOD",
                overwrite = TRUE,
                max_attempts = 10,
                sleep_time = 10,
                temp_directory = file.path(temp_directory,"/upload_check/"))

    # remove the parquet

      file.remove(file.path(temp_directory,"gsod_stations.gz.parquet"))

    # Download station data


      for(i in 1:nrow(isd_history)){

        station_i <- isd_history$STNID[i]

        message("Downloading data for ",station_i,": station ", i, " of ", nrow(isd_history))

        data_i <- robust_get_GSOD(years = as.numeric(isd_history$start_year[i]):year(Sys.Date()),
                           station = station_i,
                           max_attempts = max_attempts)

        # move on if there was an issue in the download

        if(!inherits(data_i,"data.frame")){next}

        # write data as a parquet file

          data_i %>%
            write_parquet(sink = file.path(temp_directory,paste(station_i,".gz.parquet",sep = "")),
                          compression = "gzip")

        #upload as a release

          robust_pb_upload(file = file.path(temp_directory,paste(station_i,".gz.parquet",sep = "")),
                           repo = "AdamWilsonLab/emma_report/",
                           tag = "GSOD",
                           overwrite = TRUE,
                           max_attempts = 10,
                           sleep_time = 10,
                           temp_directory = file.path(temp_directory,"/upload_check/"))

        #remove the parquet
          file.remove(file.path(temp_directory,paste(station_i,".gz.parquet",sep = "")))


      }

      # clean up temp files

        unlink(file.path(temp_directory),
               recursive = TRUE,
               force = TRUE)

      # clean up garbage

        gc()

      return(as.character(Sys.Date()))



  }#end fx


#########################

robust_get_GSOD <- function(years, station, max_attempts=10){

  n_attempts <- 1

  while(n_attempts <= max_attempts){

    #message(n_attempts)

    out_data <- tryCatch(get_GSOD(years = years,
                                  station = station),
                         error = function(e){e})

    # if it worked, move on
      if(inherits(out_data, c("data.table","data.frame"))){
        return(out_data)
      }


    # send message if giving up
      if(n_attempts == max_attempts){

        message("Reached maximum download attempts for station ", station,
                ", giving up.")

        }

    # if it failed increment
      n_attempts = n_attempts + 1


  } #while loop


  return(out_data)


}




