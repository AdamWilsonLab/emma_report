library(rnoaa)
library(piggyback)
library(arrow)
library(tidyverse)
library(sf)

# Metadata at: https://www.ncei.noaa.gov/pub/data/noaa/isd-format-document.pdf

#helper function for extracting means.  a bit faster than normal method

good_max <- function(x){

  if(all(is.na(x))){NA}else{
    base::max(x,na.rm = TRUE)
  }

}

good_mean <- function(x){

  if(all(is.na(x))){NA}else{
    base::mean(x,na.rm = TRUE)
  }

}


update_climate_data <- function(parks,
                                temp_directory = "data/temp/noaa",
                                sleep_time = 1,
                                max_attempts = 100,
                                reset_all=FALSE,
                                batch = TRUE,
                                batches = 2){

  #clean dir if needed
    if(dir.exists(temp_directory)){
      unlink(temp_directory,recursive = TRUE,force = TRUE)

    }

  #create dir if needed
    if(!dir.exists(temp_directory)){
      dir.create(temp_directory,recursive = TRUE)

    }

  #get content
    noaa_rel <- pb_list(repo = "AdamWilsonLab/emma_report",
                        tag = "NOAA") %>%
      dplyr::filter(file_name != "")

  #construct needed bounding box

    parks$cape_nature %>%
      st_union(y = parks$national_parks) %>%
      st_transform(crs = st_crs(4326)) %>%
      st_bbox() -> bb1

    class(bb1) <- "numeric"


    #update station metadata
      stations <- isd_stations_search(bbox = bb1)

      stations %>%
        mutate(start_year = substr(x = begin,start = 1,stop = 4),
               end_year = substr(x = end,start = 1,stop = 4)) -> stations

      stations %>%
      write_parquet(sink = file.path(temp_directory,"noaa_stations.gz.parquet"),
                    compression = "gzip")


      pb_upload(file = file.path(temp_directory,"noaa_stations.gz.parquet"),
                repo = "AdamWilsonLab/emma_report",
                tag = "NOAA",
                overwrite = TRUE)

    Sys.sleep(sleep_time)

  # identify which data need to be refreshed
      #any which aren't present OR
      #any which are still sampling

    noaa_rel %>%
      mutate(station_id = gsub(pattern = ".gz.parquet",replacement = "",x = file_name)) %>%
      dplyr::filter(station_id != "noaa_stations") ->
      stations_in_releases

    stations %>%
      mutate(out_name = gsub(x = station_name, pattern = "/",replacement = "",fixed = TRUE)) %>%
      mutate(out_name = gsub(x = out_name, pattern = " ",replacement = "_")) %>%
      mutate(out_name = gsub(x = out_name, pattern = ")",replacement = "_",fixed = TRUE)) %>%
      mutate(out_name = gsub(x = out_name, pattern = "(",replacement = "_",fixed = TRUE) )%>%
      dplyr::filter(!(usaf %in% stations_in_releases$station_id)|
               end_year ==  format(Sys.Date(), "%Y")) -> stations_to_update



    #to save time, we'll only download full information for stations that are missing.  for ones that just need an update, we can just pull the newest data

    if(reset_all){

      stations_to_update %>%
        mutate(action = "add") -> stations_to_update


    }else{

      stations_to_update %>%
        mutate(action = case_when(!usaf %in% stations_in_releases$station_id ~ "add",
                                  usaf %in% stations_in_releases$station_id ~ "update"
        )) -> stations_to_update


    }


    #batches=TRUE, only update the stations in the corresponding batch

      if(batches){

        batched_stations <- split(as.numeric(sort(unique(stations$usaf))), cut_number(as.numeric(sort(unique(stations$usaf))), batches))

        current_batch <- batched_stations[rep(1:batches,ceiling(366/length(1:batches)))[lubridate::yday(Sys.Date())]]

        current_batch <- unlist(current_batch) %>% unname()


        stations_to_update %>%
          filter(usaf %in% current_batch) -> stations_to_update



        }



    if(  length(unique(stations_to_update$usaf)) > 0){
      for(i in 1:length(unique(stations_to_update$usaf))){

      usaf_i <- unique(stations_to_update$usaf)[i]

      wban_i <- stations$wban[which(stations$usaf==usaf_i)]

      name_i <- stations$station_name[which(stations$usaf==usaf_i)] %>%
        gsub(pattern = "/",replacement = "",fixed = TRUE)%>% #what kind of sadist puts a slash in a name?
        gsub(pattern = " ",replacement = "_") %>%
        gsub(pattern = ")",replacement = "_",fixed = TRUE) %>%
        gsub(pattern = "(",replacement = "_",fixed = TRUE)


      action_i <- stations_to_update$action[which(stations_to_update$usaf==usaf_i)]

      if(action_i == "update"){

        message("updating ",usaf_i)

        #sometimes there are temporary issues with downloading previous uploads.  to prevent this from breaking the full workflow, I use a tryCatch and next

        to_rm <-  tryCatch(expr = robust_pb_download(file = paste(usaf_i,".gz.parquet",sep = ""),
                                             repo = "AdamWilsonLab/emma_report",
                                             tag = "NOAA",
                                             dest = file.path(temp_directory),
                                             max_attempts = max_attempts,
                                             sleep_time = sleep_time),
                   error = function(e){e}
                   )

        if(inherits(x = to_rm, what = "error")){


          message("errors with downloading ",usaf_i,", skipping")
          rm(to_rm)
          next

        }

        # robust_pb_download(file = paste(usaf_i,".gz.parquet",sep = ""),
        #             repo = "AdamWilsonLab/emma_report",
        #             tag = "NOAA",
        #             dest = file.path(temp_directory),
        #             max_attempts = max_attempts,
        #             sleep_time = sleep_time)->to_rm

        old_i <- arrow::read_parquet(file.path(temp_directory,paste(usaf_i,".gz.parquet",sep = "")))


        # should start with the previous year (in case there is any lag time)

        start_i <-
        max(old_i$date)%>%
          substr(start = 1,stop = 4)%>%
          as.numeric()-1

        end_i <- stations$end_year[which(stations$usaf==usaf_i)] #note, will need to update the code in  ~ 8000 years



        data_i <-lapply(start_i:end_i,
                        FUN = function(x){
                          tryCatch(expr = isd(usaf = usaf_i,
                                              wban = wban_i,
                                              year = x),
                                   error = function(e) message("missing year"))}) %>%
          bind_rows()

        old_i %>%
          filter(!date %in% unique(data_i$date)) -> old_i


        if("AA1_depth" %in% colnames(data_i)){
          data_i$AA1_depth <- as.numeric(data_i$AA1_depth)

        }
        if("AA2_depth" %in% colnames(data_i)){
          data_i$AA2_depth <- as.numeric(data_i$AA2_depth)

        }
        if("AA3_depth" %in% colnames(data_i)){
          data_i$AA3_depth <- as.numeric(data_i$AA3_depth)

        }

        data_i <-
        old_i%>%
          bind_rows(data_i)


      }else{

        start_i <- stations$start_year[which(stations$usaf==usaf_i)] #note, will need to update the code in  ~ 8000 years

        end_i <- stations$end_year[which(stations$usaf==usaf_i)] #note, will need to update the code in  ~ 8000 years

        data_i <-lapply(start_i:end_i,
                        FUN = function(x){
                          tryCatch(expr = isd(usaf = usaf_i,
                                              wban = wban_i,
                                              year = x),
                                   error = function(e) message("missing year"))}) %>%
          bind_rows() %>%
          mutate(station_name = name_i)


      }



      cols_i <- intersect(x = colnames(data_i),
                          y = c("station_name","usaf_station","wban_station","date","time","date_flag","latitude","longitude","type_code","elevation","call_letter",
                                "quality","temperature","temperature_quality","AA1_depth","AA2_depth","AA3_depth"))

      data_i %>%
        dplyr::select(all_of(cols_i)) -> data_i

      #convert depth to numeric
      #if all three aren't present, add them.  this is just to simplify code to take the max value.

        if("AA1_depth" %in% colnames(data_i)){

          data_i$AA1_depth[which(data_i$AA1_depth==9999)] <- NA

        }else{data_i$AA1_depth <- NA}

        if("AA2_depth" %in% colnames(data_i)){

          data_i$AA2_depth[which(data_i$AA2_depth==9999)] <- NA

        }else{data_i$AA2_depth <- NA}

        if("AA3_depth" %in% colnames(data_i)){

          data_i$AA3_depth[which(data_i$AA3_depth==9999)] <- NA

        }else{data_i$AA3_depth <- NA}

      #convert to numeric
        data_i%>%
          mutate(AA1_depth = as.numeric(AA1_depth),
                 AA2_depth = as.numeric(AA2_depth),
                 AA3_depth = as.numeric(AA3_depth)) -> data_i

      #convert temp to C
        data_i %>%
          mutate(temp_c = as.numeric(temperature)/10) %>%
          mutate(temp_c = case_when(temp_c != 999.9 ~ temp_c)) -> data_i

      #convert depth to mm


      # Calculate max and mean values ( if there is any data)

        if(!all(is.na(data_i$AA1_depth),is.na(data_i$AA2_depth),is.na(data_i$AA3_depth))){

          #data_i_og <-data_i
          #data_i <- data_i_og

      data_i %>%
        rowwise()%>%
        mutate(max_precip_mm = good_max(c(AA1_depth, AA2_depth, AA3_depth)))%>%
        mutate(max_precip_mm = max_precip_mm/10)%>%
        mutate(mean_precip_mm  = good_mean(c(AA1_depth,AA2_depth,AA3_depth))) %>%
        mutate(mean_precip_mm = mean_precip_mm/10) -> data_i

        }else{

          data_i %>%
            mutate(max_precip_mm = NA,
                   mean_precip_mm = NA) -> data_i

        }


      # write data as a parquet file

        data_i %>%
          write_parquet(sink = file.path(temp_directory,paste(usaf_i,".gz.parquet",sep = "")),
                        compression = "gzip")

      #upload as a release
        pb_upload(file = file.path(temp_directory,paste(usaf_i,".gz.parquet",sep = "")),
                  repo = "AdamWilsonLab/emma_report",
                  tag = "NOAA")

      #pause to keep github happy
        Sys.sleep(sleep_time)


    }# i loop
    }#if there are things to update
  # clean up cache

    rnoaa::isd_cache$delete_all(force = TRUE)

  # clean up temp files
    unlink(file.path(temp_directory),
           recursive = TRUE,
           force = TRUE)

  # clean up garbage
    gc()

  return(as.character(Sys.Date()))

}# end fx

