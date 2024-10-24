library(rinat)

# Note: this function requires a manual download of observations iNaturalist observations to "prime the pump"
# Once this manual download is done, the species list is used to get updated records from iNat



#' @param inat_data_location file path to an inat bulk download containing all species of interest
#' @param temp_directory a temporary directory to save things.  will be deleted.
#' @param max_attempts maximum number of attempts to download/upload before giving up
#' @param sleep_time time to pause between upload/download attempts
#' @param oldest_date Date as character (yyyy-mm-dd format). Any records older than this will be discarded
# function to pull invasive species records from inaturalist
get_inat_data <- function(inat_data_location = "data/manual_downloads/inat_project/observations-405358.csv",
                          temp_directory = "data/temp/inat/",
                          max_attemps = 10,
                          sleep_time=10,
                          oldest_date = "2020-01-01",
                          sa_parks = parks,
                          park_buffer = 10000,
                          verbose=TRUE){


  #ensure directories are empty

    if(dir.exists(file.path(temp_directory))){

      unlink(file.path(temp_directory),recursive = TRUE,force = TRUE)

    }

  #create directories if needed

    if(!dir.exists(file.path(temp_directory))){

      dir.create(file.path(temp_directory),recursive = TRUE)

    }


  # get domain

    if(verbose){message("Getting domain")}

    #env_files <- pb_list(repo = "AdamWilsonLab/emma_envdata")

    robust_pb_download(file = "domain.gpkg",
                       dest = file.path(temp_directory),
                       repo = "AdamWilsonLab/emma_envdata",
                       tag = "raw_static",
                       overwrite = TRUE,
                       max_attempts = max_attempts,
                       sleep_time = sleep_time)

    domain <- st_read(file.path(temp_directory,"domain.gpkg"))%>%
      st_transform("WGS84")

  # combine sa_parks shapefiles into one, buffer, and union with domain

    if(verbose){message("Combining park shapefiles")}

    st_union(sa_parks$cape_nature, sa_parks$national_parks) %>%
      st_combine() %>%
      st_simplify(dTolerance = 1000) %>%
      st_transform(st_crs(domain)) %>%
      st_make_valid()%>%
      st_buffer(dist = park_buffer) %>%
      st_union(domain)-> parks_and_domain

  # Read in original inat project data

    if(verbose){message("Getting original inat data")}

    inat_proj_data <- read.csv(inat_data_location)

    species_list <-
    inat_proj_data %>%
      pull(scientific_name)%>%
      unique()

    full_records <-NULL


  if(verbose){message("Starting for loop for downloading new inat data")}

  for(i in 1:length(species_list)){

    message("Getting data for species ", i, " of ", length(species_list))

    # get data

    data_i <-   tryCatch(expr =get_inat_obs(taxon_name = species_list[i],
                                                 bounds = domain,
                                                 maxresults = 10000),error = function(e){e})


    # if nothing is returned or other error occurs, move on

    if(inherits(x = data_i,what = "error")){next}

    # filter by date

      data_i %>%
        filter(observed_on > oldest_date) -> data_i

    #convert to sf

      data_i %>%
      st_as_sf(coords = c("longitude","latitude"),
               remove = FALSE) -> data_i

      st_crs(data_i) <-st_crs("WGS84")

    # filter to the domain + parks

      data_i %>%
        st_filter(parks_and_domain) -> data_i

    # plot if desired

      # library(ggplot2)
      # data_i %>%
      #   ggplot()+
      #   geom_sf()+
      #   geom_sf(data=parks_and_domain,fill=NA)

    # skip if no records
      if(nrow(data_i)==0){next}

    # combine with output
      full_records %>%
        bind_rows(data_i) -> full_records


  }


    # Save output as spatial output

      full_records %>%
        st_write(dsn = file.path(temp_directory,"current_inat_records.gpkg"))

    # Upload output

      robust_pb_upload(file = file.path(temp_directory,"current_inat_records.gpkg"),
                       repo = "AdamWilsonLab/emma_report",
                       tag = "current")

    # cleanup

      unlink(temp_directory,recursive = TRUE,force = TRUE)

      gc()

    # end

      return(Sys.Date())


}

