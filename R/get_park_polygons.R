library(sf)
library(tidyverse)
library(piggyback)
library(dplyr)

#' @param temp_directory Temporary working directory (will be deleted)
#' @param sacad_filename filename of the sacad shapefile
#' @param sapad_filename filename of the sapad shapefile
#' @note This function uses the emma_envdata domain to crop the park polygons
#' @return sf dataframe object containing the parks
get_park_polygons <- function(temp_directory = "data/temp/parks",
                              sacad_filename = "data/manual_downloads/protected_areas/SACAD_OR_2021_Q4.shp",
                              sapad_filename = "data/manual_downloads/protected_areas/SAPAD_OR_2021_Q4.shp",
                              cape_nature_filename = "data/manual_downloads/protected_areas/Provincial_Nature_Reserves/CapeNature_Reserves_gw.shp",
                              park_data_tag = "park_data"){

  # Pull in different protected areas

    sacad <- st_read(sacad_filename)
    sapad <- st_read(sapad_filename)

  # Pull in CapeNature reserves

    cn <- st_read(cape_nature_filename)

  # Combine the two pa datasets

    sacad %>%
      mutate( PROC_DOC = NA) %>%
      dplyr::select(colnames(sapad)) %>%
      bind_rows(sapad) -> all_pas

  rm(sacad,sapad)


  # Create directory if needed

    if(!dir.exists(temp_directory)){
      dir.create(temp_directory, recursive = TRUE)
    }

  # Download domain

    robust_pb_download(file = "domain.gpkg",
                tag = "raw_static",
                repo = "AdamWilsonLab/emma_envdata",
                dest = file.path(temp_directory),
                max_attempts = 10,
                sleep_time = 10)

  # Read domain

    domain <- st_read(file.path(temp_directory, "domain.gpkg"))

  # Match projections

    all_pas <- st_transform(x = all_pas,
                           crs = st_crs(domain))

    cn <- st_transform(x = cn,
                            crs = st_crs(domain))

# Filter to National Parks only

    all_pas %>%
      filter(grepl(pattern = "national park",
                   x = SITE_TYPE,ignore.case = TRUE)) -> all_pas


  # Crop PAs

    #Use this if you want the park boundaries cropped
    # all_pas <-
    # st_intersection(x = domain,
    #                 y = st_make_valid(all_pas))%>%
    #   dplyr::select(-domain)

    #Use this if you just want intersecting parks, even the bits outside our domain
    all_pas <- all_pas[which(as.logical(st_intersects(x = all_pas, y = domain))),] %>%
                  st_make_valid()

    cn <- cn[which(as.logical(st_intersects(x = cn, y = domain))),]%>%
              st_make_valid()

  # merge capenature and sanparks into single parks polygon layer
    cn2 <-  cn |>
      mutate(WMCM_TYPE = "Provincial") |>
      rename(CUR_NME = COMPLEX,  # this merges across individual sub units - change if you want each little park separate.
             SITE_TYPE=RESERVECON)

    protected_areas <- bind_rows(all_pas, cn2) |>
      dplyr::select(
        name=CUR_NME,
        type=SITE_TYPE) |>
      group_by(name) |> #and type?
      summarise(geometry = st_union(geometry), .groups = "drop") |>
      mutate(area_ha = drop_units(set_units(st_area(geometry), ha)))


  # update projection
    robust_pb_download(file = "template.tif",
                dest = file.path(temp_directory),
                repo = "AdamWilsonLab/emma_envdata",
                tag = "processed_static",
                max_attempts = 10,
                sleep_time = 10)

    template <- terra::rast(file.path(temp_directory,"template.tif"))

    protected_areas <-
    st_transform(x = protected_areas,
                 crs = st_crs(template))

  # make output

    protected_areas %>%
      st_write(dsn = file.path(temp_directory, "protected_areas.gpkg"),
               append=FALSE,
               quiet = TRUE
               )

    # release gpkg

      robust_pb_upload(file = file.path(temp_directory, "protected_areas.gpkg"),
                       repo = "AdamWilsonLab/emma_report",
                       tag = park_data_tag)

      file.remove(file.path(temp_directory, "protected_areas.gpkg"))

  # cleanup
      unlink(file.path(temp_directory),
             recursive = TRUE,
             force = TRUE)

 #convert to terra::vect

      protected_areas_v <- terra::vect(protected_areas)
      terra::crs(protected_areas_v) <- st_crs(protected_areas)$wkt  # restore projection

      print(terra::crs(protected_areas_v))
        # return data product
      return(protected_areas_v)

}
############################################



