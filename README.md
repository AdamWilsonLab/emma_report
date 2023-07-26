# emma_report

# EMMA workflow overview

The EMMA workflow consists of four modules, each with a separate Github repo:

1) The Environmental Data module (https://github.com/AdamWilsonLab/emma_envdata)

2) The Modelling and Change Detection module (https://github.com/AdamWilsonLab/emma_model)

3) The Change Classification module (https://github.com/AdamWilsonLab/emma_change_classification)

4) The Reporting module (https://github.com/AdamWilsonLab/emma_report)


## Data

The data used by this workflow are stored in multiple locations.  Weather data are stored via releases. NDVI, NDWI, and fire history data are pulled from releases in our [emma_envdata](https://github.com/AdamWilsonLab/emma_envdata) Github repo.

## Releases

* GSOD contains weather data from the [Global Surface Summary of the Day](https://www.ncei.noaa.gov/metadata/geoportal/rest/metadata/item/gov.noaa.ncdc:C00516/html), which are extracted using [GSODR](https://cran.r-project.org/package=GSODR). The file "gsod_stations" contains metadata on the stations within the focal domain, and the other files contain data for each weather station in the area. Files are in Apache Arrow format.

* NOAA contains weather data from [Integrated Surface Database](https://www.ncei.noaa.gov/pub/data/noaa/isd-format-document.pdf), which are extracted using [rnoaa](https://cran.r-project.org/package=rnoaa). The file "noaa_stations" contains metadata on the stations within the focal domain, and the other files contain data for each weather station in the area.  Files are in Apache Arrow format.
