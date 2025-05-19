# EMMA Reports

Draft reports can be found [here](https://adamwilsonlab.github.io/emma_report/)

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

## Changelog

* May 14, 2024
  - Added a new section to the README to describe the data used by the workflow and where it is stored.  Added a new section to describe the releases and what they contain.  Added a new section to describe the changelog.
* May 13, 2024 - Significant updates to the report website including:
  - Use `tar_terra_rast` and `tar_terra_vect` for spatial targets objects. This has the advantage of writing a .tif (or whatever) format for the target output itself, which eliminates the need to push the data to a release for 'publishing.'
  - Website Updates
    - Switch from separate html pages to a quarto website with multiple linked pages
    - added tar_quarto to build the project as part of the targets workflow
    - Updated the main index.html page to include a summary of the project and links to the other pages
    - Added a new page for the project summary with abstract, team, etc.
    - retired the emma.eco google sites page and replaced with this generated one.
    - Park-level reports updates
      - added a new target `report_qmd_dir` that generates all the park-level .qmd files prior to running the website build (so they are included in the site architecture)
      - condensed and enriched the time-since-fire plots
      - condensed the weather data into a three-panel linked dygraph (temp, precip, spi)
  - The Google Earth api changed a few months ago and broke the automatic updates in emma_envdata.
