# emma_report

# EMMA workflow overview

The EMMA workflow consists of four modules, each with a separate Github repo:
1) The Environmental Data module (https://github.com/AdamWilsonLab/emma_envdata)
2) The Modelling and Change Detection module (https://github.com/AdamWilsonLab/emma_model)
3) The Change Classification module (https://github.com/AdamWilsonLab/emma_change_classification)
4) The Reporting module (https://github.com/AdamWilsonLab/emma_report)

## Releases
The release tagged "NOAA" contains NOAA ISD data (https://www.ncei.noaa.gov/pub/data/noaa/isd-format-document.pdf) within our modeling domain. The file "noaa_stations" contains metadata on the stations within the domain, and the other files contain data for each weather station in the area.  Files are in Apache Arrow format.
