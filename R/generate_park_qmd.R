library(rmarkdown)
library(stars)
library(tidyverse)
library(lubridate)
library(arrow)
library(dygraphs)
library(rgee)
library(SPEI)
library(ggplot2)
source("R/get_park_polygons.R")


#tar_load(parks)

generate_park_qmds <- function(
    parks,
    report_location,
    output_directory="reports"){

#create directory if needed
if(!dir.exists(file.path(output_directory))){
  dir.create(file.path(output_directory), recursive = TRUE)
}

# Proceed if rendering the whole project, exit otherwise
#if (!nzchar(Sys.getenv("QUARTO_PROJECT_RENDER_ALL"))) {
#  quit()
#}

park_name="Table Mountain National Park"
park_name="West Coast National Park"

for (park_name in unique(parks$national_parks$CUR_NME)[1:3]){# remove [1] to process them all!
  templ <- readLines("report_prototype.qmd")

    writeLines(
      gsub("focal_park_name: \"Table Mountain National Park\"",paste0("focal_park_name: \"",park_name,"\"",sep=""),templ),
      file.path(output_directory,paste0(gsub(" ","_",park_name), ".qmd"))
    )
    message("Done with ", park_name)
  }

return(output_directory)

}#end fx

    # debug <- tryCatch(expr =
    #   render(input = report_location,
    #            output_file = gsub(pattern = " ",replacement = "_",
    #                                         x = paste0('report.', park_name, '.html')),
    #            output_dir = output_directory
    #     ),
    #   error = function(e){message("Error processing ", park_name);e}
    #   )
    #
    # if(inherits(debug,"error")){stop("Error processing ", park_name)}

#  }# end for loop


    # Generate the Cape Nature reports via a for loop
      # park_name <- unique(parks$cape_nature$COMPLEX)[33]
      # park_name <- unique(parks$cape_nature$COMPLEX)[18]

#     for (park_name in unique(parks$cape_nature$COMPLEX)[1:3]){. # remove [1] to process them all!
#
#       gc()
#
#       focal_park <- parks$cape_nature %>%
#         filter(COMPLEX == park_name)
#
#       debug <- tryCatch(expr =
#                 render(input = report_location,
#                        output_file = gsub(pattern = " ",replacement = "_",
#                                           x = paste0('report.', park_name, '.html')),
#                        output_dir = output_directory),
#               error = function(e){message("Error processing ", park_name); e}
#       )
#
#       #if(inherits(debug,"error")){stop()}
#
#     }# end for loop
#
#
#     #clean up temp directory
#       unlink(file.path(temp_directory), recursive = TRUE, force = TRUE)
#       unlink(file.path(temp_directory_ndvi), recursive = TRUE, force = TRUE)
#
#
#
# }#end fx

