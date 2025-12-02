#!/usr/bin/env Rscript
# Script to invalidate and rebuild monthly_delta_ndvi.tif

library(targets)

cat("Invalidating monthly_delta_ndvi.tif target...\n")
tar_invalidate(monthly_delta_ndvi.tif)

cat("\nRebuilding monthly_delta_ndvi.tif target...\n")
tar_make(names = c("monthly_delta_ndvi.tif"))

cat("\nDone! Now checking the result...\n")
source("debug_ndvi.R")
