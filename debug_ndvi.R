#!/usr/bin/env Rscript
# Debug script to check NDVI targets

library(targets)
library(terra)

cat("========================================\n")
cat("Checking NDVI-related targets\n")
cat("========================================\n\n")

# Check if targets exist
targets_to_check <- c(
  "most_recent_ndvi.tif",
  "monthly_mean_ndvi.tif",
  "monthly_delta_ndvi.tif"
)

for (target_name in targets_to_check) {
  cat(sprintf("\n--- Checking %s ---\n", target_name))
  
  tryCatch({
    # Try to load the target
    obj <- tar_read_raw(target_name)
    
    if (inherits(obj, "SpatRaster")) {
      cat(sprintf("✓ Loaded successfully\n"))
      cat(sprintf("  - Class: %s\n", class(obj)[1]))
      cat(sprintf("  - Layers: %d\n", nlyr(obj)))
      cat(sprintf("  - Dimensions: %d x %d\n", nrow(obj), ncol(obj)))
      cat(sprintf("  - CRS: %s\n", substr(crs(obj, proj=TRUE), 1, 50)))
      cat(sprintf("  - Extent: %s\n", paste(as.vector(ext(obj)), collapse=", ")))
      
      # Try to access values
      vals <- try(values(obj)[1:10], silent = TRUE)
      if (!inherits(vals, "try-error")) {
        cat(sprintf("  - First 10 values: %s\n", paste(round(vals, 3), collapse=", ")))
      } else {
        cat(sprintf("  ✗ ERROR accessing values: %s\n", vals))
      }
    } else {
      cat(sprintf("✗ Not a SpatRaster: %s\n", class(obj)[1]))
    }
    
  }, error = function(e) {
    cat(sprintf("✗ ERROR loading target: %s\n", e$message))
  })
}

cat("\n========================================\n")
cat("Checking _targets metadata\n")
cat("========================================\n\n")

# Check targets metadata
meta <- tar_meta(fields = c("name", "type", "bytes", "format", "error"))
meta_ndvi <- meta[grep("ndvi", meta$name, ignore.case = TRUE), ]
print(meta_ndvi)

cat("\n========================================\n")
cat("Done\n")
cat("========================================\n")
