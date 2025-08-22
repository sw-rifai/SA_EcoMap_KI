pacman::p_load(terra, lidR, tidyverse)


src_dir <- "/home/sami/Downloads/samiLiDAR_test/rasterized_pcs/canopy_height"
list_fps <- list.files(src_dir, pattern = ".tif")
dst_dir <- "/home/sami/Downloads/samiLiDAR_test/rasterized_pcs/cc_gte_2m"


# check if dst dir exists
if(dir.exists(dst_dir)==F){
  dir.create(dst_dir)
}


fn_proc_cc <- function(fp){
  src_fp <- file.path(src_dir, fp)
  dst_fp <- file.path(dst_dir, str_replace(basename(fp), ".tif", "_chgte2m_.tif"))
  
  ch <- terra::rast(src_fp)
  cc_gte_2m <- ch > 2
  

  terra::writeRaster(cc_gte_2m, 
                     filename = dst_fp, 
                     # gdal=c("COMPRESS=DEFLATE", "TFW=YES"), 
                     overwrite = T)
  print(paste("finished: ", fp))
}

for(i in 1:length(list_fps)){
  print(i)
  fn_proc_cc(list_fps[i])
}
