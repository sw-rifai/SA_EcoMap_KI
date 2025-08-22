pacman::p_load(terra, lidR, tidyverse)


src_dir <- "/home/sami/Downloads/samiLiDAR_test"
list_fps <- list.files(src_dir, pattern = ".las")
dst_dir <- "/home/sami/Downloads/samiLiDAR_test/rasterized_pcs/canopy_height"

# check if dst dir exists
if(dir.exists(dst_dir)==F){
  dir.create(dst_dir)
}


fn_proc_ch <- function(fp){
  src_fp <- file.path(src_dir, fp)
  dst_fp <- file.path(dst_dir, str_replace(basename(fp), ".las", ".tif"))
  
  
  las <- readLAS(src_fp)
  r_c <- rasterize_canopy(las = las, res=1, algorithm = p2r())
  r_t <- rasterize_terrain(las = las, res=1, algorithm = tin())
  c_h <- r_c - r_t
  
  terra::writeRaster(c_h, 
                     filename = dst_fp, 
                     # gdal=c("COMPRESS=DEFLATE", "TFW=YES"), 
                     overwrite = T)
  print(paste("finished: ", fp))
}


# list_fps %>% 
#   lapply(., fn_proc_ch)

for(i in 1:length(list_fps)){
  print(i)
  fn_proc_ch(list_fps[i])
}



