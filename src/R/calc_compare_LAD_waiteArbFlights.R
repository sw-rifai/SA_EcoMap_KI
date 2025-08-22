# Notes:
# https://r-lidar.github.io/lidRbook/point_metrics.html
# install.packages('lidR', repos = c('https://r-lidar.r-universe.dev', 'https://cloud.r-project.org'))
# CRAN version may be missing some functions. If so, re-install lidR from above. 


pacman::p_load(lidR, data.table, tidyverse)

fp <- "data/Waite_LiDAR_tests/proj_1/cloud0.las"
las <- readLAS(fp)
print(las)

plot(las)


las2 <- readLAS(fp, select = "xyzi") # load XYZ and intensity only


cloud_metrics(las, func = ~mean(Z)) # calculate mean height
cloud_metrics(las, func = ~min(Z)) # calculate mean height
cloud_metrics(las, func = ~max(Z)) # calculate mean height


