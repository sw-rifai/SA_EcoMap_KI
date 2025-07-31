## Description:
## Hack script to kmean cluster Kaynini hyperspectral data over KI
## and then compare how many NVIS MVS groups each cluster appears in. 

## Author: Sami
## Date(start): 2025-07-31

pacman::p_load(arcpullr, terra, geodata, data.table, tidyverse); 

oz <- geodata::gadm("Australia", path="data")
dim(oz)
plot(oz)

ki_ext <- terra::ext(c(xmin = 136.5199, xmax = 138.129, ymin = -36.095, ymax = -35.556))

ki_poly <- terra::crop(oz, ki_ext)
plot(ki_poly)



url_nvis <- "https://gis.environment.gov.au/gispubmap/rest/services/ogc_services/NVIS_ext_mvg/MapServer"
url_nvis <- "https://gis.environment.gov.au/gispubmap/rest/services/ogc_services/NVIS_ext_mvg/MapServer"

ki_poly <- terra::project(ki_poly, crs("EPSG:3857"))
ki_poly4326 <- terra::project(ki_poly, crs("EPSG:4326"))

nvis <- arcpullr::get_map_layer(url_nvis, sf_object = sf::st_as_sf(ki_poly))
a
plot(a)



# nvis <- terra::rast("../data_general/NVIS/GRID_NVIS6_0_AUST_EXT_MVS/mvs6_0e_by_name.lyr")
im <- terra::rast("data/kaynini/HS-L1CR-FF-04CF-20250102_L2A.tif")
im <- terra::crop(im, ki_poly4326)

terra::plot(im[[40]])


# mask water
m1 <- terra::ifel(im[[50]] > 0.20, 1, NA)
plot(m1)
im2 <- terra::crop(im, m1, mask=T)
plot(im2[[40]])

# mask clouds
m2 <- terra::ifel(im[[40]] < 0.5, 1, NA)
plot(m2)
im2 <- terra::crop(im2, m2, mask=T)
plot(im2[[40]])


ki_west_bbox <- terra::ext(c(xmin = 136.5199, xmax = 137.2, 
                             ymin = -36.095, ymax = -35.556))
crs(ki_west_bbox) <- crs('4326')

lines(ki_west_bbox, col='red')

im3 <- terra::crop(im2, ki_west_bbox)
plot(im3[[40]])


names(im3) <- paste0("b",1:50)


n7 <- terra::rast("../data_general/NVIS/nvis_v7/NVIS_V7_0_AUST_RASTERS_EXT_ALL/NVIS_V7_0_AUST_EXT.gdb")
n7 <- n7[["NVIS7_0_AUST_EXT_MVS_ALB"]]

ki_west_bbox9473 <- terra::project(ki_west_bbox, from = crs("EPSG:4326"), to = crs(n7))
n7 <- terra::crop(n7, ki_west_bbox9473)
n7_4326 <- terra::project(n7, crs("EPSG:4326"))

names(n7_4326) <- "mvs"

n7_mvs <- terra::project(n7_4326, im3, method = "mode")

d <- c(n7_mvs, im3) %>% 
  as.data.table()

d <- d[is.na(mvs)==F]

d <- d[is.na(b1)==F]
apply(d %>% select_if(is.numeric), 2, FUN = function(x) sum(is.na(x)))




d <- d %>% mutate(mvs = fct_drop(d$mvs))
table(d$mvs)
d[,`:=`(mvs_nobs = .N),by=mvs]
d <- d[mvs_nobs >= 10]
d <- d %>% mutate(mvs = fct_drop(d$mvs))

n_mvs <- d$mvs %>% unique %>% length

vec_idx <- sample.int(nrow(d), size = 5000)


tmp <- d[vec_idx, ]
tmp_mat <- tmp %>% 
  select(starts_with("b")) %>% 
  as.matrix()

j <- kmeans(tmp_mat, 10)
j$cluster


table(tmp$mvs, j$cluster) %>% print

table(tmp$mvs, j$cluster) %>% 
  apply(., 2, FUN = function(x) sum(x > 0)) %>% 
  median()


fn1 <- function(nclusters){
  j <- kmeans(tmp_mat, nclusters)
  table(tmp$mvs, j$cluster) %>% 
    apply(., 2, FUN = function(x) sum(x > 0)) %>% 
    median()
}

v <- data.table(clusters = 5:50)
v2 <- v %>% 
  rowwise() %>% 
  mutate(val = fn1(clusters))
v2 %>% setDT()


v2 %>% 
  ggplot(aes(clusters, val))+
  geom_point() + 
  lims(y=c(0, 15))


