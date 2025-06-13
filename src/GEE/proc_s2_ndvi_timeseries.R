pacman::p_load(terra, tidyverse, data.table, lubridate, 
               mgcv, phenofit,stars)

date_fire_start <- ymd("2019-12-20") # approximate!

# Load raster and corresponding times
d_time <- fread("data/gee_SA_EcoMap_KI/flinders_chase_S2_ndvi_2018-01-01_2025-06-01.csv")
r0 <- rast("data/gee_SA_EcoMap_KI/flinders_chase_S2_ndvi_2018-01-01_2025-06-01.tif")
time(r0) <- d_time$date


# convert to data.table
s0 <- st_as_stars(r0)
names(s0) <- "ndvi"
d0 <- s0 %>% as.data.table()



vec_dates <- d0$time %>% unique %>% sort

d0[,`:=`(id = .GRP), by=.(x,y)][,`:=`(month=month(time), year=year(time))]

ref <- d0[time < ymd("2019-12-20")][
  ,`:=`(month = month(time))
][,.(ndvi_u = mean(ndvi,na.rm=T)), by=.(x,y,id,month)]

tables()
setkeyv(ref, cols=c("x","y","id","month"))
setkeyv(d0, cols=c("x","y","id","month"))


d1 <- merge(ref, d0, by=c("x","y","id","month"))

d1[,`:=`(ndvi_anom = ndvi - ndvi_u)]
d1 <- d1[order(id,time)][,`:=`(ndvi_u3 = frollmean(ndvi_anom, n=3, align = 'right', na.rm=T))]
d1 <- d1[order(id,time)][,`:=`(ndvi_anom_u12 = frollmean(ndvi_anom, n=12, align = 'right', na.rm=T),
  ndvi_anom_min12 = frollmean(ndvi_anom, n=12, align = 'right', na.rm=T))]



fnp_out <- file.path("data/postproc", str_replace(basename(sources(r0)), ".tif", ".parquet"))
# fwrite(d1, file = fnp_out)

# 1.3GB
arrow::write_parquet(as.data.frame(d1), 
                     sink = fnp_out, 
                     compression = 'zstd', compression_level = 6)



# m0 <- bam(ndvi_u ~ 
#             te(doy, ndvi_u12, bs=c("cc","ts"), k=c(12,12)), 
#           data=ref %>% mutate(fid=factor(id)), 
#           discrete = T)
# plot(m0,scheme=2,pages=1)

summary(m0)

# scratch ========

sel_ids <- sample(d0$id, 10)

v0 <- d0[id%in%sel_ids]
v0 <- v0[order(id,time)][,`:=`(ndvi_u3 = frollmean(ndvi, n=3, align = 'right', na.rm=T))]
v0 <- v0[order(id,time)][,`:=`(ndvi_u12 = frollmean(ndvi, n=12, align = 'right', na.rm=T))]
v0 <- v0[order(id,time)][,`:=`(ndvi_min12 = frollmean(ndvi, n=12, align = 'right', na.rm=T))]


v0[id%in%sel_ids] %>% 
  ggplot(aes(time, ndvi_u3, color=factor(id)))+
  geom_point()

v0[id%in%sel_ids] %>% 
  ggplot(aes(time, ndvi_u12, color=factor(id)))+
  geom_point()

v0[id%in%sel_ids] %>% 
  ggplot(aes(time, ndvi_min12, color=factor(id)))+
  geom_point()

plot(r0)