pacman::p_load(ausplots, tidyverse, data.table)

d <- ausplotsR::get_ausplots()

d %>% str

dt <- d$site.info %>% as.data.table()
dt

fwrite(dt, file = "srifai@gmail.com/work/research/SA_EcoMap_KI/data/ausplots_sites/ausplots_sites_2025-05-22.csv")
dt <- fread("data/ausplots_sites/ausplots_sites_2025-05-22.csv")


dt$longitude %>% is.na %>% table
dt$latitude %>% is.na %>% table

dt %>% select(longitude, latitude)

dt2 <- dt %>% 
  select(-c(description, comments, visit_notes, location_description, 
            plot_dimensions))

dt2 <- dt2 %>% 
  mutate(x = longitude, 
         y = latitude)

fwrite(dt2, "data/ausplots_sites/ausplots_sites_subset_2025-05-22.csv")

