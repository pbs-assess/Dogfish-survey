#dogfish HBLL south index

#DOES SOG dogfish include the juan de fuca strait?
#Statistical 4b does include this area

# library -----------------------------------------------------------------
library(gfdata)
library(gfplot)
library(tidyverse)
library(here)
library(sdmTMB)
library(sf)
library(sp)


# load HBLL south data ----------------------------------------------------
x <- get_ssids()
hbllsets <- get_survey_sets(species = "north pacific spiny dogfish", ssid = c(39, 40))
saveRDS(hbllsets,  "output/dogfishhbllsouth_sets.rds")
saveRDS(hbllsets,  "output/dogfishhbllnorthsouth_sets.rds")


# clean data, get julian, etc.  -------------------------------------------
#sets <- readRDS("output/dogfishhbllsouth_sets.rds")
sets <- readRDS("output/dogfishhbllnorthsouth_sets.rds")

ggplot(sets, aes(as.factor(year), catch_count)) +
  geom_boxplot()
unique(sets$survey_series_desc)
unique(sort(sets$year))
#sets$deployment_time <- as.POSIXct(sets$fe_begin_retrieval_time) # this is from the other database that I collated
sets <- sets %>%
  mutate(dmy = lubridate::ymd_hms(time_deployed)) %>%
  mutate(julian = lubridate::yday(dmy)) #%>%
  #mutate(trip_start_julian = lubridate::yday(trip_start_date)) # see the 2008 dot that is 315? this is wrong

ggplot(sets, aes(as.factor(year), julian)) +
  geom_boxplot() # + facet_wrap(~ )

# how long is the survey
sets$julianstart <- lubridate::yday(sets$time_deployed)
sets$julianend <- lubridate::yday(sets$time_retrieved)
sets$duration <- sets$julianend - sets$julianstart

ggplot(sets, aes(as.factor(year), julian, colour = survey_abbrev)) +
  geom_boxplot() + facet_wrap(~survey_abbrev )

ggplot(sets, aes(as.factor(year), duration)) +
  geom_point(size = 2)

sets$logbot_depth <- log(sets$depth_m)
ggplot(sets, aes(longitude, latitude)) +
  geom_point() +
  facet_wrap(~year)
dog <- add_utm_columns(sets,
                       ll_names = c("longitude", "latitude"), units = "km",
                       utm_crs = 32609
) 
meanlogbot_depth <- mean(dog$logbot_depth, na.rm = TRUE)
dog$logbot_depth_cent <- dog$logbot_depth - meanlogbot_depth
dog <- filter(dog, !is.na(depth_m))
dog <- filter(dog, !is.na(julian))
dog <- filter(dog, !is.na(catch_count))
dog$offset <- log(dog$hook_count)
meanyear <- mean(unique(dog$year))
dog$yearc <- dog$year - mean(unique(dog$year))
dog$juliansmall <- dog$julian - min(dog$julian)

dog |>
  ggplot() +
  geom_point(aes(longitude, latitude)) + 
  facet_wrap(~year)

#saveRDS(dog, "output/dogsog_hbllsouthcleaned.rds")
#dog <- readRDS( "output/dogsog_hbllsouthcleaned.rds")
saveRDS(dog, "output/dogsog_hbllsouthnorthcleaned.rds")
dog <- readRDS( "output/dogsog_hbllsouthnorthcleaned.rds")


# #Remove Juan de Fuca..from 2005 and 2007 (?) PHMA survey. 
shelf_SOG <- st_read("data", "SOG_polygon")
ggplot() +  geom_sf(data = shelf_SOG, fill = NA, colour = "red") +
  geom_point(data = dog, aes(longitude, latitude))

dog$lon <- dog$longitude
dog$lat <- dog$latitude
setssf <- st_as_sf(dog, coords = c("longitude", "latitude"))
st_crs(setssf) <- 4326
setssf <- st_transform(setssf, 32609)
shelf_SOG <- st_transform(shelf_SOG, 32609)
ggplot() +  geom_sf(data = shelf_SOG, fill = NA, colour = "red") +
  geom_sf(data = setssf, colour = "blue")

setsSOG <- st_intersection(setssf, shelf_SOG)
ggplot() +  geom_sf(data = shelf_SOG, fill = NA, colour = "red") +
  geom_sf(data = setsSOG, colour = "blue")
st_geometry(setsSOG) <- NULL

#saveRDS(setsSOG, "output/dogsog_hbllsouthcleanedSOGonly.rds")
saveRDS(setsSOG, "output/dogsog_hbllnorthsouthcleaned_nojuan.rds")

# get samps from HBLL ins south --------------------------------------------


allsamps <- get_survey_samples("north pacific spiny dogfish")
unique(allsamps$survey_series_desc)
test <- filter(allsamps, survey_series_desc == "Strait of Georgia Dogfish Longline")
unique(test$year)

dogsamps <- filter(allsamps, survey_series_desc %in% c(
  "Hard Bottom Longline Inside North ",
  "Strait of Georgia Dogfish Longline",
  "Hard Bottom Longline Inside South "
))

saveRDS(dogsamps, "output/dogfish_samps.rds")

samps <- readRDS("output/dogfish_samps.rds")
# samps <- filter(samps, sex %in% c(1, 2))
samps <- filter(samps, year >= 2000)
range(samps$length)
samps <- filter(samps, length > 25)
sort(unique(samps$year))
sort(unique(samps$sex)) # 1 male, 2 female, 3 unknown?

ggplot() +
  geom_density(
    data = samps, aes(length,
                      group = as.factor(survey_series_desc),
                      fill = as.factor(survey_series_desc)
    ),
    alpha = 0.35, size = 1, colour = "black"
  ) +
  facet_grid(~sex) +
  theme_classic() +
  geom_rug(data = samps, aes(length)) +
  scale_fill_manual(values = c("lightblue", "darkblue", "yellow")) +
  # scale_fill_viridis_d() +
  ylab(label = "Density") +
  xlab(label = "Length")

ggplot() +
  # geom_jitter(data = samps, aes(year, length,
  #            colour = as.factor(survey_series_desc), alpha = 0.15)) +
  geom_jitter(
    data = samps, aes(survey_series_desc, length,
                      group = as.factor(survey_series_desc),
                      fill = as.factor(survey_series_desc)
    ),
    alpha = 0.15, size = 0.5, colour = "grey10"
  ) +
  geom_violin(
    data = samps, aes(survey_series_desc, length,
                      group = as.factor(survey_series_desc),
                      fill = as.factor(survey_series_desc)
    ),
    size = 1, colour = "black", draw_quantiles = c(0.5)
  ) +
  facet_grid(~sex) +
  theme_classic() +
  # geom_rug(data = samps, aes(length)) +
  scale_fill_manual(values = c("lightblue", "darkblue", "yellow")) +
  # scale_fill_viridis_d() +
  ylab(label = "Length") +
  xlab(label = "Year")

ggplot() +
  # geom_jitter(data = samps, aes(year, length,
  #            colour = as.factor(survey_series_desc), alpha = 0.15)) +
  geom_boxplot(
    data = filter(samps, sex %in% c(1, 2)), aes(as.factor(year), length,
                                                fill = as.factor(survey_series_desc)
    ),
    alpha = 0.35, size = 1, colour = "black"
  ) +
  # facet_wrap(~survey_series_desc)
  facet_grid(~ survey_series_desc + sex) +
  theme_classic() +
  scale_x_discrete(breaks = c(2003, 2009, 2015, 2022)) +
  # geom_rug(data = samps, aes(length)) +
  scale_fill_manual(values = c("blue", "darkblue", "yellow")) +
  # scale_fill_viridis_d() +
  ylab(label = "Length") +
  xlab(label = "Year")







# create grid from hull - use this ---------------------------------------------------
#dog <- readRDS( "output/dogsog_hbllsouthnorthcleaned.rds")
dog <- readRDS("output/dogsog_hbllnorthsouthcleanedSOGonly.rds") |> 
  mutate(latitude = lat, longitude = lon)

dog$lon <- dog$longitude
dog$lat <- dog$latitude
setssf <- st_as_sf(dog, coords = c("longitude", "latitude"))
st_crs(setssf) <- 4326
setssf <- st_transform(setssf, 32609)

plot(st_geometry(setssf))
hulls <- concaveman::concaveman(
  setssf)
  #setsSOG)
plot(hulls)

# convert to 2*2 km grid, here it's in m
grid_spacing <- 2000

polygony <- st_make_grid(hulls, square = F, cellsize = c(grid_spacing, grid_spacing)) %>%
  st_sf() %>%
  mutate(cell_ID = row_number())
plot(polygony)

center <- st_centroid(polygony)
grid_extent <- st_intersection(st_geometry(hulls), st_geometry(polygony))
center_extent <- st_intersection(st_geometry(hulls), st_geometry(center))
plot(center_extent)
center_extent2 <- st_sf(center_extent)
grid_extent2 <- st_sf(grid_extent)

grid_extent2$area_km <- st_area(grid_extent2) / 1000000 # m to km

# assign depth to points
center_extent3 <- center_extent2 %>%
  mutate(UTM.lon.m = st_coordinates(center_extent2)[, 1]) %>%
  mutate(UTM.lat.m = st_coordinates(center_extent2)[, 2]) %>%
  mutate(UTM.lon = UTM.lon.m / 1000, UTM.lat = UTM.lat.m / 1000) |>
  # select(-FID) |>
  distinct(.keep_all = TRUE)
st_crs(center_extent3) <- 32609
center_extent4 <- st_transform(center_extent3, crs = "+proj=longlat + datum=WGS84")
center_extent5 <- center_extent4 %>%
  mutate(longitude = st_coordinates(center_extent4)[, 1]) %>%
  mutate(latitude = st_coordinates(center_extent4)[, 2])
st_geometry(center_extent5) <- NULL

attr(center_extent5, "names.attrs") <- NULL
str(center_extent5)

b <- marmap::getNOAA.bathy(lon1 = -130, lon2 = -110, lat1 = 20, lat2 = 60, resolution = 1)

depthpoints_center <- marmap::get.depth(b, center_extent5[, c("longitude", "latitude")], locator = FALSE) %>%
  mutate(bot_depth = (depth * -1)) %>%
  rename(longitude = lon, latitude = lat) %>%
  filter(bot_depth > 25) %>%
  mutate(logbot_depth = log(bot_depth)) %>%
  inner_join(center_extent5, by = c("longitude" = "longitude", "latitude" = "latitude"))
plot(depthpoints_center$longitude, depthpoints_center$latitude)
depthpoints_center[duplicated(depthpoints_center), ] # just checking

# join the points back to the grid so I can predict on the grid
grid2 <- st_join(grid_extent2,
                 st_as_sf(depthpoints_center, coords = c("UTM.lon.m", "UTM.lat.m"), crs = 32609),
                 join = st_contains
) %>%
  drop_na(logbot_depth) %>%
  st_drop_geometry()

grid2$offset <- 0

ggplot(grid2, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_raster() # doesn't work, needs to be spaced evenly
ggplot(grid2, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_point()

gridtlcoastal_ras <- grid2 %>% mutate(across(c(UTM.lon, UTM.lat), round, digits = 2)) # make them evenly spaced
ggplot(gridtlcoastal_ras, aes(UTM.lon, UTM.lat, col = log(bot_depth))) +
  geom_raster()
gridnew <- gridtlcoastal_ras

gridnew$area_km <- as.numeric(gridnew$area_km)
gridnew$UTM.lon <- as.numeric(gridnew$UTM.lon)
gridnew$UTM.lat <- as.numeric(gridnew$UTM.lat)

attr(gridnew, "names.attrs") <- NULL
str(gridnew)


plot(gridnew$longitude, gridnew$latitude)

#saveRDS(gridnew, "output/predictiongrid_SOG_2km.rds")
#saveRDS(gridnew, "output/predictiongrid_SOGNOjuan_2km.rds")
#saveRDS(gridnew, "output/predictiongrid_SOGnorthsouth_2km.rds")
saveRDS(gridnew, "output/predictiongrid_SOGnorthsouthnojuan_2km.rds")


# load prediction grid ----------------------------------------------------
#grid <- readRDS("output/predictiongrid_bccoast_SOG.rds")
#grid <- readRDS("output/predictiongrid_SOG_2km.rds")
#grid <- readRDS("output/predictiongrid_SOGnorthsouth_2km.rds")
#grid <- readRDS("output/predictiongrid_SOGNOjuan_2km.rds")
grid <- readRDS("output/predictiongrid_SOGnorthsouthnojuan_2km.rds")

ggplot(grid, aes(UTM.lon, UTM.lat, colour = logbot_depth)) +
  geom_point() 

#grid <- grid |> filter(UTM.lon.m > 750000)
grid$offset <- log(1)
as.numeric(max(dog$depth_m))
as.numeric(min(dog$depth_m))
grid <- grid |> filter(logbot_depth <= log(200))
grid <- grid |> filter(logbot_depth >= log(24))
grid$X <- grid$UTM.lon
grid$Y <- grid$UTM.lat

ggplot(grid, aes(UTM.lon, UTM.lat, colour = logbot_depth)) +
  geom_point()
grid$area_km <- 4
grid$juliansmall <- 0
grid$julian <- 280
grid$logbot_depth_cent <- grid$logbot_depth - meanlogbot_depth

range(dog$Y)
range(dog$X)
glimpse(grid)
saveRDS(grid, "output/grid_northsouthcleaned.rds")
saveRDS(grid, "output/grid_northsouthcleaned_nojuan.rds")

# Model and mesh -------------------------------------------------------------

#grid <- readRDS("output/grid_northsouthcleaned.rds")
#grid <- readRDS("output/predictiongrid_SOGnorthsouthnojuan_2km.rds")
grid <- readRDS("output/grid_northsouthcleaned_nojuan.rds")

#dog <- readRDS("output/dogsog_hbllsouthcleaned.rds")
#dog <- readRDS( "output/dogsog_hbllsouthnorthcleaned.rds")
dog <- readRDS("output/dogsog_hbllnorthsouthcleaned_nojuan.rds")

#get rid of the survey that ran much later
x <- filter(dog, year == 2021 & survey_abbrev == "HBLL INS S")
dog <- filter(dog, !(fishing_event_id %in% x$fishing_event_id))
####

mesh <- make_mesh(dog, xy_cols = c("X", "Y"), cutoff = 10)

plot(mesh$mesh, asp = 1, main = "")
points(grid$X, grid$Y, col = "blue")
points(dog$X, dog$Y, col = "red")

fourth_root_power_trans <- function() {
  scales::trans_new(
    name = "fourth root power",
    transform = function(x) ifelse(x > 0, x^0.25, -(-x)^0.25),
    inverse = function(x) ifelse(x > 0, x^4, -(-x)^4),
    domain = c(-Inf, Inf)
  )
}

ggplot() +
  inlabru::gg(mesh$mesh) +
  coord_fixed() +
  geom_point(aes(X, Y), data = dog, alpha = 0.2, size = 0.5) +
  geom_point(aes(X, Y, colour = catch_count, size = catch_count),
             data = filter(dog, catch_count > 0)
  ) +
  facet_wrap(~year) +
  scale_color_viridis_c(trans = "fourth_root_power")

dog |>
  ggplot() +
  geom_point(aes(year, catch_count)) 

dog |>
  group_by(year) |>
  mutate(sum = sum(catch_count)) |>
  ggplot() +
  geom_point(aes(year, sum)) +
  geom_line(aes(year, sum))

dog |>
  group_by(year) |>
  mutate(cpue = sum(catch_count) / sum(hook_count)) |>
  ggplot() +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue)) + 
  facet_wrap(~survey_abbrev)

dog |>
  ggplot() +
  geom_point(aes(exp(logbot_depth_cent + meanlogbot_depth), catch_count)) + 
  facet_wrap(~year)

dog |>
  ggplot() +
  geom_point(aes(juliansmall, catch_count)) + 
  facet_wrap(~year)


m1 <- sdmTMB(
  formula = catch_count ~  1  + logbot_depth_cent + I(logbot_depth_cent^2),# + juliansmall, 
  offset = dog$offset/100,
  #time_varying = ~ 0 + logbot_depth_cent + I(logbot_depth_cent^2), 
  #time_varying_type = "rw0", 
  data = dog,
  mesh = mesh, 
  spatiotemporal = "rw",
  time = "year",
  priors = sdmTMBpriors(
       b = normal(location = c(0, 0, 0), #, 0),
                  scale = c(1, 1, 1))), #, 1))),
      control = sdmTMBcontrol(
         #start = list(ln_tau_V = matrix(log(0.1), 2, 1)), #acting on year
         #map = list(ln_tau_V = factor(as.vector(matrix(c(NA), 2, 1)))),
         newton_loops = 1L),
  extra_time = c(2006, 2017, 2020),
  silent = FALSE,
  family = tweedie(),
  spatial = TRUE)

sanity(m1)
m1$sd_report
#saveRDS(m1, "output/SOGmodel_hbllsouth.rds")
#saveRDS(m1, "output/SOGmodel_hbllnorthsouth.rds")
#saveRDS(m1, "output/SOGmodel_hbllnorthsouth_nojuan.rds")
#saveRDS(m1, "output/SOGmodel_hbllnorthsouth_nojuan_dg.rds") #didnt converge
#saveRDS(m1, "output/SOGmodel_hbllnorthsouth_nojuan_woutjulian_tw.rds")
saveRDS(m1, "output/SOGmodel_hbllnorthsouth_nojuan_woutjulianno2021_tw.rds")


unique(sort(dog$year))
years <- seq(min(dog$year), max(dog$year), 1)
grid <- purrr::map_dfr(as.numeric(years), ~ tibble(grid, year = .x))
unique(grid$year)
pred <- predict(m1, newdata = grid, return_tmb_object = TRUE)
index <- get_index(pred, area = grid$area_km, bias_correct = TRUE)
#saveRDS(index, "output/index_HBLLsouth.rds")
#saveRDS(index, "output/index_HBLLnorthsouth.rds")
#saveRDS(index, "output/index_HBLLnorthsouth_nojuan.rds")
#saveRDS(index, "output/index_HBLLnorthsouth_nojuan_dg.rds") #didnt converge
#saveRDS(index, "output/index_HBLLnorthsouth_nojuan_woutjulian_tw.rds")
saveRDS(index, "output/index_HBLLnorthsouth_nojuan_woutjulianno2021_tw.rds")

index <- readRDS("output/index_HBLLnorthsouth.rds")|> mutate(type = "NS")
index1 <- readRDS("output/index_HBLLnorthsouth_nojuan.rds")|> mutate(type = "NS_nojuan_tweedie")
#index2 <- readRDS("output/index_HBLLnorthsouth_nojuan_dg.rds")|> mutate(type = "NS_nojuan_dg")
index3 <- readRDS("output/index_HBLLnorthsouth_nojuan_woutjulian_tw.rds")|> mutate(type = "NS_nojuan_nojulian")
index4 <- readRDS("output/index_HBLLnorthsouth_nojuan_woutjulianno2021_tw.rds")|> mutate(type = "NS_nojuan_nojulian_no2021")

index <- rbind(index, index1, index3, index4)

ggplot(index, aes(year, est, group = type, fill = type,  colour = type)) +
  #geom_line(col = "#8D9999") +
  #geom_point(col = "#8D9999") +
  geom_line() +
  geom_point() +
  scale_fill_viridis_d() + 
  scale_colour_viridis_d() + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, colour = "black", fill = type), alpha = 0.2) + #, fill = "#8D9999") +
  theme_classic() + 
  facet_wrap(~type, ncol = 4) + 
  guides(colour = "none")

