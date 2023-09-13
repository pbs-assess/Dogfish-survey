
## sixgill catches

library(gfdata)
library(gfplot)
library(tidyverse)
library(here)
library(sdmTMB)
library(sf)
library(sp)


x <- get_ssids()
get_species()
x$SURVEY_SERIES_DESC[x$SURVEY_SERIES_ID %in% c(48, 76, 92, 93)]
# sets <- get_survey_sets(species = "north pacific spiny dogfish", ssid = c(48, 76, 92, 93))
# saveRDS(sets,  "C:/Dogfish surveys 2022/output/dogfish_sets.rds")
sets <- readRDS("output/dogfish_sets.rds")

# https://github.com/pbs-assess/yelloweye-inside/blob/master/dogfish/dogfish%20survey%20sets%20data.R

fe <- gfdata::run_sql("GFBioSQL", "SELECT
  S.SURVEY_SERIES_ID,
  SURVEY_SERIES_DESC,
  S.SURVEY_ID,
  SURVEY_DESC,
  YEAR(FE_BEGIN_RETRIEVAL_TIME) YEAR,
  FE.FISHING_EVENT_ID,
  FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
  -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS LONGITUDE,
  FE.GROUPING_CODE,
  GROUPING_DESC,
  GROUPING_DEPTH_ID,
  FE_END_DEPLOYMENT_TIME,
  FE.FE_FISHING_GROUND_COMMENT,
  FE_BEGIN_RETRIEVAL_TIME,
  FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M,
  FE_BOTTOM_WATER_TEMPERATURE,
  TR.TRIP_START_DATE,
  TR.TRIP_END_DATE,
  HOOK_CODE,
  SKATE_COUNT,
  LGLSP_HOOK_COUNT,
  0.0024384 * 0.009144 * LGLSP_HOOK_COUNT AS AREA_SWEPT_KM2 --0.024384 = 8 ft hook spacing (to calculate line length with hook count); 0.009144 = 2*gangion length (= area width)
  FROM FISHING_EVENT FE
  INNER JOIN TRIP_SURVEY TS ON FE.TRIP_ID = TS.TRIP_ID
  INNER JOIN TRIP TR ON FE.TRIP_ID = TR.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  INNER JOIN SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
  INNER JOIN LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
  INNER JOIN GROUPING G ON G.GROUPING_CODE = FE.GROUPING_CODE
  WHERE S.SURVEY_SERIES_ID IN (76)")
names(fe) <- tolower(names(fe))

ye_catch <- gfdata::run_sql("GFBioSQL", "SELECT
  FEC.FISHING_EVENT_ID,
  SUM(CATCH_COUNT) YE_COUNT
  FROM FISHING_EVENT_CATCH FEC
  INNER JOIN CATCH C ON C.CATCH_ID = FEC.CATCH_ID
  INNER JOIN TRIP_SURVEY TS ON TS.TRIP_ID = FEC.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  WHERE C.SPECIES_CODE IN ('442') AND SURVEY_SERIES_ID = 76
  GROUP BY FEC.TRIP_ID,
  FEC.FISHING_EVENT_ID,
  C.SPECIES_CODE
  ORDER BY FEC.FISHING_EVENT_ID")
names(ye_catch) <- tolower(names(ye_catch))

dogfish_catch <- gfdata::run_sql("GFBioSQL", "SELECT
  FEC.FISHING_EVENT_ID,
  SUM(CATCH_COUNT) DOGFISH_COUNT
  FROM FISHING_EVENT_CATCH FEC
  INNER JOIN CATCH C ON C.CATCH_ID = FEC.CATCH_ID
  INNER JOIN TRIP_SURVEY TS ON TS.TRIP_ID = FEC.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  WHERE C.SPECIES_CODE IN ('044') AND SURVEY_SERIES_ID = 76
  GROUP BY FEC.TRIP_ID,
  FEC.FISHING_EVENT_ID,
  C.SPECIES_CODE
  ORDER BY FEC.FISHING_EVENT_ID")
names(dogfish_catch) <- tolower(names(dogfish_catch))

d <- dplyr::left_join(fe, ye_catch)
d <- dplyr::left_join(d, dogfish_catch, by = "fishing_event_id")

saveRDS(d, "output/dogfishsql_sets.rds")




# https://github.com/pbs-assess/yelloweye-inside/blob/master/dogfish/dogfish%20survey%20sets%20data.R

fe <- gfdata::run_sql("GFBioSQL", "SELECT
  S.SURVEY_SERIES_ID,
  SURVEY_SERIES_DESC,
  S.SURVEY_ID,
  SURVEY_DESC,
  YEAR(FE_BEGIN_RETRIEVAL_TIME) YEAR,
  FE.FISHING_EVENT_ID,
  FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
  -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS LONGITUDE,
  FE.GROUPING_CODE,
  GROUPING_DESC,
  GROUPING_DEPTH_ID,
  FE_END_DEPLOYMENT_TIME,
  FE.FE_FISHING_GROUND_COMMENT,
  FE_BEGIN_RETRIEVAL_TIME,
  FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M,
  FE_BOTTOM_WATER_TEMPERATURE,
  TR.TRIP_START_DATE,
  TR.TRIP_END_DATE,
  HOOK_CODE,
  SKATE_COUNT,
  LGLSP_HOOK_COUNT,
  0.0024384 * 0.009144 * LGLSP_HOOK_COUNT AS AREA_SWEPT_KM2 --0.024384 = 8 ft hook spacing (to calculate line length with hook count); 0.009144 = 2*gangion length (= area width)
  FROM FISHING_EVENT FE
  INNER JOIN TRIP_SURVEY TS ON FE.TRIP_ID = TS.TRIP_ID
  INNER JOIN TRIP TR ON FE.TRIP_ID = TR.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  INNER JOIN SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
  INNER JOIN LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
  INNER JOIN GROUPING G ON G.GROUPING_CODE = FE.GROUPING_CODE
  WHERE S.SURVEY_SERIES_ID IN (76)")
names(fe) <- tolower(names(fe))

#does this bring all of the species code?
all_catch <- gfdata::run_sql("GFBioSQL", "SELECT
  FEC.FISHING_EVENT_ID,
  FEC.TRIP_ID,
  C.SPECIES_CODE,
  SUM(CATCH_COUNT) CATCH_COUNT
  FROM FISHING_EVENT_CATCH FEC
  INNER JOIN CATCH C ON C.CATCH_ID = FEC.CATCH_ID
  INNER JOIN TRIP_SURVEY TS ON TS.TRIP_ID = FEC.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  WHERE S.SURVEY_SERIES_ID = 76
  GROUP BY FEC.TRIP_ID,
  FEC.FISHING_EVENT_ID,
  C.SPECIES_CODE
  ORDER BY FEC.FISHING_EVENT_ID")
names(all_catch) <- tolower(names(all_catch))

# dogfish_catch <- gfdata::run_sql("GFBioSQL", "SELECT
#   FEC.FISHING_EVENT_ID,
#   SUM(CATCH_COUNT) DOGFISH_COUNT
#   FROM FISHING_EVENT_CATCH FEC
#   INNER JOIN CATCH C ON C.CATCH_ID = FEC.CATCH_ID
#   INNER JOIN TRIP_SURVEY TS ON TS.TRIP_ID = FEC.TRIP_ID
#   INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
#   WHERE C.SPECIES_CODE IN ('044') AND SURVEY_SERIES_ID = 76
#   GROUP BY FEC.TRIP_ID,
#   FEC.FISHING_EVENT_ID,
#   C.SPECIES_CODE
#   ORDER BY FEC.FISHING_EVENT_ID")
#names(dogfish_catch) <- tolower(names(dogfish_catch))

d <- dplyr::left_join(fe, all_catch)
#d <- dplyr::left_join(d, dogfish_catch, by = "fishing_event_id")

saveRDS(d, "output/dogfishsql_sets_allspecies.rds")
```


Erase?
  ```{r sql, eval = FALSE}


# WHERE SURVEY_SERIES SS S.SURVEY_SERIES_NAME = SOG Dogfish Longline
# Activity_code from aCTIVITY == 39
# WHERE S.SURVEY_ID in (468,563, 564, 499, 501, 565, 566, 502, 503, 567, 568, 453)

# return hook info and I can sum in R to get hook count.
test_hookyield <- gfdata::run_sql("GFBioSQL", "SELECT
  S.SURVEY_ID, S.SURVEY_SERIES_ID, F.FISHING_EVENT_ID,
  F.FE_MAJOR_LEVEL_ID, F.FE_PARENT_EVENT_ID,
  F.FE_SUB_LEVEL_ID, F.FE_MINOR_LEVEL_ID,
	LLSP.LGLSP_HOOKS_SET_COUNT,
	LLSP.LGLSP_HOOK_COUNT,
	LLSP.SKATE_COUNT
FROM FISHING_EVENT F
INNER JOIN TRIP_SURVEY TS ON TS.TRIP_ID = F.TRIP_ID
LEFT JOIN LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = F.FISHING_EVENT_ID
INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  WHERE S.SURVEY_SERIES_ID IN (48, 76, 92, 93)
ORDER BY FE_SUB_LEVEL_ID, FE_MINOR_LEVEL_ID
")

test <- test_hookyield |>
  filter(SURVEY_SERIES_ID == 76)

dogsurvey <- gfdata::run_sql("GFBioSQL", "SELECT
  FEC.FISHING_EVENT_ID, C.SPECIES_CODE,
  SUM(CATCH_COUNT) COUNT
  FROM FISHING_EVENT_CATCH FEC
  INNER JOIN CATCH C ON C.CATCH_ID = FEC.CATCH_ID
  INNER JOIN TRIP_SURVEY TS ON TS.TRIP_ID = FEC.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  WHERE S.SURVEY_SERIES_ID IN (48, 76, 92, 93)
  GROUP BY FEC.TRIP_ID,
  FEC.FISHING_EVENT_ID,
  C.SPECIES_CODE
  ORDER BY FEC.FISHING_EVENT_ID")
names(dogsurvey) <- tolower(names(dogsurvey))
length(unique(dogsurvey$fishing_event_id))

dogcomp <- gfdata::run_sql("GFBioSQL", "SELECT
  S.SURVEY_SERIES_ID,
  LLSP.HOOK_CODE,
  FE_MAJOR_LEVEL_ID,
  SURVEY_SERIES_DESC,  FE.FE_MISC_COMMENT, FE.FE_FISHING_GROUND_COMMENT,
  S.SURVEY_ID,
  FE_SUB_LEVEL_ID,
  SURVEY_DESC,
  FE_PARENT_EVENT_ID,
  YEAR(FE_BEGIN_RETRIEVAL_TIME) YEAR,
  FE.FISHING_EVENT_ID,
  FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
  -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS LONGITUDE,
  FE_END_DEPLOYMENT_TIME,
  FE_BEGIN_RETRIEVAL_TIME,
  FE.GROUPING_CODE,
  GROUPING_DESC,
  GROUPING_DEPTH_ID,
  TR.TRIP_START_DATE,
  TR.TRIP_END_DATE,
  FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M,
  FE.TRIP_ID
  FROM FISHING_EVENT FE
  LEFT JOIN LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
  INNER JOIN TRIP_SURVEY TS ON FE.TRIP_ID = TS.TRIP_ID
  INNER JOIN TRIP TR ON FE.TRIP_ID = TR.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  INNER JOIN SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
  INNER JOIN GROUPING G ON G.GROUPING_CODE = FE.GROUPING_CODE
  WHERE S.SURVEY_SERIES_ID IN (48, 76, 92, 93)")
#  WHERE S.SURVEY_ID in (468,563, 564, 499, 501, 565, 566, 502, 503, 567, 568, 453) ")
names(dogcomp) <- tolower(names(dogcomp))
length(unique(dogcomp$fishing_event_id))

x <- dogcomp %>%
  group_by(fe_fishing_ground_comment, year, grouping_desc, fishing_event_id) %>%
  tally() %>%
  drop_na(fe_fishing_ground_comment)

y <- filter(dogsurvey, species_code == "044")
length(unique(y$fishing_event_id))

dogcomp %>%
  tally(is.na(fe_fishing_ground_comment) == TRUE)

dog <- dogcomp %>%
  # drop_na(fe_fishing_ground_comment) %>%
  dplyr::inner_join(y)
anti <- y %>% dplyr::anti_join(dogcomp) # 45 dont match cause it is aggregated by skate (j hook and circle hook combined) and so fishing event id, and disaggregated by j-hook and circle hook

test <- filter(dog, survey_series_id == 76) # no 2022, 2004, 2019 comparison work
unique(test$survey_series_desc)
unique(sort(test$year))

test <- filter(dog, survey_series_id == 93) # no 1986, 1989,2004,  2022
unique(test$survey_series_desc)
unique(sort(test$year))

test <- filter(dog, survey_series_id == 92) # only 1986, 1989
unique(test$survey_series_desc)
unique(sort(test$year))

test <- filter(dog, survey_series_id == 48) # only 2004, 2019, 2022
unique(test$survey_series_desc)
unique(sort(test$year))

test <- filter(dog, year == 2022)
unique(test$survey_series_desc)
unique(sort(test$year))

saveRDS(dog, "output/dogfishsql_sets.rds")
```

##2022 survey sites
```{r}

dog <- readRDS("output/dogfishsql_sets.rds")
x <- filter(dog, year == 2022)
test <- filter(x, fe_fishing_ground_comment == "SoG Dogfish Site Sinclair Bank")

ggplot(dog, aes(longitude, latitude, group = survey_desc, colour = survey_desc)) + 
  geom_point(size = 2) + scale_colour_viridis_d() + theme_classic() + 
  scale_x_continuous(limits = c(-124.5, -124.1)) + 
  scale_y_continuous(limits = c(49.4, 50))

test <- filter(dog, survey_series_id %in% c(93, 48)) 
unique(test$survey_series_desc)
unique(sort(test$year))

ggplot(test, aes(longitude, latitude, group = survey_desc, colour = survey_desc)) + 
  geom_point()

```


Plots of the raw data from sql command. 
```{r}
dog <- readRDS("output/dogfishsql_sets.rds")

test <- filter(dog, year %in% c(1986, 1989))
unique(test$fe_fishing_ground_comment)
unique(dog$survey_series_id)
sort(unique(dog$year))

ggplot(dog, aes(year, dogfish_count)) +
  geom_boxplot() +
  facet_wrap(~fe_fishing_ground_comment)

test <- filter(dog, survey_series_id == 76) # no 1986, 1989,2004,  2022
unique(test$survey_series_desc)
unique(sort(test$year))

ggplot(dog, aes(grouping_desc, dogfish_count)) +
  geom_boxplot() +
  facet_wrap(~year)

```

Note: hooks fished is variable across surveys. See survey reports. 
Here, I check the dates, survey timing, survey duration. etc including on surveys completed with circle hooks `unique(sort(dog$year))`. 

```{r cleandata}
unique(dog$survey_series_desc)
unique(sort(dog$year))
dog <- filter(d, year > 2000)
dog$deployment_time <- as.POSIXct(dog$fe_begin_retrieval_time) # this is from the other database that I collated
dog2 <- dog %>%
  mutate(dmy = lubridate::ymd_hms(fe_begin_retrieval_time)) %>%
  mutate(julian = lubridate::yday(dmy)) %>%
  mutate(trip_start_julian = lubridate::yday(trip_start_date)) # see the 2008 dot that is 315? this is wrong


# there is a mistake in the database where the month and days are flipped
# I am changing it here but I have told Maria so may not needs to change in the future
# Maria changed it
# x <- lubridate::yday(as.POSIXct("2011-10-11", format = "%Y-%m-%d"))

# dog2$julian <-
#   ifelse(dog2$trip_id == 82551 & dog2$julian == 315, lubridate::yday(as.POSIXct("2011-10-11", format = "%Y-%m-%d")),
#     lubridate::yday(dog2$dmy)
#   )

ggplot(dog2, aes(as.factor(year), julian)) +
  geom_boxplot() # + facet_wrap(~ )

# how long is the survey
dog2$julianstart <- lubridate::yday(dog2$trip_start_date)
dog2$julianend <- lubridate::yday(dog2$trip_end_date)
dog2$duration <- dog2$julianend - dog2$julianstart
# ggplot(dog2, aes(as.factor(year), duration)) +
#  geom_point(size = 2)
```

Plot of DOG survey and date completed and duration. There is a lot of variability in both. 
```{r}

ggplot(dog2, aes(as.factor(year), julian)) +
  geom_boxplot() # + facet_wrap(~ )

ggplot(dog2, aes(as.factor(year), duration)) +
  geom_point(size = 2)
```

The DOG survey is a fixed station survey with 4 depth strata (five depth strata in 1986 and 1989). The locations of the 10-14 stations is stored in the comments. Here, I standardize names, spelling, and correct some of the issues with different station names and lat and longs. 
```{r fixstationnames}
# fishing ground names
ggplot(dog2, aes(grouping_desc, dogfish_count)) +
  geom_boxplot() +
  facet_wrap(~fe_fishing_ground_comment)

# fishing ground
ggplot(
  dog2,
  aes(longitude, latitude, colour = fe_fishing_ground_comment)
) +
  geom_point()

# change names
# noticed a mistake, fe_fishing_ground_comment for 2019 nad julian date 283 says Malaspina but should be Epson Point
# see here:
ggplot(
  filter(dog2, year == 2019 & fe_fishing_ground_comment == "Malaspina Strait"),
  aes(longitude, latitude, colour = julian)
) +
  geom_point()
dog2$fe_fishing_ground_comment[c(dog2$year == 2019 & dog2$fe_fishing_ground_comment == "Malaspina Strait" & dog2$julian == 283)] <- "Epsom Point"

ggplot(
  filter(dog2, year == 2019 & fe_fishing_ground_comment == "Malaspina Strait"),
  aes(longitude, latitude, colour = fe_fishing_ground_comment)
) +
  geom_point()

# fishing ground
ggplot(
  filter(dog2, longitude > -123.8),
  aes(longitude, latitude, colour = fe_fishing_ground_comment)
) +
  geom_text(aes(label = fe_fishing_ground_comment)) +
  geom_point()

# i am going to take out east valdes for now as its confused with both
dog2 <- filter(dog2, fe_fishing_ground_comment != "E. Valdes")

vec <- dog2$fe_fishing_ground_comment
dog2$location <- recode(vec,
                        "Espon Point" = "Epsom Point",
                        "Epson Point" = "Epsom Point",
                        "SoG Dogfish Site Epsom Point" = "Epsom Point",
                        "West Cape Lazo" = "Cape Lazo",
                        "White Isle - Halibut Bank" = "Halibut Banks",
                        "White Isle" = "Halibut Banks",
                        "Halibut Bank" = "Halibut Banks",
                        "While Isle - Halibut Bank" = "Halibut Banks",
                        "Polier Pass" = "Porlier Pass",
                        "Dettwiller Point Galiano Island North." = "Porlier Pass",
                        "Dettwiller Point - Galiano Island North side." = "Porlier Pass",
                        "Dettwiller Point - Galiano Island" = "Porlier Pass",
                        # "E. Valdes" = "Porlier Pass",
                        "Flora Island  NE Hornby Island." = "Hornby Island",
                        "Flora Islet - Hornby Island" = "Hornby Island",
                        "Lambert Channel - Hornby Island" = "Hornby Island",
                        "Hornby island" = "Hornby Island",
                        "Salamanca Bank - Active pass" = "Active Pass",
                        "Salamanca Bank" = "Active Pass",
                        "Gabriola Island North side - Entrance Island" = "Gabriola Island",
                        "Entrance Island" = "Gabriola Island",
                        "Grants Reef" = "Grants Reefs",
                        "Grant Reef" = "Grants Reefs",
                        "Grants Reefs" = "Grants Reefs",
                        "Grant Reefs" = "Grants Reefs",
                        "Entrance Island - Gabriola North side" = "Gabriola Island",
                        "Sinclair Bank - Malaspina Strait" = "Sinclair Bank",
                        "Malaspina Strait" = "Sinclair Bank",
                        "Stillwater Bay" = "Sinclair Bank",
                        "North East Pont" = "Sinclair Bank",
                        "Entrance Island" = "Gabriola Island",
                        "Galiano Is. North" = "Active Pass",
                        "Thormanby Island" = "Epsom Point",
                        "Sandheads" = "Sturgeon Bank",
                        "Qualicum-Parksville" = "French Creek"
)

unique(dog2$location)
ggplot(dog2, aes(grouping_desc, catch_count)) +
  geom_boxplot() +
  facet_wrap(~location)

ggplot(dog2, aes(year, catch_count)) +
  geom_boxplot() +
  facet_wrap(~location)

ggplot(
  # filter(dog2, location %in% c("Sandheads", "Sturgeon Bank")),
  # filter(dog2, location %in% c("Qualicum-Parksville", "French Creek")),
  dog2,
  aes(longitude, latitude, colour = location)
) +
  geom_point() +
  geom_text(aes(label = location)) +
  # facet_wrap(~location + year, scales = "free")
  facet_wrap(~year, scales = "free")

dog2 %>%
  group_by(year) %>%
  distinct(location) %>%
  tally()

saveRDS(dog2, "output/dogfishdata.rds")
```

Names are fixed. 
```{r checknames}
dog <- readRDS("output/dogfishdata.rds")

ggplot(dog, aes(grouping_desc, catch_count)) +
  geom_boxplot() +
  facet_wrap(~location)

ggplot(
  # filter(dog2, location %in% c("Sandheads", "Sturgeon Bank")),
  # filter(dog2, location %in% c("Qualicum-Parksville", "French Creek")),
  dog,
  aes(longitude, latitude, colour = location)
) +
  geom_point() +
  geom_text(aes(label = location)) +
  # facet_wrap(~location + year, scales = "free")
  facet_wrap(~year, scales = "free")
```

Create an index using the HBLL and DOG surveys. Load DOGFISH survey data. 
```{r DOGsurveydata }

dog <- readRDS("output/dogfishdata.rds")

#years <- seq(min(dog$year), max(dog$year), 1)
years <- unique(dog$year)

dog$logbot_depth <- log(dog$depth_m)
ggplot(dog, aes(longitude, latitude, colour = location)) +
  geom_point() +
  facet_wrap(~year)
dog <- add_utm_columns(dog,
                       ll_names = c("longitude", "latitude"), units = "km",
                       utm_crs = 32609
)
mean_logbot_depth <- mean(dog$logbot_depth, na.rm = TRUE)
dog$logbot_depth_cent <- dog$logbot_depth - mean_logbot_depth
dog <- filter(dog, !is.na(depth_m))
dog <- filter(dog, !is.na(julian))
dog <- filter(dog, !is.na(dogfish_count))
dog$offset <- log(dog$lglsp_hook_count)

dog$locationf <- as.factor(dog$location)
x <- dog |>
  group_by(year, location) |>
  tally()
# dog <- dog |>
#   group_by(year, location) |>
#   mutate(tally = n()) |>
#   filter(tally >=3)
```


Create grid
```{r grid, eval = FALSE}

shelf_SOG <- st_read("data", "SOG_polygon")
plot(st_geometry(shelf_SOG), col = "red")
shelf2 <- st_transform(shelf_SOG, "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs") #+proj=utm +zone=9 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
plot(st_geometry(shelf2), col = "red")

# Create grid that covers the BC coast polygon/jurisdiction.
grid_spacing <- 2000 # 2 X 2

polygony <- st_make_grid(shelf2, square = T, cellsize = c(grid_spacing, grid_spacing)) %>%
  st_sf() %>%
  mutate(cell_ID = row_number())
plot(st_geometry(polygony))

center <- st_centroid(polygony)
# grid_extent <- st_intersection(st_geometry(shelf2), st_geometry(polygony))
grid_extent <- st_intersection(shelf2, polygony)
plot(st_geometry(grid_extent))
center2 <- st_centroid(grid_extent)

test <- st_sf(grid_extent)
test_center <- st_sf(center2)
plot(st_geometry(grid_extent))
plot(st_geometry(test_center), add = TRUE)

test$area_km <- st_area(test) / 1000000 # m to km
plot(st_geometry(test))
plot(test_center, add = TRUE, pch = 21)
st_write(test, "output/PredictionGrid_SOG.shp", append = FALSE)
st_write(test_center, "output/PredictionGridCentres_SOG.shp", append = FALSE)


# citation("PBSmapping")
library(PBSmapping)
data(bcBathymetry) # bathymetry
contour(bcBathymetry$x, bcBathymetry$y, bcBathymetry$z, col = "pink", method = "edge", vfont = c("sans serif", "plain"))

cont <- contourLines(bcBathymetry$x, bcBathymetry$y, bcBathymetry$z, nlevels = 1000)
clines <- maptools::ContourLines2SLDF(cont)
clines@data[["level"]]
lines <- st_as_sf(clines) # make sf object
plot(lines)

st_crs(lines) <- sp::CRS("+proj=longlat")
c.linesproj <- st_transform(lines, crs = "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
plot(c.linesproj)
gridarea <- st_intersection(st_geometry(shelf2), st_geometry(c.linesproj)) %>% st_sfc(crs = "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
plot(gridarea)

gridarea2 <- st_collection_extract(gridarea, "LINESTRING")
st_write(gridarea2, "output/Contours_SOG.shp", append = FALSE)
plot(gridarea2)


# Overlap the grid and the contour to get depth per grid point
bathymetry <- st_read("output/Contours_SOG.shp")
prediction_grid <- st_read("output/PredictionGrid_SOG.shp")
plot(st_geometry(prediction_grid))
prediction_center <- st_read("output/PredictionGridCentres_SOG.shp")
glimpse(prediction_center)

centerdf <- do.call(rbind, st_geometry(prediction_center)) %>%
  as_tibble() %>%
  setNames(c("X", "Y"))
centerdf$value <- seq(1, nrow(centerdf))

# lat and longs for each year to predict on
centerdf2 <- expand.grid(unique(centerdf$value), years)
names(centerdf2) <- c("value", "year")
centerdf3 <- left_join(centerdf2, centerdf, by = c("value" = "value"))
unique(centerdf3$year)

## Create the clipping polygon
maxlat <- max(centerdf$Y) # Northing, lat
minlat <- min(centerdf$Y) # northing, lat
maxlon <- max(centerdf$X) # Easting, long
minlon <- min(centerdf$X) # easting, long

b <- marmap::getNOAA.bathy(lon1 = -150, lon2 = -110, lat1 = 30, lat2 = 70, resolution = 1)

# convert center utms to lat and longs
centerlatlon <- as.data.frame(centerdf) # y is lat, x is lon
coordinates(centerlatlon) <- c("X", "Y") # c( "X", "Y")
proj4string(centerlatlon) <- CRS("+proj=utm +zone=9 +datum=WGS84")
centertrans <- spTransform(centerlatlon, CRS = "+proj=longlat + datum=WGS84")
centertrans2 <- as.data.frame(coordinates(centertrans))

names(centertrans2) <- c("lon", "lat")
# names(centerdf3) <- c("value", "year", "UTM.lon", "UTM.lat")
centertrans3 <- cbind(centerdf, centertrans2)
names(centertrans3) <- c("UTM.lon.m", "UTM.lat.m", "value", "lon", "lat")
depthpoints_center <- marmap::get.depth(b, centertrans3[, c("lon", "lat")], locator = FALSE)
depthpoints_center2 <- depthpoints_center[!duplicated(depthpoints_center), ] # not sure why I had this...maybe to calculate mean and sd
centertrans4 <- inner_join(depthpoints_center2, centertrans3, by = c("lat" = "lat", "lon" = "lon"))


# see which point are the ones that have positive depths
depth_predictiongrid <- filter(centertrans4, depth > -4)
max(depth_predictiongrid$depth)
plot(depth_predictiongrid$lon, depth_predictiongrid$lat) # prediction grid points with depth > 0
dim(depth_predictiongrid)[1] # num pred. grid points with depth > 0

# erase positive depth points
# the range of deptsh surveyed by IPHC and Tralw and Longline is 9 - 1308 meters.
centertrans5 <- filter(centertrans4, depth < -4)
glimpse(centertrans5) # value is grid cell id
plot(centertrans5$lon, centertrans5$lat) # prediction grid points with depth > 0

names(centertrans5)[3] <- "depth_m"
centertrans5$depth_m_neg <- centertrans5$depth_m
centertrans5$depth_m <- centertrans5$depth_m_neg * -1
centertrans5$logdepth <- log10(centertrans5$depth_m)
saveRDS(centertrans5, "output/predictiongrid_bccoast_SOG.rds")
```

Load prediction grid
```{r predictiongridload}

grid <- readRDS("output/predictiongrid_bccoast_SOG.rds")
ggplot(grid, aes(UTM.lon.m, UTM.lat.m, colour = depth_m)) +
  geom_point()
grid <- grid |> filter(UTM.lon.m > 750000)
grid$offset <- log(1)
max <- as.numeric(max(dog$depth_m))
min <- as.numeric(min(dog$depth_m))
grid <- grid |> filter(depth_m <= max)
grid <- grid |> filter(depth_m >= min)
grid$logbot_depth <- log(grid$depth_m)
grid$X <- grid$UTM.lon.m / 1000
grid$Y <- grid$UTM.lat.m / 1000
sort(unique(grid$year))
ggplot(grid, aes(UTM.lon.m, UTM.lat.m, colour = depth_m)) +
  geom_point()
grid$julian <- 287
grid$juliansmall = 0
grid$logbot_depth_cent <- grid$logbot_depth - mean_logbot_depth

range(dog$Y)
range(dog$X)

grid <- grid |> filter(X <= 915.7025 )
grid <- grid |> filter(Y <=5545.813 )

grid <- grid |> filter(X >= 777.9039 )
grid <- grid |> filter(Y >= 5433.850 )

```

Create mesh
```{r}

mesh <- make_mesh(dog, xy_cols = c("X", "Y"), cutoff = 20)

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
  geom_point(aes(X, Y, colour = dogfish_count, size = dogfish_count),
             data = filter(dog, dogfish_count > 0)
  ) +
  facet_wrap(~year) +
  scale_color_viridis_c(trans = "fourth_root_power")
```


```{r indexmodelDOG}

#dog$juliansmall <- dog$julian - min(dog$julian)
#dog <- filter(dog, year > 2000)

unique(sort(dog$year))
dog |>
  ggplot() +
  geom_point(aes(year, dogfish_count)) 

sum(dog$dogfish_count ==0) # 

dog |>
  group_by(year) |>
  mutate(sum = sum(dogfish_count)) |>
  ggplot() +
  geom_point(aes(year, sum)) +
  geom_line(aes(year, sum))

dog |>
  group_by(year) |>
  mutate(cpue = sum(dogfish_count) / sum(lglsp_hook_count)) |>
  ggplot() +
  geom_point(aes(year, cpue)) +
  geom_line(aes(year, cpue))
range(dog$dogfish_count)
range(dog$offset)
range(dog$year)
dog <- droplevels(dog)
dog <- dog |> 
  mutate(dogfish_count = ifelse(year %in% c(1986, 1989), dogfish_count * 1.6, dogfish_count))
dog$smalloffset <- log(dog$hook_count/10)

t <- filter(dog, gear_type)
hist(dog$offset)

#do the calibration in the model (2004) 
#include every row of data gets a gear type
#colum of gear type
#include geartype in the model 
#different catcability for each geartype
#a ratio of catchability
#predict on a geartype.....
#for prediction grid
gfplot::dogfish_grid
ggplot(dogfish_grid$grid, aes(X, Y )) + geom_point()
ggplot(dog, aes(X, Y)) + geom_point()

mind <- sdmTMB(
  formula = dogfish_count ~  1 + as.factor(year) + juliansmall, # + logbot_depth_cent + I(logbot_depth_cent^2) + juliansmall + locationf ,
  offset = dog$offset,
  data = dog,
  mesh = mesh, 
  spatiotemporal = "off", 
  #spatiotemporal = FALSE, 
  #spatiotemporal = "IID",
  time = "year",
  #extra_time = c(2006, 2007, 2009, 2010, 2012, 2013, 2015, 2016, 2017, 2018),
  silent = FALSE,
  family = Gamma(link = "log"), #compare posson family (count dis with no dispersion, mean = variance), create new column that is unique for each observation, then create a random effect , same number of parameter, can compare AIC with Nbinom2, ( 1|obs) creates one sd allows for more variability dispersion in he data, 
  spatial = TRUE)

sanity(mind)
mind$sd_report
saveRDS(mind, "output/DOGmodel.rds")

#years <- seq(min(dog$year), max(dog$year), 1)
years <- unique(dog$year)
grid <- grid |> select(-year) |> distinct()
grid <- purrr::map_dfr(as.numeric(years), ~ tibble(dogfish_grid$grid, year = .x))
grid$juliansmall <- 0
grid$offset <- 0
#grid$locationf <- "Cape Mudge"
pred <- predict(mind, newdata = grid, return_tmb_object = TRUE)
index <- get_index(pred, area = 4, bias_correct = TRUE)

# ggplot(grid, aes(X, Y)) + geom_point()
# 
ggplot(index, aes(year, est)) +
  geom_line(col = "#8D9999") +
  geom_point(col = "#8D9999") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#8D9999") +
  theme_classic()

```
