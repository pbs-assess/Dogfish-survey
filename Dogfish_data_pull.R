
# Code for creating one database of all Dogfish surveys including comparisons, j hooks, and dogfish surveys
# Note
# SURVEY_SERIES_ID == 48) #2019 survey since it was 2 different sets with different hooks
# SURVEY_SERIES_ID == 93) # 2005 onwards dogfish survey
# SURVEY_SERIES_ID == 76) # 1986 onwards dogfish survey DROP THIS ONE
# SURVEY_SERIES_ID == 92) # 1986, 1989 survey

# library -----------------------------------------------------------------
library(sf)
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(sf)
library(sdmTMB)
library(here)
library(gfdata)


# Pull dogfish survey samples and sets --------------------------------------------------------------

info <- gfdata::run_sql("GFBioSQL", "SELECT
S.SURVEY_SERIES_ID,
SURVEY_SERIES_DESC, FE.FE_MISC_COMMENT, FE.FE_FISHING_GROUND_COMMENT,
S.SURVEY_ID, SURVEY_DESC, FE.FE_MAJOR_LEVEL_ID, SK.FE_SUB_LEVEL_ID,
SK.HOOK_DESC, SK.HOOKSIZE_DESC,
YEAR(TR.TRIP_START_DATE) AS YEAR,
FE.FISHING_EVENT_ID,
FE.FE_PARENT_EVENT_ID,
FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
-(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS LONGITUDE,
FE_END_DEPLOYMENT_TIME, FE_BEGIN_RETRIEVAL_TIME, FE.GROUPING_CODE, GROUPING_DESC, GROUPING_DEPTH_ID,
TR.TRIP_START_DATE,
TR.TRIP_END_DATE,
FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M,
FE.TRIP_ID
FROM FISHING_EVENT FE
LEFT JOIN (
  SELECT TRIP_ID, FE.FISHING_EVENT_ID, FE_MAJOR_LEVEL_ID, FE_SUB_LEVEL_ID, H.HOOK_DESC, HSZ.HOOKSIZE_DESC
  FROM FISHING_EVENT FE
  INNER JOIN LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
  LEFT JOIN HOOK H ON H.HOOK_CODE = LLSP.HOOK_CODE
  LEFT JOIN HOOKSIZE HSZ ON HSZ.HOOKSIZE_CODE = LLSP.HOOKSIZE_CODE
) SK ON SK.TRIP_ID = FE.TRIP_ID AND SK.FE_MAJOR_LEVEL_ID = FE.FE_MAJOR_LEVEL_ID
INNER JOIN TRIP_SURVEY TS ON FE.TRIP_ID = TS.TRIP_ID
INNER JOIN TRIP TR ON FE.TRIP_ID = TR.TRIP_ID
INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
INNER JOIN SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
LEFT JOIN GROUPING G ON G.GROUPING_CODE = FE.GROUPING_CODE
WHERE S.SURVEY_SERIES_ID IN (48, 76, 92, 93)
AND FE.FE_MAJOR_LEVEL_ID < 1000 AND FE_PARENT_EVENT_ID IS NULL
ORDER BY YEAR, TRIP_ID, FE_MAJOR_LEVEL_ID, FE_SUB_LEVEL_ID")
saveRDS(info, "output/dogfish_sets.rds")

dsurvey_bio <- gfdata::run_sql("GFBioSQL", "SELECT
A.ACTIVITY_DESC,
FE_SUB_LEVEL_ID,
SS.SURVEY_SERIES_ID,
FE_PARENT_EVENT_ID,
YEAR(B21.TRIP_START_DATE) AS YEAR,
B21.TRIP_COMMENT,
FISHING_EVENT_ID,
B21.TRIP_ID,
B21.FE_MAJOR_LEVEL_ID,
B21.SPECIES_CODE,
S.SPECIES_SCIENCE_NAME,
S.SPECIES_COMMON_NAME,
B22.SPECIMEN_ID,
B22.SPECIMEN_SEX_CODE,
B22.Total_Length,
B22.Round_Weight
FROM GFBioSQL.dbo.B21_Samples B21
INNER JOIN GFBioSQL.dbo.B22_Specimens B22 ON B22.SAMPLE_ID = B21.SAMPLE_ID
INNER JOIN GFBioSQL.dbo.TRIP_ACTIVITY TA ON TA.TRIP_ID = B21.TRIP_ID
INNER JOIN TRIP_SURVEY TS ON B21.TRIP_ID = TS.TRIP_ID
INNER JOIN TRIP TR ON TS.TRIP_ID = TR.TRIP_ID
INNER JOIN SURVEY SR ON TS.SURVEY_ID = SR.SURVEY_ID
INNER JOIN SURVEY_SERIES SS ON SR.SURVEY_SERIES_ID = SS.SURVEY_SERIES_ID
INNER JOIN GFBioSQL.dbo.ACTIVITY A ON A.ACTIVITY_CODE = TA.ACTIVITY_CODE
INNER JOIN GFBioSQL.dbo.SPECIES S ON S.SPECIES_CODE = B21.SPECIES_CODE
WHERE TA.ACTIVITY_CODE IN (39,53)
ORDER BY B21.TRIP_ID, B21.FE_MAJOR_LEVEL_ID, B22.SPECIMEN_ID")
saveRDS(dsurvey_bio, "output/dogfish_samples.rds")


# QA/QC data -------------------------------------------------------------
samples <- readRDS("output/dogfish_samples.rds")
sets <- readRDS("output/dogfish_sets.rds")

names(samples) <- tolower(names(samples))
names(sets) <- tolower(names(sets))

# FIX LOCATION NAMES
# overlay data locations with polygons
sites <- st_read("C:/Dogfish surveys 2023/dogfish sites shp", "dogfish_polygons_noproj2")
plot(st_geometry(sites), col = "red")
site_name <- unique(sites$site_name)
df <- data.frame(cbind(site_name, site_gis = c(
  "Ajax Exeter", "Active Pass", "Grants Reef", "Halibut Bank", "Sturgeon Bank",
  "Oyster River", "Epsom Point", "Sinclair Bank", "Porlier Pass", "Cape Mudge", "French Creek",
  "Cape Lazo", "Entrance Island", "Hornby Island"
)))
sites <- left_join(sites, df)
sites <- sites |> select(site_gis)
# convert center utms to lat and longs
finalsp <- sets
coordinates(finalsp) <- c("longitude", "latitude")
proj4string(finalsp) <- CRS("+proj=longlat + datum=WGS84")
finalsp <- st_as_sf(finalsp)
finalsp2 <- finalsp %>%
  mutate(
    latitude = unlist(purrr::map(finalsp$geometry, 2)),
    longitude = unlist(purrr::map(finalsp$geometry, 1))
  )
ptsint <- st_join(sites, finalsp2)
ptsint
st_geometry(ptsint) <- NULL

# a couple points fall outside of the polygons
missing <- anti_join(sets, ptsint)
missing |>
  select(fishing_event_id, year, fe_fishing_ground_comment) |>
  distinct(.keep_all = TRUE)
p1 <- ggplot(ptsint, aes(longitude, latitude)) +
  geom_point()
p1 + geom_point(data = missing, aes(longitude, latitude), col = "red")
# everything except this point seems ok
p1 + geom_point(data = filter(missing, fishing_event_id == 3369471), aes(longitude, latitude), col = "red")
add <- filter(missing, fishing_event_id != 3369471)
add$site_gis <- add$fe_fishing_ground_comment
final3 <- rbind(add, ptsint)

ggplot(final3, aes(longitude, latitude, colour = site_gis, group = fe_fishing_ground_comment)) +
  geom_point()


# clean names based on names - erase? -------------------------------------

# ###or check names and manually change.
# # fishing event 1989, 3369551 says Gabriola but is Galiano - Active Pass
# filter(final, FEI == 3369551)
# # fishing event 3369552 1989 Gabriola should be Galiano
# filter(final, FEI == 3369552)
# final$fe_fishing_ground_comment <- ifelse(final$FEI %in% c(3369552, 3369551), "Active Pass",
#                                           final$fe_fishing_ground_comment)
# # fishing event 1986 3369471 Porlier Pass is way outside of the box
# filter(final, FEI == 3369471)
# final <- filter(final, FEI != 3369471)
# # fishing event mid straits - not sure if that is Cape Lazo or Grants Reef, fishing event 3369532
# filter(final, fe_fishing_ground_comment == "Mid Straits")
# final <- filter(final, fe_fishing_ground_comment != "Mid Straits")
#
# # Names have different spellings etc. FIX
# vec <- (final$fe_fishing_ground_comment)
# unique(vec) #should only be 14 sites max!
#
# final$fe_fishing_ground_comment <- recode(vec,
#                                       "Salamanca Bank - Active pass" = "Active Pass",
#                                       "Salamanca Bank" = "Active Pass",
#                                       "White Isle - Halibut Bank" = "Halibut Banks",
#                                       "White Isle" = "Halibut Banks",
#                                       "While Isle - Halibut Bank" = "Halibut Banks",
#                                       "Halibut Bank" = "Halibut Banks",
#                                       "Gabriola Island North side - Entrance Island" = "Entrance Island",
#                                       "Gabriola Island" = "Entrance Island",
#                                       "Entrance Island - Gabriola North side" = "Entrance Island",
#                                       "Dettwiller Point - Galiano Island North side." = "Active Pass",
#                                       "Dettwiller Point Galiano Island North." = "Active Pass",
#                                       "Dettwiller Point - Galiano Island" = "Active Pass",
#                                       "Sinclair Bank - Malaspina Strait" = "Sinclair Bank",
#                                       "Malaspina Strait" = "Sinclair Bank",
#                                       "Stillwater Bay" = "Sinclair Bank",
#                                       "North East Pont" = "Sinclair Bank",
#                                       "Flora Island  NE Hornby Island." = "Hornby Island",
#                                       "Flora Islet - Hornby Island" = "Hornby Island",
#                                       "Lambert Channel - Hornby Island" = "Hornby Island",
#                                       "Hornby island" = "Hornby Island",
#                                       "Espom Point" = "Espon Point",
#                                       "Epson Point" = "Espon Point",
#                                       "Epsom Point" = "Espon Point",
#                                       "SoG Dogfish Site Epsom Point" = "Espon Point",
#                                       "West Cape Lazo" = "Cape Lazo",
#                                       "White Isle - Halibut Bank" = "Halibut Banks",
#                                       "White Isle" = "Halibut Banks",
#                                       "Halibut Bank" = "Halibut Banks",
#                                       "While Isle - Halibut Bank" = "Halibut Banks",
#                                       "Polier Pass" = "Porlier Pass",
#                                       "E. Valdes" = "Porlier Pass",
#                                       "Dettwiller Point Galiano Island North." = "Porlier Pass",
#                                       "Dettwiller Point - Galiano Island North side." = "Porlier Pass",
#                                       "Dettwiller Point - Galiano Island" = "Porlier Pass",
#                                       # "E. Valdes" = "Porlier Pass",
#                                       "Flora Island  NE Hornby Island." = "Hornby Island",
#                                       "Flora Islet - Hornby Island" = "Hornby Island",
#                                       "Lambert Channel - Hornby Island" = "Hornby Island",
#                                       "Hornby island" = "Hornby Island",
#                                       "Salamanca Bank - Active pass" = "Active Pass",
#                                       "Salamanca Bank" = "Active Pass",
#                                       "Gabriola Island North side - Entrance Island" = "Entrance Island",
#                                       "Grants Reef" = "Grants Reefs",
#                                       "Grant Reef" = "Grants Reefs",
#                                       "Grants Reefs" = "Grants Reefs",
#                                       "Grant Reefs" = "Grants Reefs",
#                                       "Entrance Island - Gabriola North side" = "Entrance Island",
#                                       "Sinclair Bank - Malaspina Strait" = "Sinclair Bank",
#                                       "Malaspina Strait" = "Sinclair Bank",
#                                       "Stillwater Bay" = "Sinclair Bank",
#                                       "North East Pont" = "Sinclair Bank",
#                                       "Galiano Is. North" = "Active Pass",
#                                       "Thormanby Island" = "Espon Point",
#                                       "Sandheads" = "Sturgeon Bank",
#                                       "Qualicum-Parksville" = "French Creek",
#                                       "SoG Dogfish Site Sinclair Bank" = "Sinclair Bank",
#                                       "SoG Dogfish Site Sturgeon Bank" = "Sturgeon Bank",
#                                       "SoG Dogfish Site Active Pass" = "Active Pass",
#                                       "SoG Dogfish Site Porlier Pass" = "Porlier Pass",
#                                       "SoG Dogfish Site Entrance Island" = "Entrance Island"
# )
#
# vec2 <- unique(final$fe_fishing_ground_comment)
# unique(vec2)


# QA/QC dates --------------------------------
sets <- final3
# check depths
unique(sets$grouping_desc)

sets |>
  filter(grouping_desc == "SoG Dogfish Site")

# fix
sets <- sets |>
  mutate(grouping_desc = ifelse(depth_m <= 55, "SoG Dogfish 0 - 55 m",
    ifelse(depth_m > 55 & depth_m <= 110, "SoG Dogfish 56 - 110 m",
      ifelse(depth_m > 110 & depth_m <= 165, "SoG Dogfish 111 - 165 m",
        ifelse(depth_m > 166 & depth_m <= 220, "SoG Dogfish 166 - 220 m",
          ifelse(depth_m > 220, "SoG Dogfish > 200 m",
            NA
          )
        )
      )
    )
  ))
# check
sets |>
  filter(grouping_desc == "SoG Dogfish Site")

# still NAs - WHY
test <- sets |>
  filter(is.na(grouping_desc) == TRUE) # comment says missing depth in
unique(test$fishing_event_id) # comment says missing depth in
sets <- filter(sets, fishing_event_id != 3420654)

sets |>
  group_by(year, site_gis) |>
  distinct(grouping_desc) |>
  tally() |>
  print(n = 50)

# how many sites fished
sets |>
  group_by(year) |>
  distinct(site_gis) |>
  tally()

# CALCULATE SOAK TIME -----------------------------------------------------
glimpse(sets$fe_end_deployment_time)
d <- sets |>
  mutate(
    # deploy = as.Date(fe_end_deployment_time, format = "%Y-%m-%d hh:mm:ss"),
    deployhr = lubridate::hour(fe_end_deployment_time),
    deploymin = lubridate::minute(fe_end_deployment_time),
    retrive = as.Date(fe_begin_retrieval_time, format = "%Y-%m-%d h:m:s"),
    retrivehr = lubridate::hour(fe_begin_retrieval_time),
    retrievemin = lubridate::minute(fe_begin_retrieval_time)
  ) |>
  mutate(
    hr_diff = (retrivehr - deployhr) * 60,
    min_diff = abs(retrievemin - deploymin),
    soak = hr_diff + min_diff
  )

saveRDS(d, "output/dogfishs_allsets_allspecies_clean.rds")

# MERGE SETS AND SAMPLES ---------------------------------------------------------
sets <- readRDS("output/dogfishs_allsets_allspecies_clean.rds")
samples <- readRDS("output/dogfish_samples.rds")

names(sets) <- tolower(names(sets))
names(samples) <- tolower(names(samples))
sets <- sets |> rename("FEI" = "fishing_event_id")

regsurveys <- samples |>
  filter(survey_series_id %in% c(93, 92)) |>
  rename("FEI" = "fishing_event_id") |>
  inner_join(sets)

compsurveys <- samples |>
  filter(survey_series_id == 48) |>
  rename("FEI" = "fe_parent_event_id") |>
  inner_join(sets) |>
  select(-"fishing_event_id", "fe_parent_event_id")

final <- rbind(regsurveys, compsurveys)
unique(final$grouping_desc)

saveRDS(final, "output/dogfishs_allsets_allspecies.rds")



# SUMMARY FIGURES ---------------------------------------------------------
d <- readRDS("output/dogfishs_allsets_allspecies.rds")

d |>
  ggplot() +
  geom_point(aes(year, soak)) +
  theme_classic()

d |>
  filter(species_code == "027") |> 
  filter(specimen_sex_code %in% c(1, 2)) |>
  ggplot() +
  geom_jitter(aes((grouping_depth_id), total_length,
                   colour = as.factor(hooksize_desc)
  )) +
  # geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~ specimen_sex_code + year, nrow = 2) +
  theme_classic()

d |>
  filter(species_code == "044") |> 
  filter(specimen_sex_code %in% c(1, 2)) |>
  ggplot() +
  geom_boxplot(aes((grouping_depth_id), total_length,
    colour = as.factor(hooksize_desc)
  )) +
  # geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~ specimen_sex_code + year, nrow = 2) +
  theme_classic()

d |>
  filter(species_code == "044") |> 
  filter(specimen_sex_code %in% c(1, 2)) |>
  ggplot() +
  geom_boxplot(aes(as.factor(year), total_length,
    colour = as.factor(hooksize_desc)
  )) +
  # geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~ grouping_desc + specimen_sex_code, nrow = 2) +
  theme_classic()
