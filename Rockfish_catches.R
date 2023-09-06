#Rockfish catches in 1986, and 1989


# library -----------------------------------------------------------------
library(sf)
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(remotes)
library(devtools) # load package
# remotes::install_github("pbs-assess/sdmTMB")
library(sdmTMB)
library(ggsidekick) # for fourth_root_power_trans and theme_sleek
library(patchwork)
library(here)
library(INLA)
# devtools::install_github("pbs-assess/gfplot")
library(ngfsnp)
# library(gfplot)
library(gfdata)
library(stringr)
library(lme4)
here()
theme_set(ggsidekick::theme_sleek())




# sql all species ------------------------------------------------------------

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

d <- dplyr::left_join(fe, all_catch)
saveRDS(d, "output/dogfishsql_allspecies.rds")

d <- filter(d, year %in% c(1986, 1989))
saveRDS(d, "output/dogfishsql_80sets_allspecies.rds")


# Summary statistics and checks -------------------------------------------

d <- readRDS("output/dogfishsql_80sets_allspecies.rds")
d <- readRDS("output/dogfishsql_allspecies.rds")

#fishing event 1989, 3369551 says Gabriola but is Galiano - Active Pass
filter(d, fishing_event_id == 3369551)
#fishing event 3369552 1989 Gabriola should be Galiano
filter(d, fishing_event_id == 3369552)
d$fe_fishing_ground_comment <- ifelse(d$fishing_event_id %in% c(3369552, 3369551), "Active Pass", d$fe_fishing_ground_comment )
#fishing event 1986 3369471 Porlier Pass is way outside of the box
filter(d, fishing_event_id == 3369471)
d <- filter(d, fishing_event_id != 3369471)
#fishing event mid straits - not sure if that is Cape Lazo or Grants Reef, fishing event 3369532
filter(d, fe_fishing_ground_comment == "Mid Straits")
d <- filter(d, fe_fishing_ground_comment != "Mid Straits")

#Names have different spellings etc. FIX
vec <- (d$fe_fishing_ground_comment)
Epson Point
d$fe_fishing_ground_comment <- recode(vec,
                        "Salamanca Bank - Active pass" = "Active Pass",
                        "Salamanca Bank" = "Active Pass", 
                        "White Isle - Halibut Bank" = "Halibut Banks",
                        "White Isle" = "Halibut Banks",
                        "While Isle - Halibut Bank" = "Halibut Banks",
                        "Halibut Bank" = "Halibut Banks",
                        "Gabriola Island North side - Entrance Island" = "Gabriola Island",
                        "Entrance Island" = "Gabriola Island",
                        "Entrance Island - Gabriola North side" = "Gabriola Island",
                        "Entrance Island" = "Gabriola Island",
                        "Dettwiller Point - Galiano Island North side." = "Active Pass", 
                        "Dettwiller Point Galiano Island North." = "Active Pass",
                        "Dettwiller Point - Galiano Island" = "Active Pass",
                        "Sinclair Bank - Malaspina Strait" = "Sinclair Bank",
                        "Malaspina Strait" = "Sinclair Bank",
                        "Stillwater Bay" = "Sinclair Bank",
                        "North East Pont" = "Sinclair Bank",
                        "Flora Island  NE Hornby Island." = "Hornby Island",
                        "Flora Islet - Hornby Island" = "Hornby Island",
                        "Lambert Channel - Hornby Island" = "Hornby Island",
                        "Hornby island" = "Hornby Island", 
                        "Espom Point" = "Espon Point", 
                        "Epson Point" = "Espon Point",
                         "Entrance Island" = "Gabriola Island", 
                        "Epsom Point" = "Espon Point",
                        "SoG Dogfish Site Epsom Point" = "Espon Point",
                        "West Cape Lazo" = "Cape Lazo",
                        "White Isle - Halibut Bank" = "Halibut Banks",
                        "White Isle" = "Halibut Banks",
                        "Halibut Bank" = "Halibut Banks",
                        "While Isle - Halibut Bank" = "Halibut Banks",
                        "Polier Pass" = "Porlier Pass",
                        "E. Valdes"  = "Porlier Pass",
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
                        "Thormanby Island" = "Espon Point",
                        "Sandheads" = "Sturgeon Bank",
                        "Qualicum-Parksville" = "French Creek"
)

vec2 <- unique(d$fe_fishing_ground_comment)               
vec2                        

ggplot(d, aes(longitude, latitude, colour = fe_fishing_ground_comment, group = fe_fishing_ground_comment)) + 
  geom_point()
saveRDS(d, "output/dogfishsql_allspecies_clean.rds")
#saveRDS(d, "output/dogfishsql_80sets_allspecies_clean.rds")
#write.csv(d, "output/dogfishsql_80sets_allspecies.csv")


#how many depths per site per year, should be 4 or 5
d |> 
  group_by(year, fe_fishing_ground_comment) |> 
  distinct(grouping_desc) |> 
  tally() |> 
  print (n = 50)

#how many sites fished
names <- d |> 
  group_by(year) |> 
  distinct(fe_fishing_ground_comment) |> 
  tally()


# Catch of Rockfish by depth by year --------------------------------------

#d <- readRDS("output/dogfishsql_80sets_allspecies_clean.rds")
d <- readRDS("output/dogfishsql_allspecies_clean.rds")
scodes <- read.csv("data/species_codes.csv")

#species captured
unique(d$species_code)


#missing names for species codes - complete later
species_code <- filter(d, species_code %in% c("038", "051", "3J0", "4GA", "459", "499", "602", "225")) 
species_name <-  c("brown cat shark", "skates", "anthozoa", "starfish", "greenlings", 
"buffalo sculpin", "arrowtooth flounder", "pacific hake" )
species_latin <- NA
x <- data.frame(species_code = unique(species_code[, "species_code"]), species_name, species_latin)
scodes2 <- rbind(scodes, x)
str(scodes2)

#join database
d <- left_join(d, scodes2)
#drop the species without codes ###COME BACK TO  THIS  THOUGH

#filter for rockfishes
drock <- filter(d, grepl("rockfish", species_name, ignore.case = TRUE))
saveRDS(drock, "output/dogfishsql_rockfish.rds")



# Rockfish analysis -------------------------------------------------------

drock <- readRDS("output/dogfishsql_rockfish.rds")

#how many rockfish captured each year
drock |> 
  group_by(year, species_name) |> 
  summarize(sitesum = sum(catch_count)) |> 
  print() 

#how many rockfish catured at each depth
ddepth <- drock |> 
  mutate(grouping_depth_id = as.numeric(grouping_depth_id)) |> 
  group_by(year, species_name, grouping_depth_id) |> 
  summarize(depthsum = sum(catch_count)) 

ggplot(ddepth) + 
  geom_point(aes(grouping_depth_id, depthsum, colour = species_name)) + 
  geom_line(aes(grouping_depth_id, depthsum, colour = species_name)) + 
  facet_wrap(~year) + theme_classic()



# dogfish -----------------------------------------------------------------
ddog <- filter(d, grepl("dogfish", species_name, ignore.case = TRUE))

#how many rockfish captured each year
ddog |> 
  group_by(year, species_name) |> 
  summarize(sitesum = sum(catch_count)) |> 
  print() 

#how many rockfish catured at each depth
ddepth <- ddog |> 
  mutate(grouping_depth_id = as.numeric(grouping_depth_id)) |> 
  group_by(year, species_name, grouping_depth_id) |> 
  summarize(depthsum = sum(catch_count)) 

ggplot(ddepth) + 
  geom_point(aes(grouping_depth_id, depthsum, colour = species_name)) + 
  geom_line(aes(grouping_depth_id, depthsum, colour = species_name)) + 
  facet_wrap(~year) + theme_classic()





# sixgill -----------------------------------------------------------------
dsix <- filter(d, grepl("sixgill", species_name, ignore.case = TRUE))

#how many rockfish captured each year
dsix |> 
  group_by(year, species_name) |> 
  summarize(sitesum = sum(catch_count)) |> 
  print() 

#how many rockfish catured at each depth
ddepth <- dsix |> 
  mutate(grouping_depth_id = as.numeric(grouping_depth_id)) |> 
  group_by(year, species_name, grouping_depth_id) |> 
  summarize(depthsum = sum(catch_count)) 



