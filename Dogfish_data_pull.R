
#Code for creating one database of all Dogfish surveys including comparisons, j hooks, and dogfish surveys
#Note
#SURVEY_SERIES_ID == 48) #2019 survey since it was 2 different sets with different hooks
#SURVEY_SERIES_ID == 93) # 2005 onwards dogfish survey
#SURVEY_SERIES_ID == 76) # 1986 onwards dogfish survey DROP THIS ONE
#SURVEY_SERIES_ID == 92) # 1986, 1989 survey 

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



# Pulls ALL dogfish survey samples and sets --------------------------------------------------------------

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

#merge databases
names(info) <- tolower(names(info))
names(dsurvey_bio) <- tolower(names(dsurvey_bio))
info <- info |> rename("FEI" = "fishing_event_id")

regsurveys <- dsurvey_bio |> 
  filter(survey_series_id %in% c(93, 92)) |> 
  rename("FEI" = "fishing_event_id") |> 
  inner_join(info) 

compsurveys <- dsurvey_bio |> 
  filter(survey_series_id == 48) |> 
  rename("FEI" = "fe_parent_event_id") |> 
  inner_join(info) |> 
  select(-"fishing_event_id", "fe_parent_event_id")

final <- rbind(regsurveys, compsurveys)
unique(final$grouping_desc)

saveRDS(final, "output/dogfishs_allsets_allspecies.rds")
write.csv(final, "output/dogfishs_allsets_allspecies.csv")


# Clean data (location names, dates, etc.) --------------------------------
#check depths
unique(final$grouping_desc)

final |> 
  filter(grouping_desc ==  "SoG Dogfish Site")

#fix
final <- final |> 
  mutate(grouping_desc = ifelse(depth_m <=55, "SoG Dogfish 0 - 55 m", 
                                ifelse(depth_m > 55 & depth_m <= 110, "SoG Dogfish 56 - 110 m", 
                                       ifelse(depth_m > 110 & depth_m <= 165, "SoG Dogfish 111 - 165 m", 
                                              ifelse(depth_m > 166 & depth_m <= 220, "SoG Dogfish 166 - 220 m", 
                                                     ifelse(depth_m > 220, "SoG Dogfish > 200 m", 
                                                            NA))))))
#check
final |> 
  filter(grouping_desc ==  "SoG Dogfish Site")
#still NAs
test <- final |> 
  filter(is.na(grouping_desc) ==  TRUE) # comment says missing depth in
unique(test$FEI) # comment says missing depth in



#check location names


# fishing event 1989, 3369551 says Gabriola but is Galiano - Active Pass
filter(final, FEI == 3369551)
# fishing event 3369552 1989 Gabriola should be Galiano
filter(final, FEI == 3369552)
final$fe_fishing_ground_comment <- ifelse(final$FEI %in% c(3369552, 3369551), "Active Pass", 
                                          final$fe_fishing_ground_comment)
# fishing event 1986 3369471 Porlier Pass is way outside of the box
filter(final, FEI == 3369471)
final <- filter(final, FEI != 3369471)
# fishing event mid straits - not sure if that is Cape Lazo or Grants Reef, fishing event 3369532
filter(final, fe_fishing_ground_comment == "Mid Straits")
final <- filter(final, fe_fishing_ground_comment != "Mid Straits")

# Names have different spellings etc. FIX
vec <- (final$fe_fishing_ground_comment)
unique(vec) #should only be 14 sites max!

final$fe_fishing_ground_comment <- recode(vec,
                                      "Salamanca Bank - Active pass" = "Active Pass",
                                      "Salamanca Bank" = "Active Pass",
                                      "White Isle - Halibut Bank" = "Halibut Banks",
                                      "White Isle" = "Halibut Banks",
                                      "While Isle - Halibut Bank" = "Halibut Banks",
                                      "Halibut Bank" = "Halibut Banks",
                                      "Gabriola Island North side - Entrance Island" = "Entrance Island",
                                      "Gabriola Island" = "Entrance Island",
                                      "Entrance Island - Gabriola North side" = "Entrance Island",
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
                                      "Epsom Point" = "Espon Point",
                                      "SoG Dogfish Site Epsom Point" = "Espon Point",
                                      "West Cape Lazo" = "Cape Lazo",
                                      "White Isle - Halibut Bank" = "Halibut Banks",
                                      "White Isle" = "Halibut Banks",
                                      "Halibut Bank" = "Halibut Banks",
                                      "While Isle - Halibut Bank" = "Halibut Banks",
                                      "Polier Pass" = "Porlier Pass",
                                      "E. Valdes" = "Porlier Pass",
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
                                      "Gabriola Island North side - Entrance Island" = "Entrance Island",
                                      "Grants Reef" = "Grants Reefs",
                                      "Grant Reef" = "Grants Reefs",
                                      "Grants Reefs" = "Grants Reefs",
                                      "Grant Reefs" = "Grants Reefs",
                                      "Entrance Island - Gabriola North side" = "Entrance Island",
                                      "Sinclair Bank - Malaspina Strait" = "Sinclair Bank",
                                      "Malaspina Strait" = "Sinclair Bank",
                                      "Stillwater Bay" = "Sinclair Bank",
                                      "North East Pont" = "Sinclair Bank",
                                      "Galiano Is. North" = "Active Pass",
                                      "Thormanby Island" = "Espon Point",
                                      "Sandheads" = "Sturgeon Bank",
                                      "Qualicum-Parksville" = "French Creek", 
                                      "SoG Dogfish Site Sinclair Bank" = "Sinclair Bank", 
                                      "SoG Dogfish Site Sturgeon Bank" = "Sturgeon Bank", 
                                      "SoG Dogfish Site Active Pass" = "Active Pass", 
                                      "SoG Dogfish Site Porlier Pass" = "Porlier Pass",    
                                      "SoG Dogfish Site Entrance Island" = "Entrance Island"
)

vec2 <- unique(final$fe_fishing_ground_comment)
unique(vec2)

ggplot(final, aes(longitude, latitude, colour = fe_fishing_ground_comment, group = fe_fishing_ground_comment)) +
  geom_point()

saveRDS(d, "output/dogfishsql_allspecies_clean.rds")


# how many depths per site per year, should be 4 or 5
d |>
  group_by(year, fe_fishing_ground_comment) |>
  distinct(grouping_desc) |>
  tally() |>
  print(n = 50)

# how many sites fished
names <- d |>
  group_by(year) |>
  distinct(fe_fishing_ground_comment) |>
  tally()


# Summary figures ---------------------------------------------------------
final |> 
  filter(grouping_desc !=  "SoG Dogfish Site") |>  
  ggplot() +
  geom_boxplot(aes((grouping_desc), total_length,
                   colour = as.factor(hooksize_desc)
  )) +
  #geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~specimen_sex_code, nrow = 2) +
  theme_classic()

final |> 
  filter(grouping_desc !=  "SoG Dogfish Site") |>  
  filter(specimen_sex_code %in% c(1,2)) |> 
  ggplot() +
  geom_boxplot(aes((grouping_desc), total_length,
                   colour = as.factor(hooksize_desc)
  )) +
  #geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~specimen_sex_code + year, nrow = 2) +
  theme_classic()

final |> 
  filter(grouping_desc !=  "SoG Dogfish Site") |>  
  filter(specimen_sex_code %in% c(1,2)) |> 
  ggplot() +
  geom_boxplot(aes(as.factor(year), total_length,
                   colour = as.factor(hooksize_desc)
  )) +
  #geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~grouping_desc + specimen_sex_code, nrow = 2) +
  theme_classic()
