---
  title: "06_SOG_Dogfish_analysis"
author: "davidson"
date: "1/10/2022"
output: html_document
---
  
  
  ```{r}

library(sf)
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(remotes)
library(devtools) # load package
#remotes::install_github("pbs-assess/sdmTMB")
library(sdmTMB)
library(ggsidekick) # for fourth_root_power_trans and theme_sleek
library(patchwork)
library(here)
library(INLA)
#devtools::install_github("pbs-assess/gfplot")
library(ngfsnp)
#library(gfplot)
library(gfdata)
library(stringr)
library(lme4)
here()
theme_set(ggsidekick::theme_sleek())

```


# Retrieve survey data
```{r data, eval=F}

here()
data_surveysamples <- readRDS( "C:/Dogfish_stitch/output/data_survey_samplesSOG.rds")
data_surveysets <- readRDS("C:/Dogfish_stitch/output/data_surveysetsSOG.rds")

unique(data_surveysets$survey_abbrev)
test <- filter(data_surveysets, survey_abbrev == "OTHER")
hbtest <- filter(data_surveysets, year == 2019)
hbtest <- filter(data_surveysets, survey_desc == "2019 Hard Bottom Longline Inside North")
#hbtest <- filter(data_surveysets, year == 2018)
#hbtest <- filter(data_surveysets, survey_desc == "2019 Hard Bottom Longline Inside South")

unique(hbtest$survey_abbrev)
unique(hbtest$survey_desc)
#data_surveysets <-get_survey_sets(ssid = c(39 ,40, 45, 76), species = "north pacific spiny dogfish")
#unique(data_surveysets$survey_abbrev)
# 
# data_survey_samples <-get_survey_samples(ssid = c(39 ,40, 45, 76), species = "north pacific spiny dogfish")
# unique(data_survey_samples$survey_abbrev)
# 
# saveRDS(data_survey_samples, "output/data_survey_samplesSOG.rds")
#saveRDS(data_surveysets, "output/data_surveysetsSOG.rds")

```


#strait of georgia dogfish survey
Calibration survey  - what year was that
Hook code - is one of them in there, hook code designates if it was circle or j hook, newers years are 1 and the others are 3, and one year that has both (that is the experimental year). If I look at the report, 
```{r}
?dogfish_grid
gfplot::run_sql$cell_area
dogfishgrid <- gfplot::dogfish_grid$grid
plot(dogfishgrid)

fe <- gfdata::run_sql("GFBioSQL", "SELECT
  S.SURVEY_SERIES_ID,
  SURVEY_SERIES_DESC,
  S.SURVEY_ID,
  SURVEY_DESC,
  YEAR(FE_BEGIN_RETRIEVAL_TIME) YEAR,
  FE.FISHING_EVENT_ID,
  FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
  -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS LONGITUDE,
  FE_END_DEPLOYMENT_TIME,
  FE_BEGIN_RETRIEVAL_TIME,
  FE.GROUPING_CODE,
  GROUPING_DESC,
  GROUPING_DEPTH_ID,
  FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M,
  FE_BOTTOM_WATER_TEMPERATURE,
  LLSP.HOOK_CODE,
  HOOK_DESC,
  SKATE_COUNT,
  LGLSP_HOOK_COUNT,
  0.0024384 * 0.009144 * LGLSP_HOOK_COUNT AS AREA_SWEPT_KM2 --0.024384 = 8 ft hook spacing (to calculate line length with hook count); 0.009144 = 2*gangion length (= area width)
  FROM FISHING_EVENT FE
  INNER JOIN TRIP_SURVEY TS ON FE.TRIP_ID = TS.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  INNER JOIN SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
  INNER JOIN LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
  INNER JOIN GROUPING G ON G.GROUPING_CODE = FE.GROUPING_CODE
  INNER JOIN HOOK H ON H.HOOK_CODE = LLSP.HOOK_CODE
  WHERE S.SURVEY_SERIES_ID IN (76)")

names(fe) <- tolower(names(fe))

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

d <- dplyr::left_join(fe, dogfish_catch, by ="fishing_event_id")
unique(d$grouping_desc)
d2019 <- filter(d, year == 2019)



hbll_dogfish_catch <- gfdata::run_sql("GFBioSQL", "SELECT
  FEC.FISHING_EVENT_ID,
  SUM(CATCH_COUNT) DOGFISH_COUNT
  FROM FISHING_EVENT_CATCH FEC
  INNER JOIN CATCH C ON C.CATCH_ID = FEC.CATCH_ID
  INNER JOIN TRIP_SURVEY TS ON TS.TRIP_ID = FEC.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  WHERE C.SPECIES_CODE IN ('044') AND TS.TRIP_ID = 86251
  GROUP BY FEC.TRIP_ID,
  FEC.FISHING_EVENT_ID,
  C.SPECIES_CODE
  ORDER BY FEC.FISHING_EVENT_ID")
names(hbll_dogfish_catch) <- tolower(names(hbll_dogfish_catch))

hbllsurvey_dogsites <- gfdata::run_sql("GFBioSQL", "SELECT 
T.TRIP_ID, 
F.FE_BEGIN_RETRIEVAL_TIME, 
F.FISHING_EVENT_ID, 
F.GEAR_CODE,
F.FE_MAJOR_LEVEL_ID, 
F.GROUPING_CODE, 
GR.GROUPING_DESC,
F.FE_MISC_COMMENT,
F.FE_FISHING_GROUND_COMMENT
FROM GFBioSQL.dbo.TRIP T
INNER JOIN GFBioSQL.dbo.FISHING_EVENT F ON F.TRIP_ID = T.TRIP_ID
INNER JOIN GFBioSQL.dbo.GROUPING GR ON GR.GROUPING_CODE = F.GROUPING_CODE
WHERE T.TRIP_ID = 86251 AND F.GROUPING_CODE = 327
ORDER BY F.FE_MAJOR_LEVEL_ID")
names(hbllsurvey_dogsites) <- tolower(names(hbllsurvey_dogsites))

hbllsurvey_dogsites_catchinfo <- gfdata::run_sql("GFBioSQL", "SELECT
  S.SURVEY_SERIES_ID,
  SURVEY_SERIES_DESC,
  S.SURVEY_ID,
  SURVEY_DESC,
  YEAR(FE_BEGIN_RETRIEVAL_TIME) YEAR,
  FE.FISHING_EVENT_ID,
  FE_START_LATTITUDE_DEGREE + FE_START_LATTITUDE_MINUTE / 60 AS LATITUDE,
  -(FE_START_LONGITUDE_DEGREE + FE_START_LONGITUDE_MINUTE / 60) AS LONGITUDE,
  FE_END_DEPLOYMENT_TIME,
  FE_BEGIN_RETRIEVAL_TIME,
  FE.GROUPING_CODE,
  GROUPING_DESC,
  GROUPING_DEPTH_ID,
  FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M,
  FE_BOTTOM_WATER_TEMPERATURE,
  LLSP.HOOK_CODE,
  HOOK_DESC,
  SKATE_COUNT,
  LGLSP_HOOK_COUNT,
  0.0024384 * 0.009144 * LGLSP_HOOK_COUNT AS AREA_SWEPT_KM2 --0.024384 = 8 ft hook spacing (to calculate line length with hook count); 0.009144 = 2*gangion length (= area width)
  FROM FISHING_EVENT FE
  INNER JOIN TRIP_SURVEY TS ON FE.TRIP_ID = TS.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  INNER JOIN SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
  INNER JOIN LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
  INNER JOIN GROUPING G ON G.GROUPING_CODE = FE.GROUPING_CODE
  INNER JOIN HOOK H ON H.HOOK_CODE = LLSP.HOOK_CODE")
hbllsurvey_dogsites_catchinfo2 <- filter(hbllsurvey_dogsites_catchinfo, GROUPING_CODE == 327)
names(hbllsurvey_dogsites_catchinfo2) <- tolower(names(hbllsurvey_dogsites_catchinfo2))

hbllsurvey_dogsites3 <- dplyr::left_join(hbllsurvey_dogsites, hbllsurvey_dogsites_catchinfo2)


hbllsurvey_dogsites3$HBLLyn <- grepl("HBLL", hbllsurvey_dogsites3$fe_misc_comment)
augdog <- inner_join(hbllsurvey_dogsites3, hbll_dogfish_catch)
hbllsurvey_dogsitesdoggear <- filter(hbllsurvey_dogsites3, HBLLyn == FALSE)
hbllsurvey_dogsitesdoggear2 <- filter(hbllsurvey_dogsitesdoggear, !is.na(survey_series_desc))
hbllsurvey_dogsitesdoggear3 <- inner_join(hbllsurvey_dogsitesdoggear2, hbll_dogfish_catch)

names(d2019)
names(hbllsurvey_dogsitesdoggear2)
glimpse(hbllsurvey_dogsitesdoggear2)
x <- names(d2019)
hbllsurvey_dogsitesdoggear4 <- dplyr::select(hbllsurvey_dogsitesdoggear3, c(x))
both <- rbind(d2019, hbllsurvey_dogsitesdoggear4)

```




```{r 2005dogfishcomparisonsurvey_datadownload}

#2004 Dogfish Comparison study with J hooks and HBLL circle hooks
#SQL queries to get catch counts per species per hook type per


dogcomp_catch <- gfdata::run_sql("GFBioSQL", "SELECT
  FEC.FISHING_EVENT_ID, C.SPECIES_CODE,
  SUM(CATCH_COUNT) COUNT
  FROM FISHING_EVENT_CATCH FEC
  INNER JOIN CATCH C ON C.CATCH_ID = FEC.CATCH_ID
  INNER JOIN TRIP_SURVEY TS ON TS.TRIP_ID = FEC.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  WHERE TS.TRIP_ID = 85910
  GROUP BY FEC.TRIP_ID,
  FEC.FISHING_EVENT_ID,
  C.SPECIES_CODE
  ORDER BY FEC.FISHING_EVENT_ID")
names(dogcomp_catch) <- tolower(names(dogcomp_catch))
length(unique(dogcomp_catch$fishing_event_id))



dogcomp_info2004 <- gfdata::run_sql("GFBioSQL", "SELECT
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
  FE_BEGINNING_BOTTOM_DEPTH AS DEPTH_M,
  FE.TRIP_ID
  FROM FISHING_EVENT FE
  LEFT JOIN LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = FE.FISHING_EVENT_ID
  INNER JOIN TRIP_SURVEY TS ON FE.TRIP_ID = TS.TRIP_ID
  INNER JOIN SURVEY S ON S.SURVEY_ID = TS.SURVEY_ID
  INNER JOIN SURVEY_SERIES SS ON SS.SURVEY_SERIES_ID = S.SURVEY_SERIES_ID
  INNER JOIN GROUPING G ON G.GROUPING_CODE = FE.GROUPING_CODE
  WHERE FE.TRIP_ID = 85910")
names(dogcomp_info2004) <- tolower(names(dogcomp_info2004))
length(unique(dogcomp_info2004$fishing_event_id))
dogcomp_info %>% group_by(fe_fishing_ground_comment) %>% tally()



dog_longlinespecs <- gfdata::run_sql("GFBioSQL", "SELECT
  T.TRIP_ID, F.FISHING_EVENT_ID, F.FE_MAJOR_LEVEL_ID, F.FE_PARENT_EVENT_ID, F.FE_SUB_LEVEL_ID, F.FE_MINOR_LEVEL_ID,
	LLSP.HOOK_CODE
FROM TRIP T
INNER JOIN FISHING_EVENT F ON F.TRIP_ID = T.TRIP_ID
LEFT JOIN LONGLINE_SPECS LLSP ON LLSP.FISHING_EVENT_ID = F.FISHING_EVENT_ID
WHERE T.TRIP_ID = 85910
ORDER BY FE_SUB_LEVEL_ID, FE_MINOR_LEVEL_ID
")
names(dog_longlinespecs) <- tolower(names(dog_longlinespecs))
length(unique(dog_longlinespecs$fe_parent_event_id)) #23 + 1 NA unique sites and depths
length(unique(dog_longlinespecs$fishing_event_id)) #unique fishing gear events (2 per fe_parent_event_id)
test <- dog_longlinespecs %>% filter(!is.na(fe_parent_event_id)) %>% group_by(fe_parent_event_id) %>% tally()
test <- dog_longlinespecs %>% filter(!is.na(hook_code)) %>% group_by(fishing_event_id) %>% tally()
dog_longlinespecs2 <- dog_longlinespecs %>% filter(!is.na(hook_code))



x <- dog_longlinespecs2 %>% dplyr::inner_join(dogcomp_info, by = c("fe_parent_event_id" = "fishing_event_id" ))
length(unique(x$fishing_event_id))
length(unique(x$fe_parent_event_id))
anti <- dog_longlinespecs2 %>% dplyr::anti_join(dogcomp_info, by = c("fe_parent_event_id" = "fishing_event_id" ))
anti <- dogcomp_info %>% dplyr::anti_join(dog_longlinespecs2, by = c("fishing_event_id" = "fe_parent_event_id" ))


y <- filter(dogcomp_catch, species_code == "044")
length(unique(y$fishing_event_id))
dog_2004<- x %>% dplyr::inner_join(y, by = c("fe_parent_event_id" = "fishing_event_id"))
anti <- y %>% dplyr::anti_join(x, by = c("fishing_event_id" = "fe_parent_event_id" )) #45 dont match cause it is aggregated by skate (j hook and circle hook combined) and so fishing event id, and disaggregated by j-hook and circle hook


```



```{r HBLLdogfishsurvey_HBLLgear}

View(hbllsurvey_dogsites3)

x <- filter(hbllsurvey_dogsites3, !is.na(latitude) )
x$fe_fishing_ground_comment<- ifelse(is.na(x$fe_fishing_ground_comment)== TRUE, "Cape Mudge", x$fe_fishing_ground_comment)
x$HBLLyn <- grepl("HBLL", x$fe_misc_comment)
x$GearType <- ifelse(x$HBLLyn == "FALSE", "Dogfish_gear", "HBLL_gear")


hbll_dogfish_catch2 <- inner_join(x, hbll_dogfish_catch)
ggplot(hbll_dogfish_catch2, aes(depth_m, dogfish_count/lglsp_hook_count , group = as.factor(GearType), colour = as.factor(GearType))) + geom_point(size = 2) + geom_line(size = 2) +  facet_wrap(~fe_fishing_ground_comment)

```



```{r}

hblldog <- hbllsurvey_dogsitesdoggear2
ggplot(hblldog, aes(longitude,latitude)) + geom_point() + geom_label(aes(label = fishing_event_id))

plot(hblldog$longitude, hblldog$latitude)
points(d2019$longitude, d2019$latitude, col = "red")

glimpse(hblldog)
glimpse(dsurvey2019)

hblldogsf <- st_as_sf(hblldog, coords = c("longitude","latitude"))
hblldogsf <- st_set_crs(hblldogsf, 4269) 
ggplot() +
  geom_sf(data = hblldogsf, colour = "grey70", fill = "grey90") 

hblldogsf <- select(hblldogsf, survey_id, fishing_event_id ) 
#st_write(hblldogsf, "output/HBLLdogsurvey.shp", append = FALSE)


dsurvey2019sf <- st_as_sf(d2019, coords = c("longitude","latitude"))
dsurvey2019sf <- st_set_crs(dsurvey2019sf, 4269) 
ggplot() +
  geom_sf(data = dsurvey2019sf, colour = "grey70", fill = "grey90") 
dsurvey2019sf2 <- select(dsurvey2019sf, survey_id, fishing_event_id ) 
st_write(dsurvey2019sf2, "output/dsurvey2019sf3.shp", append = FALSE)
#from QGIS points subset
dsurvey2019sf3 <- st_read("output/dsurvey2019sf2_HBLLcalibrationsites.shp")

hbll2019 <- hblldog %>%
  mutate(
    latitude2 = latitude,
    longitude2 = longitude) %>%
  st_as_sf(coords = c("longitude2","latitude2")) %>%
  st_set_crs(4269) %>%
  st_transform(26909)

ggplot() +
  geom_sf(data = hbll2019, colour = "grey70", fill = "grey90") 

dsurvey2019 <- dsurvey2019sf3 %>%
  st_as_sf(coords = c("longitude","latitude")) %>%
  st_set_crs(4269) %>%
  st_transform(26909)# %>%
#st_buffer(dist = units::set_units(5, "kilometers"))

x <- ggplot() +
  geom_sf(data = hbll2019, colour = "red", fill = "red") 
x +  geom_sf(data = dsurvey2019, colour = "grey70", fill = NA) 


# x <- st_intersects(three2019, all2019)
# x2 <- unique(unlist(x))
# 
# all2019subset <- all2019[c(11, 15, 16, 23,  1 , 8, 25, 26,  2 , 6 , 7 ,12),]
# 
# ggplot() +
#   geom_sf(data = all2019subset) 
# 
# x <- ggplot() +
#    geom_sf(data = all2019subset, colour = "red", fill = "red") 
# x +  geom_sf(data = three2019, colour = "grey70", fill = NA) 
# 
# 
# st_geometry(all2019subset) <- NULL
# glimpse(all2019subset)
# glimpse(d2019)
# 
# dog2019 <- rbind(all2019subset, d2019)



```


```{r}
doghbll <- st_read("output/HBLLdogsurvey.shp")
dog <- st_read("output/dsurvey2019sf3_subset.shp")

plot(st_geometry(doghbll), col = "red")
plot(st_geometry(dog), add = TRUE, col = "blue")

st_geometry(doghbll) <- NULL
st_geometry(dog) <- NULL
dog <- rbind(dog, doghbll)


#October dogfish catches
View(both)
Oct <- filter(both, survey_series_id == 76) 
octFID <- dog$fshng__
octdog <- filter( Oct, fishing_event_id %in% octFID)
octdog$survey <- "Oct"
octdog$gear <- "dogfish_gear"
octdog2 <- left_join(octdog, dog, by = c("fishing_event_id" = "fshng__"))
# doghbll2 <- left_join(doghbll, both, by = c("fshng__" = "fishing_event_id"))
# dog2$survey <- "dog"
# 

#HBLL dogfish catch
hbll_dogfish_catch2

#plots of geoegraphic locations
ggplot(augdog, aes(latitude,longitude, colour = as.factor(site))) + geom_point()
ggplot(octdog2, aes(latitude,longitude, colour = as.factor(site))) + geom_point()


#Combine Oct and Aug catches
octdog3 <- dplyr::select(octdog2, dogfish_count, lglsp_hook_count, survey_desc, site, survey, depth_m, gear )
# augdog2 <- dplyr::select(augdog, dogfish_count, lglsp_hook_count, survey_desc, site, survey, depth_m, gear)
hbll_dogfish_catch2

dog4 <- rbind(octdog3, augdog2)
dog4$survey_gear <- paste(dog4$survey,  dog4$gear)
dog5 <- na.omit(dog4)
dog5$sitename <- ifelse(dog5$site == 1, "Cape Mudge", ifelse(dog5$site == 2, "West Cape Lazo", "Qualicum-Parksville" ))

ggplot(dog5, aes(depth_m, dogfish_count/lglsp_hook_count, group = as.factor(survey_gear) , colour = as.factor(survey_gear))) + geom_point(size = 2) +  facet_wrap(~sitename) + geom_line(size = 2) + scale_colour_manual(values = c("#6F99ADFF", "#7876B1FF", "#E18727FF"))





dog2$cpue <- dog2$dogfish_count/dog2$lglsp_hook_count
dog3 <- filter(dog2, !is.na(dog2$depth_m))
dog3 <- filter(dog3, !is.na(dog3$dogfish_count))

dog3$survey_desc2 <- ifelse(dog3$survey_desc == "2019 Strait of Georgia Longline Dogfish Survey", "dogfish_October2019", "dogfish_Aug2019")
m_dog <- glmer(
  dogfish_count ~ poly(depth_m,2)*as.factor(survey_desc2) + offset(log(lglsp_hook_count)) + (1 + survey_desc2| site),   data = dog3,
  family = poisson(link = "log") )

# m_dog <- glm(
#   dogfish_count ~ poly(depth_m,2)*as.factor(survey_desc2) + offset(log(lglsp_hook_count)) + as.factor(site), 
#   data = dog3,
#   family = poisson(link = "log") )
# visreg(m_dog, "depth_m", cond=list(survey_desc2="2019 Strait of Georgia Longline Dogfish Survey", site = 3))


nd <- expand.grid(depth_m = seq(min(dog3$depth_m), max(dog3$depth_m), length.out = 30),
                  survey_desc2 = c("dogfish_Aug2019", "dogfish_October2019"), site = c(1,2,3))
nd$lglsp_hook_count <- mean(dog3$lglsp_hook_count)
p <- predict(m_dog, newdata = nd)
nd$prediction <- exp(p)
x <- ggplot(nd, aes(depth_m, prediction, group = survey_desc2, colour = survey_desc2)) +
  geom_line(size = 2) + facet_wrap(~site) 
x + geom_point(data = dog3, aes(depth_m, dogfish_count, group = survey_desc2 , colour = survey_desc2  )) +  facet_wrap(~site) + 
  scale_colour_manual(values = c("grey", "black"))

```


```{r dogfishcomparisonsurvey_composition}

dsurvey_bio <- gfdata::run_sql("GFBioSQL", "SELECT A.ACTIVITY_DESC, YEAR(B21.TRIP_START_DATE) AS YEAR, B21.TRIP_COMMENT,
FISHING_EVENT_ID,
       B21.TRIP_ID, B21.FE_MAJOR_LEVEL_ID,
       B21.SPECIES_CODE, S.SPECIES_SCIENCE_NAME, S.SPECIES_COMMON_NAME,
       B22.SPECIMEN_ID, B22.SPECIMEN_SEX_CODE, B22.Total_Length, B22.Round_Weight
FROM GFBioSQL.dbo.B21_Samples B21
INNER JOIN GFBioSQL.dbo.B22_Specimens B22 ON B22.SAMPLE_ID = B21.SAMPLE_ID
INNER JOIN GFBioSQL.dbo.TRIP_ACTIVITY TA ON TA.TRIP_ID = B21.TRIP_ID
INNER JOIN GFBioSQL.dbo.ACTIVITY A ON A.ACTIVITY_CODE = TA.ACTIVITY_CODE
INNER JOIN GFBioSQL.dbo.SPECIES S ON S.SPECIES_CODE = B21.SPECIES_CODE
WHERE TA.ACTIVITY_CODE IN (39,53) AND B21.SPECIES_CODE = '044'
       AND YEAR(B21.TRIP_START_DATE) = 2019
ORDER BY B21.TRIP_ID, B21.FE_MAJOR_LEVEL_ID, B22.SPECIMEN_ID")

unique(d2019$fishing_event_id)
unique(hbllsurvey_dogsitesdoggear4$fishing_event_id)

dsurvey_bio
unique(dsurvey_bio$FISHING_EVENT_ID)

test <- inner_join(dsurvey_bio, dog3, by = c("FISHING_EVENT_ID" = "fshng__"))
unique(dog3$fshng__)
unique(dog3$grouping_code)

ggplot(test, aes(depth_m, Total_Length, group = ACTIVITY_DESC, colour = ACTIVITY_DESC)) +
  geom_jitter(size = 2) + facet_wrap(~SPECIMEN_SEX_CODE)

glimpse(test)
test2 <- ifelse(test$grouping_desc)

ggplot(test, aes(grouping_desc, Total_Length, group = ACTIVITY_DESC, colour = ACTIVITY_DESC)) +
  geom_jitter(size = 2) + facet_wrap(~SPECIMEN_SEX_CODE)

glimpse(test)
unique(test$depth_m)
unique(test$grouping_desc)
test$grouping_desc <- ifelse(test$depth_m %in% c(56:110), "56 - 110 m", 
                             ifelse(test$depth_m %in% c(111:165), "111 - 165 m", 
                                    ifelse(test$depth_m %in% c(166:220), "166 - 220 m", 
                                           "> 220 m")))

ggplot(test, aes(grouping_desc, Total_Length, group = ACTIVITY_DESC, colour = ACTIVITY_DESC)) +
  geom_jitter(size = 2) + facet_wrap(~SPECIMEN_SEX_CODE+ site)

ggplot(test, aes(grouping_desc, Total_Length, group = ACTIVITY_DESC, colour = ACTIVITY_DESC)) +
  geom_jitter(aes(group = ACTIVITY_DESC), size = 2) + facet_wrap(~SPECIMEN_SEX_CODE+ site)

test$grouping_desc <- factor(test$grouping_desc,
                             levels = c("56 - 110 m", 
                                        "111 - 165 m", 
                                        "166 - 220 m", 
                                        "> 220 m" 
                             ),ordered = TRUE)

ggplot(test, aes(grouping_desc, Total_Length, group = ACTIVITY_DESC, colour = ACTIVITY_DESC)) +
  geom_jitter(size = 2) + facet_wrap(~SPECIMEN_SEX_CODE+ site)

ggplot(test, aes(grouping_desc, Total_Length, group = ACTIVITY_DESC, colour = ACTIVITY_DESC)) +
  geom_jitter(size = 2) + facet_wrap(~SPECIMEN_SEX_CODE) +scale_colour_manual(values = c("grey", "black"))


ggplot(test, aes(grouping_desc, Total_Length )) +
  #geom_point(aes(colour = ACTIVITY_DESC)) + 
  geom_boxplot(aes(fill = ACTIVITY_DESC)) + facet_wrap(~SPECIMEN_SEX_CODE + site) + 
  geom_vline(xintercept = c(650, 800)) + 
  theme_classic()+scale_fill_manual(values = c("grey", "black")) + 
  scale_colour_manual(values = c("grey", "black")) 

ggplot(test, aes(grouping_desc, Total_Length, group = ACTIVITY_DESC, colour = ACTIVITY_DESC)) +
  geom_jitter(size = 2) + facet_wrap(~SPECIMEN_SEX_CODE + site) +scale_colour_manual(values = c("grey", "black")) + 
  theme_classic()+scale_fill_manual(values = c("grey", "black")) + 
  scale_colour_manual(values = c("grey", "black")) 

test2 <- test %>%
  filter(Total_Length >650, SPECIMEN_SEX_CODE == 1) 

test3 <- test %>%
  filter(Total_Length >800, SPECIMEN_SEX_CODE == 2) 
test4 <- rbind(test2, test3)

ggplot(test4, aes(grouping_desc, Total_Length, group = ACTIVITY_DESC, colour = ACTIVITY_DESC)) +
  geom_jitter(size = 2) + facet_wrap(~SPECIMEN_SEX_CODE + site) +scale_colour_manual(values = c("grey", "black")) + 
  theme_classic()+scale_fill_manual(values = c("grey", "black")) + 
  scale_colour_manual(values = c("grey", "black")) 
ggsave("Figures/compare_matureindividuals.jpg")

test2 <- test %>%
  filter(Total_Length <650, SPECIMEN_SEX_CODE == 1) 

test3 <- test %>%
  filter(Total_Length <800, SPECIMEN_SEX_CODE == 2) 
test4 <- rbind(test2, test3)

ggplot(test4, aes(grouping_desc, Total_Length, group = ACTIVITY_DESC, colour = ACTIVITY_DESC)) +
  geom_jitter(size = 2) + facet_wrap(~SPECIMEN_SEX_CODE + site) +scale_colour_manual(values = c("grey", "black")) + 
  theme_classic()+scale_fill_manual(values = c("grey", "black")) + 
  scale_colour_manual(values = c("grey", "black")) 
ggsave("Figures/compare_immatureindividuals.jpg")

dsurvey_bio2 <- filter(test, !is.na(Total_Length))
test <- dsurvey_bio2 %>% group_by(ACTIVITY_DESC, SPECIMEN_SEX_CODE) %>% summarise(numsex = n(), meanlength = mean(Total_Length), medianlength = median(Total_Length), maxlength = max(Total_Length), minlength = min(Total_Length))


```

```{r prediction_grid_SOG}

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
data(bcBathymetry) # bathymetry
contour(bcBathymetry$x, bcBathymetry$y, bcBathymetry$z, col = "pink", method = "edge", vfont = c("sans serif", "plain"))

cont <- contourLines(bcBathymetry$x, bcBathymetry$y, bcBathymetry$z, nlevels = 1000)
clines <- ContourLines2SLDF(cont)
clines@data[["level"]]
lines <- st_as_sf(clines) # make sf object
plot(lines)

st_crs(lines) <- CRS("+proj=longlat")
test
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
plot(prediction_grid)
prediction_center <- st_read("output/PredictionGridCentres_SOG.shp")
glimpse(prediction_center)

centerdf <- do.call(rbind, st_geometry(prediction_center)) %>%
  as_tibble() %>%
  setNames(c("X", "Y"))
centerdf$value <- seq(1, nrow(centerdf))

# lat and longs for each year to predict on
centerdf2 <- expand.grid(value, years)
names(centerdf2) <- c("value", "year")
centerdf3 <- left_join(centerdf2, centerdf, by = c("value" = "value"))
unique(centerdf3$year)

## Create the clipping polygon
maxlat <- max(centerdf$Y) # Northing, lat
minlat <- min(centerdf$Y) # northing, lat
maxlon <- max(centerdf$X) # Easting, long
minlon <- min(centerdf$X) # easting, long

.f <- "output/data-generated/NOAA-bathy.rds"
if (!file.exists(.f)) {
  # get bathymetry data
  b <- marmap::getNOAA.bathy(lon1 = -150, lon2 = -110, lat1 = 30, lat2 = 70, resolution = 1)
  saveRDS(b, file = .f)
} else {
  b <- readRDS(.f)
}

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
centertrans5$posdepth <- centertrans5$depth_m * -1
centertrans5$logdepth <- log10(centertrans5$posdepth)
saveRDS(centertrans5, "output/predictiongrid_bccoast_SOG.rds")


# load prediction grid files ----------------------------------------------

centertrans5 <- readRDS("output/predictiongrid_bccoast_SOG.rds")

```


Dogfish Survey 

```{r}
d #df from above, all dogfish survey data

d$grouping_desc2 <- ifelse(d$depth_m %in% c(56:110), "a56 - 110 m", 
                           ifelse(d$depth_m %in% c(111:165), "b111 - 165 m", 
                                  ifelse(d$depth_m %in% c(166:220), "c166 - 220 m", 
                                         "d> 220 m")))

d_2005 <- filter(d, hook_desc == "CIRCLE HOOK")

# ggplot(dog4, aes(grouping_desc, count,  group = survey_desc2, colour = survey_desc2)) + geom_line(size = 2)  + scale_colour_manual(values = c("grey", "black"))
# ggsave("Figures/compare_nosites.jpg")
# 
# ggplot(dog4, aes(grouping_desc, count,  group = survey_desc2, colour = survey_desc2)) + geom_line(size = 2)  + scale_colour_manual(values = `c("grey", "black")) + facet_wrap(~site)
# ggsave("Figures/compare_sites.jpg")


d_2005_sf <- sf::st_as_sf(d_2005, coords = c("longitude", "latitude"))
st_crs(d_2005_sf) <- CRS("+proj=longlat")
d_2005_sf <- st_transform(d_2005_sf, crs = ("+proj=utm +zone=9 +datum=WGS84"))
str(d_2005_sf)
d_2005_sf2 <- select(d_2005_sf, -survey_series_desc, -survey_series_id, -survey_desc   )
st_write(d_2005_sf2, "output/dsurveys4.shp", append = FALSE)
d_2005_sf2 <- select(d_2005_sf, -survey_series_desc, -survey_series_id, -survey_desc   )
st_write(d_2005_sf2, "output/dsurveys4.shp", append = FALSE)
d_2005_sf <- st_read("output/dsurveys4.shp")


d_2005_sf2 <- d_2005_sf %>%
  mutate(
    UTM.lon.m = unlist(map(geometry, 1)),
    UTM.lat.m = unlist(map(geometry, 2))
  ) %>%
  mutate(
    UTM.lon = UTM.lon.m / 1000,
    UTM.lat = UTM.lat.m / 1000
  ) %>%
  filter(!is.na(depth_m)) %>%
  ungroup()

st_geometry(d_2005_sf2) <- NULL

d_2005_sf2$offset <- log10(d_2005_sf2$lglsp__)
d_2005_sf3 <- filter(d_2005_sf2, !is.na(dgfsh_c      ))
range(d_2005_sf3$dgfsh_c      )
x <- density(d_2005_sf3$dgfsh_c)
plot(x)


ggplot(d_2005_sf3, aes(year, dgfsh_c)) +geom_jitter() + facet_wrap(~site)
d_2005_sf4 <- filter(d_2005_sf3, site != 9)
ggplot(d_2005_sf4, aes(year, dgfsh_c)) +geom_jitter() + facet_wrap(~site)
ggplot(d_2005_sf4, aes(depth_m, dgfsh_c/lglsp__, group = as.factor(year), colour = as.factor(year))) +geom_jitter() + facet_wrap(~site) +
  geom_line()
ggsave("Figures/dogfishsurvey_depth.jpg")

#HBLL dogfish
mesh20 <- sdmTMB::make_mesh(d_2005_sf4,
                            xy_cols = c("UTM.lon", "UTM.lat"),
                            n_knots = 50
)
plot(mesh20)


#scale response variable
d_2005_sf4$dgfsh_c_scale <- scale(d_2005_sf4$dgfsh_c)

m_dog_site <- sdmTMB(
  formula = dgfsh_c_scale  ~ 0 + as.factor(year)  + as.factor(site) + s(depth_m, k=3),
  offset = d_2005_sf4$offset,
  data = d_2005_sf4,
  mesh = mesh20,
  spatiotemporal = "IID",
  time = "year",
  silent = FALSE,
  # anisotropy = TRUE,
  family = tweedie(link = "log"),
  spatial = TRUE
)
max(m_dog_site$gradients)
#saveRDS(m_dog, file = f)

x <- density(d_2005_sf4$dgfsh_c_scale)
plot(x)
m_dog_scale <- sdmTMB(
  formula = dgfsh_c_scale  ~ 0 + s(depth_m, k=3) + as.factor(year) ,
  offset = d_2005_sf4$offset,
  data = d_2005_sf4,
  mesh = mesh20,
  spatiotemporal = "IID",
  time = "year",
  silent = FALSE,
  # anisotropy = TRUE,
  family = gaussian(link = "identity"),
  spatial = TRUE
)
max(m_dog_scale$gradients)
#saveRDS(m_dog, file = f)

m_dog <- sdmTMB(
  formula = dgfsh_c  ~ 0 + s(depth_m, k=3) + as.factor(year) ,
  offset = d_2005_sf4$offset,
  data = d_2005_sf4,
  mesh = mesh20,
  spatiotemporal = "IID",
  time = "year",
  silent = FALSE,
  # anisotropy = TRUE,
  family = tweedie(link = "log"),
  spatial = TRUE
)
max(m_dog$gradients)

m_dog_p <- sdmTMB(
  formula = dgfsh_c  ~ 0 + s(depth_m, k=3) + as.factor(year) ,
  offset = d_2005_sf4$offset,
  data = d_2005_sf4,
  mesh = mesh20,
  spatiotemporal = "IID",
  time = "year",
  silent = FALSE,
  # anisotropy = TRUE,
  family = poisson(link = "log"),
  spatial = TRUE
)
max(m_dog_p$gradients)
#saveRDS(m_dog, file = f)


m_dog_nb <- sdmTMB(
  formula = dgfsh_c  ~ 0 + s(depth_m, k=3) + as.factor(year) ,
  offset = d_2005_sf4$offset,
  data = d_2005_sf4,
  mesh = mesh20,
  spatiotemporal = "IID",
  time = "year",
  silent = FALSE,
  # anisotropy = TRUE,
  family = nbinom2(link = "log"),
  spatial = TRUE
)
max(m_dog_nb$gradients)
m_dog_nb <- sdmTMB::run_extra_optimization(m_dog_nb)

#saveRDS(m_dog, file = f)





#model on depth subset data
hblldepth <- filter(d_2005_sf4, depth_m < 110 & depth_m > 24 )
ggplot(hblldepth, aes(depth_m, dgfsh_c/lglsp__, group = as.factor(year), colour = as.factor(year))) +geom_jitter() + facet_wrap(~site) +
  geom_line()
ggplot(d_2005_sf4, aes(depth_m, dgfsh_c/lglsp__, group = as.factor(year), colour = as.factor(year))) +geom_jitter() + facet_wrap(~year) +
  geom_line()
ggplot(hblldepth, aes(depth_m, dgfsh_c/lglsp__, group = as.factor(year), colour = as.factor(year))) +geom_jitter() + facet_wrap(~year) +
  geom_line()
ggplot(hblldepth, aes(depth_m, dgfsh_c/lglsp__)) + geom_point() + facet_wrap(~site) 

ggsave("Figures/dogfishsurvey_depth.jpg")

mesh <- sdmTMB::make_mesh(hblldepth,
                          xy_cols = c("UTM.lon", "UTM.lat"),
                          n_knots = 30
)
plot(mesh)

m_dog_depthsub <- sdmTMB(
  formula = dgfsh_c  ~  0 + as.factor(year) +  s(depth_m, k = 3),
  offset = hblldepth$offset,
  data = hblldepth,
  mesh = mesh,
  spatiotemporal = "IID",
  time = "year",
  silent = FALSE,
  # anisotropy = TRUE,
  family = tweedie(link = "log"),
  spatial = TRUE
)
max(m_dog_depthsub$gradients)
m_dog_depthsub <- sdmTMB::run_extra_optimization(m_dog_depthsub)
#saveRDS(m_dog, file = f)


m_dog_depthsubp <- sdmTMB(
  formula = dgfsh_c  ~ 0 + as.factor(year) +  s(depth_m, k = 3)  ,
  offset = hblldepth$offset,
  data = hblldepth,
  mesh = mesh,
  spatiotemporal = "IID",
  time = "year",
  silent = FALSE,
  # anisotropy = TRUE,
  family = nbinom2(link = "log"),
  spatial = TRUE
)
max(m_dog_depthsubp$gradients)
m_dog_depthsubp <- sdmTMB::run_extra_optimization(m_dog_depthsubp)
#saveRDS(m_dog, file = f)



#see coefficients
tidy(m_dog, "ran_pars", conf.int = TRUE)
tidy(m_dog_scale, "ran_pars", conf.int = TRUE)
tidy(m_dog_depthsub, "ran_pars", conf.int = TRUE)
tidy(m_dog_depthsubp, "ran_pars", conf.int = TRUE)

####extract aic
extractAIC.sdmTMB(m_dog_scale)
extractAIC.sdmTMB(m_dog)
extractAIC.sdmTMB(m_dog_site)
extractAIC.sdmTMB(m_dog_p)
extractAIC.sdmTMB(m_dog_nb)

extractAIC.sdmTMB(m_dog_depthsub)
extractAIC.sdmTMB(m_dog_depthsubp)

# m_dog_nbinom1
s <- simulate(m_dog, nsim = 300)
sum(s == 0) / length(s)

pred <- m_dog$family$linkinv(predict(m_dog)$est_non_rf)

r <- DHARMa::createDHARMa(
  simulatedResponse = s,
  observedResponse = d_2005_sf3$dgfsh_c,
  fittedPredictedResponse = pred
)
resid_plot <- DHARMa::testResiduals(r)
DHARMa::testSpatialAutocorrelation(m_dog, x = d_2005_sf3$UTM.lon, y = dogfish_count$UTM.lat)

# m_dog_depthsub
s <- simulate(m_dog_depthsub, nsim = 300)
sum(s == 0) / length(s)

pred <- m_dog_depthsub$family$linkinv(predict(m_dog_depthsub)$est_non_rf)

r <- DHARMa::createDHARMa(
  simulatedResponse = s,
  observedResponse = hblldepth$dgfsh_c,
  fittedPredictedResponse = pred
)
resid_plot <- DHARMa::testResiduals(r)
DHARMa::testSpatialAutocorrelation(m_dog, x = hblldepth$UTM.lon, y = hblldepth$UTM.lat)



#prediction grid m_dog
predgrid <- readRDS("output/predictiongrid_bccoast_SOG.rds")
predgrid$offset <- 0
year <- unique(d_2005_sf3$year)
predgrid5 <- purrr::map_dfr(year, function(.x) {dplyr::mutate(predgrid, year = .x)})
predgrid6 <- filter(predgrid5, UTM.lat.m >= 5433850 & UTM.lat.m <=  5545813)
predgrid6 <- filter(predgrid6, depth_m >= 5 & depth_m <= 288)
predgrid7 <- predgrid6 %>% mutate(UTM.lat = UTM.lat.m/1000, UTM.lon = UTM.lon.m/1000)
plot(predgrid7$UTM.lat, predgrid7$UTM.lon)
points(d_2005_sf3$UTM.lat, d_2005_sf3$UTM.lon)
predgrid7$site <- 3

pred <- predict(m_dog, newdata = predgrid7, return_tmb_object = TRUE)
saveRDS(pred, "output/pred_indices/pred_m_dog_SOG.rds")
index <- get_index(pred, bias_correct = TRUE)
saveRDS(index, "output/pred_indices/index_m_dog_SOG.rds")


ggplot(index, aes(year, est)) +
  geom_line(col = "dark blue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "blue") +
  xlab("Year") +
  ylab(paste0("Index of abundance")) +
  theme_sleek()


#prediction grid m_dog_depthsub
predgrid <- readRDS("output/predictiongrid_bccoast_SOG.rds")
predgrid$offset <- 0
year <- unique(hblldepth$year)
predgrid5 <- purrr::map_dfr(year, function(.x) {dplyr::mutate(predgrid, year = .x)})
range(hblldepth$UTM.lat.m)
predgrid6 <- filter(predgrid5, UTM.lat.m >= 5434918 & UTM.lat.m <=  5545814)
range(hblldepth$depth_m)
predgrid6 <- filter(predgrid6, depth_m >= 30 & depth_m <= 110)
predgrid7 <- predgrid6 %>% mutate(UTM.lat = UTM.lat.m/1000, UTM.lon = UTM.lon.m/1000)
plot(predgrid7$UTM.lat, predgrid7$UTM.lon)
points(hblldepth$UTM.lat, hblldepth$UTM.lon)
#predgrid7$site <- 3

pred <- predict(m_dog_depthsub, newdata = predgrid7, return_tmb_object = TRUE)
saveRDS(pred, "output/pred_indices/pred_m_dog_depthsub_SOG.rds")
index2 <- get_index(pred, bias_correct = TRUE)
saveRDS(index2, "output/pred_indices/index_m_dog_depthsub_SOG.rds")



ggplot(index2, aes(year, est)) +
  geom_line(col = "dark blue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "red") +
  xlab("Year") +
  ylab(paste0("Index of abundance")) +
  theme_sleek()


x <- ggplot(index, aes(year, est)) +
  geom_line(col = "black", size = 2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "gray30") +
  xlab("Year") +
  ylab(paste0("Index of abundance")) +
  theme_sleek()
x + geom_line(data = index2, aes(year, est), col = "#D95F02", size = 2) +
  geom_ribbon(data = index2, aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#D95F02")


index$est_scale <- scale(index$est)
mean <- mean(index$est) 
sd <- sd(index$est) 
index$lwr_scale <- (index$lwr-mean)/sd
index$upr_scale <- (index$upr-mean)/sd

index2$est_scale <- scale(index2$est)
mean <- mean(index2$est) 
sd <- sd(index2$est) 
index2$lwr_scale <- (index2$lwr-mean)/sd
index2$upr_scale <- (index2$upr-mean)/sd

x <- ggplot(index, aes(year, est_scale)) +
  geom_line(col = "black", size = 2) +
  geom_ribbon(aes(ymin = lwr_scale, ymax = upr_scale), alpha = 0.4, fill = "gray30") +
  xlab("Year") +
  ylab(paste0("Index of abundance")) +
  theme_sleek()
x + geom_line(data = index2, aes(year, est_scale), col = "#D95F02", size = 2) +
  geom_ribbon(data = index2, aes(ymin = lwr_scale, ymax = upr_scale), alpha = 0.4, fill = "#D95F02")


```



```{r dogfishtagging}

recoveries <- read.csv("C:/Dogfish_stitch/Taggingdata/RECOVERIES.csv")
r2 <- select(recoveries, REC_MON, REC_DAY, REC_LONG, REC_LAT)
r2$long <- r2$REC_LONG*-1
r3 <- filter(r2, !is.na(r2$REC_LONG))
r2_sf <- sf::st_as_sf(r3, coords = c("long", "REC_LAT")) %>% st_sf()
st_crs(r2_sf) <- CRS("+proj=longlat")
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
bc_coast <- st_crop(
  map_data,
  c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)
)
bc_coast_proj <- sf::st_transform(bc_coast, crs = 26909)

r2_sf2 <-   st_crop(
  r2_sf,
  c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)
) 
ggplot() +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") + 
  geom_sf(data = r2_sf2)+ 
  facet_wrap(~REC_MON) + geom_jitter() 


rsum <- recoveries %>% group_by(DEPTH, REC_MON) %>% tally()
rsum2 <- na.omit(rsum)
ggplot(rsum2, aes(DEPTH, n)) + geom_point() + geom_line() + facet_wrap(~REC_MON)

rsum <- recoveries %>% group_by(REC_LAT, REC_YR) %>% tally()
rsum2 <- na.omit(rsum)
ggplot(rsum2, aes(REC_LAT, n)) + geom_point() + geom_line() + facet_wrap(~REC_LAT)

rec2 <- filter(recoveries, !is.na(REC_LAT) & !is.na(REC_MON) )
rec2 %>% ggplot(aes(REC_LAT)) + geom_histogram() + facet_wrap(~REC_MON)
rec2 %>% ggplot(aes(DEPTH)) + geom_histogram() + facet_wrap(~REC_MON)
rec2 %>% ggplot(aes(REC_LONG)) + geom_histogram() + facet_wrap(~REC_MON )
rec2 %>% ggplot(aes(REC_LAT, fill = as.factor(SEX), group = as.factor(SEX))) + geom_histogram() + facet_wrap(~REC_MON )
rec2 %>% ggplot(aes(DEPTH, fill = as.factor(SEX), group = as.factor(SEX))) + geom_histogram() + facet_wrap(~REC_MON)
rec2 %>% ggplot(aes(DEPTH, fill = as.factor(SEX), group = as.factor(SEX))) + geom_histogram() + facet_wrap(~REC_YR)


```


#summary of catch weight by year by survey_abbrev
```{r}

unique(data_surveysets$survey_abbrev)

#sets_ll2 <- filter(data_surveysets, survey_abbrev %in% c("HBLL INS N", "HBLL INS S")) %>% mutate(type = "longline")
sets_ll2 <- filter(data_surveysets, survey_abbrev %in% c("HBLL INS S")) %>% mutate(type = "longline")
range(sets_ll2$depth_m)
#sets_ll$trip_id <- sets_ll$year
#x <- c( "Hard Bottom Longline Inside North ","Hard Bottom Longline Inside South ",
#"Strait of Georgia Synoptic Bottom Trawl"    )
x <- c( "Hard Bottom Longline Inside South " )
sets_ll <- filter(sets_ll2, survey_series_desc %in% x ) 

#samps_ll2 <- filter(data_surveysamples, survey_abbrev %in% c("SYN SOG","HBLL INS N", "HBLL INS S")) %>% mutate(type = "longline")
samps_ll2 <- filter(data_surveysamples, survey_abbrev %in% c( "HBLL INS S")) %>% mutate(type = "longline")
unique(samps_ll2$survey_series_desc)
x <- c( "Hard Bottom Longline Inside South " )

#x <- c( "Hard Bottom Longline Inside North ","Hard Bottom Longline Inside South ",
#"Strait of Georgia Synoptic Bottom Trawl"    )
samps_ll <- filter(samps_ll2, survey_series_desc %in% x ) 
unique(samps_ll$survey_abbrev)
samps_ll <- filter(samps_ll, !is.na(species_common_name) == TRUE)

unique(data_surveysets$survey_abbrev)
sets_tl <- filter(data_surveysets, survey_abbrev %in% c("SYN SOG")) %>% mutate(type = "trawl")
unique(sets_tl$survey_series_desc)
samps_tl <- filter(data_surveysamples, survey_abbrev %in% c( "SYN SOG")) %>% mutate(type = "trawl")
unique(samps_tl$survey_abbrev)
samps_tl <- filter(samps_tl, !is.na(species_common_name) == TRUE)

sets <- sets_ll2
#sets <- rbind(sets_ll, sets_tl) 
#samps <- rbind(samps_ll, samps_tl)

glimpse(sets_ll)
sets <- select(sets, catch_weight, year, fishing_event_id, latitude, longitude, depth_m, doorspread_m, tow_length_m,
               catch_count, skate_count, hook_count, survey_abbrev, type, trip_id, time_deployed)
glimpse(sets)


# trawl <- c("SYN SOG")  
# data_surveysets2 <- filter(data_surveysets, survey_abbrev %in% trawl)
# sum_survey_sets <- data_surveysets2 %>% group_by(year, survey_abbrev) %>% summarize(sum_weight = sum(catch_weight)) #depth_m
# 
# ggplot(data=sum_survey_sets, aes(year, sum_weight))  + geom_bar(stat="identity", width = 1, fill = "light blue", color="light blue") +  
#  scale_y_continuous(name = "Weight ()",expand = c(0,0)) + facet_wrap(~survey_abbrev, nrow = 3, scales = "free") +
#   theme_sleek()


```


```{r summaryfigures}
# summary figures ---------------------------------------------------------
sum_survey_sets <- sets %>%
  group_by(year, survey_abbrev, type) %>%
  summarize(sum_catch_weight = sum(catch_weight)) # depth_m

sum_count_survey_sets <- sets %>%
  group_by(year, survey_abbrev, type) %>%
  summarize(sum_counts = sum(catch_count), na.rm = TRUE)

ggplot(data = sum_survey_sets, aes(as.factor(year), sum_catch_weight)) +
  geom_bar(stat = "identity", width = 1, fill = "light blue", color = "light blue") +
  scale_y_continuous(name = "Weight (kg)", expand = c(0, 0)) +
  facet_wrap(~ survey_abbrev + type, nrow = 5, ncol = 3, scales = "free") +
  theme_sleek() # 2015 heat blob
ggsave ("Figures/SOG/summary_surveyweightSOG.pdf")

ggplot(data = sum_count_survey_sets, aes(as.factor(year), sum_counts)) +
  geom_bar(stat = "identity", width = 1, fill = "light blue", color = "light blue") +
  scale_y_continuous(name = "Counts", expand = c(0, 0)) +
  facet_wrap(~ survey_abbrev + type, nrow = 5, ncol = 3, scales = "free") +
  theme_sleek()
ggsave ("Figures/SOG/summary_surveycounts.pdf")


#dogfish surveys
glimpse(d)
unique(d$grouping_desc)
unique(d$year)
unique(d$depth_m)
dogsum <- d %>% group_by(year, hook_desc) %>% summarize(dogfish_catch_sum = sum(dogfish_count))
ggplot(data = dogsum, aes(as.factor(year), dogfish_catch_sum)) +
  geom_bar(stat = "identity", width = 1, fill = "light blue", color = "light blue") +
  scale_y_continuous(name = "Counts", expand = c(0, 0)) +
  facet_wrap(~ hook_desc, nrow = 5, ncol = 3) +
  theme_sleek()

```

```{r}
# summary figures ---------------------------------------------------------
sum_survey_samps <- samps %>%
  group_by(year, survey_abbrev, type) %>%
  summarize(sum_catch_weight = sum(catch_weight)) # depth_m

ggplot(data = sum_survey_sets, aes(year, sum_catch_weight)) +
  geom_bar(stat = "identity", width = 1, fill = "light blue", color = "light blue") +
  scale_y_continuous(name = "Weight (kg)", expand = c(0, 0)) +
  facet_wrap(~ survey_abbrev + type, nrow = 5, ncol = 3, scales = "free") +
  theme_sleek() # 2015 heat blob
ggsave ("Figures/summary_surveyweight.pdf")

ggplot(samps, aes(length, group = as.factor(sex), fill = as.factor(sex))) + geom_histogram(position = "identity") + facet_wrap(~survey_abbrev + year, scales = "free")

ggplot(samps, aes(weight, group = as.factor(sex), fill = as.factor(sex))) + geom_histogram(position = "identity") + facet_wrap(~survey_abbrev + year, scales = "free")

table(samps$survey_abbrev, samps$sex)
#table(samps$survey_abbrev, samps$length)


```

#surveys 

For now I have filtered out surveys without date information. This is 244 surveys. 

```{r}

#plot of months and survey type and year
glimpse(sets)
glimpse(samps)


#dogfish survey dates
glimpse(d)
d$julian <- lubridate::yday(d$fe_end_deployment_time)
d2 <- d %>% group_by(year, fishing_event_id) %>% filter(!is.na(fe_end_deployment_time)) %>%
  mutate(julian_min = min(julian),
         julian_max = max(julian),
         julian_mean = mean(julian)) %>%
  distinct(fishing_event_id, .keep_all = TRUE)

d3 <- d2 %>% group_by(year) %>% mutate(julian_min = min(julian),
                                       julian_max = max(julian),
                                       julian_mean = mean(julian))



#hbll dates 
sets$julian <- lubridate::yday(sets$time_deployed)
sets2 <- sets %>% group_by(type, year, fishing_event_id) %>% filter(!is.na(time_deployed)) %>%
  mutate(julian_min = min(julian),
         julian_max = max(julian),
         julian_mean = mean(julian)) %>%
  distinct(fishing_event_id, .keep_all = TRUE)

sets3 <- sets2 %>% group_by(type, year) %>% mutate(julian_min = min(julian),
                                                   julian_max = max(julian),
                                                   julian_mean = mean(julian))


# samps$julian <- lubridate::yday(samps$trip_start_date)
# samps2 <- samps %>% distinct(fishing_event_id, .keep_all = TRUE)
# samps3 <- samps2 %>% group_by(type, year, fishing_event_id) %>% filter(!is.na(trip_start_date)) %>%
#                                                              mutate(julian_min = min(julian),
#                                                                  julian_max = max(julian),
#                                                                  julian_mean = mean(julian)) %>%
#                                                                  distinct(fishing_event_id, .keep_all = TRUE)
# 
# samps4 <- samps3 %>% group_by(survey_abbrev, year) %>% mutate(julian_min = min(julian),
#                                                            julian_max = max(julian),
#                                                            julian_mean = mean(julian))

#plot yeach fishing id
ggplot() +
  facet_wrap(~survey_abbrev, scales = "free") +
  geom_pointrange(data=sets3, mapping=aes(x=year, y=julian_mean, ymin=julian_min, ymax=julian_max),
                  size=0.5, color="black", fill="white", shape=19)
ggsave("Figures/SOG/JulianDate_1yearvariability.pdf")
x <- filter(samps5, year == 2011 | year == 2012)


d5 <- d3 %>% distinct(fishing_event_id, julian,  .keep_all = TRUE)
ggplot() +
  #facet_wrap(~survey_abbrev, scales = "free") +
  geom_pointrange(data=d5, mapping=aes(x=year, y=julian_mean, ymin=julian_min, ymax=julian_max),
                  size=0.5, color="black", fill="white", shape=19)
ggsave("Figures/SOG/JulianDate_1yearvariabilityDOGFISHsurvey.pdf")

#all combined:
sets3
d5
dplot <- select(d5, year, julian_mean, julian_min, julian_max) %>% mutate(survey_abbrev = "dogfishsurvey")
setplot <- select(sets3, year, julian_mean, julian_min, julian_max, survey_abbrev) 
bothplot <- rbind(dplot, setplot)
ggplot() +
  geom_pointrange(data=bothplot, mapping=aes(x=year, y=julian_mean, ymin=julian_min, ymax=julian_max, group =survey_abbrev, colour = survey_abbrev ),
                  size=0.5,shape=19)
ggsave("Figures/SOG/JulianDate_allsurveys.pdf")


#plot yeach fishing id
sets4 <- sets2 %>% distinct(fishing_event_id, survey_abbrev, julian, type, .keep_all = TRUE)
ggplot() +
  facet_wrap(~survey_abbrev, scales = "free") +
  geom_pointrange(data=sets4, mapping=aes(x=year, y=julian_mean, ymin=julian_min, ymax=julian_max),
                  size=0.5, color="black", fill="white", shape=19)
ggsave("Figures/SOG/JulianDate_1yearvariability.pdf")


#plot by 5 year intervals
breaks <- seq(2003, 2021, 5)
years <- seq(2003, 2021, 1)
years2 <- findInterval(years, breaks)
yearsid <- data.frame(yearsid = c(rep(2003, 5), rep(2008, 5), rep(2013, 5), rep(2018, 4)))
year_bins <- data.frame(year = years, yearid = yearsid, group = years2)
set3 <- left_join (sets2, year_bins, by = c("year" = "year"))
head(set3)
set4 <- set3 %>% group_by(group, survey_abbrev)  %>% mutate(julian_min = min(julian),
                                                            julian_max = max(julian),
                                                            julian_mean = mean(julian))
ggplot(set4, aes(yearsid, julian_mean, colour = type)) + geom_point() +
  # geom_jitter() +
  facet_wrap(~survey_abbrev, scales = "free") +
  geom_pointrange(aes(ymin = julian_min, ymax = julian_max)) +
  scale_colour_manual(values = c("#00798c", "#d1495b", "dark blue"))
ggsave("Figures/SOG/JulianDate_5yearvariability.pdf")



```


```{r spatialdisturbiton}

d
sets
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
bc_coast <- st_crop(map_data,
                    c(xmin = -134, ymin = 46, xmax = -120, ymax = 57))
#bc_coast_proj <- sf::st_transform(bc_coast, crs = 26909)


ggplot() +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  geom_point(data = d, aes(longitude, latitude, col = dogfish_count), size = 1) +
  scale_colour_viridis_c(option = "plasma", name = "Count") +
  xlim(-127, -122) +
  ylim(48.5, 51) +
  facet_wrap(~as.factor(year), ncol = 4) #+ theme_sleek()
ggsave("Figures/SOG/Dogfishsurvey_raw.pdf")

glimpse(sets)


p <- ggplot(data = filter(sets, survey_abbrev == "HBLL INS S"), aes(longitude, latitude, col = catch_count, label = fishing_event_id), size = 1) + geom_point() +  ylim(48.5, 48.6) + xlim(-125, -124) 

p + geom_text()

ggplot() +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  geom_point(data = filter(sets, survey_abbrev == "HBLL INS S"), aes(longitude, latitude, col = catch_count), size = 1) + 
  scale_colour_viridis_c(option = "plasma", name = "Count") +
  xlim(-127, -122) +
  ylim(48.3, 51) +
  facet_wrap(~as.factor(year) + survey_abbrev, ncol = 4) #+ theme_sleek()
ggsave("Figures/SOG/HBLLS_survey_raw.pdf")

ggplot() +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  geom_point(data = filter(sets, survey_abbrev == "HBLL INS N"), aes(longitude, latitude, col = catch_count), size = 1) +
  scale_colour_viridis_c(option = "plasma", name = "Count") +
  #xlim(-127, -122) +
  #ylim(48.5, 51) +
  facet_wrap(~as.factor(year) + survey_abbrev, ncol = 4) #+ theme_sleek()
ggsave("Figures/SOG/HBLLN_survey_raw.pdf")

ggplot() +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  geom_point(data = filter(sets, survey_abbrev == "SYN SOG"), aes(longitude, latitude, col = catch_weight), size = 1) +
  scale_colour_viridis_c(option = "plasma", name = "Count") +
  xlim(-127, -122) +
  ylim(48.5, 51) +
  facet_wrap(~as.factor(year) + survey_abbrev, ncol = 4) #+ theme_sleek()
ggsave("Figures/SOG/SOG_survey_raw.pdf")


a <- ggplot() +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  geom_point(data = filter(sets, survey_abbrev == "HBLL INS S"), aes(longitude, latitude), col = "red", size = 1) +
  #scale_colour_viridis_c(option = "plasma", name = "Count") +
  xlim(-127, -122) +
  ylim(48.5, 51)# +
#facet_wrap(~as.factor(year) + survey_abbrev, ncol = 4) #+ theme_sleek()
b <- a + geom_point(data = d, aes(longitude, latitude), col = "black", size = 1) 
b + geom_point(data = filter(sets, survey_abbrev == "HBLL INS N"), aes(longitude, latitude), col = "blue", size = 1)
ggsave("Figures/SOG/surveyspatailoverlap_raw.pdf")



```


depth plot
```{r}

glimpse(d)
glimpse(sets)

sets2 <- sets %>% group_by(depth_m, year, survey_abbrev) %>% summarize(countbydepth = sum(catch_count))
ggplot(sets2, aes(depth_m, countbydepth, group = survey_abbrev, colour = survey_abbrev)) + geom_line() + facet_wrap(~survey_abbrev + year, scales = "free")


glimpse(d)
d2 <- d %>% group_by(depth_m, year) %>% summarize(countbydepth = sum(dogfish_count))
ggplot(d2, aes(depth_m, countbydepth)) + geom_line() + facet_wrap(~year)


#plot2
sets2 <- sets %>% group_by(depth_m, survey_abbrev) %>% summarize(countbydepth = sum(catch_count))
ggplot(filter(sets2, survey_abbrev != "SYN SOG"), aes(depth_m, countbydepth, group = survey_abbrev, colour = survey_abbrev)) + geom_line() 

d2 <- d %>% group_by(depth_m) %>% summarize(countbydepth = sum(dogfish_count)) %>% mutate(survey_abbrev = "dog")
ggplot(d2, aes(depth_m, countbydepth)) + geom_line()


both <- rbind(sets2, d2)
ggplot(subset(both, both$survey_abbrev != "SYN SOG"), aes(depth_m, countbydepth, group = survey_abbrev, colour = survey_abbrev)) + geom_line() 

sets3 <- sets %>% group_by(depth_m, year, survey_abbrev) %>% summarize(weightbydepth = sum(catch_weight))
ggplot(subset(sets3, sets3$survey_abbrev == "SYN SOG"), aes(depth_m, weightbydepth, group = survey_abbrev, colour = survey_abbrev )) + geom_line() 



#depth by age and sex
x <- distinct(data_surveysets, fishing_event_id, depth_m)
samps <- left_join(data_surveysamples, x) %>% select(sex, depth_m, year, survey_abbrev) %>% filter(!is.na(depth_m))
x <- c("SYN SOG", "OTHER")
samps4 <- filter(samps, !(survey_abbrev %in% x) )
unique(samps4$survey_abbrev)
range(samps4$depth_m)

x <- seq(24, 150, 1)
x2 <- seq(24, 150, 10)
x3 <- data.frame(cbind(x, findInterval(x, x2)))
head(x3$x)
samps2 <- left_join(samps4, x3, by = c("depth_m" = "x"))
samps3 <- samps2 %>% group_by(survey_abbrev, sex, year, V2) %>% summarize(count = n())

ggplot(samps3, aes(V2, count, group = sex, colour = as.factor(sex))) + geom_line() + facet_wrap(~year+ survey_abbrev)

```


Run checks on data using sample_catchweight_checks() 
```{r}


checkoutput <- y2 %>% sample_catchweight_checks()
# write.csv(checkoutput, "C:/GFdata_function/dogfish_diagnostics_output.csv")
# checkoutput <- read.csv("C:/GFdata_function/dogfish_diagnostics_output.csv")



sample_catchweight_checks <- function(y2) {
  stopifnot(length(unique(y2$species_common_name)) == 1L)
  
  # 1. check for sets with no samples
  nosamps <- y2 %>%
    group_by(survey_abbrev, fishing_event_id) %>%
    summarize(na_in_sample_id = sum(sample_id)) %>%
    filter(is.na(na_in_sample_id) == TRUE)
  tibble(fishing_event_id = nosamps$fishing_event_id, report = "no samples for this fishing event id")
  
  
  # 2. check for sets where sum sample weight is greater than catch_weight
  higher_sampsweight <- y2 %>%
    group_by(fishing_event_id, catch_weight) %>%
    summarize(sample_sum = sum(weight_complete) / 1000)
  
  higher_sampsweight2 <- higher_sampsweight %>%
    filter(sample_sum > catch_weight) %>%
    distinct(fishing_event_id, sample_sum, catch_weight)
  
  df <- tibble(fishing_event_id = higher_sampsweight2$fishing_event_id, report = "sum of sample weights > catch weight")
  
  plot_sample_bigger_catch <-
    ggplot(higher_sampsweight, aes(catch_weight, sample_sum), colour = "grey50") +
    geom_point()
  
  diag_setweightgreater <- plot_sample_bigger_catch +
    geom_point(data = higher_sampsweight2, mapping = aes(catch_weight, sample_sum), col = "red") +
    scale_x_log10() +
    scale_y_log10() +
    labs(x = "log10(set catch weight)", y = "log10(sum of samples weight)") +
    theme_sleek() +
    ggrepel::geom_text_repel(
      data = higher_sampsweight %>%
        mutate(label = ifelse(sample_sum > catch_weight,
                              as.character(fishing_event_id), NA
        )),
      aes(label = label),
      box.padding = 0.5,
      max.overlaps = Inf,
      show.legend = FALSE
    )
  
  # 3. check for multivariate outliers - create new column in data frame to hold Mahalanobis distances
  multi_outliers <- higher_sampsweight %>%
    select(catch_weight, sample_sum, fishing_event_id) %>%
    drop_na() %>%
    filter(catch_weight > 0 & sample_sum > 0) %>%
    mutate(
      logcatch_weight = log10(catch_weight),
      logsample_sum = log10(sample_sum)
    ) %>%
    select(logcatch_weight, logsample_sum, fishing_event_id)
  
  multi_outliers$mahal <- mahalanobis(multi_outliers, colMeans(multi_outliers, na.rm = TRUE), cov(multi_outliers))
  multi_outliers$p <- pchisq(multi_outliers$mahal, df = 3, lower.tail = FALSE)
  
  multi_outliers2 <- filter(multi_outliers, p < 0.001)
  multi_outliers3 <- filter(multi_outliers, p < 0.001) %>% distinct()
  
  x <- ggplot(multi_outliers, aes(logcatch_weight, logsample_sum / 1000)) + # shows very high catch_weights
    geom_point()
  
  diag_malahdistance <- x + geom_point(multi_outliers2, mapping = aes(logcatch_weight, logsample_sum / 1000), colour = "red") +
    labs(x = "log10(catch weight) (kg)", y = "log10(samples weight) (kg)") +
    theme_sleek() +
    # geom_text(aes(label = fishing_event_id), check_overlap = TRUE) +
    ggrepel::geom_text_repel(
      data = multi_outliers %>%
        mutate(label = ifelse(p < 0.001,
                              as.character(fishing_event_id), NA
        )),
      aes(label = label),
      box.padding = 1,
      max.overlaps = Inf,
      show.legend = FALSE
    )
  
  
  df <- rbind(df, tibble(
    fishing_event_id = multi_outliers3$fishing_event_id,
    report = "mutivariate outlier between catch weight and sample weight (mahalanobis distance"
  ))
  
  
  # 4. find catch_weights that fall outside 3 SD
  catch_weight_SD <- y2 %>%
    distinct(fishing_event_id, .keep_all = TRUE) %>%
    below_3sd_catchweight()
  catch_weight_SD2 <- filter(catch_weight_SD, within_3sd == FALSE)
  
  df <- rbind(df, tibble(
    fishing_event_id = catch_weight_SD2$fishing_event_id,
    report = "catch_weight falls outside of 3 SD of all catch weights"
  ))
  
  catch_weight_3sd <- ggplot(catch_weight_SD, aes(weight_complete / 1000, catch_weight)) +
    geom_point()
  
  diag_catchweight3sd <- catch_weight_3sd +
    geom_point(catch_weight_SD2, mapping = aes(weight_complete / 1000, catch_weight), colour = "red") +
    labs(x = "samples weight (kg)", y = "set catch weight (kg)") +
    theme_sleek() +
    ggrepel::geom_text_repel(
      data = catch_weight_SD %>%
        mutate(label = ifelse(within_3sd == FALSE,
                              as.character(fishing_event_id), NA
        )),
      aes(label = label),
      box.padding = 1,
      max.overlaps = Inf,
      show.legend = FALSE
    )
  
  
  # 5. find sample weight values outside of 3 SD
  sample_weight_SD <- y2 %>% below_3sd()
  sample_weight_SD2 <- filter(sample_weight_SD, within_3sd == FALSE)
  
  sample_weight_3SDplot <- ggplot(sample_weight_SD, aes(weight_complete / 1000, catch_weight)) +
    geom_point()
  
  diag_sampleweight3sd <- sample_weight_3SDplot +
    geom_point(sample_weight_SD2, mapping = aes(weight_complete / 1000, catch_weight), colour = "red") +
    labs(x = "samples weight (kg)", y = "set catch weight (kg)") +
    theme_sleek() +
    ggrepel::geom_text_repel(
      data = sample_weight_SD %>%
        mutate(label = ifelse(within_3sd == FALSE,
                              as.character(fishing_event_id), NA
        )),
      aes(label = label),
      box.padding = 1,
      max.overlaps = Inf,
      show.legend = FALSE
    )
  
  df <- rbind(df, tibble(
    fishing_event_id = sample_weight_SD2$fishing_event_id,
    report = "sum of sample weight per fishing id falls outside of 3 SD of all summed sample weights"
  ))
  
  x <- cowplot::plot_grid(diag_setweightgreater, diag_malahdistance, diag_catchweight3sd, diag_sampleweight3sd,
                          ncol = 2,
                          labels = c(
                            "sum sample weights > catch weight",
                            "Multivariate outliers (catch & sample weights)",
                            "Catch weights > 3 SD",
                            "Sample weights > 3 SD"
                          )
  )
  print(x)
  return(df)
}



```


Plot catch weight summaries for each of the surveys
```{r}

x <- filter(sets2,survey_abbrev != "SYN SOG")
x2 <- x %>% group_by(survey_abbrev, year) %>% summarize(counts = sum(catch_count), sumhook  = sum(hook_count), 
                                                        cpue = counts/sumhook)
ggplot(x2, aes(year, cpue, group = as.factor(survey_abbrev), colour = as.factor(survey_abbrev))) +  geom_line(size = 2)



sets2
x <- filter(sets2, year == 2003)
x2 <- sets2 %>% group_by(depth_m, survey_abbrev, year) %>% summarize(counts = sum(catch_count)) %>% filter()
ggplot(x2, aes(depth_m, counts, group = as.factor(year), colour = as.factor(year))) +  facet_grid(~survey_abbrev, scales = "free") + geom_smooth(se = FALSE)

x3 <- filter(x2, survey_abbrev == "HBLL INS N")
ggplot(x3, aes(depth_m, counts)) +  facet_wrap(~survey_abbrev+ year, nrow = 10, scale = "free_y") + geom_histogram()
ggsave("Figures/SOG/depth_HBLLINSN.pdf")

x3 <- filter(x2, survey_abbrev == "HBLL INS N")
ggplot(x3, aes(depth_m, counts)) +  facet_wrap(~survey_abbrev+ year, nrow = 10, scale = "free_y") + geom_smooth(se = FALSE) + geom_line(col = "light gray")
ggsave("Figures/SOG/depth_HBLLINSN.pdf")

x4 <- filter(x2, survey_abbrev == "HBLL INS S")
ggplot(x4, aes(depth_m, counts)) +  facet_wrap(~survey_abbrev+ year, nrow = 10, scale = "free_y") + geom_smooth(se = FALSE) + geom_line(col = "light gray")
ggsave("Figures/SOG/depth_HBLLINSN.pdf")

x4 <- filter(x2, survey_abbrev == "HBLL INS S")
ggplot(x4, aes(depth_m, counts)) +  facet_wrap(~survey_abbrev+ year, nrow = 10, scale = "free_y") + geom_smooth(se = FALSE) + geom_line(col = "light gray")
ggsave("Figures/SOG/depth_HBLLINSN.pdf")
```


Map of raw data
```{r}

sets2 <- sets
glimpse(sets2)
sets_sf <- st_as_sf(sets2,
                    coords = c("longitude", "latitude" ))
st_write(sets_sf, "output/HBLLINS_allpoints.shp", append = FALSE)
sets_sf2 <- st_read("output/HBLLINS_allpoints_wsite2.shp" )
str(unique(sets_sf2$site))
sets_sf <- filter(sets_sf2,is.na(site) == TRUE ) 
str(unique(sets_sf$site))

map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
bc_coast <- st_crop(
  map_data,
  c(xmin = -128, ymin = 48, xmax = -122, ymax = 51)
)
bc_coast_proj <- sf::st_transform(bc_coast, crs = 26909)

ggplot() +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  geom_point(data = sets, aes(longitude, latitude, col = log10(catch_count))) +
  scale_colour_viridis_c(option = "plasma", name = "Catch count (log10)") +
  facet_wrap(~year) #+ theme_sleek()
ggsave("Figures/SOG/rawpoints.pdf")


ggplot() +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  geom_point(data = sets, aes(longitude, latitude, col = log10(catch_count))) +
  scale_colour_viridis_c(option = "plasma", name = "Catch count (log10)")# +
#facet_wrap(~survey_abbrev + year + julian, ncol = 11) #+ theme_sleek()
ggsave("Figures/SOG/rawpoints.pdf")

ggplot(sets_sf, aes(colour = log10(ctch_cn ), fill = log10(ctch_cn ))) +
  geom_sf() +
  facet_wrap(~survey_abbrev + year + month, ncol = 11) +
  scale_colour_viridis_c(option = "plasma") +
  scale_fill_viridis_c(option = "plasma")
ggsave("Figures/SOG/catchraw_yearmonth.pdf")

glimpse(samps3)
samps4 <- filter(samps3, survey_abbrev == "OTHER")
samps_sum <- samps3 %>% group_by()
ggplot(samps3, aes(colour = log10(catch_count), fill = catch_count)) +
  geom_sf() +
  facet_wrap(~survey_abbrev + year + month, ncol = 11) +
  scale_colour_viridis_c(option = "plasma") +
  scale_fill_viridis_c(option = "plasma")
ggsave("Figures/SOG/catchraw_yearmonth.pdf")



x <- filter(sets2, survey_abbrev != "SYN SOG")
ggplot() +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  geom_point(data = x, aes(longitude, latitude, col = julian), size = 1) +
  scale_colour_viridis_c(option = "plasma", name = "Julian") +
  facet_wrap(~survey_abbrev + year, ncol = 10) #+ theme_sleek()
ggsave("Figures/SOG/Julian_rawpoints.pdf")


```



Prediction Grid for HBLL INS S 
```{r}
# processing done in QGIS - EEZ file intersected with 1000m bathymetry.
shelf <- st_read("data", "Shelf_polygon")
plot(st_geometry(shelf))
shelf2 <- st_transform(shelf, crs = projcrs)
plot(st_geometry(shelf2))

#hbll <- filter(both, survey_abbrev == "HBLL INS S" | survey_abbrev == "HBLL INS N")

#hbll <- filter(both, survey_abbrev == "HBLL INS S")
#projcrs <-  "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs"
#df <- st_as_sf(hbll,    coords = c("UTM.lon.m", "UTM.lat.m"), crs = projcrs)

df <- st_transform(sets_sf, crs = projcrs)
plot(st_geometry(df), add = TRUE)               
st_write(df, "output/SOGpoints.shp")

shelf <- st_read("data", "SPG_polygon_HBLLINSS_2")
plot(st_geometry(shelf))
shelf2 <- st_transform(shelf, crs = projcrs)
plot(st_geometry(shelf2))



# shelf_SOG <- st_read("data", "SOG_polygon")
# plot(st_geometry(shelf_SOG), col = "red")
# shelf2 <- st_transform(shelf_SOG, "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
# plot(st_geometry(shelf2), col = "red")

# Create grid that covers the BC coast polygon/jurisdiction.
grid_spacing <- 500 # 1 meters grid

polygony <- st_make_grid(shelf2, square = T, cellsize = c(grid_spacing, grid_spacing)) %>%
  st_sf() %>%
  mutate(cell_ID = row_number())
plot(st_geometry(polygony))

center <- st_centroid(polygony)
# grid_extent <- st_intersection(st_geometry(shelf2), st_geometry(polygony))
grid_extent <- st_intersection(shelf2, polygony)
st_crs(grid_extent) <- "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs"
plot(st_geometry(grid_extent))
center_extent <- st_intersection(st_geometry(shelf2), st_geometry(center))
plot(st_geometry(center_extent), add = TRUE)

grid_extent2 <- st_sf(grid_extent)
center_extent2 <- st_sf(center_extent)
plot(st_geometry(grid_extent2))
plot(st_geometry(center_extent2), add = TRUE)

grid_extent2$area_km <- st_area(grid_extent2) / 1000000 # 4km grid cells
st_write(grid_extent2, "output/PredictionGrid_SOG_halfkm.shp", append = FALSE)
st_write(center_extent2, "output/PredictionGridCentres_SOG_halfkm.shp", append = FALSE)
nrow(grid_extent)
grid_extent2$cell_ID <- seq(1,nrow(grid_extent),1)
saveRDS(grid_extent2, "output/grid_extentSOG.rds")

# # 
# # library(PBSmapping)
# # # citation("PBSmapping")
# # data(bcBathymetry) # bathymetry
# # contour(bcBathymetry$x, bcBathymetry$y, bcBathymetry$z, col = "pink", method = "edge", vfont = c("sans serif", "plain"))
# # 
# # cont <- contourLines(bcBathymetry$x, bcBathymetry$y, bcBathymetry$z, nlevels = 1000)
# # clines <- maptools::ContourLines2SLDF(cont)
# # clines@data[["level"]]
# # lines <- st_as_sf(clines) # make sf object
# # plot(lines)
# # st_write(lines, "output/contours_test.shp")
# # 
# # st_crs(lines) <- CRS("+proj=longlat")
# # test
# # c.linesproj <- st_transform(lines, crs = "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
# # plot(c.linesproj)
# # gridarea <- st_intersection(st_geometry(shelf2), st_geometry(c.linesproj)) %>% st_sfc(crs = "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
# # plot(gridarea)
# # gridarea2 <- st_collection_extract(gridarea, "LINESTRING")
# # st_write(gridarea2, "output/Contours_SOG.shp", append = FALSE)
# # plot(gridarea2)
# 
# 
# # Overlap the grid and the contour to get depth per grid point
# bathymetry <- st_read("output/Contours_SOG.shp")
# prediction_grid <- st_read("output/PredictionGrid_SOG.shp")
# plot(prediction_grid)
prediction_center <- st_read("output/PredictionGridCentres_SOG_wdepths_1km.shp")
glimpse(prediction_center)
plot(st_geometry(prediction_center))
prediction_center2 <- st_transform(prediction_center, crs = "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")

centerdf <- prediction_center2 %>%
  mutate(
    UTM.lon.m = unlist(map(geometry, 1)),
    UTM.lat.m = unlist(map(geometry, 2))
  ) %>%
  mutate(
    UTM.lon = UTM.lon.m / 1000,
    UTM.lat = UTM.lat.m / 1000
  ) %>%
  mutate(
    value = seq(1, nrow(prediction_center2))
  )


# lat and longs for each year to predict on
centerdf2 <- expand.grid(centerdf$value, years)
names(centerdf2) <- c("value", "year")
centerdf3 <- left_join(centerdf2, centerdf, by = c("value" = "value"))
unique(centerdf3$year)

# see which point are the ones that have positive depths
#centerdf4 <- rename(centerdf4, depth_m = bathy_extr)
centerdf3 <- rename(centerdf3, depth_m = depth_m1)
centerdf4 <- filter(centerdf3, is.na(depth_m) == FALSE ) #bathy_extr
range(centerdf4$depth_m)

depth_predictiongrid <- filter(centerdf4, depth_m < -4)
min(depth_predictiongrid$depth)
plot(depth_predictiongrid$UTM.lon, depth_predictiongrid$UTM.lat) # prediction grid points with depth > 0


depth_predictiongrid$depth_m <- depth_predictiongrid$depth_m * -1
saveRDS(depth_predictiongrid, "output/predictiongrid_bccoast_SOG_1km.rds")

centertrans5 <- readRDS("output/predictiongrid_bccoast_SOG_1km.rds")
centertrans5 <- readRDS("output/predictiongrid_bccoast_SOG.rds")


```


Mesh ignore for now
```{r}
glimpse(sets2)

p <- ggplot(data = filter(sets2, survey_abbrev == "HBLL INS S"), aes(longitude, latitude, col = catch_count, label = fishing_event_id), size = 1) + geom_point() +  ylim(48, 48.6) + xlim(-125, -123) 
p + geom_text()


test <- filter(sets2, longitude < -123 & longitude > -125)
test2 <- filter(test, latitude > 48.2 & latitude < 48.6)

df4 <- filter(sets2, !is.na(julian))
pugetsound <- unique(test2$fishing_event_id)
df5 <- filter(df4, !fishing_event_id %in% pugetsound )
p <- ggplot(data = filter(df5, survey_abbrev == "HBLL INS S"), aes(longitude, latitude, col = catch_count, label = fishing_event_id), size = 1) + geom_point()  
p + geom_text()

unique(df5$survey_abbrev)


df6 <- filter(df5, survey_abbrev != "SYN SOG" | survey_abbrev != "OTHER")
df7 <- sf::st_as_sf(df6, coords = c("longitude", "latitude")) %>% st_sf()
st_crs(df7) <- CRS("+proj=longlat")
df8 <- st_transform(df7, crs = 26909)

df9 <- df8 %>% ungroup() %>% 
  mutate(
    UTM.lon.m = unlist(map(df8$geometry, 1)),
    UTM.lat.m = unlist(map(df8$geometry, 2))
  ) %>%
  mutate(
    UTM.lon = UTM.lon.m / 1000,
    UTM.lat = UTM.lat.m / 1000
  ) %>%
  filter(!is.na(depth_m)) %>%
  ungroup()


st_geometry(df9) <- NULL

d1 <- sf::st_as_sf(d, coords = c("longitude", "latitude")) %>% st_sf()
st_crs(d1) <- CRS("+proj=longlat")
d2 <- st_transform(d1, crs = 26909)

d3 <- d2 %>% ungroup() %>% 
  mutate(
    UTM.lon.m = unlist(map(d2$geometry, 1)),
    UTM.lat.m = unlist(map(d2$geometry, 2))
  ) %>%
  mutate(
    UTM.lon = UTM.lon.m / 1000,
    UTM.lat = UTM.lat.m / 1000
  ) %>%
  filter(!is.na(depth_m)) %>%
  ungroup()


st_geometry(d3) <- NULL



d3
df9


unique(df9$survey_abbrev)
df10 <- filter(df9, survey_abbrev != "SYN SOG")
df10$offset <- log10(df10$hook_count) 
df12<- select(df10, year, fishing_event_id, offset, depth_m, catch_count,  survey_abbrev, julian, UTM.lon.m, UTM.lat.m, UTM.lat, UTM.lon)

glimpse(d3)
d3$offset <- log10(d3$lglsp_hook_count)
d3$catch_count <- d3$dogfish_count
d3$survey_abbrev <- "dogfishsurvey"
d5<- select(d3, year, fishing_event_id, offset, depth_m, catch_count, survey_abbrev, julian, UTM.lon.m, UTM.lat.m, UTM.lat, UTM.lon)

both <- rbind(df12, d5)  


mesh100 <- sdmTMB::make_mesh(both,
                             xy_cols = c("UTM.lon", "UTM.lat"),
                             n_knots = 100
)

plot(mesh100$mesh, asp = 1, main = "")
points(both$UTM.lon, both$UTM.lat, pch = ".", col = "red")

plot(both$UTM.lon, both$UTM.lat)

#saveRDS(bspdea, "output/barriermesh100_trawllongline.rds")
saveRDS(mesh100, "output/mesh100_SOG.rds")
mesh100 <- readRDS("output/mesh100_SOG.rds")

```


Mesh for HBLL INS S
```{r}

sets_sf2 <- st_read("output/HBLLINS_allpoints_wsite3.shp" )
plot(st_geometry(sets_sf2))
sets_sf3 <- st_transform(sets_sf2, crs = "+proj=utm +zone=9 +datum=WGS84 +units=m +no_defs")
str(unique(sets_sf3$site))
sets_sf <- filter(sets_sf3,is.na(site) == TRUE ) 
str(unique(sets_sf$site))
plot(st_geometry(sets_sf))

sets_sf4 <- sets_sf %>%
  mutate(
    UTM.lon.m = unlist(map(geometry, 1)),
    UTM.lat.m = unlist(map(geometry, 2))
  ) %>%
  mutate(
    UTM.lon = UTM.lon.m / 1000,
    UTM.lat = UTM.lat.m / 1000
  ) 


df4 <- filter(sets_sf4, !is.na(julian))
df4$offset <- log10(df4$hok_cnt  )
df6 <- filter(df4, year != 2021)
d5<- select(df6, year, fshng__ , offset, depth_m, ctch_cn , julian, UTM.lon.m, UTM.lat.m, UTM.lat, UTM.lon)

st_geometry(d5) <- NULL

mesh50 <- sdmTMB::make_mesh(d5,
                            xy_cols = c("UTM.lon", "UTM.lat"),
                            n_knots = 50
)

plot(mesh50$mesh, asp = 1, main = "")
points(d5$UTM.lon, d5$UTM.lat, pch = ".", col = "red")

saveRDS(mesh50, "output/mesh50_SOG.rds")
mesh50 <- readRDS("output/mesh50_SOG.rds")



```


HBLL INS S Index model

```{r HBLLINSS_model}

d5
plot(d5$UTM.lon,d5$UTM.lat)

m_dog_noyear <- sdmTMB(
  formula = ctch_cn   ~ 0 + s(depth_m, k=3) + s(julian, k=3),
  offset = d5$offset,
  data = d5,
  mesh = mesh50,
  spatiotemporal = "RW",
  time = "year",
  silent = TRUE,
  # anisotropy = TRUE,
  family = tweedie(link = "log"),
  spatial = TRUE
)
max(m_dog_noyear$gradients)
#m_dog <- sdmTMB::run_extra_optimization(m_dog)



m_dog <- sdmTMB(
  formula = ctch_cn   ~ 0 + s(depth_m, k=3) + as.factor(year) ,
  offset = d5$offset,
  data = d5,
  mesh = mesh50,
  spatiotemporal = "AR1",
  time = "year",
  silent = TRUE,
  # anisotropy = TRUE,
  family = tweedie(link = "log"),
  spatial = TRUE
)
max(m_dog$gradients)
m_dog <- sdmTMB::run_extra_optimization(m_dog)


m_dog_julian <- sdmTMB(
  formula = ctch_cn   ~ 0 + s(depth_m, k=3) + as.factor(year) + s(julian, k=3),
  offset = d5$offset,
  data = d5,
  mesh = mesh50,
  spatiotemporal = "RW",
  time = "year",
  silent = TRUE,
  # anisotropy = TRUE,
  family = tweedie(link = "log"),
  spatial = TRUE
)
max(m_dog_julian$gradients)
m_dog_julian <- sdmTMB::run_extra_optimization(m_dog_julian)

m_dog_rw <- sdmTMB(
  formula = ctch_cn   ~ 0 + s(depth_m, k=3) + as.factor(year) ,
  offset = d5$offset,
  data = d5,
  mesh = mesh50,
  spatiotemporal = "RW",
  time = "year",
  silent = TRUE,
  # anisotropy = TRUE,
  family = tweedie(link = "log"),
  spatial = TRUE
)
max(m_dog_rw$gradients)
m_dog_rw <- sdmTMB::run_extra_optimization(m_dog_rw)

m_dog_rw_noyear <- sdmTMB(
  formula = ctch_cn   ~ 0 + s(depth_m, k=3)  ,
  offset = d5$offset,
  data = d5,
  mesh = mesh50,
  spatiotemporal = "RW",
  time = "year",
  silent = TRUE,
  # anisotropy = TRUE,
  family = tweedie(link = "log"),
  spatial = TRUE
)
max(m_dog_rw_noyear$gradients)
m_dog_rw_noyear <- sdmTMB::run_extra_optimization(m_dog_rw_noyear)


m_dog_rw_nodepth <- sdmTMB(
  formula = ctch_cn   ~ 0 + as.factor(year)  ,
  offset = d5$offset,
  data = d5,
  mesh = mesh50,
  spatiotemporal = "RW",
  time = "year",
  silent = TRUE,
  # anisotropy = TRUE,
  family = tweedie(link = "log"),
  spatial = TRUE
)
max(m_dog_rw_nodepth$gradients)
m_dog_rw_nodepth <- sdmTMB::run_extra_optimization(m_dog_rw_nodepth)


m_dog_rw_nosp <- sdmTMB(
  formula = ctch_cn   ~ 0 + s(depth_m, k=3) + as.factor(year) ,
  offset = d5$offset,
  data = d5,
  mesh = mesh50,
  spatiotemporal = "RW",
  time = "year",
  silent = TRUE,
  # anisotropy = TRUE,
  family = tweedie(link = "log"),
  spatial = FALSE
)
max(m_dog_rw_nosp$gradients)
m_dog_rw_nosp <- sdmTMB::run_extra_optimization(m_dog_rw_nosp)


m_dog_iid <- sdmTMB(
  formula = ctch_cn  ~ 0 + s(depth_m, k=3) + as.factor(year) ,
  offset = d5$offset,
  data = d5,
  mesh = mesh50,
  spatiotemporal = "IID",
  time = "year",
  silent = FALSE,
  # anisotropy = TRUE,
  family = tweedie(link = "log"),
  spatial = TRUE
)
max(m_dog_iid$gradients)
m_dog_iid <- sdmTMB::run_extra_optimization(m_dog_iid)
#saveRDS(m_dog, file = f)


m_dog_nb <- sdmTMB(
  formula = ctch_cn  ~ 0 + s(depth_m, k=3) + as.factor(year) ,
  offset = d5$offset,
  data = d5,
  mesh = mesh50,
  spatiotemporal = "IID",
  time = "year",
  silent = FALSE,
  # anisotropy = TRUE,
  family = nbinom2(link = "log"),
  spatial = TRUE
)
max(m_dog_nb$gradients)
m_dog_nb <- sdmTMB::run_extra_optimization(m_dog_nb)
#saveRDS(m_dog, file = f)

m_dog_p <- sdmTMB(
  formula = ctch_cn  ~ 0 + s(depth_m, k=3) + as.factor(year) ,
  offset = d5$offset,
  data = d5,
  mesh = mesh50,
  spatiotemporal = "IID",
  time = "year",
  silent = FALSE,
  # anisotropy = TRUE,
  family = poisson(link = "log"),
  spatial = TRUE
)
max(m_dog_p$gradients)
m_dog_p <- sdmTMB::run_extra_optimization(m_dog_p)
#saveRDS(m_dog, file = f)




#see coefficients
tidy(m_dog, "ran_pars", conf.int = TRUE)
tidy(m_dog_ar1, "ran_pars", conf.int = TRUE)
tidy(m_dog_nb, "ran_pars", conf.int = TRUE)
tidy(m_dog_p, "ran_pars", conf.int = TRUE)
tidy(m_dog_rw, "ran_pars", conf.int = TRUE)

####extract aic
extractAIC.sdmTMB(m_dog_julian)
extractAIC.sdmTMB(m_dog_noyear)
extractAIC.sdmTMB(m_dog)
extractAIC.sdmTMB(m_dog_rw)
extractAIC.sdmTMB(m_dog_rw_noyear)
extractAIC.sdmTMB(m_dog_rw_nodepth)
extractAIC.sdmTMB(m_dog_iid)
extractAIC.sdmTMB(m_dog_nb)
extractAIC.sdmTMB(m_dog_p)
extractAIC.sdmTMB(m_dog_rw_nosp)



m_dog <- m_dog_iid

# m_dog
s <- simulate(m_dog, nsim = 300)
sum(s == 0) / length(s)

pred <- m_dog$family$linkinv(predict(m_dog)$est_non_rf)

r <- DHARMa::createDHARMa(
  simulatedResponse = s,
  observedResponse = d5$ctch_cn ,
  fittedPredictedResponse = pred
)
resid_plot <- DHARMa::testResiduals(r)
DHARMa::testSpatialAutocorrelation(m_dog, x = d5$ctch_cn, y = d5$ctch_cn)



#prediction grid m_dog
predgrid <- readRDS("output/predictiongrid_bccoast_SOG_1km.rds")
predgrid$offset <- 0
year <- unique(d5$year)
predgrid5 <- purrr::map_dfr(year, function(.x) {dplyr::mutate(predgrid, year = .x)})
range(d5$UTM.lat.m)
predgrid6 <- filter(predgrid5, UTM.lat.m >= 5382148 & UTM.lat.m <=  5582818)
range(d5$depth_m)
predgrid6 <- filter(predgrid6, depth_m >= 30 & depth_m <= 110)
predgrid7 <- predgrid6 %>% mutate(UTM.lat = UTM.lat.m/1000, UTM.lon = UTM.lon.m/1000)
#predgrid7$julian <- mean(d5$julian)
plot(predgrid7$UTM.lon, predgrid7$UTM.lat)
points(d5$UTM.lon, d5$UTM.lat, col = "red")

pred <- predict(m_dog, newdata = predgrid7, return_tmb_object = TRUE)
saveRDS(pred, "output/pred_indices/pred_m_dog_SOG.rds")
index3 <- get_index(pred, bias_correct = TRUE)
saveRDS(index3, "output/pred_indices/index_m_dog_HBLLINSS.rds")


ggplot(index3, aes(year, est)) +
  geom_line( size = 2, col = "#1B9E77") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#1B9E77") +
  xlab("Year") +
  ylab(paste0("Index of abundance")) +
  theme_sleek()
ggsave("Figures/Index_HBLLINSS.jpg")

x <- ggplot(index, aes(year, est)) +
  geom_line(col = "black", size = 2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "gray30") +
  xlab("Year") +
  ylab(paste0("Index of abundance")) +
  theme_sleek()
y <- x + geom_line(data = index2, aes(year, est), col = "#D95F02", size = 2) +
  geom_ribbon(data = index2, aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#D95F02")
y + geom_line(data = index3, aes(year, est), col = "#66A61E", size = 2) +
  geom_ribbon(data = index3, aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "#66A61E")


index3$est_scale <- scale(index3$est)
mean <- mean(index3$est) 
sd <- sd(index3$est) 
index3$lwr_scale <- (index3$lwr-mean)/sd
index3$upr_scale <- (index3$upr-mean)/sd


x <- ggplot(index, aes(year, est_scale)) +
  geom_line(col = "black", size = 2) +
  geom_ribbon(aes(ymin = lwr_scale, ymax = upr_scale), alpha = 0.4, fill = "gray30") +
  xlab("Year") +
  ylab(paste0("Index of abundance")) +
  theme_sleek()
y <- x + geom_line(data = index2, aes(year, est_scale), col = "#D95F02", size = 2) +
  geom_ribbon(data = index2, aes(ymin = lwr_scale, ymax = upr_scale), alpha = 0.4, fill = "#D95F02")
y + geom_line(data = index3, aes(year, est_scale), col = "#66A61E", size = 2) +
  geom_ribbon(data = index3, aes(ymin = lwr_scale, ymax = upr_scale), alpha = 0.4, fill = "#66A61E")  


```





```{r model}

both2 <- filter(both, survey_abbrev == "HBLL INS N" | survey_abbrev == "HBLL INS S")
both2 <- filter(both, survey_abbrev == "HBLL INS S") 
both2 <- filter(both, survey_abbrev == "HBLL INS N" )


mesh100 <- sdmTMB::make_mesh(both2,
                             xy_cols = c("UTM.lon", "UTM.lat"),
                             n_knots = 100
)

plot(mesh100$mesh, asp = 1, main = "")
points(both2$UTM.lon, both2$UTM.lat, pch = ".", col = "red")

plot(both2$UTM.lon, both2$UTM.lat)

#saveRDS(mesh100, "output/mesh100_SOG.rds")
#mesh100 <- readRDS("output/mesh100_SOG.rds")

m_dog_sog <- sdmTMB(
  formula = catch_count ~ 0 +  offset  + as.factor(survey_abbrev) + poly(depth_m, 2) + poly(julian,2),
  data = both2,
  mesh = mesh100,
  spatiotemporal = "AR1",
  time = "year",
  silent = FALSE,
  family = poisson(link = "log"),
  spatial = TRUE
)


m_dog_sog2 <- sdmTMB(
  formula = catch_count ~ 0 +  offset  + poly(depth_m, 2),
  data = both2,
  mesh = mesh100,
  spatiotemporal = "AR1",
  time = "year",
  silent = FALSE,
  family = poisson(link = "log"),
  spatial = TRUE
)

m_dog_sog3 <- sdmTMB(
  formula = catch_count ~ 0 +  offset  + poly(depth_m, 2) + julian,
  data = both2,
  mesh = mesh100,
  spatiotemporal = "AR1",
  time = "year",
  silent = FALSE,
  family = poisson(link = "log"),
  spatial = TRUE
)

max(m_dog_sog3$gradients) 
m_dog_sog3 <- sdmTMB::run_extra_optimization(m_dog_sog3)
saveRDS(m_dog_sog3, file = "models/dogfish-SOG3.rds")

both2$julianscale <- scale(both2$julian)
#both2$depth_scaled <- scale(both2$depth_m)
m_dog_sog4 <- sdmTMB(
  formula = catch_count ~ 0 +  offset  + poly(depth_m, 2) + as.factor(survey_abbrev) +  julian,
  data = both2,
  mesh = mesh100,
  spatiotemporal = "AR1",
  time = "year",
  silent = FALSE,
  family = nbinom2(link = "log"),
  spatial = TRUE
)
max(m_dog_sog4$gradients) 
m_dog_sog4 <- sdmTMB::run_extra_optimization(m_dog_sog4)
saveRDS(m_dog_sog4, file = "models/dogfish-SOG4.rds")



m_dog_sog5 <- sdmTMB(
  formula = catch_count ~ 0 +  offset  + poly(depth_m, 2) + as.factor(survey_abbrev),
  data = both2,
  mesh = mesh100,
  spatiotemporal = "AR1",
  time = "year",
  silent = FALSE,
  family = nbinom2(link = "log"),
  spatial = TRUE
)
max(m_dog_sog5$gradients) 
m_dog_sog5 <- sdmTMB::run_extra_optimization(m_dog_sog5)
saveRDS(m_dog_sog5, file = "models/dogfish-SOG5.rds")



nd <- expand.grid(julian =
                    seq(min(both2$julian), max(both2$julian), length.out = 100))
nd$depth_m <- mean(both2$depth_m)
nd$offset <- mean(both2$offset)
nd$year <- 2021L # L: integer to match original data
nd$survey_abbrev = "HBLL INS S"
p <- predict(m_dog_sog4, newdata = nd, se_fit = TRUE, re_form = NA)
ggplot(p, aes(julian, est,
              ymin = I(est - 1.96 * est_se), ymax = I(est + 1.96 * est_se))) +
  geom_line() + geom_ribbon(alpha = 0.4) 




```


```{r}

extractAIC.sdmTMB(m_dog_sog)
extractAIC.sdmTMB(m_dog_sog2)
extractAIC.sdmTMB(m_dog_sog3)
extractAIC.sdmTMB(m_dog_sog4)
extractAIC.sdmTMB(m_dog_sog5)

```



```{r}

#prediction grid for julian yearmonth model
pred_grid_scaled <- readRDS("output/predictiongrid_bccoast_SOG.rds")
pred_grid_scaled <- filter(pred_grid_scaled, lat > 50.2 & lon < -126) #north
pred_grid_scaled <- filter(pred_grid_scaled, lat < 50.2 & lon > -126)

plot(pred_grid_scaled$UTM.lon.m, pred_grid_scaled$UTM.lat.m)
# pred_grid_scaled <- readRDS( "output/grid_extentSOG.rds")
#st_geometry(pred_grid_scaled) <- NULL
max(both2$depth_m)
min(both2$depth_m)


predgrid2 <- filter(pred_grid_scaled,depth_m <= 150 ) %>%
  distinct(UTM.lat.m, UTM.lon.m,  .keep_all = TRUE)
predgrid2 <- filter(predgrid2, depth_m >=24)
year <- unique(both2$year)
unique(both2$julian)
predgrid2$offset <- mean(both2$offset)
predgrid2$julian <- mean(both2$julian)
predgrid2$UTM.lat <- predgrid2$UTM.lat.m/1000
predgrid2$UTM.lon <- predgrid2$UTM.lon.m/1000
predgrid2$survey_abbrev <- "HBLL INS S"

predgrid5 <- purrr::map_dfr(year, function(.x) {dplyr::mutate(predgrid2, year = .x)})
plot(predgrid5$UTM.lat.m, predgrid5$UTM.lon.m)

pred <- predict(m_dog_sog5, newdata = predgrid5, return_tmb_object = TRUE)
saveRDS(pred, "output/pred_indices/pred_sog.rds")
index <- get_index(pred, bias_correct = TRUE)
saveRDS(index, "output/pred_indices/index_sog.rds")

pred <- predict(m_dog_sog4, newdata = predgrid5, return_tmb_object = TRUE)
saveRDS(pred, "output/pred_indices/pred_sog.rds")
index <- get_index(pred, bias_correct = TRUE)
saveRDS(index, "output/pred_indices/index_sog.rds")

ggplot(index, aes(year, est)) +
  geom_line(col = "dark blue") +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4, fill = "blue") +
  xlab("Year") +
  ylab(paste0("Index of abundance")) +
  theme(
    plot.background = element_rect(fill = "white", colour = "NA"),
    # text = element_text(family= "Gill Sans MT"),
    axis.line.x = element_line(colour = "grey60"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    panel.background =  element_rect(fill = "white",colour = "grey50"),
    axis.text.x = element_text(size = 20, vjust = 1, colour = "grey20"),
    axis.text.y = element_text(size = 20, colour = c("grey20")),
    axis.title.x = element_text(size = 20, colour = "grey20"),
    axis.title.y = element_text(size = 20, colour = "grey20"),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks.x = element_line(colour = "grey60"),
    axis.ticks.y = element_line(colour = "grey60")
  )



```

