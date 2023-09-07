
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
fe.FE_PARENT_EVENT_ID
comparison_info <- gfdata::run_sql("GFBioSQL", "SELECT
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
WHERE S.SURVEY_SERIES_ID IN (48/*, 76, 92, 93*/)
AND FE.FE_MAJOR_LEVEL_ID < 1000 AND FE_PARENT_EVENT_ID IS NULL
AND YEAR(TR.TRIP_START_DATE) IN (2004, 2022)
ORDER BY YEAR, TRIP_ID, FE_MAJOR_LEVEL_ID, FE_SUB_LEVEL_ID")

glimpse(comparison_info)

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
INNER JOIN GFBioSQL.dbo.ACTIVITY A ON A.ACTIVITY_CODE = TA.ACTIVITY_CODE
INNER JOIN GFBioSQL.dbo.SPECIES S ON S.SPECIES_CODE = B21.SPECIES_CODE
WHERE TA.ACTIVITY_CODE IN (39,53) AND B21.SPECIES_CODE = '044'
ORDER BY B21.TRIP_ID, B21.FE_MAJOR_LEVEL_ID, B22.SPECIMEN_ID")

test2 <- left_join(dsurvey_bio, info, by = ("FISHING_EVENT_ID" = "FE_PARENT_EVENT_ID"))
names(test2) <- tolower(names(test2))

pe2004 <- filter(dsurvey_bio, YEAR %in% c(2004)) |> rename("FEI" = "FE_PARENT_EVENT_ID")
pe2022 <- filter(dsurvey_bio, YEAR %in% c(2022)) |> rename("FEI" = "FE_PARENT_EVENT_ID")
fe <- filter(dsurvey_bio, !(YEAR %in% c(2004, 2022))) |> rename("FEI" = "FISHING_EVENT_ID")
info <- info |> rename("FEI" = "FISHING_EVENT_ID")
#if survey == 48
#join by 

test <- filter(pe2004 , FEI == 5490354)
test2 <- filter(info , FEI == 5490354)
test3 <- inner_join(test, test2)

pej <- inner_join(pe2004, info) |> select(-"FISHING_EVENT_ID", "FE_PARENT_EVENT_ID")
pej2 <- inner_join(pe2022, info) |> select(-"FISHING_EVENT_ID", "FE_PARENT_EVENT_ID") 
fej <- inner_join(fe, info)  |> select(-"FISHING_EVENT_ID", "FE_PARENT_EVENT_ID")
final <- rbind(pej, pej2, fej)
names(final) <- tolower(names(final))
unique(final$grouping_desc)

final |> 
  filter(grouping_desc ==  "SoG Dogfish Site") |> 
  summarize(unique(trip_comment))

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

# Summary statistics ------------------------------------------------------

names(dsurvey_bio) <- tolower(names(dsurvey_bio))
dsurvey_bio |>
  filter(year == 2022) |>
  group_by(fe_parent_event_id) |>
  distinct(fishing_event_id, .keep_all = TRUE) |>
  tally() # 2 hooks types for each string ie. fe_parent_event_id

names(dsurvey_bio)[names(dsurvey_bio) == "fe_parent_event_id"] <- "fishing_event"
names(dsurvey_bio)[names(dsurvey_bio) == "fishing_event_id"] <- "skate"
names(dsurvey_bio)[names(dsurvey_bio) == "fe_sub_level_id"] <- "hookcode"
names(dsurvey_bio)[names(dsurvey_bio) == "specimen_sex_code"] <- "sex"
unique(dsurvey_bio$hookcode)

dsurvey_bio <- dsurvey_bio |> 
  mutate(hookcode = ifelse(is.na(hookcode) == TRUE, 3, hookcode))
dsurvey_bio <- dsurvey_bio |>
  mutate(hooktype = ifelse(hookcode == 1, "HBLLgear", 
                    ifelse(hookcode == 2, "DOGgear",
                    "NA")))

test <- filter(dsurvey_bio, is.na(hookcode) == TRUE)
unique(test$trip_comment)

# Lengths per hook type ---------------------------------------------------
dsurvey_bio <- test
og <- dsurvey_bio

dsurvey_bio<- filter(dsurvey_bio, year == 2022)


filter(dsurvey_bio, is.na(total_length) == TRUE)
range(dsurvey_bio$total_length)

x <- dsurvey_bio |>
  filter(sex %in% c(1, 2))
x <- filter(x, is.na(total_length) != TRUE)
x <- filter(x, total_length > 0 )

x |>
  group_by(hooktype, sex, year) |>
  summarize(
    min = min(total_length), max = max(total_length), median = median(total_length),
    mean = mean(total_length)
  ) |> 
  print(n = 30)

x$event <- factor(x$fishing_event)
# are these statistically different?
ggplot() +
  geom_boxplot(data = x, aes(as.character(event), total_length,
    colour = as.factor(hooktype)
  )) +
  geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~sex + year, nrow = 2) +
  theme_classic()

ggplot() +
  geom_boxplot(data = x, aes(as.character(event), total_length,
                             colour = as.factor(hooktype)
  )) +
  geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~grouping_depth_id, nrow = 2) +
  theme_classic()

ggplot() +
  geom_boxplot(data = x, aes(as.character(event), total_length,
                             colour = as.factor(hooktype)
  )) +
  geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~sex + year, nrow = 2) +
  theme_classic()

ggplot() +
  geom_boxplot(data = x, aes(
    hooktype, total_length,
    colour = as.factor(hooktype), group = sex
  )) +
  geom_boxplot(position = position_dodge(1)) +
  facet_wrap(~event) +
  theme_classic()


ggplot(x, aes(as.factor(hookcode), total_length, group = as.factor(hooktype), colour = as.factor(hooktype))) +
  geom_jitter() +
  # geom_point(data = dsurvey_bio, aes(as.factor(hookcode), total_length, group = specimen_sex_code, colour = specimen_sex_code)) +
  facet_wrap(~sex) +
  theme_classic()

ggplot(x, aes(as.factor(hookcode), total_length, group = as.factor(hooktype), colour = as.factor(hooktype))) +
  geom_jitter() +
  # geom_point(data = dsurvey_bio, aes(as.factor(hookcode), total_length, group = specimen_sex_code, colour = specimen_sex_code)) +
  facet_wrap(~sex) +
  theme_classic()

ggplot() +
  geom_density(data = x, aes(total_length,
    group = as.factor(hooktype),
    colour = as.factor(hooktype)
  )) +
  facet_wrap(~sex) +
  geom_rug(data = x, aes(total_length,
    group = as.factor(hooktype),
    colour = as.factor(hooktype)
  )) +
  theme_classic()



# Maturities by depth -----------------------------------------------------


