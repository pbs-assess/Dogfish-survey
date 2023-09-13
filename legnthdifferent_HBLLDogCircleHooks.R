
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

#see Dogfish_data_pull.R 
#pull those save databases

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


