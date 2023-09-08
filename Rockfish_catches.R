# Rockfish catches in 1986, and 1989


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




# Data pull  ------------------------------------------------------------

#pull from Dogfish_data_pull.R

d <- readRDS("output/dogfishs_allsets_allspecies.rds")


# Catch of Rockfish by depth by year --------------------------------------
d <- left_join(d, scodes2)
# drop the species without codes ###COME BACK TO  THIS  THOUGH

# filter for rockfishes
drock <- filter(d, grepl("rockfish", species_name, ignore.case = TRUE))
saveRDS(drock, "output/dogfishsql_rockfish.rds")



# Rockfish analysis -------------------------------------------------------

drock <- readRDS("output/dogfishsql_rockfish.rds")

# how many rockfish captured each year
drock |>
  group_by(year, species_name) |>
  summarize(sitesum = sum(catch_count)) |>
  print()

# how many rockfish catured at each depth
ddepth <- drock |>
  mutate(grouping_depth_id = as.numeric(grouping_depth_id)) |>
  group_by(year, species_name, grouping_depth_id) |>
  summarize(depthsum = sum(catch_count))

ggplot(ddepth) +
  geom_point(aes(grouping_depth_id, depthsum, colour = species_name)) +
  geom_line(aes(grouping_depth_id, depthsum, colour = species_name)) +
  facet_wrap(~year) +
  theme_classic()



# dogfish -----------------------------------------------------------------
ddog <- filter(d, grepl("dogfish", species_name, ignore.case = TRUE))

# how many rockfish captured each year
ddog |>
  group_by(year, species_name) |>
  summarize(sitesum = sum(catch_count)) |>
  print()

# how many rockfish catured at each depth
ddepth <- ddog |>
  mutate(grouping_depth_id = as.numeric(grouping_depth_id)) |>
  group_by(year, species_name, grouping_depth_id) |>
  summarize(depthsum = sum(catch_count))

ggplot(ddepth) +
  geom_point(aes(grouping_depth_id, depthsum, colour = species_name)) +
  geom_line(aes(grouping_depth_id, depthsum, colour = species_name)) +
  facet_wrap(~year) +
  theme_classic()





# sixgill -----------------------------------------------------------------
dsix <- filter(d, grepl("sixgill", species_name, ignore.case = TRUE))

# how many rockfish captured each year
dsix |>
  group_by(year, species_name) |>
  summarize(sitesum = sum(catch_count)) |>
  print()

# how many rockfish catured at each depth
ddepth <- dsix |>
  mutate(grouping_depth_id = as.numeric(grouping_depth_id)) |>
  group_by(year, species_name, grouping_depth_id) |>
  summarize(depthsum = sum(catch_count))
