# Rockfish catches in 1986, and 1989


# library -----------------------------------------------------------------
library(sf)
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)
library(gfdata)


# Data pull  ------------------------------------------------------------

#pull from Dogfish_data_pull.R
d <- readRDS("output/dogfishs_allsets_allspecies_counts.rds")
dd <- filter(d, year == 1986)
unique(dd$species_science_name)

# Catch of Rockfish by depth by year --------------------------------------
# filter for rockfishes
drock <- filter(d, grepl("SEBASTES ", species_science_name, ignore.case = TRUE))


# Rockfish analysis -------------------------------------------------------
# how many rockfish captured each year
drock |>
  group_by(year, species_science_name) |>
  summarize(sitesum = sum(catch_count)) |>
  print()

# how many rockfish captured at each depth
ddepth <- drock |>
  mutate(grouping_depth_id = as.numeric(grouping_depth_id)) |>
  group_by(year, species_science_name, grouping_depth_id) |>
  summarize(depthsum = sum(catch_count))

ggplot(ddepth) +
  geom_point(aes(grouping_depth_id, depthsum, colour = species_science_name)) +
  geom_line(aes(grouping_depth_id, depthsum, colour = species_science_name)) +
  facet_wrap(~year) +
  theme_classic()



# dogfish -----------------------------------------------------------------
ddog <- filter(d, grepl("squalus", species_science_name, ignore.case = TRUE))
unique(ddog$species_common_name)

# how many dogfish captured each year
ddog |>
  group_by(year, species_science_name) |>
  summarize(sitesum = sum(catch_count)) |>
  print()

# how many rockfish catured at each depth
ddepth <- ddog |>
  mutate(grouping_depth_id = as.numeric(grouping_depth_id)) |>
  group_by(year, species_science_name, grouping_depth_id) |>
  summarize(depthsum = sum(catch_count))

ggplot(ddepth) +
  geom_point(aes(grouping_depth_id, depthsum, colour = species_science_name)) +
  geom_line(aes(grouping_depth_id, depthsum, colour = species_science_name)) +
  facet_wrap(~year) +
  theme_classic()


# sixgill -----------------------------------------------------------------
dsix <- filter(d, grepl("sixgill", species_common_name, ignore.case = TRUE))

# how many sixgills captured each year
dsix |>
  group_by(year, species_science_name) |>
  summarize(sitesum = sum(catch_count)) |>
  print()

# how many sixgills catured at each depth
dsix |>
  mutate(grouping_depth_id = as.numeric(grouping_depth_id)) |>
  group_by(year, species_science_name, grouping_depth_id) |>
  summarize(depthsum = sum(catch_count))
