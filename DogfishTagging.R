# Dogfish tagging ---------------------------------------------------------
recoveries <- read.csv("C:/Dogfish_stitch/Taggingdata/RECOVERIES.csv")
r2 <- select(recoveries, REC_MON, REC_DAY, REC_LONG, REC_LAT)
r2$long <- r2$REC_LONG * -1
r3 <- filter(r2, !is.na(r2$REC_LONG))
r2_sf <- sf::st_as_sf(r3, coords = c("long", "REC_LAT")) %>% st_sf()
st_crs(r2_sf) <- CRS("+proj=longlat")
map_data <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf")
bc_coast <- st_crop(
  map_data,
  c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)
)
bc_coast_proj <- sf::st_transform(bc_coast, crs = 26909)

r2_sf2 <- st_crop(
  r2_sf,
  c(xmin = -134, ymin = 46, xmax = -120, ymax = 57)
)
ggplot() +
  geom_sf(data = bc_coast, colour = "grey70", fill = "grey90") +
  geom_sf(data = r2_sf2) +
  facet_wrap(~REC_MON) +
  geom_jitter()


rsum <- recoveries %>%
  group_by(DEPTH, REC_MON) %>%
  tally()
rsum2 <- na.omit(rsum)
ggplot(rsum2, aes(DEPTH, n)) +
  geom_point() +
  geom_line() +
  facet_wrap(~REC_MON)

rsum <- recoveries %>%
  group_by(REC_LAT, REC_YR) %>%
  tally()
rsum2 <- na.omit(rsum)
ggplot(rsum2, aes(REC_LAT, n)) +
  geom_point() +
  geom_line() +
  facet_wrap(~REC_LAT)

rec2 <- filter(recoveries, !is.na(REC_LAT) & !is.na(REC_MON))
rec2 %>% ggplot(aes(REC_LAT)) +
  geom_histogram() +
  facet_wrap(~REC_MON)
rec2 %>% ggplot(aes(DEPTH)) +
  geom_histogram() +
  facet_wrap(~REC_MON)
rec2 %>% ggplot(aes(REC_LONG)) +
  geom_histogram() +
  facet_wrap(~REC_MON)
rec2 %>% ggplot(aes(REC_LAT, fill = as.factor(SEX), group = as.factor(SEX))) +
  geom_histogram() +
  facet_wrap(~REC_MON)
rec2 %>% ggplot(aes(DEPTH, fill = as.factor(SEX), group = as.factor(SEX))) +
  geom_histogram() +
  facet_wrap(~REC_MON)
rec2 %>% ggplot(aes(DEPTH, fill = as.factor(SEX), group = as.factor(SEX))) +
  geom_histogram() +
  facet_wrap(~REC_YR)
