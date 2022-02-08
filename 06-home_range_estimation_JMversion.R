# code run using R version 4.0.2 (2020-06-22) -- "Taking Off Again"

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(sp)
library(scales)
library(adehabitatHR)
library(here)

# Data --------------------------------------------------------------------

##pulling the 'fulltrap' dataset that was created with all the datacleaning and I wrote to a Rdata file
locations <-  readRDS(file = "fulltrap_11.18.21.rds")
##I don't know what the 'no_overlap" is or what it means
# locations_no_overlap <- read_csv("data/output/05-combined_logger_and_field_location_data-fixed-coords-removed-records-of-multiple-grids.csv")


# Do we need to consider animals found across multiple years? -------------

#how many observations do we have split over different years?

# locations <- locations %>%
#   mutate(year = format(date, format = "%Y")) 
# 
# locations_no_overlap <- locations_no_overlap %>%
#   mutate(year = format(date, format = "%Y")) 
# 
# locations %>%
#   group_by(tag, year) %>%
#   count() %>%
#   group_by(tag) %>%
#   pivot_wider(names_from = occasion, values_from = n) %>%
#   drop_na()
# 
# locations_no_overlap %>%
#   group_by(tag, year) %>%
#   count() %>% 
#   group_by(tag) %>%
#   pivot_wider(names_from = year, values_from = n) %>%
#   drop_na()
# 
# # let's filter these observations - for animals split over multiple years,
# # filter out the observations with fewer observations
# locations <- locations %>%
#   dplyr::filter(!(tag == "041F000387" & year == "2020"),
#                 !(tag == "041F0003EE" & year == "2020"),
#                 !(tag == "041F000578" & year == "2020"),
#                 !(tag == "041F0005D8" & year == "2019"))
# 
# locations_no_overlap <- locations_no_overlap %>%
#   dplyr::filter(!(tag == "041F000387" & year == "2020"),
#                 !(tag == "041F0003EE" & year == "2020"),
#                 !(tag == "041F0005D8" & year == "2019"))

 
# Data filtering - number of sites and observations -----------------------
# plot these out

#plot histogram of count of # different locations seen
theme_set(theme_bw())
locations %>%
  group_by(tag) %>%
  summarise(number_of_different_locations_seen = length(unique(trap))) %>%
  ggplot(aes(x = number_of_different_locations_seen)) +
  geom_histogram(stat="count") +
  scale_x_continuous(breaks=c(1:20))

# locations_no_overlap %>%
#   group_by(tag) %>%
#   summarise(number_of_different_locations_seen = length(unique(full_trap_id))) %>%
#   ggplot(aes(x = number_of_different_locations_seen)) +
#   geom_histogram(stat="count") +
#   scale_x_continuous(breaks=c(1:20))

# locations %>%
#   mutate(latlon = paste0(lat, lon)) %>%
#   group_by(tag) %>%
#   summarise(number_of_different_locations_seen = length(unique(latlon))) %>%
#   ggplot(aes(x = number_of_different_locations_seen)) +
#   geom_histogram(stat="count") +
#   scale_x_continuous(breaks=c(1:20))

#write a df of count of animals seen at _# of locations
sum_locs_seen <- locations %>%
  group_by(tag) %>%
  summarise(number_of_different_locations_seen = length(unique(trap))) %>%
  group_by(number_of_different_locations_seen) %>%
  count()


# count()
# ggplot(aes(x = number_of_different_locations_seen)) +
#   geom_histogram(stat="count") +
#   scale_x_continuous(breaks=c(1:20))

# adehabitatHR needs at least 5 observations per animal
# we'll filter out any animals with less than 5 observations here
# add column called 'n' with number of observations per tag, use it to filter, then remove it
locations <- locations %>%
  group_by(tag) %>%
  add_tally() %>%
  ungroup() %>% 
  filter(n >= 5) %>% 
  dplyr::select(-n)

# locations_no_overlap <- locations_no_overlap %>%
#   group_by(tag) %>%
#   add_tally() %>%
#   ungroup() %>%
#   filter(n > 5) %>%
#   dplyr::select(-n)

#how many animals are there that were captured at least 5 times
locations %>%
  group_by(tag) %>%
  summarise(individual_animals = length(unique(tag))) %>%
  count()
#121 animals

#can we see that by grid
locations %>%
  group_by(site) %>%
  summarise(n_distinct(tag))
#not horrible, 8 sites have at least 9 animals seen 5 or more times
#poor kuoppa, still only 2 :(


# In addition, for KDM (as opposed to MCP) needs at least 2 different locations of observation
# need only animals found in more than one place (e.g. 2 different traps)

# how many do we have at over 1 or over 2 different locations?
locations %>%
  group_by(tag) %>%
  summarise(number_of_different_locations_seen = length(unique(trap))) %>%
  dplyr::filter(number_of_different_locations_seen > 1) %>%
  count()

#118 animals at more than one location, 108 @ more than two
#3 animals only seen at 1 location  --  filter(number_of_different_locations_seen == 1)


# let's add jittered point for supp. materials
# Add a jittered location
# jitter points

locations_jittered <- locations %>%
  mutate(x_jitter = jitter(x),
         y_jitter = jitter(y),
         rand = runif(nrow(.)),
         trap_jitter = paste0(trap, rand)) 

locations <- locations %>%
  group_by(tag) %>%
  summarise(number_of_different_locations_seen = length(unique(trap))) %>%
  right_join(locations, by = "tag") %>% 
  dplyr::filter(number_of_different_locations_seen > 1)

# locations_no_overlap <- locations_no_overlap %>%
#   group_by(tag) %>%
#   summarise(number_of_different_locations_seen = length(unique(full_trap_id))) %>%
#   right_join(locations_no_overlap, by = "tag") %>% 
#   # I've done over 2 here, but could be >1 or any number
#   dplyr::filter(number_of_different_locations_seen > 1)

locations_jittered <- locations_jittered %>%
  group_by(tag) %>%
  summarise(number_of_different_locations_seen = length(unique(trap_jitter))) %>% 
  right_join(locations_jittered, by = "tag") 
  dplyr::filter(number_of_different_locations_seen > 1)

# how many do we have at > 1 different location?
locations_jittered %>%
    group_by(tag) %>%
    summarise(number_of_different_locations_seen = length(unique(trap_jitter))) %>%
    dplyr::filter(number_of_different_locations_seen > 1) %>%
    count() 
#if we jitter the trap locations, we can keep three extra animals in the KDE analysis 
#because now they've been found at >1 location

############## JM stopped here - already done much of the following code in 'mcp_musings.R' #######################
################### going to create a new document (kde_heatmaps.R) that pulls that and this together ##############################

# Home range KDM method ---------------------------------------------------

#converting into metres
locations$X_dummy_m <- locations$X_dummy * 10
locations$Y_dummy_m <- locations$Y_dummy * 10

locations_no_overlap$X_dummy_m <- locations_no_overlap$X_dummy * 10
locations_no_overlap$Y_dummy_m <- locations_no_overlap$Y_dummy * 10

locations_no_overlap_jittered$X_dummy_m <- locations_no_overlap_jittered$X_dummy_jitter * 10
locations_no_overlap_jittered$Y_dummy_m <- locations_no_overlap_jittered$Y_dummy_jitter * 10

# minimise our dataframe to format needed
locations.sp <- dplyr::select(locations, id = tag, x = X_dummy_m, y = Y_dummy_m)
# locations.sp.nooverlap <- dplyr::select(locations_no_overlap, id = tag, x = X_dummy_m, y = Y_dummy_m)
locations.sp.nooverlap_jitter <- dplyr::select(locations_no_overlap_jittered, id = tag, x = X_dummy_m, y = Y_dummy_m)

# check there's no missing locations
locations.sp <- locations.sp %>% filter(!is.na(x) & !is.na(y))
# locations.sp.nooverlap <- locations.sp.nooverlap %>% filter(!is.na(x) & !is.na(y))
locations.sp.nooverlap.jitter <- locations.sp.nooverlap_jitter %>% filter(!is.na(x) & !is.na(y))

coordinates(locations.sp) <- c("x", "y")
proj4string(locations.sp) <- CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs")

# coordinates(locations.sp.nooverlap) <- c("x", "y")
# proj4string(locations.sp.nooverlap) <- CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs")

coordinates(locations.sp.nooverlap.jitter) <- c("x", "y")
proj4string(locations.sp.nooverlap.jitter) <- CRS("+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs")

# All commented out now to avoid re-calculating as they're large - load the RDS instead
#kernel.ref <- kernelUD(locations.sp, h = "href", grid = 1000, same4all = TRUE) # href = the reference bandwidth
#kernel.ref.nooverlap <- kernelUD(locations.sp.nooverlap, h = "href", grid = 1000, same4all = TRUE) 
#kernel.ref.nooverlap.jitter <- kernelUD(locations.sp.nooverlap.jitter, h = "href", grid = 1000, same4all = TRUE)

#saveRDS(kernel.ref, file = "data/output/06-kernel.ref")
#saveRDS(kernel.ref.nooverlap, file = "data/output/06-kernel.ref.nooverlap")
#saveRDS(kernel.ref.nooverlap.jitter, file = "data/output/06-kernel.ref.nooverlap.jitter")

kernel.ref <- readRDS("data/output/06-kernel.ref")
# kernel.ref.nooverlap <- readRDS("data/output/06-kernel.ref.nooverlap")
kernel.ref.nooverlap.jitter <- readRDS("data/output/06-kernel.ref.nooverlap.jitter")

# Home Range Estimation  --------------------------------------------------
areas <- kernel.area(kernel.ref, percent=seq(50, 95, by=5), unin="m", unout="m2")
areas_no_overlap <- kernel.area(kernel.ref.nooverlap, percent=seq(50, 95, by=5), unin="m", unout="m2")
areas_no_overlap_jitter <- kernel.area(kernel.ref.nooverlap.jitter, percent=seq(50, 95, by=5), unin="m", unout="m2")


# Plot homeranges? --------------------------------------------------------
kernel.ref.nooverlap.grid10 <- kernelUD(locations.sp.nooverlap, h = "href", grid = 1000, same4all = TRUE)


image(kernel.ref.nooverlap)
plotLSCV(kernel.ref)

par(mar=c(1,1,1,1))
plot.hrsize(areas)

# Clean up output ---------------------------------------------------------
areas_df <- areas %>% 
  t() %>%
  data.frame() %>%
  rownames_to_column(var = "tag") %>% 
  mutate(tag = str_remove(tag, "X")) %>% 
  dplyr::select(tag,
                fifty = X50, fiftyfive = X55, sixty = X60, sixtyfive = X65, seventy = X70,
                seventyfive = X75, eighty = X80, eightyfive = X85, nintety = X90, ninetyfive = X95) 

# areas_no_overlap_df <- areas_no_overlap %>% 
#   t() %>%
#   data.frame() %>%
#   rownames_to_column(var = "tag") %>% 
#   mutate(tag = str_remove(tag, "X")) %>%
#   dplyr::select(tag,
#                 fifty = X50, fiftyfive = X55, sixty = X60, sixtyfive = X65, seventy = X70,
#                 seventyfive = X75, eighty = X80, eightyfive = X85, nintety = X90, ninetyfive = X95) 
 
areas_no_overlap_jitter_df <- areas_no_overlap_jitter %>% 
  t() %>%
  data.frame() %>%
  rownames_to_column(var = "tag") %>% 
  mutate(tag = str_remove(tag, "X")) %>%
  dplyr::select(tag,
                fifty = X50, fiftyfive = X55, sixty = X60, sixtyfive = X65, seventy = X70,
                seventyfive = X75, eighty = X80, eightyfive = X85, nintety = X90, ninetyfive = X95) 


# Write out data ----------------------------------------------------------
write_csv(areas_df, "data/output/06-home_range_estimates.csv")
# write_csv(areas_no_overlap_df, "data/output/06-home_range_estimates_no_overlap.csv")
write_csv(areas_no_overlap_jitter_df, "data/output/06-home_range_estimates_no_overlap_jittered.csv")
