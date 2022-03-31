# code run using R version 4.0.2 (2020-06-22) -- "Taking Off Again"

#load libraries
library(here)
library(tidyverse)
library(rgdal)
library(sp)
library(adehabitatHR)

#clear environment
rm(list = ls())

#load the fulltrap dataset (make sure it's the most recent version)
fulltrap <- readRDS(file = "fulltrap_12.30.21.rds")


################################ new things 12.1.21  ####################################

#clean fulltrap for use in mcp analysis
mcp_trap <- fulltrap %>%
  group_by(site, tag) %>%
  mutate(cap.freq = n()) %>% #create column for capture frequency (# times seen)
  filter(cap.freq >= 5) #filter for at least 5 captures


#change the x,y coordinates of the trap to easting, northing (required for mcp)
#let's use A1 at Puro because TREEBRIDGE! is the best - easting:398049	northing: 6791091

mcp_trap <- mcp_trap %>%
  #easting (x coordinate) should be [(trap.letter.as.number-1)*10] + A1.easting
  mutate(easting = (((x)-1)*10) + 398049) %>%
  relocate(easting, .after = x) %>%
  #adjust y (northing) - the trap number
  #if remainder of x/2 is 0 (if x is even), multiply y by 2 - if else, multiply by 2 then subtract 1
  mutate(northing = ifelse(
    (x %% 2) == 0, ((y-1)*10) + 6766143, ((y-1)*10) + 6766143)) %>%
  relocate(northing, .after = y) %>%
  dplyr::select(-x, -y)

#split() fulltrap into ... A LIST! by site
mcp_list <- split(mcp_trap, f = mcp_trap$site)

###########################################################################################################



#### looping through all 12 cites to create a separate spatialpolygonsdf for each site ####

library(scales) #used for pretty plots in the loop
library(Hmisc) #has capitalize() function

sitespdf_list <- list()
HRarea_list <- list()

for(i in 1:length(mcp_list)){
  
  GRID <- mcp_list[[i]]
  
  #Create Spatial Points for all capture locations and assign vole IDs to each location
  
  #pull the easting and northing coordinates of all capture locations
  data.xy = GRID[c("easting","northing")]
  #Create object of class Spatial Points for all capture locations
  xysp <- SpatialPoints(data.xy)
  #assign projection for Finland
  proj4string(xysp) <- CRS("+proj=utm +zone=35 +units=m +no_defs +ellps=GRS80")
  
  #Create a Spatial Data Frame from Spatial Points
  spdf <- data.frame(xysp)
  #Creates a data frame of ID
  idsp <- data.frame(GRID$tag)
  #Merges ID and Date into the same spatial data frame   #### ???? how is this working? ####
  merge <- data.frame(idsp)
  #Adds ID and Date data frame with locations data frame   #### ???? how is this working? ####
  coordinates(merge) <- spdf
  #head(merge)

  #create MCPs for our new dataset "merge" by individual animal ID
  cp <- mcp(merge[,1], percent=100) #(95% is the default)
  #write cp to the sitespdf_list
  sitespdf_list[[i]] <- cp
  
  ## to get area of the bounding polygon for each MCP (ie 'HR' size)
  #area expressed in hectares if map projection was UTM
  mcpdata <- as.data.frame(cp)
  #write the ID/HR area df to the HRarea_list
  HRarea_list[[i]] <- mcpdata
  
  # ## Plot the home ranges - just base R with no colors
  # plot(cp)
  # ## ... And the relocations as crosshairs at each capture loc
  # plot(merge, add=TRUE)

  # Plot the home ranges and save to a png file in my working directory
  # library(scales) # Helps make polygons partly transparent using the alpha argument below
  
  png(paste("caparea_", names(mcp_list)[i], "_2021", ".png", sep = ""))
  
  plot(cp, col = cp@data$id, pch = 16, main = paste(capitalize(names(mcp_list)[i]), "Capture Areas of Resident Voles", sep = " "))
  plot(cp, col = alpha(1:10, 0.5), add = TRUE)
  #add crosshairs for each recapture location
  plot(merge, add=TRUE)
  
  dev.off()
  
}

#name the 12 1st order elements as their sites
names(sitespdf_list) <- names(mcp_list)
names(HRarea_list) <- names(mcp_list)





## make HRarea_list into df
HRarea_summary <- do.call(rbind.data.frame, HRarea_list)

#clean up the df
HRarea_summary <- HRarea_summary %>% 
  rownames_to_column("name") %>% #row names are the sites, make that a column
  separate(name, c("site", NA)) %>% #separate the site part from the index and get rid of the index
  mutate(site = as.factor(site)) #make site a factor

#join the grid_trts to HRarea_summary
HRarea_summary <- HRarea_summary %>% 
  left_join(y=grid_trts, by = "site")














#################   NEXT :: Get this into a loop so we can create one for each grid  ################################
############# oooooo and can we see all the grids side by side? I want this shit on my wall #########################
############################################ DONE March 31 2022 #####################################################

# GRID <- mcp_list$radio
# 
# #Create Spatial Points for all relocations and assign IDs to each location
# 
# data.xy = GRID[c("easting","northing")]
# #Creates class Spatial Points for all locations
# xysp <- SpatialPoints(data.xy)
# proj4string(xysp) <- CRS("+proj=utm +zone=35 +units=m +no_defs +ellps=GRS80")
# 
# #Creates a Spatial Data Frame from Spatial Points
# sppt <- data.frame(xysp)
# #Creates a data frame of ID
# idsp <- data.frame(GRID$tag)
# #Merges ID and Date into the same spatial data frame
# merge <- data.frame(idsp)
# #Adds ID and Date data frame with locations data frame
# coordinates(merge) <- sppt
# #head(merge)
# 
# 
# #create MCPs for our new dataset "merge" by individual animal ID
# cp <- mcp(merge[,1], percent=100) #(95% is the default)
# ## to get area of the bounding polygon for each MCP (ie 'HR' size)
#   #area expressed in hectares if map projection was UTM
# mcpdata <- as.data.frame(cp)
# # ## Plot the home ranges
# # plot(cp)
# # ## ... And the relocations
# # plot(merge, add=TRUE)
# 
# 
# # Plot the home ranges
# library(scales) # Helps make polygons partly transparent using the alpha argument below
# plot(cp, col = cp@data$id, pch = 16)
# plot(cp, col = alpha(1:10, 0.5), add = TRUE)
# #add crosshairs for each recapture location
# plot(merge, add=TRUE)

################ all of the above is now in the loop, this can probably be deleted eventually  #########################


#count of how many polygons are in the spatialpolygonsdf
length(cp@polygons)











### code from SamHillman - plot each critter on their own
library(sf)
library(ggplot2)

#rather pretty - plot to see each animal in its own space
st_as_sf(cp) %>% 
  ggplot(., aes(fill = id)) +
  geom_sf(alpha = 0.5) +
  scale_fill_discrete(name = "Animal id") +
  facet_wrap(~id) +
  theme_bw() +
  theme(legend.position="none")





####### % overlapping for all the sites in a loop ##############


pct_overlap_list <- list()


for(i in 1:length(sitespdf_list)){
  
  print(i)

  cp <- sitespdf_list[[i]]
  
  newcp <- st_as_sfc(sitespdf_list[[i]])
  
  l <- lapply( newcp, function(x) {lapply(newcp, function(y) st_intersection(x,y) %>% st_area() ) })
  
  mat <- matrix(unlist(l), ncol = length(newcp), byrow = TRUE)
  diag(mat) <- NA #set the diagonal to NA so I don't get confused later
  colnames(mat) <- cp@data$id
  rownames(mat) <- cp@data$id
  # mat #shows area of overlap in hectares (because UTM)
  
  df <- as.data.frame(mat) #convert matrix to df
  df <- tibble::rownames_to_column(df, "focal") #row names become column
  df <- df %>% pivot_longer(!focal, names_to="neighbor", values_to="area_overlap")
  df <- df %>% filter(area_overlap > 0) #remove 0 and NA values
  # df

  l2 <- lapply( newcp, function(x) {lapply(newcp, function(y) st_intersection( x, y ) %>% st_area() / st_area(x)  ) })
  
  mat2 <- matrix(unlist(l2), ncol = length(newcp), byrow = TRUE)
  diag(mat2) <- NA #set the diagonal to NA so I don't get confused later
  rownames(mat2) <- cp@data$id
  colnames(mat2) <- cp@data$id
  # mat2 #shows percent overlap: how much of rowID overlaps with columnID
  
  df2 <- as.data.frame(mat2) #convert matrix to df
  df2 <- tibble::rownames_to_column(df2, "focal") #row names become column
  df2 <- df2 %>% pivot_longer(!focal, names_to="neighbor", values_to="pct.overlap")
  df2 <- df2 %>% filter(pct.overlap > 0) #remove 0 and NA values
  # df2
  
  df_full <- left_join(df, df2)
  # df_full
  
  pct_overlap_list[[i]] <- df_full
  
}

#name the 12 1st order elements as their sites
names(pct_overlap_list) <- names(mcp_list)


## make pct_overlap_list into freiggein huge df
pct_overlap_summary <- do.call(rbind.data.frame, pct_overlap_list)

#clean up the df
pct_overlap_summary <- pct_overlap_summary %>% 
  rownames_to_column("name") %>% #row names are the sites, make that a column
  separate(name, c("site", NA)) %>% #separate the site part from the index and get rid of the index
  mutate(site = as.factor(site)) #make site a factor

#read in a csv of the grid treatments
grid_trts <- read.csv(here("grid_trts.csv"))
#combine food_trt and helm_trt into single 'trt' column
grid_trts <- grid_trts %>% 
  unite(trt, food_trt, helm_trt, sep = "_", remove = FALSE) #keep the original food_trt and helm_trt columns

#join the grid_trts to net_mets_summary
pct_overlap_summary <- pct_overlap_summary %>% 
  left_join(y=grid_trts, by = "site")

######## NICE - this now has the % overlap and area of overlap for all voles that did overlap in their HR
### HOWEVER - we've now lost any data on any individuals that did not overlap




## pulling from code above where I made cp@data into a big df for all sites
#summarise count of 'resident' voles per site
n.HRs <- HRarea_summary %>% 
  group_by(site) %>% 
  summarise(unique(id)) %>% 
  count()

#summarise number of focal voles from pct_overlap_summary (ie number per site with overlaps)
n.overlaps <- pct_overlap_summary %>% group_by(site) %>% 
  summarise(unique(focal)) %>% 
  count() %>%
  rename(n.overlapping = n)

pct_overlapping_summary <- left_join(n.HRs, n.overlaps)
#replace NA with 0 for n.overlapping at Kuoppa (didn't have a row in joined df)
pct_overlapping_summary <- pct_overlapping_summary %>% replace(is.na(.), 0)
pct_overlapping_summary <- pct_overlapping_summary %>% mutate(pct.overlapping = n.overlapping/n)

#join the grid_trts to net_mets_summary
pct_overlapping_summary <- pct_overlapping_summary %>% 
  left_join(y=grid_trts, by = "site")




### Plot some trends - area of overlap, percent overlap, percent overlapping

HRarea_summary %>% ggplot(aes(x=trt, y=area, fill=trt)) +
  geom_boxplot(show.legend = FALSE) +
  labs(x="treatment", y="HR area (hectares)",
       title="HR area by treatment")
#HR area is pretty similar between treatments

pct_overlap_summary %>% ggplot(aes(x=trt, y=pct.overlap, fill=trt)) +
  geom_boxplot(show.legend = FALSE) +
  labs(x="treatment", y="percent overlap",
       title="percent overlap by treatment")
#the percent overlap (how much any single vole overlaps with any other vole) is also pretty similar

pct_overlapping_summary %>% ggplot(aes(x=trt, y=pct.overlapping, fill=trt)) +
  geom_boxplot(show.legend=FALSE) +
  labs(x="treatment", y="percent overlapping",
       title="percent overlapping by treatment")
#HOWEVER - the percent overlapping (how many of the voles have an overlapping HR) 
    #is different between food-added and not



















############ for just one site at a time -- now done in a loop above #################

## trying to get overlap between polys for a single site
#https://stackoverflow.com/questions/54234895/calculate-area-overlap-for-pairs-of-polygons-in-matrix-format-in-r


# newcp <- st_as_sfc(cp)
# 
# l <- lapply( newcp, function(x) { 
#   lapply(newcp, function(y) st_intersection( x, y ) %>% st_area() ) 
#   })
# 
# mat <- matrix(unlist(l), ncol = length(newcp), byrow = TRUE)
# diag(mat) <- NA #set the diagonal to NA so I don't get confused later
# colnames(mat) <- cp@data$id
# rownames(mat) <- cp@data$id
# mat #shows area of overlap in hectares (because UTM)
# 
# df <- as.data.frame(mat) #convert matrix to df
# df <- tibble::rownames_to_column(df, "focal") #row names become column
# df <- df %>% pivot_longer(!focal, names_to="neighbor", values_to="area_overlap")
# df <- df %>% filter(area_overlap > 0) #remove 0 and NA values
# df
# 
# l2 <- lapply( newcp, function(x) {
#   lapply(newcp, function(y) st_intersection( x, y ) %>% st_area()  / st_area(x)  ) 
#   })
# 
# mat2 <- matrix(unlist(l2), ncol = length(newcp), byrow = TRUE)
# diag(mat2) <- NA #set the diagonal to NA so I don't get confused later
# rownames(mat2) <- cp@data$id
# colnames(mat2) <- cp@data$id
# mat2 #shows percent overlap: how much of rowID overlaps with columnID
# 
# df2 <- as.data.frame(mat2) #convert matrix to df
# df2 <- tibble::rownames_to_column(df2, "focal") #row names become column
# df2 <- df2 %>% pivot_longer(!focal, names_to="neighbor", values_to="pct_overlap")
# df2 <- df2 %>% filter(pct_overlap > 0) #remove 0 and NA values
# df2
# 
# df_full <- left_join(df, df2)
# df_full

















#### this was learning from 3.29.22 - I believe this is no longer needed given the lapply (3.31.22)

################ NEW! in 2022 -- 3.29.22 -- measuring HR overlap ##########
## still in a single site ###

# library(amt) #this is a package John Fieberg wrote, has function hr_overlap() which might be useful?
#https://cran.r-project.org/web/packages/amt/vignettes/p2_hr.html



# st_intersection(newcp) #creates geometry of the shared portion of x and y
# ### but ^^ this kind of a mess because it sees all the overlaps (eg between 3 polygons at once)
#     ## what I really need to do is pull each polygon separately 
#     ## and then test the overlap between each possible pairing (ugh)
# 
# 
# 
# thing1 <- newcp %>% filter(id=="226056")
# thing2 <- newcp %>% filter(id=="226091")
# 
# st_intersection(thing1, thing2) #these two overlap and I get 'POLYGON'
# 
# #this reports the size of that overlap
# st_area(st_intersection(thing1, thing2))
# 
# thing3 <- newcp %>% filter(id=="219868")
# thing4 <- newcp %>% filter(id=="226023")
# 
# st_intersection(thing3, thing4) #these two intersect at a point, it says 'POINT'
# 
# st_area(st_intersection(thing3, thing4)) #these two intersect at a point and I still get 0 for area of overlap - GOOD
# 
# #however, with both of those, I still need to extract the geometry of the overlap to get an area of overlap
#   #the areas reported are the areas of the HRs for those given animals
# 
# st_intersection(thing3, thing1) #these two do not overlap and I get nothing
# 
# #size of overlap
# st_area(st_intersection(thing1, thing4))
# 
# 
# ## percent overlap
# st_area(st_intersection(thing1, thing2))/st_area(thing1)
# st_area(st_intersection(thing1, thing2))/st_area(thing2)
# 
# st_area(st_intersection(thing1, thing4))/st_area(thing1)
# st_area(st_intersection(thing1, thing4))/st_area(thing4)
# # 
# ## 3.29.22 -- Coooooool - so I can find a way to get the percent overlaps, it's just going to be a cucumbersome process
#   ## to do this for each pairing
#   ## probably the easiest would be a loop per site:
#   ## first row with all rows - delete self/self, second with all rows, delete self/self etc.











































############################### at the bottom because it's probably trashcode ###########################




#and to make things simpler, filter (tidyverse for 'subset()') for a single site
janakkala <- fulltrap %>% filter(site == 'janakkala')
freq <- janakkala %>% count(tag)
filter(freq, n>=5) #13 individuals caught >5 times

radio <- fulltrap %>% filter(site == 'radio')
freq <- radio %>% count(tag)
filter(freq, n>=5) #7 individuals caught >5 times

hevonen <- fulltrap %>% filter(site == 'hevonen')
freq <- hevonen %>% count(tag)
filter(freq, n>=5) #16 individuals caught >5 times

kiirastuli <- fulltrap %>% filter(site == 'kiirastuli')
freq <- kiirastuli %>% count(tag)
filter(freq, n>=5) #5 individuals caught >5 times

talo <- fulltrap %>% filter(site == 'talo')
freq <- talo %>% count(tag)
filter(freq, n>=5) #15 individuals caught >5 times

asema <- fulltrap %>% filter(site == 'asema')
freq <- asema %>% count(tag)
filter(freq, n>=5) #9 individuals caught >5 times

mustikka <- fulltrap %>% filter(site == 'mustikka')
freq <- mustikka %>% count(tag)
filter(freq, n>=5) #9 individuals caught >5 times

kuoppa <- fulltrap %>% filter(site == 'kuoppa')
freq <- kuoppa %>% count(tag)
filter(freq, n>=5) #2 individuals caught >5 times

ketunpesa <- fulltrap %>% filter(site == 'ketunpesa')
freq <- ketunpesa %>% count(tag)
filter(freq, n>=5) #9 individuals caught >5 times

helmipollo <- fulltrap %>% filter(site == 'helmipollo')
freq <- helmipollo %>% count(tag)
filter(freq, n>=5) #14 individuals caught >5 times

vaarinkorpi <- fulltrap %>% filter(site == 'vaarinkorpi')
freq <- vaarinkorpi %>% count(tag)
filter(freq, n>=5) #14 individuals caught >5 times

puro <- fulltrap %>% filter(site == 'puro')
freq <- puro %>% count(tag)
filter(freq, n>=5) #6 individuals caught >5 times



###recode this bit to see different sites with less typing!
GRID <- vaarinkorpi

##################################################################################
########## stealing code from 'gpslocs.R' version 9.30.21  #######################
##########  goal: convert trap number to a UTM coordinate  #######################
##################################################################################

################### uh wait, why would I do that when I already did that? #######################
####### later Janine, please check what earlier Janine did because current Janine thinks she made more work ######

#get rid of the 'x' and 'y' columns since we'll make our own easting/northing coordinates
fulltrap <- fulltrap %>%
  dplyr::select(-c(x, y))
#select() is also in the MASS package and will default to that... 
#so tell R you want the dplyr version to avoid [ERROR: unused arguments]


#separate the letter and number parts of the trapID
GRID <- 
  GRID %>%
  separate(trap, into = c("x", "y"), sep = "(?<=[A-Z])(?=[0-9])", remove=FALSE)

#recode each letter as a number
GRID <- 
  GRID %>%
  #this makes a new column with the letter turned into its corresponding number
  mutate(new_x = case_when(
    x == "A" ~ 1,
    x == "B" ~ 2, 
    x == "C" ~ 3, 
    x == "D" ~ 4, 
    x == "E" ~ 5, 
    x == "F" ~ 6, 
    x == "G" ~ 7, 
    x == "H" ~ 8, 
    x == "I" ~ 9, 
    x == "J" ~ 10, 
    x == "K" ~ 11)) %>%
  #move that new column after the original letter column
  relocate(new_x, .after = x) %>%
  #remove that original letter column
  dplyr::select(-x) %>%
  #rename the new number x column
  rename(x = new_x)

#make sure y and x are both numeric, not characters
GRID <- GRID %>%
  mutate(x = as.numeric(x), y = as.numeric(y))
#str(GRID)

##this is using the coordinates of A1 at Janakkala (east,north) (383900,6766143)

#adjust trap letter (now number) to easting
GRID <-
  GRID %>%
  #easting should be trap [(letternumber-1)*10] + A1easting
  mutate(easting = (((x)-1)*10) + 383900) %>%
  relocate(easting, .after = x) 

#adjust the trap number so it corresponds to a grid number
GRID <-
  GRID %>%
  #if remainder of x/2 is 0 (if x is even), multiply y by 2 - if else multiply by 2 then subtract 1
  mutate(northing = ifelse(
    (x %% 2) == 0, 
    (((y*2)-1)*10) + 6766143, 
    ((y-1)*20) + 6766143   
  )) %>%
  relocate(northing, .after = y) %>%
  dplyr::select(-x, -y)


######################################################################


#need 5 relocs to make a polygon HR

#count of how many times each individual is recapped
freq <- GRID %>% count(tag)
#make it a df with tag and number of captures
hrvoles <- freq %>% filter(n >= 5)

target <- hrvoles$tag
GRID.mcp <- GRID %>% 
  filter(tag %in% target)

