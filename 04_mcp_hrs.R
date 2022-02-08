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
fulltrap <- readRDS(file = "fulltrap_11.18.21.rds")


################################ new things 12.1.21  ####################################

#clean fulltrap for use in mcp analysis
mcp_trap <- fulltrap %>%
  group_by(site, tag) %>%
  mutate(cap.freq = n()) %>% #create column for capture frequency (# times seen)
  filter(cap.freq >= 5) #filter for at least 5 relocs


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




GRID <- mcp_list$helmipollo


#################   NEXT :: Get this into a loop so we can create one for each grid  ################################
############# oooooo and can we see all the grids side by side? I want this shit on my wall #########################

#Create Spatial Points for all relocations and assign IDs to each location

data.xy = GRID[c("easting","northing")]
#Creates class Spatial Points for all locations
xysp <- SpatialPoints(data.xy)
proj4string(xysp) <- CRS("+proj=utm +zone=35 +units=m +no_defs +ellps=GRS80")

#Creates a Spatial Data Frame from Spatial Points
sppt <- data.frame(xysp)
#Creates a data frame of ID
idsp <- data.frame(GRID$tag)
#Merges ID and Date into the same spatial data frame
merge <- data.frame(idsp)
#Adds ID and Date data frame with locations data frame
coordinates(merge) <- sppt
#head(merge)


#create MCPs for our new dataset "merge" by individual animal ID
cp <- mcp(merge[,1], percent=100) #(95% is the default)
## The size of the bounding polygon
as.data.frame(cp)
## Plot the home ranges
plot(cp)
## ... And the relocations
plot(merge, add=TRUE)


# Plot
library(scales) # Helps make polygons partly transparent using the alpha argument below
plot(cp, col = cp@data$id, pch = 16)
plot(cp, col = alpha(1:10, 0.5), add = TRUE)

plot(merge, add=TRUE)






























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

