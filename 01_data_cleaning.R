# code run using R version 4.0.2 (2020-06-22) -- "Taking Off Again"

#load libraries
library(here)
library(tidyverse)
# library(mosaic)
library(janitor)
library(lubridate)

#clear environment
rm(list = ls())

###############################   ENTRY & CLEANING VOLE CAPTURE DATA   ##################################

#load data
voledata <- read.csv(here("vole_capture_data_12.30.21_SEXCORRECTED.csv"))

#clean names
voledata <- voledata %>%
  clean_names %>%
  #rename column
  rename(session = sess, samp_id = id) %>%
  #mutate columns to their proper formats
  mutate(year = as.numeric(year),
         occasion = as.factor(occasion),
         site = as.character(site),
         food_trt = as.factor(food_trt),
         helm_trt = as.factor(helm_trt),
         date = as_date(date, format= "%m/%d/%Y"),
         session = as.numeric(session),
         trap = as.character(trap),
         tag = as.character(tag),
         samp_id = as.factor(samp_id),
         sex = as.factor(sex),
         ow = as.factor(ow),
         per = as.factor(per), 
         nip = as.factor(nip), 
         preg = as.factor(preg),
         test = as.factor(test),
         head = as.numeric(head), 
         mass = as.numeric(mass),
         ticks = as.numeric(ticks),
         ear_ed = as.factor(ear_ed),
         saliva_sr = as.factor(saliva_sr),
         smear_bs = as.factor(smear_bs),
         bloodspin_bc = as.factor(bloodspin_bc),
         bloodrna_br = as.factor(bloodrna_br),
         fecalegg_fe = as.factor(fecalegg_fe),
         fecalrna_fr = as.factor(fecalrna_fr),
         deworm = as.factor(deworm),
         fate = as.factor(fate),
         handler = as.factor(handler),
         notes = as.character(notes)) %>%
  #remove any animals without a trap location
  filter(!is.na(trap)) %>%
  #remove any animals without a tag id
  filter(!is.na(tag)) %>%
  #remove animals found dead when setting/supplementing
  filter(!session == "0") %>%
  filter(!is.na(session))

####################################### how to incorporate animals found dead NOT during an occasion? ######################################
# voledata %>% filter(is.na(trap))
# #219825 - euthanized 9.1.21 during field course (caught off the grid)
# #219917 - euthanized 9.1.21 during field course (caught off the grid)
# voledata %>% filter(is.na(tag)) #these are fine to remove, nothing important here
# voledata %>% filter(session=="0") 
# #these are animals found dead when setting - but I don't know when they went into the trap (assumed right after we left)
# #they aren't technically session 4 captures, though so I don't know what I'd do with them if I left them in
# voledata %>% filter(is.na(session)) #these animals were found DT when supplementing

########## RIGHT NOW, all of the DP, DT, and S animals (except those that were part of ^ above) are still in the dataset ###################
# #remove animals euthanized for terminal sampling
# filter(!fate == "S")
# #remove animals DP or DT
# filter(!fate == "DP") %>%
# filter(!fate == "DT")

#check for spelling errors, extra groups, weird data
# unique(voledata$site)
# unique(voledata$year)
# unique(voledata$occasion)
# unique(voledata$session)
# unique(voledata$food_trt)
# unique(voledata$helm_trt)
# unique(voledata$sex)
# unique(voledata$fate)
# unique(voledata$ow)
# unique(voledata$per)
# unique(voledata$nip)
# unique(voledata$preg)
# unique(voledata$test)
# range(voledata$head, na.rm = TRUE)
# range(voledata$mass, na.rm = TRUE)
# range(voledata$ticks, na.rm = TRUE)


# #basically a find and replace command
# voledata$column[voledata$column == "findthis"] <- "replacewiththis"


################################ create a time column to replace session (for CMRnet) ##################################
voledata <-
  voledata %>%
  mutate(time = case_when(
    session == "1" ~ "06:00:00",
    session == "2" ~ "16:00:00",
    session == "3" ~ "06:00:00",
    session == "4" ~ "16:00:00",)) %>%
  relocate(time, .after = session)

#turn the time into a lubridate time
voledata$time <- hms(voledata$time)
#combine date and time columns into a lubridate time
voledata$date_time <- ymd_hms(paste(voledata$date, voledata$time))
#move date and time around
voledata <-
  voledata %>%
  relocate(date, .after = year) %>%
  relocate(session, .after = occasion) %>%
  relocate(date_time, .after = date) %>%
  dplyr::select(-time)

# str(voledata)
######################################################### END ##################################################################



# find out if any of the DP or DT or S animals were ones we'd caught before
# dead <- voledata %>%
#   filter(!fate == "R") %>%
#   summarise(tag)
# 
# dead.v <- as.vector(dead$tag)
# length(dead.v) #22 individuals are DT or DP
# 
# dead.df <- voledata %>% filter(tag %in% dead.v)
# nrow(dead.df) #76 entries for those animals


#check the number of animals at each grid in may (lowest month)
# voledata %>%
#   filter(occasion == "1")
#igraph gets annoyed and won't convert nets to igraph objects if there is only 1 animal in the grid... 
    # and therefore no adj matrix
#get rid of the one critter at janakkala and kuoppa in May because they make trouble in the networks
voledata <- voledata %>%
  filter(!(site == "janakkala" & date <= "2021-06-01")) %>%
  filter(!(site == "kuoppa" & date <= "2021-06-01"))




################################ convert trap number to a grid coordinate ####################################
voledata<- 
  voledata %>%
  separate(trap, into = c("x", "y"), sep = "(?<=[A-Z])(?=[0-9])", remove=FALSE)

#recode each letter as a number
voledata <- 
  voledata %>%
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
  #move that new column right after the original letter column
  relocate(new_x, .after = x) %>%
  #remove that original letter column
  dplyr::select(-x) %>%
  #rename the new number x column
  rename(x = new_x)

#make sure y and x are both numeric, not characters
voledata <- voledata %>%
  mutate(x = as.numeric(x), y = as.numeric(y))

# str(voledata)

#adjust the trap number so it corresponds to a grid number
voledata <-
  voledata %>%
  #if remainder of x/2 is 0 (if x is even), multiply y by 2 - if else multiply by 2 then subtract 1
  mutate(y_new = ifelse((x %% 2) == 0, ((voledata$y)*2), (((voledata$y)*2)-1))) %>%
  relocate(y_new, .after = y) %>%
  #remove the old y column and rename the new one
  dplyr::select(-y) %>%
  rename(y = y_new)
######################################################### END ##################################################################


#remove un-needed columns from datatable - make a smaller version for easier networks
trap <- voledata %>%
  dplyr::select(-id_as_number, -year, -date, -ow, -ticks, -ear_ed, -saliva_sr, -smear_bs, 
         -bloodspin_bc, -bloodrna_br, -fecalegg_fe, -fecalrna_fr, -deworm,
         -samp_id, -handler, -notes)




#############################  LOAD AND CLEAN WEEK RECAP DATA   ###############################################

#load the data
wr_data <- read.csv(here("week_recap_data_12.30.21_SEXCORRECTED.csv"))

#several columns (sex, per, nip, preg, test, fate, handler) have "" or "not noted" --> change these to NA
wr_data$sex[wr_data$sex == "not noted"] <- NA
wr_data$sex[wr_data$sex == ""] <- NA
wr_data$per[wr_data$per == "not noted"] <- NA
wr_data$per[wr_data$per == ""] <- NA
wr_data$nip[wr_data$nip == "not noted"] <- NA
wr_data$nip[wr_data$nip == ""] <- NA
wr_data$preg[wr_data$preg == "not noted"] <- NA
wr_data$preg[wr_data$preg == ""] <- NA
wr_data$test[wr_data$test == "not noted"] <- NA
wr_data$test[wr_data$test == ""] <- NA
wr_data$fate[wr_data$fate == ""] <- NA
wr_data$handler[wr_data$handler == ""] <- NA

#clean names
wr_data <- wr_data %>%
  remove_empty %>%
  clean_names %>%
  #rename column
  rename(session = sess) %>%
  #mutate columns to their proper formats
  mutate(year = as.numeric(year),
         occasion = as.factor(occasion),
         site = as.character(site),
         food_trt = as.factor(food_trt),
         helm_trt = as.factor(helm_trt),
         date = as_date(date, format= "%m/%d/%Y"),
         session = as.numeric(session),
         trap = as.character(trap),
         tag = as.character(tag),
         sex = as.factor(sex),
         per = as.factor(per), 
         nip = as.factor(nip), 
         preg = as.factor(preg),
         test = as.factor(test),
         fate = as.factor(fate),
         handler = as.factor(handler),
         notes = as.character(notes))

# #check for missing data
# wr_data %>% filter(is.na(trap)) #no missing traps
# wr_data %>% filter(is.na(tag)) #no missing tags
# wr_data %>% filter(is.na(session)) #no missing sessions
# wr_data %>% filter(is.na(date)) #no missing dates


############ DEAD ANIMALS ARE STILL IN THE DATASET ###################
  # #remove animals euthanized for terminal sampling
  # filter(!fate == "S") %>%
  # #remove animals DP or DT
  # filter(!fate == "DP") %>%
  # filter(!fate == "DT")


#check for spelling errors, extra groups, weird data
# unique(wr_data$year)
# unique(wr_data$occasion)
# unique(wr_data$site)
# unique(wr_data$food_trt)
# unique(wr_data$helm_trt)
# unique(wr_data$session)
# unique(wr_data$sex)
# unique(wr_data$per)
# unique(wr_data$nip)
# unique(wr_data$preg)
# unique(wr_data$test)
# unique(wr_data$fate)



#########################################################################################################

#create a time column to replace session (for CMRnet)
wr_data <-
  wr_data %>%
  mutate(time = case_when(
    session == "1" ~ "06:00:00",
    session == "2" ~ "16:00:00",
    session == "3" ~ "06:00:00",
    session == "4" ~ "16:00:00",)) %>%
  relocate(time, .after = session)

#turn the time into a lubridate time
wr_data$time <- hms(wr_data$time)
#combine date and time columns into a lubridate time
wr_data$date_time <- ymd_hms(paste(wr_data$date, wr_data$time))
#move date and time around
wr_data <-
  wr_data %>%
  relocate(date, .after = year) %>%
  relocate(session, .after = occasion) %>%
  relocate(date_time, .after = date) %>%
  dplyr::select(-time)

# str(wr_data)

#convert trap number to a grid coordinate
wr_data<- 
  wr_data %>%
  separate(trap, into = c("x", "y"), sep = "(?<=[A-Z])(?=[0-9])", remove=FALSE)

#recode each letter as a number
wr_data <- 
  wr_data %>%
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
  #move that new column right after the original letter column
  relocate(new_x, .after = x) %>%
  #remove that original letter column
  dplyr::select(-x) %>%
  #rename the new number x column
  rename(x = new_x)

#make sure y and x are both numeric, not characters
wr_data <- wr_data %>%
  mutate(x = as.numeric(x), y = as.numeric(y))
# str(wr_data)

#adjust the trap number so it corresponds to a grid number
wr_data <-
  wr_data %>%
  #if remainder of x/2 is 0 (if x is even), multiply y by 2 - if else multiply by 2 then subtract 1
  mutate(y_new = ifelse((x %% 2) == 0, ((wr_data$y)*2), (((wr_data$y)*2)-1))) %>%
  relocate(y_new, .after = y) %>%
  #remove the old y column and rename the new one
  dplyr::select(-y) %>%
  rename(y = y_new)

#remove un-needed columns from datatable - make a smaller version for easier networks
recap <- wr_data %>%
  dplyr::select(-year, -date, -handler, -notes)



#################################################################################

#combining trap and recap dataframes

#compare columns and their classes
# compare_df_cols(trap, recap)

fulltrap <- bind_rows(trap, recap)


# str(fulltrap)




##################################################################################################################################################################
##############################   WORK ZONE   WORK ZONE   WORK ZONE  WORK ZONE WORK ZONE ##########################################
##################################################################################################################################################################


#####there are also a number of animals that were on the vole processing sheet twice because they were 
## captured, weighed, and released due to time issues (mostly occasion 6)
### I need to find these animals and make sure they only have 0 or 1 for the first occasion they were seen, 
## not both if they have two processing entries


# ## working prelim version (no longer needed)
# #find the first occurrence of each tag ID
# first <- fulltrap %>%
#   unite(occ.sess, occasion, session, sep = ".", remove = FALSE) %>%
#   group_by(tag) %>%
#   filter(occ.sess == min(occ.sess)) %>%
#   slice(1) %>% #takes the first occurrence if there is a tie
#   ungroup()
# 
# #making sure it's working
# notfirst <- fulltrap %>%
#   unite(occ.sess, occasion, session, sep = ".", remove = FALSE) %>%
#   group_by(tag) %>%
#   filter(occ.sess != min(occ.sess)) %>%
#   ungroup()
# #12.2.21 - something is still weird because 1 animal is missing between first and notfirst but all have occ and sess

#RECAP column: give a 1 if the capture is the first occurrence of that tag, else 0
fulltrap <- fulltrap %>%
  unite(occ.sess, occasion, session, sep = ".", remove = FALSE) %>% #make a new occ.sess column so I can do things in order
  group_by(tag) %>%
  mutate(firstcap = ifelse(occ.sess == min(occ.sess), 1, 0)) %>%
  relocate(firstcap, .after=tag)
### this will record all the WRs as 0 as well

## BUT THINGS ARE STILL WRONG FOR ANY ANIMAL CAPTURED ON 2 GRIDS



################# do a check to see how many unique grids animals are on #####################
fulltrap %>%
  group_by(tag) %>%
  summarise(site_unique = n_distinct(site)) %>%
  filter(site_unique > 1) %>%
  ungroup()

## the two animals here are 219742 and 219958

############ WORKING as of 12.30.21 there are still 2 animals with records at two sites
###############################################################################################


## check to make sure animals don't have more entries than unique occ.sess combos
check <- fulltrap %>% group_by(tag) %>% summarise(count = length(occ.sess), unique = n_distinct(occ.sess))

check <- check %>% mutate(diff=unique-count) %>% filter(diff != 0)
#### STILL NEED TO DEAL WITH THIS ##### 219765 is on this list - something is still wrong there (12.30.21)


#############################################################################################

######  STILL NEED TO DO THIS  ############animals that are questinonable enough to remove

#capture of 219941 at janakkala occ3sess3 - can't confirm PIT tag #
#animals 219875 capped once at both mustikka and kiirastuli - don't know who they actually are

###############################################################################################





##################################################################################################################################################################
################################################## end WORK ZONE end WORK ZONE  end WORK ZONE end WORK ZONE ######################################################
##################################################################################################################################################################


# #remove un-needed columns from datatable - make a smaller version for easier networks
# trap2 <- voledata %>%
#   dplyr::select(-id_as_number, -year, -date, -ow, -ticks, -ear_ed, -saliva_sr, -smear_bs, 
#                 -bloodspin_bc, -bloodrna_br, -fecalegg_fe, -fecalrna_fr, -deworm,
#                 -samp_id, -notes)
# 
# #remove un-needed columns from datatable - make a smaller version for easier networks
# recap2 <- wr_data %>%
#   dplyr::select(-year, -date, -notes)
# 
# 
# #combining trap and recap dataframes
# fulltrap2 <- bind_rows(trap2, recap2)
# 
# #RECAP column: give a 1 if the capture is the first occurrence of that tag, else 0
# fulltrap2 <- fulltrap2 %>%
#   unite(occ.sess, occasion, session, sep = ".", remove = FALSE) %>% #make a new occ.sess column so I can do things in order
#   group_by(tag) %>%
#   mutate(firstcap = ifelse(occ.sess == min(occ.sess), 1, 0)) %>%
#   relocate(firstcap, .after=tag)



############################## THESE NEED TO BE RESOLVED - PROBABLY IN THE RAW DATA ##############################

## NEW! 4.12.22

#check to make sure everyone only has one sex
fulltrap$sex <- as.character(fulltrap$sex)
check <- fulltrap %>% group_by(tag) %>%
  summarise(n = unique(sex)) %>%
  filter(!is.na(n))
#save the tag IDs of animals with multiple sexes
duplist <- as.vector(check$tag[duplicated(check$tag)])
#how many?
length(duplist) #23 voles
sexswaps <- fulltrap %>% 
  filter(tag %in% duplist) %>% 
  arrange(tag, occ.sess)

#write as csv to share
# write.csv(sexswaps, here("confirm_sex.csv"), row.names=FALSE)

######################################################













###############################################################################################

#save fulltrap so I can pull it for other scripts

# Save fulltrap to a rdata file
saveRDS(fulltrap, file = here("fulltrap_12.30.21.rds"))

# Restore fulltrap from the rdata file
# fulltrap <- readRDS(file = "fulltrap_12.30.21.rds")



##I can also write this to a csv for posterity
write.csv(fulltrap, here("fulltrap_12.30.21.csv"), row.names=FALSE)
# fulltrap <- read.csv(here("fulltrap_12.30.21.csv"))

#HOWEVER, R to csv will change POSIX dates back to characters - this code would avoid that
# (haven't bothered with this, but it's here in case I want it)
# #save it
# dat <- data.frame(time=as.POSIXlt("2013-04-25 09:00 BST"), quantity=1) # example data
# write.csv(mutate(dat, time=format(time, "%FT%H:%M:%S%z")), file="test.csv", row.names=FALSE)
# #read it back in
# df2 <- read.csv("test.csv") %>% mutate(time=ymd_hms(time))






