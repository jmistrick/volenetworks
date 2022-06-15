library(tidyverse)
library(here)
library(lubridate)

#clear environment
rm(list = ls())

#load the fulltrap dataset (make sure it's the most recent version)
fulltrap <- readRDS(file = "fulltrap_06.03.22.rds")



########### PREP CODE #############

#add UTM coordinates to fulltrap for distance calculations
fulltrap_move <- fulltrap %>%
  #easting (x coordinate) should be A1.easting - [(trap.letter.as.number-1)*10] 
  mutate(easting = ( 398049 - ((x)-1)*10) ) %>% #minus because A1 is on the E side of the grid, so coordinates go W
  relocate(easting, .after = x) %>%
  #northing (y coordinate) should be [(trap# - 1)*10] + A1.northing
  mutate(northing = ((y-1)*10) + 6766143 ) %>%
  relocate(northing, .after = y) %>%
  dplyr::select(-x, -y)

######### END PREP CODE ############

###############################################################################################
############ TOTAL DISTANCE MOVED / AVG DISTANCE PER DAY within trapping occasion #############
####################### code based on work by Lisa Meyer DVM student ##########################
###############################################################################################

#subset fulltrap_dist to just the animals with recaptures in an occasion
onlyrecapped_wiocc <- fulltrap_move %>%
  group_by(occasion, tag) %>%
  arrange(session, .by_group = TRUE) %>%
  filter(caps_per_occ > 1) %>% #keep only animals captured at least 2x in that occasion
  ungroup()

#create vector of months
month.vector <- c("may", "june","july", "aug", "sept", "oct")
#create tibble to hold final results
distances.df.final <- tibble()

for(i in month.vector){ #for each month (vector, May-Oct)
  
  print(i)
  
  occ_recaps_ids <- onlyrecapped_wiocc %>% 
    filter(month==i) %>% #filter for the month we want
    dplyr::select(tag) %>% unique() #pull the tags and keep only one entry per unique tagid
  #this makes a df with two columns "occasion" and "tag" - one row per animal captured
  
  distances.df.occ <- tibble() #empty df to store the results per occasion
  d=0 #set initial distance to 0
  
  for(j in occ_recaps_ids$tag){ #for each individual tag within a month
    
    # print(j)
    
    voledat <- onlyrecapped_wiocc %>% filter(month==i) %>% filter(tag==j) #just one tag in one month
    
    for(k in 1:((nrow(voledat))-1) ){ #for (n observations that occ) - 1, the number of segments
      
      x.prev <- voledat$easting[k]
      y.prev <- voledat$northing[k]
      x.new <- voledat$easting[k+1]
      y.new <- voledat$northing[k+1]
      d = d + sqrt( (x.new-x.prev)^2 + (y.new-y.prev)^2 ) 
      #each time through the loop (additional trap-trap segment) will add to previous d value for that animal
    }
    
    #calculate min and max date of capture in that occasion for animal
    min.date <- min(voledat$date_time)
    max.date <- max(voledat$date_time)
    
    dtime = d / as.numeric(max.date-min.date) #avg movement (m) per 24 hours
    #overwrite after each tagID to hold all the entries for a single vole, single occasion
    distances.df.occ <- rbind(distances.df.occ, data_frame(month=i, tag=j, total.dist.occ=d, avg.dist.occ=dtime))
    #total.dist.occ is total distance (in m) moved that occasion across all captures
    #avg.dist.occ is avg distance moved PER DAY (24hrs) ##### YOU MIGHT WANT TO CHANGE THIS TO PER CAPTURE EVENT? #######
    d=0 #reset d value back to 0 before moving on to the next animal
    
  }
  
  #overwrite at the end of each occasion to add all the occasions together
  distances.df.final <- rbind(distances.df.final, distances.df.occ)
  
}

#add distances.df.final back to fulltrap_dist
fulltrap_move <- left_join(fulltrap_move, distances.df.final, by=c("month"="month", "tag"="tag")) %>%
  mutate(across(c(total.dist.occ, avg.dist.occ), ~round(., 2))) #round values

####################### END total/avg dist per occ ####################################  


##################################################################
################## calculate trap:cap ratio ######################
################# "willingness to move" WTM  #####################
##################################################################

#calculated as # traps/ # captures --> both wi occ and lifetime
#value ranges from 0 to 1 -- 1 means you moved traps every time
#think of it like a 'willingness to move' - smaller value = less willingness to move
 
################ THINK ON THIS - WTM_OCC ONLY IF RECAPPED IN THAT OCC ?? ################
#WTM requires an animal to be recapped at least once (?? but not necessarily in that occasion)
  # ?? so you can have score of 1 in an occasion because you were only caught once ??
  # ... but a score of less than one per lifetime ?? IS THAT FAIR ??

fulltrap_move <- fulltrap_move %>%
  mutate(wtm_occ = traps_per_occ/caps_per_occ) %>%
  mutate(wtm_occ = ifelse(n_cap==1, NA, wtm_occ)) %>% #no WTM if only trapped 1x
  mutate(wtm_life = traps_per_life/n_cap) %>%
  mutate(wtm_life = ifelse(n_cap==1, NA, wtm_life)) %>% #no WTM if only trapped 1x
  mutate(across(c(wtm_occ, wtm_life), ~round(., 2))) #round values 


########################## END WTM ##################################### 


#################################################################
################### MAX DIAMETER wi occ  ########################
#################################################################

## uhh I did between occasions too at one point, not sure if I still care


#create tibble to hold final results
diameter.df.occ.final <- tibble()

for(i in month.vector){ #for each month (vector, May-Oct)
  
  print(i)
  
  occ_recaps_ids <- onlyrecapped_wiocc %>% 
    filter(month==i) %>% #filter for the month we want
    dplyr::select(tag) %>% unique() #pull the tags and keep only one entry per unique tagid
  #this makes a df with two columns "occasion" and "tag" - one row per animal captured
  
  diameter.df.occ <- tibble() #empty df to store the results per occasion

  for(j in occ_recaps_ids$tag){ #for each individual tag within a month
    
    # print(j)
    
    locs <- onlyrecapped_wiocc %>% 
      filter(month==i) %>% filter(tag==j) %>% #subset for just one tag in one month
      dplyr::select(easting, northing)
    
    #for each individual in that site/occasion, calculate their max dist moved
    max <- max(dist(locs, method="euclidean"))
    
    #overwrite after each tagID to hold all the entries for a single vole, single occasion
    diameter.df.occ <- rbind(diameter.df.occ, data_frame(month=i, tag=j, maxdiam_occ=max))
    #max diam is the max distance between any two capture locations that month
    
  }
  
  #overwrite at the end of each occasion to add all the occasions together
  diameter.df.occ.final <- rbind(diameter.df.occ.final, diameter.df.occ)
  
}

#add diameter.df.final back to fulltrap_dist
fulltrap_move <- left_join(fulltrap_move, diameter.df.occ.final, by=c("month"="month", "tag"="tag")) %>%
  mutate(across(c(maxdiam_occ), ~round(., 2))) #round values


############################# END max diam wi occ ###########################


#################################################################
##################  MAX DIAMETER lifetime  ######################
#################################################################

#create tibble to hold final results
diameter.df.life.final <- tibble()

#need a new df to work from, this one is animals with recaps ever (ignoring occasion)
onlyrecapped_lifetime <- fulltrap_move %>%
  filter(n_cap > 1) %>%
  ungroup()

#vector of all tags of animals with multiple captures across lifetime
life_recaps_ids <- onlyrecapped_lifetime %>%
  dplyr::select(tag) %>%
  unique()


for(i in life_recaps_ids$tag){ #for each animal with lifetime recaps
  
  diameter.df.tag <- tibble() #empty df to store the results per tag
  
  locs <- onlyrecapped_lifetime %>% 
    filter(tag==i) %>% #subset for just one tag
    dplyr::select(easting, northing)
    
  #for each individual, calculate their max diam across lifetime
  max <- max(dist(locs, method="euclidean"))
  
  diameter.df.tag <- data_frame(tag=i, maxdiam_life=max)
  #max diam is the max distance between any two capture locations in lifetime (2021 season)
    
  #overwrite to add all the individuals together
  diameter.df.life.final <- rbind(diameter.df.life.final, diameter.df.tag)
  
}

#add diameter.df.final back to fulltrap_dist
fulltrap_move <- left_join(fulltrap_move, diameter.df.life.final, by="tag") %>%
  mutate(across(c(maxdiam_life), ~round(., 2))) #round values

############################# END max diam lifetime ###########################

