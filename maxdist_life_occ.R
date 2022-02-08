library(tidyverse)
library(here)

fulltrap <-  read.csv(here("fulltrap_9.30.21.csv"))

### I think my life would be much easier here if the occasions where a factor (and words) as opposed to numbers
fulltrap$occasion <- as.factor(fulltrap$occasion)
#tidyverse to replace factor levels with words
fulltrap$occasion <- fulltrap$occasion %>%
  recode_factor("1" = "may", "2" = "june", "3" = "july", "4" = "aug", "5" = "sept")
  





############## step (1) working to calculate max distance moved ##########################

## tibble with count of number of unique traps per animal per occasion
unique_traps_occasion <- fulltrap %>%
  group_by(tag, occasion) %>%
  summarise(trap_unique = length(unique(trap))) ##n_distinct() is a dplyr wrapper for length(unique())

unique_traps_occasion %>% filter(tag == 226001) ## shows number of unique traps in an occasion


## tibble with count of number of unique traps per animal in lifetime
unique_traps_lifetime <- fulltrap %>%
  group_by(tag) %>%
  summarise(trap_unique = length(unique(trap)))

unique_traps_lifetime %>% filter(tag == 226001) #shows lifetime unique traps

fulltrap %>% filter(tag == 226001) #see the lifetime traps individual was caught in


## tibble with a column of all the traps in individual was found in (lifetime)
lifetime_traps <- fulltrap %>%
  group_by(tag) %>%
  summarise(locs = paste(unique(trap), collapse='-'))


#############################################

##### step (2) a different approach ######

#subset fulltrap down to something manageable until I know what I'm doing 
test.grid <- fulltrap %>% 
  filter(site == "kuoppa")

#going to start with lifetime distances moved
#these are the animals that have been seen in more than 1 trap
test.grid %>%
  group_by(tag) %>%
  summarise(trap_unique = n_distinct(trap)) %>%
  filter(trap_unique > 1)

#add a column of trap_unique to test.grid
#remove anyone seen only once
test.grid <- test.grid %>%
  group_by(tag) %>%
  mutate(trap_unique = n_distinct(trap)) %>%
  filter(trap_unique > 1) %>%
  ungroup()



# #### calculate max distance between traps for one individual
# test <- test.grid %>%
#   filter(tag == 226099)
# 
# locs <- test %>% 
#   dplyr::select(x,y)
# 
# max(dist(locs, method="euclidean"))
# #max distance for 226094 is 9.48
# #max distance for 226053 is 6
# #max distance for 226099 is 5.09



#pseudocodin
#for each unique tag, create a distance matrix of all the trap locations
#find the maximum distance
#report that in a new column - maxdist
#repeat for every individual

#make test.grid df into a list of dfs - one for each unique tag
test.grid_list <- split(test.grid, f = test.grid$tag)

#make a df to hold the results
maxdist <- data.frame("tag" = character(0), "maxdist" = integer(0))

for(i in 1:length(helmi_list)){
  
  locs <- test.grid_list[[i]] %>% dplyr::select(x,y)
  max <- max(dist(locs, method="euclidean"))
  maxdist[i,1] <- names(helmi_list[i])
  maxdist[i,2] <- max
    
}

maxdist #this prints a df with tag and maxdist as columns (this is for one site)


##################################################### step (3) ###############################################################
##################calculate max dist moved over lifetime per vole##############################################
### to scale this up to multiple sites: 
  # need to have a nested list of 12 elements (sites) and under each of the those are lists of the data for each individual

## still focused on lifetime distance moved (not within an occasion)

#take fulltrap and remove any individuals only captured once
fulltrap <- fulltrap %>%
  group_by(tag) %>%
  mutate(trap_unique = n_distinct(trap)) %>%
  filter(trap_unique > 1) %>%
  dplyr::select(-trap_unique) %>%
  ungroup()

#turn fulltrap into a list of elements by site
fulltrap_list <- split(fulltrap, f = fulltrap$site)
#and then split each site into a nested list by tag
fulltrap_list <- lapply(fulltrap_list, function(x) split(x, x$tag))




### calculate lifetime distance moved for every individual at all 12 grids
## this code works 
dist_list_life <- list()


for(i in 1:length(fulltrap_list)){
  
  print(i)
  
  tag <- names(fulltrap_list[[i]])
  site <- data.frame(tag)
  
  for(j in 1:length(fulltrap_list[[i]])){
    
    locs <- fulltrap_list[[i]][[j]] %>% dplyr::select(x,y)
    max <- max(dist(locs, method="euclidean"))
    
    site$maxdist[j] <- max
    
  }
  
  dist_list_life[[i]] <- site
  
}
  
#name the 12 1st order elements of nets_list as their sites
names(dist_list_life) <- names(fulltrap_list)

#############################  END  ##################################################



######################### step (4) A BABY STEP I USED TO GET TO THE FINAL ##################

###### neeeeeeeeat -- so this will do it for one site/occasion combo 

#subset down to just one site/occasion [but all the animals their locs] for simplicity until I know wtf I'm doing
test <- fulltrap_occ_list$asema[[4]] %>% 
  group_by(tag) %>%
  dplyr::select(x,y) %>%
  ungroup()

test1 <- split(test, f=test$tag)  

fulltrap_occ_list[[1]][[4]] <- test1

#then loop through to do the max distances for each animal in that occasion
tag <- names(test1)
occ <- data.frame("tag" = tag, "maxdist" = integer(length(names(test1))))

for(i in 1:length(test1)){
  
  for(j in 1:length(test1[[i]])){
    
    locs <- test1[[i]] %>% dplyr::select(x,y)
    max <- max(dist(locs, method="euclidean"))
    
    occ[i,2] <- max
    
  }
  
}

#################################### END ####################################





##########################################################here is the good stuff######################################################


################# step (5)  calculate max dist moved per occasion for each vole (rep. at all 12 sites)  #############
## within occasion distance moved

# #view tibble of ndistinct traps per occasion
# fulltrap %>%
#   group_by(occasion, tag) %>%
#   summarise(trap_unique = n_distinct(trap))

######## first, get the data to a nestedlynested list
#first order: 12 lists, 1 per site
  #second order: 6 occasions per site
    #third order: 1 list per individual (with all locations) for each occasion

#take fulltrap, group by tag then occasion and remove any individuals only captured once in an occasion
fulltrap_occ <- fulltrap %>%
  group_by(occasion, tag) %>%
  mutate(trap_unique = n_distinct(trap)) %>%
  filter(trap_unique > 1) %>%
  dplyr::select(-trap_unique) %>%
  ungroup() %>%
  ungroup()

#turn fulltrap into a list of elements by site
fulltrap_occ_list <- split(fulltrap_occ, f = fulltrap_occ$site)
#and then split each site into a nested list by occasion
fulltrap_occ_list <- lapply(fulltrap_occ_list, function(x) split(x, x$occasion))

#and then make each occasion into a series of lists by tag ID

for(i in 1:length(fulltrap_occ_list)){
  
  print(i)
  
  for(j in 1:length(fulltrap_occ_list[[i]])){
    
    #split each occasion into a set of nested lists by tag ID
    print(j)
    
    temp1 <- fulltrap_occ_list[[i]][[j]] %>% 
      group_by(tag) %>%
      dplyr::select(x,y) %>%
      ungroup()
    
    temp2 <- split(temp1, f=temp1$tag)  
    
    fulltrap_occ_list[[i]][[j]] <- temp2
    
  }
  
}


## this list (fulltrap_occ_list) will include all occasions for a site, even if no animals were captured
#need to remove any sublists that don't have data

for(i in 1:length(fulltrap_occ_list)){
  
  temp <- list()
  temp <- compact(fulltrap_occ_list[[i]]) #compact() is part of the purrr package, in tidyverse
  fulltrap_occ_list[[i]] <- temp
  
}


####### so far I have a list of 12 sites with nested lists for each occasion
#### each occasion contains nested lists - 1 for each animal captured in >1 trap during that occasion
############# okay, now we have the listedlylisty list to actually calculate the stuff ######################
  


### step (6) calculate max dist moved for every individual for each occasion across all sites

#this code will that go through each of the site/occasion/animals and calculate the max distance moved
#and report a table per site/occasion of the max distances of all the animals captured in >1 trap during that occasion

dist_list_occ <- list()

for(i in 1:length(fulltrap_occ_list)){
  
  #this is the site level
  #print(i)
  
  #make a list for each site (12 total)
  dist_list_occ[[i]] <- list()


  for(j in 1:length(fulltrap_occ_list[[i]])){
    
    #this is occasion within a site
    #print(j)
    
    #for each occasion, make a table (temp) that has all the tag IDs and space to fill in the maxdist
    tag <- names(fulltrap_occ_list[[i]][[j]])
    temp <- data.frame("tag" = tag, "maxdist" = integer(length(names(fulltrap_occ_list[[i]][[j]]))))
    
    for(k in 1:length(fulltrap_occ_list[[i]][[j]])){
      
      #these are the individuals trapped >2x per that occasion
      #print(k)
      
      #for each individual in that site/occasion, calculate their max dist moved
      locs <- fulltrap_occ_list[[i]][[j]][[k]] %>% dplyr::select(x,y)
      max <- max(dist(locs, method="euclidean"))
      
      #populate a table for that site/occasion
      temp[k,2] <- max
      
    }
    
    #write the table to that each site/occasion
    dist_list_occ[[i]][[j]] <- temp
    
    #do this for all occasions per site
    
  }
  
  #make sure the month (occasion) names stay with the 2nd order elements
  names(dist_list_occ[[i]]) <- names(fulltrap_occ_list[[i]])
  
  #do this for all sites
  
}

#name the 12 1st order elements of nets_list as their sites
names(dist_list_occ) <- names(fulltrap_occ_list)


# (2nd order items (the occasions per site) are still just numbers)


############ STILL TO DO:    Figure out how to get this shirt out of a list and into a df     ####################



dist_list_occ_grid <- list()

for(i in 1:length(dist_list_occ)){

  summary <- do.call("rbind", dist_list_occ[[i]])
  dist_list_occ_grid[[i]] <- summary

}
#and make sure all the 1st order elements are the grid names
names(dist_list_occ_grid) <- names(dist_list_occ)
 


#for each occasion in the list, move row names to column, get rid of the number

for(i in 1:length(dist_list_occ_grid)){
  
  #move the row names to a column
  dist_list_occ_grid[[i]] <- tibble::rownames_to_column(dist_list_occ_grid[[i]], "row")
  #separate the ".#" and get rid of that column
  #add a column for occasion
  dist_list_occ_grid[[i]] <- dist_list_occ_grid[[i]] %>% 
    separate("row", c("site", "id")) %>% 
    dplyr::select(-id)
  
}





########################################################################################################################################
##################### END of the good stuff ############################









### (completed) -- Between Occasion Movement ###
#to calculate distance moved between 2 occasions (1,2 - 2,3 - etc.)
#there's likely a way to do this with the data already as it is, but I don't know what that is
#it makes most sense to me to make a new column for this
#so a column combining occasion 1 and 2 into "1-2" and 2 and 3 into "2-3" 
#the only problem this creates is that captures will be in two different groupings - 
  #a capture in occasion 2 will be in the 1-2 and the 2-3 groups
#so that would require repeat rows of the same capture so it can appear with two different groupings?
#that doesn't sound great
#what if, I create a separate df for each pairing? (MayJune, JuneJuly, JulyAug, AugSept, SeptOct)
#that is 5 different dfs which is kind of annoying but for now that might be the easiest way?

mayjune_trap <- fulltrap %>%
  filter(occasion == "1" | occasion == "2")

#and then we want to turn these into lists by site, by critter

#take trapping data and remove any individuals only captured once
mayjune_trap <- mayjune_trap %>%
  group_by(tag) %>%
  mutate(trap_unique = n_distinct(trap)) %>%
  filter(trap_unique > 1) %>%
  dplyr::select(-trap_unique) %>%
  ungroup()
#turn monthpair trapping data into a list of elements by site
mayjunetrap_list <- split(mayjune_trap, f = mayjune_trap$site)
#and then split each site into a nested list by tag
mayjunetrap_list <- lapply(mayjunetrap_list, function(x) split(x, x$tag))

###### repeat for the other monthpair trapping data

#make monthpair df from fulltrap
junejuly_trap <- fulltrap %>%
  filter(occasion == "2" | occasion == "3")
#take trapping data and remove any individuals only captured once
junejuly_trap <- junejuly_trap %>%
  group_by(tag) %>%
  mutate(trap_unique = n_distinct(trap)) %>%
  filter(trap_unique > 1) %>%
  dplyr::select(-trap_unique) %>%
  ungroup()
#turn monthpair trapping data into a list of elements by site
junejulytrap_list <- split(junejuly_trap, f = junejuly_trap$site)
#and then split each site into a nested list by tag
junejulytrap_list <- lapply(junejulytrap_list, function(x) split(x, x$tag))

#make monthpair df from fulltrap
julyaug_trap <- fulltrap %>%
  filter(occasion == "3" | occasion == "4")
#take trapping data and remove any individuals only captured once
julyaug_trap <- julyaug_trap %>%
  group_by(tag) %>%
  mutate(trap_unique = n_distinct(trap)) %>%
  filter(trap_unique > 1) %>%
  dplyr::select(-trap_unique) %>%
  ungroup()
#turn monthpair trapping data into a list of elements by site
julyaugtrap_list <- split(julyaug_trap, f = julyaug_trap$site)
#and then split each site into a nested list by tag
julyaugtrap_list <- lapply(julyaugtrap_list, function(x) split(x, x$tag))

#make monthpair df from fulltrap
augsept_trap <- fulltrap %>%
  filter(occasion == "4" | occasion == "5")
#take trapping data and remove any individuals only captured once
augsept_trap <- augsept_trap %>%
  group_by(tag) %>%
  mutate(trap_unique = n_distinct(trap)) %>%
  filter(trap_unique > 1) %>%
  dplyr::select(-trap_unique) %>%
  ungroup()
#turn monthpair trapping data into a list of elements by site
augsepttrap_list <- split(augsept_trap, f = augsept_trap$site)
#and then split each site into a nested list by tag
augsepttrap_list <- lapply(augsepttrap_list, function(x) split(x, x$tag))

# #make monthpair df from fulltrap
# septoct_trap <- fulltrap %>%
#   filter(occasion == "5" | occasion == "6")
# #take trapping data and remove any individuals only captured once
# septoct_trap <- septoct_trap %>%
#   group_by(tag) %>%
#   mutate(trap_unique = n_distinct(trap)) %>%
#   filter(trap_unique > 1) %>%
#   dplyr::select(-trap_unique) %>%
#   ungroup()
# #turn monthpair trapping data into a list of elements by site
# septocttrap_list <- split(septoct_trap, f = septoct_trap$site)
# #and then split each site into a nested list by tag
# septocttrap_list <- lapply(septocttrap_list, function(x) split(x, x$tag))


## and then I can use the code from a single occasion to calculate distance moved in the occasion pair

### calculate distance moved for every individual at all 12 grids (in a single monthpair)

dist_list_mayjune <- list()

for(i in 1:length(mayjunetrap_list)){
  print(i)
  tag <- names(mayjunetrap_list[[i]])
  site <- data.frame(tag)
  
  for(j in 1:length(mayjunetrap_list[[i]])){
    locs <- mayjunetrap_list[[i]][[j]] %>% dplyr::select(x,y)
    max <- max(dist(locs, method="euclidean"))
    site$maxdist[j] <- max
  }
  dist_list_mayjune[[i]] <- site
}

#name the 12 1st order elements of nets_list as their sites
names(dist_list_mayjune) <- names(mayjunetrap_list)


############ then repeat that code for all the other month pairs

#junejuly dist list
dist_list_junejuly <- list()
for(i in 1:length(junejulytrap_list)){
  print(i)
  tag <- names(junejulytrap_list[[i]])
  site <- data.frame(tag)
  for(j in 1:length(junejulytrap_list[[i]])){
    locs <- junejulytrap_list[[i]][[j]] %>% dplyr::select(x,y)
    max <- max(dist(locs, method="euclidean"))
    site$maxdist[j] <- max
  }
  dist_list_junejuly[[i]] <- site
}
#name the 12 1st order elements of nets_list as their sites
names(dist_list_junejuly) <- names(junejulytrap_list)


#julyaug dist list
dist_list_julyaug <- list()
for(i in 1:length(julyaugtrap_list)){
  print(i)
  tag <- names(julyaugtrap_list[[i]])
  site <- data.frame(tag)
  for(j in 1:length(julyaugtrap_list[[i]])){
    locs <- julyaugtrap_list[[i]][[j]] %>% dplyr::select(x,y)
    max <- max(dist(locs, method="euclidean"))
    site$maxdist[j] <- max
  }
  dist_list_julyaug[[i]] <- site
}
#name the 12 1st order elements of nets_list as their sites
names(dist_list_julyaug) <- names(julyaugtrap_list)


#augsept dist list
dist_list_augsept <- list()
for(i in 1:length(augsepttrap_list)){
  print(i)
  tag <- names(augsepttrap_list[[i]])
  site <- data.frame(tag)
  for(j in 1:length(augsepttrap_list[[i]])){
    locs <- augsepttrap_list[[i]][[j]] %>% dplyr::select(x,y)
    max <- max(dist(locs, method="euclidean"))
    site$maxdist[j] <- max
  }
  dist_list_augsept[[i]] <- site
}
#name the 12 1st order elements of nets_list as their sites
names(dist_list_augsept) <- names(augsepttrap_list)


# #septoct dist list
# dist_list_septoct <- list()
# for(i in 1:length(septocttrap_list)){
#   print(i)
#   tag <- names(septocttrap_list[[i]])
#   site <- data.frame(tag)
#   for(j in 1:length(septocttrap_list[[i]])){
#     locs <- septocttrap_list[[i]][[j]] %>% dplyr::select(x,y)
#     max <- max(dist(locs, method="euclidean"))
#     site$maxdist[j] <- max
#   }
#   dist_list_septoct[[i]] <- site
# }
# #name the 12 1st order elements of nets_list as their sites
# names(dist_list_septoct) <- names(septocttrap_list)



# convert a monthpair list to a df and clean it up
mayjune_dist <- data.frame(do.call(rbind.data.frame, dist_list_mayjune))
#move the row names to a column
mayjune_dist <- tibble::rownames_to_column(mayjune_dist, "row")
#separate the ".#" and get rid of that column
#add a column for occasionpair
mayjune_dist <- mayjune_dist %>% 
  separate("row", c("site", "id")) %>% 
  dplyr::select(-id) %>%
  mutate(occasionpair = "mayjune")

# convert a monthpair list to a df and clean it up
junejuly_dist <- data.frame(do.call(rbind.data.frame, dist_list_junejuly))
#move the row names to a column
junejuly_dist <- tibble::rownames_to_column(junejuly_dist, "row")
#separate the ".#" and get rid of that column
#add a column for occasionpair
junejuly_dist <- junejuly_dist %>% 
  separate("row", c("site", "id")) %>% 
  dplyr::select(-id) %>%
  mutate(occasionpair = "junejuly")

# convert a monthpair list to a df and clean it up
julyaug_dist <- data.frame(do.call(rbind.data.frame, dist_list_julyaug))
#move the row names to a column
julyaug_dist <- tibble::rownames_to_column(julyaug_dist, "row")
#separate the ".#" and get rid of that column
#add a column for occasionpair
julyaug_dist <- julyaug_dist %>% 
  separate("row", c("site", "id")) %>% 
  dplyr::select(-id) %>%
  mutate(occasionpair = "julyaug")

# convert a monthpair list to a df and clean it up
augsept_dist <- data.frame(do.call(rbind.data.frame, dist_list_augsept))
#move the row names to a column
augsept_dist <- tibble::rownames_to_column(augsept_dist, "row")
#separate the ".#" and get rid of that column
#add a column for occasionpair
augsept_dist <- augsept_dist %>% 
  separate("row", c("site", "id")) %>% 
  dplyr::select(-id) %>%
  mutate(occasionpair = "augsept")

# # convert a monthpair list to a df and clean it up
# septoct_dist <- data.frame(do.call(rbind.data.frame, dist_list_septoct))
# #move the row names to a column
# septoct_dist <- tibble::rownames_to_column(septoct_dist, "row")
# #separate the ".#" and get rid of that column
# #add a column for occasionpair
# septoct_dist <- septoct_dist %>% 
#   separate("row", c("site", "id")) %>% 
#   dplyr::select(-id) %>%
#   mutate(occasionpair = "septoct")



## combine all the monthpair distances into one df
monthpair_dist <- bind_rows(mayjune_dist, junejuly_dist, julyaug_dist, augsept_dist) #this will need septoct to be added
