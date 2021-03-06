# code run using R version 4.0.2 (2020-06-22) -- "Taking Off Again"

# install CMRnet
# remotes::install_github("matthewsilk/CMRnet", build_vignettes = FALSE)

# load packages
library(here)
library(tidyverse)
library(lubridate)
library(janitor)
# library(mosaic) #mosaic does stats stuff, useful but not necessary yet 11.29.21


#clear environment
rm(list = ls())

#load the fulltrap dataset (make sure it's the most recent version)
fulltrap <- readRDS(file = "fulltrap_12.30.21.rds")
## NOTE ## as of 11.18.21 version, all DP, DT, or S animals are still in the fulltrap dataset



#######################################################################################################################
############ calculate the number of days between occasions so the between occasion networks are accurate #############
#######################################################################################################################

# #summarise fulltrap to have each site and the start and end date of all trapping occasions at that site
# trapdates <- fulltrap %>%
#   arrange(occasion) %>%
#   group_by(site, occasion) %>%
#   summarise(start = min(date_time),
#             end = max(date_time))
# 
# trapdates$occasion <- as.numeric(trapdates$occasion)
# 
# #create a new df that will show the time interval between start of one occasion and end of the next
# 
# ######## Matt M-S wrote this code ################
# 
# intervals <- trapdates %>%
#   group_by(site) %>%
#   summarise(occpair = paste(occasion, occasion+1, sep="-"),
#             days.between = as.duration(start %--% lead(end)) / ddays(1)) %>%
#   filter(occpair != "6-7")
# 
# 
# max(intervals$days.between)
# #max 33.41 days
# min(intervals$days.between)
# #min 27.41 days

###################################################################################################




##################################################################################################
######################################### CMRnet analysis #########################################
##################################################################################################

library(CMRnet)
library(igraph)

#############################################################################################
######################################### set up the data ###################################
##############################################################################################

#CMRnet requires a dataframe with columns (in this order) id, loc, x, y, date
#loc refers to where the animal was caught - so trap ID
#but I also want to look at networks separately by site so I'll keep site in for now

#create the cmr dataframe, rename columns
cmr_full <- data.frame(fulltrap$tag, fulltrap$site, fulltrap$trap, fulltrap$x, fulltrap$y, fulltrap$date_time)
names(cmr_full) <- c("id","site","loc","x","y","date")

# str(cmr_full)
# #confirm x and y are properly numeric
# cmr_full$x <- as.numeric(unlist(cmr_full$x))
# cmr_full$y <- as.numeric(unlist(cmr_full$y))
# #CMRnet wants the date in 'Date' format, no time (using lubridate pkg, not base R as.Date() function)
# cmr_full$date <- as_datetime(cmr_full$date, origin = lubridate::origin)


#subset cmr_full into separate data.frames by site (and remove the 'site' column)
#split() makes a list consisting of individual data.frames based on a condition ('site' in this case)
#the list contains 12 subsets of cmr_full, one for each 'site'
#each of these is the full site-level dataset with the needed columns for CMRnet analysis
cmr_list <- split(cmr_full, cmr_full$site)



#################################################################################################
##################  loop to create network objects for all 12 sites at once #####################
#################################################################################################

#hold onto your pants, we're attempting a loop

#create list to store output network data
nets_list <- list()

#for loop to take each item in cmr_list...
#remove site and then run the network function on it
#save each index of cmr_list to its own index in nets_list
#and name each new index with the same name (done alphabetically because R)

#add print(i) at start of troublesome loops to see where it's breaking ^^

######################### TROUBLESHOOT: can't convert to igraph with 1 individual in net 
    ## so had to remove 1 indiv from Kuoppa and Janakkala from May 2021 in order to start with May trapping
    ## so now there are no animals at Kuoppa or Janakkala in May (but there was actually 1 at each)
    ## this is the only way to still have May networks for the other grids (lol, does that even matter?)

#Define parameters for the network
mindate<- as.Date("2021-04-30") 
maxdate<- as.Date("2021-10-30")
#interaction window - time for a contact (days)
intwindow<-34 #this is based off the max window between occasions at any site (see above)
#network window (months)
netwindow<-2
overlap<-1 #overlap is 1 month so we get may-june and june-july
#distance to consider contact (0=same trap)
spacewindow<-1.5

for(i in 1:length(cmr_list)){
  
  print(i)
  
  tempdf <- cmr_list[[i]] #temp storage for each site in loop
  tempdf <- select(tempdf, -site) #remove site because CMRnet doesn't want the extra column
  
  nets_list[[i]] <- DynamicNetCreate(data = tempdf,
                                     intwindow = intwindow,
                                     mindate = mindate,
                                     maxdate = maxdate,
                                     netwindow = netwindow,
                                     overlap = overlap,
                                     spacewindow = spacewindow,
                                     index=FALSE)
  
}
#this has now made a list of network objects, 1 set of networks per site

#name the 12 1st order elements of nets_list as their sites
names(nets_list) <- names(cmr_list)

#rename the sublist items for each grid
for(i in 1:length(nets_list)){
  names(nets_list[[i]]) <- c("edgelist", "adjmatrix", "netwindows")
}

#to access the 1st 2ndorder element in each of the 12 elements in nets_list (e.g. the edge lists)
#lapply(nets_list,'[[',1)

#you can access named elements in a list using the $ convention, just stack them
#nets_list$hevonen$edgelist

##each [[#]] in nets_list corresponds to a grid (12 total)
## each [[#]] contains 3 things:
## first is an edge list, second is adjacency matrix, third is which netwindow each individual is in

## nets_list[[1]][[1]] is array of edge lists for all network windows at site 1 (asema)
## nets_list[[1]][[1]][,,1] is just the edge list for asema during network window 1 (May)

###########################################   END   ###################################################
#######################################################################################################




###########################################################################################################
############## calculate network metrics for a list of networks (1 network/site)  #########################
########################  FOR ALL 12 SITES   ###############  IN A LOOP  ##################################
###########################################################################################################

# Create list to store results
net_mets_list <- list()

# Calculate network metrics - using the nets_list list (from dynamicnetcreate() function)
for(i in 1:length(nets_list)){
  
  #length(nets_list) == for each of the 12 sites
  
  print(i)
  site <- list()
  
  for(j in 1:5){
    
    #(j in 1:5) is for the 5 trapping occasion pairs, this is an easy enough thing to write in
    
    #only want the animals on the grid that month in the network
    #nets_list[SITE][3] has the info on who is in which netwindow
    #need the column of $netwindows that corresponds with occasion (column 1 = id, 2=occ 1, etc.)
    ongrid <- nets_list[[i]][[3]][ ,c(1,(j+1))] 
    #pull tag ids (column 1) & the correct column (j+1) of the netwindows df (1,0 if on grid or not) for occasion j
    ongrid <- ongrid %>% filter(.[ ,2] == 1) #filter for only animals on the grid that month
    ids <- as.vector(ongrid$ids) #get tag IDs as a vector
    
    adjmat <- nets_list[[i]][[2]][,,j] #pull the adjmatrix for the given occasion (includes all animals)
    adjmat <- adjmat[ids,ids] #subset the adjmatrix for only the animals on the grid that month
    
    #make a network from the subset adj matrix for a given occasion (j)
    inet <- graph.adjacency(adjmat, weighted=NULL, mode="undirected") 
    
    #set up the df to hold the network metrics
    occ <- rep(j,length(ids)) #this puts j==occasion # in a column for all animals
    # in this month-pair version, it just counts the paired occasions 1=may/june 2=june/july etc.
    site[[j]] <- data.frame(ids,occ) #start the df with the tag numbers of all ongrid animals and the occasion #
    
    
    #network metrics to calculate
    site[[j]]$deg <- igraph::degree(inet)
    #site[[j]]$eig <- igraph::eigen_centrality(inet)$vector
    #eigenvector centrality takes into account second-order connections (ie, friends of friends)
    site[[j]]$betw <- igraph::betweenness(inet)
    #betweeness is how often you're on the shortest path between others - role in connecting the network
    #site[[j]]$close <- igraph::closeness(inet) #not good for disconnected graphs
    #closeness measures normalized path length from you to all others in network
    site[[j]]$clust.node <- igraph::transitivity(inet, type="local") #type="local" for each node ="global" for whole network
    site[[j]]$clust.net <- rep(igraph::transitivity(inet, type="global"), length(ids))
    #transitivity (clustering coef) is ratio of closed triplets to possible triplets
    #tightly connected communities have high transitivity
    site[[j]]$netdens <- rep(igraph::edge_density(inet, loops=FALSE), length(ids)) #network density
    #site[[j]]$avgdeg <- rep(mean(site[[j]]$deg), length(ids)) #calculate average degree for a site/occasion
    site[[j]]$components <- rep(igraph::count_components(inet), length(ids)) #number of components 
    site[[j]]$netsize <- rep(igraph::gorder(inet), length(ids)) #gorder gives number of nodes in the net
    
    #determine number of clusters (using edge.betweenness - THERE MIGHT BE A BETTER WAY)
    if( all(adjmat == 0) ) { site[[j]]$n.clust <- rep(NaN, length(ids)) } 
    else {
      eb <- edge.betweenness.community(inet)
      site[[j]]$n.clust <- rep(length(eb), length(ids)) #number of clusters (not necessarily number of components)
    }
    #calculate modularity
    if( all(adjmat == 0) ) { site[[j]]$mod <- rep(NaN, length(ids)) } 
    else {
      site[[j]]$mod <- rep(modularity(eb), length(ids)) #values -1 to 1, 0 means heterogeneous connections, positive values indicate modular structure
    }


  }
  net_mets_list[[i]] <- site
}

######### a note on calculating Network Density ###################
# calculated per network = ratio of realized edges:possible edges
# edge_density(igraph network file, loops=FALSE) 
## a loop is an edge connecting a node back to itself, FALSE if these are meaningless in your network


#name the 12 1st order elements of nets_list as the sites
names(net_mets_list) <- names(cmr_list)

#rename the sublist items (months) for each site
for(i in 1:length(net_mets_list)){
  names(net_mets_list[[i]]) <- c("may-june", "june-july", "july-aug", "aug-sept", "sept-oct")
}



######### a note on calculating Network Density ###################
# calculated per network = ratio of realized edges:possible edges
# edge_density(igraph network file, loops=FALSE) 
## a loop is an edge connecting a node back to itself, FALSE if these are meaningless in your network

#name the 12 1st order elements of nets_list as the sites
names(net_mets_list) <- names(cmr_list)

#rename the sublist items (months) for each site
#accounting for the fact that some sites have 5 months of data and others have 6
for(i in 1:length(net_mets_list)){
  ifelse( length(net_mets_list[[i]]) == 6, names(net_mets_list[[i]]) <- c("may", "june", "july", "aug", "sept", "oct"), 
          names(net_mets_list[[i]]) <- c("june", "july", "aug", "sept", "oct") )
}


################################### ABOUT net_mets_list ################################################
#the output of net_mets_list is a list of 12 1st-order items, 1 per site
  #under each site, there are 5 second-order items, 1 per occasion pair (named by month names)
    #each of those 2nd-order items (monthpairs) is a df with tag ID, occasion #, and all the network metrics...
      #for ONLY THE ANIMALS captured on that grid, during that occasion
      # (column that says whether the animal was on the grid has been removed)
########################################################################################################



############## clean net_mets_list to make it easier to use for analysis #######################

#collate results
#this collapses the 2nd order elements (network metrics for a single month) down to the 1st order element (site)
#so now net_mets_list_summary is a list of 12 dfs, each is all the network metrics for a site across all the networkwindows

#make a list to store things
net_mets_list_summary <- list()

#loop across all sites and collapse the dfs per occasion into one df for the site
for(i in 1:length(net_mets_list)){
  
  #for all 12 sites
  summary <- do.call("rbind", net_mets_list[[i]])
  net_mets_list_summary[[i]] <- summary
}

#remove the row names of the dfs
for(i in 1:length(net_mets_list_summary)){
  row.names(net_mets_list_summary[[i]]) <- NULL
}


#name the 12 1st order elements as their sites
names(net_mets_list_summary) <- names(cmr_list)


#change the numeric occasions to actual month words
for(i in 1:length(net_mets_list_summary)){
  net_mets_list_summary[[i]]$occ <- as.factor(net_mets_list_summary[[i]]$occ)
  net_mets_list_summary[[i]]$occ <-  net_mets_list_summary[[i]]$occ %>%
    recode_factor("1" = "may-june", "2" = "june-july", "3" = "july-aug", "4" = "aug-sept", "5" = "sept-oct")
}


### HOT DAMN - net_mets_list_summary is now a list of 12 elements where each element represents a site...
  # and contains a df of all the individuals on the grid that month, their tag number, and all their network metrics

# net_mets_list_summary$vaarinkorpi




## make net_mets_list_summary into freiggein huge df
net_mets_summary <- do.call(rbind.data.frame, net_mets_list_summary)

#clean up the df
net_mets_summary <- net_mets_summary %>% 
  rownames_to_column("name") %>% #row names are the sites, make that a column
  separate(name, c("site", NA)) %>% #separate the site part from the index and get rid of the index
  mutate(site = as.factor(site)) #make site a factor

#read in a csv of the grid treatments
grid_trts <- read.csv(here("grid_trts.csv"))
#combine food_trt and helm_trt into single 'trt' column
grid_trts <- grid_trts %>% 
  unite(trt, food_trt, helm_trt, sep = "_", remove = FALSE) #keep the original food_trt and helm_trt columns

#join the grid_trts to net_mets_summary
net_mets_summary <- net_mets_summary %>% 
  left_join(y=grid_trts, by = "site")




##### for shiggles, here are some (not so exciting) plots
net_mets_summary %>% ggplot(aes(x=occ, y=deg, fill=site)) + geom_boxplot()
net_mets_summary %>% ggplot(aes(x=occ, y=deg, fill=trt)) + 
  geom_boxplot() +
  labs(x="occasion", y="individual vole degree (edge count)", 
       title="individual vole degree by treatment")
net_mets_summary %>% ggplot(aes(x=occ, y=deg, fill=food_trt)) + 
  geom_boxplot() +
  labs(x="occasion", y="individual vole degree (edge count)", 
       title="effect of food addition on individual vole degree")

net_mets_summary %>% ggplot(aes(x=occ, y=netdens, fill=site)) + geom_point(aes(color=site))
net_mets_summary %>% ggplot(aes(x=occ, y=netdens, fill=trt)) + 
  geom_boxplot() + 
  labs(x="occasion", y="network density", title="network density by treatment type")
net_mets_summary %>% ggplot(aes(x=occ, y=netdens, fill=food_trt)) + geom_boxplot()

net_mets_summary %>% ggplot(aes(x=occ, y=netsize, fill=site)) + geom_point(aes(color=site))
net_mets_summary %>% ggplot(aes(x=occ, y=netsize, fill=trt)) + 
  geom_boxplot() +
  labs(x="occasion", y="network size", title="network size by treatment type")
net_mets_summary %>% ggplot(aes(x=occ, y=netsize, fill=food_trt)) + 
  geom_boxplot() + 
  labs(x="occasion", y="network size", title="effect of food addition on network size")

net_mets_summary %>% ggplot(aes(x=occ, y=clust.net, fill=site)) + geom_point(aes(color=site))
net_mets_summary %>% ggplot(aes(x=occ, y=clust.net, fill=trt)) + 
  geom_boxplot() + 
  labs(x="occasion", y="clustering coefficient", title="network-level clustering by treatment type")
net_mets_summary %>% ggplot(aes(x=occ, y=clust.net, fill=food_trt)) + geom_boxplot()
net_mets_summary %>% ggplot(aes(x=occ, y=clust.net, fill=helm_trt)) + geom_boxplot()

net_mets_summary %>% ggplot(aes(x=occ, y=clust.node, fill=site)) + geom_point(aes(color=site))
net_mets_summary %>% ggplot(aes(x=occ, y=clust.node, fill=trt)) + geom_boxplot()
net_mets_summary %>% ggplot(aes(x=occ, y=clust.node, fill=food_trt)) + geom_boxplot()
  
net_mets_summary %>% ggplot(aes(x=occ, y=mod)) + geom_point(aes(color=trt))
net_mets_summary %>% ggplot(aes(x=occ, y=mod, fill=trt)) + 
  geom_boxplot() +
  labs(x="occasion", y = "modularity", title="modularity score by treatment")
net_mets_summary %>% ggplot(aes(x=occ, y=mod, fill=food_trt)) + geom_boxplot()









###########################################################################################################
############################ convert to igraph objects  --   in a loop #################################
##########################  this list is really only useful for plotting #################################
############################################################################################################

#first make a list to store the output
igraph_list <- list()

#loop to convert all the items in nets_list to igraph objects

for(i in 1:length(nets_list)){
  print(i)
  igraph_list[[i]] <- cmr_igraph(nets_list[[i]], type="social")
}

#name the 12 1st order elements of igraph_list as their sites
names(igraph_list) <- names(cmr_list)

#rename the sublist items for each grid
for(i in 1:length(igraph_list)){
  names(igraph_list[[i]]) <- c("edgelist", "aggnet")
}

####### REMINDER #########
# cmr_igraph() function converts output of DynamicNetCreate() to list of igraph objects
# nested list with 2 levels of hierarchy
# igraph_list[[1]] = first element = list of igraph network objects (edgelists), 1 per network window
# second element = full aggregated network (edge list?) for all critters, entire study period


########### Alternatively  ####################
#lapply to make a big list of igraph objects
#test2 <- lapply(nets_list, function(x) cmr_igraph(x, type="social"))


##################################################################################################

#plot networks in a loop
for(i in 1:length(igraph_list)) {
  CMRnet::cmrSocPlot(nets=igraph_list[[i]],fixed_locs=TRUE,dynamic=FALSE,vertex.label=NA, vertex.size=5)
  mtext(paste(names(igraph_list)[i], "- between occasion"), side = 3, line = -15, outer = TRUE)
}

##### WORKING ##### how to change the labels above each network and how to give the whole shebang a title



#then save the networks to .png in a loop
for(i in 1:length(igraph_list)) {
  png(paste("betweenocc_", names(igraph_list)[i], "_2021", ".png", sep = ""))
  
  CMRnet::cmrSocPlot(nets=igraph_list[[i]],fixed_locs=TRUE,dynamic=FALSE,vertex.label=NA, vertex.size=5)
  mtext(paste(names(igraph_list)[i], "- between occasion"), side = 3, line = -50, outer = TRUE)
  
  dev.off()
}

####################################### END plotting networks #########################################






####################################################################################################################
#######################################    Degree Distributions    #################################################
####################################################################################################################

#create dfs of the frequency of degree counts for each site/monthpair - to be used for degree distribution histograms

# Create list to store results
degreefreq_list <- list()

# Calculate network metrics to use
for(i in 1:length(net_mets_list)){
  
  print(i)
  site <- list()
  
  for(j in 1:length(net_mets_list[[i]])){
    #each occasion under a site
    
    degree <- net_mets_list[[i]][[j]]$deg
    degree.df <- data.frame(table(degree=factor(degree, levels=seq(0, max(degree), by=1))))
    degree.df$degree <- as.numeric(as.character(degree.df$degree))
    
    site[[j]] <- degree.df #write df for each occasion as a separate item under 1st order site
    
  }
  degreefreq_list[[i]] <- site
}

#name the 12 1st order elements of nets_list as the sites
names(degreefreq_list) <- names(cmr_list)

#rename the sublist items (monthpairs) for each site
for(i in 1:length(degreefreq_list)){
  names(degreefreq_list[[i]]) <- c("may-june", "june-july", "july-aug", "aug-sept", "sept-oct")
}

################## ABOUT degreefreq_list ####################
# a list of 12 items (1 per site), with 5 sub-items each (1 per monthpair)
# under each site/monthpair, is a df of degree count and frequency of occurrence



########compress degreefreq_list down to a df

#collate results
#this collapses the 2nd order elements (degree distributions for a monthpair) down to the 1st order element (site)
#so now degreefreq_list_summary is a list of 12 dfs, each is all the degree distributions for a site across for all monthpairs

#make a list to store things
degreefreq_list_summary <- list()

#loop across all sites and collapse the dfs per occasion into one df for the site
for(i in 1:length(degreefreq_list)){
  
  #for all 12 sites
  summary <- do.call("rbind", degreefreq_list[[i]])
  
  #the monthpair is in the row name, make this a column
  #row names are the sites, make that a column 
  #separate the site part from the index and get rid of the index
  summary <- summary %>% rownames_to_column("name") %>% 
    separate(name, c("occ.pair", NA), sep = "[.]") %>% 
    mutate(occ.pair = as.factor(occ.pair))
  
  degreefreq_list_summary[[i]] <- summary
  
}

#name the 12 1st order elements as their sites
names(degreefreq_list_summary) <- names(cmr_list)

## make degreefreq_list_summary into freiggein huge df
degreefreq_summary <- do.call(rbind.data.frame, degreefreq_list_summary)

#clean up the df
degreefreq_summary <- degreefreq_summary %>% 
  rownames_to_column("name") %>% #row names are the sites, make that a column
  separate(name, c("site", NA)) %>% #separate the site part from the index and get rid of the index
  mutate(site = as.factor(site)) #make site a factor
degreefreq_summary <- degreefreq_summary %>% clean_names()



# per site, occasion - summarize max degree, min degree, avg degree (weighted) - max frequency
degreestats <- degreefreq_summary %>%
  group_by(site, occ_pair) %>%
  filter(freq > 0) %>%
  summarise(min.deg = min(degree),
            max.deg = max(degree),
            avg.deg = round(weighted.mean(degree, freq),2), 
            max.freq = max(freq), 
            deg.max.freq = degree[which.max(freq)]) %>%
  left_join(y=grid_trts, by = "site") #join the grid treatments for plotting
#occasions reorganized themselves alphabetically?
degreestats$occ_pair <- factor(degreestats$occ_pair, levels = c("may-june", "june-july", "july-aug", "aug-sept", "sept-oct"))

#### TROUBLESHOOT #### sometimes the max freq is shared across several degree values but only the first is reported above ^^
# test <- degreefreq_summary %>% group_by(site, occ) %>% filter(freq == max(freq))



#plot degree stats
degreestats %>% ggplot(aes(x=occ_pair, y=max.deg, fill=trt)) + 
  geom_boxplot() +
  labs(title = "maximum degree value by occasion pair", x = "occasion pair", y = "maximum degree value")

degreestats %>% ggplot(aes(x=occ_pair, y=avg.deg, fill=trt)) + 
  geom_boxplot() +
  labs(title = "average degree value by occasion pair", x = "occasion pair", y = "average degree value")






###################################################


#load packages for plotting multiple plots together (the tidyverse parmfrow)
library(gridExtra) #for the grid.arrange() function
library(grid) #this is to add a title to the grid.arrange() complex o' graphs


#create a list to store output
plot_list <- list()

#loop through degreefreq_list and make plots for each site/occasion
#save all plots for a site to a list
#print the plots for a site using grid.arrange() - save to .png
for(i in 1:length(degreefreq_list)){
  
  print(i)
  site <- list()
  
  for(j in 1:length(degreefreq_list[[i]])){
    
    df <- degreefreq_list[[i]][[j]]
    plot <- ggplot(df, aes(x=degree, y=Freq)) + 
      geom_bar(stat="identity") +
      xlim(-1, 155) + #max degree is 151
      ylim(0,10)
    site[[j]] <- plot
    
  }
  
  plot_list[[i]] <- site
  
  #save the grid.arrange() of all the degdist plots as a .png
  png(paste("bwocc_degdist_", names(degreefreq_list)[i], "_2021", ".png", sep = ""))
  do.call(grid.arrange, c(plot_list[[i]], 
                          ncol=3, 
                          top = paste(names(degreefreq_list)[i], "degree distributions", sep = " ")))
  dev.off()
  
  ####axes could have better tick marks
  ####there could be more code here to define the dimensions of the output
  
}

####################################################################################################################


























###################################################################################################
####                         NETWORK THINGS FOR INDIVIDUAL SITES                               ####
###################################################################################################

#to visualize networks at each site, need individual dfs by site
#subset cmr_list into separate data.frames by site (and remove the 'site' column)

# cmr_list
#the list contains several subsets of the df each named based on the values in 'site'
#but that list isn't accessible in your environment, you'd have to call on it from the list --> cmr_list$puro
#list2env(cmr_list, .GlobalEnv) --> this converts the list to individual things (dfs) in your environment
#and the lapply() part (listapply) is removing the second column (site) from each of the resulting dfs 
#does not remove the 'site' column from the df stored within cmr_list

list2env(lapply(cmr_list, '[',-2), .GlobalEnv)


#############################     code from MattSilk github       #############################
#####################   to make a network for just one site at a time   #######################

#Define parameters for the network
mindate<- as.Date("2021-04-30") 
maxdate<- as.Date("2021-10-30")
#interaction window - time for a contact (days)
intwindow<-34
#network window (months)
netwindow<-2
overlap<-1
#distance to consider contact (0=same trap)
spacewindow<-1.5

# Create co-capture (social) networks
#index=FALSE indicates that we want edges to be weighted by the number of interactions
#index=TRUE weights edges based on association indices
testnet <- DynamicNetCreate(data = hevonen,
                            intwindow = intwindow,
                            mindate = mindate,
                            maxdate = maxdate,
                            netwindow = netwindow,
                            overlap = overlap,
                            spacewindow = spacewindow,
                            index=FALSE)

######## testnet is actually a list of three elements  ###########
########  [1] edge list for each of the netwindows as an array
########  [2] adj matrix for the network in each netwindow as array
########  [3] matrix indicating which net window each animal appears
#### an array is a grouping of matrices
#### access parts of an array using NAMEOFARRAY[row,column,matrix]


#Convert social networks into igraph networks
#the CMRnet:: part (before cmr_igraph) isn't really necessary
cc_nets <- cmr_igraph(testnet, type="social")

####### cmr_igraph() function converts output of DynamicNetCreate() to list of igraph objects
#### nested list with 2 levels of hierarchy
### cc_nets[[1]] = first element = list of igraph network objects (edgelists), 1 per network window
### second element = full aggregated network (adj matrix?) for all critters, entire study period

#look at the structure of the object generated
#print(cc_nets)

#plot the igraph networks (CMRnet:: isn't necessary)
CMRnet::cmrSocPlot(nets=cc_nets,
                   fixed_locs=TRUE,
                   dynamic=FALSE,
                   vertex.label=NA, 
                   vertex.size=5)

#############################################################################################


############################# calculate network statistics ##########################################
################################  for a single network   ##########################################

## this is making a list - one item per network window
## it calculating the degree for each individual in each network window and making those into a matrix
########### when applied to a list [[n]] pulls the nth element as a lone thing // while [n] pulls it as a list
#this code is saying apply the igraph function degree() to all the things in the 1st element in cc_nets
degree <- lapply(cc_nets[[1]], igraph::degree)

#output is a list of length()=#network windows
#each element in the list has the degree counts per individual in the network (I think as a vector?)
str(degree)

#this shows the 3rd element in the list/array which is a matrix(?) of degree count by individual for the 3rd networkwindow
degree[[3]]
# and you could save the individual vectors as dataframes
netwin1_deg <- as.data.frame(degree[[1]])

#######################################################################################################


################# get network stats for each network window (for a single grid) as a list #############
########################################################################################################

#this is an adj matrix [[2]] from a single grid during the 3rd netwindow [,,3]
#testnet[[2]][,,3]

# Create list to store results
t_net_mets <- list()

# Calculate network metrics to use
for(i in 1:length(cc_nets[[1]])){
  
  inet<-graph.adjacency(testnet[[2]][,,i], weighted=NULL, mode="undirected")
  ind<-testnet[[3]]$ids
  tp<-rep(i,length(testnet[[3]]$ids)) #tp is time point? counts for each iteration through all the IDs
  
  t_net_mets[[i]]<-data.frame(ind,tp)
  t_net_mets[[i]]$deg<-igraph::degree(inet)
  t_net_mets[[i]]$eig<-igraph::eigen_centrality(inet)$vector
  t_net_mets[[i]]$bet<-igraph::betweenness(inet)
  
}

# Collate results
net_mets <- do.call("rbind", t_net_mets)


####################################        END SINGLE NETWORK         #######################################
##############################################################################################################


