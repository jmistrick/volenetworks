# install CMRnet
# remotes::install_github("matthewsilk/CMRnet", build_vignettes = FALSE)

# load packages
library(here)
library(tidyverse)
# library(mosaic) #mosaic does stats stuff, useful but not necessary yet 11.29.21
library(CMRnet)
library(igraph)
library(lubridate)
library(janitor)

#load packages for plotting multiple plots together (the tidyverse parmfrow)
library(gridExtra) #for the grid.arrange() function
library(grid) #this is to add a title to the grid.arrange() complex o' graphs

#clear environment
rm(list = ls())

#load the fulltrap dataset (make sure it's the most recent version)
fulltrap <- readRDS(file = "fulltrap_06.03.22.rds")
## NOTE ## all DP, DT, or S animals are still in the fulltrap dataset

#create a fulltrap_traits df to have one entry per tag, per occasion so we can add vole data to network data
  #in occasions 1-5, we (nearly) always did all our data collection on the first capture per occasion
    #however, in occ6, some animals were recorded,weighed and released on first capture and processed later
      #this *might* oversimplify and make a few mistakes, but generally going to fill missing data based on any entry in that occasion, 
        #then slice for the first entry for an occasion - that will make the fulltrap_traits df
fulltrap_traits <- fulltrap %>% group_by(tag, occasion) %>% 
  arrange(occ.sess, .by_group=TRUE) %>% #arrange by occ.sess within the grouping variables
  fill(c(sex,mass,head,current_breeder,per,nip,preg,test), .direction="downup") %>% #fill missing data within an occasion
  slice(1) %>% #keep the first entry per grouping
  dplyr::ungroup() %>%
  dplyr::select(!c(per, nip, preg, test)) %>% #remove extra repro columns
  dplyr::select(!c(date_time, session, occ.sess, trap, x, y, fate)) %>% #remove non-trait data
  unite(sex_trt, sex, trt, sep="_", remove=FALSE, na.rm=TRUE) %>% #make a new sex_trt column
  mutate(sex_trt = ifelse(is.na(sex), NA, sex_trt)) %>% #but leave it blank if we don't know the sex
  mutate(res = ifelse(n_cap >= 5, "resident", "nonresident"))


##################################################################################################
######################################### CMRnet analysis ########################################
##################################################################################################

#############################################################################################
######################################### set up the data ###################################
#############################################################################################

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
intwindow<-2
#network window (months)
netwindow<-1
overlap<-0
#distance to consider contact (0=same trap)
spacewindow<-1.5

for(i in 1:length(cmr_list)){
  
  print(i)
  
  tempdf <- cmr_list[[i]] #temp storage for each site in loop
  tempdf <- dplyr::select(tempdf, -site) #remove site because CMRnet doesn't want the extra column
  
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
## first is an edge list, second is adjacency matrix, third is a df of which netwindow each individual is in

## nets_list[[1]][[1]] is array of edge lists for all network windows at site 1 (asema)
## nets_list[[1]][[1]][,,1] is just the edge list for asema during network window 1 (May)

## ALL OF THESE ARE WEIGHTED - if two voles overlapped 4 times in June, they will have an edge count of 4

#######################################################################################################
###########################################   END   ###################################################
#######################################################################################################



###########################################################################################################
############## calculate network metrics for a list of networks (1 network/site)  #########################
########################  FOR ALL 12 SITES   ###############  IN A LOOP  ##################################
##################################  BINARY NETWORK - NO EDGE WEIGHTS   ####################################
###########################################################################################################

# Create list to store results
binary_net_mets_list <- list()

# Calculate network metrics - using the nets_list list (from dynamicnetcreate() function)
for(i in 1:length(nets_list)){
  
  #length(nets_list) == for each of the 12 sites
  
  print(i)
  site <- list()
  
  for(j in 1:6){
    
    #(j in 1:6) is for the 6 trapping occasions, this is an easy enough thing to write in...
        #and there is no easy way to pull the number of occasions (6) from nets_list
    #it used to be: length(igraph_list[[1]][[1]]) == for each of the 6 trapping occasions - but that's really complicated
    
    ongrid <- nets_list[[i]][[3]][,c(1,(j+1))] #this pulls tag ids & the correct column of the netwindows df
    ongrid <- ongrid %>% filter(.[,2] == 1) #filter for only animals on the grid
    ids <- as.vector(ongrid$ids)
    
    adjmat <- nets_list[[i]][[2]][,,j] #pull the adjmatrix for the given occasion (includes all animals)
    
    adjmat <- adjmat[ids,ids] #subset the adjmatrix for only the animals on the grid that month
    
    inet <- graph.adjacency(adjmat >= 1, mode="undirected", weighted=NULL) 
    #this code will make a binary network where there is only one edge between 2 voles, even if they overlapped multiple times in 48hr
    
      #make a network from the subset adj matrix for a given occasion (j)
    tag <- ids #pull the tag numbers for all the animals on that grid
    month <- rep(j,length(ids)) #this puts j==occasion # in a column for all animals
    
    #network metrics to calculate
    site[[j]] <- data.frame(tag,month)
    site[[j]]$deg <- igraph::degree(inet) #this is binary degree (number of individuals overlapped with)
    # site[[j]]$eig <- igraph::eigen_centrality(inet)$vector
      #eigenvector centrality takes into account second-order connections (ie, friends of friends)
    site[[j]]$norm.betw <- igraph::betweenness(inet, normalized = TRUE) #normalized to compare nets of different sizes
      #betweeness is how often you're on the shortest path between others
    # site[[j]]$close <- igraph::closeness(inet) #not good for disconnected graphs
      #closeness measures normalized path length from you to all others in network
    site[[j]]$node.clust <- igraph::transitivity(inet, type="local") #type="local" for each node ="global" for whole network
      #node-level clustering sends a warning for networks with multiple components
    site[[j]]$net.clust <- rep(igraph::transitivity(inet, type="global"), length(ids))
      #transitivity (clustering coef) is ratio of closed triplets to possible triplets
        #tightly connected communities have high transitivity
    site[[j]]$net.dens <- rep(igraph::edge_density(inet, loops=FALSE), length(ids)) #network density
    # site[[j]]$avgdeg <- mean(site[[j]]$deg) #calculate average degree for a site/occasion
    site[[j]]$n.component <- rep(igraph::count_components(inet), length(ids))
    site[[j]]$n.node <- rep(igraph::gorder(inet), length(ids))
    site[[j]]$n.edge <- rep(igraph::gsize(inet), length(ids)) #### WEIGHTED EDGES ? ####
    
    #calculate modularity
    if( all(adjmat == 0) ) { site[[j]]$n.clust <- rep(NaN, length(ids)) } 
    else {
      eb <- edge.betweenness.community(inet)
      site[[j]]$n.clust <- rep(length(eb), length(ids)) #number of clusters (not necessarily number of components)
    }
    
    if( all(adjmat == 0) ) { site[[j]]$mod <- rep(NaN, length(ids)) } 
    else {
      site[[j]]$mod <- rep(modularity(eb), length(ids)) #values -1 to 1, 0 means heterogeneous connections, positive values indicate modular structure
    }

    
  }
  #write the list 'site' as a 1st-order item in net_mets_list
  binary_net_mets_list[[i]] <- site
  
  #remove any site/occasion list that has no captures for that month
  temp <- binary_net_mets_list[[i]][sapply(binary_net_mets_list[[i]], nrow)>0]
  binary_net_mets_list[[i]] <- temp
  
}

######### a note on calculating Network Density ###################
# calculated per network = ratio of realized edges:possible edges
# edge_density(igraph network file, loops=FALSE) 
## a loop is an edge connecting a node back to itself, FALSE if these are meaningless in your network


#name the 12 1st order elements of nets_list as the sites
names(binary_net_mets_list) <- names(cmr_list)

#rename the sublist items (months) for each site
#accounting for the fact that some sites have 5 months of data and others have 6
for(i in 1:length(binary_net_mets_list)){
  ifelse( length(binary_net_mets_list[[i]]) == 6, names(binary_net_mets_list[[i]]) <- c("may", "june", "july", "aug", "sept", "oct"), 
          names(binary_net_mets_list[[i]]) <- c("june", "july", "aug", "sept", "oct") )
}

################################### ABOUT binary_net_mets_list ################################################
#the output of binary_net_mets_list is a list of 12 1st-order items, 1 per site
  #under each site, there are 5-6 second-order items, 1 per trapping occasion (named by month)
    #each of those 2nd-order items (months) is a df with tag ID, occasion #, and all the network metrics...
      #for ONLY THE ANIMALS captured on that grid, during that occasion
      # (column that says whether the animal was on the grid has been removed)
########################################################################################################

############## condense binary_net_mets_list to make it easier to use for analysis #######################

#collate results
#this collapses the 2nd order elements (network metrics for a single month) down to the 1st order element (site)
#so now binary_net_mets_list_summary is a list of 12 dfs, each is all the network metrics for a site across all the networkwindows

#make a list to store things
binary_net_mets_list_summary <- list()

#loop across all sites and collapse the dfs per occasion into one df for the site
for(i in 1:length(binary_net_mets_list)){
  
  #for all 12 sites
  summary <- do.call("rbind", binary_net_mets_list[[i]])
  binary_net_mets_list_summary[[i]] <- summary
}

#remove the row names of the dfs
for(i in 1:length(binary_net_mets_list_summary)){
  row.names(binary_net_mets_list_summary[[i]]) <- NULL
}

#name the 12 1st order elements as their sites
names(binary_net_mets_list_summary) <- names(cmr_list)

#this code has been taken care of in the loop creating net_mets_list
# #filter each list item (site df) to only have animals on the grid (ongrid==1) in the given month
# net_mets_list_summary <- lapply(net_mets_list_summary, function(x) filter(x, ongrid == 1))
# #then remove the 'ongrid' column
# net_mets_list_summary <- lapply(net_mets_list_summary, function(x) select(x, -ongrid))

#change the numeric occasions to actual month words
for(i in 1:length(binary_net_mets_list_summary)){
  binary_net_mets_list_summary[[i]]$month <- as.factor(binary_net_mets_list_summary[[i]]$month)
  binary_net_mets_list_summary[[i]]$month <-  binary_net_mets_list_summary[[i]]$month %>%
    recode_factor("1" = "may", "2" = "june", "3" = "july", "4" = "aug", "5" = "sept", "6" = "oct")
}


### HOT DAMN - binary_net_mets_list_summary is now a list of 12 elements where each element represents a site...
  # and contains a df of all the individuals on the grid that month, their tag number, 
    #and all their network metrics

## make binary_net_mets_list_summary into freiggein huge df
binary_net_mets_summary <- do.call(rbind.data.frame, binary_net_mets_list_summary)

#clean up the df
binary_net_mets_summary <- binary_net_mets_summary %>% 
  rownames_to_column("name") %>% #row names are the sites, make that a column
  separate(name, c("site", NA)) %>% #separate the site part from the index and get rid of the index
  mutate(site = as.factor(site)) #make site a factor
  
##########################################################################################################
########################### END BINARY NETWORK METRICS CALCULATIONS ######################################
##########################################################################################################



###########################################################################################################
############## calculate network metrics for a list of networks (1 network/site)  #########################
########################  FOR ALL 12 SITES   ###############  IN A LOOP  ##################################
##################################  WEIGHTED NETWORK - WITH EDGE WEIGHTS   ################################
###########################################################################################################

########## NOW THINGS ARE GOING TO CHANGE (3.1.22) -- (intentionally) make WEIGHTED networks instead of binary
# inet <- graph.adjacency(adjmat, weighted=TRUE, mode="undirected") 
# site[[j]]$strength <- igraph::strength(inet) #this is the sum of all degree weights for a node
# trying to plot networks with edge weights

# Create list to store results
wt_net_mets_list <- list()

# Calculate network metrics - using the wt_nets_list list (from dynamicnetcreate() function)
for(i in 1:length(nets_list)){
  
  #length(nets_list) == for each of the 12 sites
  
  print(i)
  site <- list()
  
  for(j in 1:6){
    
    #(j in 1:6) is for the 6 trapping occasions, this is an easy enough thing to write in...
    #and there is no easy way to pull the number of occasions (6) from nets_list
    #it used to be: length(igraph_list[[1]][[1]]) == for each of the 6 trapping occasions - but that's really complicated
    
    ongrid <- nets_list[[i]][[3]][,c(1,(j+1))] #this pulls tag ids & the correct column of the netwindows df
    ongrid <- ongrid %>% filter(.[,2] == 1) #filter for only animals on the grid
    ids <- as.vector(ongrid$ids)
    
    adjmat <- nets_list[[i]][[2]][,,j] #pull the adjmatrix for the given occasion (includes all animals)
    
    adjmat <- adjmat[ids,ids] #subset the adjmatrix for only the animals on the grid that month
    
    inet <- graph.adjacency(adjmat, weighted=TRUE, mode="undirected") 
    #this code will make WEIGHTED networks, higher weights meaning more overlaps occurred in the 48hrs
    
    #make a network from the subset adj matrix for a given occasion (j)
    tag <- ids #pull the tag numbers for all the animals on that grid
    month <- rep(j,length(ids)) #this puts j==occasion # in a column for all animals
    
    #network metrics to calculate
    site[[j]] <- data.frame(tag,month)
    # site[[j]]$deg <- igraph::degree(inet) #this is binary degree count
    site[[j]]$wt.deg <- igraph::strength(inet) #this is the sum of all degree weights for a node
    # site[[j]]$eig <- igraph::eigen_centrality(inet)$vector
      #eigenvector centrality takes into account second-order connections (ie, friends of friends)
    site[[j]]$wt.norm.betw <- igraph::betweenness(inet, normalized=TRUE) ## normalized between 0 and 1 to compare bw nets
      #weighted by default in weighted graphs, weights perceived as lengths
      #betweeness is how often you're on the shortest path between others
    # site[[j]]$close <- igraph::closeness(inet) #not good for disconnected graphs
      #closeness measures normalized path length from you to all others in network
    site[[j]]$wt.node.clust <- igraph::transitivity(inet, type="weighted") #type="local" for each node ="global" for whole network
      #node-level clustering sends a warning for networks with multiple components
      #if graph is weighted, weights used by default -- only for local transitivity (not global)
    # site[[j]]$clust.net <- rep(igraph::transitivity(inet, type="global", isolates=NaN), length(ids))
      #no weighted option for global (only local)
      #transitivity (clustering coef) is ratio of closed triplets to possible triplets
        #tightly connected communities have high transitivity
    # site[[j]]$netdens <- rep(igraph::edge_density(inet, loops=FALSE), length(ids)) #network density
      #network density is never weighted since it's edge counts, and weighting it doesn't make sense
    # site[[j]]$avgdeg <- mean(site[[j]]$deg) #calculate average degree for a site/occasion
    # site[[j]]$components <- rep(igraph::count_components(inet), length(ids))
    # site[[j]]$nodect <- rep(igraph::gorder(inet), length(ids))
    site[[j]]$wt.n.edge <- rep(sum(E(inet)$weight), length(ids)) ##### THIS IS WEIGHTED #####
    
    #calculate modularity
    #################### WARNINGS: this might not make sense with weighted networks!! ##########################
    # if( all(adjmat == 0) ) { site[[j]]$n.clust <- rep(NaN, length(ids)) } 
    # else {
    #   eb <- edge.betweenness.community(inet)
    #   site[[j]]$n.clust <- rep(length(eb), length(ids)) #number of clusters (not necessarily number of components)
    # }
    # 
    # if( all(adjmat == 0) ) { site[[j]]$mod <- rep(NaN, length(ids)) } 
    # else {
    #   site[[j]]$mod <- rep(modularity(eb), length(ids)) #values -1 to 1, 0 means heterogeneous connections, positive values indicate modular structure
    # }
    
    
  }
  #write the list 'site' as a 1st-order item in wt_net_mets_list
  wt_net_mets_list[[i]] <- site
  
  #remove any site/occasion list that has no captures for that month
  temp <- wt_net_mets_list[[i]][sapply(wt_net_mets_list[[i]], nrow)>0]
  wt_net_mets_list[[i]] <- temp
  
}

#name the 12 1st order elements of wt_nets_list as the sites
names(wt_net_mets_list) <- names(cmr_list)

#rename the sublist items (months) for each site
#accounting for the fact that some sites have 5 months of data and others have 6
for(i in 1:length(wt_net_mets_list)){
  ifelse( length(wt_net_mets_list[[i]]) == 6, names(wt_net_mets_list[[i]]) <- c("may", "june", "july", "aug", "sept", "oct"), 
          names(wt_net_mets_list[[i]]) <- c("june", "july", "aug", "sept", "oct") )
}

################################### ABOUT wt_net_mets_list ################################################
#the output of wt_net_mets_list is a list of 12 1st-order items, 1 per site
#under each site, there are 5-6 second-order items, 1 per trapping occasion (named by month)
#each of those 2nd-order items (months) is a df with tag ID, occasion #, and all the network metrics...
#for ONLY THE ANIMALS captured on that grid, during that occasion
# (column that says whether the animal was on the grid has been removed)
########################################################################################################

############## condense wt_net_mets_list to make it easier to use for analysis #######################

#collate results
#this collapses the 2nd order elements (network metrics for a single month) down to the 1st order element (site)
#so now wt_net_mets_list_summary is a list of 12 dfs, each is all the network metrics for a site across all the networkwindows

#make a list to store things
wt_net_mets_list_summary <- list()

#loop across all sites and collapse the dfs per occasion into one df for the site
for(i in 1:length(wt_net_mets_list)){
  
  #for all 12 sites
  summary <- do.call("rbind", wt_net_mets_list[[i]])
  wt_net_mets_list_summary[[i]] <- summary
}

#remove the row names of the dfs
for(i in 1:length(wt_net_mets_list_summary)){
  row.names(wt_net_mets_list_summary[[i]]) <- NULL
}

#name the 12 1st order elements as their sites
names(wt_net_mets_list_summary) <- names(cmr_list)

#this code has been taken care of in the loop creating net_mets_list
# #filter each list item (site df) to only have animals on the grid (ongrid==1) in the given month
# net_mets_list_summary <- lapply(net_mets_list_summary, function(x) filter(x, ongrid == 1))
# #then remove the 'ongrid' column
# net_mets_list_summary <- lapply(net_mets_list_summary, function(x) select(x, -ongrid))

#change the numeric occasions to actual month words
for(i in 1:length(wt_net_mets_list_summary)){
  wt_net_mets_list_summary[[i]]$month <- as.factor(wt_net_mets_list_summary[[i]]$month)
  wt_net_mets_list_summary[[i]]$month <-  wt_net_mets_list_summary[[i]]$month %>%
    recode_factor("1" = "may", "2" = "june", "3" = "july", "4" = "aug", "5" = "sept", "6" = "oct")
}

### HOT DAMN - wt_net_mets_list_summary is now a list of 12 elements where each element represents a site...
# and contains a df of all the individuals on the grid that month, their tag number, 
#and all their network metrics

## make net_mets_list_summary into freiggein huge df
wt_net_mets_summary <- do.call(rbind.data.frame, wt_net_mets_list_summary)

#clean up the df
wt_net_mets_summary <- wt_net_mets_summary %>% 
  rownames_to_column("name") %>% #row names are the sites, make that a column
  separate(name, c("site", NA)) %>% #separate the site part from the index and get rid of the index
  mutate(site = as.factor(site)) #make site a factor


##########################################################################################################
########################### END WEIGHTED NETWORK METRICS CALCULATIONS ####################################
##########################################################################################################


############### network level centralization scores ##################
################## to go in with net_mets_summary ####################

centralization.list <- list()

#calculate centralization score for each network ###### THIS IS ? NOT ? A WEIGHTED MEASURE ######
for(i in 1:length(nets_list)){
  
  #12 sites
  print(i)
  tmp <- list()
  
  for(j in 1:6){
    
    ongrid <- nets_list[[i]][[3]][,c(1,(j+1))] #this pulls tag ids & the correct column of the netwindows df
    ongrid <- ongrid %>% filter(.[,2] == 1) #filter for only animals on the grid
    ids <- as.vector(ongrid$ids)
    
    adjmat <- nets_list[[i]][[2]][,,j] #pull the adjmatrix for the given occasion (includes all animals)
    
    adjmat <- adjmat[ids,ids] #subset the adjmatrix for only the animals on the grid that month
    
    inet <- graph.adjacency(adjmat >= 1, mode="undirected", weighted=NULL) 
    #this code will make a binary network where there is only one edge between 2 voles, even if they overlapped multiple times in 48hr
    
    tmp[[j]] <- igraph::centr_betw(inet, directed=FALSE, normalized=TRUE)
      ##tmp is a list with three elements:
        ##the centrality score for each node, the graph centrality, and the theoretical max centrality
    
    tmp[[j]] <- as.data.frame(do.call(rbind, tmp[[j]][-c(1,3)]))
    
    
    df <- tmp[[j]][-c(1,3)]
      #remove [1] the 'res' item - this is the individual node-level centrality measures
      #remove [3] the 'theoretical max' item

    #make it a df not a list of 1 element
    df <- as.data.frame(do.call(rbind, df))

    site[[j]] <- df
    
  }
  #write the list 'site' as a 1st-order item in centralization.list
  centralization.list[[i]] <- as.data.frame(do.call(rbind, tmp))
  rownames(centralization.list[[i]]) <- c("may", "june", "july", "aug", "sept", "oct")
}


#name the 12 1st order elements of nets_list as their sites
names(centralization.list) <- names(cmr_list)

#rename the sublist items for each grid
for(i in 1:length(centralization.list)){
  names(centralization.list[[i]]) <- "net.centralization"
}

#this creates a list with 12 items, each is a df for a single site
#take that list of dfs and make it one big df
centralization_summary <- do.call(rbind.data.frame, centralization.list) %>%
  rownames_to_column("name") %>% #move the row labels to be their own column
  separate(name, c("site", "month"))

############################# end network centralization ###########################

#combine the weighted and unweighted network metrics
net_mets_summary <- left_join(binary_net_mets_summary, wt_net_mets_summary, 
                              by=c('site' = 'site', 'tag'='tag', 'month'='month')) %>%
  relocate(wt.deg, .after=deg) %>%
  relocate(wt.norm.betw, .after=norm.betw) %>%
  relocate(wt.n.edge, .after=n.edge) %>%
  relocate(wt.node.clust, .after=node.clust)

#add in the fulltrap_traits df
net_mets_summary <- left_join(net_mets_summary, fulltrap_traits, by=c('site' = 'site', 'tag'='tag', 'month'='month'))

#add centralization score to net_mets_summary
net_mets_summary <- left_join(net_mets_summary, centralization_summary, by=c('site' = 'site', 'month'='month'))

#and reorganize a bit
net_mets_summary <- net_mets_summary %>%
  relocate(month, .after=site) %>%
  relocate(occasion, .after=month) %>%
  relocate(c(trt, food_trt, helm_trt), .after=site) %>%
  relocate(c(n.node, n.edge, wt.n.edge, net.dens, net.centralization, net.clust, mod, n.clust), .after=wt.node.clust) %>%
  relocate(c(n_cap, recapped), .after=n.component) %>%
  mutate(month = factor(month, levels=c("may", "june", "july", "aug", "sept", "oct"))) #month to factor


##########################################################################################################
############################### net_mets_summary is now complete and useful ##############################
##########################################################################################################



########################################################################
################ if you so desire in the future ########################
##### reinsert the '08_trapping_networks_permutations' code here #######
########################################################################





################### Visualize from net_mets_summary ######################

#degree by site, month
net_mets_summary %>% ggplot(aes(x=month, y=deg, fill=site)) + 
  geom_boxplot()
#degree by treatment, month
net_mets_summary %>% ggplot(aes(x=month, y=deg, fill=trt)) + 
  geom_boxplot() +
  labs(x="month", 
       y="individual vole degree (edge count)", 
       title="individual vole degree by treatment")
#degree by food treatment, month
# net_mets_summary %>% ggplot(aes(x=month, y=deg, fill=food_trt)) + 
#   geom_boxplot() + 
#   labs(x="month", y="individual vole degree (edge count)", 
#        title="effect of food addition on individual vole degree")

#weighted degree by site, month
net_mets_summary %>% ggplot(aes(x=month, y=wt.deg, fill=site)) + 
  geom_boxplot()
#weighted degree by treatment, month
net_mets_summary %>% ggplot(aes(x=month, y=wt.deg, fill=trt)) + 
  geom_boxplot() +
  labs(x="month", 
       y="weighted vole degree", 
       title="weighted vole degree by treatment")
#weighted degree by sex, treatment, month
net_mets_summary %>%
  drop_na(sex) %>%
  ggplot(aes(x=trt, y=wt.deg, fill=sex)) + 
  geom_boxplot() + 
  facet_wrap(~month) +
  labs(title="weighted degree by sex, treatment")
#weighted degree by breeding, treatment, month
net_mets_summary %>%
  drop_na(current_breeder) %>%
  ggplot(aes(x=trt, y=wt.deg, fill=current_breeder)) + 
  geom_boxplot() + 
  facet_wrap(~month) +
  labs(title="weighted degree by breeding status, treatment")

#compare degree vs weighted degree
grid.arrange(a, b, ncol=2)
#eh, similar ish?

#network density by site, month
# net_mets_summary %>% ggplot(aes(x=month, y=netdens, fill=site)) + geom_point(aes(color=site))
net_mets_summary %>% ggplot(aes(x=month, y=net.dens, fill=trt)) + 
  geom_boxplot() + 
  labs(x="month", y="network density", title="network density by treatment type")
#network density by food treatment, month
net_mets_summary %>% ggplot(aes(x=month, y=netdens, fill=food_trt)) + geom_boxplot()

# net_mets_summary %>% ggplot(aes(x=month, y=n.node, fill=site)) + geom_point(aes(color=site))
#network size (n.nodes) by treatment, month
plot1 <- net_mets_summary %>% ggplot(aes(x=month, y=n.node, fill=trt)) + 
  geom_boxplot() + 
  scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#CAB2D6", "#6A3D9A"),
                    name = "Treatment",
                    labels = c("Unfed - Control", "Unfed - Deworm", "Fed - Control", "Fed - Deworm")) +
  labs(x="Month", y="Population Size", fill="Treatment") +
  scale_x_discrete(labels=c("may" = "May",
                            "june" = "June",
                            "july" = "July",
                            "aug" = "Aug",
                            "sept" = "Sept",
                            "oct" = "Oct")) +
  theme(legend.position = c(0.178, 0.82),
        legend.title = element_text(size=15),
        legend.text = element_text(size=11),
        legend.title.align=0.5,
        axis.text = element_text(size=12),
        axis.title = element_text(size=15))

ggsave("netsize_by_month.png", plot=plot1, height=6, width=6.5, units=c("in"), dpi=600)

### scale_fill_brewer(palette="Paired") #Set2 and Paired are both colorblind-friendly

library(colorBlindness)
cvdPlot(plot1)

library(RColorBrewer)  
brewer.pal(12, "Paired") #https://gist.github.com/jtoll/4021792
display.brewer.pal(12, "Paired")


# net_mets_summary %>% ggplot(aes(x=month, y=n.node, fill=food_trt)) + 
#   geom_boxplot() + 
#   labs(x="month", 
#        y="network size", 
#        title="effect of food addition on network size")
# net_mets_summary %>% ggplot(aes(x=month, y=n.node, fill=helm_trt)) + 
#   geom_boxplot() + 
#   labs(x="month", 
#        y="network size", 
#        title="effect of helminth removal on network size")

# net_mets_summary %>% ggplot(aes(x=month, y=clust.net, fill=site)) + geom_point(aes(color=site))
net_mets_summary %>% ggplot(aes(x=month, y=net.clust, fill=trt)) + 
  geom_boxplot() + 
  labs(x="month", y="clustering coefficient", title="network-level clustering by treatment type") 
#overall, network level clustering is high
# net_mets_summary %>% ggplot(aes(x=month, y=net.clust, fill=food_trt)) + geom_boxplot()  + 
#   labs(x="month", 
#        y="clustering coefficient", 
#        title="effect of food addition on network-level clustering")
# net_mets_summary %>% ggplot(aes(x=month, y=net.clust, fill=helm_trt)) + geom_boxplot()  + 
#   labs(x="month", 
#        y="clustering coefficient", 
#        title="effect of helminth removal on network-level clustering")


net_mets_summary %>% ggplot(aes(x=month, y=norm.betw, fill=trt)) + geom_boxplot()
net_mets_summary %>% ggplot(aes(x=month, y=wt.norm.betw, fill=trt)) + 
  geom_boxplot() +
  labs(title="weighted normalized bewteenness by treatment")
  #there are animals on all grids with v high betweenness -- that might be interesting?
  #important conduits for transmission?

net_mets_summary %>% ggplot(aes(x=month, y=node.clust, fill=trt)) + geom_boxplot()
net_mets_summary %>% ggplot(aes(x=month, y=wt.node.clust , fill=trt)) + geom_boxplot()
  #node level clustering is generally high --> lots of triangles

# net_mets_summary %>% ggplot(aes(x=month, y=n.component, fill=trt)) + geom_boxplot()
# net_mets_summary %>% ggplot(aes(x=month, y=n.clust, fill=trt)) + geom_boxplot()

net_mets_summary %>% ggplot(aes(x=month, y=mod, fill=trt)) + geom_boxplot()
  #modularity increases as season goes on, generally high Aug-Oct
  #network is arranged into clusters

net_mets_summary %>% ggplot(aes(x=month, y=n.edge, fill=trt)) + geom_boxplot()
  #number of edges generally follows population trends - suggesting density-dep overlaps?
net_mets_summary %>% ggplot(aes(x=month, y=wt.n.edge, fill=trt)) + geom_boxplot()
  #weighted number of edges a little less so

net_mets_summary %>% ggplot(aes(x=month, y=net.centralization, fill=trt)) + geom_boxplot()
  #betweenness centrality at the graph level... don't really know what that means
  #0 is a line and 1 is a star? so the nets are more liney than star-like?






#### NEXT TO DO : 'net_mets_summary' is at a point where it could be the data fed into some sort of lmer model
## BUT I don't know how appropriate that actually is
# library(lme4)
# 
# test <- lmer(degree ~ site + occ + food_trt + helm_trt + (1|tag), data = net_mets_summary)
# summary(m_lymph)


#something to try comparing groups (I know this is probably not okay because networks)
# library(ggpubr)
# 
# net_mets_summary %>%
#   ggplot(aes(x=month, y=norm.betw, fill=trt)) +
#   geom_boxplot() +
#   stat_compare_means()
############### this is pretty useful, might be good to see if this is allowed...

























###########################################################################
################    Visualizing Degree Distributions    ###################
###########################################################################


###########################################################################
### 09_degree_freq_calculations was here - bring it back if you need it ###
###########################################################################


###########################################################################
############ violin / boxplots per site, color by trt, facet by month ######

#degree distribution per site (group by trt) facet by month
##### can't get the boxplots to be thin and on top of their violin
# net_mets_summary %>% 
#   filter(month=="sept") %>%
#   ggplot(aes(x=trt, y=deg, group=site, color=trt)) +
#   geom_violin() +
#   geom_boxplot()

#degree distribution per site (group by trt) facet by month
# net_mets_summary %>%
#   ggplot(aes(x=trt, y=deg, group=site, fill=trt)) + 
#   geom_violin() + 
#   facet_wrap(~month)

#degree distribution per site, month
net_mets_summary %>%
  ggplot(aes(x=trt, y=deg, group=site, fill=trt)) + 
  geom_boxplot() + 
  facet_wrap(~month, 
             labeller = month.labs) +
  scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#CAB2D6", "#6A3D9A"),
                    name = "Treatment",
                    labels = c("Unfed - Control", "Unfed - Deworm", "Fed - Control", "Fed - Deworm")) +
  labs(x="Treatment", y="Spatial Overlap Degree", fill="Treatment") +
  scale_x_discrete(labels=c("control_control" = "Unfed/Control",
                            "control_deworm" = "Unfed/Deworm",
                            "supplement_control" = "Fed/Control",
                            "supplement_deworm" = "Fed/Deworm")) +
  theme(legend.position = "none",
        legend.title = element_text(size=15),
        legend.text = element_text(size=11),
        legend.title.align=0.5,
        axis.text = element_text(size=7),
        axis.title = element_text(size=15))

#name the above 'plot_degsummary <- ' to save
# ggsave("degdist_by_sitemonth.png", plot=plot_degsummary, height=6, width=9.5, units=c("in"), dpi=600)


###########################################################################
################## treatment by month with mean, median ###################

#degree distribution per trt, month- not really helpful
# degree_summary %>% mutate(month = fct_relevel(month, "may", "june", "july", "aug", "sept", "oct")) %>%
#   ggplot(aes(x=trt, y=degree, fill=trt)) +
#   geom_boxplot() +
#   facet_wrap(~month)

#degree distribution per site (group by trt) facet by month
month.labs <- as_labeller(c("may" = "May", "june" = "June", "july" = "July", "aug" = "Aug", "sept" = "Sept", "oct" = "Oct"))

#violin plot by month/trt with box plot and mean overlay
# net_mets_summary %>% 
#   ggplot(aes(x=trt, y=deg, group=trt, fill=trt)) + 
#   geom_violin() + 
#   scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#CAB2D6", "#6A3D9A"),
#                     name = "Treatment",
#                     labels = c("Unfed - Control", "Unfed - Deworm", "Fed - Control", "Fed - Deworm")) +
#   geom_boxplot(width = 0.15, color="black") +
#   facet_wrap(~month,
#              labeller=month.labs) +
#   labs(x="Treatment", y="Spatial Overlap Degree", fill="Treatment") +
#   scale_x_discrete(labels=c("control_control" = "Unfed/Control",
#                             "control_deworm" = "Unfed/Deworm",
#                             "supplement_control" = "Fed/Control",
#                             "supplement_deworm" = "Fed/Deworm")) +
#   theme(legend.position = "none",
#         legend.title = element_text(size=15),
#         legend.text = element_text(size=11),
#         legend.title.align=0.5,
#         axis.text = element_text(size=7),
#         axis.title = element_text(size=15)) + 
#   stat_summary(fun.data = "mean_cl_boot", 
#                geom = "pointrange", 
#                colour = "red") #https://r-charts.com/distribution/violin-plot-mean-ggplot2/

#violin plot by month/trt with box plot and mean overlay
net_mets_summary %>%
  ggplot(aes(x=trt, y=deg, group=trt, fill=trt)) + 
  geom_violin() + 
  scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#CAB2D6", "#6A3D9A"),
                    name = "Treatment",
                    labels = c("Unfed - Control", "Unfed - Deworm", "Fed - Control", "Fed - Deworm")) +
  geom_boxplot(width = 0.15, color="black") +
  facet_wrap(~month,
             labeller=month.labs) +
  labs(x="Treatment", y="Spatial Overlap Degree", fill="Treatment") +
  scale_x_discrete(labels=c("control_control" = "Unfed/Control",
                            "control_deworm" = "Unfed/Deworm",
                            "supplement_control" = "Fed/Control",
                            "supplement_deworm" = "Fed/Deworm")) +
  theme(legend.position = "none",
        legend.title = element_text(size=15),
        legend.text = element_text(size=11),
        legend.title.align=0.5,
        axis.text = element_text(size=7),
        axis.title = element_text(size=15)) +
  stat_summary(fun=mean, geom="point", size=2, color="red")
#mean is the red dot, median is the line of the boxplot

#to save plot above, name as 'plot2 <- '
# plot2
# library(colorBlindness)
# cvdPlot(plot2)
# 
# ggsave("degdist_by_month.png", plot=plot2, height=6, width=9.5, units=c("in"), dpi=600)

###################################################################################
########## ridgeline plots - by site, color by treat, face by month ###############

#5.31.22 for EEID poster: ridgeline plot of degree dist - binary degree
library(ggridges)
library(hrbrthemes)

month.labs <- as_labeller(c("may" = "May", "june" = "June", "july" = "July", "aug" = "Aug", "sept" = "Sept", "oct" = "Oct"))

net_mets_summary %>%
  mutate(site = fct_relevel(site, "janakkala", "puro", "talo",
                            "kuoppa", "radio", "vaarinkorpi",
                            "ketunpesa", "kiirastuli", "mustikka",
                            "asema", "helmipollo", "hevonen")) %>%
  ggplot( aes(y=site, x=deg,  fill=trt)) +
  # stat_density_ridges(quantile_lines=TRUE,
  #                     quantile_fun=function(x,...)median(x),
  #                     alpha=0.6) +
  geom_density_ridges(alpha=0.75, stat="binline", bins=15) +
  #geom_density_ridges(alpha=0.6, bandwidth=1) +
  facet_wrap(~month, labeller=month.labs) +
  xlim(-1, 14) +
  scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#CAB2D6", "#6A3D9A"),
                    name = "Treatment",
                    labels = c("Unfed - Control", "Unfed - Deworm", "Fed - Control", "Fed - Deworm")) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        legend.position="none",
        legend.text=element_text(size=9),
        axis.title=element_text(size=15),
        panel.spacing = unit(0.2, "lines"),
        strip.text.x = element_text(size = 10)) +
  labs(x="Number of Unique Spatial Overlaps", y="Replicate Population", fill="Treatment")

#to save, name the above as 'plot_degdistsummary <- '
# ggsave("degdisthist_by_sitemonth.png", plot=plot_degdistsummary, height=6, width=9.5, units=c("in"), dpi=600)


#ridgeline plot of weighted degree ########## NEED TO ADJUST AXES ###############
net_mets_summary %>%
  mutate(site = fct_relevel(site, "janakkala", "puro", "talo",
                            "kuoppa", "radio", "vaarinkorpi",
                            "ketunpesa", "kiirastuli", "mustikka",
                            "asema", "helmipollo", "hevonen")) %>%
  ggplot( aes(y=site, x=wt.deg,  fill=trt)) +
  # stat_density_ridges(quantile_lines=TRUE,
  #                     quantile_fun=function(x,...)median(x),
  #                     alpha=0.6) +
  geom_density_ridges(alpha=0.75, stat="binline", bins=15) +
  #geom_density_ridges(alpha=0.6, bandwidth=1) +
  facet_wrap(~month, labeller=month.labs) +
  xlim(-1, 40) + #x axis should go to 60, but you can't see anything that way
  scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#CAB2D6", "#6A3D9A"),
                    name = "Treatment",
                    labels = c("Unfed - Control", "Unfed - Deworm", "Fed - Control", "Fed - Deworm")) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        legend.position="none",
        legend.text=element_text(size=9),
        axis.title=element_text(size=15),
        panel.spacing = unit(0.2, "lines"),
        strip.text.x = element_text(size = 10)) +
  labs(x="Number of Unique Spatial Overlaps", y="Replicate Population", fill="Treatment")



## ridgeline plots but with other measures
#by sex
net_mets_summary %>%
  drop_na(sex) %>%
  ggplot( aes(y=trt, x=wt.deg,  fill=sex)) +
  # stat_density_ridges(quantile_lines=TRUE,
  #                     quantile_fun=function(x,...)median(x),
  #                     alpha=0.6) +
  geom_density_ridges(alpha=0.5, stat="binline", bins=15, scale = 0.8) +
  #geom_density_ridges(alpha=0.6, bandwidth=1) +
  facet_wrap(~month, labeller=month.labs) +
  xlim(-1, 14) +
  theme(axis.text.y=element_text(), axis.ticks.y=element_blank(), 
        legend.position="bottom",
        legend.text=element_text(size=9),
        axis.title=element_text(size=15),
        panel.spacing = unit(0.2, "lines"),
        strip.text.x = element_text(size = 10)) +
  labs(x="Weighted Spatial Overlaps", y="Treatment", fill="Sex")

#resident, non-resident
net_mets_summary %>%
  ggplot( aes(y=trt, x=wt.deg,  fill=res)) +
  # stat_density_ridges(quantile_lines=TRUE,
  #                     quantile_fun=function(x,...)median(x),
  #                     alpha=0.6) +
  geom_density_ridges(alpha=0.5, stat="binline", bins=15, scale = 0.8) +
  #geom_density_ridges(alpha=0.6, bandwidth=1) +
  facet_wrap(~month, labeller=month.labs) +
  xlim(-1, 14) +
  theme(axis.text.y=element_text(), axis.ticks.y=element_blank(), 
        legend.position="bottom",
        legend.text=element_text(size=9),
        axis.title=element_text(size=15),
        panel.spacing = unit(0.2, "lines"),
        strip.text.x = element_text(size = 10)) +
  labs(x="Weighted Spatial Overlaps", y="Treatment", fill="Sex")



################################### CODE CLEANED TO HERE #######################################








#############  detecting degree outliers ?   May 4 2022  ############################

# https://statsandr.com/blog/outliers-detection-in-r/
# degree_summary <- degree_summary %>% group_by(site, month)
# boxplot.stats(degree_summary$degree)$out
# out <- boxplot.stats(degree_summary$degree)$out
# out_ind <- which(degree_summary$degree %in% c(out))
# out_ind
# degreeoutliers <- degree_summary[out_ind, ]
# degreeoutliers
############# this isn't quite working as expected - doesn't pull out outliers less than 9...

####detecting degree outliers
#create a new column for grouping: site_month, trt_month
degree_summary <- degree_summary %>%
  unite(site_month, site, month, sep = "_", remove = FALSE) %>%
  unite(trt_month, trt, month, sep = "_", remove = FALSE)
  

#a little function to identify outliers based on IQR (what a boxplot would show)
#pulled from the StackOverflow response below, edited to report if high or low outlier
#if not an outlier, returns the degree value
#https://stackoverflow.com/questions/44737985/r-remove-outliers-in-a-dataframe-grouped-by-factor
id_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- "outlier_low"
  y[x > (qnt[2] + H)] <- "outlier_high"
  y
}

#identify the outliers with high degree compared to their grid, keep only those entries
#for a given site, month
degreeoutliers_sitemonth <- degree_summary %>% 
  group_by(site_month) %>% 
  mutate(outlier = id_outliers(degree)) %>%
  filter(outlier == "outlier_high")
#31 individuals

#pull the month, tag, outlier status
degout_short <- degreeoutliers_sitemonth %>%
  ungroup() %>%
  dplyr::select(month, tag, outlier)

#combine outlier status into fulltrap_traits
#pulls over non-outliers as NA, replace with "norm" (...inlier?)
fulltrap_traits_degout <- left_join(fulltrap_traits, degout_short, by=c("month", "tag")) %>%
  mutate(outlier = replace(outlier, is.na(outlier), "norm")) %>%
  mutate(outlier = as.factor(outlier))
######### HOWEVER - BE ADVISED:
  ###being an outlier is particular to that month, NOT THE INDIVIDUAL 
  ###the same individual wasn't always a deg outlier each month they were captured

# #other ways to ID outliers (less inclusive)
# #for a given treatment, month
# degreeoutliers_trtmonth <- degree_summary %>% 
#   group_by(trt_month) %>% 
#   mutate(outlier = id_outliers(degree)) %>%
#   filter(outlier == "outlier_high")
# #28 individuals
# 
# #ignoring all grouping variables, just combining all the degree measures across grids, months
#   #this is what the boxplot.stats() fxn was reporting
# degreeoutliers_ever <- degree_summary %>% 
#   ungroup() %>%
#   mutate(outlier = id_outliers(degree)) %>%
#   filter(outlier == "outlier_high")
# #9 individuals

## what do I want to know about these animals?
  # what makes them unique (compared to who? the rest of their grid? all other voles?)
  # breeding class, sex, bodysize (body condition?), number of times captured? distance moved?


####### SOME WEE STATS <- maybe I'm not doing this right
data <- fulltrap_traits_degout
kruskal.test(cap.freq ~ outlier, data = data)
#ehh outliers don't really have more captures than non-outliers (p=0.0532)
kruskal.test(mass ~ outlier, data = data)
#mass is not different between deg outliers and non

########### this isn't quite what I want it to do yet... 
    ##want a way to test if the number of outliers is different by treatment
#by food added or not
outliers_sitemonth_summary <- fulltrap_traits_degout %>% group_by(food_trt, month, outlier) %>%
  summarise(n = length(tag)) %>%
  unite(trt_out, food_trt, outlier, sep = "_", remove = FALSE)
#by all treatments
outliers_sitemonth_summary <- fulltrap_traits_degout %>% group_by(trt, month, outlier) %>%
  summarise(n = length(tag)) %>%
  unite(trt_out, trt, outlier, sep = "_", remove = FALSE)

outliers_trt_summary <- fulltrap_traits_degout %>% group_by(trt, outlier) %>%
  summarise(n = length(tag)) %>%
  ungroup() %>%
  add_row(trt="control_control", outlier="outlier_high", n=0) %>% #add missing data
  dplyr::mutate(across(where(is.character), as_factor))
#pivot from long to wide
dat.wide <- outliers_trt_summary %>%
  pivot_wider(names_from = outlier, values_from = n)


outliers_sex_summary <- fulltrap_traits_degout %>% group_by(sex, outlier) %>%
  summarise(n = length(tag))
####### SOMETHING IS WRONG HERE BECAUSE THERE ARE 37 VOLES WITH NO SEX ###########
  


fulltrap %>% unite(trt, food_trt, helm_trt, sep="_", remove=FALSE) %>%
  group_by(trt) %>%
  summarise(n = length(tag))

dat.wide <- dat.wide %>% mutate(sum = norm+outlier_high)

#############################
### this isn't really working, probably because there are so few counts of outliers?
############################
library(data.table)
setDT(dat.wide)
dat.wide

dat.wide <- matrix(unlist(dat.wide), 2)
chisq <- chisq.test(dat.wide)
chisq
library(chisq.posthoc.test)

chisq.posthoc.test(as.matrix(dat.wide))
########################################


################################# end degree outliers ###############################




######## Simple visualizations -- violin plots of deg dist ###########

# #degree distribution per site across time, facet by treatment
# net_mets_summary %>% 
#   ggplot(aes(x=month, y=wt.deg, fill=site)) + 
#   geom_violin() + 
#   facet_wrap(~trt)
# 
# net_mets_summary %>% 
#   ggplot(aes(x=month, y=wt.deg, fill=site)) + 
#   geom_boxplot() + 
#   facet_wrap(~trt)
# 
# # EXAMPLE: degree distribution per site in August, color by trt
# net_mets_summary %>% filter(month == "aug") %>%
#   ggplot(aes(x=site, y=wt.deg, fill = trt)) +
#   geom_violin()



#compare distribution on degree across treatments, per month ##FUN TRENDS!
net_mets_summary %>% 
  ggplot(aes(x=trt, y=wt.deg, fill=trt)) + 
  geom_violin() + 
  facet_wrap(~month)

net_mets_summary %>%
  ggplot(aes(x=trt, y=wt.deg, fill=trt)) + 
  geom_boxplot() + 
  facet_wrap(~month)

net_mets_summary %>% 
  drop_na(sex) %>%
  ggplot(aes(x=trt, y=wt.deg, fill=sex)) + 
  geom_violin() + 
  facet_wrap(~month)



######## new 5.3.22 - adding in sex to degree distribution

#degree by month, sex - facet trt
#violin plot
net_mets_summary %>% 
  drop_na(sex) %>%
  ggplot(aes(x=month, y=wt.deg, fill = sex)) +
  geom_violin() +
  facet_wrap(~trt)
#degree by month, sex - facet trt
#box plot
net_mets_summary %>% 
  drop_na(sex) %>%
  ggplot(aes(x=month, y=wt.deg, fill = sex)) +
  geom_boxplot() +
  facet_wrap(~trt)
#degree by trt, sex - facet month
#violin plot
net_mets_summary %>% 
  drop_na(sex) %>%
  ggplot(aes(x=trt, y=wt.deg, fill = sex)) +
  geom_violin() +
  facet_wrap(~month)
#degree by trt, sex - facet month
#box plot
net_mets_summary %>% 
  drop_na(sex) %>%
  ggplot(aes(x=trt, y=wt.deg, fill = sex)) +
  geom_boxplot() +
  facet_wrap(~month)


###### new 5.4.22 -- pulling out outliers by degree #######


















# degree_summary$site <- recode_factor(degree_summary$site,
#                                      asema = "1",
#                                      helmipollo = "2",
#                                      hevonen = "3",
#                                      janakkala = "4",
#                                      ketunpesa = "5",
#                                      kiirastuli = "6",
#                                      kuoppa = "7",
#                                      mustikka = "8",
#                                      puro = "9",
#                                      radio = "10",
#                                      talo = "11",
#                                      vaarinkorpi = "12")
# degree_summary$site <- as.numeric(degree_summary$site)

#comparing degree distributions of different sizes
# https://stats.stackexchange.com/questions/264785/comparing-distributions-of-unequal-sample-sizes
# less helpful: https://mgimond.github.io/ES218/Week05b.html#The_quantile-quantile_(Q-Q)_plot

data <- degree_summary %>%
  filter(month == "oct")

kruskal.test(degree ~ trt, data = data)

# library(FSA) #package for dunnTest() function
#https://rcompanion.org/handbook/F_08.html
dunnTest(degree ~ trt, data=data, method="bh") 
PT <-dunnTest(degree ~ trt, data=data, method="bh")$res
    #pvalue adjusts for multiple comparison, so as to not increase possibility of type-I error
#to instead see the compact letter display
# library(rcompanion)
cldList(P.unadj ~ Comparison, data=PT, threshold = 0.05)

#this will give you the groups, but maybe gives slightly different answers than above ?
# library(agricolae)
# kruskal(data$degree, data$trt, group = TRUE, p.adj = "BH")$statistics
# kruskal(data$degree, data$trt, group = TRUE, p.adj = "BH")$groups




 # degree ~ trt (per month)
# may: p=0.3131
# june: p < 0.05 [c/c different from all others, others are similar]
# july: p=0.05011 (eh maybe c/deworm|supp/c and supp/c|supp/dworm are different? p.unadj)
# aug: p=0.05584 (eh maybe c/dworm|supp/dworm are different? p.unadj)
# sept: p < 0.05 [c/c different from all others, c/dworm diff from all others...
    # ...supp grids are diff from control, but not each other]
#oct: p < 0.05 [control grids same, supp grids diff from control, supp grids the same]

################################ end 4/21/22 #####################################





# per site, occasion - summarize max degree, min degree, avg degree (weighted) - max frequency
degreestats <- degreefreq_summary %>%
  group_by(site, occ) %>%
  filter(freq > 0) %>%
  summarise(min.deg = min(degree),
            max.deg = max(degree),
            avg.deg = round(weighted.mean(degree, freq),2),
            max.freq = max(freq),
            deg.max.freq = degree[which.max(freq)]) %>%
  left_join(y=grid_trts, by = "site") #join the grid treatments for plotting
#occasions reorganized themselves alphabetically?
degreestats$occ <- factor(degreestats$occ, levels = c("may", "june", "july", "aug", "sept", "oct"))

#### TROUBLESHOOT #### sometimes the max freq is shared across several degree values but only the first is reported above ^^
# test <- degreefreq_summary %>% group_by(site, occ) %>% filter(freq == max(freq))


#plot degree stats
degreestats %>% ggplot(aes(x=occ, y=max.deg, fill=trt)) + 
  geom_boxplot()

degreestats %>% ggplot(aes(x=occ, y=avg.deg, fill=trt)) + 
  geom_boxplot()





###################################################


# #load packages for plotting multiple plots together (the tidyverse parmfrow)
# library(gridExtra) #for the grid.arrange() function
# library(grid) #this is to add a title to the grid.arrange() complex o' graphs


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
      xlim(-1, 40) +
      ylim(0,10)
    site[[j]] <- plot
    
  }
  
  plot_list[[i]] <- site
  
  #save the grid.arrange() of all the degdist plots as a .png
  png(paste("wiocc_degdist_", names(degreefreq_list)[i], "_2021", ".png", sep = ""))
  do.call(grid.arrange, c(plot_list[[i]], 
                          ncol=3, 
                          top = paste(names(degreefreq_list)[i], "degree distributions", sep = " ")))
  dev.off()
  
  ####axes could have better tick marks
  ####there could be more code here to define the dimensions of the output
  
}

####################################################################################################################











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
  mtext(paste(names(igraph_list)[i], "- within occasion"), side = 3, line = -15, outer = TRUE)
}
########## struggling with how to change the labels above each network and how to give the whole shebang a title



#then save the networks to .png in a loop
for(i in 1:length(igraph_list)) {
  png(paste("withinocc_", names(igraph_list)[i], "_2021", ".png", sep = ""))
  
  CMRnet::cmrSocPlot(nets=igraph_list[[i]],fixed_locs=TRUE,dynamic=FALSE,vertex.label=NA, vertex.size=5)
  mtext(paste(names(igraph_list)[i], "- within occasion"), side = 3, line = -50, outer = TRUE)
  
  dev.off()
}

######################################## END plotting networks ###########################################
