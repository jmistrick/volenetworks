############ Degree Frequency list creation ##############

#used to be in '02_trapping_networks' but it was long and I wasn't using it - put it back if you need it


mutate(month = fct_relevel(month, "may", "june", "july", "aug", "sept", "oct"))
#might be useful at some point


############### THIS IS THE OG CODE ##################

#create dfs of the frequency of degree counts for each site/occasion - to be used for degree distribution histograms

# Create list to store results
degreefreq_list <- list()
degree_list <- list()

# Calculate network metrics to use
for(i in 1:length(net_mets_list)){

  print(i)
  site <- list()
  site.deg <- list()

  for(j in 1:length(net_mets_list[[i]])){
    #each occasion under a site

    degree <- net_mets_list[[i]][[j]]$deg
    tag <- net_mets_list[[i]][[j]]$tag
    degree.df <- data.frame(table(degree=factor(degree, levels=seq(0, max(degree), by=1))))
    degree.df$degree <- as.numeric(as.character(degree.df$degree))

    site[[j]] <- degree.df #write df for each occasion as a separate item under 1st order site

    site.deg[[j]] <- data.frame(tag, degree)

  }
  degreefreq_list[[i]] <- site
  degree_list[[i]] <- site.deg
}

#name the 12 1st order elements of nets_list as the sites
names(degreefreq_list) <- names(cmr_list)
names(degree_list) <- names(cmr_list)

#rename the sublist items (months) for each site
for(i in 1:length(degreefreq_list)){
  ifelse( length(degreefreq_list[[i]]) == 6, names(degreefreq_list[[i]]) <- c("may", "june", "july", "aug", "sept", "oct"),
          names(degreefreq_list[[i]]) <- c("june", "july", "aug", "sept", "oct") )
  }

for(i in 1:length(degree_list)){
  ifelse( length(degree_list[[i]]) == 6, names(degree_list[[i]]) <- c("may", "june", "july", "aug", "sept", "oct"),
          names(degree_list[[i]]) <- c("june", "july", "aug", "sept", "oct") )
}


################## ABOUT degreefreq_list ####################
# a list of 12 items (1 per site), with 5-6 sub-items each (1 per occasion)
# under each site/occasion, is a df of degree count and frequency of occurrence


########compress degreefreq_list down to a df

#collate results
#this collapses the 2nd order elements (degre distributions for a single month) down to the 1st order element (site)
#so now degreefreq_list_summary is a list of 12 dfs, each is all the degree distributions for a site across for all months

#make a list to store things
degreefreq_list_summary <- list()

#loop across all sites and collapse the dfs per occasion into one df for the site
for(i in 1:length(degreefreq_list)){

  #for all 12 sites
  summary <- do.call("rbind", degreefreq_list[[i]])

  #the occasion is in the row name, make this a column
    #row names are the sites, make that a column
    #separate the site part from the index and get rid of the index
  summary <- summary %>% rownames_to_column("name") %>% separate(name, c("occ", NA)) %>% mutate(occ = as.factor(occ))

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

#add grid treatments
#read in a csv of the grid treatments
grid_trts <- read.csv(here("grid_trts.csv"))
#combine food_trt and helm_trt into single 'trt' column
grid_trts <- grid_trts %>%
  unite(trt, food_trt, helm_trt, sep = "_", remove = FALSE) #keep the original food_trt and helm_trt columns
degreefreq_summary <- degreefreq_summary %>%
  left_join(grid_trts, by="site") %>%
#   relocate(c(trt, food_trt, helm_trt), .after="site")
  
  
  
  
  
  
  
  
  
  




################ then I did some trimming of the above ^^ (6.13.22) to make just degree_list (not frequency counts)
          ##### but this info is already in net_mets... so I don't need this code


#create degree_list - just a list of all the degree counts in the network

# Create list to store results
degree_list <- list()

# Calculate network metrics to use
for(i in 1:length(binary_net_mets_list)){
  
  print(i)
  site.deg <- list()
  
  for(j in 1:length(binary_net_mets_list[[i]])){
    #each occasion under a site
    
    degree <- binary_net_mets_list[[i]][[j]]$deg
    tag <- binary_net_mets_list[[i]][[j]]$tag
    
    site.deg[[j]] <- data.frame(tag, degree)
    
  }
  degree_list[[i]] <- site.deg
}

#name the 12 1st order elements of nets_list as the sites
names(degree_list) <- names(cmr_list)

#rename the sublist items (months) for each site
for(i in 1:length(degree_list)){
  ifelse( length(degree_list[[i]]) == 6, names(degree_list[[i]]) <- c("may", "june", "july", "aug", "sept", "oct"),
          names(degree_list[[i]]) <- c("june", "july", "aug", "sept", "oct") )
}

#make a list to store things
degree_list_summary <- list()

#loop across all sites and collapse the dfs per occasion into one df for the site
for(i in 1:length(degree_list)){
  
  #for all 12 sites
  summary <- do.call("rbind", degree_list[[i]])
  
  #the occasion is in the row name, make this a column
  #row names are the sites, make that a column 
  #separate the site part from the index and get rid of the index
  summary <- summary %>% rownames_to_column("name") %>% 
    separate(name, c("month", NA))
  
  degree_list_summary[[i]] <- summary
  
}

#name the 12 1st order elements as their sites
names(degree_list_summary) <- names(cmr_list)

#make degreefreq_list_summary into freiggein huge df
degree_summary <- do.call(rbind.data.frame, degree_list_summary)

#clean up the df
degree_summary <- degree_summary %>% 
  rownames_to_column("name") %>% #row names are the sites, make that a column
  separate(name, c("site", NA)) #separate the site part from the index and get rid of the index

#overwrite degree_summary to include vole-level traits
degree_summary <- left_join(degree_summary, fulltrap_traits, by=c('tag'='tag', 'month'='month')) %>% 
  dplyr::select(!site.y) %>%
  rename(site = site.x) %>%
  relocate(c(trt, food_trt, helm_trt), .after="site") %>%
  relocate(occasion, .after="month") %>%
  relocate(degree, .after=tag) %>%
  mutate(site = as.factor(site)) %>%
  mutate(trt = as.factor(trt)) %>%
  mutate(month = as.factor(month))