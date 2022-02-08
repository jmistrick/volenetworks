#just in case - backup backup of single network CMRnet code (from network noodling)


#############################     code from MattSilk github       #############################
#####################   to make a network for just one site at a time   #######################

#Define parameters for the network
mindate<- as.Date("2021-04-30") 
maxdate<- as.Date("2021-08-30")
#interaction window - time for a contact (days)
intwindow<-2
#network window (months)
netwindow<-1
overlap<-0
#distance to consider contact (0=same trap)
spacewindow<-1.5

# Create co-capture (social) networks
#index=FALSE indicates that we want edges to be weighted by the number of interactions
#index=TRUE weights edges based on association indices
testnet <- DynamicNetCreate(data = mustikka,
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
#CMRnet::cmrSocPlot(nets=cc_nets,fixed_locs=TRUE,dynamic=FALSE,vertex.label=NA, vertex.size=5)


#############################################################################################


################################  calculate network statistics ##########################################
################################  from a single network   ##########################################

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