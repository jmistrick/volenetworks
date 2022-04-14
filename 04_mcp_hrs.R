# code run using R version 4.0.2 (2020-06-22) -- "Taking Off Again"

#load libraries
library(here)
library(tidyverse)
library(rgdal)
library(sp)
library(sf)
library(adehabitatHR)

#clear environment
rm(list = ls())

#load the fulltrap dataset (make sure it's the most recent version)
fulltrap <- readRDS(file = "fulltrap_12.30.21.rds")

################################  prep code  ####################################

#subset fulltrap for use in mcp analysis -- only animals captured at least 5 times
mcp_trap <- fulltrap %>%
  group_by(site, tag) %>%
  mutate(cap_freq = n()) %>% #create column for capture frequency (# times seen)
  filter(cap_freq >= 5) #filter for at least 5 captures


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
 

#### also, it would be nice to have useful individual-level data for each of our residents

# #check to make sure everyone only has one sex
# resident_traits <- mcp_trap %>% 
#   dplyr::select(-date_time, -session, -occasion)
#
# resident_traits$sex <- as.character(resident_traits$sex)
# check <- resident_traits %>% group_by(tag) %>%
#   summarise(n = unique(sex)) %>%
#   filter(!is.na(n))
# #save the tag IDs of animals with multiple sexes
# duplist <- as.vector(check$tag[duplicated(check$tag)])
# #how many?
# length(duplist) #1 vole
# sexswaps <- resident_traits %>%
#   filter(tag %in% duplist) %>%
#   arrange(tag, occ.sess)

###### FOR NOW: there is one animal at Talo that I can't confirm the sex of - just going to make their sex NA #######
####################### changed in 12.30.21_SEXCORRECTED ###########################
########################## CHANGE THIS LATER!!! CHANGE THIS LATER!!! CHANGE THIS LATER!!! #######################


#trim down extra data to just summary traits for each individual 
    #this means avg/mass and head, no breeding status
mcp_trap <- mcp_trap %>%
  group_by(tag) %>%
  arrange(tag, occ.sess) %>%
  mutate(avg_mass = mean(mass, na.rm=TRUE),
         avg_head = mean(head, na.rm=TRUE)) %>%
  unite(trt, food_trt, helm_trt, sep = "_", remove = FALSE) %>%
  dplyr::select(!c(fate, date_time, session, occasion)) %>%
  relocate(avg_mass, .after=mass) %>%
  relocate(avg_head, .after=head) %>%
  mutate(status = "res")

#mcp_trap now includes: one row for each time the animal was trapped
#occ.sess 
#avg_head, avg_mass - and just head, mass for each capture
#cap_freq (number of times captured)
#sex & all breeding status stuff ####### PLAY WITH THIS LATER? ######

################# WORKING HERE 4.14.22 #################

#this code will first make a column (breeder) of "breeder"/"nonbreeder" based on 1 for nip/preg or test
#NAs persist
#then it will make a new column based on breeder column where if ever the animal was breeder, it gets "active"
#use NA.rm in this to make sure NAs are ignored and get replaced with the right tag
#and then remove the unneeded columns (breeder, perf, nip, preg, test)
#### IF YOU WANT TO LOOK BY TIME, ADD THESE BACK IN #####
mcp_trap <- mcp_trap %>% mutate(breeder = case_when( nip=="1" | preg == "1" ~ "breeder",
                                                   test == "1" ~ "breeder",
                                                   nip=="0" & preg == "0" ~ "nonbreeder",
                                                   test == "0" ~ "nonbreeder")) %>%
  relocate(breeder, .after=sex) %>% 
  group_by(tag) %>%
  mutate(repro = ifelse(any(breeder == "breeder", na.rm = TRUE), "active", "inactive")) %>%
  relocate(repro, .after = breeder) %>%
  dplyr::select(!c(breeder, per, nip, preg, test))
  


######### WORKING HERE - THEN NEED TO PULL THIS COLUMN, 
  ## IF EVER BREEDER, use that as vole-level data for summary 'traits' df

  
  

#pull out tag, sex
mcp_sex <- mcp_trap %>% 
  filter(firstcap == "1") %>%
  dplyr::select(c(tag, sex))

#pull out vole-level other traits
mcp_traits <- mcp_trap %>% 
  filter(firstcap == "1") %>%
  dplyr::select(c(occ.sess, tag, repro, avg_head, avg_mass, cap_freq, status))

#remove sex from mcp_trap (had sex with M,F,or NA)
mcp_trap <- mcp_trap %>%
  dplyr::select(-sex)

#re-add sex so its listed each time the tag is listed (no NAs)
mcp_trap <- left_join(mcp_trap, mcp_sex, by="tag") %>%
  relocate(sex, .after=tag)


#split() fulltrap into ... A LIST! by site
mcp_list <- split(mcp_trap, f = mcp_trap$site)


###############################################  end prep code   ################################################






######### Loop to Create SpatialPointsDF for each site - polygons for each resident vole in 2021 #############
### results are: one list of 12 spdf objects, one list of 12 dfs with polygon areas, .png file for each site ###

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

## this has created two lists and created png files for each site
  #first list is basically the 'cp' file for each site - it's the full spatialpointsdf of all polygons
  #second list is the cp@data file that gives ID of each animal and the area of their HR



### for later analysis - make HRarea_list into df
HRarea_summary <- do.call(rbind.data.frame, HRarea_list)

#clean up the df
HRarea_summary <- HRarea_summary %>% 
  rownames_to_column("name") %>% #row names are the sites, make that a column
  separate(name, c("site", NA)) %>% #separate the site part from the index and get rid of the index
  mutate(site = as.factor(site)) #make site a factor

#read in a csv of the grid treatments
grid_trts <- read.csv(here("grid_trts.csv"))
#combine food_trt and helm_trt into single 'trt' column
grid_trts <- grid_trts %>% 
  unite(trt, food_trt, helm_trt, sep = "_", remove = FALSE) #keep the original food_trt and helm_trt columns

#join the grid_trts to HRarea_summary
HRarea_summary <- HRarea_summary %>% 
  left_join(y=grid_trts, by = "site") %>%
  rename(tag = id)
#join the mcp_sex to HRarea_summary
HRarea_summary <- HRarea_summary %>%
  left_join(y=mcp_sex, by="tag") %>%
  relocate(sex, .after=tag)
#join mcp_traits to HRarea_summary
HRarea_summary <- HRarea_summary %>%
  left_join(y=mcp_traits, by="tag")






####################### in a loop for all 12 sites in 2021 ###########################
####### Calculate area of overlap & % overlap (how much of each vole's HR overlaps with  another's) ###########


pct_overlap_list <- list()

for(i in 1:length(sitespdf_list)){
  
  print(i)

  cp <- sitespdf_list[[i]]
  
  newcp <- st_as_sfc(sitespdf_list[[i]])
  
  #calculate amount of overlap between polygons (in hectares)
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
  # df #is a df version of the matrix, shows area of overlap in hectares

  #calculate percent overlap (a directed measure) between polygons
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
  # df2 #is a df version of the matrix, shows percent overlap
  
  df_full <- left_join(df, df2) #combine area of overlap and percent overlap into one df
  # df_full
  
  pct_overlap_list[[i]] <- df_full #and write that df for a given site as a list item
  
}

#name the 12 1st order elements as their sites
names(pct_overlap_list) <- names(mcp_list)
#this is a list of 12 sites, each item has a df matrix of the amt and percentage overlap between residents

## make pct_overlap_list into freiggein huge df
pct_overlap_summary <- do.call(rbind.data.frame, pct_overlap_list)

#clean up the df
pct_overlap_summary <- pct_overlap_summary %>% 
  rownames_to_column("name") %>% #row names are the sites, make that a column
  separate(name, c("site", NA)) %>% #separate the site part from the index and get rid of the index
  mutate(site = as.factor(site)) #make site a factor

#join the grid_trts to pct_overlap_summary
pct_overlap_summary <- pct_overlap_summary %>% 
  left_join(y=grid_trts, by = "site")


#join the sex of focal animal, neighbor animal
new <- left_join(pct_overlap_summary, mcp_sex, by = c("focal" = "tag")) %>% 
  rename(focal_sex = sex) %>% 
  relocate(focal_sex, .after=focal) 
pct_overlap_summary <- left_join(new, mcp_sex, by=c("neighbor" = "tag")) %>%
  rename(neighbor_sex = sex) %>%
  relocate(neighbor_sex, .after=neighbor)
#make a new column for focal-neighbor sexes
pct_overlap_summary <- pct_overlap_summary %>% 
  unite(f_n_sex, focal_sex, neighbor_sex, sep = "_", remove = FALSE) %>% #keep the original columns
  relocate(f_n_sex, .before=focal) %>%
  drop_na() #########remove sex=NA######
#make f_n_sex a factor, combine M_F and F_M
pct_overlap_summary$f_n_sex <- as.factor(pct_overlap_summary$f_n_sex)
levels(pct_overlap_summary$f_n_sex) <- c("F_F", "mixed", "mixed", "M_M")

################## IS THERE A CLEVER WAY TO ADD BODY SIZE or CAP_FREQ HERE? ############################

######## NICE - this now has the % overlap and area of overlap for all voles that did overlap in their HR
### HOWEVER - we've now lost any data on any individuals that did not overlap which we'll need to do % overlapping




## hence this next bit of code:

################## Calculate percent overlapping (how many of the resident voles have any HR overlap) ############

## pulling from code above where I made cp@data into a big df for all sites (ie the HRarea_summary df)
#summarise count of 'resident' voles per site
n.HRs <- HRarea_summary %>% 
  group_by(site) %>% 
  summarise(unique(tag)) %>% 
  count() %>%
  ungroup()

#summarise number of focal voles from pct_overlap_summary (ie number per site with overlaps)
n.overlaps <- pct_overlap_summary %>% group_by(site) %>% 
  summarise(unique(focal)) %>% 
  count() %>%
  rename(n.overlapping = n) %>%
  ungroup()

pct_overlapping_summary <- left_join(n.HRs, n.overlaps, by="site") #join the two summaries

#replace NA with 0 for n.overlapping at Kuoppa 
  #(there were no overlapping voles there so it didn't have a row in n.overlaps so it got NA in 'pct_ovlpping_summary')
pct_overlapping_summary <- pct_overlapping_summary %>% replace(is.na(.), 0)
#add a column for percent overlapping (how many voles have a HR that overlaps with another)
pct_overlapping_summary <- pct_overlapping_summary %>% mutate(pct.overlapping = n.overlapping/n)

#join the grid_trts to pct_overlapping_summary
pct_overlapping_summary <- pct_overlapping_summary %>% 
  left_join(y=grid_trts, by = "site")




##################################################################################################################
##########################################   end percent overlap/lapping  ########################################
##################################################################################################################


############### Plot some trends - area of overlap, percent overlap, percent overlapping  ########################


############### REMOVE THE DROP.NA WHEN WE NO LONGER HAVE NA for SEX ############


HRarea_summary %>% ggplot(aes(x=trt, y=area, fill=trt)) +
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Capture Area (hectares)",
       title="Capture Area by Treatment")
#HR area is pretty similar between treatments

HRarea_summary %>% 
  drop_na(sex) %>%
  ggplot(aes(x=trt, y=area, fill=sex)) +
  geom_boxplot(show.legend = TRUE) +
  scale_fill_manual(values = c("F" = "#DF173B",
                               "M" = "#62B5D9")) +
  labs(x="Treatment", y="Capture Area (hectares)",
       title="Capture Area by Sex, Treatment", fill="Sex")
#females ish have smaller HRs

HRarea_summary %>% 
  drop_na(sex) %>%
  ggplot(aes(x=sex, y=area, fill=sex)) +
  geom_boxplot(show.legend = TRUE) +
  labs(x="sex", y="HR area (hectares)",
       title="HR area by sex")
#this may not be meaningful/useful since we combine across treatments



pct_overlap_summary %>% 
  ggplot(aes(x=trt, y=pct.overlap, fill=trt)) +
  geom_boxplot(show.legend = FALSE) +
  labs(x="Treatment", y="Percent Overlap bewteen Focal and Neighbor",
       title="Percent Overlap by Treatment")
#the percent overlap (how much any single vole overlaps with any other vole) is also pretty similar

pct_overlap_summary %>% 
  drop_na() %>%
  ggplot(aes(x=trt, y=pct.overlap, fill=f_n_sex)) +
  scale_fill_manual(values = c("F_F" = "#DF173B",
                               "mixed" = "#DDCC77",
                               "M_M" = "#62B5D9")) +
  geom_boxplot(show.legend = TRUE) +
  labs(x="Treatment", y="Percent Overlap bewteen Focal and Neighbor",
       title="Percent Overlap by Sex Pairing, Treatment", fill = "Focal-Neigbor Sex")




pct_overlapping_summary %>% ggplot(aes(x=trt, y=pct.overlapping, fill=trt)) +
  geom_boxplot(show.legend=FALSE) +
  labs(x="Treatment", y="Percent Overlapping",
       title="Percent of Voles with Overlapping Capture Areas by Treatment")
#HOWEVER - the percent overlapping (how many of the voles have an overlapping HR) 
    #is different between food-added and not







pct_overlap_summary %>% 
  ggplot(aes(x=trt, y=area_overlap, fill=trt)) +
  geom_boxplot(show.legend = FALSE) +
  labs(x="treatment", y="percent overlap",
       title="percent overlap by treatment")
#the percent overlap (how much any single vole overlaps with any other vole) is also pretty similar


pct_overlap_summary %>% 
  drop_na() %>%
  ggplot(aes(x=trt, y=area_overlap, fill=f_n_sex)) +
  scale_fill_manual(values = c("F_F" = "#DF173B",
                               "mixed" = "#DDCC77",
                               "M_M" = "#62B5D9")) +
  geom_boxplot(show.legend = TRUE) +
  labs(x="Treatment", y="CA overlap area (hectares)",
       title="Capture Area Overlap by Sex Pairing",
       fill="Focal-Neighbor Sex")

pct_overlap_summary %>% 
  drop_na() %>%
  ggplot(aes(x=f_n_sex, y=area_overlap, fill=f_n_sex)) +
  scale_fill_manual(values = c("F_F" = "#B10A28", 
                               "mixed" = "#8C6BD0", 
                               "M_M" = "#00A5F1")) +
  geom_boxplot(show.legend = TRUE) +
  labs(x="sexes of overlappers", y="HR overlap area (hectares)",
       title="HR overlap area by sex, treatment")
#a little less F-F overlap than M-F or M-M overlap?
#again, this might be suspect since it's combining across treatments





#this is interesting
HRarea_summary %>% 
  drop_na() %>% 
  ggplot(aes(x=avg_mass, y=area, color=sex)) +
  scale_color_manual(values = c("F" = "#DF173B",
                               "M" = "#62B5D9")) +
  geom_point() + 
  geom_smooth(method=lm, 
              formula = y ~ x,
              se=TRUE) +
  labs(title="Capture Area by Average Mass, Sex")


## just a few models to look at HR area based on female avg mass
femaleHR <- HRarea_summary %>%
  filter(sex == "F")
maleHR <- HRarea_summary %>%
  filter(sex == "M")

fHR_mod <- lm(area ~ avg_mass, data=femaleHR)
summary(fHR_mod)
fHR_modpoly <- lm(area ~ poly(avg_mass, 2), data=femaleHR)
summary(fHR_modpoly)
AIC(fHR_mod, fHR_modpoly) #polynomial fit is better for females

mHR_mod <- lm(area ~ avg_mass, data=maleHR)
summary(mHR_mod)
mHR_modpoly <- lm(area ~ poly(avg_mass, 2), data=maleHR)
summary(mHR_modpoly)
AIC(mHR_mod, mHR_modpoly) #linear fit is better for males




HRarea_summary %>% 
  drop_na() %>% 
  ggplot(aes(x=avg_head, y=area, color=sex)) + 
  geom_point() + 
  geom_smooth(method=lm, se=TRUE) +
  labs(title="HR area by avg head width for males and females")

HRarea_summary %>% 
  drop_na() %>% 
  ggplot(aes(x=cap_freq, y=area, color=sex)) + 
  geom_point() + 
  geom_smooth(method=lm, se=TRUE) +
  labs(title="HR area by capture frequency for males and females")





#################### CURRENTLY SET UP TO DO % HR OVERLAP by SEX #########################



#load the data you want
dat <- fulltrap2
category <- fulltrap2$sex_status
response <- fulltrap2$avg_head
category <- as.factor(category)


mod <- aov(response ~ category, data=dat)
mod

##oof, let's check them assumptions
#data are normally distributed and the variance across groups are homogeneous

#plot residuals vs. fitted - for homogeneity of variance
#looking for outliers (several)
#but the red line fits the 0 pretty well
#however, there is some fanning
plot(mod, 1)

#or use the Levene's test in car package - less sensitive to departures from normal dist
library(car)
leveneTest(response ~ category, data=dat)

#check the output to see if p-value is less than the significance level of 0.05. 
#IF pvalue is NOT significant: This means that there is no evidence to suggest that the 
#variance across groups is statistically significantly different. 
#We can assume the homogeneity of variances in the different treatment groups.
## IF pvalue is significant, this suggests variances are not the same across groups 


#classical ANOVA has a strict assumption of equal variance for all groups
# Welch one-way test DOES NOT require equal variance
# Welch test is for normally distributed data that violates assumption of homogeneity of variance

#ANOVA with no assumption of equal variances
oneway.test(response ~ category, data=dat)

#pairwise t-tests with no assumption of equal variances
pairwise.t.test(response, category,
                p.adjust.method = "BH", pool.sd = FALSE)
########### NOT SURE WHAT THE p.adjust.method means CHECK THIS >> ?p.adjust

#check normality
plot(mod, 2) #if the points fall mostly along the line (ignoring outliers) - you're good
#the Skapiro-Wilk test can also test normality from the residuals of the ANOVA
residuals <- residuals(mod)
shapiro.test(x=residuals) #as long as the pvalue is large, you're good


###### Kruskal-Wallis rank-sum test is a non-parametric alternative to ANOVA
# non-parametric meaning that it doesn't assume your data come from a specific distribution 
#(ie they don't have to be normally distributed)
#commonly used if you have 2 or more levels - for only 2 levels, use Mann-Whitney
# ? also does not assume equal variance // but another source said it was unstable when variance was not equal

#Kruskal-Wallis rank-sum test
kruskal.test(response ~ category, data=dat)

#if result is significant, a common post-hoc test is the Dunn test - this again allows for pvalue adjustments to
# "control the familywise error rate or the false discovery rate"
# Zar 2010 states that Dunn test is appropriate for groups with unequal numbers of observations
#Dunn test is a post-hoc non-parametric test but can be VERY conservative

library(FSA) #package for dunnTest() function
dunnTest(response ~ category, data=dat, method="bh")


#########################################################################################################################

#HR overlap by sex -- homogeneity of variance and normality are both violated - ran Kruskal-Wallis
  # HR overlap of F-F vs mixed vs M-M are all very different

































############### more to do: (as of 31 March 2022)


############# oooooo and can we see all the grids side by side? I want this shit on my wall #########################
### the loop above saves each plot as a .png file - can we keep them all in R and do a lil parmfrow thing? ####


############## 3.31.22 this code should be built into a loop too for funsies ###################
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

#################################################################################################














mcp_trap %>% ggplot(aes(x=trt, y=cap_freq, fill=sex)) +
  geom_boxplot(show.legend=TRUE) +
  labs(x="treatment", y="capture frequency",
       title="capture frequency by sex, treatment")












#how many individuals did we tag
fulltrap %>%
  group_by(tag) %>%
  summarise(n = length(tag)) %>%
  nrow()
#772

#how many animals were recaptured at least 5 times
fulltrap %>%
  group_by(tag) %>%
  summarise(n = length(tag)) %>%
  filter(n >= 5) %>%
  nrow()
#123

#percentage that are 'resident'
123/772


#subset fulltrap for nonresidents - captured less than 5 times
nonres <- fulltrap %>%
  group_by(site, tag) %>%
  mutate(cap_freq = n()) %>% #create column for capture frequency (# times seen)
  filter(cap_freq <= 4) #filter for at least 5 captures

#get avg_head, avg_mass for all nonres, just one entry per animal
nonres <- nonres %>%
  group_by(tag) %>%
  arrange(occ.sess) %>%
  mutate(avg_mass = mean(mass, na.rm=TRUE),
         avg_head = mean(head, na.rm=TRUE)) %>%
  filter(firstcap == "1") %>%
  unite(trt, food_trt, helm_trt, sep = "_", remove = FALSE) %>%
  dplyr::select(!c(head, mass, fate, date_time, session, occasion, trap, x, y)) %>%
  dplyr::select(!c(per, nip, preg, test)) %>% #dropping all the breeding status for now
  relocate(avg_mass, .after=sex) %>%
  relocate(avg_head, .after=avg_mass) %>%
  mutate(status = "nonres") %>%
  ungroup()

#separate nonresident females
nonresF <- nonres %>%
  filter(sex == "F")
#separate nonresident males
nonresM <- nonres %>% 
  filter(sex == "M")

mean(nonresF$avg_head)
#12.79
mean(nonresM$avg_head, na.rm=TRUE)
#12.97
mean(maleHR$avg_head)
#13.06
mean(femaleHR$avg_head)
#13.10

mean(nonresF$avg_mass)
#15.99
mean(nonresM$avg_mass, na.rm=TRUE)
#16.09
mean(maleHR$avg_mass)
#16.78
mean(femaleHR$avg_mass)
#19.21


fulltrap2 <- bind_rows(mcp_trap, nonres) %>%
  dplyr::select(!c(trap, easting, northing)) %>%
  filter(firstcap == "1") %>%
  unite(sex_status, sex, status, sep = "_", remove = FALSE)


fulltrap2 %>% drop_na() %>%
  ggplot(aes(x=sex, y=avg_mass, color=status)) +
  geom_boxplot() +
  labs(title = "Average Mass by Sex, Resident status")
#### the only trouble with this is that avg_mass will even out pregnancy the more times we catch you

fulltrap2 %>% drop_na() %>%
  ggplot(aes(x=sex, y=avg_head, color=status)) +
  geom_boxplot() +
  labs(title = "Average Head Width by Sex, Resident Status")












#count of how many polygons are in the spatialpolygonsdf
length(cp@polygons)




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

