# code run using R version 4.0.2 (2020-06-22) -- "Taking Off Again"

#load libraries
library(here)
library(tidyverse)
library(cowplot)
library(lubridate)

#clear environment
rm(list = ls())

#read in the fulltrap data
fulltrap <- readRDS(file = "fulltrap_12.30.21.rds") 


################################################################################################

n_distinct(fulltrap$tag)
#772 PIT tags

nrow(fulltrap)
#2088 capture entries

#how many animals were recaptured
fulltrap %>%
  group_by(tag) %>%
  summarise(n = length(tag)) %>%
  filter(n > 1) %>%
  nrow()
#458 animals were captured at least twice (59.3%)

#summarise number of recaps per tag
#average number of captures per individual, sd
fulltrap %>%
  group_by(tag) %>%
  summarise(n = length(tag)) %>%
  summarise(mean = mean(n), sd = sd(n))
#mean: 2.70   sd: 2.30

#what if we take out everyone that was only captured once
#average number of captures for RECAPTURED animals, sd
fulltrap %>%
  group_by(tag) %>%
  summarise(n = length(tag)) %>%
  filter(n > 1) %>%
  summarise(mean = mean(n), sd = sd(n))
#mean: 3.87   sd:2.36

fulltrap <- fulltrap %>% ungroup()

#################################################################################################

#summarize number of captures per grid in 2021
total.capbygrid <- fulltrap %>%
  group_by(site) %>%
  summarise(captures = length(tag)) 

#range, mean, sd of capture by grid
range(total.capbygrid$captures)
#73, 271
mean(total.capbygrid$captures)
#174
sd(total.capbygrid$captures)
#64.834

#summarize number of individual animals per grid in 2021
total.indivbygrid <- fulltrap %>%
  group_by(site) %>%
  summarise(individuals = n_distinct(tag))

#range, mean, sd of individual by grid
range(total.indivbygrid$individuals)
#24, 109
mean(total.indivbygrid$individuals)
#64.5
sd(total.indivbygrid$individuals)
#28.047

#join these together
total.gridsummary <- total.capbygrid %>%
  left_join(y = total.indivbygrid, by = "site")

#just to check
sum(total.gridsummary$captures) #2088 - yes, this is the number of rows in fulltrap
sum(total.gridsummary$individuals) #774 is not the same as 772 which is the number of unique PIT tags, 2 repeats
############################# STILL NEEDS TO BE FIXED ^^ ###################################


#summarize number of captures per grid per occ
capbygrid <- fulltrap %>%
  group_by(site, occasion) %>%
  summarise(captures = length(tag))

#summarize number of individual animals per grid per occ
indivbygrid <- fulltrap %>%
  group_by(site, occasion) %>%
  summarise(individuals = n_distinct(tag))

#summarise number of new animal captures per grid per occ
newbygrid <- fulltrap %>%
  group_by(site, occasion) %>%
  summarise(news = sum(firstcap))

#summarise number of recap animal captures per grid per occ
recapbygrid <- fulltrap %>%
  group_by(site, occasion, tag) %>%
  slice(1) %>% #takes the first instance of any tag
  summarise(is.recap = sum(firstcap == "0")) %>% #row for each site/occ/tag - 1 if 1st instance is recap, else 0 (new that occasion)
  group_by(site, occasion) %>% #combine all rows (individual voles) for a single site/occ
  summarise(recaps = sum(is.recap)) #sum the number of recap animals


#read in a csv of the grid treatments
grid_trts <- read.csv(here("grid_trts.csv"))
#combine food_trt and helm_trt into single 'trt' column
grid_trts <- grid_trts %>% 
  unite(trt, food_trt, helm_trt, sep = "_", remove = FALSE) #keep the original food_trt and helm_trt columns

#join these together into one df
gridsummary <- capbygrid %>%
  left_join(y = indivbygrid) %>%
  left_join(y = recapbygrid) %>%
  left_join(y = newbygrid) %>%
  left_join(y = grid_trts) %>%
  relocate(trt, .after = site) %>%
  relocate(food_trt, .after = trt) %>%
  relocate(helm_trt, .after = food_trt)





#plot captures per grid
captureplot_site <- gridsummary %>% ggplot(aes(x = occasion, y = captures, group=site)) +
  geom_line(aes(color = site)) +
  ylim(0,100)
# #alternatively, color each line by treatment
captureplot_trt <- gridsummary %>% ggplot(aes(x = occasion, y = captures, group = site)) +
  geom_line(aes(color=trt)) +
  ylim(0,100)

plot_grid(captureplot_site, captureplot_trt)

#plot individuals per grid
indivplot_site <- gridsummary %>% ggplot(aes(x = occasion, y = individuals, group=site)) +
  geom_line(aes(color = site)) +
  ylim(0,100)
# #alternatively, color each line by treatment
indivplot_trt <- gridsummary %>% ggplot(aes(x = occasion, y = individuals, group = site)) +
  geom_line(aes(color = trt)) +
  ylim(0,100)

plot_grid(indivplot_site, indivplot_trt)

#combine captures and individuals plots
plot_grid(captureplot_site, indivplot_site) #labels = "AUTO" will add an A, B etc. for formatting for journals

#combine captures and individuals plots
plot_grid(captureplot_trt, indivplot_trt) #labels = "AUTO" will add an A, B etc. for formatting for journals



#average captures per treatment type
#st dev of the mean (measure of variability of individual datapoints around the mean)
meancapbygrid <- gridsummary %>% group_by(trt, occasion) %>%
  summarise(mean=mean(captures), sd=sd(captures))
#plot
meancapplot <- meancapbygrid %>% ggplot(aes(x = occasion, y = mean, group=trt)) +
  geom_line(aes(color = trt)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, color = trt), width=.2, position=position_dodge(0.3)) +
  ylim(0,100)

#average captures per treatment type
#st dev of the mean (measure of variability of individual datapoints around the mean)
meanindivbygrid <- gridsummary %>% group_by(trt, occasion) %>%
  summarise(mean=mean(individuals), sd=sd(individuals))
#plot
meanindivplot <- meanindivbygrid %>% ggplot(aes(x = occasion, y = mean, group=trt)) +
  geom_line(aes(color = trt)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, color = trt), width=.2, position=position_dodge(0.3)) +
  ylim(0,100)

#combine plots
plot_grid(meancapplot, meanindivplot)





#boxplots by food treat

gridsummary %>% ggplot(aes(x=occasion, y=captures, fill=food_trt)) + 
  geom_boxplot() +
  labs(title = "Effect of food supplementation on total number of vole captures, by occasion")

gridsummary %>% ggplot(aes(x=occasion, y=individuals, fill=food_trt)) + 
  geom_boxplot() +
  labs(title = "Effect of food supplementation on number of individual voles captured, by occasion")

gridsummary %>% ggplot(aes(x=occasion, y=recaps, fill=food_trt)) + 
  geom_boxplot() +
  labs(title = "Effect of food supplementation on number of recapture voles captured, by occasion")

gridsummary %>% ggplot(aes(x=occasion, y=news, fill=food_trt)) + 
  geom_boxplot() + 
  labs(title = "Effect of food supplementation on number of new voles captured, by occasion")



#boxplots by worm treat

gridsummary %>% ggplot(aes(x=occasion, y=captures, fill=helm_trt)) + 
  geom_boxplot() +
  labs(title = "Effect of food supplementation on total number of vole captures, by occasion")

gridsummary %>% ggplot(aes(x=occasion, y=individuals, fill=helm_trt)) + 
  geom_boxplot() +
  labs(title = "Effect of food supplementation on number of individual voles captured, by occasion")

gridsummary %>% ggplot(aes(x=occasion, y=recaps, fill=helm_trt)) + 
  geom_boxplot() +
  labs(title = "Effect of food supplementation on number of recapture voles captured, by occasion")

gridsummary %>% ggplot(aes(x=occasion, y=news, fill=helm_trt)) + 
  geom_boxplot() + 
  labs(title = "Effect of food supplementation on number of new voles captured, by occasion")


#boxplots by grid type

gridsummary %>% ggplot(aes(x=occasion, y=captures, fill=trt)) + 
  geom_boxplot() +
  labs(title = "Effect of grid manipulation on total number of vole captures, by occasion")

gridsummary %>% ggplot(aes(x=occasion, y=individuals, fill=trt)) + 
  geom_boxplot() +
  labs(title = "Effect of grid manipulation on number of individual voles captured, by occasion")

gridsummary %>% ggplot(aes(x=occasion, y=recaps, fill=trt)) + 
  geom_boxplot() + 
  labs(title = "Effect of grid manipulation on number of voles recaptured, by occasion")

gridsummary %>% ggplot(aes(x=occasion, y=news, fill=trt)) + 
  geom_boxplot() + 
  labs(title = "Effect of grid manipulation on number of new voles captured, by occasion")





##################################################################################

#unique number of animals
length(unique(fulltrap$tag))
#772 individual voles

# number of sightings of each animal
freq <- fulltrap %>%
  count(tag)

# #another view, count of recaptures of each animal
# aggregate(data.frame(count = fulltrap$tag), list(value = fulltrap$tag), length)

freq %>%
  filter(n >= 2) %>%
  nrow()
#458 have been caught at least twice (~60%)

freq %>%
  filter(n >= 3) %>%
  nrow()
#292 have been caught at least 3 times (38%)

freq %>% 
  filter (n >= 4) %>%
  nrow()
#196 have been caught at least 4 times (25%)

freq %>%
  filter (n >= 5) %>%
  nrow()
#123 at least 5 times

freq %>%
  filter (n >= 6) %>%
  nrow()
#84 animals!! caught at least 6 times

freq %>%
  filter (n >= 7) %>%
  nrow()
#54 caught at least 7 times

freq %>%
  filter (n >= 10) %>%
  nrow()
#17 animals caught at least 10 times!!!!

#most frequently captured?
max(freq$n)
#17 times
freq %>% filter(n == "17")
#animal 226080






########## look at number of voles that were caught once vs multiple times per grid #########################

#create the df
single_vs_recap <- fulltrap %>%
  group_by(site) %>%
  count(tag) %>%
  summarise(caught.once = sum(n == "1"), caught.multiple = sum(n > "1")) %>% #count number caught once vs caught multiple times
  pivot_longer(cols = !site, names_to = "capture.type", values_to = "count") %>% #pivot longer
  left_join(y = grid_trts, by = "site") #add the treatment types

#adjust the levels of capture.type
single_vs_recap$capture.type <- factor(single_vs_recap$capture.type, levels = c("caught.once", "caught.multiple"))

# plot of count of single vs multiple captures for each site - too busy, hard to read
# single_vs_recap %>% ggplot(aes(x=site, y=count, group=capture.type)) + 
#   geom_point(aes(color=capture.type)) + 
#   facet_wrap(~trt) +
#   labs(title = "Count of voles captured once vs multiple times by grid and treatment type")

#boxplots of capture counts for single vs multiple, color by treatment
single_vs_recap %>% ggplot(aes(x=capture.type, y=count, fill=trt)) + 
  geom_boxplot() +
  labs(title = "Count of voles caught once and caught multiple times by treatment type")

# #boxplots of capture counts for treatments, color by single vs multiple - less intuitive
# single_vs_recap %>% ggplot(aes(x=trt, y=count, fill=capture.type)) +
#   geom_boxplot()

#plot by food treament
single_vs_recap %>% ggplot(aes(x=capture.type, y=count, fill=food_trt)) + 
  geom_boxplot() +
  labs(title = "Count of voles caught once and caught multiple times by food treatment")

#plot by deworm treatment
single_vs_recap %>% ggplot(aes(x=capture.type, y=count, fill=helm_trt)) + 
  geom_boxplot() +
  labs(title = "Count of voles caught once and caught multiple times by deworm treatment")




####################### residents (used for MCP analysis) ####################################

#number of residents per grid (over 2021 season)
resbygrid <- fulltrap %>% 
  group_by(site) %>%
  count(tag) %>%
  filter(n>4) %>%
  count(site)

resbygrid <- resbygrid %>% left_join(grid_trts)

#identities of the residents
IDresbygrid <- fulltrap %>% 
  group_by(site) %>%
  count(tag) %>%
  filter(n>4)

#number single caps per grid (over 2021 season)
singlecapbygrid <- fulltrap %>% 
  group_by(site) %>%
  count(tag) %>%
  filter(n==1) %>%
  count(site)

singlecapbygrid <- singlecapbygrid %>% left_join(grid_trts)

#identities of the single caps
IDsinglecapbygrid <- fulltrap %>% 
  group_by(site) %>%
  count(tag) %>%
  filter(n==1)


####################### some anovas for giggles ######################

resbygrid$trt <- as.factor(resbygrid$trt)
singlecapbygrid$trt <- as.factor(singlecapbygrid$trt)

# data = resbygrid
data = singlecapbygrid

#plot
data %>% ggplot(aes(x=trt, y=n, fill=trt)) + 
  geom_boxplot() +
  labs(title = "THIS IS THE TITLE")

### number of resident voles (captured 5 or more times) is not different by treatment type
### number of singlcaptures is not different by treatment type (though boxplots look more so)
    #unless you run the Kruskal-Wallis, then control_deworm and supp_x's are different


mod <- aov(n ~ trt, data=data)
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
leveneTest(n ~ trt, data=data)
#check the output to see if p-value is less than the significance level of 0.05. 
#IF pvalue is NOT significant: This means that there is no evidence to suggest that the 
#variance across groups is statistically significantly different. 
#We can assume the homogeneity of variances in the different treatment groups.
## IF pvalue is significant, this suggests variances are not the same across groups 


#classical ANOVA has a strict assumption of equal variance for all groups
# Welch one-way test DOES NOT require equal variance
# Welch test is for normally distributed data that violates assumption of homogeneity of variance

#ANOVA with no assumption of equal variances
oneway.test(n ~ trt, data=data)

#pairwise t-tests with no assumption of equal variances
pairwise.t.test(data$n, data$trt,
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
kruskal.test(n ~ trt, data=data)

#if result is significant, a common post-hoc test is the Dunn test - this again allows for pvalue adjustments to
# "control the familywise error rate or the false discovery rate"
# Zar 2010 states that Dunn test is appropriate for groups with unequal numbers of observations
#Dunn test is a post-hoc non-parametric test but can be VERY conservative

library(FSA) #package for dunnTest() function
dunnTest(n ~ trt, data=data, method="bh")
