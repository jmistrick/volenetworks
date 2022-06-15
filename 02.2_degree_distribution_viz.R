# load packages
library(here)
library(tidyverse)
#load packages for plotting multiple plots together (the tidyverse parmfrow)
library(gridExtra) #for the grid.arrange() function
library(grid) #this is to add a title to the grid.arrange() complex o' graphs

#clear environment
rm(list = ls())

#Restore net_mets from the rdata file
net_mets_summary <- readRDS(file = "net_mets_summary_06.14.22.rds")


###########################################################################
################    Visualizing Degree Distributions    ###################
###########################################################################


###########################################################################
### 09_degree_freq_calculations was here - bring it back if you need it ###
###########################################################################


###########################################################################
############ violin / boxplots per site, color by trt, facet by month #####
###########################################################################


#degree distribution per site (group by trt) facet by month
month.labs <- as_labeller(c("may" = "May", "june" = "June", "july" = "July", "aug" = "Aug", "sept" = "Sept", "oct" = "Oct"))


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

#boxplot degree distribution per site, month
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
###########################################################################

#degree distribution per trt, month- not really helpful
# degree_summary %>% mutate(month = fct_relevel(month, "may", "june", "july", "aug", "sept", "oct")) %>%
#   ggplot(aes(x=trt, y=degree, fill=trt)) +
#   geom_boxplot() +
#   facet_wrap(~month)


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

##############################################################################################
########## ridgeline plots of degree - by site, color by treat, facet by month ###############
##############################################################################################


#5.31.22 for EEID poster: ridgeline plot of degree dist - binary degree
library(ggridges)
library(hrbrthemes)

# month.labs <- as_labeller(c("may" = "May", "june" = "June", "july" = "July", "aug" = "Aug", "sept" = "Sept", "oct" = "Oct"))

#ridgeline plot (density) of degree with each replicate site, color by trt, facet by month
# net_mets_summary %>%
#   mutate(site = fct_relevel(site, "janakkala", "puro", "talo",
#                             "kuoppa", "radio", "vaarinkorpi",
#                             "ketunpesa", "kiirastuli", "mustikka",
#                             "asema", "helmipollo", "hevonen")) %>%
#   ggplot( aes(y=site, x=deg,  fill=trt)) +
#   # stat_density_ridges(quantile_lines=TRUE,
#   #                     quantile_fun=function(x,...)median(x),
#   #                     alpha=0.6) +
#   geom_density_ridges(alpha=0.75, stat="binline", bins=15) +
#   #geom_density_ridges(alpha=0.6, bandwidth=1) +
#   facet_wrap(~month, labeller=month.labs) +
#   xlim(-1, 14) +
#   scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#CAB2D6", "#6A3D9A"),
#                     name = "Treatment",
#                     labels = c("Unfed - Control", "Unfed - Deworm", "Fed - Control", "Fed - Deworm")) +
#   theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
#         legend.position="none",
#         legend.text=element_text(size=9),
#         axis.title=element_text(size=15),
#         panel.spacing = unit(0.2, "lines"),
#         strip.text.x = element_text(size = 10)) +
#   labs(x="Number of Unique Spatial Overlaps", y="Replicate Population", fill="Treatment")

#to save, name the above as 'plot_degdistsummary <- '
# ggsave("degdisthist_by_sitemonth.png", plot=plot_degdistsummary, height=6, width=9.5, units=c("in"), dpi=600)


#ridgeline plot of weighted degree (density), each site separate
net_mets_summary %>%
  mutate(site = fct_relevel(site, "janakkala", "puro", "talo",
                            "kuoppa", "radio", "vaarinkorpi",
                            "ketunpesa", "kiirastuli", "mustikka",
                            "asema", "helmipollo", "hevonen")) %>%
  ggplot( aes(y=site, x=wt.deg,  fill=trt)) +
  # stat_density_ridges(quantile_lines=TRUE,
  #                     quantile_fun=function(x,...)median(x),
  #                     alpha=0.6) +
  geom_density_ridges(alpha=0.75, stat="binline", bins=55) +
  #geom_density_ridges(alpha=0.6, bandwidth=1) +
  facet_wrap(~month, labeller=month.labs) +
  xlim(-1, 54) + 
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

#ridgeline plot (density) of weighted degree, sites combined by trt
net_mets_summary %>%
  ggplot( aes(y=trt, x=wt.deg,  fill=trt)) +
  # stat_density_ridges(quantile_lines=TRUE,
  #                     quantile_fun=function(x,...)median(x),
  #                     alpha=0.6) +
  geom_density_ridges(alpha=0.75, stat="binline", bins=55) +
  #geom_density_ridges(alpha=0.6, bandwidth=1) +
  facet_wrap(~month, labeller=month.labs) +
  xlim(-1, max(net_mets_summary$wt.deg)) + #x axis should go to 60, but you can't see anything that way
  scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#CAB2D6", "#6A3D9A"),
                    name = "Treatment",
                    labels = c("Unfed - Control", "Unfed - Deworm", "Fed - Control", "Fed - Deworm")) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        legend.position="bottom",
        legend.text=element_text(size=9),
        axis.title=element_text(size=15),
        panel.spacing = unit(0.2, "lines"),
        strip.text.x = element_text(size = 10)) +
  labs(x="Number of Overlaps", y="Density", 
       fill="Treatment", title="Density Plots of Weighted Degree Distribution")


# #ridgeline plot of weighted degree, showing a single trt, facet by month
# net_mets_summary %>%
#   mutate(month = fct_relevel(month, "oct", "sept", "aug", "july", "june", "may")) %>%
#   ggplot( aes(y=month, x=wt.deg,  fill=trt)) +
#   # stat_density_ridges(quantile_lines=TRUE,
#   #                     quantile_fun=function(x,...)median(x),
#   #                     alpha=0.6) +
#   geom_density_ridges(alpha=0.75, stat="binline", bins=55) +
#   #geom_density_ridges(alpha=0.6, bandwidth=1) +
#   facet_wrap(~trt) +
#   xlim(-1, max(net_mets_summary$wt.deg)) + #x axis should go to 60, but you can't see anything that way
#   scale_fill_manual(values = c("#B2DF8A", "#33A02C", "#CAB2D6", "#6A3D9A"),
#                     name = "Treatment",
#                     labels = c("Unfed - Control", "Unfed - Deworm", "Fed - Control", "Fed - Deworm")) +
#   theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
#         legend.position="bottom",
#         legend.text=element_text(size=9),
#         axis.title=element_text(size=15),
#         panel.spacing = unit(0.2, "lines"),
#         strip.text.x = element_text(size = 10)) +
#   labs(x="Number of Overlaps", y="Density", 
#        fill="Treatment", title="Density Plots of Weighted Degree Distribution")


###################################################################################
##########   ridgeline plots of degree - with sex, resident status  ###############
###################################################################################

## ridgeline plots but with other measures
#by sex
net_mets_summary %>%
  drop_na(sex) %>%
  ggplot( aes(y=trt, x=wt.deg,  fill=sex)) +
  # stat_density_ridges(quantile_lines=TRUE,
  #                     quantile_fun=function(x,...)median(x),
  #                     alpha=0.6) +
  geom_density_ridges(alpha=0.5, stat="binline", bins=41, scale = 0.8) +
  #geom_density_ridges(alpha=0.6, bandwidth=1) +
  facet_wrap(~month, labeller=month.labs) +
  xlim(-1, 40) +
  theme(axis.text.y=element_text(), axis.ticks.y=element_blank(), 
        legend.position="bottom",
        legend.text=element_text(size=9),
        axis.title=element_text(size=15),
        panel.spacing = unit(0.2, "lines"),
        strip.text.x = element_text(size = 10)) +
  labs(x="Weighted Spatial Overlaps", y="Treatment", fill="Sex",
       title="Density plot of Weighted Degree by Sex")

#resident, non-resident
net_mets_summary %>%
  ggplot( aes(y=trt, x=wt.deg,  fill=res)) +
  # stat_density_ridges(quantile_lines=TRUE,
  #                     quantile_fun=function(x,...)median(x),
  #                     alpha=0.6) +
  geom_density_ridges(alpha=0.5, stat="binline", bins=41, scale = 0.8) +
  #geom_density_ridges(alpha=0.6, bandwidth=1) +
  facet_wrap(~month, labeller=month.labs) +
  xlim(-1, 40) +
  theme(axis.text.y=element_text(), axis.ticks.y=element_blank(), 
        legend.position="bottom",
        legend.text=element_text(size=9),
        axis.title=element_text(size=15),
        panel.spacing = unit(0.2, "lines"),
        strip.text.x = element_text(size = 10)) +
  labs(x="Weighted Spatial Overlaps", y="Treatment", fill="Sex",
       title="Density plot of Weighted Degree by Resident/Non-")
