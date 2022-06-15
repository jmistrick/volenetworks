#Figures -- based on net_mets_summary (02_trapping_networks)

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

#to view figure with common colorblind interpretations
library(colorBlindness)
cvdPlot(plot1)

### scale_fill_brewer(palette="Paired") #Set2 and Paired are both colorblind-friendly
library(RColorBrewer)  
brewer.pal(12, "Paired") #https://gist.github.com/jtoll/4021792
display.brewer.pal(12, "Paired")
