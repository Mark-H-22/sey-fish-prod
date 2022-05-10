# Figure 1: Seychelles coral & macroalgae cover over time

# Hamilton et al, 2022. "Climate impacts alter fisheries productivity 
#  and turnover on coral reefs". Coral Reefs

library(tidyverse)
library(sciplot)
library(cowplot)

theme_set(theme_classic())

MyTheme <- theme(axis.title=element_text(size=16), axis.text=element_text(size=14),
      legend.title=element_text(size=14), legend.text=element_text(size=12),
      plot.title=element_text(face="bold",size=16,hjust=0.5),
      panel.background=element_rect(fill="transparent"))

## Read in benthic data:
benthic <- read.csv("data/Fig1-data.csv")
benthic


## Plot coral data between reef states:

coral <- ggplot(benthic, aes(x=factor(Year), y=total.coral, fill=state)) +
  geom_boxplot(outlier.colour="gray50", colour="black") +
  ylab("Coral cover (%)") + xlab(" ") + 
  scale_fill_manual(values=c("dodgerblue4", "red3"), name="Reef state", 
                    labels=c("Recovering coral","Regime-shifted")) +
  theme(legend.position=c(0.85,0.9)) + MyTheme
coral


## Plot macroalgae data between reef states:

algae <- ggplot(benthic, aes(x=factor(Year), y=total.macroalgae, fill=state)) +
  geom_boxplot(outlier.colour="gray50", colour="black") +
  ylab("Macroalgae cover (%)") + xlab(" ") + 
  scale_fill_manual(values=c("dodgerblue4", "red3"), name="Reef state", 
                    labels=c("Recovering coral","Regime-shifted")) +
  theme(legend.position="none") + MyTheme
algae


## Join panels into one plot:

cor.alg <- plot_grid(coral, algae, nrow=1, labels=c("a","b"), 
                     label_size=18, label_x=0.15, label_y=1)
cor.alg


##### END OF SCRIPT #####
