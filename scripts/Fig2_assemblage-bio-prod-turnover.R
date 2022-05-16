#Figure 2: Biomass, productivity & turnover - Whole fish assemblage

# Hamilton et al, 2022. "Climate impacts alter fisheries productivity 
#  and turnover on coral reefs". Coral Reefs

library(tidyverse)
library(sciplot) #to calculate standard error of the mean
library(cowplot)

theme_set(theme_classic())

MyTheme <- theme(axis.title=element_text(size=16), axis.text=element_text(size=14),
                 legend.title=element_text(size=14), legend.text=element_text(size=12),
                 plot.title=element_text(face="bold",size=16,hjust=0.5),
                 panel.background=element_rect(fill="transparent"))

## Read in data - estimates for whole fish assemblages:

assemb <- read.csv("data/Fig2-data.csv")
head(assemb)


## Calculate net turnover to get location level estimates:
# (P / B) * 100

# without fishing mortality:
assemb$net.turn <- (assemb$prod_yr_kgha/assemb$biomass.kgha)*100

# with fishing mortality:
assemb$net.turnF <- (assemb$prod_yr_F0.2/assemb$biomass.kgha)*100



## Aggregate across reef states and management type:
assemb2 <- data.frame(aggregate(assemb[,c(5:9)], 
                          by=list(assemb$state, assemb$Management, assemb$Year), mean, na.rm=T))

names(assemb2)[1] <- "state"; names(assemb2)[2] <- "Management"; names(assemb2)[3] <- "Year" 
head(assemb2)



### BIOMASS

## Calculate SEM:
assemb2$bioSE <- aggregate(biomass.kgha ~ state + Management + Year, assemb, se)$biomass.kgha
#make sure terms are in same order as aggregate step above.


## Plot biomass on fished reefs:
b.fish <- ggplot(assemb2[which(assemb2$Year!=1994 & assemb2$Management=="Fished"),]) + 
  aes(x=Year, y=biomass.kgha, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=((assemb2[1,"biomass.kgha"]+assemb2[2,"biomass.kgha"])/2) - ((assemb2[1,"bioSE"]+assemb2[2,"bioSE"])/2), 
           ymax=((assemb2[1,"biomass.kgha"]+assemb2[2,"biomass.kgha"])/2) + ((assemb2[1,"bioSE"]+assemb2[2,"bioSE"])/2), 
           fill="gray85",alpha=0.8) +
  geom_hline(yintercept=(assemb2[1,"biomass.kgha"]+assemb2[2,"biomass.kgha"])/2, 
             col="gray30",lty=1,lwd=0.5) + 
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + 
  scale_linetype_manual(values=c("solid")) +
  geom_point(size=3, position=position_dodge(0.4)) +
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) + 
  
  ylim(180,920) +
  MyTheme +
  theme(legend.position=c(0.2,0.8),legend.box="vertical", 
        plot.title=element_text(face="bold",size=18,hjust=0.5)) + 
  ylab(expression(Biomass~(kg~ha^-1))) + ggtitle("Fished") + guides(linetype="none") 

b.fish

## Add SEM bars: 
b.fish <- b.fish + geom_errorbar(aes(ymin=biomass.kgha-bioSE, ymax=biomass.kgha+bioSE), 
                                 width=0.9, size=0.6, position=position_dodge(0.4)) 
b.fish



## Plot biomass on protected reefs:
b.prot <- ggplot(assemb2[which(assemb2$Year!=1994 & assemb2$Management=="Protected"),]) + 
  aes(x=Year, y=biomass.kgha, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=((assemb2[3,"biomass.kgha"]+assemb2[4,"biomass.kgha"])/2) - ((assemb2[3,"bioSE"]+assemb2[4,"bioSE"])/2), 
           ymax=((assemb2[3,"biomass.kgha"]+assemb2[4,"biomass.kgha"])/2) + ((assemb2[3,"bioSE"]+assemb2[4,"bioSE"])/2), 
           fill="gray85",alpha=0.8) +
  geom_hline(yintercept=(assemb2[3,"biomass.kgha"]+assemb2[4,"biomass.kgha"])/2, 
             col="gray30",lty=5,lwd=0.5) + 
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + 
  scale_linetype_manual(values=c("longdash")) +
  geom_point(size=3, position=position_dodge(0.4)) +
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) + 
  
  ylim(180,920) +
  MyTheme +
  theme(legend.position="none", plot.title=element_text(face="bold",size=18,hjust=0.5),
        axis.title.y=element_blank()) + 
  ggtitle("Protected") + guides(linetype="none") 

b.prot

## Add SEM bars: 
b.prot <- b.prot + geom_errorbar(aes(ymin=biomass.kgha-bioSE, ymax=biomass.kgha+bioSE), 
                                 width=0.9, size=0.6, position=position_dodge(0.4)) 
b.prot



### NET PRODUCTIVITY

## Calculate SEM:
assemb2$prod.yrF.SE <- aggregate(prod_yr_F0.2 ~ state + Management + Year, assemb, se)$prod_yr_F0.2


## Plot productivity on fished reefs:
p.fish <- ggplot(assemb2[which(assemb2$Year!=1994 & assemb2$Management=="Fished"),]) + 
  aes(x=Year, y=prod_yr_F0.2, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=((assemb2[1,"prod_yr_F0.2"]+assemb2[2,"prod_yr_F0.2"])/2) - ((assemb2[1,"prod.yrF.SE"]+assemb2[2,"prod.yrF.SE"])/2), 
           ymax=((assemb2[1,"prod_yr_F0.2"]+assemb2[2,"prod_yr_F0.2"])/2) + ((assemb2[1,"prod.yrF.SE"]+assemb2[2,"prod.yrF.SE"])/2), 
           fill="gray85",alpha=0.8) +
  geom_hline(yintercept=(assemb2[1,"prod_yr_F0.2"]+assemb2[2,"prod_yr_F0.2"])/2, 
             col="gray30",lty=1,lwd=0.5) + 
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + 
  scale_linetype_manual(values=c("solid")) +
  geom_point(size=3, position=position_dodge(0.4)) +
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) + 
  
  ylim(40,300) +
  MyTheme +
  theme(legend.position="none") + 
  ylab(expression(Net~productivity~(kg~ha^-1~yr^-1))) + guides(linetype="none") 

p.fish

## Add SEM bars: 
p.fish <- p.fish + geom_errorbar(aes(ymin=prod_yr_F0.2-prod.yrF.SE, ymax=prod_yr_F0.2+prod.yrF.SE), 
                                 width=0.9, size=0.6, position=position_dodge(0.4)) 
p.fish



## Plot productivity on protected reefs:
p.prot <- ggplot(assemb2[which(assemb2$Year!=1994 & assemb2$Management=="Protected"),]) + 
  aes(x=Year, y=prod_yr_F0.2, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=((assemb2[3,"prod_yr_F0.2"]+assemb2[4,"prod_yr_F0.2"])/2) - ((assemb2[3,"prod.yrF.SE"]+assemb2[4,"prod.yrF.SE"])/2), 
           ymax=((assemb2[3,"prod_yr_F0.2"]+assemb2[4,"prod_yr_F0.2"])/2) + ((assemb2[3,"prod.yrF.SE"]+assemb2[4,"prod.yrF.SE"])/2), 
           fill="gray85",alpha=0.8) +
  geom_hline(yintercept=(assemb2[3,"prod_yr_F0.2"]+assemb2[4,"prod_yr_F0.2"])/2, 
             col="gray30",lty=5,lwd=0.5) + 
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + 
  scale_linetype_manual(values=c("longdash")) +
  geom_point(size=3, position=position_dodge(0.4)) +
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) + 
  
  ylim(40,300) +
  MyTheme +
  theme(legend.position="none", axis.title.y=element_blank()) + 
  guides(linetype="none") 

p.prot

## Add SEM bars: 
p.prot <- p.prot + geom_errorbar(aes(ymin=prod_yr_F0.2-prod.yrF.SE, ymax=prod_yr_F0.2+prod.yrF.SE), 
                                 width=0.9, size=0.6, position=position_dodge(0.4)) 
p.prot



### NET TURNOVER

assemb2$netT.F.SE <- aggregate(net.turnF ~ state + Management + Year, assemb, se)$net.turnF


## Plot turnover on fished reefs:
t.fish <- ggplot(assemb2[which(assemb2$Year!=1994 & assemb2$Management=="Fished"),]) + 
  aes(x=Year, y=net.turnF, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=((assemb2[1,"net.turnF"]+assemb2[2,"net.turnF"])/2) - ((assemb2[1,"netT.F.SE"]+assemb2[2,"netT.F.SE"])/2), 
           ymax=((assemb2[1,"net.turnF"]+assemb2[2,"net.turnF"])/2) + ((assemb2[1,"netT.F.SE"]+assemb2[2,"netT.F.SE"])/2), 
           fill="gray85",alpha=0.8) +
  geom_hline(yintercept=(assemb2[1,"net.turnF"]+assemb2[2,"net.turnF"])/2, 
             col="gray30",lty=1,lwd=0.5) + 
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + 
  scale_linetype_manual(values=c("solid")) +
  geom_point(size=3, position=position_dodge(0.4)) +
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) + 
  
  ylim(20,55) +
  MyTheme +
  theme(legend.position="none") + 
  ylab("Net turnover (%)") + guides(linetype="none") 

t.fish

## Add SEM bars: 
t.fish <- t.fish + geom_errorbar(aes(ymin=net.turnF-netT.F.SE, ymax=net.turnF+netT.F.SE), 
                                 width=0.9, size=0.6, position=position_dodge(0.4)) 
t.fish


## Plot turnover on protected reefs:
t.prot <- ggplot(assemb2[which(assemb2$Year!=1994 & assemb2$Management=="Protected"),]) + 
  aes(x=Year, y=net.turnF, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=((assemb2[3,"net.turnF"]+assemb2[4,"net.turnF"])/2) - ((assemb2[3,"netT.F.SE"]+assemb2[4,"netT.F.SE"])/2), 
           ymax=((assemb2[3,"net.turnF"]+assemb2[4,"net.turnF"])/2) + ((assemb2[3,"netT.F.SE"]+assemb2[4,"netT.F.SE"])/2), 
           fill="gray85",alpha=0.8) +
  geom_hline(yintercept=(assemb2[3,"net.turnF"]+assemb2[4,"net.turnF"])/2, 
             col="gray30",lty=5,lwd=0.5) + 
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + 
  scale_linetype_manual(values=c("longdash")) +
  geom_point(size=3, position=position_dodge(0.4)) +
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) + 
  
  ylim(20,55) +
  MyTheme +
  theme(legend.position="none", axis.title.y=element_blank()) + 
  guides(linetype="none") 

t.prot

## Add SEM bars: 
t.prot <- t.prot + geom_errorbar(aes(ymin=net.turnF-netT.F.SE, ymax=net.turnF+netT.F.SE), 
                                 width=0.9, size=0.6, position=position_dodge(0.4)) 
t.prot




## Arrange panels into final figure:
ace <- plot_grid(b.fish, p.fish, t.fish, labels=c("a","c","e"), 
                 label_size=18, label_x=0.15, label_y=1, align="v", nrow=3)
ace


bdf <- plot_grid(b.prot, p.prot, t.prot, labels=c("b","d","f"), 
                 label_size=18, label_x=0.1, label_y=1, align="v", nrow=3)
bdf


final <- plot_grid(ace, bdf, align="h", rel_widths=c(5.4/10,5/10))
final



##### END OF SCRIPT #####
