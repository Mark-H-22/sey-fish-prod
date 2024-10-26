## Figure 2: Productivity and turnover by diet groups (FISHED reefs)

# Hamilton et al, 2022. "Climate impacts alter fisheries productivity 
#  and turnover on coral reefs". Coral Reefs

library(tidyverse); theme_set(theme_classic())
library(sciplot)   #for calculating standard errors
library(devtools)
devtools::install_github("nschiett/fishualize", force = TRUE)
library(fishualize)
library(cowplot)

MyTheme <- theme(axis.title=element_text(size=16), axis.text=element_text(size=14),
                 legend.title=element_text(size=14), legend.text=element_text(size=12),
                 plot.title=element_text(face="bold",size=16,hjust=0.5),
                 panel.background=element_rect(fill="transparent"))


## Read in site-level data for fish diet groups:
diets <- read.csv("data/Fig3-data.csv")
head(diets)

## Aggregate to get means over all recovering & regime-shifted reefs:
diets2 <- data.frame(aggregate(diets[,c(6:10)], 
                               by=list(diets$state, diets$Management, diets$Year, 
                                       diets$Diet), mean, na.rm=T))
names(diets2)[1] <- "state"; names(diets2)[2] <- "Management"; names(diets2)[3] <- "Year"; names(diets2)[4] <- "Diet"
head(diets2)


## Change diet labels:
diets2$Diet <- as.factor(diets2$Diet)
levels(diets2$Diet)
#[1] "FisCep" "HerDet" "HerMac" "InvMob" "InvSes" "Omnivr" "Plktiv"
diets2$Diet <- factor(diets2$Diet, levels = c("HerMac", "HerDet", "InvMob", "FisCep"))
levels(diets2$Diet) <- c("Macroalgal Browser", "Herbivore-Detritivore", "Mobile Invertivore", "Piscivore")
head(diets2)



### NET PRODUCTIVITY

## Calculate SEM:
diets2$netP.F.SE <- aggregate(prod_yr_F0.2 ~ state + Management + Year + Diet, diets, se)$prod_yr_F0.2


## Add 1994 baselines for each diet group
View(diets2)
pis.94.p <- (diets2[1,"prod_yr_F0.2"] + diets2[2,"prod_yr_F0.2"])/2    # FisCep, fished

her.94.p <- (diets2[24,"prod_yr_F0.2"] + diets2[25,"prod_yr_F0.2"])/2  # HerDet, fished

bro.94.p <- (diets2[47,"prod_yr_F0.2"] + diets2[48,"prod_yr_F0.2"])/2  # HerMac, fished

inv.94.p <- (diets2[70,"prod_yr_F0.2"] + diets2[71,"prod_yr_F0.2"])/2  # InvMob, fished


## Available species in fishualize package (must have internet connection):
fishapes()



## Macroalgal Browsers

P.f.bro <- ggplot(diets2[which(diets2$Year!=1994 & diets2$Diet=="Macroalgal Browser" & diets2$Management=="Fished"),]) + 
            aes(x=Year, y=prod_yr_F0.2, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=(bro.94.p - (diets2[47,"netP.F.SE"]+diets2[48,"netP.F.SE"])/2), 
           ymax=(bro.94.p + (diets2[47,"netP.F.SE"]+diets2[48,"netP.F.SE"])/2), 
           fill="gray85",alpha=0.5) +
  geom_hline(data=subset(diets2, Diet=="Macroalgal Browser"), 
             aes(yintercept = (bro.94.p)), linetype=1, lwd=0.5, col="gray30") +
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + ylim(-5,90) +
  scale_linetype_manual(values=c("solid")) + 
  geom_point(size=2,position=position_dodge(0.4)) + 
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state:", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) +
  
  MyTheme + theme(legend.position=c(0.3,0.6)) +
  guides(linetype="none") + ylab(expression(Net~productivity~(kg~ha^-1~yr^-1)))

P.f.bro

P.f.bro2 <- P.f.bro + geom_errorbar(aes(ymin=prod_yr_F0.2-netP.F.SE, 
                                       ymax=prod_yr_F0.2+netP.F.SE), 
                                 width=0.9, size=0.6, position=position_dodge(0.4))
P.f.bro2

# Add fish silhouette + title:
P.f.bro3 <- P.f.bro2 + add_fishape(family = "Siganidae", option = "Siganus_sutor",
                      xmin = 2008, xmax = 2014, ymin = 77, ymax =86, fill="black") +
            geom_text(x=2011, y=90, label="Macroalgal Browser", size=5, color=1)
P.f.bro3



## Herbivore-Detritivores

P.f.her <- ggplot(diets2[which(diets2$Year!=1994 & diets2$Diet=="Herbivore-Detritivore" & diets2$Management=="Fished"),]) + 
  aes(x=Year, y=prod_yr_F0.2, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=(her.94.p - (diets2[24,"netP.F.SE"]+diets2[25,"netP.F.SE"])/2), 
           ymax=(her.94.p + (diets2[24,"netP.F.SE"]+diets2[25,"netP.F.SE"])/2), 
           fill="gray85",alpha=0.5) +
  geom_hline(data=subset(diets2, Diet=="Herbivore-Detritivore"), 
             aes(yintercept = (her.94.p)), linetype=1, lwd=0.5, col="gray30") +
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + ylim(20,130) +
  scale_linetype_manual(values=c("solid")) + 
  geom_point(size=2,position=position_dodge(0.4)) + 
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state:", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) +
  
  MyTheme + theme(legend.position="none", axis.title.y=element_blank()) +
  guides(linetype="none")

P.f.her

P.f.her2 <- P.f.her + geom_errorbar(aes(ymin=prod_yr_F0.2-netP.F.SE, 
                                        ymax=prod_yr_F0.2+netP.F.SE), 
                                    width=0.9, size=0.6, position=position_dodge(0.4))
P.f.her2

# Add fish silhouette + title:
P.f.her3 <- P.f.her2 + add_fishape(family = "Labridae", option = "Chlorurus_sordidus",
                      xmin = 2008, xmax = 2014, ymin=115, ymax=127, fill="black") +
            geom_text(x=2011, y=130, label="Herbivore-Detritivore", size=5, color=1)
P.f.her3



## Mobile Invertivores

P.f.inv <- ggplot(diets2[which(diets2$Year!=1994 & diets2$Diet=="Mobile Invertivore" & diets2$Management=="Fished"),]) + 
  aes(x=Year, y=prod_yr_F0.2, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=(inv.94.p - (diets2[70,"netP.F.SE"]+diets2[71,"netP.F.SE"])/2), 
           ymax=(inv.94.p + (diets2[70,"netP.F.SE"]+diets2[71,"netP.F.SE"])/2), 
           fill="gray85",alpha=0.5) +
  geom_hline(data=subset(diets2, Diet=="Mobile Invertivore"), 
             aes(yintercept = (inv.94.p)), linetype=1, lwd=0.5, col="gray30") +
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + ylim(15,75) +
  scale_linetype_manual(values=c("solid")) + 
  geom_point(size=2,position=position_dodge(0.4)) + 
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state:", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) +
  
  MyTheme + theme(legend.position="none", axis.title.y=element_blank()) +
  guides(linetype="none")

P.f.inv

P.f.inv2 <- P.f.inv + geom_errorbar(aes(ymin=prod_yr_F0.2-netP.F.SE, 
                                        ymax=prod_yr_F0.2+netP.F.SE), 
                                    width=0.9, size=0.6, position=position_dodge(0.4))
P.f.inv2

# Add fish silhouette + title:
P.f.inv3 <- P.f.inv2 + add_fishape(family = "Lethrinidae", option = "Lethrinus_nebulosus",
                      xmin = 2008, xmax = 2014, ymin=67, ymax=73.5, fill="black") +
            geom_text(x=2011, y=75, label="Mobile Invertivore", size=5, color=1)
P.f.inv3



## Piscivores

P.f.pis <- ggplot(diets2[which(diets2$Year!=1994 & diets2$Diet=="Piscivore" & diets2$Management=="Fished"),]) + 
  aes(x=Year, y=prod_yr_F0.2, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=(pis.94.p - (diets2[1,"netP.F.SE"]+diets2[2,"netP.F.SE"])/2), 
           ymax=(pis.94.p + (diets2[1,"netP.F.SE"]+diets2[2,"netP.F.SE"])/2), 
           fill="gray85",alpha=0.5) +
  geom_hline(data=subset(diets2, Diet=="Piscivore"), 
             aes(yintercept = (pis.94.p)), linetype=1, lwd=0.5, col="gray30") +
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + ylim(0,35) +
  scale_linetype_manual(values=c("solid")) + 
  geom_point(size=2,position=position_dodge(0.4)) + 
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state:", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) +
  
  MyTheme + theme(legend.position="none", axis.title.y=element_blank()) +
  guides(linetype="none")

P.f.pis

P.f.pis2 <- P.f.pis + geom_errorbar(aes(ymin=prod_yr_F0.2-netP.F.SE, 
                                        ymax=prod_yr_F0.2+netP.F.SE), 
                                    width=0.9, size=0.6, position=position_dodge(0.4))
P.f.pis2

# Add fish silhouette + title:
P.f.pis3 <- P.f.pis2 + add_fishape(family = "Serranidae", option = "Cephalopholis_miniata",
                      xmin = 2008, xmax = 2014, ymin = 30.5, ymax =34.5, fill="black") +
            geom_text(x=2011, y=35, label="Piscivore", size=5, color=1)
P.f.pis3




### NET TURNOVER

## Calculate SEM:
diets2$netT.F.SE <- aggregate(net.turnF ~ state + Management + Year + Diet, diets, se)$net.turnF


## Add 1994 baselines for each diet group:
pis.94.t <- (diets2[1,"net.turnF"]+diets2[2,"net.turnF"])/2    #FisCep, fished

her.94.t <- (diets2[24,"net.turnF"]+diets2[25,"net.turnF"])/2  #HerDet, fished

bro.94.t <- (diets2[47,"net.turnF"]+diets2[48,"net.turnF"])/2  #HerMac, fished

inv.94.t <- (diets2[70,"net.turnF"]+diets2[71,"net.turnF"])/2  #InvMob, fished


## Macroalgal browsers

T.f.bro <- ggplot(diets2[which(diets2$Year!=1994 & diets2$Diet=="Macroalgal Browser" & diets2$Management=="Fished"),]) + 
  aes(x=Year, y=net.turnF, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=(bro.94.t - (diets2[47,"netP.F.SE"]+diets2[48,"netP.F.SE"])/2), 
           ymax=(bro.94.t + (diets2[47,"netP.F.SE"]+diets2[48,"netP.F.SE"])/2), 
           fill="gray85",alpha=0.5) +
  geom_hline(data=subset(diets2, Diet=="Macroalgal Browser"), 
             aes(yintercept = (bro.94.t)), linetype=1, lwd=0.5, col="gray30") +
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + ylim(0,160) +
  scale_linetype_manual(values=c("solid")) + 
  geom_point(size=2,position=position_dodge(0.4)) + 
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state:", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) +
  
  MyTheme + theme(legend.position="none") +
  guides(linetype="none") + ylab("Net turnover (%)")

T.f.bro

T.f.bro2 <- T.f.bro + geom_errorbar(aes(ymin=net.turnF-netT.F.SE, 
                                        ymax=net.turnF+netT.F.SE), 
                                    width=0.9, size=0.6, position=position_dodge(0.4))
T.f.bro2


## Herbivore-Detritivores

T.f.her <- ggplot(diets2[which(diets2$Year!=1994 & diets2$Diet=="Herbivore-Detritivore" & diets2$Management=="Fished"),]) + 
  aes(x=Year, y=net.turnF, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=(her.94.t - (diets2[24,"netP.F.SE"]+diets2[25,"netP.F.SE"])/2), 
           ymax=(her.94.t + (diets2[24,"netP.F.SE"]+diets2[25,"netP.F.SE"])/2), 
           fill="gray85",alpha=0.5) +
  geom_hline(data=subset(diets2, Diet=="Herbivore-Detritivore"), 
             aes(yintercept = (her.94.t)), linetype=1, lwd=0.5, col="gray30") +
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + ylim(10,70) +
  scale_linetype_manual(values=c("solid")) + 
  geom_point(size=2,position=position_dodge(0.4)) + 
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state:", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) +
  
  MyTheme + theme(legend.position="none", axis.title.y=element_blank()) +
  guides(linetype="none")

T.f.her

T.f.her2 <- T.f.her + geom_errorbar(aes(ymin=net.turnF-netT.F.SE, 
                                        ymax=net.turnF+netT.F.SE), 
                                    width=0.9, size=0.6, position=position_dodge(0.4))
T.f.her2


## Invertivores

T.f.inv <- ggplot(diets2[which(diets2$Year!=1994 & diets2$Diet=="Mobile Invertivore" & diets2$Management=="Fished"),]) + 
  aes(x=Year, y=net.turnF, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=(inv.94.t - (diets2[70,"netP.F.SE"]+diets2[71,"netP.F.SE"])/2), 
           ymax=(inv.94.t + (diets2[70,"netP.F.SE"]+diets2[71,"netP.F.SE"])/2), 
           fill="gray85",alpha=0.5) +
  geom_hline(data=subset(diets2, Diet=="Mobile Invertivore"), 
             aes(yintercept = (inv.94.t)), linetype=1, lwd=0.5, col="gray30") +
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + ylim(25,60) +
  scale_linetype_manual(values=c("solid")) + 
  geom_point(size=2,position=position_dodge(0.4)) + 
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state:", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) +
  
  MyTheme + theme(legend.position="none", axis.title.y=element_blank()) +
  guides(linetype="none")

T.f.inv

T.f.inv2 <- T.f.inv + geom_errorbar(aes(ymin=net.turnF-netT.F.SE, 
                                        ymax=net.turnF+netT.F.SE), 
                                    width=0.9, size=0.6, position=position_dodge(0.4))
T.f.inv2


## Piscivores

T.f.pis <- ggplot(diets2[which(diets2$Year!=1994 & diets2$Diet=="Piscivore" & diets2$Management=="Fished"),]) + 
  aes(x=Year, y=net.turnF, colour=state) + 
  
  # 1994 estimate w/ shading for SEM:
  annotate("rect", xmin=-Inf, xmax=Inf, 
           ymin=(pis.94.t - (diets2[1,"netP.F.SE"]+diets2[2,"netP.F.SE"])/2), 
           ymax=(pis.94.t + (diets2[1,"netP.F.SE"]+diets2[2,"netP.F.SE"])/2), 
           fill="gray85",alpha=0.5) +
  geom_hline(data=subset(diets2, Diet=="Piscivore"), 
             aes(yintercept = (pis.94.t)), linetype=1, lwd=0.5, col="gray30") +
  
  # Post-1998 bleaching data:
  geom_line(size=1.25, aes(linetype=Management)) + ylim(15,115) +
  scale_linetype_manual(values=c("solid")) + 
  geom_point(size=2,position=position_dodge(0.4)) + 
  scale_color_manual(values=c("dodgerblue4", "red3"), 
                     name="Reef state:", labels=c("Recovering coral","Regime-shifted")) +
  scale_x_continuous(name=NULL,breaks=c(2005,2008,2011,2014,2017)) +
  
  MyTheme + theme(legend.position="none", axis.title.y=element_blank()) +
  guides(linetype="none")

T.f.pis

T.f.pis2 <- T.f.pis + geom_errorbar(aes(ymin=net.turnF-netT.F.SE, 
                                        ymax=net.turnF+netT.F.SE), 
                                    width=0.9, size=0.6, position=position_dodge(0.4))
T.f.pis2



## Arrange panels for final figure:

PT.bro <- plot_grid(P.f.bro3, T.f.bro2, nrow=2, align="v")
PT.bro

PT.her <- plot_grid(P.f.her3, T.f.her2, nrow=2, align="v")
PT.her

PT.inv <- plot_grid(P.f.inv3, T.f.inv2, nrow=2, align="v")
PT.inv

PT.pis <- plot_grid(P.f.pis3, T.f.pis2, nrow=2, align="v")
PT.pis

PT.all <- plot_grid(PT.bro, PT.her, PT.inv, PT.pis, nrow=1, align="h", rel_widths=c(1.15/4,1/4,1.05/4,1.05/4))
PT.all



##### END OF SCRIPT #####