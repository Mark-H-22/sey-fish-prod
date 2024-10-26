# Figure 5 - Net productivity and length frequency of primary target species over time

library(tidyverse); theme_set(theme_classic())
library(sciplot) 
library(RColorBrewer)
library(cowplot)

# Read in data for primary target species on fished reefs only:
df1 <- read.csv("data/Fig5-data_a-d.csv")
head(df1)
# productivity & net_turnover are estimates for a fishing mortality of 0.2


# Calculate means for each reef state per species per year:
df2 <- data.frame(aggregate(df1[,c(6:10)], 
                            by=list(df1$state, df1$Year, df1$species), 
                            mean, na.rm=T)) 
names(df2)[1:3] <- c("state","Year","species")
head(df2)  


#add in SE (aggregate order must be same as df2 !)
df2$netP.SE <- aggregate(prod_yr_F0.2 ~ state + Year + species, df1, se)$prod_yr_F0.2
df2$netT.SE <- aggregate(net_turnover_F0.2 ~ state + Year + species, df1, se)$net_turnover_F0.2


df2$species <- as.factor(df2$species)
levels(df2$species)
# "Aprion virescens"   "Cephalopholis leopardus"   "Chlorurus sordidus"    "Lutjanus bohar"         
# "Scarus ghobban"     "Scarus rubroviolaceus"     "Siganus sutor" 

# Display colours from RColorBrewer:
display.brewer.pal(n=8, name = 'Dark2')
brewer.pal(n=8, name = "Dark2")

# Assign colours to species based on order above:
sp.cols <- c("#666666","#D95F02","#1B9E77","#E7298A","#E6AB02","#7570B3","#66A61E")


# Make sure years are in the correct order:
df2$Year <- factor(df2$Year, levels=c("1994","2005","2008","2011","2014","2017"))


# Plot net productivity of primary targets over time, for each reef state separately:
Prim_netP <- ggplot(df2) + aes(x=Year, y=prod_yr_F0.2, colour=species) + 
  geom_point(size=2, position=position_dodge(0.25)) +  # points first for 1994
  geom_line(data=df2  %>%filter(Year!=1994), size=1.5, aes(group=species)) + # lines for other years
  facet_wrap(~state, nrow=1) +
  scale_color_manual(values=sp.cols, name="Target species") + 
  scale_x_discrete(name="Y",breaks=c(1994,2005,2008,2011,2014,2017)) +
  theme(axis.title.y=element_text(size=14), axis.title.x=element_text(size=14, colour="transparent"),
        axis.text.x=element_text(size=11), axis.text.y=element_text(size=11),
        panel.background=element_rect(fill="transparent"), legend.position=c(0.15,0.7)) + 
  ylab(expression(Net~productivity~(kg~ha^-1~yr^-1))) 

Prim_netP

# Plot net turnover of primary targets over time, for each reef state separately:
Prim_netT <- ggplot(df2) + aes(x=Year, y=net_turnover_F0.2, colour=species) +
  geom_point(size=2, position=position_dodge(0.25)) + 
  geom_line(data=df2  %>%filter(Year!=1994), size=1.5, aes(group=species)) + 
  facet_wrap(~state, nrow=1) +
  scale_color_manual(values=sp.cols, name="Target species") + 
  scale_x_discrete(name="Y",breaks=c(1994,2005,2008,2011,2014,2017)) +
  theme(axis.title.y=element_text(size=14), axis.title.x=element_text(size=14, colour="transparent"),
        axis.text.x=element_text(size=11), axis.text.y=element_text(size=11),
        panel.background=element_rect(fill="transparent"), legend.position="none") + 
  ylab("Net turnover (%)") 
Prim_netT

##### Fig S4 (in supplementary) = Prim_netP + Prim_netT

#pdf(file="figures/FigS4_Primary-targets_prod_turnover.pdf", height=9, width=11)
plot_grid(Prim_netP, Prim_netT, nrow=2)
#dev.off()




##### Create Fig 5 - 2 most productive species only on each reef state
# (Chlorurus sordidus & Scarus rubroviolaceus on recovering reefs,
# C. sordidus & Siganus sutor on regime-shifted reefs)

focus.sp <- c("Chlorurus sordidus","Scarus rubroviolaceus","Siganus sutor")

df3 <- df2[which(df2$species %in% focus.sp),]
df3$species <- factor(df3$species, levels = c("Chlorurus sordidus","Scarus rubroviolaceus","Siganus sutor"))

# Assign colours to species in order above:
sp.cols2 <- c("#1B9E77","#7570B3","#66A61E")


# Make sure years are in the correct order:
df3$Year <- factor(df3$Year, levels=c("1994","2005","2008","2011","2014","2017"))


# Plot net productivity for fished recovering reefs:
F_recov <- ggplot(df3[which(df3$state=="Recovering" & df3$species!="Siganus sutor"),]) + 
  aes(x=Year, y=prod_yr_F0.2, colour=species) + 
  geom_point(size=3, position=position_dodge(0.25)) +
  geom_line(size=1.5, data=df3[which(df3$state=="Recovering" & df3$species!="Siganus sutor"),] 
            %>%filter(Year!=1994), aes(group=species)) + 
  scale_color_manual(values=c("#1B9E77","#7570B3"), name="Recovering coral:") + 
  scale_linetype_manual(values=c("solid")) +
  scale_x_discrete(name="Y",breaks=c(1994,2005,2008,2011,2014,2017)) + ylim(-5,50) +
  theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14, colour="transparent"),
        axis.text.x=element_text(size=12), axis.text.y=element_text(size=12),
        panel.background=element_rect(fill="transparent"),legend.position=c(0.27,0.85),
        legend.title=element_text(size=12), legend.text=element_text(size=10)) + 
  ylab(expression(Net~productivity~(kg~ha^-1~yr^-1))) 

F_recov
# Add standard error bars:
F_recov2 <- F_recov + geom_errorbar(aes(ymin=prod_yr_F0.2-netP.SE, ymax=prod_yr_F0.2+netP.SE), 
                                    width=0.3, size=0.5, position=position_dodge(0.25)) 
F_recov2


# Plot net productivity for fished regime-shifted reefs:
F_shift <- ggplot(df3[which(df3$state=="Shifted" & df3$species!="Scarus rubroviolaceus"),]) + 
  aes(x=Year, y=prod_yr_F0.2, colour=species) + 
  geom_line(size=1.5, data=df3[which(df3$state=="Shifted" & df3$species!="Scarus rubroviolaceus"),]
            %>%filter(Year!=1994), aes(group=species)) + 
  geom_point(size=3, position=position_dodge(0.1)) +
  scale_color_manual(values=c("#1B9E77","#66A61E"), name="Regime-shifted:") + 
  scale_x_discrete(name="Y",breaks=c(1994,2005,2008,2011,2014,2017)) + ylim(-5,50) +
  theme(axis.title.y=element_text(size=14),axis.title.x=element_text(size=14, colour="transparent"),
        axis.text.x=element_text(size=12), axis.text.y=element_text(size=12),
        panel.background=element_rect(fill="transparent"),legend.position=c(0.25,0.85),
        legend.title=element_text(size=12), legend.text=element_text(size=10)) + 
  ylab(expression(Net~productivity~(kg~ha^-1~yr^-1))) 

F_shift
# Add standard error bars:
F_shift2 <- F_shift + geom_errorbar(aes(ymin=prod_yr_F0.2-netP.SE, ymax=prod_yr_F0.2+netP.SE), 
                                    width=0.3, size=0.5, position=position_dodge(0.1)) 
F_shift2





##### Add length-frequency plots for species on each reef state

# Read in abundance data per size class (cm) for primary target species:

lngth_freq <- read.csv("data/Fig5-data_b-c-e-f.csv")
head(lngth_freq)



### Chlorurus sordidus

C.s <- lngth_freq[which(lngth_freq$species=="Chlorurus sordidus"),]

summary(C.s[which(C.s$abundance.ha>0),]$Length2)
#max 40
#maxTL=40


# Length-frequency plot for recovering reefs:
cs_rec_lf <- ggplot(data=C.s[which(C.s$state!="Shifted"),], aes(x=Length2, y=abundance.ha)) + 
  geom_col(fill="#1B9E77",width=2) + facet_wrap(~Year, nrow=1) +
  labs(x = NULL, y = expression(Abundance~(ha^"-1"))) + xlim(8,45) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
cs_rec_lf

# Get total abundance per year (across all lengths):
sum(C.s[which(C.s$Year=="1994" & C.s$state=="Recovering"),]$abundance.ha)  # 89
sum(C.s[which(C.s$Year=="2005" & C.s$state=="Recovering"),]$abundance.ha)  # 43
sum(C.s[which(C.s$Year=="2008" & C.s$state=="Recovering"),]$abundance.ha)  # 59
sum(C.s[which(C.s$Year=="2011" & C.s$state=="Recovering"),]$abundance.ha)  # 77
sum(C.s[which(C.s$Year=="2014" & C.s$state=="Recovering"),]$abundance.ha)  # 73
sum(C.s[which(C.s$Year=="2017" & C.s$state=="Recovering"),]$abundance.ha)  # 154

# Create labels for each year:
cs_rec_text <- data.frame(label = c("n = 89","n = 43","n = 59","n = 77","n = 73","n = 154"),
                       Year = c(1994, 2005, 2008, 2011, 2014, 2017))

# Plot:
cs_rec_lf2 <- cs_rec_lf + geom_text(data=cs_rec_text, size=3.5,
                              mapping=aes(x = 38, y = 17, label = label))
cs_rec_lf2


# Length-frequency plot for regime-shifted reefs:
cs_shi_lf <- ggplot(data=C.s[which(C.s$state=="Shifted"),], aes(x=Length2, y=abundance.ha)) + 
  geom_col(fill="#1B9E77",width=2) + facet_wrap(~Year, nrow=1) +
  labs(x = "Total length (cm)", y = expression(Abundance~(ha^"-1"))) + xlim(8,45) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
cs_shi_lf

sum(C.s[which(C.s$Year=="1994" & C.s$state=="Shifted"),]$abundance.ha)  # 95
sum(C.s[which(C.s$Year=="2005" & C.s$state=="Shifted"),]$abundance.ha)  # 62
sum(C.s[which(C.s$Year=="2008" & C.s$state=="Shifted"),]$abundance.ha)  # 88
sum(C.s[which(C.s$Year=="2011" & C.s$state=="Shifted"),]$abundance.ha)  # 48
sum(C.s[which(C.s$Year=="2014" & C.s$state=="Shifted"),]$abundance.ha)  # 38
sum(C.s[which(C.s$Year=="2017" & C.s$state=="Shifted"),]$abundance.ha)  # 38

cs_shi_text <- data.frame(label = c("n = 95","n = 62","n = 88","n = 48","n = 38","n = 38"),
                       Year = c(1994, 2005, 2008, 2011, 2014, 2017))
cs_shi_lf2 <- cs_shi_lf + geom_text(data=cs_shi_text, size=3.5,
                                    mapping=aes(x = 38, y = 16, label = label))
cs_shi_lf2



### Scarus rubroviolaceus

S.r <- lngth_freq[which(lngth_freq$species=="Scarus rubroviolaceus"),]

summary(S.r[which(S.r$abundance.ha>0),]$Length2)
#max 62
#maxTL=70


sr_rec_lf <- ggplot(data=S.r[which(S.r$state=="Recovering"),], aes(x=Length2, y=abundance.ha)) + 
  geom_col(fill="#7570B3", width=2) + facet_wrap(~Year, nrow=1) +
  labs(x = "Total length (cm)", y = expression(Abundance~(ha^"-1"))) + xlim(8,65) +
  theme(axis.text=element_text(size=12), axis.title.y=element_text(size=14), 
        axis.title.x=element_text(size=14,colour="transparent"))
sr_rec_lf

sum(S.r[which(S.r$Year=="1994" & S.r$state=="Recovering"),]$abundance.ha)  # 6
sum(S.r[which(S.r$Year=="2005" & S.r$state=="Recovering"),]$abundance.ha)  # 10
sum(S.r[which(S.r$Year=="2008" & S.r$state=="Recovering"),]$abundance.ha)  # 26
sum(S.r[which(S.r$Year=="2011" & S.r$state=="Recovering"),]$abundance.ha)  # 30
sum(S.r[which(S.r$Year=="2014" & S.r$state=="Recovering"),]$abundance.ha)  # 15
sum(S.r[which(S.r$Year=="2017" & S.r$state=="Recovering"),]$abundance.ha)  # 34

sr_rec_text <- data.frame(label = c("n = 6","n = 10","n = 26","n = 30","n = 15","n = 34"),
                       Year = c(1994, 2005, 2008, 2011, 2014, 2017))
sr_rec_lf2 <- sr_rec_lf + geom_text(data=sr_rec_text, size=3.5,
                              mapping=aes(x = 55, y = 4, label = label))
sr_rec_lf2



### Siganus sutor

S.s <- lngth_freq[which(lngth_freq$species=="Siganus sutor"),]

summary(S.s[which(S.s$abundance.ha>0),]$Length2)
#max 32
#maxTL=50


ss_shi_lf <- ggplot(data=S.s[which(S.s$state=="Shifted"),], aes(x=Length2, y=abundance.ha)) + 
  geom_col(fill="#66A61E", width=2) + facet_wrap(~Year, nrow=1) +
  labs(x =NULL, y = expression(Abundance~(ha^"-1"))) + xlim(8,40) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
ss_shi_lf

sum(S.s[which(S.s$Year=="1994" & S.s$state=="Shifted"),]$abundance.ha)  # 3
sum(S.s[which(S.s$Year=="2005" & S.s$state=="Shifted"),]$abundance.ha)  # 0
sum(S.s[which(S.s$Year=="2008" & S.s$state=="Shifted"),]$abundance.ha)  # 11
sum(S.s[which(S.s$Year=="2011" & S.s$state=="Shifted"),]$abundance.ha)  # 7
sum(S.s[which(S.s$Year=="2014" & S.s$state=="Shifted"),]$abundance.ha)  # 43
sum(S.s[which(S.s$Year=="2017" & S.s$state=="Shifted"),]$abundance.ha)  # 15

ss_shi_text <- data.frame(label = c("n = 3","n = 0","n = 11","n = 7","n = 43","n = 15"),
                       Year = c(1994, 2005, 2008, 2011, 2014, 2017))
ss_shi_lf2 <- ss_shi_lf + geom_text(data=ss_shi_text,
                              mapping=aes(x = 34, y = 19.5, label = label), size=3.5)
ss_shi_lf2



# Arrange all panels for Fig 5

# length-freq panels for recovering reefs:
LF_rec <- plot_grid(cs_rec_lf2, sr_rec_lf2, nrow=2, labels=c("b","c"), 
                    align="v", rel_heights = c(1/2, 1.2/2))
LF_rec

# length-freq panels for regime-shifted reefs:
LF_shi <- plot_grid(ss_shi_lf2, cs_shi_lf2, nrow=2, labels=c("e","f"), 
                    align="v", rel_heights = c(1/2, 1.2/2))
LF_shi

# Combine productivity trends with length freq plots:
rec <- plot_grid(F_recov2, LF_rec, nrow=1, labels=c("a"), rel_widths = c(1.1/3, 2/3))
rec

shi <- plot_grid(F_shift2, LF_shi, nrow=1, labels=c("d"), rel_widths = c(1.1/3, 2/3))
shi



all <- plot_grid(rec, shi, align="v", nrow=2)
all

pdf(file="figures/Fig5_primary-targets.pdf", height=9,width=12)
all
dev.off()



##### END #####