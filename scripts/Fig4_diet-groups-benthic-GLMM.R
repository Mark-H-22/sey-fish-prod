# Diet groups GLMM: benthic variables and productivity


library(ggplot2); theme_set(theme_classic())
library(lme4)
library(sjPlot)
library(cowplot)


# read in data:
diets <- read.csv("data/Fig4-GLMM-data.csv")
head(diets)
str(diets)


diets$Location  <- factor(diets$Location)
diets$state     <- factor(diets$state)
diets$Management<- factor(diets$Management)
diets$Year      <- factor(diets$Year)
diets$Diet      <- factor(diets$Diet)
diets$habitat   <- factor(diets$habitat)



# Model per trophic group (data exploration in other script)



## Macroalgal browser

hermac <- diets[which(diets$Diet=="HerMac" & diets$Year!="1994"),]
dim(hermac)
head(hermac)


glm.hm <- lmer(prod_yr_F0.2 ~ str.complexS + depthS + total.coralS + total.deadS + 
                 macroalgaeS + Management + habitat + 
                 (1|Location) + (1|Year), data=hermac)

summary(glm.hm)

plot(glm.hm)
# Unequal variance - larger at higher fitted values. 
# Quite a lot of zeros (diagonal line)

length(hermac[which(hermac$prod_yr_F0.2==0),]$prod_yr_F0.2)  #25 zeros
length(hermac$prod_yr_F0.2) # 102   
25/102  #24.5% zeros


#Try log of response:
hermac$prod_F.log <- log(hermac$prod_yr_F0.2 + 1)

glm.hm2 <- lmer(prod_F.log ~ str.complexS + depthS + total.coralS + total.deadS +
                  macroalgaeS + Management + habitat + 
                  (1|Location) + (1|Year), data=hermac)
summary(glm.hm2)

plot(glm.hm2)  #better

# Plot residuals versus fitted values:
E2b <- resid(glm.hm2, type="pearson")
F2b <- fitted(glm.hm2, type="response")

#Normality
hist(E2b, 
     breaks = 50,
     cex.lab = 1.5,
     ylab ="",
     xlab ="Pearson residuals")


par(mfrow = c(3, 3), mar = c(5,5,2,2))

plot(x = F2b, 
     y = E2b,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(0,0, lty = 2)

#Influential observations
plot(cooks.distance(glm.hm2),
     type = "h",
     ylim = c(0,1.1),
     cex.lab = 1.5)
abline(h=1) 


#Independence:
plot(x = hermac[complete.cases(hermac),]$str.complexS, 
     y = E2b,
     xlab = "Structural complexity (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = hermac[complete.cases(hermac),]$depthS, 
     y = E2b,
     xlab = "Depth (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = hermac[complete.cases(hermac),]$total.coralS, 
     y = E2b,
     xlab = "Coral cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = hermac[complete.cases(hermac),]$total.deadS, 
     y = E2b,
     xlab = "Dead coral cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = hermac[complete.cases(hermac),]$macroalgaeS, 
     y = E2b,
     xlab = "Macroalgae cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

boxplot(E2b ~ Management, 
        data = hermac[complete.cases(hermac),], 
        xlab = "Management",
        ylab = "Pearson residuals",
        cex.lab = 1.5)

boxplot(E2b ~ habitat, 
        data = hermac[complete.cases(hermac),], 
        xlab = "Habitat",
        ylab = "Pearson residuals",
        cex.lab = 1.5)

boxplot(E2b ~ Year, 
        data = hermac[complete.cases(hermac),], 
        xlab = "Year",
        ylab = "Pearson residuals",
        cex.lab = 1.5)     #Nothing for 1994 as no dead coral data

boxplot(E2b ~ Location, 
        data = hermac[complete.cases(hermac),], 
        xlab = "Location",
        ylab = "Pearson residuals",
        cex.lab = 1.5)


par(mfrow = c(1, 1), mar = c(5,5,2,2))


acf(residuals(glm.hm2, retype="normalized"))
pacf(residuals(glm.hm2, retype="normalized"))


# plot figure panel
plot_model(glm.hm2)

hm.plot <- plot_model(glm.hm2, type="est", colors="tan4", show.values=T, 
                      line.size=1, dot.size=3, value.offset=0.25, 
                      title="Macroalgal browser", axis.lim=c(-1,1.5),
                      axis.labels=c("Habitat (granite)","Management (protected)", 
                         "Macroalgae cover","Dead coral cover","Live coral cover",
                         "Depth","Structural complexity"))
hm.plot

#use ggplot to set axis limits:
hm.plot2 <- plot_model(glm.hm2, type="est", colors="tan4", show.values=T, 
                      value.size=3, line.size=1, dot.size=3, value.offset=0.25, 
                      title="Macroalgal browser", vline.color="gray70",
                      axis.labels=c("Habitat (granite)","Management (protected)",
                                    "Macroalgae cover","Dead coral cover",
                                    "Live coral cover","Depth","Structural complexity")) + 
                      ylim(-1, 1.5) + font_size(labels.x=10, labels.y=11)
hm.plot2

##########



## Herbivore-detritivore:

herdet <- diets[which(diets$Diet=="HerDet" & diets$Year!="1994"),]

head(herdet)


# Log response:
herdet$prod_F.log <- log(herdet$prod_yr_F0.2 + 1)

glm.hd <- lmer(prod_F.log ~ str.complexS + depthS + total.coralS + total.deadS + 
                  macroalgaeS + Management + habitat + 
                  (1|Location) + (1|Year), data=herdet)
summary(glm.hd)  


plot(glm.hd)


# Plot residuals versus fitted values:
E2g <- resid(glm.hd, type="pearson")
F2g <- fitted(glm.hd, type="response")

#Normality
hist(E2g, 
     breaks = 50,
     cex.lab = 1.5,
     ylab ="",
     xlab ="Pearson residuals")


par(mfrow = c(3, 3), mar = c(5,5,2,2))

plot(x = F2g, 
     y = E2g,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(0,0, lty = 2)

#Influential observations
plot(cooks.distance(glm.hd),
     type = "h",
     ylim = c(0,1.1),
     cex.lab = 1.5)
abline(h=1) 


#Independence:
plot(x = herdet[complete.cases(herdet),]$str.complexS, 
     y = E2g,
     xlab = "Structural complexity (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = herdet[complete.cases(herdet),]$depthS, 
     y = E2g,
     xlab = "Depth (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = herdet[complete.cases(herdet),]$total.coralS, 
     y = E2g,
     xlab = "Coral cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = herdet[complete.cases(herdet),]$total.deadS, 
     y = E2g,
     xlab = "Dead coral cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = herdet[complete.cases(herdet),]$macroalgaeS, 
     y = E2g,
     xlab = "Macroalgae cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

boxplot(E2g ~ Management, 
        data = herdet[complete.cases(herdet),], 
        xlab = "Management",
        ylab = "Pearson residuals",
        cex.lab = 1.5)

boxplot(E2g ~ habitat, 
        data = herdet[complete.cases(herdet),], 
        xlab = "Habitat",
        ylab = "Pearson residuals",
        cex.lab = 1.5)

boxplot(E2g ~ Year, 
        data = herdet[complete.cases(herdet),], 
        xlab = "Year",
        ylab = "Pearson residuals",
        cex.lab = 1.5)     #Nothing for 1994 as no dead coral data

boxplot(E2g ~ Location, 
        data = herdet[complete.cases(herdet),], 
        xlab = "Location",
        ylab = "Pearson residuals",
        cex.lab = 1.5)


par(mfrow = c(1, 1), mar = c(5,5,2,2))


acf(residuals(glm.hd, retype="normalized"))
pacf(residuals(glm.hd, retype="normalized"))


# Plot figure panel
plot_model(glm.hd)

hd.plot <- plot_model(glm.hd, colors="forestgreen", show.values=T, value.size=3, 
                     line.size=1, dot.size=3, value.offset=0.3, 
                     title="Herbivore-detritivore", axis.lim=c(-0.5, 1), 
                     vline.color="gray70",
            axis.labels=c("Habitat (granite)","Management (protected)",
                         "Macroalgae cover","Dead coral cover","Live coral cover",
                         "Depth","Structural complexity")) +
            font_size(labels.x=10) + theme(axis.title.y=element_blank(), 
                                 axis.text.y=element_blank()) + ylim(-0.5, 1) 
hd.plot

##########




## Mobile invertivore

invmob <- diets[which(diets$Diet=="InvMob" & diets$Year!="1994"),]
head(invmob)


# log response
invmob$prod_F.log <- log(invmob$prod_yr_F0.2 + 1)

glm.im <- lmer(prod_F.log ~ str.complexS + depthS + total.coralS + total.deadS + 
                 macroalgaeS + Management + habitat + 
                 (1|Location) + (1|Year), data=invmob)

summary(glm.im) 


plot(glm.im)  

# Plot residuals versus fitted values:
Ei <- resid(glm.im, type="pearson")
Fi <- fitted(glm.im, type="response")

#Normality
hist(Ei, 
     breaks = 50,
     cex.lab = 1.5,
     ylab ="",
     xlab ="Pearson residuals")


par(mfrow = c(3, 3), mar = c(5,5,2,2))

plot(x = Fi, 
     y = Ei,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(0,0, lty = 2)

#Influential observations
plot(cooks.distance(glm.im),
     type = "h",
     ylim = c(0,1.1),
     cex.lab = 1.5)
abline(h=1) 


#Independence:
plot(x = invmob[complete.cases(invmob),]$str.complexS, 
     y = Ei,
     xlab = "Structural complexity (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = invmob[complete.cases(invmob),]$depthS, 
     y = Ei,
     xlab = "Depth (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = invmob[complete.cases(invmob),]$total.coralS, 
     y = Ei,
     xlab = "Coral cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = invmob[complete.cases(invmob),]$total.deadS, 
     y = Ei,
     xlab = "Dead coral cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = invmob[complete.cases(invmob),]$macroalgaeS, 
     y = Ei,
     xlab = "Macroalgae cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

boxplot(Ei ~ Management, 
        data = invmob[complete.cases(invmob),], 
        xlab = "Management",
        ylab = "Pearson residuals",
        cex.lab = 1.5)

boxplot(Ei ~ habitat, 
        data = invmob[complete.cases(invmob),], 
        xlab = "Habitat",
        ylab = "Pearson residuals",
        cex.lab = 1.5)

boxplot(Ei ~ Year, 
        data = invmob[complete.cases(invmob),], 
        xlab = "Year",
        ylab = "Pearson residuals",
        cex.lab = 1.5)     #Nothing for 1994 as no dead coral data

boxplot(Ei ~ Location, 
        data = invmob[complete.cases(invmob),], 
        xlab = "Location",
        ylab = "Pearson residuals",
        cex.lab = 1.5)



par(mfrow = c(1, 1), mar = c(5,5,2,2))

acf(residuals(glm.im, retype="normalized"))
pacf(residuals(glm.im, retype="normalized"))


# plot figure panel
plot_model(glm.im)

im.plot <- plot_model(glm.im, type="est", colors="darkorange", show.values=T, 
                     value.size=3, line.size=1, dot.size=3, value.offset=0.25, 
                     title="Mobile invertivore", vline.color="gray70",
           axis.labels=c("Habitat (granite)","Management (protected)",
                         "Macroalgae cover","Dead coral cover","Live coral cover",
                         "Depth","Structural complexity")) +
          ylim(-0.5, 1) + font_size(labels.x=10) + 
  theme(axis.title.y=element_blank(), axis.text.y=element_blank()) 

im.plot

##########




# Piscivore

fiscep <- diets[which(diets$Diet=="FisCep" & diets$Year!="1994"),]
head(fiscep)


#log response:
fiscep$prod_F.log <- log(fiscep$prod_yr_F0.2 + 1)

glm.fc <- lmer(prod_F.log ~ str.complexS + depthS + total.coralS + total.deadS + 
                 macroalgaeS + Management + habitat + 
                 (1|Location) + (1|Year), data=fiscep)
summary(glm.fc)


plot(glm.fc)  

# Plot residuals versus fitted values:
Ep <- resid(glm.fc, type="pearson")
Fp <- fitted(glm.fc, type="response")

#Normality
hist(Ep, 
     breaks = 50,
     cex.lab = 1.5,
     ylab ="",
     xlab ="Pearson residuals")


par(mfrow = c(3, 3), mar = c(5,5,2,2))

plot(x = Fp, 
     y = Ep,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(0,0, lty = 2)

#Influential observations
plot(cooks.distance(glm.fc),
     type = "h",
     ylim = c(0,1.1),
     cex.lab = 1.5)
abline(h=1) 


#Independence:
plot(x = fiscep[complete.cases(fiscep),]$str.complexS, 
     y = Ep,
     xlab = "Structural complexity (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = fiscep[complete.cases(fiscep),]$depthS, 
     y = Ep,
     xlab = "Depth (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = fiscep[complete.cases(fiscep),]$total.coralS, 
     y = Ep,
     xlab = "Coral cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = fiscep[complete.cases(fiscep),]$total.deadS, 
     y = Ep,
     xlab = "Dead coral cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

plot(x = fiscep[complete.cases(fiscep),]$macroalgaeS, 
     y = Ep,
     xlab = "Macroalgae cover (scaled)",
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(0,0, lty=2)

boxplot(Ep ~ Management, 
        data = fiscep[complete.cases(fiscep),], 
        xlab = "Management",
        ylab = "Pearson residuals",
        cex.lab = 1.5)

boxplot(Ep ~ habitat, 
        data = fiscep[complete.cases(fiscep),], 
        xlab = "Habitat",
        ylab = "Pearson residuals",
        cex.lab = 1.5)

boxplot(Ep ~ Year, 
        data = fiscep[complete.cases(fiscep),], 
        xlab = "Year",
        ylab = "Pearson residuals",
        cex.lab = 1.5)     #Nothing for 1994 as no dead coral data

boxplot(Ep ~ Location, 
        data = fiscep[complete.cases(fiscep),], 
        xlab = "Location",
        ylab = "Pearson residuals",
        cex.lab = 1.5)



par(mfrow = c(1, 1), mar = c(5,5,2,2))

acf(residuals(glm.fc, retype="normalized"))
pacf(residuals(glm.fc, retype="normalized"))


# plot figure panel
plot_model(glm.fc)

fc.plot <- plot_model(glm.fc, type="est", colors="indianred3", show.values=T, 
                      value.size=3, line.size=1, dot.size=3, value.offset=0.25, 
                      title="Piscivore",vline.color="gray70",
                      axis.labels=c("Habitat (granite)","Management (protected)",
                                    "Macroalgae cover","Dead coral cover",
                                    "Live coral cover","Depth","Structural complexity")) + 
                      ylim(-0.5, 1.1) + font_size(labels.x=10) + 
                      theme(axis.title.y=element_blank(),axis.text.y=element_blank())
fc.plot


########################################


# Arrange panels
plot_grid(hm.plot2, hd.plot, im.plot, fc.plot, nrow=1, 
          rel_widths=c(1.8/4, 1/4, 1/4, 1/4))


pdf(file="figures/Fig4_GLMM.pdf", height=6,width=10)
plot_grid(hm.plot2, hd.plot, im.plot, fc.plot, nrow=1, 
          rel_widths=c(1.8/4, 1/4, 1/4, 1/4))
dev.off() 



##### END #####