# GLM - comparison of biomass, productivity and turnover between fish assemblages
#       on fished and protected reefs in 1994
# (data plotted in Fig 1)

# Hamilton et al, 2022. "Climate impacts alter fisheries productivity 
#  and turnover on coral reefs". Coral Reefs

library(tidyverse) 
library(lme4)
library(sjPlot)

theme_set(theme_classic())


## Read in data:
glm.data <- read.csv("data/whole-assemblage-1994-GLM.csv")
head(glm.data)


## 1994 data only (pre-bleaching):
glm94 <- glm.data[which(glm.data$Year==1994),]
head(glm94)


## Calculate net turnover:
glm94$net.turnF <- (glm94$prod_yr_F0.2 / glm94$biomass.kgha) *100


## Data exploration:

dotchart(glm94$biomass.kgha)
dotchart(glm94$prod_yr_F0.2)
dotchart(glm94$net.turnF)

boxplot(biomass.kgha ~ state, data = glm94)
boxplot(prod_yr_F0.2 ~ state, data = glm94) 
boxplot(net.turnF ~ state, data = glm94)
# these reef states didn't exist in 1994, but reefs that ended up shifting to 
# algal regime had slightly higher biomass, productivity & turnover.

boxplot(biomass.kgha ~ Management, data = glm94) 
boxplot(prod_yr_F0.2 ~ Management, data = glm94) 
boxplot(net.turnF ~ Management, data = glm94) 



### GLM: biomass, productivity and turnover, 1994



## BIOMASS

# log of response:
glm94$bio_log <- log(glm94$biomass.kgha + 1)

glm.bio <- glm(bio_log ~ Management, data=glm94)
summary(glm.bio)

# Model validation:
par(mfrow=c(2,2))
plot(glm.bio)

# Effect size:
plot_model(glm.bio, show.values=T)


## Plot residuals versus fitted values:
Eb <- resid(glm.bio, type="pearson")
Fb <- fitted(glm.bio, type="response")

plot(x = Fb, 
     y = Eb,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(0,0, lty = 2)  

#Influential observations
plot(cooks.distance(glm.bio),
     type = "h",
     ylim = c(0,1.1),
     cex.lab = 1.5)
abline(h=1)  

#Independence:
boxplot(Eb ~ Management, 
        data = glm94, 
        xlab = "Management",
        ylab = "Pearson residuals",
        cex.lab = 1.5)    

#Normality
hist(Eb,
     breaks = 50,
     cex.lab = 1.5,
     ylab ="",
     xlab ="Pearson residuals")



## PRODUCTIVITY

# log of response:
glm94$prod_log <- log(glm94$prod_yr_F0.2 + 1)

glm.prod <- glm(prod_log ~ Management, data=glm94)
summary(glm.prod)

# Model validation:
plot(glm.prod)

# Effect size:
plot_model(glm.prod, show.values=T)


## Plot residuals versus fitted values:
Ep <- resid(glm.prod, type="pearson")
Fp <- fitted(glm.prod, type="response")

plot(x = Fp, 
     y = Ep,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(0,0, lty = 2)  

#Influential observations
plot(cooks.distance(glm.prod),
     type = "h",
     ylim = c(0,1.1),
     cex.lab = 1.5)
abline(h=1)  

#Independence:
boxplot(Ep ~ Management, 
        data = glm94, 
        xlab = "Management",
        ylab = "Pearson residuals",
        cex.lab = 1.5)    

#Normality
hist(Ep,
     breaks = 50,
     cex.lab = 1.5,
     ylab ="",
     xlab ="Pearson residuals")



## TURNOVER

# log of response:
glm94$turn_log <- log(glm94$net.turnF + 1)

glm.turn <- glm(turn_log ~ Management, data=glm94)
summary(glm.turn)

# Model validation:
plot(glm.turn)

# Effect size:
plot_model(glm.turn, show.values=T)


## Plot residuals versus fitted values:
Et <- resid(glm.prod, type="pearson")
Ft <- fitted(glm.prod, type="response")

plot(x = Ft, 
     y = Et,
     xlab = "Fitted values",
     ylab = "Residuals",
     cex.lab = 1.5)
abline(0,0, lty = 2)  

#Influential observations
plot(cooks.distance(glm.turn),
     type = "h",
     ylim = c(0,1.1),
     cex.lab = 1.5)
abline(h=1)  

#Independence:
boxplot(Et ~ Management, 
        data = glm94, 
        xlab = "Management",
        ylab = "Pearson residuals",
        cex.lab = 1.5)    

#Normality
hist(Et,
     breaks = 50,
     cex.lab = 1.5,
     ylab ="",
     xlab ="Pearson residuals")




##### END OF SCRIPT #####