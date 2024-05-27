## Diet groups GLMM: data exploration

# Hamilton et al, 2022. "Climate impacts alter fisheries productivity 
#  and turnover on coral reefs". Coral Reefs

library(lattice) 
library(tidyverse); theme_set(theme_classic())
library(sciplot)


diets <- read.csv("data/Fig4-GLMM-data.csv")
head(diets)


### Data exploration protocol from Zuur, Ieno & Elphick (2010) 
#   A protocol for data exploration to avoid common statistical problems.

# Functions from Highland Statistics:

#panel.cor
panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}

#Mypairs
Mypairs <- function(Z) {
  MyVarx <- colnames(Z)
  pairs(Z, labels = MyVarx,
        cex.labels =  2,
        lower.panel = function(x, y, digits=2, prefix="", cex.cor = 7) {
          panel.cor(x, y, digits, prefix, cex.cor)}, 
        upper.panel =  function(x, y) points(x, y, 
                                             pch = 16, cex = 0.8, 
                                             col = gray(0.1)))
  #print(P)
}



## 1) Outliers in Y and X:

# number of obs (locations) per year for each diet group:

table(diets$diet, as.factor(diets$year))  
#        1994 2005 2008 2011 2014 2017
# FisCep   21   21   21   21   21   18
# HerDet   21   21   21   21   21   18
# HerMac   21   21   21   21   21   18
# InvMob   21   21   21   21   21   18

# 18 sites in 2017 as 3 could not be surveyed.


# Paste reef state and management together into one variable:
diets$state.mgmt <- as.factor(paste(diets$state, diets$management, sep="_")) 


#Are categorical covariates balanced?
table(diets$location) # all 24 except shifted-protected (as 2017 removed)

table(diets$state)
# Recovering    Shifted 
#        288        204    

table(diets$management) # same as state
# Fished    Protected 
#    288          204 

table(diets$state.mgmt)
# Recovering_Fished    Recovering_Protected    Shifted_Fished    Shifted_Protected 
#               168                     120               120                   84 

table(diets$habitat)
# Carbonate   Granite 
#       328       164 


# Productivity dotcharts for each diet group:
dotchart(diets[which(diets$diet=="HerDet"),]$prod_yr_F0.2)

dotchart(diets[which(diets$diet=="HerMac"),]$prod_yr_F0.2)    
# lots of 0s. Possible outlier (>150)

dotchart(diets[which(diets$diet=="InvMob"),]$prod_yr_F0.2)    

dotchart(diets[which(diets$diet=="FisCep"),]$prod_yr_F0.2)  


# Dotcharts of explanatory variables:
dotchart(diets$depth)             
dotchart(diets$struc.complexity)
dotchart(diets$total.coral)  
dotchart(diets$total.macroalgae)   
# same variables for each diet group, so points appear repeated four times.

p <- ggplot(data = diets) + geom_point(aes(y =prod_yr_F0.2, x = year)) + 
  facet_grid(~state)
p


p <- ggplot(data = diets) + geom_point(aes(y =prod_yr_F0.2, x = year)) + 
  facet_grid(~management)
p


p <- ggplot(data=diets) + geom_point(aes(y=prod_yr_F0.2, x=year, group=state, 
                                         col=state), position=position_dodge(0.5)) 
p <- p + facet_grid(management~diet)
p <- p + theme(legend.position =c(0.1,0.9))       
p <- p + theme(strip.background = element_blank())
p  



## 2) Homogeneity of variance

boxplot(prod_yr_F0.2 ~ year, data = diets)  
boxplot(prod_yr_F0.2 ~ state, data = diets) 
boxplot(prod_yr_F0.2 ~ management, data = diets) 
boxplot(prod_yr_F0.2 ~ habitat, data = diets)   



# 3) Are data normally distributed

ggplot(data=diets[which(diets$diet=="HerDet"),], aes(x=log(prod_yr_F0.2+1))) +
        geom_histogram(alpha=0.6, position = 'identity', binwidth=0.25) +
        labs(fill="") + facet_wrap(~year)

ggplot(data=diets[which(diets$diet=="HerMac"),], aes(x=log(prod_yr_F0.2+1))) +
        geom_histogram(alpha=0.6, position = 'identity', binwidth=0.25) +
        labs(fill="") + facet_wrap(~year)
#lots of 0s, so right-skewed.

ggplot(data=diets[which(diets$diet=="InvMob"),], aes(x=log(prod_yr_F0.2+1))) +
        geom_histogram(alpha=0.6, position = 'identity', binwidth=0.25) +
        labs(fill="") + facet_wrap(~year)

ggplot(data=diets[which(diets$diet=="FisCep"),], aes(x=log(prod_yr_F0.2+1))) +
        geom_histogram(alpha=0.6, position = 'identity', binwidth=0.25) +
        labs(fill="") + facet_wrap(~year)



## 4) Zeros in the data

#Herbivore-detritivores
length(diets[which(diets$diet=="HerDet" & diets$prod_yr_F0.2==0),]$prod_yr_F0.2)
# 0

#Macroalgal browsers
length(diets[which(diets$diet=="HerMac" & diets$prod_yr_F0.2==0),]$prod_yr_F0.2)
# 34
length(diets[which(diets$diet=="HerMac"),]$prod_yr_F0.2)
# 123
34/123
# 27.6% sites are 0s

#Mobile invertivores
length(diets[which(diets$diet=="InvMob" & diets$prod_yr_F0.2==0),]$prod_yr_F0.2)
# 0

#Piscivores
length(diets[which(diets$diet=="FisCep" & diets$prod_yr_F0.2==0),]$prod_yr_F0.2)
# 2
length(diets[which(diets$diet=="FisCep"),]$prod_yr_F0.2)
# 123
2/123
# 1.6% sites are 0s



# 5) Collinearity among covariates

Mypairs(diets[,c("depthS","str.complexS","total.coralS","total.deadS","macroalgaeS","year")]) 



# 6) relationships between Y and X

# Depth
xyplot(log(prod_yr_F0.2+1) ~ depthS | diet, groups=state, 
       scales=list(y=list(relation="free")), 
       auto.key=T, data=diets)
# No clear patterns

# Structural complexity
xyplot(log(prod_yr_F0.2+1) ~ str.complexS | diet, groups=state,
       scales=list(y=list(relation="free")), 
       auto.key=T, data=diets)
# slightly higher values at higher SC, but no strong paterns

# Live coral cover
xyplot(log(prod_yr_F0.2+1) ~ total.coralS | diet, groups=state,
       scales=list(y=list(relation="free")), 
       auto.key=T, data=diets)
# slight decrease at higher coral cover for browsers.

# Dead coral cover
xyplot(log(prod_yr_F0.2+1) ~ total.deadS | diet, groups=state,
       scales=list(y=list(relation="free")), 
       auto.key=T, data=diets)
# no dead coral cover on a lot of regime-shifted sites

# Macroalgae cover
xyplot(log(prod_yr_F0.2+1) ~ macroalgaeS | diet, groups=state,
       scales=list(y=list(relation="free")), 
       auto.key=T, data=diets)
# no macroalgae on a lot of recovering coral sites


#Add response into Pairs plot:

Mypairs(diets[which(diets$diet=="HerDet"), c("prod_yr_F0.2","depthS","str.complexS","total.coralS","total.deadS","macroalgaeS","year")]) 
Mypairs(diets[which(diets$diet=="HerMac"), c("prod_yr_F0.2","depthS","str.complexS","total.coralS","total.deadS","macroalgaeS","year")])
Mypairs(diets[which(diets$diet=="InvMob"), c("prod_yr_F0.2","depthS","str.complexS","total.coralS","total.deadS","macroalgaeS","year")])
Mypairs(diets[which(diets$diet=="FisCep"), c("prod_yr_F0.2","depthS","str.complexS","total.coralS","total.deadS","macroalgaeS","year")])



# 7) Consider interactions

# year * state

coplot(log(prod_yr_F0.2+1) ~ depthS | year*state, 
       scales=list(y=list(relation="free")), auto.key=T, 
       data=diets[which(diets$diet=="HerDet"),])
# shifted sites are shallower (already knew that)

coplot(log(prod_yr_F0.2+1) ~ depthS | year*state, 
       scales=list(y=list(relation="free")), auto.key=T, 
       data=diets[which(diets$diet=="HerMac"),])

coplot(log(prod_yr_F0.2+1) ~ depthS | year*state, 
       scales=list(y=list(relation="free")), auto.key=T, 
       data=diets[which(diets$diet=="InvMob"),])

coplot(log(prod_yr_F0.2+1) ~ depthS | year*state, 
       scales=list(y=list(relation="free")), auto.key=T, 
       data=diets[which(diets$diet=="FisCep"),])


# state * management

coplot(log(prod_yr_F0.2+1) ~ depthS | state*management, 
       scales=list(y=list(relation="free")), auto.key=T,
       data=diets[which(diets$diet=="HerDet"),])

coplot(log(prod_yr_F0.2+1) ~ depthS | state*management, 
       scales=list(y=list(relation="free")), auto.key=T, 
       data=diets[which(diets$diet=="HerMac"),])

coplot(log(prod_yr_F0.2+1) ~ depthS | state*management, 
       scales=list(y=list(relation="free")), auto.key=T, 
       data=diets[which(diets$diet=="InvMob"),])

coplot(log(prod_yr_F0.2+1) ~ depthS | state*management, 
       scales=list(y=list(relation="free")), auto.key=T, 
       data=diets[which(diets$diet=="FisCep"),])

#No patterns showing, so no interaction terms needed in models.



# 8) Response observations independent?

# No, as repeated measurements at same sites in different years.
# Include random terms in models, for "location" (over time within each site).
# Also "year" (within year similarities)




##### END OF SCRIPT #####