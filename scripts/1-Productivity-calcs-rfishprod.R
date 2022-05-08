#Productivity calculations

# Hamilton et al, 2022. "Climate impacts alter fisheries productivity 
#  and turnover on coral reefs". Coral Reefs

library(devtools)
devtools::install_github("renatoamorais/rfishprod")
library(rfishprod)
library(Rlab)
library(plyr)
library(dplyr)

## Load species trait data (more details in online supplementary info):
Sey.trait <- read.csv("data/species-traits_Seychelles.csv")
head(Sey.trait)


## Load UVC fish data:

#example dataset from Cousin Carbonate site in 1994:
Sey.fish <- read.csv("data/fish-data_Seychelles-94-Cousin-Carbonate.csv")
head(Sey.fish)


## Merge survey & trait data:
Sey.prod <- merge(Sey.fish, Sey.trait, by=c("species"), all.x=T)
head(Sey.prod)


## Check if any individual fish lengths are larger or equal to MaxSizeTL:
Sey.prod[Sey.prod$Length>Sey.prod$MaxSizeTL,]

Sey.prod[Sey.prod$Length==Sey.prod$MaxSizeTL,]

# change these individuals to MaxSizeTL - 0.1cm:
Sey.prod$Length2 <- ifelse(Sey.prod$Length>=Sey.prod$MaxSizeTL, Sey.prod$MaxSizeTL-0.1, Sey.prod$Length)
head(Sey.prod)
#Now use Length2 vector for further analyses

#--------------------------------------------------


## Use rfishprod package to calculate productivity per individual fish

?rfishprod

?tidytrait
Sey.prod <- tidytrait (Sey.prod, Sey.trait) 


#from Morais & Bellwood, 2018. "Global drivers of reef fish growth". Fish & Fisheries:

levels(db$Method)
#using "Otolth" for all species/individuals

# model for predicting Kmax values:
fmod <- formula (~ sstmean + MaxSizeTL + Diet + Position + Method)


## Predicting Kmax, the standardised Von Bertalanffy Growth Function (VBGF) parameter. 
#  (Recommendation: use 100s to 1000s of bootstrap iterations) 
?predKmax

datagr <- predKmax (Sey.prod, 
                    dataset = db,
                    fmod = fmod,
                    niter = 1000,
                    return = 'pred')

datagr <- datagr$pred



## Position your fish on their growth trajectory:
# i.e. what size should they be the next day? 
?applyVBGF

datagr$L.1day <- with (datagr, applyVBGF (Lmeas = Length2,
                         Lmax = MaxSizeTL,
                         Kmax = Kmax))

head(datagr)
# each fish should have grown a small amount.



## Calculate age estimates:
datagr$EstAge <- (1/datagr$Kmax)*log((datagr$MaxSizeTL)/((1-datagr$Length2/datagr$MaxSizeTL)*datagr$MaxSizeTL))
summary(datagr$EstAge)


## Calculate growth with VBGF, using ages:

#Age to add for each day of the year:
age <- (1:365)/365

#Create table for new length: 1 column per day, 1 row per individual fish
VB_lngth <-  matrix(ncol=length(age), nrow=nrow(datagr), dimnames=list(NULL, paste("Day", 1:365, sep="_")))


## For each individual, calculate new length for each day using VBGF formula
#  VBGF:  Lt = Lmax*(1-exp(-K*t))
#  t = Estimated age + time interval

for(u in 1:nrow(datagr)) {
  VB_lngth[u, ] <- datagr$MaxSizeTL[u]*(1-exp(-datagr$Kmax[u]*(datagr$EstAge[u] + age)))
}

VB_lngth

# Now have a matrix of lengths for each day of the year per fish


## Convert lengths to weights using a & b coefficents:
VB_wt <-  apply(VB_lngth, 2, function(x) datagr$a*(x^datagr$b))   
head(VB_wt)
nrow(VB_wt)
# units= grams (per fish)


## Predicting M: the instantaneous mortality rate
?predM
datagr$Md <- with (datagr,
                   predM (Lmeas = Length2,
                          t = 1,   
                          Lmax = MaxSizeTL,
                          Kmax = Kmax,
                          method = 'Gislason'))  
datagr
#Md changes with individual length.




## Calculate probability of survival

# probability of survival = exp(-Mday)        (does not scale with age)

datagr$SurvivalDay <- exp(-datagr$Md)
head(datagr)

#Make a table of daily survival probabilities for the whole year:
prob.surv_tab <- matrix(ncol=365, nrow=nrow(datagr), dimnames=list(NULL, paste("Day", 1:365)))

for(u in 1:nrow(datagr)) {
 prob.surv_tab [u, ] <- exp(-datagr$Md[u] * ((datagr$EstAge[u]+age)/(datagr$EstAge[u]+1)))
 }
View(prob.surv_tab)


prob.surv_cum.tab <- prob.surv_tab

for (u in 2:ncol(prob.surv_cum.tab))
  for (v in 1:nrow(prob.surv_cum.tab)) {
    prob.surv_cum.tab[v,u] <- prob.surv_cum.tab[v,u]*prob.surv_cum.tab[v,(u-1)]
  }

View(prob.surv_cum.tab)
# Probability of survival DECREASES through time, because this is now CUMULATIVE survival probability.



## Simulate stochastic mortality of each fish per day:
# (Bernoulli trials: run multiple times and take the mean)


## 1) SINGLE ITERATION OF SURVIVAL
surv.outcome_day <- matrix(ncol=365, nrow=nrow(prob.surv_tab))

for (u in 1:nrow(prob.surv_tab))
  for (v in 1:ncol(prob.surv_tab)) {
    surv.outcome_day[u,v] <- rbern(1,prob=prob.surv_tab[u,v])
  }

View(surv.outcome_day)
# 1 = survives to next day, 0 = fish dies   
# This is one iteration only. Iterated version further down...


# create function: once a 0 appears in a row, all subsequent values are 0 (i.e. fish stays dead!)
zero_row_replace <- function(x) {
  x[which(cumany(x==0))] <- 0
  return(x)
}

# apply function:
surv.outcome_day2 <- adply(surv.outcome_day, 1, zero_row_replace)
View(surv.outcome_day2)

# Remove first column, X1:
surv.outcome_day2 <- surv.outcome_day2[,-1]


## calculate fish mass from length using formula: W = a * L^b
head(datagr)

biomass.g <- (datagr$a * datagr$Length2^datagr$b)

# add mass as first column (Day 0) in matrix:
VB_wt2 <- cbind(biomass.g, VB_wt)
head(VB_wt2)
colnames(VB_wt2)[1] <- "Day_0"

View(VB_wt2)


## Convert fish masses to mass gained per day (productivity) by subtracting previous day:

#create new object:
VB_prod_wt <- VB_wt2

for(u in 2:ncol(VB_prod_wt))
  for(v in 1:nrow(VB_prod_wt)) {
    VB_prod_wt[v,u] <- VB_wt2[v,u] - VB_wt2[v,(u-1)]
  }

View(VB_prod_wt)

#Delete first column:
VB_prod_wt2 <- VB_prod_wt[,2:ncol(VB_prod_wt)]
View(VB_prod_wt2) 
# Now have a table of daily production in g/individual over a year (not cumulative)


## Multiply survival table by production table to get production estimates:

# (check matrices are the same size:)
dim(VB_prod_wt2)       # 525  365
dim(surv.outcome_day2) # 525  365

est_prod_day <- surv.outcome_day2*VB_prod_wt2
View(est_prod_day)  
# All days where fish did not survive have 0 productivity


# Now sum daily production over the course of a year:
prod_yr <- rowSums(est_prod_day)
# 1 value per row (grams produced over 1 year)

prod_day <- est_prod_day$V1    #value on day one      
# 1 value per row (i.e. grams produced after 1 day)

# Combine daily and annual productivity  estimates:
prod.per.fish <- cbind(prod_day,prod_yr)
View(prod.per.fish)



## 2) MULTIPLE ITERATIONS OF SURVIVAL

prob.surv_tab
y<-matrix(ncol=1, nrow= nrow(prob.surv_tab))

for (x in 1:100){   # 100 iterations
  
  surv_outcome_stoc <- matrix (ncol=365, nrow=nrow(prob.surv_tab))
  
  for (u in 1:nrow(prob.surv_tab))
    for (v in 1:ncol(prob.surv_tab)) {
      surv_outcome_stoc [u,v ] <- rbern(1,prob= prob.surv_tab[u,v])
    }
  
  surv_outcome_stoc2<-adply(surv_outcome_stoc, 1, zero_row_replace)
  surv_outcome_stoc2<-select(surv_outcome_stoc2, -c(X1))
  
  est_daily_prod_tab<-surv_outcome_stoc2*VB_prod_wt2
  
  prod_year<-rowSums(est_daily_prod_tab)
  
  y   <- cbind(y,prod_year)
}

head(y)
#annual productivity per individual (each column = separate 1 year simulation)

# (remove first column of NAs)
y <- y[,-1]
head(y)

## get mean per fish across iterations:
mean.prod <- data.frame(prod_yr=rowSums(y)/100)
head(mean.prod)

## Combine with daily estimates from above
prod.per.fish2 <- cbind(prod_day,mean.prod)
View(prod.per.fish2)

#------------------------------------------------------------

# Combine "datagr" dataset with individual productivity estimates:

final.prod <- cbind(datagr, prod.per.fish2)

head(final.prod)

