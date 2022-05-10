# Fishing simulation functions

# Hamilton et al, 2022. "Climate impacts alter fisheries productivity 
#  and turnover on coral reefs". Coral Reefs


library(tidyverse)
require(scales)


## Load UVC fish data:

#example dataset from Cousin Carbonate site in 1994:
Sey.fish <- read.csv("data/fish-productivity-estimates.csv")
head(Sey.fish)


## Load size at first capture for targeted species: 
# (from Graham et al, 2007. "Lag effects in the impacts of mass coral bleaching
# on coral reef fish, fisheries, and ecosystems". Conservation Biology)
size1stC <- read.csv("data/target-species_Seychelles-UVC.csv", na.strings=c(""," ","NA"))
head(size1stC)


## Merge UVC and size at first capture data:
seyF <- merge(Sey.fish, size1stC, by=c("species"), all.x=TRUE)
head(seyF)


## Round length estimates up to whole cm:
seyF$Length2 <- round(seyF$Length2)


## Subset to get only target species:
sey.targ <- seyF[which(seyF$target!="NA"),]


#-------------------------------------------------

# Fishing probability distribution function: 
# (from Morais et al, 2020. "Human exploitation shapes productivity-biomass
# relationships on coral reefs". Global Change Biology)

fpdmpg <- function (x, x0 = 36, k = 0.025, L = 1, b = 6, propBL = 1.25) {   
  B <- propBL * L
  a <- (B + ((L - B) * exp (exp (-k * ((x0 / 2) - x0))))) / (x0 /2) ^ b
  rescale (ifelse (x > (x0 / 2),
                   B + ((L - B) * exp (exp (-k * (x - x0)))),
                   a * (x ^ b)), c (0, L))
}

# target size = size at which fish are considered fishable = 18cm (estimate for our study)
# set xtar at 2 * target size = 36cm


fpd <- fpdmpg(sey.targ$Length2)
# Values are the fishing susceptibility. Multiply by a capture rate to get
# fishing mortality (F) values.

plot(fpd ~ Length2, data=sey.targ, xlab="Length (cm)", ylab="Fishing susceptibility")
abline(v=18, lty=1, lwd=0.5)


## Add values into dataset:
sey.targ$fpd <- fpd
head(sey.targ)

#extract only Length2 and fpd columns:
L.fpd <- sey.targ[,c("Length2","fpd")]
head(L.fpd)

#reorder by ascending Length2
L.fpd2 <- arrange(L.fpd, Length2)
head(L.fpd2)

# remove duplicate length values to leave only one row per length
L.fpd3 <- L.fpd2[!duplicated(L.fpd2$Length2),]
L.fpd3


## Can now merge value with other data if required

# e.g.:   merge(mydata, L.fpd3, by=c("Length2"), all.x=TRUE)


## Example, using capture rate of 0.2:

# mydata$F_0.2 <- mydata$fpd * 0.2

plot((fpd*0.2) ~ Length2, data=L.fpd3, xlab="Length (cm)", ylab="Fishing mortality")
# length-dependent F values for capture rate of 0.2


## Use F values to remove fish biomass (in addition to natural mortality) 

#e.g.: 
sey.targ$F_0.2 <- sey.targ$fpd * 0.2

sey.targ$prod_yr_F0.2 <- sey.targ$prod_yr - (sey.targ$prod_yr * sey.targ$F_0.2)
head(sey.targ)

# Natural mortality (M) losses were stochastic during productivity calculations,
# Fishing mortality (F) losses are applied afterwards, here.

##### END OF SCRIPT #####