# Productivity length relationships for primary targets

library(tidyverse); theme_set(theme_classic())


# Read in daily productivity data for all possible cm length classes:
length_prod <- read.csv("data/Fig6-data_theoretical-lengths.csv", header=T)
head(length_prod)

# Read in daily productivity data for all possible cm length classes:


obs_lengths <- read.csv("data/Fig6-data_observed-lengths.csv")

C.s <- obs_lengths[which(obs_lengths$species=="Chlorurus sordidus"),]
S.r <- obs_lengths[which(obs_lengths$species=="Scarus rubroviolaceus"),]
S.s <- obs_lengths[which(obs_lengths$species=="Siganus sutor"),]


# Plot daily productivity (g/day) against total length (Length2):
p <- ggplot() +
  
  # gray lines for theoretical lengths (min to max)
  geom_line(data=length_prod, aes(x=Length, y=prod_day, group=species), size=0.75, colour="gray70") + 
  
  # colour lines for observed lengths
  geom_line(data = C.s, aes(x = Length2, y = prod_day, group=1), size=1.5, colour="#1B9E77") +
  geom_line(data = S.r, aes(x = Length2, y = prod_day, group=1), size=1.5, colour="#7570B3") + 
  geom_line(data = S.s, aes(x = Length2, y = prod_day, group=1), size=1.5, colour="#66A61E") +
  
  # Add dashed line at 0 g/day
  geom_segment(aes(x=8, y=0, xend=70, yend=0), linetype="longdash", color="black", size=0.5) +
  
  ylab(expression(Productivity~(g~day^-1))) + xlab("Total length (cm)") +
  theme(legend.position=c(0.2,0.8), legend.title=element_text(size=12), 
        legend.text=element_text(size=10), axis.text=element_text(size=11), 
        axis.title=element_text(size=14)) 
p

# Add species name labels:
p2 <- p +
  annotate(geom="text", x=28, y=0.1, label="Chlorurus sordidus", color="#1B9E77") +
  annotate(geom="text", x=37, y=0.54, label="Siganus sutor", color="#66A61E") +
  annotate(geom="text", x=29, y=1, label="Scarus rubroviolaceus", color="#7570B3")
p2



pdf(file="figures/Fig6_lngth-prod-relationship.pdf", height=5, width=5.5)
p2
dev.off()



##### END #####