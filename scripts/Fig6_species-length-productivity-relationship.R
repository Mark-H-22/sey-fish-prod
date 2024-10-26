#######################################################################################
# productivity length relationship:

lengthprod <- read.csv("TargetSpecies_daily productivity.csv", header=T)
head(lengthprod)

#library(ggplot2); theme_set(theme_classic())

dayP <- ggplot(lengthprod) + aes(x=Length, y=prod_day, colour=species) + 
  geom_line(size=1, data=lengthprod, aes(group=species)) + 
  scale_color_manual(values=c("gray80","gray80","gray80")) + 
  theme(axis.title=element_text(size=14), axis.text=element_text(size=12), 
        panel.background=element_rect(fill="transparent"),legend.position="none") + 
  ylab(expression(Productivity~(g~day^-1))) + xlab("Total length (cm)")
dayP

C.s.gray <- lengthprod[which(lengthprod$species=="Chlorurus sordidus"),]
S.r.gray <- lengthprod[which(lengthprod$species=="Scarus rubroviolaceus"),]
S.s.gray <- lengthprod[which(lengthprod$species=="Siganus sutor"),]


head(sey.all)
table(sey.all$species)
C.s <- sey.all[which(sey.all$species=="Chlorurus sordidus"),]
S.g <- sey.all[which(sey.all$species=="Scarus ghobban"),]
S.r <- sey.all[which(sey.all$species=="Scarus rubroviolaceus"),]
S.s <- sey.all[which(sey.all$species=="Siganus sutor"),]


pl <- ggplot(S.s) + aes(x=Length2, y=prod_day) + geom_line(size=3) +
  labs(x = "Total length (cm)", y = expression(Productivity~(g~day^"-1"))) +
  theme(axis.text=element_text(size=11), axis.title=element_text(size=14))
pl



pl <- ggplot() + 
  geom_line(data = C.s.gray, aes(x = Length, y = prod_day, group=1, colour="C.sordidus.g"), size=1) +
  geom_line(data = C.s, aes(x = Length2, y = prod_day, group=1, colour="C.sordidus"), size=1.5) +
  geom_line(data = S.r, aes(x = Length2, y = prod_day, group=1, colour="Sc.rubroviolaceus"), size=1.5) + 
  geom_line(data = S.s, aes(x = Length2, y = prod_day, group=1, colour="Si.sutor"), size=1.5) + 
  ylab(expression(Productivity~(g~day^-1))) + xlab("Total length (cm)") +
  theme(legend.position=c(0.2,0.8),legend.title=element_text(size=12), legend.text=element_text(size=10)) +
  scale_color_manual(name="Species",values=c(C.sordidus="#1B9E77", Sc.rubroviolaceus="#7570B3", Si.sutor="#66A61E",
                                             C.sordidus.g="gray80"))

pl


p <- ggplot() +
  # gray lines
  geom_line(data=lengthprod, aes(x=Length, y=prod_day, group=species), size=0.75, colour="gray70") + 
  # colour lines
  geom_line(data = C.s, aes(x = Length2, y = prod_day, group=1), size=1.5, colour="#1B9E77") +
  geom_line(data = S.r, aes(x = Length2, y = prod_day, group=1), size=1.5, colour="#7570B3") + 
  geom_line(data = S.s, aes(x = Length2, y = prod_day, group=1), size=1.5, colour="#66A61E") +
  ylab(expression(Productivity~(g~day^-1))) + xlab("Total length (cm)") +
  theme(legend.position=c(0.2,0.8),legend.title=element_text(size=12), legend.text=element_text(size=10),
        axis.text=element_text(size=11), axis.title=element_text(size=14)) 
p

p2 <- p + geom_segment(aes(x=8, y=0, xend=70, yend=0), linetype="longdash", color="black", size=0.5) +
  annotate(geom="text", x=28, y=0.1, label="Chlorurus sordidus", color="#1B9E77") +
  annotate(geom="text", x=37, y=0.54, label="Siganus sutor", color="#66A61E") +
  annotate(geom="text", x=29, y=1, label="Scarus rubroviolaceus", color="#7570B3")
p2


# coloured lines at size of max Productivity  
#  geom_segment(aes(x=27, y=0, xend=27, yend=0.38), linetype="longdash", color="#1B9E77", size=0.5) + 
#  geom_segment(aes(x=47, y=0, xend=47, yend=1.029), linetype="longdash", color="#7570B3", size=0.5) +
#  geom_segment(aes(x=34, y=0, xend=34, yend=0.505), linetype="longdash", color="#66A61E", size=0.5)
#p2




pdf(file="Fig5_v2.pdf", height=5,width=5.5)
p2
dev.off()




library(fishualize)
fishapes()

pl + add_fishape(family="Labridae",option="Scarus_oviceps", 
                 xmin=9, xmax=13, ymin=0.01, ymax=0.05) +
  add_fishape(family="Labridae",option="Scarus_oviceps", 
              xmin=26, xmax=38, ymin=0.01, ymax=0.08) +
  add_fishape(family="Labridae",option="Scarus_oviceps", 
              xmin=20, xmax=31, ymin=0.19, ymax=0.24)

library(ggplot2)
fish1 <- ggplot() + aes(x=(1:3),y=(1:3)) +geom_point()
fish1
fish1 + add_fishape(family="Labridae",option="Chlorurus_sordidus", 
                    xmin=1.25, xmax=2.75, ymin=1.5, ymax=2.75) + theme_void()


