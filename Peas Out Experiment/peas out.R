setwd("C:/Users/Myrmica/OneDrive - Washington State University (email.wsu.edu)/peas out")


install.packages(c("multcomp","multcompView","ggplot2","lsmeans","MuMIn","piecewiseSEM"))
install.packages("emmeans")
install.packages("piecewiseSEM", dependencies = TRUE)
install.packages("nloptr")

library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("MuMIn")
library("piecewiseSEM")
library("multcompView")
library("dplyr")


#data on pemv proportion infected
po.virus.dat <- read.csv("peas out pemv.csv", header=TRUE)
str(po.virus.dat)

#22 and 23 need to be fixed and are not in the analyses

#does predator treatment or plant treatment impact the proportion of infected plants?

treatment.glm <- glm(pemv.prop ~ Predator.Treatment*Plant.Treatment + Ladybug.pupae, weight = total.samples, family=binomial, data=po.virus.dat)
summary(treatment.glm)
Anova(treatment.glm)

glm.insects <- glm(pemv.prop ~ Clean + Moderate.Infestation + Heavy.Infestation + Tissue.Damage + Dead, family=binomial, weight=total.samples, data=po.virus.dat)
summary(glm.insects)

#did virus negatively impact yield?

glm.yield <- glm(pea.pods ~ pemv.prop + Dead + Predator.Treatment*Plant.Treatment, data=po.virus.dat)
summary(glm.yield)

#pods per plant
#pemv increases yield?
glm.pods <- glm(pods.per.plant ~ pemv.prop + Dead + Predator.Treatment*Plant.Treatment, data=po.virus.dat)
summary(glm.pods)

#is viral frequency density-dependent?
glm.density <- glm(pemv.prop ~ median.count, family=binomial, weight=total.samples, data=po.virus.dat)
summary(glm.density)

glm.aphid.treatment <- glm(Dead ~ Predator.Treatment*Plant.Treatment, data=po.virus.dat)
summary(glm.aphid.treatment)

#just plant infestation categorical counts from each dorm

aphid.dat <- read.csv("peas out.csv", header = TRUE)
str(aphid.dat)
aphid.end.dat <- aphid.dat %>%
  filter(End == "End")
aphid.end.dat



weevils <- read.csv("weevils.csv", header=TRUE)

weevils.glm <- lm(dry.weight ~ Plant.edge, data=weevils)
Anova(weevils.glm)




pupae <- read.csv("predators.csv", header=TRUE)
pupae

pupae.glm <- glm.nb(Ladybug.pupae ~ Predator.Treatment*Plant.Treatment, data=pupae)
summary(pupae.glm)
Anova(pupae.glm)



pupae.lsm <- (emmeans(pupae.glm, ~ Predator.Treatment*Plant.Treatment))
pupae.cld <- cld(pupae.lsm, sort=FALSE, adjust="none", type="response")

pupae.fig <- ggplot(pupae.cld, aes(x=Plant.Treatment, y=response, fill=Predator.Treatment)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE)), position=position_dodge(0.8), width=0.5) +
  theme_bw(base_size = 12) + 
  #geom_text(aes(x=Weevil.Treatment,y=emmean+2,label=tukey)) +
  #scale_x_discrete(limits=c("None","First","Second")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Number of Pupae", x="Plant Treatment") +
  scale_fill_grey()
  #scale_x_discrete(breaks=c("Sap-feeders", "No Sap-feeders"),
  #                 labels=c("+ Sap-feeders", "- Sap-feeders")) +
  #annotate("text", x=2, y=18.5, label=" italic(P)<0.001", parse=TRUE) +
  #annotate("text", x=1, y= 13, label=" italic(P)==0.292", parse=TRUE) +
  #theme(axis.line.x = element_line(color="black", size = 0.5),
  #      axis.line.y = element_line(color="black", size = 0.5)) +
  #geom_text(nudge_y=(1)) +
  #facet_wrap( ~ Alternative.Host, nrow=3)
pupae.fig

yield.glm <- glm.nb(pea.pods ~ Predator.Treatment*Plant.Treatment, data=pupae)
yield.lsm <- emmeans(yield.glm, ~Predator.Treatment*Plant.Treatment)
yield.cld <- cld(yield.lsm, sort=FALSE, adjust="none", type="response")
yield.cld




yield.fig <- ggplot(yield.cld, aes(x=Plant.Treatment, y=response, fill=Predator.Treatment)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE)), position=position_dodge(0.8), width=0.5) +
  theme_bw(base_size = 12) + 
  #geom_text(aes(x=Weevil.Treatment,y=emmean+2,label=tukey)) +
  #scale_x_discrete(limits=c("None","First","Second")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Number of Pea Pods", x="Plant Treatment") +
  scale_fill_grey()
#scale_x_discrete(breaks=c("Sap-feeders", "No Sap-feeders"),
#                 labels=c("+ Sap-feeders", "- Sap-feeders")) +
#annotate("text", x=2, y=18.5, label=" italic(P)<0.001", parse=TRUE) +
#annotate("text", x=1, y= 13, label=" italic(P)==0.292", parse=TRUE) +
#theme(axis.line.x = element_line(color="black", size = 0.5),
#      axis.line.y = element_line(color="black", size = 0.5)) +
#geom_text(nudge_y=(1)) +
#facet_wrap( ~ Alternative.Host, nrow=3)
yield.fig



















#model with score of infestation over time








aphid.dat$Duration <- as.factor(aphid.dat$Duration)
aphid.dat$Intervals <- as.factor(aphid.dat$Intervals)

score.glm <- glm.nb(Ratio ~ Intervals*Predator.Supplmented*Alternative.Host, data=aphid.dat)
summary(score.glm)
Anova(score.glm)

lady.lsm <- emmeans(score.glm, ~ Intervals*Predator.Supplmented|Alternative.Host, mult.name="Alternative.Host", adjust="none", sort=FALSE)
lady.cld <- cld(lady.lsm, sort=FALSE, adjust="none", type="response")
lady.cld

#ladybug based figure
lady2.fig <- ggplot(lady.cld, aes(x=Duration, y=response, col=Predator.Supplmented)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE)), position=position_dodge(0.8), width=0.5) +
  #theme_bw(base_size = 12) + 
  #geom_text(aes(x=Weevil.Treatment,y=emmean+2,label=tukey)) +
  #scale_x_discrete(limits=c("None","First","Second")) +
  #theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  #      panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Infestation Raw Score", x="Duration") + 
  #scale_fill_grey() +
  #scale_x_discrete(breaks=c("Sap-feeders", "No Sap-feeders"),
  #                 labels=c("+ Sap-feeders", "- Sap-feeders")) +
  #annotate("text", x=2, y=18.5, label=" italic(P)<0.001", parse=TRUE) +
  #annotate("text", x=1, y= 13, label=" italic(P)==0.292", parse=TRUE) +
  #theme(axis.line.x = element_line(color="black", size = 0.5),
  #      axis.line.y = element_line(color="black", size = 0.5)) +
  #geom_text(nudge_y=(1)) +
  facet_wrap( ~ Alternative.Host, nrow=3)
lady2.fig

duration.lady.plot <- ggplot(data=lady.cld, aes(x = Duration, y = response, col = Predator.Supplmented)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5, position=position_dodge(0.3)) +
  geom_errorbar(aes(ymin=response-SE, ymax=response+SE), width=0, size=1, position=position_dodge(0.3)) +
  geom_line(aes(group=Predator.Supplmented), size=1, position=position_dodge(0.3)) +
  #theme(legend.position=c(0.25,0.75)) +
  ylab("Infestation Score") +
  xlab("Duration (days)") +
  facet_wrap(~ Alternative.Host, nrow=3)
duration.lady.plot

host.lsm <- emmeans(score.glm, ~ Intervals*Alternative.Host, adjust="none", sort=FALSE)
host.cld <- cld(host.lsm, sort=FALSE, adjust="none", type="response")
host.cld


duration.plant.plot <- ggplot(data=host.cld, aes(x = Intervals, y = response, col = Alternative.Host)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=response-SE, ymax=response+SE), width=0, size=1) +
  #geom_line(aes(group=Alternative.Host), size=1) +
  #theme(legend.position=c(0.25,0.75)) +
  ylab("Infestation Score") +
  xlab("Duration (weeks)")
duration.plant.plot



######

#manova with raw values (not proportions)

#MANOVA
peasout.manova <- manova(cbind(Moderate.Infestation, Heavy.Infestation, Tissue.Damage, Dead) ~ Alternative.Host*Predator.Supplmented, data=aphid.dat)
summary(peasout.manova)
summary.aov(peasout.manova)

#estimated marginal means for the MANOVA

peasout.lsm <- emmeans(peasout.manova, ~ Alternative.Host*Predator.Supplmented|Health, mult.name = "Health", adjust="none")
peasout.lsm
peasout.cld <- cld(peasout.lsm, sort = FALSE, adjust="none", type="response")
peasout.cld

predator.lsm <- emmeans(peasout.manova, ~ Predator.Supplmented|Health, mult.name = "Health")
cld(predator.lsm, adjust="none")

host.lsm <- emmeans(peasout.manova, ~Alternative.Host|Health, mult.name = "Health")
host.cld <- cld(host.lsm, adjust="none")

host.cld$tukey <- as.numeric(host.cld$.group)
host.cld


######## MANOVA for aphid-plants on peas out experiment figure ######################

#example figure
host.fig <- ggplot(host.cld, aes(x=Alternative.Host, y=emmean, label=.group)) +
  geom_bar(stat="identity", width=0.8, position="dodge") +
  geom_errorbar(aes(ymin=emmean-(SE), ymax=emmean+(SE)), position=position_dodge(0.8), width=0.5) +
  theme_bw(base_size = 12) + 
  #geom_text(aes(x=Weevil.Treatment,y=emmean+2,label=tukey)) +
  #scale_x_discrete(limits=c("None","First","Second")) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Pea Plant Count", x="Alternative Plant Species in Dorm") + 
  scale_fill_grey() +
  #scale_x_discrete(breaks=c("Sap-feeders", "No Sap-feeders"),
  #                 labels=c("+ Sap-feeders", "- Sap-feeders")) +
  #annotate("text", x=2, y=18.5, label=" italic(P)<0.001", parse=TRUE) +
  #annotate("text", x=1, y= 13, label=" italic(P)==0.292", parse=TRUE) +
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  #geom_text(nudge_y=(1)) +
  facet_wrap( ~ Health, nrow=2)
host.fig


#MANOVA2
peasout.manova.2 <- manova(cbind(prop.mod,	prop.heavy,	prop.tiss,	prop.dead) ~ Alternative.Host*Predator.Supplmented, data=aphid.dat)
summary(peasout.manova.2)
summary.aov(peasout.manova.2)

