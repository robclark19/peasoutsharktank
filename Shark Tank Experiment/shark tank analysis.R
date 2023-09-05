#setwd("C:/Users/Myrmica/Desktop/side projects/shark tank")

shark.dat <- read.csv("shark tank.csv", header=TRUE)
str(shark.dat)

#cumulative foraging time model

shark.forage.dat <- shark.dat %>%
  filter(response.type == "ladybug foraging") 

str(shark.forage.dat)
hist(shark.forage.dat$total.events)

# did ladybug foraging effort vary between plant species and infection status?
# was variation in ladybug foraging effort amoung plant species contingent on infection status?

ladybug.glm <- glmer(total.events ~ host.species*pemv.treatment + log(start.adults) + (1|observer), family=poisson, data=shark.forage.dat)
summary(ladybug.glm)
Anova(ladybug.glm)

#do pairwise comparison
#
#
#
#
ladybug.cld <- cld(lsmeans(ladybug.glm, ~host.species, type="response"), adjust="none")
ladybug.cld

shark.plot <- ggplot(data=ladybug.cld, aes(x = host.species, y = rate)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=rate-(SE), ymax=rate+(SE), width=0)) +
  #geom_line(aes(group=PEMV), size=1) +
  theme(legend.position=c(0.25,0.75)) +
  ylab("Cumulative minutes foraging on plant") +
  xlab("Host-plant species")
  #ylim(c(0,150))
shark.plot

shark.2.plot <- ggplot(data=ladybug.cld, aes(x = host.species, y = rate, shape = pemv.treatment)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=rate-(SE), ymax=rate+(SE), width=0)) +
  #geom_line(aes(group=PEMV), size=1) +
  theme(legend.position=c(0.25,0.75)) +
  ylab("Cumulative minutes foraging on plant") +
  xlab("Host-plant species") +
  labs(shape="Virus (PEMV)")
#ylim(c(0,150))
shark.2.plot

# to do:
# new figure on virus and plant response to herbivores

# figure 2a

# figure 2b


### effects on predation of aphids ###
#### effect of plants and virus on ladybird beetle predation of aphids (change in aphid abundance over 30 minutes)

shark.forage.dat <- shark.dat %>%
  filter(response.type == "ladybug foraging") 
str(shark.forage.dat)

# was rate of predation dependent on host plant species or presence of virus?
# was variation in predation rates among host plant species mediated by virus? Did virus increase or reduce risk?

delta.glm <- glmer(deleta.nymphs ~ host.species*pemv.treatment + start.adults + (1|observer), family=poisson, data=shark.dat)

summary(delta.glm)
Anova(delta.glm)

delta.cld <- cld(lsmeans(delta.glm, ~host.species*pemv.treatment, type="response"), adjust="tukey")
delta.cld


delta.plot <- ggplot(data=delta.cld, aes(x = host.species, y = rate, shape = pemv.treatment)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=rate-(SE), ymax=rate+(SE), width=0)) +
  #geom_line(aes(group=PEMV), size=1) +
  theme(legend.position=c(0.25,0.75)) +
  ylab("Change in aphid abundance due to predation") +
  xlab("Host-plant species") +
  labs(shape="Virus (PEMV)")
#ylim(c(0,150))
delta.plot

#number of observed feeding events

shark.feed.dat <- shark.dat %>%
  filter(response.type == "feeding event") 
library(nlme)
predation.glm <- glm(total.events ~ host.species*pemv.treatment + start.adults, family=poisson, data=shark.feed.dat)
summary(predation.glm)
Anova(predation.glm)
#make sure singular doesnt bias the model


predation.cld <- cld(lsmeans(predation.glm, ~ pemv.treatment, type="response"))
predation.cld

predation.plot <- ggplot(data=predation.cld, aes(x = pemv.treatment, y = rate)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=rate-(SE), ymax=rate+(SE), width=0)) +
  #geom_line(aes(group=PEMV), size=1) +
  theme(legend.position=c(0.25,0.75)) +
  ylab("Total number of ladybug attacks on prey") +
  xlab("Host-plant species") +
  labs(shape="Virus (PEMV)")
#ylim(c(0,150))
predation.plot



#number of observed dispersal events
shark.disperse.dat <- shark.dat %>%
  filter(response.type == "aphid dispersal")

dispersal.glm <- glm.nb(total.events ~ host.species*pemv.treatment + start.adults, data=shark.disperse.dat)
summary(dispersal.glm)
Anova(dispersal.glm)
hist(shark.disperse.dat$total.events)

dispersal.cld <- cld(lsmeans(dispersal.glm, ~ host.species*pemv.treatment, type="response"), adjust="none")
dispersal.cld

plant.cld <- cld(lsmeans(dispersal.glm, ~ host.species, type="response"))
plant.cld
 
virus.cld <- cld(lsmeans(dispersal.glm, ~ pemv.treatment, type="response"))
virus.cld

dispersal.plot <- ggplot(data=dispersal.cld, aes(x = host.species, y = response, shape = pemv.treatment)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  theme(legend.position=c(0.25,0.75)) +
  ylab("Cumulative aphid dispersal events") +
  xlab("Host-plant species") +
  labs(shape="Virus (PEMV)")
dispersal.plot

test.glm <- glm(start.adults ~ host.species + pemv.treatment, family=poisson, data=shark.disperse.dat)
Anova(test.glm)

