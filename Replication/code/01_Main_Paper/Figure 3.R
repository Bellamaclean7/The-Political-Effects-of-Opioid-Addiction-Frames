#####################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Main paper
## Figure 3
## Steps: regression models, calculate treatment effects, create figure
#####################
rm(list=ls()) ## clear list space

#####################
### Load packages ########
#####################

library(ggplot2) ## used to create figure
library(gridExtra) ## used to print figure
library(glm.predict) ## used for "predicts" function
library(gtools) ## used for "smartbind" function


#####################
### Load Data ########
#####################

## read in data
data <- read.csv("tessdata.csv", head = T) ## 1517 obs

#####################################################
## Eq 2: Racial Sympathy hypothesis
#####################################################

###############
## DV 1: policy 
###############
eq2.policy.controls <- glm(policy ~ sb 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$sw == 1| data$sb == 1, ])
# summary(eq2.policy.controls)



## sympathetic black
test <- predicts(eq2.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

sb.policy.eff <- -1*test$dc_mean

sb.policy.upper <- -1*test$dc_lower

sb.policy.lower <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq2.candidate.controls <- lm(candidatesupport ~ sb  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data[data$sw == 1| data$sb == 1, ])
# summary(eq2.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq2.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

sb.candidate.eff <- candidate.estimates[2]

sb.candidate.lower <- candidate.estimates[2] - 1.96*candidate.se[2]

sb.candidate.upper <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq2.taxes.controls <- lm(taxesfortreatment ~ sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$sw == 1| data$sb == 1, ])
# summary(eq2.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq2.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

sb.taxes.eff <- taxes.estimates[2]

sb.taxes.lower <- taxes.estimates[2] - 1.96*taxes.se[2]

sb.taxes.upper <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq2.emotions.controls <- lm(emotionscale ~ sb 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data[ data$sw == 1| data$sb == 1, ])
# summary(eq2.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq2.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

sb.emotions.eff <- emotions.estimates[2]

sb.emotions.lower <- emotions.estimates[2] - 1.96*emotions.se[2]

sb.emotions.upper <- emotions.estimates[2] + 1.96*emotions.se[2]



data.test1 <- data.frame(Outcome = c("Support treatment policy", 
                                     "Support treatment candidate" , 
                                     "Taxes for treatment", 
                                     "Sympathetic emotional response"),
                         treat = c("Symp: Black - White"),
                         effect = c(sb.policy.eff, 
                                    sb.candidate.eff, 
                                    sb.taxes.eff,
                                    sb.emotions.eff),
                         lower = c(sb.policy.lower, 
                                   sb.candidate.lower, 
                                   sb.taxes.lower,
                                   sb.emotions.lower),
                         upper = c(sb.policy.upper,
                                   sb.candidate.upper,
                                   sb.taxes.upper, 
                                   sb.emotions.upper))

# data.test1

#####################################################
## Eq 4 - Racial Antipathy Hypothesis
#####################################################

###############
## DV 1: policy 
###############
## logit so we need predicted probabilities

eq4.policy.controls <- glm(policy ~ ub 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$uw == 1| data$ub == 1, ])
# summary(eq4.policy.controls)



## sympathetic black
test <- predicts(eq4.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

ub.policy.eff <- -1*test$dc_mean

ub.policy.upper <- -1*test$dc_lower

ub.policy.lower <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq4.candidate.controls <- lm(candidatesupport ~ ub  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data[data$uw == 1| data$ub == 1, ])
# summary(eq4.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq4.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

ub.candidate.eff <- candidate.estimates[2]

ub.candidate.lower <- candidate.estimates[2] - 1.96*candidate.se[2]

ub.candidate.upper <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq4.taxes.controls <- lm(taxesfortreatment ~ ub 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$uw == 1| data$ub == 1, ])
# summary(eq4.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq4.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

ub.taxes.eff <- taxes.estimates[2]

ub.taxes.lower <- taxes.estimates[2] - 1.96*taxes.se[2]

ub.taxes.upper <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq4.emotions.controls <- lm(emotionscale ~ ub 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data[ data$uw == 1| data$ub == 1, ])
# summary(eq4.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq4.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

ub.emotions.eff <- emotions.estimates[2]

ub.emotions.lower <- emotions.estimates[2] - 1.96*emotions.se[2]

ub.emotions.upper <- emotions.estimates[2] + 1.96*emotions.se[2]

data.test2 <- data.frame(Outcome = c("Support treatment policy",
                                     "Support treatment candidate" ,
                                     "Taxes for treatment",
                                     "Sympathetic emotional response"),
                         treat = c("Unsymp: Black - White"),
                         effect = c(ub.policy.eff,
                                    ub.candidate.eff,
                                    ub.taxes.eff,
                                    ub.emotions.eff),
                         lower = c(ub.policy.lower, 
                                   ub.candidate.lower,
                                   ub.taxes.lower,
                                   ub.emotions.lower
                         ),
                         upper = c(ub.policy.upper,
                                   ub.candidate.upper,
                                   ub.taxes.upper,
                                   ub.emotions.upper
                         ))

# data.test2

#####################################################
## Eq 7: Racial main effect hypothesis
#####################################################

###############
## DV 1: policy 
###############
## logit so we need predicted probabilities

eq7.policy.controls <- glm(policy ~ pooledblack
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$pooledwhite == 1| data$pooledblack== 1, ])
# summary(eq7.policy.controls)



## sympathetic black
test <- predicts(eq7.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

pooledblackpolicy.eff <- -1*test$dc_mean

pooledblackpolicy.upper <- -1*test$dc_lower

pooledblackpolicy.lower <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq7.candidate.controls <- lm(candidatesupport ~ pooledblack 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data[data$pooledwhite == 1| data$pooledblack== 1, ])
# summary(eq7.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq7.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

pooledblackcandidate.eff <- candidate.estimates[2]

pooledblackcandidate.lower <- candidate.estimates[2] - 1.96*candidate.se[2]

pooledblackcandidate.upper <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq7.taxes.controls <- lm(taxesfortreatment ~ pooledblack
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$pooledwhite == 1| data$pooledblack== 1, ])
# summary(eq7.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq7.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

pooledblacktaxes.eff <- taxes.estimates[2]

pooledblacktaxes.lower <- taxes.estimates[2] - 1.96*taxes.se[2]

pooledblacktaxes.upper <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq7.emotions.controls <- lm(emotionscale ~ pooledblack
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data[ data$pooledwhite == 1| data$pooledblack== 1, ])
# summary(eq7.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq7.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

pooledblackemotions.eff <- emotions.estimates[2]

pooledblackemotions.lower <- emotions.estimates[2] - 1.96*emotions.se[2]

pooledblackemotions.upper <- emotions.estimates[2] + 1.96*emotions.se[2]


data.test3 <- data.frame(Outcome = c("Support treatment policy", 
                                     "Support treatment candidate" , 
                                     "Taxes for treatment", 
                                     "Sympathetic emotional response"),
                         treat = c("Pooled: Black - White"),
                         effect = c(pooledblackpolicy.eff, 
                                    pooledblackcandidate.eff, 
                                    pooledblacktaxes.eff,
                                    pooledblackemotions.eff),
                         lower = c(pooledblackpolicy.lower, 
                                   pooledblackcandidate.lower, 
                                   pooledblacktaxes.lower,
                                   pooledblackemotions.lower),
                         upper = c(pooledblackpolicy.upper,
                                   pooledblackcandidate.upper,
                                   pooledblacktaxes.upper, 
                                   pooledblackemotions.upper))

# data.test3


data.test <- smartbind(data.test1, data.test2, data.test3)

data.test$treat <- factor(data.test$treat , levels =c("Pooled: Black - White", "Unsymp: Black - White",
                                                      "Symp: Black - White"))
# data.test 

margins <- c(.5,.5, .5,.5)
p1 <- ggplot(data.test[c(1,5, 9),], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"),axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.2,0.3)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(plot.margin=unit(margins,"cm"))

# p1


p2<- ggplot(data.test[c(2, 6, 10),], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.20,0.10)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(plot.margin=unit(margins,"cm"))

# p2

p3<- ggplot(data.test[c(3, 7, 11),], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"),axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.20,0.10)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(plot.margin=unit(margins,"cm"))

# p3

p4<- ggplot(data.test[c(4, 8, 12),], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"),axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.20,0.10)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(plot.margin=unit(margins,"cm"))

# p4

# grid.arrange(p1, p2, p3, p4, nrow = 2) ## view in R

## print figure

cat("\nFigure 3: Comparing White and Black treatments (Racial hypotheses)\n")
pdf("./output/Figure3.pdf", width = 11, height = 6)
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()
cat("Saved Figure 3 in /output")

