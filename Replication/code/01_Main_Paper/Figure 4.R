#####################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Main paper
## Figure 4
## Steps: regression models for high and low values of RR, calculate treatment effects, create figure
#####################

rm(list=ls()) ## clear list space

#####################
### Load packages ########
#####################

library(ggplot2) ## used to create figure
library(gridExtra) ## used to print figure
library(glm.predict) ## used for "predicts" function
library(gtools) ## used for "smartbind" function
library(ggpubr) ## used to print figure

#####################
### Load Data ########
#####################

## read in data
data <- read.csv("tessdata.csv", head = T) ## 1517 obs
####################################
## Moderator: Racial resentment
####################################

####################################
## Create tercile bins of RR
####################################

#quantile(data$RR, c(0, 1/3, 2/3, 1), na.rm = T)

## bottom tercile RR <= 0.375
data.lowRR <- data[data$RR < 0.375 & !is.na(data$RR), ] ## 495 obs.

## top tercile RR > 0.75
data.highRR <- data[data$RR >= 0.75 & !is.na(data$RR), ] ## 532 obs.

#####################################################
## Eq 2: Racial Sympathy hypothesis
# low RR
#####################################################

###############
## DV 1: policy 
###############

eq2.policy.controls <- glm(policy ~ sb 
                           + age + region + hsgrad + somecollege 
                           + female + d35kto49k + d50kto74k + d75kto99k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.lowRR[data.lowRR$sw == 1| data.lowRR$sb == 1, ])

## sympathetic black
test <- predicts(eq2.policy.controls, "0-1,1; mean; F(2); mean; mean; 
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)
# test
sb.policy.eff.lrr <- -1*test$dc_mean
sb.policy.upper.lrr <- -1*test$dc_lower
sb.policy.lower.lrr <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq2.candidate.controls <- lm(candidatesupport ~ sb  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad +
                               + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.lowRR[data.lowRR$sw == 1| data.lowRR$sb == 1, ])
# summary(eq2.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq2.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black
sb.candidate.eff.lrr <- candidate.estimates[2]
sb.candidate.lower.lrr <- candidate.estimates[2] - 1.96*candidate.se[2]
sb.candidate.upper.lrr <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq2.taxes.controls <- lm(taxesfortreatment ~ sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.lowRR[data.lowRR$sw == 1| data.lowRR$sb == 1, ])
# summary(eq2.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq2.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black
sb.taxes.eff.lrr <- taxes.estimates[2]
sb.taxes.lower.lrr <- taxes.estimates[2] - 1.96*taxes.se[2]
sb.taxes.upper.lrr <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############
eq2.emotions.controls <- lm(emotionscale ~ sb 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.lowRR[ data.lowRR$sw == 1| data.lowRR$sb == 1, ])

emotions.coefs <- data.frame(coef(summary(eq2.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black
sb.emotions.eff.lrr <- emotions.estimates[2]
sb.emotions.lower.lrr <- emotions.estimates[2] - 1.96*emotions.se[2]
sb.emotions.upper.lrr <- emotions.estimates[2] + 1.96*emotions.se[2]


#####################################################
## Eq 2: Racial Sympathy hypothesis
## high RR
#####################################################

###############
## DV 1: policy 
###############
# table(data.highRR$policy)
eq2.policy.controls <- glm(policy ~ sb 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.highRR[data.highRR$sw == 1| data.highRR$sb == 1, ])

## sympathetic black
test <- predicts(eq2.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL, doPar = T)
# test
sb.policy.eff.hrr <- -1*test$dc_mean
sb.policy.upper.hrr <- -1*test$dc_lower
sb.policy.lower.hrr <- -1*test$dc_upper


###############
## DV 2: candidate
###############
eq2.candidate.controls <- lm(candidatesupport ~ sb  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.highRR[data.highRR$sw == 1| data.highRR$sb == 1, ])

candidate.coefs <- data.frame(coef(summary(eq2.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black
sb.candidate.eff.hrr <- candidate.estimates[2]
sb.candidate.lower.hrr <- candidate.estimates[2] - 1.96*candidate.se[2]
sb.candidate.upper.hrr <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############
eq2.taxes.controls <- lm(taxesfortreatment ~ sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.highRR[data.highRR$sw == 1| data.highRR$sb == 1, ])

taxes.coefs <- data.frame(coef(summary(eq2.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black
sb.taxes.eff.hrr <- taxes.estimates[2]
sb.taxes.lower.hrr <- taxes.estimates[2] - 1.96*taxes.se[2]
sb.taxes.upper.hrr <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############
eq2.emotions.controls <- lm(emotionscale ~ sb 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.highRR[ data.highRR$sw == 1| data.highRR$sb == 1, ])
# summary(eq2.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq2.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black
sb.emotions.eff.hrr <- emotions.estimates[2]
sb.emotions.lower.hrr <- emotions.estimates[2] - 1.96*emotions.se[2]
sb.emotions.upper.hrr <- emotions.estimates[2] + 1.96*emotions.se[2]

data.testRacialSymp <- data.frame(Outcome = c("Support treatment policy", 
                                              "Support treatment policy",
                                              "Support treatment candidate" , 
                                              "Support treatment candidate" , 
                                              "Taxes for treatment", 
                                              "Taxes for treatment", 
                                              "Sympathetic emotional response",
                                              "Sympathetic emotional response"),
                                  treat = c("Symp: Black - White"),
                                  effect = c(sb.policy.eff.lrr,
                                             sb.policy.eff.hrr,
                                             sb.candidate.eff.lrr,
                                             sb.candidate.eff.hrr,
                                             sb.taxes.eff.lrr,
                                             sb.taxes.eff.hrr,
                                             sb.emotions.eff.lrr,
                                             sb.emotions.eff.hrr),
                                  lower = c(sb.policy.lower.lrr,
                                            sb.policy.lower.hrr,
                                            sb.candidate.lower.lrr, 
                                            sb.candidate.lower.hrr,
                                            sb.taxes.lower.lrr,
                                            sb.taxes.lower.hrr,
                                            sb.emotions.lower.lrr,
                                            sb.emotions.lower.hrr),
                                  upper = c(sb.policy.upper.lrr,
                                            sb.policy.upper.hrr,
                                            sb.candidate.upper.lrr,
                                            sb.candidate.upper.hrr,
                                            sb.taxes.upper.lrr,
                                            sb.taxes.upper.hrr,
                                            sb.emotions.upper.lrr,
                                            sb.emotions.upper.hrr),
                                  Moderator = c("Low RR", "High RR"))
# data.testRacialSymp

#####################################################
## Eq 4: Racial antipathy hypothesis
## low RR
#####################################################

###############
## DV 1: policy 
###############

eq4.policy.controls <- glm(policy ~ ub 
                           + age + region + hsgrad + somecollege + collegegrad 
                           + female  + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.lowRR[data.lowRR$uw == 1| data.lowRR$ub == 1, ])
# summary(eq4.policy.controls)

## sympathetic black
test <- predicts(eq4.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)
ub.policy.eff.lrr <- -1*test$dc_mean
ub.policy.upper.lrr  <- -1*test$dc_lower
ub.policy.lower.lrr  <- -1*test$dc_upper


###############
## DV 2: candidate
###############
eq4.candidate.controls <- lm(candidatesupport ~ ub  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.lowRR[data.lowRR$uw == 1| data.lowRR$ub == 1, ])

candidate.coefs <- data.frame(coef(summary(eq4.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black
ub.candidate.eff.lrr  <- candidate.estimates[2]
ub.candidate.lower.lrr  <- candidate.estimates[2] - 1.96*candidate.se[2]
ub.candidate.upper.lrr  <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############
eq4.taxes.controls <- lm(taxesfortreatment ~ ub 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.lowRR[data.lowRR$uw == 1| data.lowRR$ub == 1, ])

taxes.coefs <- data.frame(coef(summary(eq4.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black
ub.taxes.eff.lrr  <- taxes.estimates[2]
ub.taxes.lower.lrr  <- taxes.estimates[2] - 1.96*taxes.se[2]
ub.taxes.upper.lrr  <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############

eq4.emotions.controls <- lm(emotionscale ~ ub 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.lowRR[ data.lowRR$uw == 1| data.lowRR$ub == 1, ])

emotions.coefs <- data.frame(coef(summary(eq4.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black
ub.emotions.eff.lrr  <- emotions.estimates[2]
ub.emotions.lower.lrr  <- emotions.estimates[2] - 1.96*emotions.se[2]
ub.emotions.upper.lrr <- emotions.estimates[2] + 1.96*emotions.se[2]

#####################################################
## Eq 4: Racial antipathy hypothesis
## high RR
#####################################################

###############
## DV 1: policy 
###############

eq4.policy.controls <- glm(policy ~ ub 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.highRR[data.highRR$uw == 1| data.highRR$ub == 1, ])
# summary(eq4.policy.controls)

## sympathetic black
test <- predicts(eq4.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

ub.policy.eff.hrr <- -1*test$dc_mean
ub.policy.upper.hrr  <- -1*test$dc_lower
ub.policy.lower.hrr  <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq4.candidate.controls <- lm(candidatesupport ~ ub  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.highRR[data.highRR$uw == 1| data.highRR$ub == 1, ])
# summary(eq4.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq4.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black
ub.candidate.eff.hrr  <- candidate.estimates[2]
ub.candidate.lower.hrr  <- candidate.estimates[2] - 1.96*candidate.se[2]
ub.candidate.upper.hrr  <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq4.taxes.controls <- lm(taxesfortreatment ~ ub 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.highRR[data.highRR$uw == 1| data.highRR$ub == 1, ])
# summary(eq4.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq4.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black
ub.taxes.eff.hrr  <- taxes.estimates[2]
ub.taxes.lower.hrr  <- taxes.estimates[2] - 1.96*taxes.se[2]
ub.taxes.upper.hrr  <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq4.emotions.controls <- lm(emotionscale ~ ub 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.highRR[ data.highRR$uw == 1| data.highRR$ub == 1, ])
# summary(eq4.emotions.controls)

emotions.coefs <- data.frame(coef(summary(eq4.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black
ub.emotions.eff.hrr  <- emotions.estimates[2]
ub.emotions.lower.hrr  <- emotions.estimates[2] - 1.96*emotions.se[2]
ub.emotions.upper.hrr <- emotions.estimates[2] + 1.96*emotions.se[2]


data.testRacialAntipathy <- data.frame(Outcome = c("Support treatment policy", 
                                                   "Support treatment policy", 
                                                   "Support treatment candidate" , 
                                                   "Support treatment candidate" ,  
                                                   "Taxes for treatment", 
                                                   "Taxes for treatment",
                                                   "Sympathetic emotional response",
                                                   "Sympathetic emotional response"),
                                       treat = c("Unsymp: Black - White"),
                                       effect = c(ub.policy.eff.lrr,
                                                  ub.policy.eff.hrr,
                                                  ub.candidate.eff.lrr, 
                                                  ub.candidate.eff.hrr, 
                                                  ub.taxes.eff.lrr,
                                                  ub.taxes.eff.hrr,
                                                  ub.emotions.eff.lrr,
                                                  ub.emotions.eff.hrr ),
                                       lower = c(ub.policy.lower.lrr,
                                                 ub.policy.lower.hrr,
                                                 ub.candidate.lower.lrr, 
                                                 ub.candidate.lower.hrr, 
                                                 ub.taxes.lower.lrr,
                                                 ub.taxes.lower.hrr,
                                                 ub.emotions.lower.lrr,
                                                 ub.emotions.lower.hrr),
                                       upper = c(ub.policy.upper.lrr,
                                                 ub.policy.upper.hrr,
                                                 ub.candidate.upper.lrr, 
                                                 ub.candidate.upper.hrr, 
                                                 ub.taxes.upper.lrr,
                                                 ub.taxes.upper.hrr,
                                                 ub.emotions.upper.lrr,
                                                 ub.emotions.upper.hrr ),
                                       Moderator = c("Low RR", "High RR"))


# data.testRacialAntipathy

#####################################################
## Eq 7: Racial main effect hypothesis
## low RR
#####################################################

###############
## DV 1: policy 
###############

eq7.policy.controls <- glm(policy ~ pooledblack
                           + age + region + hsgrad + somecollege + collegegrad 
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.lowRR[data.lowRR$pooledwhite == 1| data.lowRR$pooledblack== 1, ])
# summary(eq7.policy.controls)



## sympathetic black
test <- predicts(eq7.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

pooledblackpolicy.eff.lrr <- -1*test$dc_mean

pooledblackpolicy.upper.lrr <- -1*test$dc_lower

pooledblackpolicy.lower.lrr <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq7.candidate.controls <- lm(candidatesupport ~ pooledblack 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.lowRR[data.lowRR$pooledwhite == 1| data.lowRR$pooledblack== 1, ])
# summary(eq7.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq7.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

pooledblackcandidate.eff.lrr <- candidate.estimates[2]

pooledblackcandidate.lower.lrr <- candidate.estimates[2] - 1.96*candidate.se[2]

pooledblackcandidate.upper.lrr <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq7.taxes.controls <- lm(taxesfortreatment ~ pooledblack
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.lowRR[data.lowRR$pooledwhite == 1| data.lowRR$pooledblack== 1, ])
# summary(eq7.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq7.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

pooledblacktaxes.eff.lrr <- taxes.estimates[2]

pooledblacktaxes.lower.lrr <- taxes.estimates[2] - 1.96*taxes.se[2]

pooledblacktaxes.upper.lrr <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq7.emotions.controls <- lm(emotionscale ~ pooledblack
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.lowRR[ data.lowRR$pooledwhite == 1| data.lowRR$pooledblack== 1, ])
# summary(eq7.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq7.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

pooledblackemotions.eff.lrr <- emotions.estimates[2]

pooledblackemotions.lower.lrr <- emotions.estimates[2] - 1.96*emotions.se[2]

pooledblackemotions.upper.lrr <- emotions.estimates[2] + 1.96*emotions.se[2]


#####################################################
## Eq 7: Racial main effect hypothesis
## high RR
#####################################################

###############
## DV 1: policy 
###############
## logit so we need predicted probabilities

eq7.policy.controls <- glm(policy ~ pooledblack
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.highRR[data.highRR$pooledwhite == 1| data.highRR$pooledblack== 1, ])
# summary(eq7.policy.controls)



## sympathetic black
test <- predicts(eq7.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

pooledblackpolicy.eff.hrr <- -1*test$dc_mean

pooledblackpolicy.upper.hrr <- -1*test$dc_lower

pooledblackpolicy.lower.hrr <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq7.candidate.controls <- lm(candidatesupport ~ pooledblack 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.highRR[data.highRR$pooledwhite == 1| data.highRR$pooledblack== 1, ])
# summary(eq7.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq7.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

pooledblackcandidate.eff.hrr <- candidate.estimates[2]

pooledblackcandidate.lower.hrr <- candidate.estimates[2] - 1.96*candidate.se[2]

pooledblackcandidate.upper.hrr <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq7.taxes.controls <- lm(taxesfortreatment ~ pooledblack
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.highRR[data.highRR$pooledwhite == 1| data.highRR$pooledblack== 1, ])
# summary(eq7.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq7.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

pooledblacktaxes.eff.hrr <- taxes.estimates[2]

pooledblacktaxes.lower.hrr <- taxes.estimates[2] - 1.96*taxes.se[2]

pooledblacktaxes.upper.hrr <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############
eq7.emotions.controls <- lm(emotionscale ~ pooledblack
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.highRR[ data.highRR$pooledwhite == 1| data.highRR$pooledblack== 1, ])
# summary(eq7.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq7.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black
pooledblackemotions.eff.hrr <- emotions.estimates[2]
pooledblackemotions.lower.hrr <- emotions.estimates[2] - 1.96*emotions.se[2]
pooledblackemotions.upper.hrr <- emotions.estimates[2] + 1.96*emotions.se[2]


data.testRacialMain <- data.frame(Outcome = c("Support treatment policy", 
                                              "Support treatment policy",
                                              "Support treatment candidate" , 
                                              "Support treatment candidate" ,
                                              "Taxes for treatment", 
                                              "Taxes for treatment",
                                              "Sympathetic emotional response",
                                              "Sympathetic emotional response"),
                                  treat = c("Pooled: Black - White"),
                                  effect = c(pooledblackpolicy.eff.lrr,
                                             pooledblackpolicy.eff.hrr,
                                             pooledblackcandidate.eff.lrr,
                                             pooledblackcandidate.eff.hrr,
                                             pooledblacktaxes.eff.lrr,
                                             pooledblacktaxes.eff.hrr,
                                             pooledblackemotions.eff.lrr,
                                             pooledblackemotions.eff.hrr),
                                  lower = c(pooledblackpolicy.lower.lrr,
                                            pooledblackpolicy.lower.hrr,
                                            pooledblackcandidate.lower.lrr,
                                            pooledblackcandidate.lower.hrr,
                                            pooledblacktaxes.lower.lrr,
                                            pooledblacktaxes.lower.hrr,
                                            pooledblackemotions.lower.lrr,
                                            pooledblackemotions.lower.hrr
                                  ),
                                  upper = c(pooledblackpolicy.upper.lrr,
                                            pooledblackpolicy.upper.hrr,
                                            pooledblackcandidate.upper.lrr,
                                            pooledblackcandidate.upper.hrr,
                                            pooledblacktaxes.upper.lrr,
                                            pooledblacktaxes.upper.hrr,
                                            pooledblackemotions.upper.lrr,
                                            pooledblackemotions.upper.hrr),
                                  Moderator = c("Low RR", "High RR"))

# data.testRacialMain

data.test <- smartbind(data.testRacialSymp, data.testRacialAntipathy, data.testRacialMain)

data.test$treat <- factor(data.test$treat , levels =c("Pooled: Black - White", "Unsymp: Black - White",
                                                      "Symp: Black - White"))

data.test$Moderator <- factor(data.test$Moderator , levels =c("High RR", "Low RR"))


# data.test

margins <- c(.5,.5,.5,.5)

p1<- ggplot(data.test[c(1:2,9:10, 17:18), ], aes(y=treat, x=effect, shape = Moderator)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity", position= ggstance::position_dodgev(height = 0.3), size = 3) +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.4,0.2)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3), position= ggstance::position_dodgev(height = 0.3)) +
  theme(plot.margin=unit(margins,"cm")) +
  guides(shape = guide_legend(reverse = T))

# p1

p2<- ggplot(data.test[c(3:4,11:12, 19:20),], aes(y=treat, x=effect, shape = Moderator)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity", position= ggstance::position_dodgev(height = 0.3), size = 3) +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.4,0.2)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3), position= ggstance::position_dodgev(height = 0.3)) +
  theme(plot.margin=unit(margins,"cm")) +
  guides(shape = guide_legend(reverse = T))
# p2

p3<- ggplot(data.test[c(5:6, 13:14, 21:22), ], aes(y=treat, x=effect, shape = Moderator)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity", position= ggstance::position_dodgev(height = 0.3), size = 3) +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.4,0.2)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3), position= ggstance::position_dodgev(height = 0.3)) +
  theme(plot.margin=unit(margins,"cm")) +
  guides(shape = guide_legend(reverse = T))
# p3

p4<- ggplot(data.test[c(7:8, 15:16, 23:24),], aes(y=treat, x=effect, shape = Moderator)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity", position= ggstance::position_dodgev(height = 0.3), size = 3) +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.4,0.2)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3), position= ggstance::position_dodgev(height = 0.3)) +
  theme(plot.margin=unit(margins,"cm")) +
  guides(shape = guide_legend(reverse = T))
# p4

# grid.arrange(p1, p2, p3, p4, nrow = 2) ## view in R

## print figure
cat("\nFigure 4: Comparing Black and White treatments (Racial hypotheses, by racial resentment)")
pdf("./output/Figure4.pdf", width = 11, height = 8, onefile = F)
ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="top")
dev.off()

cat("Saved Figure 4 in /output")
