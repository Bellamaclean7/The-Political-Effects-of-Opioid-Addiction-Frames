#####################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Main paper
## Figure A6
## Steps: regression models for high and low values of RS, calculate treatment effects, create figure
#####################
rm(list=ls()) ## clear list space

#####################
### Packages ########
#####################

library(ggplot2)
library(glm.predict)
library(gtools)
library(gridExtra)
library(ggpubr) ## needed to place legend on top of figure
#####################
### Load Data ########
#####################


## read in data
data <- read.csv("tessdata.csv", head = T) ## 1517 obs

####################################
## Moderator: Racial stereotype
####################################

####################################
## Create binary bins of racial stereotypes
####################################

## racial stereotypes
## bottom bin of stereotype = 0 (indicates belief that there is no diff btwn blacks and whites)
data.lowRS <- data[data$racialstereotype == 0 & !is.na(data$racialstereotype), ] ## 1078 obs.

## top top bin of stereotype > 0 (indicates beleif that blacks are more lazy than whites)
data.highRS <- data[data$racialstereotype > 0 & !is.na(data$racialstereotype), ] ## 276 obs.

#####################################################
## Eq 2: Racial Sympathy hypothesis
## low RS
#####################################################

###############
## DV 1: policy 
###############

eq2.policy.controls <- glm(policy ~ sb 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.lowRS[data.lowRS$sw == 1| data.lowRS$sb == 1, ])
# summary(eq2.policy.controls)



## sympathetic black
test <- predicts(eq2.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

sb.policy.eff.lrs <- -1*test$dc_mean

sb.policy.upper.lrs <- -1*test$dc_lower

sb.policy.lower.lrs <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq2.candidate.controls <- lm(candidatesupport ~ sb  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.lowRS[data.lowRS$sw == 1| data.lowRS$sb == 1, ])
# summary(eq2.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq2.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

sb.candidate.eff.lrs <- candidate.estimates[2]

sb.candidate.lower.lrs <- candidate.estimates[2] - 1.96*candidate.se[2]

sb.candidate.upper.lrs <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq2.taxes.controls <- lm(taxesfortreatment ~ sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.lowRS[data.lowRS$sw == 1| data.lowRS$sb == 1, ])
# summary(eq2.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq2.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

sb.taxes.eff.lrs <- taxes.estimates[2]

sb.taxes.lower.lrs <- taxes.estimates[2] - 1.96*taxes.se[2]

sb.taxes.upper.lrs <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq2.emotions.controls <- lm(emotionscale ~ sb 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.lowRS[ data.lowRS$sw == 1| data.lowRS$sb == 1, ])
# summary(eq2.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq2.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

sb.emotions.eff.lrs <- emotions.estimates[2]

sb.emotions.lower.lrs <- emotions.estimates[2] - 1.96*emotions.se[2]

sb.emotions.upper.lrs <- emotions.estimates[2] + 1.96*emotions.se[2]



#####################################################
## Eq 2: Racial Sympathy hypothesis
## high RS
#####################################################

###############
## DV 1: policy 
###############

# table(data.highRS$policy)
eq2.policy.controls <- glm(policy ~ sb 
                           + age + region + hsgrad + somecollege + collegegrad #+ postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.highRS[data.highRS$sw == 1| data.highRS$sb == 1, ])
# summary(eq2.policy.controls)



## sympathetic black
test <- predicts(eq2.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; 
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL, doPar = T)

sb.policy.eff.hrs <- -1*test$dc_mean

sb.policy.upper.hrs <- -1*test$dc_lower

sb.policy.lower.hrs <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq2.candidate.controls <- lm(candidatesupport ~ sb  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.highRS[data.highRS$sw == 1| data.highRS$sb == 1, ])
# summary(eq2.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq2.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

sb.candidate.eff.hrs <- candidate.estimates[2]

sb.candidate.lower.hrs <- candidate.estimates[2] - 1.96*candidate.se[2]

sb.candidate.upper.hrs <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq2.taxes.controls <- lm(taxesfortreatment ~ sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.highRS[data.highRS$sw == 1| data.highRS$sb == 1, ])
#summary(eq2.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq2.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

sb.taxes.eff.hrs <- taxes.estimates[2]

sb.taxes.lower.hrs <- taxes.estimates[2] - 1.96*taxes.se[2]

sb.taxes.upper.hrs <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq2.emotions.controls <- lm(emotionscale ~ sb 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.highRS[ data.highRS$sw == 1| data.highRS$sb == 1, ])
# summary(eq2.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq2.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

sb.emotions.eff.hrs <- emotions.estimates[2]

sb.emotions.lower.hrs <- emotions.estimates[2] - 1.96*emotions.se[2]

sb.emotions.upper.hrs <- emotions.estimates[2] + 1.96*emotions.se[2]

data.testRS.RS <- data.frame(Outcome = c("Support treatment policy", 
                                         "Support treatment policy",
                                         "Support treatment candidate" , 
                                         "Support treatment candidate" , 
                                         "Taxes for treatment", 
                                         "Taxes for treatment", 
                                         "Sympathetic emotional response",
                                         "Sympathetic emotional response"),
                             treat = c("Symp: Black - White"),
                             effect = c(sb.policy.eff.lrs,
                                        sb.policy.eff.hrs,
                                        sb.candidate.eff.lrs,
                                        sb.candidate.eff.hrs,
                                        sb.taxes.eff.lrs,
                                        sb.taxes.eff.hrs,
                                        sb.emotions.eff.lrs,
                                        sb.emotions.eff.hrs),
                             lower = c(sb.policy.lower.lrs,
                                       sb.policy.lower.hrs,
                                       sb.candidate.lower.lrs, 
                                       sb.candidate.lower.hrs,
                                       sb.taxes.lower.lrs,
                                       sb.taxes.lower.hrs,
                                       sb.emotions.lower.lrs,
                                       sb.emotions.lower.hrs),
                             upper = c(sb.policy.upper.lrs,
                                       sb.policy.upper.hrs,
                                       sb.candidate.upper.lrs,
                                       sb.candidate.upper.hrs,
                                       sb.taxes.upper.lrs,
                                       sb.taxes.upper.hrs,
                                       sb.emotions.upper.lrs,
                                       sb.emotions.upper.hrs),
                             Moderator = c("Low RS", "High RS"))


#####################################################
## Eq 4: Racial Antipathy hypothesis
## low RS
#####################################################

###############
## DV 1: policy 
###############

eq4.policy.controls <- glm(policy ~ ub 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.lowRS[data.lowRS$uw == 1| data.lowRS$ub == 1, ])
# summary(eq4.policy.controls)



## sympathetic black
test <- predicts(eq4.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

ub.policy.eff.lrs <- -1*test$dc_mean

ub.policy.upper.lrs  <- -1*test$dc_lower

ub.policy.lower.lrs  <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq4.candidate.controls <- lm(candidatesupport ~ ub  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.lowRS[data.lowRS$uw == 1| data.lowRS$ub == 1, ])
# summary(eq4.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq4.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

ub.candidate.eff.lrs  <- candidate.estimates[2]

ub.candidate.lower.lrs  <- candidate.estimates[2] - 1.96*candidate.se[2]

ub.candidate.upper.lrs  <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq4.taxes.controls <- lm(taxesfortreatment ~ ub 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.lowRS[data.lowRS$uw == 1| data.lowRS$ub == 1, ])
# summary(eq4.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq4.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

ub.taxes.eff.lrs  <- taxes.estimates[2]

ub.taxes.lower.lrs  <- taxes.estimates[2] - 1.96*taxes.se[2]

ub.taxes.upper.lrs  <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq4.emotions.controls <- lm(emotionscale ~ ub 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.lowRS[ data.lowRS$uw == 1| data.lowRS$ub == 1, ])
# summary(eq4.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq4.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

ub.emotions.eff.lrs  <- emotions.estimates[2]

ub.emotions.lower.lrs  <- emotions.estimates[2] - 1.96*emotions.se[2]

ub.emotions.upper.lrs <- emotions.estimates[2] + 1.96*emotions.se[2]


#####################################################
## Eq 4: Racial Antipathy hypothesis
## high RS
#####################################################

###############
## DV 1: policy 
###############

eq4.policy.controls <- glm(policy ~ ub 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.highRS[data.highRS$uw == 1| data.highRS$ub == 1, ])
# summary(eq4.policy.controls)



## sympathetic black
test <- predicts(eq4.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

ub.policy.eff.hrs <- -1*test$dc_mean

ub.policy.upper.hrs  <- -1*test$dc_lower

ub.policy.lower.hrs  <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq4.candidate.controls <- lm(candidatesupport ~ ub  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.highRS[data.highRS$uw == 1| data.highRS$ub == 1, ])
# summary(eq4.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq4.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

ub.candidate.eff.hrs  <- candidate.estimates[2]

ub.candidate.lower.hrs  <- candidate.estimates[2] - 1.96*candidate.se[2]

ub.candidate.upper.hrs  <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq4.taxes.controls <- lm(taxesfortreatment ~ ub 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.highRS[data.highRS$uw == 1| data.highRS$ub == 1, ])
# summary(eq4.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq4.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

ub.taxes.eff.hrs  <- taxes.estimates[2]

ub.taxes.lower.hrs  <- taxes.estimates[2] - 1.96*taxes.se[2]

ub.taxes.upper.hrs  <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq4.emotions.controls <- lm(emotionscale ~ ub 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.highRS[ data.highRS$uw == 1| data.highRS$ub == 1, ])
# summary(eq4.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq4.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

ub.emotions.eff.hrs  <- emotions.estimates[2]

ub.emotions.lower.hrs  <- emotions.estimates[2] - 1.96*emotions.se[2]

ub.emotions.upper.hrs <- emotions.estimates[2] + 1.96*emotions.se[2]


data.test.RS.RA <- data.frame(Outcome = c("Support treatment policy", 
                                          "Support treatment policy",
                                          "Support treatment candidate" ,
                                          "Support treatment candidate" , 
                                          "Taxes for treatment", 
                                          "Taxes for treatment",
                                          "Sympathetic emotional response",
                                          "Sympathetic emotional response"),
                              treat = c("Unsymp: Black - White"),
                              effect = c(ub.policy.eff.lrs,
                                         ub.policy.eff.hrs,
                                         ub.candidate.eff.lrs, 
                                         ub.candidate.eff.hrs,
                                         ub.taxes.eff.lrs,
                                         ub.taxes.eff.hrs,
                                         ub.emotions.eff.lrs,
                                         ub.emotions.eff.hrs),
                              lower = c(ub.policy.lower.lrs,
                                        ub.policy.lower.hrs,
                                        ub.candidate.lower.lrs, 
                                        ub.candidate.lower.hrs, 
                                        ub.taxes.lower.lrs,
                                        ub.taxes.lower.hrs,
                                        ub.emotions.lower.lrs,
                                        ub.emotions.lower.hrs),
                              upper = c(ub.policy.upper.lrs,
                                        ub.policy.upper.hrs,
                                        ub.candidate.upper.lrs, 
                                        ub.candidate.upper.hrs, 
                                        ub.taxes.upper.lrs,
                                        ub.taxes.upper.hrs,
                                        ub.emotions.upper.lrs,
                                        ub.emotions.upper.hrs),
                              Moderator = c("Low RS", "High RS"))


# data.test.RS.RA

#####################################################
## Eq 7: Racial main effect hypothesis
## Low RS
#####################################################

###############
## DV 1: policy 
###############

eq7.policy.controls <- glm(policy ~ pooledblack
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.lowRS[data.lowRS$pooledwhite == 1| data.lowRS$pooledblack== 1, ])
# summary(eq7.policy.controls)



## sympathetic black
test <- predicts(eq7.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

pooledblackpolicy.eff.lrs <- -1*test$dc_mean

pooledblackpolicy.upper.lrs <- -1*test$dc_lower

pooledblackpolicy.lower.lrs <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq7.candidate.controls <- lm(candidatesupport ~ pooledblack 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.lowRS[data.lowRS$pooledwhite == 1| data.lowRS$pooledblack== 1, ])
# summary(eq7.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq7.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

pooledblackcandidate.eff.lrs <- candidate.estimates[2]

pooledblackcandidate.lower.lrs <- candidate.estimates[2] - 1.96*candidate.se[2]

pooledblackcandidate.upper.lrs <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq7.taxes.controls <- lm(taxesfortreatment ~ pooledblack
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.lowRS[data.lowRS$pooledwhite == 1| data.lowRS$pooledblack== 1, ])
# summary(eq7.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq7.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

pooledblacktaxes.eff.lrs <- taxes.estimates[2]

pooledblacktaxes.lower.lrs <- taxes.estimates[2] - 1.96*taxes.se[2]

pooledblacktaxes.upper.lrs <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq7.emotions.controls <- lm(emotionscale ~ pooledblack
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.lowRS[ data.lowRS$pooledwhite == 1| data.lowRS$pooledblack== 1, ])
# summary(eq7.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq7.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

pooledblackemotions.eff.lrs <- emotions.estimates[2]

pooledblackemotions.lower.lrs <- emotions.estimates[2] - 1.96*emotions.se[2]

pooledblackemotions.upper.lrs <- emotions.estimates[2] + 1.96*emotions.se[2]



#####################################################
## Eq 7: Racial main effect hypothesis
## high RS
#####################################################

###############
## DV 1: policy 
###############

eq7.policy.controls <- glm(policy ~ pooledblack
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data.highRS[data.highRS$pooledwhite == 1| data.highRS$pooledblack== 1, ])
# summary(eq7.policy.controls)



## sympathetic black
test <- predicts(eq7.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

pooledblackpolicy.eff.hrs <- -1*test$dc_mean

pooledblackpolicy.upper.hrs <- -1*test$dc_lower

pooledblackpolicy.lower.hrs <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq7.candidate.controls <- lm(candidatesupport ~ pooledblack 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data.highRS[data.highRS$pooledwhite == 1| data.highRS$pooledblack== 1, ])
# summary(eq7.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq7.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

pooledblackcandidate.eff.hrs <- candidate.estimates[2]

pooledblackcandidate.lower.hrs <- candidate.estimates[2] - 1.96*candidate.se[2]

pooledblackcandidate.upper.hrs <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq7.taxes.controls <- lm(taxesfortreatment ~ pooledblack
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data.highRS[data.highRS$pooledwhite == 1| data.highRS$pooledblack== 1, ])
# summary(eq7.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq7.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

pooledblacktaxes.eff.hrs <- taxes.estimates[2]

pooledblacktaxes.lower.hrs <- taxes.estimates[2] - 1.96*taxes.se[2]

pooledblacktaxes.upper.hrs <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq7.emotions.controls <- lm(emotionscale ~ pooledblack
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data.highRS[ data.highRS$pooledwhite == 1| data.highRS$pooledblack== 1, ])
# summary(eq7.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq7.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

pooledblackemotions.eff.hrs <- emotions.estimates[2]

pooledblackemotions.lower.hrs <- emotions.estimates[2] - 1.96*emotions.se[2]

pooledblackemotions.upper.hrs <- emotions.estimates[2] + 1.96*emotions.se[2]


data.testRS.ME <- data.frame(Outcome = c("Support treatment policy", 
                                         "Support treatment policy",
                                         "Support treatment candidate" , 
                                         "Support treatment candidate" ,
                                         "Taxes for treatment", 
                                         "Taxes for treatment",
                                         "Sympathetic emotional response",
                                         "Sympathetic emotional response"),
                             treat = c("Pooled: Black - White"),
                             effect = c(pooledblackpolicy.eff.lrs,
                                        pooledblackpolicy.eff.hrs,
                                        pooledblackcandidate.eff.lrs,
                                        pooledblackcandidate.eff.hrs,
                                        pooledblacktaxes.eff.lrs,
                                        pooledblacktaxes.eff.hrs,
                                        pooledblackemotions.eff.lrs,
                                        pooledblackemotions.eff.hrs),
                             lower = c(pooledblackpolicy.lower.lrs,
                                       pooledblackpolicy.lower.hrs,
                                       pooledblackcandidate.lower.lrs,
                                       pooledblackcandidate.lower.hrs,
                                       pooledblacktaxes.lower.lrs,
                                       pooledblacktaxes.lower.hrs,
                                       pooledblackemotions.lower.lrs,
                                       pooledblackemotions.lower.hrs
                             ),
                             upper = c(pooledblackpolicy.upper.lrs,
                                       pooledblackpolicy.upper.hrs,
                                       pooledblackcandidate.upper.lrs,
                                       pooledblackcandidate.upper.hrs,
                                       pooledblacktaxes.upper.lrs,
                                       pooledblacktaxes.upper.hrs,
                                       pooledblackemotions.upper.lrs,
                                       pooledblackemotions.upper.hrs),
                             Moderator = c("Low RS", "High RS"))

# data.testRS.ME
data.test <- smartbind(data.testRS.RS, data.test.RS.RA, data.testRS.ME)


data.test$treat <- factor(data.test$treat , levels =c("Pooled: Black - White", "Unsymp: Black - White",
                                                      "Symp: Black - White"))

data.test$Moderator <- factor(data.test$Moderator , levels =c("High RS", "Low RS"))
# data.test

margins <- c(.5,.5, .5,.5)
p1 <- ggplot(data.test[c(1:2,9:10, 17:18),], aes(y=treat, x=effect, fill=treat, shape = Moderator)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity", position= ggstance::position_dodgev(height = 0.3), size = 3) +
  guides(fill=FALSE) +
  theme(legend.position="bottom")+
  
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(-0.61,0.2)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3), position= ggstance::position_dodgev(height = 0.3)) +
  theme(plot.margin=unit(margins,"cm"), axis.title.x = element_blank())+
  guides(shape = guide_legend(reverse = T))

# p1


p2<- ggplot(data.test[c(3:4, 11:12, 19:20),], aes(y=treat, x=effect, fill=treat, shape = Moderator)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity", position= ggstance::position_dodgev(height = 0.3), size = 3) +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(-0.20,0.2)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3), position= ggstance::position_dodgev(height = 0.3)) +
  theme(plot.margin=unit(margins,"cm"), axis.title.x = element_blank())+
  guides(shape = guide_legend(reverse = T))


# p2

p3<- ggplot(data.test[c(5:6, 13:14, 21:22),], aes(y=treat, x=effect, fill=treat, shape = Moderator)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity", position= ggstance::position_dodgev(height = 0.3), size = 3) +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(-0.2,0.2)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3),position= ggstance::position_dodgev(height = 0.3)) +
  theme(plot.margin=unit(margins,"cm"), axis.title.x = element_blank())+
  guides(shape = guide_legend(reverse = T))

# p3

p4 <- ggplot(data.test[c(7:8, 15:16, 23:24),], aes(y=treat, x=effect, fill=treat, shape = Moderator)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity", position= ggstance::position_dodgev(height = 0.3), size = 3) +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(-0.20,0.10)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3), position= ggstance::position_dodgev(height = 0.3)) +
  theme(plot.margin=unit(margins,"cm"), axis.title.x = element_blank())+
  guides(shape = guide_legend(reverse = T))

# p4

# grid.arrange(p1, p2, p3, p4, nrow = 2) ## view in R

## print figure
cat("\nFigure A6 Comparing Black and White treatments (Racial hypotheses, by racial stereotypes) \n")
pdf("./output/FigureA6.pdf", width = 11, height = 8, onefile = F)
ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend = TRUE, legend="top")
dev.off()
cat("Saved Figure A6 in /output")
