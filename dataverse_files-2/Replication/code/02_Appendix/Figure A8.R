#####################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Main paper
## Figure A8
## Steps: regression models for low news exposure respondents, calculate treatment effects, create figure
#####################
rm(list=ls()) ## clear list space


#####################
### Load packages ####
#####################

library(ggplot2) ## used to create figure
library(gridExtra) ## used to print figure
library(glm.predict) ## used for "predicts" function

#####################
### Load Data ########
#####################

## read in data
data <- read.csv("tessdata.csv", head = T) ## 1517 obs

#####################
# subset to low news exposure respondents
#####################

data.lowNews <- subset(data, opioidnews %in% c("Not too closely", "Not closely at all"))

data <- data.lowNews ## n = 891

#####################################################
## Eq 1: Sympathy hypothesis
#####################################################

###############
## DV 1: policy 
###############

eq1.policy.controls <- glm(policy ~ sw + sb 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.policy.controls)

## sympathetic black
test <- predicts(eq1.policy.controls, "0; 0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 2, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

sb.policy.eff <- -1*test$dc_mean

sb.policy.upper <- -1*test$dc_lower

sb.policy.lower <- -1*test$dc_upper

## sympathetic white
test <- predicts(eq1.policy.controls, "0-1,1; 0; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)
# test 

sw.policy.eff <- -1*test$dc_mean

sw.policy.upper <- -1*test$dc_lower

sw.policy.lower <- -1*test$dc_upper

###############
## DV 2: candidate
###############

eq1.candidate.controls <- lm(candidatesupport ~ sw + sb 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                             data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq1.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

sb.candidate.eff <- candidate.estimates[3]

sb.candidate.lower <- candidate.estimates[3] - 1.96*candidate.se[3]

sb.candidate.upper <- candidate.estimates[3] + 1.96*candidate.se[3]

## sympathetic white

sw.candidate.eff <- candidate.estimates[2]

sw.candidate.lower <- candidate.estimates[2] - 1.96*candidate.se[2]

sw.candidate.upper <- candidate.estimates[2] + 1.96*candidate.se[2]

###############
## DV 3: taxes
###############

eq1.taxes.controls <- lm(taxesfortreatment ~ sw + sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq1.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

sb.taxes.eff <- taxes.estimates[3]

sb.taxes.lower <- taxes.estimates[3] - 1.96*taxes.se[3]

sb.taxes.upper <- taxes.estimates[3] + 1.96*taxes.se[3]

## sympathetic white

sw.taxes.eff <- taxes.estimates[2]

sw.taxes.lower <- taxes.estimates[2] - 1.96*taxes.se[2]

sw.taxes.upper <- taxes.estimates[2] + 1.96*taxes.se[2]

###############
## DV 4: emotions
###############


eq1.emotions.controls <- lm(emotionscale ~ sw + sb 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.emotions.controls)

emotions.coefs <- data.frame(coef(summary(eq1.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

sb.emotions.eff <- emotions.estimates[3]

sb.emotions.lower <- emotions.estimates[3] - 1.96*emotions.se[3]

sb.emotions.upper <- emotions.estimates[3] + 1.96*emotions.se[3]

## sympathetic white

sw.emotions.eff <- emotions.estimates[2]

sw.emotions.lower <- emotions.estimates[2] - 1.96*emotions.se[2]

sw.emotions.upper <- emotions.estimates[2] + 1.96*emotions.se[2]

#####################################################
## Eq 3: Antipathy hypothesis
#####################################################

###############
## DV 1: policy 
###############
## logit so we need predicted probabilities

eq3.policy.controls <- glm(policy ~ uw + ub 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.policy.controls)


## sympathetic black
test <- predicts(eq3.policy.controls, "0; 0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 2, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

ub.policy.eff <- -1*test$dc_mean

ub.policy.upper <- -1*test$dc_lower

ub.policy.lower <- -1*test$dc_upper

## sympathetic white
test <- predicts(eq3.policy.controls, "0-1,1; 0; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)
# test 

uw.policy.eff <- -1*test$dc_mean

uw.policy.upper <- -1*test$dc_lower

uw.policy.lower <- -1*test$dc_upper

###############
## DV 2: candidate
###############

eq3.candidate.controls <- lm(candidatesupport ~ uw + ub 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                             data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq3.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

ub.candidate.eff <- candidate.estimates[3]

ub.candidate.lower <- candidate.estimates[3] - 1.96*candidate.se[3]

ub.candidate.upper <- candidate.estimates[3] + 1.96*candidate.se[3]

## sympathetic white

uw.candidate.eff <- candidate.estimates[2]

uw.candidate.lower <- candidate.estimates[2] - 1.96*candidate.se[2]

uw.candidate.upper <- candidate.estimates[2] + 1.96*candidate.se[2]

###############
## DV 3: taxes
###############

eq3.taxes.controls <- lm(taxesfortreatment ~ uw + ub 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq3.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

ub.taxes.eff <- taxes.estimates[3]

ub.taxes.lower <- taxes.estimates[3] - 1.96*taxes.se[3]

ub.taxes.upper <- taxes.estimates[3] + 1.96*taxes.se[3]

## sympathetic white

uw.taxes.eff <- taxes.estimates[2]

uw.taxes.lower <- taxes.estimates[2] - 1.96*taxes.se[2]

uw.taxes.upper <- taxes.estimates[2] + 1.96*taxes.se[2]

###############
## DV 4: emotions
###############


eq3.emotions.controls <- lm(emotionscale ~ uw + ub 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.emotions.controls)

emotions.coefs <- data.frame(coef(summary(eq3.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

ub.emotions.eff <- emotions.estimates[3]

ub.emotions.lower <- emotions.estimates[3] - 1.96*emotions.se[3]

ub.emotions.upper <- emotions.estimates[3] + 1.96*emotions.se[3]

## sympathetic white

uw.emotions.eff <- emotions.estimates[2]

uw.emotions.lower <- emotions.estimates[2] - 1.96*emotions.se[2]

uw.emotions.upper <- emotions.estimates[2] + 1.96*emotions.se[2]

data.test0 <- data.frame(Outcome = c("Support treatment policy", "Support treatment policy", 
                                     "Support treatment policy", "Support treatment policy", 
                                     "Support treatment candidate" , "Support treatment candidate" ,
                                     "Support treatment candidate" , "Support treatment candidate" ,
                                     "Taxes for treatment", "Taxes for treatment", 
                                     "Taxes for treatment", "Taxes for treatment",
                                     "Sympathetic emotional response", "Sympathetic emotional response",
                                     "Sympathetic emotional response", "Sympathetic emotional response"),
                         treat = c("Symp. Black ",
                                   "Symp. White ",
                                   "Unsymp. Black",
                                   "Unsymp. White"),
                         effect = c(sb.policy.eff, sw.policy.eff,
                                    ub.policy.eff, uw.policy.eff,
                                    sb.candidate.eff, sw.candidate.eff,
                                    ub.candidate.eff, uw.candidate.eff,
                                    sb.taxes.eff, sw.taxes.eff,
                                    ub.taxes.eff, uw.taxes.eff,
                                    sb.emotions.eff, sw.emotions.eff,
                                    ub.emotions.eff, uw.emotions.eff),
                         lower = c(sb.policy.lower, sw.policy.lower,
                                   ub.policy.lower, uw.policy.lower,
                                   sb.candidate.lower, sw.candidate.lower,
                                   ub.candidate.lower, uw.candidate.lower,
                                   sb.taxes.lower, sw.taxes.lower,
                                   ub.taxes.lower, uw.taxes.lower,
                                   sb.emotions.lower, sw.emotions.lower,
                                   ub.emotions.lower, uw.emotions.lower),
                         upper = c(sb.policy.upper, sw.policy.upper,
                                   ub.policy.upper, uw.policy.upper,
                                   sb.candidate.upper, sw.candidate.upper,
                                   ub.candidate.upper, uw.candidate.upper,
                                   sb.taxes.upper, sw.taxes.upper,
                                   ub.taxes.upper, uw.taxes.upper,
                                   sb.emotions.upper, sw.emotions.upper,
                                   ub.emotions.upper, uw.emotions.upper)) 

# data.test0


data.test <- data.test0

# levels(data.test$treat)

data.test$group <- c(rep(c(0,4,8,12), 4))

margins <- c(.5,.5, .5,.5)
labels <- c("Symp. Black",
            "Symp. White",
            "Unsymp. Black",
            "Unsymp. White")

p1 <- ggplot(data.test[c(1:4),], aes(y=group, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  #("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.125,0.39)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(plot.margin=unit(margins,"cm")) 

p1 <- p1 + scale_y_continuous(breaks = c(0,4,8,12), labels=labels, limits = c(-2.5, 13.5)) + theme(panel.grid.minor =   element_blank())
# p1


p2<- ggplot(data.test[c(5:8),], aes(y=group, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  #xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.125,0.39)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  #geom_text(aes(-0.06, 0,label = "Baseline = 'Symp. White'", vjust = -1)) +
  theme(plot.margin=unit(margins,"cm"))
p2 <- p2 + scale_y_continuous(breaks = c(0,4,8,12), labels=labels, limits = c(-2.5, 13.5)) + theme(panel.grid.minor =   element_blank())

# p2

p3<- ggplot(data.test[c(9:12),], aes(y=group, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  #xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.125,0.39)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  #geom_text(aes(-0.06, 0,label = "Baseline = 'Symp. White'", vjust = -1)) + 
  theme(plot.margin=unit(margins,"cm"))
p3 <- p3 + scale_y_continuous(breaks = c(0,4,8,12), labels=labels, limits = c(-2.5, 13.5)) + theme(panel.grid.minor =   element_blank())

# p3

p4<- ggplot(data.test[c(13:16),], aes(y=group, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  #xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.125,0.39)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  #geom_text(aes(-0.06, 0,label = "Baseline = 'Symp. White'", vjust = -1)) + 
  theme(plot.margin=unit(margins,"cm"))
p4 <- p4 + scale_y_continuous(breaks = c(0,4,8,12), labels=labels, limits = c(-2.5, 13.5)) + theme(panel.grid.minor =   element_blank())

# p4
# 
# grid.arrange(p1, p2, p3, p4, nrow = 2) # view in R
# 
## print figure
cat("\nFigure A8: Comparing each treatment to no-story control (Low news exposure respondents) \n") 
pdf("./output/FigureA8.pdf", width = 11, height = 6)
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()
cat("Saved Figure A8 in /output")

