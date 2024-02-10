####################################################################################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Main paper
## Figure 2
## Steps: regression models, calculate treatment effects, create figure
####################################################################################
rm(list=ls()) ## clear list space
#####################
### Load packages ####
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
## Eq 5: full valence hypothesis
#####################################################

###############
## DV 1: policy 
###############

eq5.policy.controls <- glm(policy ~ sw 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$uw == 1| data$sw == 1, ])
#summary(eq5.policy.controls)



## sympathetic black
test <- predicts(eq5.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

sw.policy.eff <- -1*test$dc_mean

sw.policy.upper <- -1*test$dc_lower

sw.policy.lower <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq5.candidate.controls <- lm(candidatesupport ~ sw  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data[data$uw == 1| data$sw == 1, ])
#summary(eq5.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq5.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

sw.candidate.eff <- candidate.estimates[2]

sw.candidate.lower <- candidate.estimates[2] - 1.96*candidate.se[2]

sw.candidate.upper <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq5.taxes.controls <- lm(taxesfortreatment ~ sw 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$uw == 1| data$sw == 1, ])
#summary(eq5.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq5.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

sw.taxes.eff <- taxes.estimates[2]

sw.taxes.lower <- taxes.estimates[2] - 1.96*taxes.se[2]

sw.taxes.upper <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq5.emotions.controls <- lm(emotionscale ~ sw 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data[ data$uw == 1| data$sw == 1, ])
#summary(eq5.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq5.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

sw.emotions.eff <- emotions.estimates[2]

sw.emotions.lower <- emotions.estimates[2] - 1.96*emotions.se[2]

sw.emotions.upper <- emotions.estimates[2] + 1.96*emotions.se[2]


data.test1 <- data.frame(Outcome = c("Support treatment policy", 
                                     "Support treatment candidate" , 
                                     "Taxes for treatment", 
                                     "Sympathetic emotional response"),
                         treat = c("White: Symp. - Unsymp."),
                         effect = c( sw.policy.eff,
                                     sw.candidate.eff, 
                                     sw.taxes.eff,
                                     sw.emotions.eff),
                         lower = c( sw.policy.lower, 
                                    sw.candidate.lower, 
                                    sw.taxes.lower,
                                    sw.emotions.lower),
                         upper = c(sw.policy.upper,
                                   sw.candidate.upper,
                                   sw.taxes.upper, 
                                   sw.emotions.upper))
#data.test1
#####################################################
## Eq 6: Full valence hypothesis
#####################################################

###############
## DV 1: policy 
###############
eq6.policy.controls <- glm(policy ~ sb 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$ub == 1| data$sb == 1, ])
#summary(eq6.policy.controls)



## sympathetic black
test <- predicts(eq6.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

sb.policy.eff <- -1*test$dc_mean

sb.policy.upper <- -1*test$dc_lower

sb.policy.lower <- -1*test$dc_upper


###############
## DV 2: candidate
###############

eq6.candidate.controls <- lm(candidatesupport ~ sb  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data[data$ub == 1| data$sb == 1, ])
#summary(eq6.candidate.controls)

candidate.coefs <- data.frame(coef(summary(eq6.candidate.controls)))
candidate.estimates <- candidate.coefs$Estimate
candidate.se <- candidate.coefs$Std..Error

## sympathetic black

sb.candidate.eff <- candidate.estimates[2]

sb.candidate.lower <- candidate.estimates[2] - 1.96*candidate.se[2]

sb.candidate.upper <- candidate.estimates[2] + 1.96*candidate.se[2]


###############
## DV 3: taxes
###############

eq6.taxes.controls <- lm(taxesfortreatment ~ sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$ub == 1| data$sb == 1, ])
#summary(eq6.taxes.controls)

taxes.coefs <- data.frame(coef(summary(eq6.taxes.controls)))
taxes.estimates <- taxes.coefs$Estimate
taxes.se <- taxes.coefs$Std..Error

## sympathetic black

sb.taxes.eff <- taxes.estimates[2]

sb.taxes.lower <- taxes.estimates[2] - 1.96*taxes.se[2]

sb.taxes.upper <- taxes.estimates[2] + 1.96*taxes.se[2]


###############
## DV 4: emotions
###############


eq6.emotions.controls <- lm(emotionscale ~ sb 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data[ data$ub == 1| data$sb == 1, ])
#summary(eq6.emotions.controls)


emotions.coefs <- data.frame(coef(summary(eq6.emotions.controls)))
emotions.estimates <- emotions.coefs$Estimate
emotions.se <- emotions.coefs$Std..Error

## sympathetic black

sb.emotions.eff <- emotions.estimates[2]

sb.emotions.lower <- emotions.estimates[2] - 1.96*emotions.se[2]

sb.emotions.upper <- emotions.estimates[2] + 1.96*emotions.se[2]


data.test2 <- data.frame(Outcome = c("Support treatment policy", 
                                     "Support treatment candidate" , 
                                     "Taxes for treatment", 
                                     "Sympathetic emotional response"),
                         treat = c("Black: Symp. - Unsymp."),
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

#data.test2

data.test <- smartbind( data.test1, data.test2)

data.test$treat <- factor(data.test$treat , levels =c("Black: Symp. - Unsymp.",
                                                      "White: Symp. - Unsymp."))

# levels(data.test$treat)
# data.test


data.test$group <- c(rep(5, 4), rep(-3.5, 4))

margins <- c(.5,.5, .5,.5)
labels <- c("Black: Symp. - Unsymp.",
            "White: Symp. - Unsymp.")

p1 <- ggplot(data.test[c(1, 5),], aes(y=group, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.1,0.32)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(plot.margin=unit(margins,"cm")) 

p1 <- p1 + scale_y_continuous(breaks = c(-3.5,5)#,7.5,9,10.5,12)
                              , labels=labels, limits = c(-6, 6.5)) + theme(panel.grid.minor =   element_blank())


#p1

p2<- ggplot(data.test[c(2,6),], aes(y=group, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.1,0.32)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(plot.margin=unit(margins,"cm"))

p2 <- p2 + scale_y_continuous(breaks = c(-3.5,5 ), labels=labels, limits = c(-6, 6.5)) + theme(panel.grid.minor =   element_blank())

#p2

p3<- ggplot(data.test[c(3,7),], aes(y=group, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.1,0.32)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(plot.margin=unit(margins,"cm"))
p3 <- p3 + scale_y_continuous(breaks = c(-3.5,5), labels=labels, limits = c(-6, 6.5)) + theme(panel.grid.minor =   element_blank())

#p3

p4<- ggplot(data.test[c(4,8),], aes(y=group, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(-0.1,0.32)) +
  geom_vline(xintercept=0, linetype = "dashed", col = "grey") +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(plot.margin=unit(margins,"cm"))
p4 <- p4 + scale_y_continuous(breaks = c(-3.5,5), labels=labels, limits = c(-6, 6.5)) + theme(panel.grid.minor =   element_blank())

#p4

#grid.arrange(p1, p2, p3, p4, nrow = 2) ## view in R

## print figure

cat("\nFigure 2: Comparing sympathetic to unsympathetic treatments within race (Full valence)\n")
pdf("./output/Figure2.pdf", width = 11, height = 6)
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()
cat("Saved Figure 2 in /output")
