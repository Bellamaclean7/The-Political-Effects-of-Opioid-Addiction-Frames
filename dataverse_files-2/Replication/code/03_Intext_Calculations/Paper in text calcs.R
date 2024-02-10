####################################################################################
## Replication file: "Political Effects of Opioid Addiction Frames"
## In-text calculations for Main Paper
## Calculations presented with substantive descriptions
## in the order that they appear in the main paper
## output saved to log rather than files 
####################################################################################
rm(list=ls()) ## clear list space

#####################
### Load packages #####
#####################

library(stargazer) ## print tables

#####################
### Load Data ########
#####################

# set working directory for example: setwd("~/[download location path]/Replication/code") 

## read in pre-test data
data.pretest <- read.csv("dynatadata.csv", head = T) ## 336 obs

## read in main study data
data.main <- read.csv("tessdata.csv", head = T) ## 1517 obs

##########################################
## Main paper, Section "Experimental design"
## Footnote 9 
## Power analysis (theoretical)
##########################################

## Sample size calculated using: https://clincalc.com/stats/samplesize.aspx
## Specifications: Study group design: "Two independent study groups"
## Primary end point: "Continuous"
## Statistical parameters: Mean 1: 0.40 (baseline mean) +/ 0.30 (standard deviation)
## Mean 2: 0.52; Alpha = 0.05; Power: 80%

## Power calculation steps:

## Define variables:
## n1 = sample size for group 1 (e.g. n per condition)
## n2 = sample size for group 2
## K = ratio of sample size for group 1 and 2 = n1/n2 = 1
## delta = absolute value of difference between two means = 0.12
## sigma1 and sigma2 = variance of mean 1 and mean 2 = 0.30
## a = probability of type 1 error = 0.05
## b = probability of type 2 error = 0.2
## z = critical Z value for a given a or b

## Calculate n1 and n2
## n1 = (((sigma1^2 + sigma2^2)/K)* (Z1-a/2 + z1-b)^2))/ (delta^2)
n1 <- (((0.3^2 + 0.3^2)/1)* (1.96 + 0.84)^2)/ (0.12^2) ## 98

## n2 = K*n1 
n2 <- (1*98) ## 98

## total n = n1* 5 conditions * terciles of racial predispositions
totaln <- 98*5*3 ## 1470

##########################################
## Main paper, Section "Experimental design"
## Footnote 12 
## Regress moderators onto treatments (main study)
##########################################
## Racial resentment
rr <- lm(RR ~ sw + sb + uw + ub, data = data.main)
summary(rr)

## Racial stereotypes
stereotype <- lm(racialstereotype ~ sw + sb + uw + ub, data = data.main)
summary(stereotype)

## White identity
whiteid <- lm(whiteid ~ sw + sb + uw + ub, data = data.main)
summary(whiteid)

## View table in R
stargazer(rr, stereotype, whiteid,
          type = "text", omit.table.layout = "n",
          covariate.labels = c("Sympathetic White",
                               "Sympathetic Black",
                               "Unsympathetic White",
                               "Unsympathetic Black",
                               "Constant"), 
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"))

##########################################
## Main paper, Section "Study 1: Manipulation checks"
## Rating of article sympathy (pre-test)
##########################################
## mean values across conditions (NA for control)
tapply(data.pretest$articlesymp, data.pretest$treatment, mean, na.rm = T)

## t-test: difference in means for sympathetic and unsympathetic treatments
test <- t.test(data.pretest$articlesymp[data.pretest$treatment == "noracesymp"|
                                   data.pretest$treatment == "whitesymp"|
                                   data.pretest$treatment == "blacksymp"], 
                data.pretest$articlesymp[data.pretest$treatment == "noraceunsymp"|
                                   data.pretest$treatment == "whiteunsymp"|
                                   data.pretest$treatment == "blackunsymp"], 
                alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test ## mean 1 = 0.70, mean 2 = 0.51 (p < 0.001)

##########################################
## Main paper, Section "Study 1: Manipulation checks"
## Recall of Mike's race (pre-test)
##########################################
## Recollections of Mike's race
table(data.pretest$articlerace)

## Mike's race was perceived correctly by nearly all respondents 
## proportion recalling Mike as White
tapply(data.pretest$mikewhite, data.pretest$treatment, mean, na.rm = T)

## proportion recalling Mike as Black
tapply(data.pretest$mikeblack, data.pretest$treatment, mean, na.rm = T)

## proportion recalling that his race wasn't mentioned (or not sure)
tapply(data.pretest$mikenorace, data.pretest$treatment, mean, na.rm = T)

## Recall of Mike's race in "no race" conditions
round(mean(data.pretest$mikewhite[data.pretest$treatment == "noracesymp"| data.pretest$treatment == "noraceunsymp"]),2) ## 52%
round(mean(data.pretest$mikeblack[data.pretest$treatment == "noracesymp"| data.pretest$treatment == "noraceunsymp"]),2) ## 4%
round(mean(data.pretest$mikenorace[data.pretest$treatment == "noracesymp"| data.pretest$treatment == "noraceunsymp"]),2) ## 44%

##########################################
## Main paper, Section "Study 1: Manipulation checks"
## Footnote 19
## Perception of race of drug addicts (pre-test)
##########################################
## code indicator variables:

## Over-report percent white drug addicts
data.pretest$morewhitedrug <- 0
data.pretest$morewhitedrug[data.pretest$perwhitedrug >= 64] <- 1

## Over-report percent black drug addicts
data.pretest$moreblackdrug <- 0
data.pretest$moreblackdrug[data.pretest$perblackdrug >= 12] <- 1

##########################
## Proportion over-reporting white addicts
##########################

## calculate means across racial conditions

control <- mean(data.pretest$morewhitedrug[data.pretest$treatment == "control"])
control

white <- mean(data.pretest$morewhitedrug[data.pretest$treatment == "whitesymp"|
                                           data.pretest$treatment == "whiteunsymp"])
white

black <- mean(data.pretest$morewhitedrug[data.pretest$treatment == "blacksymp"|
                                           data.pretest$treatment == "blackunsymp"])
black

norace <- mean(data.pretest$morewhitedrug[data.pretest$treatment == "noracesymp"|
                                            data.pretest$treatment == "noraceunsymp"])
norace

## View values in dataframe 
data.test <- data.frame(Outcome = c("Over-report white 'drug addicts'", "Over-report white 'drug addicts'", 
                                    "Over-report white 'drug addicts'", "Over-report white 'drug addicts'"),
                        
                        treat = c("Control",
                                  "Black",
                                  "No Race",
                                  "White"),
                        effect = c(control,
                                   black,
                                   norace,
                                   white)) 



data.test

##########################
## Proportion over-reporting black addicts
##########################
## calculate means across racial conditions

control <- mean(data.pretest$moreblackdrug[data.pretest$treatment == "control"])
control

white <- mean(data.pretest$moreblackdrug[data.pretest$treatment == "whitesymp"|
                                           data.pretest$treatment == "whiteunsymp"])
white

black <- mean(data.pretest$moreblackdrug[data.pretest$treatment == "blacksymp"|
                                           data.pretest$treatment == "blackunsymp"])
black

norace <- mean(data.pretest$moreblackdrug[data.pretest$treatment == "noracesymp"|
                                            data.pretest$treatment == "noraceunsymp"])
norace

## View values in dataframe 
data.test <- data.frame(Outcome = c("Over-report black 'drug addicts'", "Over-report black 'drug addicts'", 
                                    "Over-report black'drug addicts'", "Over-report black 'drug addicts'"),
                        
                        treat = c("Control",
                                  "Black",
                                  "No Race",
                                  "White"),
                        effect = c(control,
                                   black,
                                   norace,
                                   white)) 

data.test

##########################################
## Main paper, Section "Study 2: Main results"
## Footnote 20
## Re-estimate main regression results with PID and ideology coded as terciles (main study)
##########################################

## recode region factor levels so that South = baseline
data.main$region <- factor(data.main$region, levels = c("South", "Northeast", "Midwest", "West"))
summary(data.main$region)

## Tercile PID coding: Democrat (Baseline); Independent; Republican
## Tercile ideology ideology: Liberal (Baseline); Moderate; Conservative


####################################
## Re-estimate regression results: Eq 1-7
####################################

##########################
## Equation 1
## Tercile PID, Ideo controls
##########################

################
## policy
################

eq1.policy.controls <- glm(policy ~ sw + sb 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                           family = "binomial", data = data.main[data.main$ns == 1| data.main$sw == 1| data.main$sb == 1, ])
summary(eq1.policy.controls)


################
## taxes
################

eq1.taxes.controls <- lm(taxesfortreatment ~ sw + sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                         data = data.main[data.main$ns == 1| data.main$sw == 1| data.main$sb == 1, ])
summary(eq1.taxes.controls)

################
## candidate
################

eq1.candidate.controls <- lm(candidatesupport ~ sw + sb 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                             data = data.main[data.main$ns == 1| data.main$sw == 1| data.main$sb == 1, ])
summary(eq1.candidate.controls)

################
## Feelings
################

eq1.emotions.controls <- lm(emotionscale ~ sw + sb 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                            data = data.main[data.main$ns == 1| data.main$sw == 1| data.main$sb == 1, ])
summary(eq1.emotions.controls)

################
## View table (Equation 1)
## Tercile PID, Ideo controls
################

stargazer(eq1.policy.controls, eq1.candidate.controls, 
          eq1.taxes.controls, eq1.emotions.controls,
          type = "text", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "republican", "independent", "moderate", "conservative"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"))

##########################
## Equation 2
## Tercile PID, Ideo controls
##########################

################
## policy
################

eq2.policy.controls <- glm(policy ~ sb 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                           family = "binomial", data = data.main[data.main$sw == 1| data.main$sb == 1, ])
summary(eq2.policy.controls)


################
## taxes
################

eq2.taxes.controls <- lm(taxesfortreatment ~ sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                         data = data.main[data.main$sw == 1| data.main$sb == 1, ])
summary(eq2.taxes.controls)

################
## candidate
################

eq2.candidate.controls <- lm(candidatesupport ~ sb  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                             data = data.main[data.main$sw == 1| data.main$sb == 1, ])
summary(eq2.candidate.controls)

################
## Feelings
################

eq2.emotions.controls <- lm(emotionscale ~ sb 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                            data = data.main[ data.main$sw == 1| data.main$sb == 1, ])
summary(eq2.emotions.controls)

################
## View table (Equation 2)
## Tercile PID, Ideo controls
################

stargazer(eq2.policy.controls, eq2.candidate.controls, 
          eq2.taxes.controls, eq2.emotions.controls,
          type = "text", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "republican", "independent", "moderate", "conservative"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"))

##########################
## Equation 3
## Tercile PID, Ideo controls
##########################

################
## policy
################

eq3.policy.controls <- glm(policy ~ uw + ub 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative , 
                           family = "binomial", data = data.main[data.main$ns == 1| data.main$uw == 1| data.main$ub == 1, ])
summary(eq3.policy.controls)

################
## taxes
################

eq3.taxes.controls <- lm(taxesfortreatment ~ uw + ub 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                         data = data.main[data.main$ns == 1| data.main$uw == 1| data.main$ub == 1, ])
summary(eq3.taxes.controls)

################
## candidate
################

eq3.candidate.controls <- lm(candidatesupport ~ uw + ub 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                             data = data.main[data.main$ns == 1| data.main$uw == 1| data.main$ub == 1, ])
summary(eq3.candidate.controls)

################
## feelings
################

eq3.emotions.controls <- lm(emotionscale ~ uw + ub 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                            data = data.main[data.main$ns == 1| data.main$uw == 1| data.main$ub == 1, ])
summary(eq3.emotions.controls)


################
## View table (Equation 3)
## Tercile PID, Ideo controls
################

stargazer( eq3.policy.controls, eq3.candidate.controls, 
           eq3.taxes.controls, eq3.emotions.controls,
           type = "text", omit.table.layout = "n",
           dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
           omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                    "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                    "d150kormore", "republican", "independent", "moderate", "conservative"),
           star.cutoffs = NA,
           keep.stat = c("adj.rsq", "n"))

##########################
## Equation 4
## Tercile PID, Ideo controls
##########################

################
## policy
################

eq4.policy.controls <- glm(policy ~ ub 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                           family = "binomial", data = data.main[data.main$uw == 1| data.main$ub == 1, ])
summary(eq4.policy.controls)

################
## taxes
################

eq4.taxes.controls <- lm(taxesfortreatment ~ ub 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                         data = data.main[data.main$uw == 1| data.main$ub == 1, ])
summary(eq4.taxes.controls)

################
## candidate
################

eq4.candidate.controls <- lm(candidatesupport ~ ub  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                             data = data.main[data.main$uw == 1| data.main$ub == 1, ])
summary(eq4.candidate.controls)

################
## feelings
################

eq4.emotions.controls <- lm(emotionscale ~ ub  
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                            data = data.main[data.main$uw == 1| data.main$ub == 1, ])
summary(eq4.emotions.controls)


################
## View table (Equation 4)
## Tercile PID, Ideo controls
################

stargazer(eq4.policy.controls, eq4.candidate.controls, 
          eq4.taxes.controls, eq4.emotions.controls,
          type = "text", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "republican", "independent", "moderate", "conservative"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"))

##########################
## Equation 5
## Tercile PID, Ideo controls
##########################

################
## policy
################

eq5.policy.controls <- glm(policy ~ sw 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                           family = "binomial", data = data.main[data.main$uw == 1| data.main$sw == 1, ])
summary(eq5.policy.controls)

################
## taxes
################

eq5.taxes.controls <- lm(taxesfortreatment ~ sw 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                         data = data.main[data.main$uw == 1| data.main$sw == 1, ])
summary(eq5.taxes.controls)

################
## candidate
################

eq5.candidate.controls <- lm(candidatesupport ~ sw  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                             data = data.main[data.main$uw == 1| data.main$sw == 1, ])
summary(eq5.candidate.controls)

################
## feelings
################

eq5.emotions.controls <- lm(emotionscale ~ sw  
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                            data = data.main[data.main$uw == 1| data.main$sw == 1, ])
summary(eq5.emotions.controls)

################
## View table (Equation 5)
## Tercile PID, Ideo controls
################

stargazer(eq5.policy.controls, eq5.candidate.controls, 
          eq5.taxes.controls, eq5.emotions.controls,
          type = "text", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "republican", "independent", "moderate", "conservative"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"))

##########################
## Equation 6
## Tercile PID, Ideo controls
##########################

################
## policy
################

eq6.policy.controls <- glm(policy ~ sb 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                           family = "binomial", data = data.main[data.main$ub == 1| data.main$sb == 1, ])
summary(eq6.policy.controls)

################
## taxes
################

eq6.taxes.controls <- lm(taxesfortreatment ~ sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                         data = data.main[data.main$ub == 1| data.main$sb == 1, ])
summary(eq6.taxes.controls)

################
## candidate
################

eq6.candidate.controls <- lm(candidatesupport ~ sb  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                             data = data.main[data.main$ub == 1| data.main$sb == 1, ])
summary(eq6.candidate.controls)

################
## feelings
################

eq6.emotions.controls <- lm(emotionscale ~ sb  
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                            data = data.main[data.main$ub == 1| data.main$sb == 1, ])
summary(eq6.emotions.controls)

################
## View table (Equation 6)
## Tercile PID, Ideo controls
################

stargazer(eq6.policy.controls, eq6.candidate.controls, 
          eq6.taxes.controls, eq6.emotions.controls,
          type = "text", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "republican", "independent", "moderate", "conservative"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"))

##########################
## Equation 7
## Tercile PID, Ideo controls
##########################

################
## policy
################

eq7.policy.controls <- glm(policy ~ pooledblack 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                           family = "binomial", data = data.main[data.main$pooledwhite == 1| data.main$pooledblack == 1, ])
summary(eq7.policy.controls)

################
## taxes
################

eq7.taxes.controls <- lm(taxesfortreatment ~ pooledblack 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + republican + independent +moderate + conservative, 
                         data = data.main[data.main$pooledwhite == 1| data.main$pooledblack == 1, ])
summary(eq7.taxes.controls)

################
## candidate
################

eq7.candidate.controls <- lm(candidatesupport ~ pooledblack  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                             data = data.main[data.main$pooledwhite == 1| data.main$pooledblack == 1, ])
summary(eq7.candidate.controls)

################
## feelings
################

eq7.emotions.controls <- lm(emotionscale ~ pooledblack  
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ republican + independent +moderate + conservative, 
                            data = data.main[data.main$pooledwhite == 1| data.main$pooledblack == 1, ])
summary(eq7.emotions.controls)

################
## View table (Equation 7)
## Tercile PID, Ideo controls
################

stargazer(eq7.policy.controls, eq7.candidate.controls, 
          eq7.taxes.controls, eq7.emotions.controls,
          type = "text", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "republican", "independent", "moderate", "conservative"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"))

##########################################
## Main paper, Section "Study 2: Main results"
## Footnote 22
## State-level mortality moderator analysis
## Analysis conducted for valence effects (Eq 1 and 3 only)
##########################################
## read in state mortality data (downloaded from CDC website)
## Data is restricted to deaths per 100,000 in each state in 2018

statedeaths <- read.csv("statemortality.csv", head = T) ## 50 obs
head(statedeaths)

## merge mortality data with experimental data

data <- merge(data.main, statedeaths, by.x = "state", by.y = "STATE", all.x = T) 
head(data)

#####################
### Create state death tercile measure
## based on deaths per 100k
#####################
summary(data$DEATHS)
quantile(data$DEATHS, c(0, 1/3, 2/3, 1), na.rm = T)

data$statedeathrt <- NA
data$statedeathrt[data$DEATHS <= 1140] <- "Low mortality rate" 
data$statedeathrt[data$DEATHS > 1140 & data$DEATHS < 2722] <- "Medium mortality rate" 
data$statedeathrt[data$DEATHS >= 2722] <- "High mortality rate" 
table(data$statedeathrt)
data$statedeathrt <- factor(data$statedeathrt, levels = c("Low mortality rate", "Medium mortality rate", "High mortality rate"))

#####################################################
## Run regressions for sympathy and antipatyh hypotheses (Eq 1 and 3 only)
## including interaction term between treatment indicators and state deaths
#####################################################

#####################################################
## Eq 1: Sympathy hypothesis
#####################################################

###############
## DV 1: policy 
###############

eq1.policy.controls.int <- glm(policy ~ sw*statedeathrt + sb*statedeathrt 
                               + age + region + hsgrad + somecollege + collegegrad + postgrad
                               + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                               family = "binomial", data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
summary(eq1.policy.controls.int)


###############
## DV 2: candidate
###############

eq1.candidate.controls.int <- lm(candidatesupport ~ sw*statedeathrt + sb*statedeathrt 
                                 + age + region + hsgrad + somecollege + collegegrad + postgrad
                                 + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                                 data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
summary(eq1.candidate.controls.int)

###############
## DV 3: taxes
###############
eq1.taxes.controls.int <- lm(taxesfortreatment ~ sw*statedeathrt + sb*statedeathrt 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                             data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
summary(eq1.taxes.controls.int)

###############
## DV 4: emotions
###############
eq1.emotions.controls.int <- lm(emotionscale ~ sw*statedeathrt + sb*statedeathrt 
                                + age + region + hsgrad + somecollege + collegegrad + postgrad
                                + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                                data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
summary(eq1.emotions.controls.int)

## no consistent moderating effects
stargazer(eq1.policy.controls.int, eq1.candidate.controls.int, 
          eq1.taxes.controls.int, eq1.emotions.controls.int,
          type = "text", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
         star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"))

#####################################################
## Eq 3: Antipathy hypothesis
#####################################################

###############
## DV 1: policy 
###############
eq3.policy.controls.int <- glm(policy ~ uw*statedeathrt + ub*statedeathrt 
                               + age + region + hsgrad + somecollege + collegegrad + postgrad
                               + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                               family = "binomial", data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
summary(eq3.policy.controls.int)

###############
## DV 2: candidate
###############
eq3.candidate.controls.int <- lm(candidatesupport ~ uw*statedeathrt + ub*statedeathrt 
                                 + age + region + hsgrad + somecollege + collegegrad + postgrad
                                 + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                                 data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
summary(eq3.candidate.controls.int)

###############
## DV 3: taxes
###############
eq3.taxes.controls.int <- lm(taxesfortreatment ~ uw*statedeathrt + ub*statedeathrt 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                             data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
summary(eq3.taxes.controls.int)

###############
## DV 4: emotions
###############
eq3.emotions.controls.int <- lm(emotionscale ~ uw*statedeathrt + ub*statedeathrt 
                                + age + region + hsgrad + somecollege + collegegrad + postgrad
                                + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                                data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
summary(eq3.emotions.controls.int)

## no consistent moderating effects
stargazer(eq3.policy.controls.int, eq3.candidate.controls.int, 
          eq3.taxes.controls.int, eq3.emotions.controls.int,
          type = "text", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"))