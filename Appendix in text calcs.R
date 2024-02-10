####################################################################################
## Replication file: "Political Effects of Opioid Addiction Frames"
## In-text calculations for Appendix
## Calculations presented with substantive descriptions
## in the order they appear in the appendix
####################################################################################
rm(list=ls()) ## clear list space

#####################
### Load packages #####
#####################

library(stargazer) ## print tables
library('ivpack') ## 2SLS for CACE
library(glm.predict) ## used for "predicts" function

#####################
### Function #####
#####################
se <- function(x) sqrt(var(x, na.rm = T)/length(x)) ## standard error

#####################
### Load Data ########
#####################

# set working directory for example: setwd("~/[download location path]/Replication/code") 

## read in pre-test data
data.pretest <- read.csv("dynatadata.csv", head = T) ## 336 obs

## read in main study data
data.main <- read.csv("tessdata.csv", head = T) ## 1517 obs

##########################################
## Appendix, Section A3
## Differences in emotions across experimental conditions (pre-test)
## (T-tests for the differences in means plotted in Figure A3 and described in paragraph after figure)
##########################################

## Comparisons: white vs. control, black vs. control, no race vs. control, black vs. white 

## anger
white <- t.test(data.pretest$anger[data.pretest$treatment == "control"], 
                data.pretest$anger[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)

white

black <- t.test(data.pretest$anger[data.pretest$treatment == "control"], 
                 data.pretest$anger[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
black ## sig

norace <- t.test(data.pretest$anger[data.pretest$treatment == "control"], 
                 data.pretest$anger[data.pretest$treatment == "noraceunsymp" | data.pretest$treatment == "noracesymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
norace

racecomp <- t.test(data.pretest$anger[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                data.pretest$anger[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
racecomp


## fear
white <- t.test(data.pretest$fear[data.pretest$treatment == "control"], 
                data.pretest$fear[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
white ## sig

black <- t.test(data.pretest$fear[data.pretest$treatment == "control"], 
                 data.pretest$fear[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
black ## sig

norace <- t.test(data.pretest$fear[data.pretest$treatment == "control"], 
                 data.pretest$fear[data.pretest$treatment == "noraceunsymp" | data.pretest$treatment == "noracesymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
norace ## sig

racecomp <- t.test(data.pretest$fear[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                   data.pretest$fear[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                   alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
racecomp

## disgust
white <- t.test(data.pretest$disgust[data.pretest$treatment == "control"], 
                data.pretest$disgust[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
white

black <- t.test(data.pretest$disgust[data.pretest$treatment == "control"], 
                 data.pretest$disgust[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
black

norace <- t.test(data.pretest$disgust[data.pretest$treatment == "control"], 
                 data.pretest$disgust[data.pretest$treatment == "noraceunsymp" | data.pretest$treatment == "noracesymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
norace

racecomp <- t.test(data.pretest$disgust[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                   data.pretest$disgust[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                   alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
racecomp

## sympathy
white <- t.test(data.pretest$sympathy[data.pretest$treatment == "control"], 
                data.pretest$sympathy[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
white ## sig

black <- t.test(data.pretest$sympathy[data.pretest$treatment == "control"], 
                 data.pretest$sympathy[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
black

norace <- t.test(data.pretest$sympathy[data.pretest$treatment == "control"], 
                 data.pretest$sympathy[data.pretest$treatment == "noraceunsymp" | data.pretest$treatment == "noracesymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
norace ## sig

racecomp <- t.test(data.pretest$sympathy[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                   data.pretest$sympathy[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                   alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
racecomp ## p = 0.08

## compassion
white <- t.test(data.pretest$compassion[data.pretest$treatment == "control"], 
                data.pretest$compassion[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
white

black <- t.test(data.pretest$compassion[data.pretest$treatment == "control"], 
                 data.pretest$compassion[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
black

norace <- t.test(data.pretest$compassion[data.pretest$treatment == "control"], 
                 data.pretest$compassion[data.pretest$treatment == "noraceunsymp" | data.pretest$treatment == "noracesymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
norace

racecomp <- t.test(data.pretest$compassion[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                   data.pretest$compassion[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                   alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
racecomp

## pity
white <- t.test(data.pretest$pity[data.pretest$treatment == "control"], 
                data.pretest$pity[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
white ## sig


black <- t.test(data.pretest$pity[data.pretest$treatment == "control"], 
                 data.pretest$pity[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
black

norace <- t.test(data.pretest$pity[data.pretest$treatment == "control"], 
                 data.pretest$pity[data.pretest$treatment == "noraceunsymp" | data.pretest$treatment == "noracesymp"], 
                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
norace ## sig

racecomp <- t.test(data.pretest$pity[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], 
                   data.pretest$pity[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], 
                   alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
racecomp

################################
## Appendix, Section A3
## Regress moderators onto treatments (pre-test)
################################
## code treatment as factor with baseline = control
data.pretest$treatment <- factor(data.pretest$treatment, levels = c("control",
                                                                    "whitesymp",
                                                                    "blacksymp",
                                                                    "noracesymp",
                                                                    "whiteunsymp",
                                                                    "blackunsymp",
                                                                    "noraceunsymp"))

## racial resentment
fit1 <- lm(rr ~ treatment, data = data.pretest)
summary(fit1) ## neg association with whitesymp condition

## white id
fit2 <- lm(whiteid ~ treatment, data = data.pretest)
summary(fit2)

## workstereotypediff
fit3 <- lm(workstereotypediff ~ treatment, data = data.pretest)
summary(fit3)

## View table
stargazer(fit1, fit2, fit3, 
          type = "text", omit.table.layout = "n", star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"))

################################
## Appendix, Section A3
## Calculate emotional outcome means for black vs. white conditions
## across high and low white identity and racial stereotype subsets
################################

################################
## Code low and high white id subsets ##
################################

quantile(data.pretest$whiteid, c(0, 0.5), na.rm = T)

data.highwhiteid <- data.pretest[data.pretest$whiteid >= 0.50, ] ## 203 responses
data.lowwhiteid <- data.pretest[data.pretest$whiteid < 0.50, ] ## 133 responses

############################
## Emotional responses (black vs. white conditions) by white id
############################

## anger 
test.white.anger.highwhiteid <- t.test(data.highwhiteid$anger[data.highwhiteid$treatment == "blackunsymp" | data.highwhiteid$treatment == "blacksymp"], 
                                       data.highwhiteid$anger[data.highwhiteid$treatment == "whiteunsymp" | data.highwhiteid$treatment == "whitesymp"], 
                                       alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.anger.highwhiteid

test.white.anger.lowwhiteid <- t.test(data.lowwhiteid$anger[data.lowwhiteid$treatment == "blackunsymp" | data.lowwhiteid$treatment == "blacksymp"], 
                                       data.lowwhiteid$anger[data.lowwhiteid$treatment == "whiteunsymp" | data.lowwhiteid$treatment == "whitesymp"], 
                                       alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.anger.lowwhiteid


## fear

test.white.fear.highwhiteid <- t.test(data.highwhiteid$fear[data.highwhiteid$treatment == "blackunsymp" | data.highwhiteid$treatment == "blacksymp"], 
                                       data.highwhiteid$fear[data.highwhiteid$treatment == "whiteunsymp" | data.highwhiteid$treatment == "whitesymp"], 
                                       alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.fear.highwhiteid

test.white.fear.lowwhiteid <- t.test(data.lowwhiteid$fear[data.lowwhiteid$treatment == "blackunsymp" | data.lowwhiteid$treatment == "blacksymp"], 
                                      data.lowwhiteid$fear[data.lowwhiteid$treatment == "whiteunsymp" | data.lowwhiteid$treatment == "whitesymp"], 
                                      alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.fear.lowwhiteid

## disgust
test.white.disgust.highwhiteid <- t.test(data.highwhiteid$disgust[data.highwhiteid$treatment == "blackunsymp" | data.highwhiteid$treatment == "blacksymp"], 
                                      data.highwhiteid$disgust[data.highwhiteid$treatment == "whiteunsymp" | data.highwhiteid$treatment == "whitesymp"], 
                                      alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.disgust.highwhiteid

test.white.disgust.lowwhiteid <- t.test(data.lowwhiteid$disgust[data.lowwhiteid$treatment == "blackunsymp" | data.lowwhiteid$treatment == "blacksymp"], 
                                     data.lowwhiteid$disgust[data.lowwhiteid$treatment == "whiteunsymp" | data.lowwhiteid$treatment == "whitesymp"], 
                                     alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.disgust.lowwhiteid

## sympathy 
test.white.sympathy.highwhiteid <- t.test(data.highwhiteid$sympathy[data.highwhiteid$treatment == "blackunsymp" | data.highwhiteid$treatment == "blacksymp"], 
                                         data.highwhiteid$sympathy[data.highwhiteid$treatment == "whiteunsymp" | data.highwhiteid$treatment == "whitesymp"], 
                                         alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.sympathy.highwhiteid

test.white.sympathy.lowwhiteid <- t.test(data.lowwhiteid$sympathy[data.lowwhiteid$treatment == "blackunsymp" | data.lowwhiteid$treatment == "blacksymp"], 
                                        data.lowwhiteid$sympathy[data.lowwhiteid$treatment == "whiteunsymp" | data.lowwhiteid$treatment == "whitesymp"], 
                                        alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.sympathy.lowwhiteid

## compassion
test.white.compassion.highwhiteid <- t.test(data.highwhiteid$compassion[data.highwhiteid$treatment == "blackunsymp" | data.highwhiteid$treatment == "blacksymp"], 
                                          data.highwhiteid$compassion[data.highwhiteid$treatment == "whiteunsymp" | data.highwhiteid$treatment == "whitesymp"], 
                                          alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.compassion.highwhiteid

test.white.compassion.lowwhiteid <- t.test(data.lowwhiteid$compassion[data.lowwhiteid$treatment == "blackunsymp" | data.lowwhiteid$treatment == "blacksymp"], 
                                         data.lowwhiteid$compassion[data.lowwhiteid$treatment == "whiteunsymp" | data.lowwhiteid$treatment == "whitesymp"], 
                                         alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.compassion.lowwhiteid

## pity
test.white.pity.highwhiteid <- t.test(data.highwhiteid$pity[data.highwhiteid$treatment == "blackunsymp" | data.highwhiteid$treatment == "blacksymp"], 
                                            data.highwhiteid$pity[data.highwhiteid$treatment == "whiteunsymp" | data.highwhiteid$treatment == "whitesymp"], 
                                            alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.pity.highwhiteid

test.white.pity.lowwhiteid <- t.test(data.lowwhiteid$pity[data.lowwhiteid$treatment == "blackunsymp" | data.lowwhiteid$treatment == "blacksymp"], 
                                           data.lowwhiteid$pity[data.lowwhiteid$treatment == "whiteunsymp" | data.lowwhiteid$treatment == "whitesymp"], 
                                           alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.pity.lowwhiteid

################################
## Code low and high racial stereotype subsets ##
################################
summary(data.pretest$workstereotypediff) ## 17 missing
quantile(data.pretest$workstereotypediff, c(0, 0.5), na.rm = T)

data.highrs <- data.pretest[!is.na(data.pretest$workstereotypediff) & data.pretest$workstereotypediff >= 0.1, ] ## 180 responses
data.lowrs <- data.pretest[!is.na(data.pretest$workstereotypediff) & data.pretest$workstereotypediff < 0.1, ] ## 139 responses

############################
## Emotional responses (black vs. white conditions) by racial stereotypes
############################

## anger 
test.white.anger.highrs <- t.test(data.highrs$anger[data.highrs$treatment == "blackunsymp" | data.highrs$treatment == "blacksymp"], 
                                  data.highrs$anger[data.highrs$treatment == "whiteunsymp" | data.highrs$treatment == "whitesymp"], 
                                  alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.anger.highrs

test.white.anger.lowrs <- t.test(data.lowrs$anger[data.lowrs$treatment == "blackunsymp" | data.lowrs$treatment == "blacksymp"], 
                                 data.lowrs$anger[data.lowrs$treatment == "whiteunsymp" | data.lowrs$treatment == "whitesymp"], 
                                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.anger.lowrs


## fear

test.white.fear.highrs <- t.test(data.highrs$fear[data.highrs$treatment == "blackunsymp" | data.highrs$treatment == "blacksymp"], 
                                 data.highrs$fear[data.highrs$treatment == "whiteunsymp" | data.highrs$treatment == "whitesymp"], 
                                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.fear.highrs

test.white.fear.lowrs <- t.test(data.lowrs$fear[data.lowrs$treatment == "blackunsymp" | data.lowrs$treatment == "blacksymp"], 
                                data.lowrs$fear[data.lowrs$treatment == "whiteunsymp" | data.lowrs$treatment == "whitesymp"], 
                                alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.fear.lowrs

## disgust
test.white.disgust.highrs <- t.test(data.highrs$disgust[data.highrs$treatment == "blackunsymp" | data.highrs$treatment == "blacksymp"], 
                                    data.highrs$disgust[data.highrs$treatment == "whiteunsymp" | data.highrs$treatment == "whitesymp"], 
                                    alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.disgust.highrs

test.white.disgust.lowrs <- t.test(data.lowrs$disgust[data.lowrs$treatment == "blackunsymp" | data.lowrs$treatment == "blacksymp"], 
                                   data.lowrs$disgust[data.lowrs$treatment == "whiteunsymp" | data.lowrs$treatment == "whitesymp"], 
                                   alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.disgust.lowrs

## sympathy 
test.white.sympathy.highrs <- t.test(data.highrs$sympathy[data.highrs$treatment == "blackunsymp" | data.highrs$treatment == "blacksymp"], 
                                     data.highrs$sympathy[data.highrs$treatment == "whiteunsymp" | data.highrs$treatment == "whitesymp"], 
                                     alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.sympathy.highrs

test.white.sympathy.lowrs <- t.test(data.lowrs$sympathy[data.lowrs$treatment == "blackunsymp" | data.lowrs$treatment == "blacksymp"], 
                                    data.lowrs$sympathy[data.lowrs$treatment == "whiteunsymp" | data.lowrs$treatment == "whitesymp"], 
                                    alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.sympathy.lowrs

## compassion
test.white.compassion.highrs <- t.test(data.highrs$compassion[data.highrs$treatment == "blackunsymp" | data.highrs$treatment == "blacksymp"], 
                                       data.highrs$compassion[data.highrs$treatment == "whiteunsymp" | data.highrs$treatment == "whitesymp"], 
                                       alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.compassion.highrs

test.white.compassion.lowrs <- t.test(data.lowrs$compassion[data.lowrs$treatment == "blackunsymp" | data.lowrs$treatment == "blacksymp"], 
                                      data.lowrs$compassion[data.lowrs$treatment == "whiteunsymp" | data.lowrs$treatment == "whitesymp"], 
                                      alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.compassion.lowrs

## pity
test.white.pity.highrs <- t.test(data.highrs$pity[data.highrs$treatment == "blackunsymp" | data.highrs$treatment == "blacksymp"], 
                                 data.highrs$pity[data.highrs$treatment == "whiteunsymp" | data.highrs$treatment == "whitesymp"], 
                                 alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.pity.highrs

test.white.pity.lowrs <- t.test(data.lowrs$pity[data.lowrs$treatment == "blackunsymp" | data.lowrs$treatment == "blacksymp"], 
                                data.lowrs$pity[data.lowrs$treatment == "whiteunsymp" | data.lowrs$treatment == "whitesymp"], 
                                alternative = c("two.sided"), var.equal = TRUE, conf.level = 0.83)
test.white.pity.lowrs

################################
## Appendix, Section A4.1
## Mike's race (pre-test)
################################

## Percentage who perceived Mike as White in White conditions (97%)
round(mean(data.pretest$mikewhite[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], na.rm = T),2)

## Percentage who perceived Mike as White in Black conditions (6%)
round(mean(data.pretest$mikewhite[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], na.rm = T),2)

## Percentage who perceived Mike as Black in Black conditions (93%)
round(mean(data.pretest$mikeblack[data.pretest$treatment == "blackunsymp" | data.pretest$treatment == "blacksymp"], na.rm = T),2)

## Percentage who perceived Mike as Black in White conditions (1%)
round(mean(data.pretest$mikeblack[data.pretest$treatment == "whiteunsymp" | data.pretest$treatment == "whitesymp"], na.rm = T),2)

################################
## Appendix, Section A7 (Foonotes 5)
## CACE (main study)
## Calculate a more lenient compliance measure
################################

data.main$overrepwhiteaddictsLenient <- ifelse(data.main$perwhiteaddicts >= .64-(.64*.1), 1, 0)
table(data.main$overrepwhiteaddictsLenient)
table(data.main$overrepwhiteaddicts)

data.main$overrepblackaddictsLenient <- ifelse(data.main$perblackaddicts >= .12-(.12*.1), 1, 0)
table(data.main$overrepblackaddictsLenient)
table(data.main$overrepblackaddicts)
# Note: lenient definition for Black addicts does not change compliance values, run analysis for strict def. only 

################################
## Appendix, Section A7 (Footnotes 6, 7, and 8)
## CACE (main study)
## Mean and se of proportion over-estimating 
## White and Black addicts in pooled racial conditions
################################

## Respondents who over-report White addicts
v <- "overrepwhiteaddicts" 

## Proportion and se over-estimated White addicts for pooled White conditions (rounded to third decimal place)
## reported in footnote 6
mean.1.pw <-round(mean(data.main[[v]][data.main$treat == "Sympathetic White"|data.main$treat == "Unsympathetic White"], na.rm =T), 3) ## 0.443
se.1.pw <-round(se(data.main[[v]][data.main$treat == "Sympathetic White"|data.main$treat == "Unsympathetic White"]),3) ## 0.02

## Proportion and se over-estimated White addicts for pooled Black conditions (rounded to third decimal place)
## reported in footnotef 6
mean.2.pw <-round(mean(data.main[[v]][data.main$treat == "Sympathetic Black"|data.main$treat == "Unsympathetic Black"], na.rm =T), 3) ## 0.15
se.2.pw <-round(se(data.main[[v]][data.main$treat == "Sympathetic Black"|data.main$treat == "Unsympathetic Black"]), 3)  ## 0.014

## Respondents who over-report Black addicts
v2 <- "overrepblackaddicts"

## Proportion and se over-estimated Black addicts for pooled White conditions (rounded to third decimal place)
## reported in footnote 7
mean.3.pw <-round(mean(data.main[[v2]][data.main$treat == "Sympathetic White"|data.main$treat == "Unsympathetic White"], na.rm =T), 3) ## 0.788
se.3.pw <-round(se(data.main[[v2]][data.main$treat == "Sympathetic White"|data.main$treat == "Unsympathetic White"]), 3)  ## 0.017

## Proportion and se over-estimated White addicts for pooled Black conditions (rounded to third decimal place)
## reported in footnote 7
mean.4.pw <-round(mean(data.main[[v2]][data.main$treat == "Sympathetic Black"|data.main$treat == "Unsympathetic Black"], na.rm =T), 3) ## 0.957
se.4.pw <-round(se(data.main[[v2]][data.main$treat == "Sympathetic Black"|data.main$treat == "Unsympathetic Black"]), 3) ## 0.008

## Raw proportion of respondents who over-report White addicts
v3 <- "perwhiteaddicts" 

## Raw proportion and se estimated White addicts for pooled White conditions (rounded to third decimal place)
## reported in footnote 8
mean.5.pw <-round(mean(data.main[[v3]][data.main$treat == "Sympathetic White"|data.main$treat == "Unsympathetic White"], na.rm =T), 3) ## 0.578
se.5.pw <-round(se(data.main[[v3]][data.main$treat == "Sympathetic White"|data.main$treat == "Unsympathetic White"]), 3) ## 0.007

##  Raw proportion and se estimated White addicts for pooled Black conditions (rounded to third decimal place)
## reported in footnote 8
mean.6.pw <-round(mean(data.main[[v3]][data.main$treat == "Sympathetic Black"|data.main$treat == "Unsympathetic Black"], na.rm =T), 3) ## 0.441
se.6.pw <-round(se(data.main[[v3]][data.main$treat == "Sympathetic Black"|data.main$treat == "Unsympathetic Black"]), 3) ## 0.007

## Raw proportion of respondents who over-report Black addicts
v4 <- "perblackaddicts"

## Raw proportion and se estimated Black addicts for pooled White conditions (rounded to third decimal place)
## reported in footnote 8
mean.7.pw <-round(mean(data.main[[v4]][data.main$treat == "Sympathetic White"|data.main$treat == "Unsympathetic White"], na.rm =T), 3) ## 0.245
se.7.pw <-round(se(data.main[[v4]][data.main$treat == "Sympathetic White"|data.main$treat == "Unsympathetic White"]), 3) ## 0.006

## Raw proportion and se estimated addicts for pooled Black conditions (rounded to third decimal place)
## reported in footnote 8
mean.8.pw <-round(mean(data.main[[v4]][data.main$treat == "Sympathetic Black"|data.main$treat == "Unsympathetic Black"], na.rm =T), 3) ## 0.349
se.8.pw <-round(se(data.main[[v4]][data.main$treat == "Sympathetic Black"|data.main$treat == "Unsympathetic Black"]), 3) ## 0.006

################################
## Appendix, Section A7 (Footnotes 9, 10, 11)
## CACE (main study)
## 2SLS regression results
################################

#####################
# 2SLS regression results
#####################
# re-estimate eqs 7 and 3 with 2SLS (2 stage least squares regression)

# subset to pooled white and pooled black only
data.pool <- subset(data.main, pooledwhite ==1 | pooledblack == 1)

# pretreatment covariates
ptcov <- "age + as.factor(region) + as.factor(hsgrad) + as.factor(somecollege) + as.factor(collegegrad) + as.factor(postgrad) + as.factor(female) + as.factor(d25kto34k) + as.factor(d35kto49k) + as.factor(d50kto74k) + as.factor(d75kto99k) + as.factor(d100kto149k) + as.factor(d150kormore) + pid7 + ideo"

# outcome variables
outc.1 <- "policy"
outc.2 <- "candidatesupport"
outc.3 <- "taxesfortreatment"
outc.4 <- "emotionscale"

# compliance variable in version 1
c.1 <- "overrepblackaddicts"
# compliance variable in version 2
c.2 <- "overrepwhiteaddicts"

# treatment in version 1
t.1 <- "pooledblack"
# treatment in version 2 
t.2 <- "pooledwhite"

# set formulas for equation 7 (pooled white vs. pooled black conditions)
# version 1
form.v1m1 <- formula(paste(outc.1, "~", c.1, "+" , ptcov, "|", t.1, "+", ptcov))
form.v1m2 <- formula(paste(outc.2, "~", c.1, "+" , ptcov, "|", t.1, "+", ptcov))
form.v1m3 <- formula(paste(outc.3, "~", c.1, "+" , ptcov, "|", t.1, "+", ptcov))
form.v1m4 <- formula(paste(outc.4, "~", c.1, "+" , ptcov, "|", t.1, "+", ptcov))

# version 2
form.v2m1 <- formula(paste(outc.1, "~", c.2, "+" , ptcov, "|", t.2, "+", ptcov))
form.v2m2 <- formula(paste(outc.2, "~", c.2, "+" , ptcov, "|", t.2, "+", ptcov))
form.v2m3 <- formula(paste(outc.3, "~", c.2, "+" , ptcov, "|", t.2, "+", ptcov))
form.v2m4 <- formula(paste(outc.4, "~", c.2, "+" , ptcov, "|", t.2, "+", ptcov))

# first stage formulas
form.v1m1.fs <- formula(paste(c.1, "~", t.1, "+", ptcov))
form.v2m1.fs <- formula(paste(c.2, "~", t.2, "+", ptcov))

#####################
# Version 1: T = 1 if black addicts stories. T = 0 if white addicts stories. 
#####################

#####################
# Equation 7 (Racial main effect hypothesis)
#####################

# outcome 1 - policy
v1m1 <- ivreg(form.v1m1, data = data.pool)
v1m1fs <- lm(form.v1m1.fs, data = data.pool)

# outcome 2 - candidate
v1m2 <- ivreg(form.v1m2, data = data.pool)
v1m2fs <- lm(form.v1m1.fs, data = data.pool[! is.na(data.pool$candidatesupport),])

# outcome 3 - taxes
v1m3 <- ivreg(form.v1m3, data = data.pool)
v1m3fs <- lm(form.v1m1.fs, data = data.pool)

# outcome 4
v1m4 <- ivreg(form.v1m4, data = data.pool)
v1m4fs <- lm(form.v1m1.fs, data = data.pool)

# View table reporting stage 2 results (re-estimate equation 7) 
## Coefficients and values reported in footnote 9
stargazer(v1m1, v1m2, 
          v1m3,  v1m4,
          type = "text", 
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA, omit.table.layout = "n")

#####################
# Equation 3 (Racial sympathy hypothesis)
#####################

# Change treatments to racial sympathy treatments
# treatment in version 1
t.1 <- "sb"
# treatment in version 2 
t.2 <- "sw"

# subset to symp white and symp black only
data.symp <- subset(data.main, sw ==1 | sb == 1)

# outcome 1 - policy
v1m1.symp <- ivreg(form.v1m1, data = data.symp)
v1m1fs.symp <- lm(form.v1m1.fs, data = data.symp)

# outcome 2 - candidate
v1m2.symp <- ivreg(form.v1m2, data = data.symp)
v1m2fs.symp <- lm(form.v1m1.fs, data = data.symp[! is.na(data.symp$candidatesupport),])

# outcome 3 - taxes
v1m3.symp <- ivreg(form.v1m3, data = data.symp)
v1m3fs.symp <- lm(form.v1m1.fs, data = data.symp)


# outcome 4
v1m4.symp <- ivreg(form.v1m4, data = data.symp)
v1m4fs.symp <- lm(form.v1m1.fs, data = data.symp)


# View table reporting stage 2 results (re-estimate equation 3) 
## Coefficients and values reported in footnote 10
stargazer(v1m1.symp, v1m2.symp, 
          v1m3.symp,  v1m4.symp,
          type = "text", 
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA, omit.table.layout = "n")


#####################
# Version 2: T = 1 if white addicts stories. T = 0 if black addicts stories. 
#####################

## Revert back to pooled race treatments
# treatment in version 1
t.1 <- "pooledblack"
# treatment in version 2 
t.2 <- "pooledwhite"

# outcome 1 - policy
v2m1 <- ivreg(form.v2m1, data = data.pool)
v2m1fs <- lm(form.v2m1.fs, data = data.pool[! is.na(data.pool$policy),])


# outcome 2 - candidate
v2m2 <- ivreg(form.v2m2, data = data.pool)
v2m2fs <- lm(form.v2m1.fs, data = data.pool[! is.na(data.pool$candidatesupport),])


# outcome 3 - taxes
v2m3 <- ivreg(form.v2m3, data = data.pool)
v2m3fs <- lm(form.v2m1.fs, data = data.pool[! is.na(data.pool$taxesfortreatment),])

# outcome 4
v2m4 <- ivreg(form.v2m4, data = data.pool)
v2m4fs <- lm(form.v2m1.fs, data = data.pool[! is.na(data.pool$emotionscale),])


# View table reporting stage 2 results (re-estimate equation 7) 
## Coefficients and values reported in footnote 11
stargazer(v2m1, v2m2, 
          v2m3,  v2m4,
          type = "text", 
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA, omit.table.layout = "n")

#####################
# Equation 3 (Racial sympathy hypothesis)
#####################

# Change treatments to racial sympathy treatments
# treatment in version 1
t.1 <- "sb"
# treatment in version 2 
t.2 <- "sw"


# outcome 1 - policy
v2m1.symp <- ivreg(form.v2m1, data = data.symp)
v2m1fs.symp <- lm(form.v2m1.fs, data = data.symp)

# outcome 2 - candidate
v2m2.symp <- ivreg(form.v2m2, data = data.symp)
v2m2fs.symp <- lm(form.v2m1.fs, data = data.symp[! is.na(data.symp$candidatesupport),])

# outcome 3 - taxes
v2m3.symp <- ivreg(form.v2m3, data = data.symp)
v2m3fs.symp <- lm(form.v2m1.fs, data = data.symp)


# outcome 4
v2m4.symp <- ivreg(form.v2m4, data = data.symp)
v2m4fs.symp <- lm(form.v2m1.fs, data = data.symp)


# View table reporting stage 2 results (re-estimate equation 3) 
## Coefficients and values reported in footnote 11
stargazer(v2m1.symp, v2m2.symp, 
          v2m3.symp,  v2m4.symp,
          type = "text", 
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA, omit.table.layout = "n")

################################
## Appendix, Section A8 
## News exposure (main study)
## Racial main effect (Equation 7) on policy
## for respondents with low news exposure
################################

#####################
# subset to low news exposure respondents
#####################
table(data.main$opioidnews)

# subset
data <- subset(data.main, opioidnews %in% c("Not too closely", "Not closely at all"))

#####################################################
## Eq 7: Racial main effect hypothesis
#####################################################

###############
## DV 1: policy 
###############

eq7.policy.controls <- glm(policy ~ pooledblack
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$pooledwhite == 1| data$pooledblack== 1, ])
summary(eq7.policy.controls)


## calculate treatment effect as predicted probability 
test <- predicts(eq7.policy.controls, "0-1,1; mean; F(2); mean; mean; mean; mean; mean; mean;
                 mean; mean; mean; mean; mean; mean; mean", position = 1, sim.count = 10000, conf.int = 0.95,
                 sigma = NULL, set.seed = NULL)

pooledblackpolicy.eff <- -1*test$dc_mean
pooledblackpolicy.eff ## 10 percentage points