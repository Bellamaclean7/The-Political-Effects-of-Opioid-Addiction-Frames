####################################################################################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Appendix
## Tables: A11-A20
## Steps: interactive regression models for each moderator, print tables
####################################################################################
rm(list=ls()) ## clear list space

#####################
### Packages ########
#####################

library(stargazer) ## print tables

#####################
### Functions ########
#####################
mod_stargazer <- function(output.file, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=TRUE)
}

#####################
### Load Data ########
#####################

## read in data
data <- read.csv("tessdata.csv", head = T) ## 1517 obs


####################################
## Moderator 1: Racial resentment
####################################

####################################
## Create tercile bins of RR
####################################

## dummy for high RR
data$highRR <- 0
data$highRR[data$RR >= 0.75] <- 1

## dummy for middle RR
data$midRR <- 0
data$midRR[data$RR >= 0.375 & data$RR < .75] <- 1

## dataset of all respondents, except those with RR = NA
data.RR <- subset(data, !is.na(RR)) ## 1515 obs.

##########################
## Equation 1
##########################

################
## policy
################

eq1.policy.rr<- glm(policy ~ sw*highRR + sb*highRR + sw*midRR + sb*midRR + 
                      + age + region + hsgrad + somecollege + collegegrad + postgrad
                    + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                    family = "binomial", data = data.RR[data.RR$ns == 1| data.RR$sw == 1| data.RR$sb == 1, ])
# summary(eq1.policy.rr)



################
## taxes
################

eq1.taxes.rr<- lm(taxesfortreatment ~ sw*highRR + sb*highRR + sw*midRR + sb*midRR + 
                    + age + region + hsgrad + somecollege + collegegrad + postgrad
                  + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                  data = data.RR[data.RR$ns == 1| data.RR$sw == 1| data.RR$sb == 1, ])
# summary(eq1.taxes.rr)

################
## candidate
################

eq1.candidate.rr<- lm(candidatesupport~ sw*highRR + sb*highRR  + sw*midRR + sb*midRR
                      + age + region + hsgrad + somecollege + collegegrad + postgrad
                      + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                      data = data.RR[data.RR$ns == 1| data.RR$sw == 1| data.RR$sb == 1, ])
# summary(eq1.candidate.rr)

################
## Feelings
################
eq1.emotions.rr<- lm(emotionscale ~ sw*highRR + sb*highRR  + sw*midRR + sb*midRR
                     + age + region + hsgrad + somecollege + collegegrad + postgrad
                     + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                     data = data.RR[data.RR$ns == 1| data.RR$sw == 1| data.RR$sb == 1, ])
# summary(eq1.emotions.rr)


##########################
## Equation 2
##########################


################
## policy
################

eq2.policy.rr<- glm(policy ~ sb*highRR + sb*midRR + 
                      + age + region + hsgrad + somecollege + collegegrad + postgrad
                    + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                    family = "binomial", data = data.RR[data.RR$sw == 1| data.RR$sb == 1, ])
# summary(eq2.policy.rr)


################
## taxes
################

eq2.taxes.rr<- lm(taxesfortreatment ~ sb*highRR + sb*midRR + 
                    + age + region + hsgrad + somecollege + collegegrad + postgrad
                  + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                  data = data.RR[data.RR$sw == 1| data.RR$sb == 1, ])
# summary(eq2.taxes.rr)

################
## candidate
################


eq2.candidate.rr<- lm(candidatesupport ~ sb*highRR + sb*midRR +   
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                      + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                      data = data.RR[data.RR$sw == 1| data.RR$sb == 1, ])
# summary(eq2.candidate.rr)


################
## Feelings
################

eq2.emotions.rr<- lm(emotionscale ~ sb*highRR  + sb*midRR + 
                       + age + region + hsgrad + somecollege + collegegrad + postgrad
                     + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                     data = data.RR[ data.RR$sw == 1| data.RR$sb == 1, ])
# summary(eq2.emotions.rr)


##########################
## Equation 3
##########################

################
## policy
################

eq3.policy.rr<- glm(policy ~ uw*highRR + ub*highRR  + uw*midRR + ub*midRR + 
                      + age + region + hsgrad + somecollege + collegegrad + postgrad
                    + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                    family = "binomial", data = data.RR[data.RR$ns == 1| data.RR$uw == 1| data.RR$ub == 1, ])
# summary(eq3.policy.rr)



################
## taxes
################

eq3.taxes.rr<- lm(taxesfortreatment ~ uw*highRR + ub*highRR  + uw*midRR + ub*midRR +
                    + age + region + hsgrad + somecollege + collegegrad + postgrad
                  + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                  data = data.RR[data.RR$ns == 1| data.RR$uw == 1| data.RR$ub == 1, ])
# summary(eq3.taxes.rr)

################
## candidate
################

eq3.candidate.rr<- lm(candidatesupport~ uw*highRR + ub*highRR  + uw*midRR + ub*midRR +
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                      + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                      data = data.RR[data.RR$ns == 1| data.RR$uw == 1| data.RR$ub == 1, ])
# summary(eq3.candidate.rr)

################
## Feelings
################
 
eq3.emotions.rr<- lm(emotionscale ~ uw*highRR + ub*highRR  + uw*midRR + ub*midRR +
                       + age + region + hsgrad + somecollege + collegegrad + postgrad
                     + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                     data = data.RR[data.RR$ns == 1| data.RR$uw == 1| data.RR$ub == 1, ])
# summary(eq3.emotions.rr)


##########################
## Equation 4
##########################


################
## policy
################

eq4.policy.rr<- glm(policy ~ ub*highRR + ub*midRR + 
                      + age + region + hsgrad + somecollege + collegegrad + postgrad
                    + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                    family = "binomial", data = data.RR[data.RR$uw == 1| data.RR$ub == 1, ])
# summary(eq4.policy.rr)


################
## taxes
################

eq4.taxes.rr<- lm(taxesfortreatment ~ ub*highRR + ub*midRR + 
                    + age + region + hsgrad + somecollege + collegegrad + postgrad
                  + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                  data = data.RR[data.RR$uw == 1| data.RR$ub == 1, ])
# summary(eq4.taxes.rr)

################
## candidate
################

eq4.candidate.rr<- lm(candidatesupport ~ ub*highRR + ub*midRR + 
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                      + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                      data = data.RR[data.RR$uw == 1| data.RR$ub == 1, ])
# summary(eq4.candidate.rr)


################
## Feelings
################

eq4.emotions.rr<- lm(emotionscale ~ ub*highRR + ub*midRR + 
                       + age + region + hsgrad + somecollege + collegegrad + postgrad
                     + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                     data = data.RR[ data.RR$uw == 1| data.RR$ub == 1, ])
# summary(eq4.emotions.rr)


##########################
## Equation 7
##########################

################
## policy
################

eq7.policy.rr<- glm(policy ~ pooledblack*highRR + pooledblack*midRR + 
                      + age + region + hsgrad + somecollege + collegegrad + postgrad
                    + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                    family = "binomial", data = data.RR[data.RR$pooledwhite == 1| data.RR$pooledblack == 1, ])
# summary(eq7.policy.rr)


################
## taxes
################


eq7.taxes.rr<- lm(taxesfortreatment ~ pooledblack*highRR + pooledblack*midRR + 
                    + age + region + hsgrad + somecollege + collegegrad + postgrad
                  + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                  data = data.RR[data.RR$pooledwhite == 1| data.RR$pooledblack == 1, ])
# summary(eq7.taxes.rr)

################
## candidate
################

eq7.candidate.rr<- lm(candidatesupport ~ pooledblack*highRR + pooledblack*midRR +  
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                      + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                      data = data.RR[data.RR$pooledwhite == 1| data.RR$pooledblack == 1, ])
# summary(eq7.candidate.rr)


################
## Feelings
################


eq7.emotions.rr<- lm(emotionscale ~ pooledblack*highRR + pooledblack*midRR + 
                       + age + region + hsgrad + somecollege + collegegrad + postgrad
                     + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                     data = data.RR[ data.RR$pooledwhite == 1| data.RR$pooledblack == 1, ])
# summary(eq7.emotions.rr)


####################################
## Moderator 2: White identity
####################################


####################################
## Create tercile bins of whiteid
####################################

## dummy for high whiteid
data$highwhiteid <- 0
data$highwhiteid[data$whiteid >= 0.5] <- 1

## dummy for middle white id
data$midwhiteid <- 0
data$midwhiteid[data$whiteid > 0 & data$whiteid < 0.5] <- 1

## dataset of all respondents, except those with whiteid = NA
data.whiteid <- subset(data, !is.na(whiteid))

##########################
## Equation 1
##########################

################
## policy
################


eq1.policy.whiteid<- glm(policy ~ sw*highwhiteid + sb*highwhiteid + sw*midwhiteid + sb*midwhiteid 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         family = "binomial", data = data.whiteid[data.whiteid$ns == 1| data.whiteid$sw == 1| data.whiteid$sb == 1, ])
# summary(eq1.policy.whiteid)

################
## taxes
################

eq1.taxes.whiteid<- lm(taxesfortreatment ~ sw*highwhiteid + sb*highwhiteid + sw*midwhiteid + sb*midwhiteid 
                       + age + region + hsgrad + somecollege + collegegrad + postgrad
                       + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                       data = data.whiteid[data.whiteid$ns == 1| data.whiteid$sw == 1| data.whiteid$sb == 1, ])
# summary(eq1.taxes.whiteid)

################
## candidate
################

eq1.candidate.whiteid<- lm(candidatesupport~ sw*highwhiteid + sb*highwhiteid + sw*midwhiteid + sb*midwhiteid 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           data = data.whiteid[data.whiteid$ns == 1| data.whiteid$sw == 1| data.whiteid$sb == 1, ])
# summary(eq1.candidate.whiteid)

################
## Feelings
################

eq1.emotions.whiteid<- lm(emotionscale ~ sw*highwhiteid + sb*highwhiteid + sw*midwhiteid + sb*midwhiteid 
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                          data = data.whiteid[data.whiteid$ns == 1| data.whiteid$sw == 1| data.whiteid$sb == 1, ])
# summary(eq1.emotions.whiteid)


##########################
## Equation 2
##########################


################
## policy
################

eq2.policy.whiteid<- glm(policy ~ sb*highwhiteid + sb*midwhiteid
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         family = "binomial", data = data.whiteid[data.whiteid$sw == 1| data.whiteid$sb == 1, ])
# summary(eq2.policy.whiteid)


################
## taxes
################

eq2.taxes.whiteid<- lm(taxesfortreatment ~ sb*highwhiteid + sb*midwhiteid
                       + age + region + hsgrad + somecollege + collegegrad + postgrad
                       + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                       data = data.whiteid[data.whiteid$sw == 1| data.whiteid$sb == 1, ])
# summary(eq2.taxes.whiteid)

################
## candidate
################


eq2.candidate.whiteid<- lm(candidatesupport ~ sb*highwhiteid  + sb*midwhiteid
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                           data = data.whiteid[data.whiteid$sw == 1| data.whiteid$sb == 1, ])
# summary(eq2.candidate.whiteid)


################
## Feelings
################

eq2.emotions.whiteid<- lm(emotionscale ~ sb*highwhiteid + sb*midwhiteid
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                          data = data.whiteid[ data.whiteid$sw == 1| data.whiteid$sb == 1, ])
# summary(eq2.emotions.whiteid)



##########################
## Equation 3
##########################

################
## policy
################

eq3.policy.whiteid<- glm(policy ~ uw*highwhiteid + ub*highwhiteid  + uw*midwhiteid + ub*midwhiteid 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         family = "binomial", data = data.whiteid[data.whiteid$ns == 1| data.whiteid$uw == 1| data.whiteid$ub == 1, ])
# summary(eq3.policy.whiteid)



################
## taxes
################

eq3.taxes.whiteid<- lm(taxesfortreatment ~ uw*highwhiteid + ub*highwhiteid  + uw*midwhiteid + ub*midwhiteid 
                       + age + region + hsgrad + somecollege + collegegrad + postgrad
                       + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                       data = data.whiteid[data.whiteid$ns == 1| data.whiteid$uw == 1| data.whiteid$ub == 1, ])
# summary(eq3.taxes.whiteid)

################
## candidate
################

eq3.candidate.whiteid<- lm(candidatesupport~ uw*highwhiteid + ub*highwhiteid  + uw*midwhiteid + ub*midwhiteid 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           data = data.whiteid[data.whiteid$ns == 1| data.whiteid$uw == 1| data.whiteid$ub == 1, ])
# summary(eq3.candidate.whiteid)

################
## Feelings
################

eq3.emotions.whiteid<- lm(emotionscale ~ uw*highwhiteid + ub*highwhiteid  + uw*midwhiteid + ub*midwhiteid 
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                          data = data.whiteid[data.whiteid$ns == 1| data.whiteid$uw == 1| data.whiteid$ub == 1, ])
# summary(eq3.emotions.whiteid)

##########################
## Equation 4
##########################


################
## policy
################

eq4.policy.whiteid<- glm(policy ~ ub*highwhiteid + ub*midwhiteid
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         family = "binomial", data = data.whiteid[data.whiteid$uw == 1| data.whiteid$ub == 1, ])
# summary(eq4.policy.whiteid)


################
## taxes
################

eq4.taxes.whiteid<- lm(taxesfortreatment ~ ub*highwhiteid+ ub*midwhiteid
                       + age + region + hsgrad + somecollege + collegegrad + postgrad
                       + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                       data = data.whiteid[data.whiteid$uw == 1| data.whiteid$ub == 1, ])
# summary(eq4.taxes.whiteid)

################
## candidate
################


eq4.candidate.whiteid<- lm(candidatesupport ~ ub*highwhiteid   + ub*midwhiteid
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                           data = data.whiteid[data.whiteid$uw == 1| data.whiteid$ub == 1, ])
# summary(eq4.candidate.whiteid)


################
## Feelings
################

eq4.emotions.whiteid<- lm(emotionscale ~ ub*highwhiteid + ub*midwhiteid
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                          data = data.whiteid[ data.whiteid$uw == 1| data.whiteid$ub == 1, ])
# summary(eq4.emotions.whiteid)

##########################
## Equation 7
##########################

################
## policy
################

eq7.policy.whiteid<- glm(policy ~ pooledblack*highwhiteid + pooledblack*midwhiteid
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         family = "binomial", data = data.whiteid[data.whiteid$pooledwhite == 1| data.whiteid$pooledblack == 1, ])
# summary(eq7.policy.whiteid)


################
## taxes
################

eq7.taxes.whiteid<- lm(taxesfortreatment ~ pooledblack*highwhiteid  + pooledblack*midwhiteid
                       + age + region + hsgrad + somecollege + collegegrad + postgrad
                       + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                       data = data.whiteid[data.whiteid$pooledwhite == 1| data.whiteid$pooledblack == 1, ])
# summary(eq7.taxes.whiteid)

################
## candidate
################


eq7.candidate.whiteid<- lm(candidatesupport ~ pooledblack*highwhiteid   + pooledblack*midwhiteid
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                           data = data.whiteid[data.whiteid$pooledwhite == 1| data.whiteid$pooledblack == 1, ])
# summary(eq7.candidate.whiteid)


################
## Feelings
################

eq7.emotions.whiteid<- lm(emotionscale ~ pooledblack*highwhiteid + pooledblack*midwhiteid
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                          data = data.whiteid[ data.whiteid$pooledwhite == 1| data.whiteid$pooledblack == 1, ])
# summary(eq7.emotions.whiteid)

####################################
## Moderator 3: Racial stereotypes
####################################
## dummy for high RS
data$highRS <- 0
data$highRS[data$racialstereotype > 0] <- 1

## dataset of all respondents, except those with racialstereotype = NA
data.RS <- subset(data, ! is.na(racialstereotype))

##########################
## Equation 1
##########################

################
## policy
################
eq1.policy.RS<- glm(policy ~ sw*highRS + sb*highRS 
                    + age + region + hsgrad + somecollege + collegegrad + postgrad
                    + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                    family = "binomial", data = data.RS[data.RS$ns == 1| data.RS$sw == 1| data.RS$sb == 1, ])
# summary(eq1.policy.RS)

################
## taxes
################

eq1.taxes.RS<- lm(taxesfortreatment ~ sw*highRS + sb*highRS 
                  + age + region + hsgrad + somecollege + collegegrad + postgrad
                  + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                  data = data.RS[data.RS$ns == 1| data.RS$sw == 1| data.RS$sb == 1, ])
# summary(eq1.taxes.RS)

################
## candidate
################
eq1.candidate.RS<- lm(candidatesupport~ sw*highRS + sb*highRS 
                      + age + region + hsgrad + somecollege + collegegrad + postgrad
                      + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                      data = data.RS[data.RS$ns == 1| data.RS$sw == 1| data.RS$sb == 1, ])
# summary(eq1.candidate.RS)

################
## Feelings
################
eq1.emotions.RS<- lm(emotionscale ~ sw*highRS + sb*highRS 
                     + age + region + hsgrad + somecollege + collegegrad + postgrad
                     + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                     data = data.RS[data.RS$ns == 1| data.RS$sw == 1| data.RS$sb == 1, ])
# summary(eq1.emotions.RS)


##########################
## Equation 2
##########################


################
## policy
################

eq2.policy.RS<- glm(policy ~ sb*highRS
                    + age + region + hsgrad + somecollege + collegegrad + postgrad
                    + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                    family = "binomial", data = data.RS[data.RS$sw == 1| data.RS$sb == 1, ])
# summary(eq2.policy.RS)


################
## taxes
################


eq2.taxes.RS<- lm(taxesfortreatment ~ sb*highRS
                  + age + region + hsgrad + somecollege + collegegrad + postgrad
                  + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                  data = data.RS[data.RS$sw == 1| data.RS$sb == 1, ])
# summary(eq2.taxes.RS)

################
## candidate
################


eq2.candidate.RS<- lm(candidatesupport ~ sb*highRS  
                      + age + region + hsgrad + somecollege + collegegrad + postgrad
                      + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                      data = data.RS[data.RS$sw == 1| data.RS$sb == 1, ])
# summary(eq2.candidate.RS)


################
## Feelings
################


eq2.emotions.RS<- lm(emotionscale ~ sb*highRS
                     + age + region + hsgrad + somecollege + collegegrad + postgrad
                     + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                     data = data.RS[ data.RS$sw == 1| data.RS$sb == 1, ])
# summary(eq2.emotions.RS)

##########################
## Equation 3
##########################

################
## policy
################

eq3.policy.RS<- glm(policy ~ uw*highRS + ub*highRS 
                    + age + region + hsgrad + somecollege + collegegrad + postgrad
                    + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                    family = "binomial", data = data.RS[data.RS$ns == 1| data.RS$uw == 1| data.RS$ub == 1, ])
# summary(eq3.policy.RS)



################
## taxes
################

eq3.taxes.RS<- lm(taxesfortreatment ~ uw*highRS + ub*highRS 
                  + age + region + hsgrad + somecollege + collegegrad + postgrad
                  + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                  data = data.RS[data.RS$ns == 1| data.RS$uw == 1| data.RS$ub == 1, ])
# summary(eq3.taxes.RS)

################
## candidate
################

eq3.candidate.RS<- lm(candidatesupport~ uw*highRS + ub*highRS 
                      + age + region + hsgrad + somecollege + collegegrad + postgrad
                      + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                      data = data.RS[data.RS$ns == 1| data.RS$uw == 1| data.RS$ub == 1, ])
# summary(eq3.candidate.RS)

################
## Feelings
################

eq3.emotions.RS<- lm(emotionscale ~ uw*highRS + ub*highRS 
                     + age + region + hsgrad + somecollege + collegegrad + postgrad
                     + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                     data = data.RS[data.RS$ns == 1| data.RS$uw == 1| data.RS$ub == 1, ])
# summary(eq3.emotions.RS)

##########################
## Equation 4
##########################


################
## policy
################

eq4.policy.RS<- glm(policy ~ ub*highRS
                    + age + region + hsgrad + somecollege + collegegrad + postgrad
                    + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                    family = "binomial", data = data.RS[data.RS$uw == 1| data.RS$ub == 1, ])
# summary(eq4.policy.RS)


################
## taxes
################

eq4.taxes.RS<- lm(taxesfortreatment ~ ub*highRS
                  + age + region + hsgrad + somecollege + collegegrad + postgrad
                  + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                  data = data.RS[data.RS$uw == 1| data.RS$ub == 1, ])
# summary(eq4.taxes.RS)

################
## candidate
################


eq4.candidate.RS<- lm(candidatesupport ~ ub*highRS  
                      + age + region + hsgrad + somecollege + collegegrad + postgrad
                      + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                      data = data.RS[data.RS$uw == 1| data.RS$ub == 1, ])
# summary(eq4.candidate.RS)


################
## Feelings
################

eq4.emotions.RS<- lm(emotionscale ~ ub*highRS
                     + age + region + hsgrad + somecollege + collegegrad + postgrad
                     + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                     data = data.RS[ data.RS$uw == 1| data.RS$ub == 1, ])
# summary(eq4.emotions.RS)


##########################
## Equation 7
##########################


################
## policy
################

eq7.policy.RS<- glm(policy ~ pooledblack*highRS
                    + age + region + hsgrad + somecollege + collegegrad + postgrad
                    + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                    family = "binomial", data = data.RS[data.RS$pooledwhite == 1| data.RS$pooledblack == 1, ])
# summary(eq7.policy.RS)


################
## taxes
################
eq7.taxes.RS<- lm(taxesfortreatment ~ pooledblack*highRS
                  + age + region + hsgrad + somecollege + collegegrad + postgrad
                  + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                  data = data.RS[data.RS$pooledwhite == 1| data.RS$pooledblack == 1, ])
# summary(eq7.taxes.RS)

################
## candidate
################

eq7.candidate.RS<- lm(candidatesupport ~ pooledblack*highRS  
                      + age + region + hsgrad + somecollege + collegegrad + postgrad
                      + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                      data = data.RS[data.RS$pooledwhite == 1| data.RS$pooledblack == 1, ])
# summary(eq7.candidate.RS)


################
## Feelings
################

eq7.emotions.RS<- lm(emotionscale ~ pooledblack*highRS
                     + age + region + hsgrad + somecollege + collegegrad + postgrad
                     + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                     data = data.RS[ data.RS$pooledwhite == 1| data.RS$pooledblack == 1, ])
# summary(eq7.emotions.RS)


####################################
## Moderator 4: Ideology
####################################

## dataset of all respondents, except those with ideo = NA
data.ideology <- subset(data, !is.na(ideo))

##########################
## Equation 1
##########################

################
## policy
################

eq1.policy.ideology<- glm(policy ~ sw*conservative + sb*conservative +  sw*moderate + sb*moderate
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                          family = "binomial", data = data.ideology[data.ideology$ns == 1| data.ideology$sw == 1| data.ideology$sb == 1, ])
# summary(eq1.policy.ideology)

################
## taxes
################

eq1.taxes.ideology<- lm(taxesfortreatment ~ sw*conservative + sb*conservative +  sw*moderate + sb*moderate
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                        + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                        data = data.ideology[data.ideology$ns == 1| data.ideology$sw == 1| data.ideology$sb == 1, ])
# summary(eq1.taxes.ideology)

################
## candidate
################

eq1.candidate.ideology<- lm(candidatesupport~ sw*conservative + sb*conservative +  sw*moderate + sb*moderate
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                            data = data.ideology[data.ideology$ns == 1| data.ideology$sw == 1| data.ideology$sb == 1, ])
# summary(eq1.candidate.ideology)

################
## Feelings
################

eq1.emotions.ideology<- lm(emotionscale ~ sw*conservative + sb*conservative +  sw*moderate + sb*moderate
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                           data = data.ideology[data.ideology$ns == 1| data.ideology$sw == 1| data.ideology$sb == 1, ])
# summary(eq1.emotions.ideology)

##########################
## Equation 2
##########################

################
## policy
################

eq2.policy.ideology<- glm(policy ~ sb*conservative + sb*moderate
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                          family = "binomial", data = data.ideology[data.ideology$sw == 1| data.ideology$sb == 1, ])
# summary(eq2.policy.ideology)


################
## taxes
################

eq2.taxes.ideology<- lm(taxesfortreatment ~ sb*conservative + sb*moderate
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                        + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                        data = data.ideology[data.ideology$sw == 1| data.ideology$sb == 1, ])
# summary(eq2.taxes.ideology)

################
## candidate
################

eq2.candidate.ideology<- lm(candidatesupport ~ sb*conservative   + sb*moderate
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ RR + whiteid + racialstereotype, 
                            data = data.ideology[data.ideology$sw == 1| data.ideology$sb == 1, ])
# summary(eq2.candidate.ideology)


################
## Feelings
################

eq2.emotions.ideology<- lm(emotionscale ~ sb*conservative + sb*moderate
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                           data = data.ideology[ data.ideology$sw == 1| data.ideology$sb == 1, ])
# summary(eq2.emotions.ideology)

##########################
## Equation 3
##########################

################
## policy
################

eq3.policy.ideology<- glm(policy ~ uw*conservative + ub*conservative  +  uw*moderate + ub*moderate 
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                          family = "binomial", data = data.ideology[data.ideology$ns == 1| data.ideology$uw == 1| data.ideology$ub == 1, ])
# summary(eq3.policy.ideology)

################
## taxes
################

eq3.taxes.ideology<- lm(taxesfortreatment ~ uw*conservative + ub*conservative  +  uw*moderate + ub*moderate
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                        + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                        data = data.ideology[data.ideology$ns == 1| data.ideology$uw == 1| data.ideology$ub == 1, ])
# summary(eq3.taxes.ideology)

################
## candidate
################

eq3.candidate.ideology<- lm(candidatesupport~ uw*conservative + ub*conservative  +  uw*moderate + ub*moderate
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                            data = data.ideology[data.ideology$ns == 1| data.ideology$uw == 1| data.ideology$ub == 1, ])
# summary(eq3.candidate.ideology)

################
## Feelings
################

eq3.emotions.ideology<- lm(emotionscale ~ uw*conservative + ub*conservative  +  uw*moderate + ub*moderate
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                           data = data.ideology[data.ideology$ns == 1| data.ideology$uw == 1| data.ideology$ub == 1, ])
# summary(eq3.emotions.ideology)

##########################
## Equation 4
##########################

################
## policy
################

eq4.policy.ideology<- glm(policy ~ ub*conservative + ub*moderate
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                          family = "binomial", data = data.ideology[data.ideology$uw == 1| data.ideology$ub == 1, ])
# summary(eq4.policy.ideology)


################
## taxes
################

eq4.taxes.ideology<- lm(taxesfortreatment ~ ub*conservative + ub*moderate
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                        + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                        data = data.ideology[data.ideology$uw == 1| data.ideology$ub == 1, ])
# summary(eq4.taxes.ideology)

################
## candidate
################

eq4.candidate.ideology<- lm(candidatesupport ~ ub*conservative   + ub*moderate
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ RR + whiteid + racialstereotype, 
                            data = data.ideology[data.ideology$uw == 1| data.ideology$ub == 1, ])
# summary(eq4.candidate.ideology)


################
## Feelings
################


eq4.emotions.ideology<- lm(emotionscale ~ ub*conservative + ub*moderate
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                           data = data.ideology[ data.ideology$uw == 1| data.ideology$ub == 1, ])
# summary(eq4.emotions.ideology)

##########################
## Equation 7
##########################


################
## policy
################

eq7.policy.ideology<- glm(policy ~ pooledblack*conservative  +  pooledblack*moderate
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                          family = "binomial", data = data.ideology[data.ideology$pooledwhite == 1| data.ideology$pooledblack == 1, ])
# summary(eq7.policy.ideology)


################
## taxes
################

eq7.taxes.ideology<- lm(taxesfortreatment ~ pooledblack*conservative +  pooledblack*moderate
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                        + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                        data = data.ideology[data.ideology$pooledwhite == 1| data.ideology$pooledblack == 1, ])
# summary(eq7.taxes.ideology)

################
## candidate
################

eq7.candidate.ideology<- lm(candidatesupport ~ pooledblack*conservative   +  pooledblack*moderate
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ RR + whiteid + racialstereotype, 
                            data = data.ideology[data.ideology$pooledwhite == 1| data.ideology$pooledblack == 1, ])
# summary(eq7.candidate.ideology)


################
## Feelings
################

eq7.emotions.ideology<- lm(emotionscale ~ pooledblack*conservative +  pooledblack*moderate
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                           data = data.ideology[ data.ideology$pooledwhite == 1| data.ideology$pooledblack == 1, ])
# summary(eq7.emotions.ideology)

####################################
## Moderator 5: PID
####################################
## dataset of all respondents, except those with pid7 = NA
data.partisanship <- subset(data, !is.na(pid7))

##########################
## Equation 1
##########################

################
## policy
################

eq1.policy.partisan<- glm(policy ~ sw*republican + sb*republican + sw*independent + sb*independent 
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                          family = "binomial", data = data.partisanship[data.partisanship$ns == 1| data.partisanship$sw == 1| data.partisanship$sb == 1, ])
# summary(eq1.policy.partisan)

################
## taxes
################

eq1.taxes.partisan<- lm(taxesfortreatment ~ sw*republican + sb*republican + sw*independent + sb*independent 
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                        + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                        data = data.partisanship[data.partisanship$ns == 1| data.partisanship$sw == 1| data.partisanship$sb == 1, ])
# summary(eq1.taxes.partisan)

################
## candidate
################

eq1.candidate.partisan<- lm(candidatesupport~sw*republican + sb*republican + sw*independent + sb*independent 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                            data = data.partisanship[data.partisanship$ns == 1| data.partisanship$sw == 1| data.partisanship$sb == 1, ])
# summary(eq1.candidate.partisan)

################
## Feelings
################

eq1.emotions.partisan<- lm(emotionscale ~ sw*republican + sb*republican + sw*independent + sb*independent 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                           data = data.partisanship[data.partisanship$ns == 1| data.partisanship$sw == 1| data.partisanship$sb == 1, ])
# summary(eq1.emotions.partisan)


##########################
## Equation 2
##########################


################
## policy
################


eq2.policy.partisan<- glm(policy ~ sb*republican + sb*independent 
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                          family = "binomial", data = data.partisanship[data.partisanship$sw == 1| data.partisanship$sb == 1, ])
# summary(eq2.policy.partisan)


################
## taxes
################

eq2.taxes.partisan<- lm(taxesfortreatment ~ sb*republican+ sb*independent 
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                        + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                        data = data.partisanship[data.partisanship$sw == 1| data.partisanship$sb == 1, ])
# summary(eq2.taxes.partisan)

################
## candidate
################

eq2.candidate.partisan<- lm(candidatesupport ~ sb*republican  + sb*independent 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ RR + whiteid + racialstereotype, 
                            data = data.partisanship[data.partisanship$sw == 1| data.partisanship$sb == 1, ])
# summary(eq2.candidate.partisan)


################
## Feelings
################

eq2.emotions.partisan<- lm(emotionscale ~ sb*republican+ sb*independent 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                           data = data.partisanship[ data.partisanship$sw == 1| data.partisanship$sb == 1, ])
# summary(eq2.emotions.partisan)

##########################
## Equation 3
##########################

################
## policy
################

eq3.policy.partisan<- glm(policy ~ uw*republican + ub*republican +uw*independent + ub*independent 
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                          family = "binomial", data = data.partisanship[data.partisanship$ns == 1| data.partisanship$uw == 1| data.partisanship$ub == 1, ])
# summary(eq3.policy.partisan)



################
## taxes
################

eq3.taxes.partisan<- lm(taxesfortreatment ~ uw*republican + ub*republican   +uw*independent + ub*independent 
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                        + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                        data = data.partisanship[data.partisanship$ns == 1| data.partisanship$uw == 1| data.partisanship$ub == 1, ])
# summary(eq3.taxes.partisan)

################
## candidate
################

eq3.candidate.partisan<- lm(candidatesupport~ uw*republican + ub*republican   +uw*independent + ub*independent 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                            data = data.partisanship[data.partisanship$ns == 1| data.partisanship$uw == 1| data.partisanship$ub == 1, ])
# summary(eq3.candidate.partisan)

################
## Feelings
################

eq3.emotions.partisan<- lm(emotionscale ~ uw*republican + ub*republican  +uw*independent + ub*independent 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                           data = data.partisanship[data.partisanship$ns == 1| data.partisanship$uw == 1| data.partisanship$ub == 1, ])
# summary(eq3.emotions.partisan)

##########################
## Equation 4
##########################


################
## policy
################

eq4.policy.partisan<- glm(policy ~ ub*republican + ub*independent
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                          family = "binomial", data = data.partisanship[data.partisanship$uw == 1| data.partisanship$ub == 1, ])
# summary(eq4.policy.partisan)


################
## taxes
################

eq4.taxes.partisan<- lm(taxesfortreatment ~ ub*republican + ub*independent
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                        + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                        data = data.partisanship[data.partisanship$uw == 1| data.partisanship$ub == 1, ])
# summary(eq4.taxes.partisan)

################
## candidate
################

eq4.candidate.partisan<- lm(candidatesupport ~ ub*republican  + ub*independent
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ RR + whiteid + racialstereotype, 
                            data = data.partisanship[data.partisanship$uw == 1| data.partisanship$ub == 1, ])
# summary(eq4.candidate.partisan)


################
## Feelings
################

eq4.emotions.partisan<- lm(emotionscale ~ ub*republican + ub*independent
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                           data = data.partisanship[ data.partisanship$uw == 1| data.partisanship$ub == 1, ])
# summary(eq4.emotions.partisan)

##########################
## Equation 7
##########################


################
## policy
################

eq7.policy.partisan<- glm(policy ~ pooledblack*republican + pooledblack*independent
                          + age + region + hsgrad + somecollege + collegegrad + postgrad
                          + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                          family = "binomial", data = data.partisanship[data.partisanship$pooledwhite == 1| data.partisanship$pooledblack == 1, ])
# summary(eq7.policy.partisan)


################
## taxes
################


eq7.taxes.partisan<- lm(taxesfortreatment ~ pooledblack*republican+ pooledblack*independent
                        + age + region + hsgrad + somecollege + collegegrad + postgrad
                        + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                        data = data.partisanship[data.partisanship$pooledwhite == 1| data.partisanship$pooledblack == 1, ])
# summary(eq7.taxes.partisan)

################
## candidate
################
eq7.candidate.partisan<- lm(candidatesupport ~ pooledblack*republican  + pooledblack*independent
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ RR + whiteid + racialstereotype, 
                            data = data.partisanship[data.partisanship$pooledwhite == 1| data.partisanship$pooledblack == 1, ])
# summary(eq7.candidate.partisan)


################
## Feelings
################

eq7.emotions.partisan<- lm(emotionscale ~ pooledblack*republican + pooledblack*independent
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + RR + whiteid + racialstereotype, 
                           data = data.partisanship[ data.partisanship$pooledwhite == 1| data.partisanship$pooledblack == 1, ])
# summary(eq7.emotions.partisan)

################
## Print tables
################

## --- Racial resentment, stereotypes, white ID


## Note: Coefficients for RR, RS, and whiteID categories are presented on a single line in Tables A11-A15
## entitled "RP" (e.g. racial predispositions)

# Table A11 (Equation 1)
cat("\nTable A11:  Sympathy & anti-Black hypotheses (Equation 1, Baseline = Control), Racial Predisposition (RP) Moderators  \n")
mod_stargazer(eq1.policy.rr, eq1.policy.RS,  eq1.policy.whiteid,
          eq1.candidate.rr,eq1.candidate.RS, eq1.candidate.whiteid,
          eq1.taxes.rr, eq1.taxes.RS, eq1.taxes.whiteid,
          eq1.emotions.rr,eq1.emotions.RS, eq1.emotions.whiteid,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA11.html",
          order = c(1, 5, 2, 6, 3,4, 7:35)
)
cat("Saved Table A11 in /output")

# Table A12 (Equation 2)
cat("\nTable A12: Racially selective sympathy hypothesis (Equation 2, Baseline = Sympathetic White), Racial Predisposition (RP) Moderators \n")
mod_stargazer(eq2.policy.rr,eq2.policy.RS,  eq2.policy.whiteid,
          eq2.candidate.rr,eq2.candidate.RS, eq2.candidate.whiteid,
          eq2.taxes.rr, eq2.taxes.RS, eq2.taxes.whiteid,
          eq2.emotions.rr,eq2.emotions.RS, eq2.emotions.whiteid,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Treatment policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA12.html"
)
cat("Saved Table A12 in /output")

## Table A13 (Equation 3)
cat("\nTable A13: Antipathy & pro-White hypothesis (Equation 3, Baseline = Control), Racial Predisposition (RP) Moderators \n")

mod_stargazer(eq3.policy.rr,eq3.policy.RS,  eq3.policy.whiteid,
          eq3.candidate.rr,eq3.candidate.RS, eq3.candidate.whiteid,
          eq3.taxes.rr, eq3.taxes.RS, eq3.taxes.whiteid,
          eq3.emotions.rr,eq3.emotions.RS, eq3.emotions.whiteid,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA13.html",
          order = c(1, 5, 2, 6, 3,4, 7:35))
cat("Saved Table A13 in /output")

# Table A14 (Equation 4)
cat("\n Table A14: Racial antipathy hypothesis (Equation 4, Baseline = Unsympathetic White), Racial Predisposition (RP) Moderators \n")
mod_stargazer(eq4.policy.rr,eq4.policy.RS,  eq4.policy.whiteid,
          eq4.candidate.rr,eq4.candidate.RS, eq4.candidate.whiteid,
          eq4.taxes.rr, eq4.taxes.RS, eq4.taxes.whiteid,
          eq4.emotions.rr,eq4.emotions.RS, eq4.emotions.whiteid,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA14.html"#,
          #order = c(1, 5, 2, 3,4, 6:29)
)
cat("Saved Table A14 in /output")

# Table A15 (Equation 7)
cat("\n Table A15: Racial main effect hypothesis (Equation 7, Baseline = Pooled White), Racial Predisposition (RP) Moderators \n")
mod_stargazer(eq7.policy.rr,eq7.policy.RS,  eq7.policy.whiteid,
          eq7.candidate.rr,eq7.candidate.RS, eq7.candidate.whiteid,
          eq7.taxes.rr, eq7.taxes.RS, eq7.taxes.whiteid,
          eq7.emotions.rr,eq7.emotions.RS, eq7.emotions.whiteid,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA15.html"#,
          #order = c(1, 5, 2, 3,4, 6:29)
)
cat("Saved Table A15 in /output")


##----  Party ID and Ideology Moderators

## Note: Coefficients for PID and Ideology factors are presented on a single line in Tables A16-A20
## entitled "Rep. or Cons." or "Ind. or Mod."

# Table A16 (Equation 1)
cat("\n Table A16: Sympathy & anti-Black hypotheses (Equation 1, Baseline = Control), Political Moderators \n")
mod_stargazer(eq1.policy.partisan,eq1.policy.ideology,
          eq1.candidate.partisan,eq1.candidate.ideology,
          eq1.taxes.partisan, eq1.taxes.ideology,
          eq1.emotions.partisan,eq1.emotions.ideology,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "RR", "whiteid", "racialstereotype"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA16.html",
          order = c(1, 4, 2, 5, 3, 6, 7:33)
)
cat("Saved Table A16 in /output")

# Table A17 (Equation 2)
cat("\n Table A17: Racially selective sympathy hypothesis (Equation 2, Baseline = Sympathetic White), Political Moderators \n")
mod_stargazer(eq2.policy.partisan,eq2.policy.ideology,  
          eq2.candidate.partisan,eq2.candidate.ideology, 
          eq2.taxes.partisan, eq2.taxes.ideology,
          eq2.emotions.partisan,eq2.emotions.ideology,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "RR", "whiteid", "racialstereotype"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA17.html"#
)
cat("Saved Table A17 in /output")

# Table A18 (Equation 3)
cat("\n Table A18: Antipathy & pro-White hypothesis (Equation 3, Baseline = Control), Political Moderators \n")

mod_stargazer(eq3.policy.partisan,eq3.policy.ideology,  
          eq3.candidate.partisan,eq3.candidate.ideology, 
          eq3.taxes.partisan, eq3.taxes.ideology, 
          eq3.emotions.partisan,eq3.emotions.ideology,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "RR", "whiteid", "racialstereotype"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA18.html",
          order = c(1, 4, 2, 5, 3, 6, 7:33)
)
cat("Saved Table A18 in /output")


# Table A19 (Equation 4)
cat("\n Table A19: Racial antipathy hypothesis (Equation 4, Baseline = Unsympathetic White), Political Moderators \n")

mod_stargazer(eq4.policy.partisan,eq4.policy.ideology,
          eq4.candidate.partisan,eq4.candidate.ideology,
          eq4.taxes.partisan, eq4.taxes.ideology, 
          eq4.emotions.partisan,eq4.emotions.ideology,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "RR", "whiteid", "racialstereotype"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA19.html"
)
cat("Saved Table A19 in /output")

# Table A20 (Equation 7)
cat("\n Table Table A20: Racial main effect hypothesis (Equation 7, Baseline = Pooled White), Political Moderators \n")
mod_stargazer(eq7.policy.partisan,eq7.policy.ideology, 
          eq7.candidate.partisan,eq7.candidate.ideology, 
          eq7.taxes.partisan, eq7.taxes.ideology, 
          eq7.emotions.partisan,eq7.emotions.ideology,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "RR", "whiteid", "racialstereotype"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA20.html"
)
cat("Saved Table A20 in /output")

