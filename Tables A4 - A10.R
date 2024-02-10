####################################################################################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Appendix
## Tables: A4 - A10
## Steps: regression models, print tables
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
## Regression results
####################################
## recode region factor levels so that South = baseline
data$region <- factor(data$region, levels = c("South", "Northeast", "Midwest", "West"))
summary(data$region)

##########################
## Equation 1
##########################

################
## policy
################

eq1.policy <- glm(policy ~ sw + sb, family = "binomial", data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.policy)

eq1.policy.controls <- glm(policy ~ sw + sb 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.policy.controls)


################
## taxes
################

eq1.taxes <- lm(taxesfortreatment ~ sw + sb, 
                data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.taxes)

eq1.taxes.controls <- lm(taxesfortreatment ~ sw + sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.taxes.controls)

################
## candidate
################

eq1.candidate <- lm(candidatesupport ~ sw + sb, 
                    data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.candidate)

eq1.candidate.controls <- lm(candidatesupport ~ sw + sb 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                             data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.candidate.controls)

################
## Feelings
################

eq1.emotions <- lm(emotionscale ~ sw + sb, 
                   data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.emotions)

eq1.emotions.controls <- lm(emotionscale ~ sw + sb 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data[data$ns == 1| data$sw == 1| data$sb == 1, ])
# summary(eq1.emotions.controls)

################
## Table A4 (Equation 1)
################
cat("\nTable A4: Sympathy & anti-Black hypotheses (Equation 1, Baseline = Control) \n")
mod_stargazer(eq1.policy, eq1.policy.controls, eq1.candidate, eq1.candidate.controls, 
          eq1.taxes, eq1.taxes.controls, eq1.emotions, eq1.emotions.controls,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA4.html")
cat("Saved Table A4 in /output")

##########################
## Equation 2
##########################


################
## policy
################

eq2.policy <- glm(policy ~ sb, family = "binomial", data = data[data$sw == 1| data$sb == 1, ])
# summary(eq2.policy)

eq2.policy.controls <- glm(policy ~ sb 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$sw == 1| data$sb == 1, ])
# summary(eq2.policy.controls)


################
## taxes
################

eq2.taxes <- lm(taxesfortreatment ~ sb, 
                data = data[data$sw == 1| data$sb == 1, ])
# summary(eq2.taxes)

eq2.taxes.controls <- lm(taxesfortreatment ~ sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$sw == 1| data$sb == 1, ])
# summary(eq2.taxes.controls)

################
## candidate
################

eq2.candidate <- lm(candidatesupport ~ sb,
                    data = data[data$sw == 1| data$sb == 1, ])
# summary(eq2.candidate)

eq2.candidate.controls <- lm(candidatesupport ~ sb  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data[data$sw == 1| data$sb == 1, ])
# summary(eq2.candidate.controls)

################
## Feelings
################

eq2.emotions <- lm(emotionscale ~ sb, 
                   data = data[data$sw == 1| data$sb == 1, ])
# summary(eq2.emotions)

eq2.emotions.controls <- lm(emotionscale ~ sb 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data[ data$sw == 1| data$sb == 1, ])
# summary(eq2.emotions.controls)

################
## Table A5 (Equation 2)
################
cat("\nTable A5: Racially selective sympathy hypothesis (Equation 2, Baseline = Sympathetic White) \n")
mod_stargazer(eq2.policy, eq2.policy.controls, eq2.candidate, eq2.candidate.controls, 
          eq2.taxes, eq2.taxes.controls, eq2.emotions, eq2.emotions.controls,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA5.html")

cat("Saved Table A5 in /output")

##########################
## Equation 3
##########################

################
## policy
################

eq3.policy <- glm(policy ~ uw + ub, family = "binomial", data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.policy)

eq3.policy.controls <- glm(policy ~ uw + ub 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo , 
                           family = "binomial", data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.policy.controls)

################
## taxes
################

eq3.taxes <- lm(taxesfortreatment ~ uw + ub, 
                data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.taxes)

eq3.taxes.controls <- lm(taxesfortreatment ~ uw + ub 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                         data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.taxes.controls)

################
## candidate
################

eq3.candidate <- lm(candidatesupport ~ uw + ub, 
                    data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.candidate)

eq3.candidate.controls <- lm(candidatesupport ~ uw + ub 
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.candidate.controls)

################
## feelings
################

eq3.emotions <- lm(emotionscale ~ uw + ub, 
                   data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.emotions)

eq3.emotions.controls <- lm(emotionscale ~ uw + ub 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                            data = data[data$ns == 1| data$uw == 1| data$ub == 1, ])
# summary(eq3.emotions.controls)


################
## Table A6 (Equation 3)
################
cat("\nTable A6:Antipathy & pro-White hypothesis (Equation 3, Baseline = Control)\n")
mod_stargazer(eq3.policy, eq3.policy.controls, eq3.candidate, eq3.candidate.controls, 
          eq3.taxes, eq3.taxes.controls, eq3.emotions, eq3.emotions.controls,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA6.html")
cat("Saved Table A6 in /output")


##########################
## Equation 4
##########################

################
## policy
################

eq4.policy <- glm(policy ~ ub, family = "binomial", data = data[data$uw == 1| data$ub == 1, ])
# summary(eq4.policy)

eq4.policy.controls <- glm(policy ~ ub 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$uw == 1| data$ub == 1, ])
# summary(eq4.policy.controls)

################
## taxes
################

eq4.taxes <- lm(taxesfortreatment ~ ub, 
                data = data[data$uw == 1| data$ub == 1, ])
# summary(eq4.taxes)

eq4.taxes.controls <- lm(taxesfortreatment ~ ub 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$uw == 1| data$ub == 1, ])
# summary(eq4.taxes.controls)

################
## candidate
################

eq4.candidate <- lm(candidatesupport ~ ub,
                    data = data[data$uw == 1| data$ub == 1, ])
# summary(eq4.candidate)

eq4.candidate.controls <- lm(candidatesupport ~ ub  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data[data$uw == 1| data$ub == 1, ])
# summary(eq4.candidate.controls)

################
## feelings
################

eq4.emotions <- lm(emotionscale ~ ub,
                   data = data[data$uw == 1| data$ub == 1, ])
# summary(eq4.emotions)

eq4.emotions.controls <- lm(emotionscale ~ ub  
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                            data = data[data$uw == 1| data$ub == 1, ])
# summary(eq4.emotions.controls)


################
## Table A7 (Equation 4)
################
cat("\nTable A7: Racial antipathy hypothesis (Equation 4, Baseline = Unsympathetic White) \n")
mod_stargazer(eq4.policy, eq4.policy.controls, eq4.candidate, eq4.candidate.controls, 
          eq4.taxes, eq4.taxes.controls, eq4.emotions, eq4.emotions.controls,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA7.html")
cat("Saved Table A7 in /output")

##########################
## Equation 5
##########################

################
## policy
################

eq5.policy <- glm(policy ~ sw, family = "binomial", data = data[data$uw == 1| data$sw == 1, ])
# summary(eq5.policy)

eq5.policy.controls <- glm(policy ~ sw 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$uw == 1| data$sw == 1, ])
# summary(eq5.policy.controls)

################
## taxes
################

eq5.taxes <- lm(taxesfortreatment ~ sw, 
                data = data[data$uw == 1| data$sw == 1, ])
# summary(eq5.taxes)

eq5.taxes.controls <- lm(taxesfortreatment ~ sw 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$uw == 1| data$sw == 1, ])
# summary(eq5.taxes.controls)

################
## candidate
################

eq5.candidate <- lm(candidatesupport ~ sw,
                    data = data[data$uw == 1| data$sw == 1, ])
# summary(eq5.candidate)

eq5.candidate.controls <- lm(candidatesupport ~ sw  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data[data$uw == 1| data$sw == 1, ])
# summary(eq5.candidate.controls)

################
## feelings
################

eq5.emotions <- lm(emotionscale ~ sw,
                   data = data[data$uw == 1| data$sw == 1, ])
# summary(eq5.emotions)

eq5.emotions.controls <- lm(emotionscale ~ sw  
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                            data = data[data$uw == 1| data$sw == 1, ])
# summary(eq5.emotions.controls)

################
## Table A8 (Equation 5)
################
cat("\nTable A8: Full valence hypothesis (Equation 5, Baseline = Unsympathetic White) \n")
mod_stargazer(eq5.policy, eq5.policy.controls, eq5.candidate, eq5.candidate.controls, 
          eq5.taxes, eq5.taxes.controls, eq5.emotions, eq5.emotions.controls,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA8.html")
cat("Saved Table A8 in /output")

##########################
## Equation 6
##########################

################
## policy
################

eq6.policy <- glm(policy ~ sb, family = "binomial", data = data[data$ub == 1| data$sb == 1, ])
# summary(eq6.policy)

eq6.policy.controls <- glm(policy ~ sb 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$ub == 1| data$sb == 1, ])
 summary(eq6.policy.controls)

################
## taxes
################

eq6.taxes <- lm(taxesfortreatment ~ sb, 
                data = data[data$ub == 1| data$sb == 1, ])
# summary(eq6.taxes)

eq6.taxes.controls <- lm(taxesfortreatment ~ sb 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$ub == 1| data$sb == 1, ])
# summary(eq6.taxes.controls)

################
## candidate
################

eq6.candidate <- lm(candidatesupport ~ sb,
                    data = data[data$ub == 1| data$sb == 1, ])
# summary(eq6.candidate)

eq6.candidate.controls <- lm(candidatesupport ~ sb  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data[data$ub == 1| data$sb == 1, ])
# summary(eq6.candidate.controls)

################
## feelings
################

eq6.emotions <- lm(emotionscale ~ sb,
                   data = data[data$ub == 1| data$sb == 1, ])
# summary(eq6.emotions)

eq6.emotions.controls <- lm(emotionscale ~ sb  
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                            data = data[data$ub == 1| data$sb == 1, ])
# summary(eq6.emotions.controls)

################
## Table A9 (Equation 6)
################
cat("\nTable A9: Full valence hypothesis (Equation 6, Baseline = Unsympathetic Black)\n")
mod_stargazer(eq6.policy, eq6.policy.controls, eq6.candidate, eq6.candidate.controls, 
          eq6.taxes, eq6.taxes.controls, eq6.emotions, eq6.emotions.controls,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA9.html")
cat("Saved Table A9 in /output")

##########################
## Equation 7
##########################

################
## policy
################

eq7.policy <- glm(policy ~ pooledblack, family = "binomial", data = data[data$pooledwhite == 1| data$pooledblack == 1, ])
# summary(eq7.policy)

eq7.policy.controls <- glm(policy ~ pooledblack 
                           + age + region + hsgrad + somecollege + collegegrad + postgrad
                           + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                           family = "binomial", data = data[data$pooledwhite == 1| data$pooledblack == 1, ])
# summary(eq7.policy.controls)

################
## taxes
################

eq7.taxes <- lm(taxesfortreatment ~ pooledblack, 
                data = data[data$pooledwhite == 1| data$pooledblack == 1, ])
# summary(eq7.taxes)

eq7.taxes.controls <- lm(taxesfortreatment ~ pooledblack 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                         data = data[data$pooledwhite == 1| data$pooledblack == 1, ])
# summary(eq7.taxes.controls)

################
## candidate
################

eq7.candidate <- lm(candidatesupport ~ pooledblack,
                    data = data[data$pooledwhite == 1| data$pooledblack == 1, ])
# summary(eq7.candidate)

eq7.candidate.controls <- lm(candidatesupport ~ pooledblack  
                             + age + region + hsgrad + somecollege + collegegrad + postgrad
                             + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                             data = data[data$pooledwhite == 1| data$pooledblack == 1, ])
# summary(eq7.candidate.controls)

################
## feelings
################

eq7.emotions <- lm(emotionscale ~ pooledblack,
                   data = data[data$pooledwhite == 1| data$pooledblack == 1, ])
# summary(eq7.emotions)

eq7.emotions.controls <- lm(emotionscale ~ pooledblack  
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore+ pid7 + ideo, 
                            data = data[data$pooledwhite == 1| data$pooledblack == 1, ])
# summary(eq7.emotions.controls)

################
## Table A10 (Equation 7)
################
cat("\nTable A10: Racial main effect hypothesis (Equation 7, Baseline = Pooled White) \n")
mod_stargazer(eq7.policy, eq7.policy.controls, eq7.candidate, eq7.candidate.controls, 
          eq7.taxes, eq7.taxes.controls, eq7.emotions, eq7.emotions.controls,
          type = "html", omit.table.layout = "n",
          dep.var.labels=c('Policy','Candidate', "Taxes", "Emotions"),
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA10.html")
cat("Saved Table A10 in /output")
