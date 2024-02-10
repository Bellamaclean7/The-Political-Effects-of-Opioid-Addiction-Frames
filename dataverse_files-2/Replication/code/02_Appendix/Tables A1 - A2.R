####################################################################################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Appendix
## Tables: A1 - A2
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
## manipulation checks
####################################

##########################
## sympathy towards Mike 
##########################

mc1.sympathy <-   lm(sympathymike ~ pooledsymp, data = data[data$pooledsymp == 1| data$pooledunsymp == 1, ])
# summary(mc1.sympathy)

mc1.sympathy.controls <- lm(sympathymike ~ pooledsymp 
                            + age + region + hsgrad + somecollege + collegegrad + postgrad
                            + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, 
                            data = data[data$pooledsymp == 1| data$pooledunsymp == 1, ])
# summary(mc1.sympathy.controls)

## Table A1
cat("\nTable A1: Valence manipulation check\n")
mod_stargazer(mc1.sympathy, mc1.sympathy.controls, type = "html", 
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          omit.table.layout = "n",
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA1.html")
cat("Saved Table A1 in /output")

##########################
## over-report white users 
##########################

mc2.race <- glm(overrepwhiteaddicts ~ pooledwhite + pooledblack, family = "binomial", data = data)

# summary(mc2.race)

mc2.race.controls <- glm(overrepwhiteaddicts ~ pooledwhite + pooledblack 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, family = "binomial",
                         data = data)
# summary(mc2.race.controls)

##########################
## over-report white users 
##########################

mc3.race <- glm(overrepblackaddicts ~ pooledwhite + pooledblack, family = "binomial", data = data)

# summary(mc3.race)

mc3.race.controls <- glm(overrepblackaddicts ~ pooledwhite + pooledblack 
                         + age + region + hsgrad + somecollege + collegegrad + postgrad
                         + female + d25kto34k + d35kto49k + d50kto74k + d75kto99k + d100kto149k + d150kormore + pid7 + ideo, family = "binomial",
                         data = data)
# summary(mc3.race.controls)


## Table A2
cat("\nTable A2: Racial perception manipulation check \n")
mod_stargazer(mc2.race, mc2.race.controls, mc3.race, mc3.race.controls,type = "html",
          omit = c("age", "region", "hsgrad", "somecollege", "collegegrad", "postgrad",
                   "female", "d25kto34k", "d35kto49k", "d50kto74k", "d75kto99k", "d100kto149k",
                   "d150kormore", "pid7", "ideo"),
          omit.table.layout = "n",
          star.cutoffs = NA,
          keep.stat = c("adj.rsq", "n"),
          out = "./output/TableA2.html")
cat("Saved Table A2 in /output")



