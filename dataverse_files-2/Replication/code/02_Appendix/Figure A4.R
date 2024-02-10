####################################################################################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Appendix
## Figures A4
## Steps: Create over-reporting measures, calculate mean and se for each outcome; Plot panels race conditions 
## Create figure
####################################################################################
rm(list=ls()) ## clear list space

#####################
### Load packages #####
#####################

library(ggplot2) ## used to create figure
library(gridExtra) ## used to print figure

#####################
### Function #####
#####################
se <- function(x) sqrt(var(x, na.rm = T)/length(x)) ## standard error


#####################
### Load Data ########
#####################
## read in data
data <- read.csv("./dynatadata.csv", head = T) ## 336 obs

##########################
## create binary measures of whether respondent
## over-reported white and black opioid users, relative to adult population prop
##########################

## percent white opioid
data$morewhiteopioid <- 0
data$morewhiteopioid[data$perwhiteopioid >= 0.64] <- 1

## percent black opioid
data$moreblackopioid <- 0
data$moreblackopioid[data$perblackopioid >= 0.12] <- 1


##########################
## Over-report white opioid users
##########################
## calculate mean and se across race conditions 

control <- mean(data$morewhiteopioid[data$treatment == "control"], na.rm = T)
# control

control.sd <- se(data$morewhiteopioid[data$treatment == "control"])
# control.sd

control.lower <- control - 1.39*control.sd
control.upper <- control + 1.39*control.sd


white <- mean(data$morewhiteopioid[data$treatment == "whitesymp"|
                                     data$treatment == "whiteunsymp"])
# white
white.sd <- se(data$morewhiteopioid[data$treatment == "whitesymp"|
                                             data$treatment == "whiteunsymp"])

white.lower <- white - 1.39*white.sd
white.upper <- white + 1.39*white.sd


black <- mean(data$morewhiteopioid[data$treatment == "blacksymp"|
                                     data$treatment == "blackunsymp"])
black.sd <- se(data$morewhiteopioid[data$treatment == "blacksymp"|
                                             data$treatment == "blackunsymp"])

black.lower <- black - 1.39*black.sd
black.upper <- black + 1.39*black.sd


norace <- mean(data$morewhiteopioid[data$treatment == "noracesymp"|
                                      data$treatment == "noraceunsymp"])
norace.sd <- se(data$morewhiteopioid[data$treatment == "noracesymp"|
                                              data$treatment == "noraceunsymp"])

norace.lower <- norace - 1.39*norace.sd
norace.upper <- norace + 1.39*norace.sd

## values
data.test <- data.frame(Outcome = c("Over-report White 'opioid addicts'", "Over-report White 'opioid addicts'", 
                                    "Over-report White 'opioid addicts'", "Over-report White 'opioid addicts'"),
                        
                        treat = c("Control",
                                  "Black",
                                  "No Race",
                                  "White"),
                        effect = c(control,
                                   black,
                                   norace,
                                   white),
                        lower = c(control.lower,
                                  black.lower,
                                  norace.lower,
                                  white.lower),
                        upper = c(control.upper,
                                  black.upper,
                                  norace.upper,
                                  white.upper)) 

# data.test

data.test$treat <- factor(data.test$treat , levels =c("Control",
                                                      "Black",
                                                      "No Race",
                                                      "White"))

p1 <- ggplot(data.test, aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  #xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=16, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0,1)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

p1 + theme(axis.title.x = element_blank())


##########################
## Over-report black opioid users
##########################
## calculate mean and se across race conditions 

control <- mean(data$moreblackopioid[data$treatment == "control"], na.rm = T)
# control

control.sd <- se(data$moreblackopioid[data$treatment == "control"])
# control.sd

control.lower <- control - 1.39*control.sd
control.upper <- control + 1.39*control.sd

white <- mean(data$moreblackopioid[data$treatment == "whitesymp"|
                                     data$treatment == "whiteunsymp"])
white.sd <- se(data$moreblackopioid[data$treatment == "whitesymp"|
                                             data$treatment == "whiteunsymp"])

white.lower <- white - 1.39*white.sd
white.upper <- white + 1.39*white.sd

black <- mean(data$moreblackopioid[data$treatment == "blacksymp"|
                                     data$treatment == "blackunsymp"])
black.sd <- se(data$moreblackopioid[data$treatment == "blacksymp"|
                                             data$treatment == "blackunsymp"])

black.lower <- black - 1.39*black.sd
black.upper <- black + 1.39*black.sd

norace <- mean(data$moreblackopioid[data$treatment == "noracesymp"|
                                      data$treatment == "noraceunsymp"])
norace.sd <- se(data$moreblackopioid[data$treatment == "noracesymp"|
                                              data$treatment == "noraceunsymp"])

norace.lower <- norace - 1.39*norace.sd
norace.upper <- norace + 1.39*norace.sd

## values
data.test <- data.frame(Outcome = c("Over-report Black 'opioid addicts'", "Over-report Black 'opioid addicts'", 
                                    "Over-report Black 'opioid addicts'", "Over-report Black 'opioid addicts'"),
                        treat = c("Control",
                                  "Black",
                                  "No Race",
                                  "White"),
                        effect = c(control,
                                   black,
                                   norace,
                                   white),
                        lower = c(control.lower,
                                  black.lower,
                                  norace.lower,
                                  white.lower),
                        upper = c(control.upper,
                                  black.upper,
                                  norace.upper,
                                  white.upper)) 

# data.test

data.test$treat <- factor(data.test$treat , levels =c("Control",
                                                      "Black",
                                                      "No Race",
                                                      "White"))

p2 <- ggplot(data.test, aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  #xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=18, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0,1)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# p2 

#######################
## Print figures
########################

#######################
## Figure A2
########################

## View in R
# grid.arrange(p1, p2, nrow = 1)


## Print figure 
cat("\nFigure A4: Prop. over-reporting White and Black ‘opioid addicts,’ by racial condition (raw values, 83% CIs) \n")
pdf("./output/FigureA4.pdf", width = 11, height= 5) 
par(mfrow=c(3,2), mar= c(3,3,2,1), mgp = c(2,0.5, 0))
grid.arrange(p1, p2, nrow = 1)
dev.off()
cat("Saved Figure A4 in /output")

