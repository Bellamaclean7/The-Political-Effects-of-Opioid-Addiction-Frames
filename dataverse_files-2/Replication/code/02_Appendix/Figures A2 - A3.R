####################################################################################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Appendix
## Figures A2 - A3
## Steps: Calculate mean and se for each emotion; Plot panels for valence and race conditions 
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
data <- read.csv("dynatadata.csv", head = T) ## 336 obs


#######################
## Anger 
########################
## calculate mean and se across conditions 

control <- mean(data$anger[data$treatment == "control"], na.rm = T)


control.sd <- se(data$anger[data$treatment == "control"])

# control.sd 

control.lower <- control - 1.39*control.sd
control.upper <- control + 1.39*control.sd

symp <- mean(data$anger[data$treatment == "noracesymp"|
                          data$treatment == "whitesymp"|
                          data$treatment == "blacksymp"], na.rm = T)


symp.sd <- se(data$anger[data$treatment == "noracesymp"|
                                  data$treatment == "whitesymp"|
                                  data$treatment == "blacksymp"])

# symp.sd 

symp.lower <- symp - 1.39*symp.sd
symp.upper <- symp + 1.39*symp.sd

unsymp <- mean(data$anger[data$treatment == "noraceunsymp"|
                            data$treatment == "whiteunsymp"|
                            data$treatment == "blackunsymp"], na.rm = T)

unsymp.sd <- se(data$anger[data$treatment == "noraceunsymp"|
                                    data$treatment == "whiteunsymp"|
                                    data$treatment == "blackunsymp"])


unsymp.lower <- unsymp - 1.39*unsymp.sd
unsymp.upper <- unsymp + 1.39*unsymp.sd


white <- mean(data$anger[data$treatment == "whitesymp"|
                           data$treatment == "whiteunsymp"])
white.sd <- se(data$anger[data$treatment == "whitesymp"|
                                   data$treatment == "whiteunsymp"])

white.lower <- white - 1.39*white.sd
white.upper <- white + 1.39*white.sd

black <- mean(data$anger[data$treatment == "blacksymp"|
                           data$treatment == "blackunsymp"])
black.sd <- se(data$anger[data$treatment == "blacksymp"|
                                   data$treatment == "blackunsymp"])

black.lower <- black - 1.39*black.sd
black.upper <- black + 1.39*black.sd

norace <- mean(data$anger[data$treatment == "noracesymp"|
                            data$treatment == "noraceunsymp"])
norace.sd <- se(data$anger[data$treatment == "noracesymp"|
                                    data$treatment == "noraceunsymp"])

norace.lower <- norace - 1.39*norace.sd
norace.upper <- norace + 1.39*norace.sd

data.test <- data.frame(Outcome = c("Anger", "Anger",
                                    "Anger", "Anger",
                                    "Anger", "Anger"),
                        treat = c("Control",
                                  "Black",
                                  "No Race",
                                  "White",
                                  "Sympathetic",
                                  "Unsympathetic"),
                        effect = c(control,
                                   black,
                                   norace,
                                   white,
                                   symp,
                                   unsymp),
                        lower = c(control.lower,
                                  black.lower,
                                  norace.lower,
                                  white.lower,
                                  symp.lower,
                                  unsymp.lower),
                        upper = c(control.upper,
                                  black.upper,
                                  norace.upper,
                                  white.upper,
                                  symp.upper,
                                  unsymp.upper)) 

# data.test

data.test$treat <- factor(data.test$treat , levels =c("Control",
                                                      "Black",
                                                      "No Race",
                                                      "White",
                                                      "Sympathetic",
                                                      "Unsympathetic"))


anger.emotions <- ggplot(data.test[c(1, 5,6), ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# anger.emotions

anger.race <- ggplot(data.test[1:4, ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# anger.race

#######################
## sympathy 
########################
## calculate mean and se across conditions 


control <- mean(data$sympathy[data$treatment == "control"], na.rm = T)


control.sd <- se(data$sympathy[data$treatment == "control"])

# control.sd 

control.lower <- control - 1.39*control.sd
control.upper <- control + 1.39*control.sd

symp <- mean(data$sympathy[data$treatment == "noracesymp"|
                             data$treatment == "whitesymp"|
                             data$treatment == "blacksymp"], na.rm = T)


symp.sd <- se(data$sympathy[data$treatment == "noracesymp"|
                                     data$treatment == "whitesymp"|
                                     data$treatment == "blacksymp"])

# symp.sd 

symp.lower <- symp - 1.39*symp.sd
symp.upper <- symp + 1.39*symp.sd

unsymp <- mean(data$sympathy[data$treatment == "noraceunsymp"|
                               data$treatment == "whiteunsymp"|
                               data$treatment == "blackunsymp"], na.rm = T)

unsymp.sd <- se(data$sympathy[data$treatment == "noraceunsymp"|
                                       data$treatment == "whiteunsymp"|
                                       data$treatment == "blackunsymp"])


unsymp.lower <- unsymp - 1.39*unsymp.sd
unsymp.upper <- unsymp + 1.39*unsymp.sd


white <- mean(data$sympathy[data$treatment == "whitesymp"|
                              data$treatment == "whiteunsymp"])
white.sd <- se(data$sympathy[data$treatment == "whitesymp"|
                                      data$treatment == "whiteunsymp"])

white.lower <- white - 1.39*white.sd
white.upper <- white + 1.39*white.sd

black <- mean(data$sympathy[data$treatment == "blacksymp"|
                              data$treatment == "blackunsymp"])
black.sd <- se(data$sympathy[data$treatment == "blacksymp"|
                                      data$treatment == "blackunsymp"])

black.lower <- black - 1.39*black.sd
black.upper <- black + 1.39*black.sd

norace <- mean(data$sympathy[data$treatment == "noracesymp"|
                               data$treatment == "noraceunsymp"])
norace.sd <- se(data$sympathy[data$treatment == "noracesymp"|
                                       data$treatment == "noraceunsymp"])

norace.lower <- norace - 1.39*norace.sd
norace.upper <- norace + 1.39*norace.sd

data.test <- data.frame(Outcome = c("Sympathy", "Sympathy",
                                    "Sympathy", "Sympathy",
                                    "Sympathy", "Sympathy"),
                        treat = c("Control",
                                  "Black",
                                  "No Race",
                                  "White",
                                  "Sympathetic",
                                  "Unsympathetic"),
                        effect = c(control,
                                   black,
                                   norace,
                                   white,
                                   symp,
                                   unsymp),
                        lower = c(control.lower,
                                  black.lower,
                                  norace.lower,
                                  white.lower,
                                  symp.lower,
                                  unsymp.lower),
                        upper = c(control.upper,
                                  black.upper,
                                  norace.upper,
                                  white.upper,
                                  symp.upper,
                                  unsymp.upper)) 

# data.test

data.test$treat <- factor(data.test$treat , levels =c("Control",
                                                      "Black",
                                                      "No Race",
                                                      "White",
                                                      "Sympathetic",
                                                      "Unsympathetic"))


sympathy.emotions <- ggplot(data.test[c(1, 5,6), ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# sympathy.emotions


sympathy.race <- ggplot(data.test[1:4, ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# sympathy.race

#######################
## fear 
########################
## calculate mean and se across conditions 

control <- mean(data$fear[data$treatment == "control"], na.rm = T)


control.sd <- se(data$fear[data$treatment == "control"])

# control.sd 

control.lower <- control - 1.39*control.sd
control.upper <- control + 1.39*control.sd

symp <- mean(data$fear[data$treatment == "noracesymp"|
                         data$treatment == "whitesymp"|
                         data$treatment == "blacksymp"], na.rm = T)


symp.sd <- se(data$fear[data$treatment == "noracesymp"|
                                 data$treatment == "whitesymp"|
                                 data$treatment == "blacksymp"])

# symp.sd 

symp.lower <- symp - 1.39*symp.sd
symp.upper <- symp + 1.39*symp.sd

unsymp <- mean(data$fear[data$treatment == "noraceunsymp"|
                           data$treatment == "whiteunsymp"|
                           data$treatment == "blackunsymp"], na.rm = T)

unsymp.sd <- se(data$fear[data$treatment == "noraceunsymp"|
                                   data$treatment == "whiteunsymp"|
                                   data$treatment == "blackunsymp"])


unsymp.lower <- unsymp - 1.39*unsymp.sd
unsymp.upper <- unsymp + 1.39*unsymp.sd


white <- mean(data$fear[data$treatment == "whitesymp"|
                          data$treatment == "whiteunsymp"])
white.sd <- se(data$fear[data$treatment == "whitesymp"|
                                  data$treatment == "whiteunsymp"])

white.lower <- white - 1.39*white.sd
white.upper <- white + 1.39*white.sd

black <- mean(data$fear[data$treatment == "blacksymp"|
                          data$treatment == "blackunsymp"])
black.sd <- se(data$fear[data$treatment == "blacksymp"|
                                  data$treatment == "blackunsymp"])

black.lower <- black - 1.39*black.sd
black.upper <- black + 1.39*black.sd

norace <- mean(data$fear[data$treatment == "noracesymp"|
                           data$treatment == "noraceunsymp"])
norace.sd <- se(data$fear[data$treatment == "noracesymp"|
                                   data$treatment == "noraceunsymp"])

norace.lower <- norace - 1.39*norace.sd
norace.upper <- norace + 1.39*norace.sd

data.test <- data.frame(Outcome = c("Fear", "Fear",
                                    "Fear", "Fear",
                                    "Fear", "Fear"),
                        treat = c("Control",
                                  "Black",
                                  "No Race",
                                  "White",
                                  "Sympathetic",
                                  "Unsympathetic"),
                        effect = c(control,
                                   black,
                                   norace,
                                   white,
                                   symp,
                                   unsymp),
                        lower = c(control.lower,
                                  black.lower,
                                  norace.lower,
                                  white.lower,
                                  symp.lower,
                                  unsymp.lower),
                        upper = c(control.upper,
                                  black.upper,
                                  norace.upper,
                                  white.upper,
                                  symp.upper,
                                  unsymp.upper)) 

# data.test

data.test$treat <- factor(data.test$treat , levels =c("Control",
                                                      "Black",
                                                      "No Race",
                                                      "White",
                                                      "Sympathetic",
                                                      "Unsympathetic"))


fear.emotions <- ggplot(data.test[c(1, 5,6), ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# fear.emotions

fear.race <- ggplot(data.test[1:4, ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# fear.race

#######################
## compassion
########################
## calculate mean and se across conditions 

control <- mean(data$compassion[data$treatment == "control"], na.rm = T)


control.sd <- se(data$compassion[data$treatment == "control"])

# control.sd 

control.lower <- control - 1.39*control.sd
control.upper <- control + 1.39*control.sd

symp <- mean(data$compassion[data$treatment == "noracesymp"|
                               data$treatment == "whitesymp"|
                               data$treatment == "blacksymp"], na.rm = T)


symp.sd <- se(data$compassion[data$treatment == "noracesymp"|
                                       data$treatment == "whitesymp"|
                                       data$treatment == "blacksymp"])

# symp.sd 

symp.lower <- symp - 1.39*symp.sd
symp.upper <- symp + 1.39*symp.sd

unsymp <- mean(data$compassion[data$treatment == "noraceunsymp"|
                                 data$treatment == "whiteunsymp"|
                                 data$treatment == "blackunsymp"], na.rm = T)

unsymp.sd <- se(data$compassion[data$treatment == "noraceunsymp"|
                                         data$treatment == "whiteunsymp"|
                                         data$treatment == "blackunsymp"])


unsymp.lower <- unsymp - 1.39*unsymp.sd
unsymp.upper <- unsymp + 1.39*unsymp.sd


white <- mean(data$compassion[data$treatment == "whitesymp"|
                                data$treatment == "whiteunsymp"])
white.sd <- se(data$compassion[data$treatment == "whitesymp"|
                                        data$treatment == "whiteunsymp"])

white.lower <- white - 1.39*white.sd
white.upper <- white + 1.39*white.sd

black <- mean(data$compassion[data$treatment == "blacksymp"|
                                data$treatment == "blackunsymp"])
black.sd <- se(data$compassion[data$treatment == "blacksymp"|
                                        data$treatment == "blackunsymp"])

black.lower <- black - 1.39*black.sd
black.upper <- black + 1.39*black.sd

norace <- mean(data$compassion[data$treatment == "noracesymp"|
                                 data$treatment == "noraceunsymp"])
norace.sd <- se(data$compassion[data$treatment == "noracesymp"|
                                         data$treatment == "noraceunsymp"])

norace.lower <- norace - 1.39*norace.sd
norace.upper <- norace + 1.39*norace.sd

data.test <- data.frame(Outcome = c("Compassion", "Compassion",
                                    "Compassion", "Compassion",
                                    "Compassion", "Compassion"),
                        treat = c("Control",
                                  "Black",
                                  "No Race",
                                  "White",
                                  "Sympathetic",
                                  "Unsympathetic"),
                        effect = c(control,
                                   black,
                                   norace,
                                   white,
                                   symp,
                                   unsymp),
                        lower = c(control.lower,
                                  black.lower,
                                  norace.lower,
                                  white.lower,
                                  symp.lower,
                                  unsymp.lower),
                        upper = c(control.upper,
                                  black.upper,
                                  norace.upper,
                                  white.upper,
                                  symp.upper,
                                  unsymp.upper)) 

# data.test

data.test$treat <- factor(data.test$treat , levels =c("Control",
                                                      "Black",
                                                      "No Race",
                                                      "White",
                                                      "Sympathetic",
                                                      "Unsympathetic"))


compassion.emotions <- ggplot(data.test[c(1, 5,6), ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# compassion.emotions

compassion.race <- ggplot(data.test[1:4, ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# compassion.race
#######################
## disgust 
########################
## calculate mean and se across conditions 


control <- mean(data$disgust[data$treatment == "control"], na.rm = T)


control.sd <- se(data$disgust[data$treatment == "control"])

# control.sd 

control.lower <- control - 1.39*control.sd
control.upper <- control + 1.39*control.sd

symp <- mean(data$disgust[data$treatment == "noracesymp"|
                            data$treatment == "whitesymp"|
                            data$treatment == "blacksymp"], na.rm = T)


symp.sd <- se(data$disgust[data$treatment == "noracesymp"|
                                    data$treatment == "whitesymp"|
                                    data$treatment == "blacksymp"])

# symp.sd 

symp.lower <- symp - 1.39*symp.sd
symp.upper <- symp + 1.39*symp.sd

unsymp <- mean(data$disgust[data$treatment == "noraceunsymp"|
                              data$treatment == "whiteunsymp"|
                              data$treatment == "blackunsymp"], na.rm = T)

unsymp.sd <- se(data$disgust[data$treatment == "noraceunsymp"|
                                      data$treatment == "whiteunsymp"|
                                      data$treatment == "blackunsymp"])


unsymp.lower <- unsymp - 1.39*unsymp.sd
unsymp.upper <- unsymp + 1.39*unsymp.sd


white <- mean(data$disgust[data$treatment == "whitesymp"|
                             data$treatment == "whiteunsymp"])
white.sd <- se(data$disgust[data$treatment == "whitesymp"|
                                     data$treatment == "whiteunsymp"])

white.lower <- white - 1.39*white.sd
white.upper <- white + 1.39*white.sd

black <- mean(data$disgust[data$treatment == "blacksymp"|
                             data$treatment == "blackunsymp"])
black.sd <- se(data$disgust[data$treatment == "blacksymp"|
                                     data$treatment == "blackunsymp"])

black.lower <- black - 1.39*black.sd
black.upper <- black + 1.39*black.sd

norace <- mean(data$disgust[data$treatment == "noracesymp"|
                              data$treatment == "noraceunsymp"])
norace.sd <- se(data$disgust[data$treatment == "noracesymp"|
                                      data$treatment == "noraceunsymp"])

norace.lower <- norace - 1.39*norace.sd
norace.upper <- norace + 1.39*norace.sd

data.test <- data.frame(Outcome = c("Disgust", "Disgust",
                                    "Disgust", "Disgust",
                                    "Disgust", "Disgust"),
                        treat = c("Control",
                                  "Black",
                                  "No Race",
                                  "White",
                                  "Sympathetic",
                                  "Unsympathetic"),
                        effect = c(control,
                                   black,
                                   norace,
                                   white,
                                   symp,
                                   unsymp),
                        lower = c(control.lower,
                                  black.lower,
                                  norace.lower,
                                  white.lower,
                                  symp.lower,
                                  unsymp.lower),
                        upper = c(control.upper,
                                  black.upper,
                                  norace.upper,
                                  white.upper,
                                  symp.upper,
                                  unsymp.upper)) 

# data.test

data.test$treat <- factor(data.test$treat , levels =c("Control",
                                                      "Black",
                                                      "No Race",
                                                      "White",
                                                      "Sympathetic",
                                                      "Unsympathetic"))

disgust.emotions <- ggplot(data.test[c(1, 5,6), ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# disgust.emotions


disgust.race <- ggplot(data.test[1:4, ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# disgust.race

#######################
## pity 
########################
## calculate mean and se across conditions 

control <- mean(data$pity[data$treatment == "control"], na.rm = T)


control.sd <- se(data$pity[data$treatment == "control"])

# control.sd 

control.lower <- control - 1.39*control.sd
control.upper <- control + 1.39*control.sd

symp <- mean(data$pity[data$treatment == "noracesymp"|
                         data$treatment == "whitesymp"|
                         data$treatment == "blacksymp"], na.rm = T)


symp.sd <- se(data$pity[data$treatment == "noracesymp"|
                                 data$treatment == "whitesymp"|
                                 data$treatment == "blacksymp"])

# symp.sd 

symp.lower <- symp - 1.39*symp.sd
symp.upper <- symp + 1.39*symp.sd

unsymp <- mean(data$pity[data$treatment == "noraceunsymp"|
                           data$treatment == "whiteunsymp"|
                           data$treatment == "blackunsymp"], na.rm = T)

unsymp.sd <- se(data$pity[data$treatment == "noraceunsymp"|
                                   data$treatment == "whiteunsymp"|
                                   data$treatment == "blackunsymp"])


unsymp.lower <- unsymp - 1.39*unsymp.sd
unsymp.upper <- unsymp + 1.39*unsymp.sd


white <- mean(data$pity[data$treatment == "whitesymp"|
                          data$treatment == "whiteunsymp"])
white.sd <- se(data$pity[data$treatment == "whitesymp"|
                                  data$treatment == "whiteunsymp"])

white.lower <- white - 1.39*white.sd
white.upper <- white + 1.39*white.sd

black <- mean(data$pity[data$treatment == "blacksymp"|
                          data$treatment == "blackunsymp"])
black.sd <- se(data$pity[data$treatment == "blacksymp"|
                                  data$treatment == "blackunsymp"])

black.lower <- black - 1.39*black.sd
black.upper <- black + 1.39*black.sd

norace <- mean(data$pity[data$treatment == "noracesymp"|
                           data$treatment == "noraceunsymp"])
norace.sd <- se(data$pity[data$treatment == "noracesymp"|
                                   data$treatment == "noraceunsymp"])

norace.lower <- norace - 1.39*norace.sd
norace.upper <- norace + 1.39*norace.sd

data.test <- data.frame(Outcome = c("Pity", "Pity",
                                    "Pity", "Pity",
                                    "Pity", "Pity"),
                        treat = c("Control",
                                  "Black",
                                  "No Race",
                                  "White",
                                  "Sympathetic",
                                  "Unsympathetic"),
                        effect = c(control,
                                   black,
                                   norace,
                                   white,
                                   symp,
                                   unsymp),
                        lower = c(control.lower,
                                  black.lower,
                                  norace.lower,
                                  white.lower,
                                  symp.lower,
                                  unsymp.lower),
                        upper = c(control.upper,
                                  black.upper,
                                  norace.upper,
                                  white.upper,
                                  symp.upper,
                                  unsymp.upper)) 

# data.test

data.test$treat <- factor(data.test$treat , levels =c("Control",
                                                      "Black",
                                                      "No Race",
                                                      "White",
                                                      "Sympathetic",
                                                      "Unsympathetic"))


pity.emotions <- ggplot(data.test[c(1, 5,6), ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# pity.emotions


pity.race <- ggplot(data.test[1:4, ], aes(y=treat, x=effect, fill=treat)) +
  facet_grid(.~Outcome) +
  geom_point(stat="identity") +
  xlab("Average treatment effect") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank()) +
  scale_x_continuous(limits=c(0.3,0.7)) +
  geom_errorbarh(aes(xmin = lower, xmax= upper, height=.3)) +
  theme(axis.title.x = element_blank())

# pity.race

#######################
## Print figures
########################

#######################
## Figure A2
########################

## View in R
# grid.arrange(anger.emotions,disgust.emotions,fear.emotions, 
#              pity.emotions, compassion.emotions,sympathy.emotions, 
#              nrow= 2)  

## Print figure 
cat("\nFigure A2: Mean emotional responses to ‘drug addicts’, by valence condition (raw values, 83% CIs)\n")
pdf("./output/FigureA2.pdf", width = 12, height= 7) 
par(mfrow=c(3,2), mar= c(3,3,2,1), mgp = c(2,0.5, 0))

grid.arrange(anger.emotions,disgust.emotions,fear.emotions, 
             pity.emotions, compassion.emotions,sympathy.emotions, 
             nrow= 2)  

dev.off()
cat("Saved Figure A2 in /output")

#######################
## Figure A3
########################

# ## View in R
# grid.arrange(anger.race, disgust.race,fear.race, 
#              pity.race,compassion.race,   sympathy.race, nrow = 2) 
# 


## Print figure 
cat("\nFigure A3: Mean emotional responses to ‘drug addicts’, by racial condition (raw values, 83% CIs)\n")
pdf("./output/FigureA3.pdf", width = 12, height= 7) 
par(mfrow=c(3,2), mar= c(3,3,2,1), mgp = c(2,0.5, 0))
grid.arrange(anger.race, disgust.race,fear.race, 
             pity.race,compassion.race,   sympathy.race, nrow = 2) 

dev.off()
cat("Saved Figure A3 in /output")
