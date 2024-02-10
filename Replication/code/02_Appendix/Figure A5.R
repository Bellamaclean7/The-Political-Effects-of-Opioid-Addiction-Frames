####################################################################################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Appendix
## Figure A5
## Steps: calculate raw means and ses by experimental condition, plot, print figure
####################################################################################
rm(list=ls()) ## clear list space

#####################
### Packages ########
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
data <- read.csv("tessdata.csv", head = T) ## 1517 obs


#####################################################
## calculate means and ses
#####################################################

## DV 1: Policy
mean.1.policy <- mean(data$policy[data$treat == "No Story"], na.rm =T) 
mean.2.policy <- mean(data$policy[data$treat == "Sympathetic Black"], na.rm =T) 
mean.3.policy <-mean(data$policy[data$treat == "Sympathetic White"], na.rm =T) 
mean.4.policy <- mean(data$policy[data$treat == "Unsympathetic Black"], na.rm =T) 
mean.5.policy <- mean(data$policy[data$treat == "Unsympathetic White"], na.rm =T) ##

se.1.policy <- se(data$policy[data$treat == "No Story"]) 
se.2.policy <-se(data$policy[data$treat == "Sympathetic Black"]) 
se.3.policy <-se(data$policy[data$treat == "Sympathetic White"]) 
se.4.policy <- se(data$policy[data$treat == "Unsympathetic Black"])
se.5.policy <- se(data$policy[data$treat == "Unsympathetic White"])


## DV 2: Candidate pref
mean.1.candidatesupport <- mean(data$candidatesupport[data$treat == "No Story"], na.rm =T) 
mean.2.candidatesupport <- mean(data$candidatesupport[data$treat == "Sympathetic Black"], na.rm =T) 
mean.3.candidatesupport <-mean(data$candidatesupport[data$treat == "Sympathetic White"], na.rm =T) 
mean.4.candidatesupport <- mean(data$candidatesupport[data$treat == "Unsympathetic Black"], na.rm =T) 
mean.5.candidatesupport <- mean(data$candidatesupport[data$treat == "Unsympathetic White"], na.rm =T) ##

se.1.candidatesupport <- se(data$candidatesupport[data$treat == "No Story"]) 
se.2.candidatesupport <-se(data$candidatesupport[data$treat == "Sympathetic Black"]) 
se.3.candidatesupport <-se(data$candidatesupport[data$treat == "Sympathetic White"]) 
se.4.candidatesupport <- se(data$candidatesupport[data$treat == "Unsympathetic Black"])
se.5.candidatesupport <- se(data$candidatesupport[data$treat == "Unsympathetic White"])


## DV 3: Taxes for treatment
mean.1.taxesfortreatment <- mean(data$taxesfortreatment[data$treat == "No Story"], na.rm =T) 
mean.2.taxesfortreatment <- mean(data$taxesfortreatment[data$treat == "Sympathetic Black"], na.rm =T) 
mean.3.taxesfortreatment <-mean(data$taxesfortreatment[data$treat == "Sympathetic White"], na.rm =T) 
mean.4.taxesfortreatment <- mean(data$taxesfortreatment[data$treat == "Unsympathetic Black"], na.rm =T) 
mean.5.taxesfortreatment <- mean(data$taxesfortreatment[data$treat == "Unsympathetic White"], na.rm =T) ##

se.1.taxesfortreatment <- se(data$taxesfortreatment[data$treat == "No Story"]) 
se.2.taxesfortreatment <-se(data$taxesfortreatment[data$treat == "Sympathetic Black"]) 
se.3.taxesfortreatment <-se(data$taxesfortreatment[data$treat == "Sympathetic White"]) 
se.4.taxesfortreatment <- se(data$taxesfortreatment[data$treat == "Unsympathetic Black"])
se.5.taxesfortreatment <- se(data$taxesfortreatment[data$treat == "Unsympathetic White"])

## DV 4: Emotion scale
mean.1.emotionscale <- mean(data$emotionscale[data$treat == "No Story"], na.rm =T) 
mean.2.emotionscale <- mean(data$emotionscale[data$treat == "Sympathetic Black"], na.rm =T) 
mean.3.emotionscale <-mean(data$emotionscale[data$treat == "Sympathetic White"], na.rm =T) 
mean.4.emotionscale <- mean(data$emotionscale[data$treat == "Unsympathetic Black"], na.rm =T) 
mean.5.emotionscale <- mean(data$emotionscale[data$treat == "Unsympathetic White"], na.rm =T) ##

se.1.emotionscale <- se(data$emotionscale[data$treat == "No Story"]) 
se.2.emotionscale <-se(data$emotionscale[data$treat == "Sympathetic Black"]) 
se.3.emotionscale <-se(data$emotionscale[data$treat == "Sympathetic White"]) 
se.4.emotionscale <- se(data$emotionscale[data$treat == "Unsympathetic Black"])
se.5.emotionscale <- se(data$emotionscale[data$treat == "Unsympathetic White"])

## create data frame to plot

data.test = data.frame(variable = c("Support treatment policy", "Support treatment policy", "Support treatment policy", "Support treatment policy", "Support treatment policy",
                                    "Support treatment candidate " , "Support treatment candidate " , "Support treatment candidate " , "Support treatment candidate " , "Support treatment candidate " , 
                                    "Taxes for treatment ", "Taxes for treatment ", "Taxes for treatment ", "Taxes for treatment ", "Taxes for treatment ",
                                    "Sympathetic emotional response ", "Sympathetic emotional response ", "Sympathetic emotional response ", "Sympathetic emotional response ","Sympathetic emotional response "),
                       treat = c("Control",
                                 "Symp. Black",
                                 "Symp. White",
                                 "Unsymp. Black",
                                 "Unsymp. White"),
                       effect = c(mean.1.policy, mean.2.policy, mean.3.policy, mean.4.policy, mean.5.policy,
                                  mean.1.candidatesupport, mean.2.candidatesupport, mean.3.candidatesupport, mean.4.candidatesupport, mean.5.candidatesupport,
                                  mean.1.taxesfortreatment, mean.2.taxesfortreatment, mean.3.taxesfortreatment, mean.4.taxesfortreatment, mean.5.taxesfortreatment,
                                  mean.1.emotionscale, mean.2.emotionscale, mean.3.emotionscale, mean.4.emotionscale, mean.5.emotionscale),
                       
                       
                       se = c(se.1.policy, se.2.policy, se.3.policy, se.4.policy, se.5.policy,
                              se.1.candidatesupport, se.2.candidatesupport, se.3.candidatesupport, se.4.candidatesupport, se.5.candidatesupport,
                              se.1.taxesfortreatment, se.2.taxesfortreatment, se.3.taxesfortreatment, se.4.taxesfortreatment, se.5.taxesfortreatment,
                              se.1.emotionscale, se.2.emotionscale, se.3.emotionscale, se.4.emotionscale, se.5.emotionscale)) 
# data.test

data.test$treat <- factor(data.test$treat , levels =c("Control",
                                                      "Symp. Black",
                                                      "Symp. White",
                                                      "Unsymp. Black",
                                                      "Unsymp. White"))

# data.test


## plot

p1<- ggplot(data.test[1:5,], aes(x=effect, y=treat, fill=treat)) +
  facet_grid(.~variable) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"),  axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(0.2,0.9)) +
  geom_errorbarh(aes(xmin = (effect) - 1.39*(se), xmax= (effect) + 1.39*(se)), height = .3)
# p1


p2<- ggplot(data.test[6:10,], aes(x=effect, y=treat, fill=treat)) +
  facet_grid(.~variable) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"),  axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(0.2,0.9)) +
  geom_errorbarh(aes(xmin = (effect) - 1.39*(se), xmax= (effect) + 1.39*(se), height = .3))

# p2

p3<- ggplot(data.test[11:15,], aes(x=effect, y=treat, fill=treat)) +
  facet_grid(.~variable) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"),  axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(0.2,0.9)) +
  geom_errorbarh(aes(xmin = (effect) - 1.39*(se), xmax= (effect) + 1.39*(se), height = .3))

# p3

p4<- ggplot(data.test[16:20,], aes(x=effect, y=treat, fill=treat)) +
  facet_grid(.~variable) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"),  axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=c(0.2,0.9)) +
  geom_errorbarh(aes(xmin = (effect) - 1.39*(se), xmax= (effect) + 1.39*(se), height = .3))

# p4

# grid.arrange(p1, p2, p3, p4, nrow = 2) # View in R

## Print figure
cat("\nFigure A5: Mean outcome values across experimental conditions (raw values, 83% CIs)\n")
pdf("./output/FigureA5.pdf", width = 10.5, height = 6)
grid.arrange(p1, p2, p3, p4, nrow = 2)
dev.off()
cat("Saved Figure A5 in /output")

