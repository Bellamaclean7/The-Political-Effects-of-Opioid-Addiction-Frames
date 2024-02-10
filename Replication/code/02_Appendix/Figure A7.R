####################################################################################
## Replication file: "Political Effects of Opioid Addiction Frames"
## Appendix
## Figure 7
## Steps: 1. calculate mean and se for observed compliance,
## 2. calculate mean and se for raw percentages 3. create figure
####################################################################################
rm(list=ls()) ## clear list space

#####################
### Packages ########
#####################

library(ggplot2)
library(gridExtra)

#####################
### Function #####
#####################
se <- function(x) sqrt(var(x, na.rm = T)/length(x)) ## standard error

#####################
### Load Data ########
#####################
## read in data
data <- read.csv("tessdata.csv", head = T) ## 1517 obs

#########################################################################################
## 1) Report means of each observed compliance measure (whether respondents over-estimate
# whites’ and blacks’ percentage of drug users), for each experimental condition 
#########################################################################################

v <- "overrepwhiteaddicts" 

mean.1.pw <- mean(data[[v]][data$treat == "No Story"], na.rm =T) 
mean.2.pw <-mean(data[[v]][data$treat == "Sympathetic White"], na.rm =T) 
mean.3.pw <- mean(data[[v]][data$treat == "Unsympathetic White"], na.rm =T) ##
mean.4.pw <- mean(data[[v]][data$pooledwhite ==1], na.rm =T) ##
mean.5.pw <- mean(data[[v]][data$treat == "Sympathetic Black"], na.rm =T) 
mean.6.pw <- mean(data[[v]][data$treat == "Unsympathetic Black"], na.rm =T) 
mean.7.pw <- mean(data[[v]][data$pooledblack ==1], na.rm =T) ##

se.1.pw <- se(data[[v]][data$treat == "No Story"]) 
se.2.pw <-se(data[[v]][data$treat == "Sympathetic White"]) 
se.3.pw <- se(data[[v]][data$treat == "Unsympathetic White"])
se.4.pw <- se(data[[v]][data$pooledwhite ==1])
se.5.pw <-se(data[[v]][data$treat == "Sympathetic Black"]) 
se.6.pw <- se(data[[v]][data$treat == "Unsympathetic Black"])
se.7.pw <- se(data[[v]][data$pooledblack ==1])


# DV2: 
v2 <- "overrepblackaddicts"
mean.1.pb <- mean(data[[v2]][data$treat == "No Story"], na.rm =T) 
mean.2.pb <-mean(data[[v2]][data$treat == "Sympathetic White"], na.rm =T) 
mean.3.pb <- mean(data[[v2]][data$treat == "Unsympathetic White"], na.rm =T) ##
mean.4.pb <- mean(data[[v2]][data$pooledwhite ==1], na.rm =T) ##
mean.5.pb <- mean(data[[v2]][data$treat == "Sympathetic Black"], na.rm =T) 
mean.6.pb <- mean(data[[v2]][data$treat == "Unsympathetic Black"], na.rm =T) 
mean.7.pb <- mean(data[[v2]][data$pooledblack ==1], na.rm =T) ##

se.1.pb <- se(data[[v2]][data$treat == "No Story"]) 
se.2.pb <-se(data[[v2]][data$treat == "Sympathetic White"]) 
se.3.pb <- se(data[[v2]][data$treat == "Unsympathetic White"])
se.4.pb <- se(data[[v2]][data$pooledwhite ==1])
se.5.pb <-se(data[[v2]][data$treat == "Sympathetic Black"]) 
se.6.pb <- se(data[[v2]][data$treat == "Unsympathetic Black"])
se.7.pb <- se(data[[v2]][data$pooledblack ==1])

############################################
## Create dataframe for plot
############################################
# Labels
a2 <- c(rep("Proportion Over-Estimated White Addicts", 7), rep("Proportion Over-Estimated Black Addicts", 7))

data.test = data.frame(variable = a2, 
                       treat = c("Control",
                                 "Symp. White",
                                 "Unsymp. White",
                                 "Pooled White",
                                 "Symp. Black",
                                 "Unsymp. Black",
                                 "Pooled Black"),
                       effect = c(mean.1.pw, mean.2.pw, mean.3.pw, 
                                  mean.4.pw, 
                                  mean.5.pw,mean.6.pw, 
                                  mean.7.pw,
                                  mean.1.pb, mean.2.pb, mean.3.pb, 
                                  mean.4.pb, 
                                  mean.5.pb,mean.6.pb, 
                                  mean.7.pb),
                       
                       se = c(se.1.pw, se.2.pw, se.3.pw, 
                              se.4.pw, 
                              se.5.pw,se.6.pw, 
                              se.7.pw,
                              se.1.pb, se.2.pb, se.3.pb, 
                              se.4.pb, 
                              se.5.pb,se.6.pb, 
                              se.7.pb)) 

data.test$treat <- factor(data.test$treat , levels =c("Control",
                                                      "Symp. White",
                                                      "Unsymp. White",
                                                      "Pooled White",
                                                      "Symp. Black",
                                                      
                                                      "Unsymp. Black",
                                                      
                                                      "Pooled Black"))
# data.test

############################################
## Plot facets 1 and 2
############################################
set <- c(0.09, 1) 
p1 <- ggplot(data.test[c(1:3, 5:6),], aes(x=effect, y=treat, fill=treat)) +
  facet_grid(.~variable) +
  geom_point(stat="identity") +
  #xlab("Mean value") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=set) +
  geom_errorbarh(aes(xmin = (effect) - 1.39*(se), xmax= (effect) + 1.39*(se)), height = .3)
# p1

p2 <- ggplot(data.test[c(8:10, 12:13),], aes(x=effect, y=treat, fill=treat)) +
  facet_grid(.~variable) +
  geom_point(stat="identity") +
  # xlab("Mean value") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=set) +
  geom_errorbarh(aes(xmin = (effect) - 1.39*(se), xmax= (effect) + 1.39*(se), height = .3))

# p2

#########################################################################################
# 2) Report means of the raw percentages of black and of white addicts, 
# for each experimental condition
#########################################################################################

# set outcome measure 
v <- "perwhiteaddicts"

mean.1.pw <- mean(data[[v]][data$treat == "No Story"], na.rm =T) 
mean.2.pw <-mean(data[[v]][data$treat == "Sympathetic White"], na.rm =T) 
mean.3.pw <- mean(data[[v]][data$treat == "Unsympathetic White"], na.rm =T) ##
mean.4.pw <- mean(data[[v]][data$pooledwhite ==1], na.rm =T) ##
mean.5.pw <- mean(data[[v]][data$treat == "Sympathetic Black"], na.rm =T) 
mean.6.pw <- mean(data[[v]][data$treat == "Unsympathetic Black"], na.rm =T) 
mean.7.pw <- mean(data[[v]][data$pooledblack ==1], na.rm =T) ##

se.1.pw <- se(data[[v]][data$treat == "No Story"]) 
se.2.pw <-se(data[[v]][data$treat == "Sympathetic White"]) 
se.3.pw <- se(data[[v]][data$treat == "Unsympathetic White"])
se.4.pw <- se(data[[v]][data$pooledwhite ==1])
se.5.pw <-se(data[[v]][data$treat == "Sympathetic Black"]) 
se.6.pw <- se(data[[v]][data$treat == "Unsympathetic Black"])
se.7.pw <- se(data[[v]][data$pooledblack ==1])


# DV2: 
v2 <- "perblackaddicts"

mean.1.pb <- mean(data[[v2]][data$treat == "No Story"], na.rm =T) 
mean.2.pb <-mean(data[[v2]][data$treat == "Sympathetic White"], na.rm =T) 
mean.3.pb <- mean(data[[v2]][data$treat == "Unsympathetic White"], na.rm =T) ##
mean.4.pb <- mean(data[[v2]][data$pooledwhite ==1], na.rm =T) ##
mean.5.pb <- mean(data[[v2]][data$treat == "Sympathetic Black"], na.rm =T) 
mean.6.pb <- mean(data[[v2]][data$treat == "Unsympathetic Black"], na.rm =T) 
mean.7.pb <- mean(data[[v2]][data$pooledblack ==1], na.rm =T) ##

se.1.pb <- se(data[[v2]][data$treat == "No Story"]) 
se.2.pb <-se(data[[v2]][data$treat == "Sympathetic White"]) 
se.3.pb <- se(data[[v2]][data$treat == "Unsympathetic White"])
se.4.pb <- se(data[[v2]][data$pooledwhite ==1])
se.5.pb <-se(data[[v2]][data$treat == "Sympathetic Black"]) 
se.6.pb <- se(data[[v2]][data$treat == "Unsympathetic Black"])
se.7.pb <- se(data[[v2]][data$pooledblack ==1])

############################################
## Create dataframe for plot
############################################
# Labels
a1 <- c(rep("Proportion White Addicts", 7), rep("Proportion Black Addicts", 7))

data.test = data.frame(variable = a1, 
                       treat = c("Control",
                                 "Symp. White",
                                 "Unsymp. White",
                                 "Pooled White",
                                 "Symp. Black",
                                 "Unsymp. Black",
                                 "Pooled Black"),
                       effect = c(mean.1.pw, mean.2.pw, mean.3.pw, 
                                  mean.4.pw, 
                                  mean.5.pw,mean.6.pw, 
                                  mean.7.pw,
                                  mean.1.pb, mean.2.pb, mean.3.pb, 
                                  mean.4.pb, 
                                  mean.5.pb,mean.6.pb, 
                                  mean.7.pb),
                       
                       se = c(se.1.pw, se.2.pw, se.3.pw, 
                              se.4.pw, 
                              se.5.pw,se.6.pw, 
                              se.7.pw,
                              se.1.pb, se.2.pb, se.3.pb, 
                              se.4.pb, 
                              se.5.pb,se.6.pb, 
                              se.7.pb)) 

data.test$treat <- factor(data.test$treat , levels =c("Control",
                                                      "Symp. White",
                                                      "Unsymp. White",
                                                      "Pooled White",
                                                      "Symp. Black",
                                                      
                                                      "Unsymp. Black",
                                                      
                                                      "Pooled Black"))
# data.test

############################################
## Plots facets 3 and 4
############################################
set <- c(0.09, 1) 
p3 <- ggplot(data.test[c(1:3, 5:6),], aes(x=effect, y=treat, fill=treat)) +
  facet_grid(.~variable) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=set) +
  geom_errorbarh(aes(xmin = (effect) - 1.39*(se), xmax= (effect) + 1.39*(se)), height = .3)
# p3

p4<- ggplot(data.test[c(8:10, 12:13),], aes(x=effect, y=treat, fill=treat)) +
  facet_grid(.~variable) +
  geom_point(stat="identity") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(text = element_text(size=14, face="bold"), axis.title.y=element_blank(), 
        axis.title.x=element_blank()) +
  scale_x_continuous(limits=set) +
  geom_errorbarh(aes(xmin = (effect) - 1.39*(se), xmax= (effect) + 1.39*(se), height = .3))

# p4

######################################
## Create full plot
######################################
# grid.arrange(p1, p2, p3, p4, nrow = 2) ## view in R


# print figure
cat("\nFigure A7: Observed compliance means across experimental conditions (raw values, 83% CIs) \n")

pdf("./output/FigureA7.pdf", width = 9.5, height = 6)
grid.arrange(p1, p2, p3, p4, nrow = 2) 
dev.off()
cat("Saved Figure A7 in /output")

