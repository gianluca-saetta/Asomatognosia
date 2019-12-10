# set data input folder as default location of input files
workspace = getwd()
setwd(workspace)


#Import the database in excel
library(readxl)
MentalRotation <- read_excel("Data.xlsx")


#Inform R about the desired structure of some variables as factors
MentalRotation$Subject <- as.factor(MentalRotation$Subject)
MentalRotation$Laterality <- as.factor(MentalRotation$Laterality)
MentalRotation$Limb  <- as.factor(MentalRotation$Limb)
MentalRotation$View  <- as.factor(MentalRotation$View)
MentalRotation$Rotation  <- as.factor(MentalRotation$Rotation)


#Rename levels of the factors (only for aesthetics)
levels(MentalRotation$Laterality)[levels(MentalRotation$Laterality)=="right"] <- "Right"
levels(MentalRotation$Laterality)[levels(MentalRotation$Laterality)=="left"] <- "Left"
levels(MentalRotation$View)[levels(MentalRotation$View)=="back"] <- "Back"
levels(MentalRotation$View)[levels(MentalRotation$View)=="palm"] <- "Palm"
MentalRotation$Rotation <- as.factor(MentalRotation$Rotation)
levels(MentalRotation$Rotation)[levels(MentalRotation$Rotation)=="0"] <- "0°"
levels(MentalRotation$Rotation)[levels(MentalRotation$Rotation)=="180"] <- "180°"
levels(MentalRotation$Rotation)[levels(MentalRotation$Rotation)=="90"] <- "90°"
levels(MentalRotation$Rotation)[levels(MentalRotation$Rotation)=="270"] <- "270°"
MentalRotation$Group <- as.factor(MentalRotation$Group)
levels(MentalRotation$Group)[levels(MentalRotation$Group)== "P1"] <- "ASG" 
levels(MentalRotation$Group)[levels(MentalRotation$Group)== "Controls"] <- "Ten Controls" 


#Relevel the levels of the factor "group" (only for the plot aesthetics)
MentalRotation$Group<- relevel(MentalRotation$Group, "ASG")
MentalRotation$Rotation <- as.factor(MentalRotation$Rotation)


#Obtain information about sex age and group of the participants
MentalRotation_demographics  <- aggregate(data = MentalRotation, Group ~ Subject + Age + Sex,
                                          FUN = unique)


#Mean age and standard deviation of the controls 
mean(MentalRotation_demographics$Age[which(MentalRotation_demographics$Group == "Ten Controls")])
sd(MentalRotation_demographics$Age[which(MentalRotation_demographics$Group == "Ten Controls")])


#Calculate accuracy for each participant
MentalRotation_ACC_by_subject  <- aggregate(data = MentalRotation, ACC ~ Group + Subject + Age + Sex,
                                          FUN = mean)


##Mean error rates in ASG and and in the controls
#Controls 
mean(MentalRotation_ACC_by_subject$ACC[which(MentalRotation_demographics$Group == "Ten Controls")])
sd(MentalRotation_ACC_by_subject$ACC[which(MentalRotation_demographics$Group == "Ten Controls")])


#ASG
mean(MentalRotation_ACC_by_subject$ACC[which(MentalRotation_demographics$Group == "ASG")])


#For RTs analysis we consider only RTs associated with correct responses
library(dplyr)
MentalRotation <- MentalRotation %>%
  filter(ACC == 1)


#### RTS to stimuli presented at 90° and 270° ####
MentalRotation_RTS_MOLA <- MentalRotation %>%
  filter(Rotation == "90°"  | Rotation == "270°")


# Plot 
#Create an interaction factor Laterality * View * Limb 
MentalRotation_RTS_MOLA$Laterality_View_Limb <-  interaction(MentalRotation_RTS_MOLA$Laterality, MentalRotation_RTS_MOLA$View, MentalRotation_RTS_MOLA$Limb,  sep = " ")


#Aggregate the data of each participant around the mean RTS
plot_MOLA <- aggregate(data = MentalRotation_RTS_MOLA, RT ~ View + Subject + Group + Laterality_View_Limb + Rotation, FUN  = mean)


#Calculate Standard deviation for error bar in the plot
library(Rmisc)
plot_MOLA <- summarySE(plot_MOLA, measurevar="RT", groupvars=c("Rotation","Group", "Laterality_View_Limb"))


#### Fig. 4 ####
library(ggplot2)
ggplot_control_ResponseTime <- ggplot(plot_MOLA, aes(x = Rotation , y= RT,linetype = Group, shape = Group))
ggplot_control_ResponseTime +
  facet_grid(. ~ Laterality_View_Limb) + 
  stat_summary(fun.y =mean, geom = "point", size = 3) + 
  scale_colour_brewer(palette= "Set1", direction= 1) +
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), width=.1) +
  stat_summary(fun.y = mean, geom = "line", size = 1,  aes(group = Group)) + 
  labs( x = "", y = "RTs (msec)")  +
  expand_limits(y=c(0, 7000)) +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16,face="bold")) + 
  theme(legend.text=element_text(size=16)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 


####Bayesian Crawford  T-test for comparing ASG's RTS to left feet shown at 270° in palm view with
#those of the controls#


#Arrange the data for application of the function crawford.test from the package "psycho"
MentalRotation_RTS_MOLA$Laterality_Rotation_View_Limb <- interaction(MentalRotation_RTS_MOLA$Laterality, MentalRotation_RTS_MOLA$Rotation, MentalRotation_RTS_MOLA$View, MentalRotation_RTS_MOLA$Limb, sep = " ")
scores_contr_MOLA <- aggregate(data = MentalRotation_RTS_MOLA[which(MentalRotation_RTS_MOLA$Laterality_Rotation_View_Limb =="Left 270° Palm foot" &  MentalRotation_RTS_MOLA$Group =='Ten Controls'),], 
                              RT ~ Subject, FUN = mean)
scores_contr_MOLA_vector <- as.vector(scores_contr_MOLA$RT)
Mean_Controls_MOLA <- mean(scores_contr_MOLA$RT)
SD_Controls_MOLA <- sd(scores_contr_MOLA$RT)
Mean_P1_MOLA <- mean(subset(MentalRotation_RTS_MOLA,Laterality_Rotation_View_Limb == "Left 270° Palm foot" & Group == "ASG")$RT)


#Crawford test 
library(psycho)
crawford.test(Mean_P1_MOLA, scores_contr_MOLA_vector, Mean_Controls_MOLA, SD_Controls_MOLA, n = 10)
plot(crawford.test(Mean_P1_MOLA, scores_contr_MOLA_vector, Mean_Controls_MOLA, SD_Controls_MOLA, n = 10))


####Bqyesian Crawford  T-test for comparing ASG's RTS to left feet shown at 90° in palm view with
#those of the controls


#Arrange the data for application of the function crawford.test from the package "psycho"
scores_contr_MOLA_90 <- aggregate(data = MentalRotation_RTS_MOLA[which(MentalRotation_RTS_MOLA$Laterality_Rotation_View_Limb =="Left 90° Palm foot" &  MentalRotation_RTS_MOLA$Group =='Ten Controls'),], 
                               RT ~ Subject, FUN = mean)
scores_contr_MOLA_90_vector <- as.vector(scores_contr_MOLA_90$RT)
Mean_Controls_MOLA_90 <- mean(scores_contr_MOLA_90$RT)
SD_Controls_MOLA_90 <- sd(scores_contr_MOLA_90$RT)
Mean_P1_MOLA_90 <- mean(subset(MentalRotation_RTS_MOLA,Laterality_Rotation_View_Limb == "Left 90° Palm foot" & Group == "ASG")$RT)


#Crawford test 
crawford.test(Mean_P1_MOLA_90, scores_contr_MOLA_90_vector, Mean_Controls_MOLA_90, SD_Controls_MOLA_90, n = 10)
plot(crawford.test(Mean_P1_MOLA, scores_contr_MOLA_vector, Mean_Controls_MOLA, SD_Controls_MOLA, n = 10))


#### RTS to stimuli presented at 0° and 80°  #### 
MentalRotation_RTS_inversion <- MentalRotation %>%
  filter(Rotation == "0°" | Rotation == "180°" )


# Plot 
#Create an interaction factor Laterality * View * Limb 
MentalRotation_RTS_inversion$Laterality_View_Limb <-  interaction(MentalRotation_RTS_inversion$Laterality, MentalRotation_RTS_inversion$View, MentalRotation_RTS_inversion$Limb, sep = " ")


#Aggregate the data of each participant around the mean RTS
plot_Inversion <- aggregate(data = MentalRotation_RTS_inversion, RT ~ View + Subject + Group + Laterality_View_Limb + Rotation, FUN  = mean)


#Calculate Standard deviation for error bar in the plot
plot_Inversion <- summarySE(plot_Inversion, measurevar="RT", groupvars=c("Rotation","Group", "Laterality_View_Limb"))


#### Fig. 5 ####
ggplot_control_ResponseTime <- ggplot(plot_Inversion, aes(x = Rotation , y= RT,linetype = Group, shape = Group))
ggplot_control_ResponseTime +
  facet_grid(. ~ Laterality_View_Limb) + 
  stat_summary(fun.y =mean, geom = "point", size = 3) + 
  scale_colour_brewer(palette= "Set1", direction= 1) +
  geom_errorbar(aes(ymin=RT-se, ymax=RT+se), width=.1) +
  stat_summary(fun.y = mean, geom = "line", size = 1, aes(group = Group)) + 
  labs( x = "", y = "RTs (msec)")  +
  expand_limits(y=c(0, 7000)) +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=15,face="bold")) + 
  theme(legend.text=element_text(size=14)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 


####Bayesian Crawford  T-test for comparing ASG's RTS to left feet shown at 180° in back view with
#those of the controls


#Arrange the data for application of the function crawford.test from the package "psycho"
MentalRotation_RTS_inversion$Laterality_Rotation_View_Limb <- interaction(MentalRotation_RTS_inversion$Laterality, MentalRotation_RTS_inversion$Rotation, MentalRotation_RTS_inversion$View,
                                                                     MentalRotation_RTS_inversion$Limb, sep = " ")
scores_contr_inversion_back <- aggregate(data = MentalRotation_RTS_inversion[which(MentalRotation_RTS_inversion$Laterality_Rotation_View_Limb =="Left 180° Back foot" &  MentalRotation_RTS_inversion$Group =='Ten Controls'),], 
                                         RT ~ Subject, FUN = mean)
scores_contr_inversion_back_vector <- as.vector(scores_contr_inversion_back$RT)
Mean_Controls_Inversion_Back <- mean(scores_contr_inversion_back$RT)
SD_Controls_Inversion_Back <- sd(scores_contr_inversion_back$RT)
Mean_P1_Inversion_Back <- mean(subset(MentalRotation_RTS_inversion,Laterality_Rotation_View_Limb == "Left 180° Back foot" & Group == "ASG")$RT)


#Crawford test 
library(psycho)
crawford.test(Mean_P1_Inversion_Back, scores_contr_inversion_back_vector, Mean_Controls_Inversion_Back, SD_Controls_Inversion_Back, n = 9)
plot(crawford.test(Mean_P1_MOLA, scores_contr_MOLA_vector, Mean_Controls_MOLA, SD_Controls_MOLA, n = 10))

