#IS THERE AN EFFECT BETWEEN ATTENDANCE AND TUTOR AND COURSE SATISFACTION?
#H0: There is no effect
#H1: There is an effect.


df <- read.csv("Data1(1) (2).csv", header = T)
View(df)
df <- na.omit(df)
dfpca <- df[6:33]
library(psych)

#Use KMO to check adequacy of sampling
KMO(dfpca)
#KMO = 0.99 - this shows that sampling is adequate

#Factor Analysis 

fit <- prcomp(dfpca)
screeplot(fit, type = "line")
#Screeplot shows that there are 3 factors

library(paran)
paran(dfpca)

#Parallel analysis shows that there are 2 components 
#Agree with parallel analysis as there is no theory leading the study.

#Loadings 
fit <- pca(dfpca,2)
fit

#Set 2 components in line with parallel analysis 
library(psych)
fit <- pca(dfpca, nfactors = 2)
fit

#Name RC1 and RC2 
#RC1 = "Instructor Satisfaction"
#RC2 = "Course Satisfaction"
df$RC1 <- fit$scores[,1]
df$RC2 <- fit$scores[,2]

#Change into factors
df$instr<- as.factor(df$instr)
df$class <- as.factor(df$class)
df$nb.repeat <- as.factor(df$nb.repeat)
df$attendance <- as.factor(df$attendance)
df$difficulty <- as.factor(df$difficulty)

#Check for normality
shapiro.test(df$RC1)
shapiro.test(`Instructor Satisfaction`)
#Datasets too large for shapiro test so will check for normality using QQPlots

library(tidyverse)
qqnorm(df$RC1)
qqline(df$RC1, col = "red")
qqnorm(df$RC2)
qqline(df$RC2, col = "red")
#Datasets look ok, but could be improved using log function

log(df$RC1) 
qqnorm(log(df$RC1)) 
qqline(log(df$RC1), col = "blue")

log(df$RC2) 
qqnorm(log(df$RC2)) 
qqline(log(df$RC2), col = "blue")

#Course Satisfaction = RC1
shapiro.test(log(fit$scores[,1]))
shapiro.test(log(fit$scores[,2]))
#QQplot spread for RC1 and RC2 looks worse using log function, 
#so will continue without using log function

#Check using SQRT function
dfRC1SQRT <- sqrt(fit$scores[,1])
shapiro.test(dfRC1SQRT)
qqnorm(dfRC1SQRT)
qqline(dfRC1SQRT, col = "red")

dfRC2SQRT <- sqrt(fit$scores[,2])
qqnorm(dfRC2SQRT)
qqline(dfRC2SQRT, col = "red")

#QQplot spread for RC1/RC2 looks worse using 
#SQRT function, 
#so will continue without using SQRT function and use 
#original

df$RC1 <- fit$scores[,1]
df$RC2 <- fit$scores[,2]


plot(density(df$RC1))
plot(density(df$RC2))


library(car)

#Test for homogeneity of variances using Levene's and boxplot

#LEVENE'S TEST FOR HOMOGENEITY OF VARIANCES [RC1]

leveneTest(df$RC1~df$instr)
#Levene's test shows significance between class and F(2,5817) 7.4841, P<0.05
leveneTest(df$RC1~df$class)
#Levene's test shows significance F(12, 5807) 4.1216, P<0.05
leveneTest(df$RC1~df$nb.repeat)
#Levene's test shows no significance F(2, 5817), 0.0914, P>0.05
leveneTest(df$RC1~df$attendance)
#Levene's test shows significance in F(4,5815), 8.3093, P<0.05
leveneTest(df$RC1~df$difficulty)
#Levene's test shows significance in F(4,5815), 13.915, P<0.05

#LEVENE'S TEST FOR HOMOGENEITY OF VARIANCES [RC2]

leveneTest(df$RC2~df$class)
#Levene's test shows violation. F(12,5807), 4.3021, P<0.05
leveneTest(df$RC2~df$instr)
#Levene's test shows violation. F(2, 5817) 4.571, P<0.05
leveneTest(df$RC2~df$nb.repeat)
#Levene's test shows no violation. F(2, 5817), 0.3571, P>0.05
leveneTest(df$RC2~df$attendance)
#Levene's test shows violation. F(4,5815), 8.1744, P<0.05
leveneTest(df$RC2~df$difficulty)
#Levene's test shows violation. F(4,5815), 11.244, P<0.05

#CHANGE DIFFICULTY BACK TO NUMERIC
df$difficulty <- as.numeric(df$difficulty)
t.test(df$RC1,df$difficulty)
#[t(10731) = -126.46, P <0.05] therefore there is a significant difference
#between RC1 and df$difficulty
t.test(df$RC2,df$difficulty)
#[t(10731) = -126.46, P <0.05] therefore there is a significant difference
#between RC2 and df$difficulty

#As the p value of 'nb.repeat' is greater than .05, it shows that the variances
#are not exactly different. This means that for 'nb.repeat' H0 is accepted.
#For the rest, H0 is rejected.

#RC1 boxplot
boxplot(df$RC1~df$instr)
boxplot(df$RC1~df$class)
boxplot(df$RC1~df$nb.repeat)
boxplot(df$RC1~df$attendance)
boxplot(df$RC1~df$difficulty) #we are focusing on difficulty

#RC2 boxplot
boxplot(df$RC2~df$instr)
boxplot(df$RC2~df$class)
boxplot(df$RC2~df$nb.repeat)
boxplot(df$RC2~df$attendance) 
boxplot(df$RC2~df$difficulty) #we are focusing on difficulty

#MANOVA

DFFit <- manova(cbind(df$RC1, df$RC2) ~ 
                  df$class+df$difficulty+df$attendance+df$instr+df$nb.repeat)
table <- 
summary(DFFit)
summary.aov(DFFit)

#RESULTS: 
#Course Satisfaction and Instructor Satisfaction
#MANOVA assumes a significant effect between class (0.06), difficulty (0.04),
#attendance (0.02), and instructor (<0.01)

#RC1 
#Course Satisfaction
#Class has a significant effect on RC1. 
#F(12, 5796), 15.107, P<0.05

#Difficulty has a significant effect on RC1
#F(4,5796), 60.4112, P<0.05

#Attendance has a significant effect on RC1
#F(4,5796), 29.065, P<0.05

#Instructor has a significant effect on RC1.
#F(1,5796), 32.341, P<0.05

#NB Repeats has a significant effect on RC1
#F(2,5796), 7.435, P<0.05

#RC2 Instructor Satisfaction

#Class has a significant effect on RC2. 
#F(12, 5796), 17.5040, P<0.05

#Difficulty has a significant effect on RC2
#F(4,5796), 10.8088, P<0.05

#Attendance has a significant effect on RC2
#F(4,5796), 3.6648, P<0.05

#Instructor has no significant effect on RC2.
#F(1,5796), 0.0029, P>0.05

#NB Repeat has no significant effect on RC2.
#F(2,5796) 2.1981, P>0.05

#ANOVA for significant variables

#RC1 ~ Class 
Class1Ao <- aov(df$RC1 ~df$class)
summary(Class1Ao)
etaSquared(Class1Ao)
library(Rcmdr)
#Anova shows significance F(12, 5807) = 14.15, P<0.05, eta-squared = 0.03

library(Rcmdr)
load(Rcmdr)

#RC1 ~ Attendance 
Attendance1Ao <- aov(df$RC1 ~df$attendance)
summary(Attendance1Ao)
summary.aov(Attendance1Ao)
install.packages(FSA)
library(FSA)
etaSquared(Attendance1Ao)
TukeyHSD(Attendance1Ao)

#Anova shows significance F(4, 5815) 70.64, P<0.05, eta-squared = 0.05

library(lsr)
etaSquared(Attendance1Ao)

#RC1 ~ NB Repeats
NbRepeats <- aov(df$RC1~df$nb.repeat)
summary(NbRepeats)
etaSquared(NbRepeats)
#F(2, 5817) = 7.24, P<0.05, eta-squared = 0.003

#WE ARE LOOKING AT DIFFICULTY AS OUR DEPENDENT VARIABLE
#RC1 ~ Difficulty 
Difficulty1Ao <- aov(df$RC1 ~df$difficulty)
summary(Difficulty1Ao)
summary.aov(Difficulty1Ao)
etaSquared(Difficulty1Ao)
TukeyHSD(Difficulty1Ao)
#Anova shows significance F(4, 5815) = 71.55, P<0.05, eta-squared = 0.05

#RC1 ~ Instr
Instr1Ao <- aov(df$RC1 ~df$instr)
summary(Instr1Ao)
etaSquared(Instr1Ao)
TukeyHSD(Instr1Ao)
#Anova shows significance F(2, 5817) 21.88, P<0.05, eta-squared = 0.008

#RC2 ANOVA 

#RC2 ~ Attendance 
Attendace2Ao <- aov(df$RC2 ~df$attendance)
summary(Attendace2Ao)
summary.aov(Attendace2Ao)
etaSquared(Attendace2Ao)
TukeyHSD(Attendace2Ao)
#Anova shows significance F(4, 5815) = 4.361, P<0.05, eta-squared = 0.003

#RC2 ~ Class
Class2Ao <- aov(df$RC2 ~df$class)
summary(Class2Ao)
summary.aov(Class2Ao)
etaSquared(Class2Ao)
#Anova shows significance F(12, 5807) = 17.35, P<0.05, eta-squared = 0.04

#WE ARE LOOKING AT DIFFICULTY AS OUR DEPENDENT VARIABLE
#RC2 ~ Difficulty
Difficulty2Ao <- aov(df$RC2 ~df$difficulty)
summary(Difficulty2Ao)
summary.aov(Difficulty2Ao)
etaSquared(Difficulty2Ao)
TukeyHSD(Difficulty2Ao)
#Anova shows significance F(4, 5815) = 15.24, P<0.05, eta-squared = 0.02

library(Rcmdr)
load(Rcmdr)

df$CourseSatisfaction <- df$RC1
df$InstructorSatisfaction <- df$RC2
df$RC1 <- NULL
df$RC2 <- NULL

write.csv(df, file = "exploratory.csv")
