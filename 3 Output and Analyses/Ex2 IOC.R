####Ex 2 Illusion of competence####
JOL = read.csv("Scored Output/JOL2.csv")

##load libraries
library(reshape)
library(ez)
library(psychReport)

colnames(JOL)[7] = "Direction"

JOL$Response.JOL[JOL$Response.JOL > 100] = NA

dat = JOL[ , c(1, 7, 9, 13)]

dat = na.omit(dat)

long.dat = melt(dat, measure.vars = c("Response.JOL", "Recall_Score"))

colnames(long.dat)[3:4] = c("Task", "Score")

##Descriptives
tapply(long.dat$Score, long.dat$Task, mean)

tapply(long.dat$Score, long.dat$Direction, mean)

tapply(long.dat$Score, list(long.dat$Task, long.dat$Direction), mean)

##Anova
model1 = ezANOVA(long.dat,
                 dv = Score,
                 within = .(Task, Direction),
                 wid = Username,
                 return_aov = T,
                 type = 3,
                 detailed = T)
model1

#Get MSE here
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

##effect size
aovEffectSize(model1, effectSize = "pes")

##Post hocs here
IOC2 = cast(long.dat, Username ~ Direction, mean) #Main effect of Direction

#Get mean JOL by direction (interaction)
IOC_data2 = subset(long.dat, long.dat$Task == "Response.JOL")

JOL.ph = cast(IOC_data2, Username ~ Direction, mean)

IOC_data3 = subset(long.dat, long.dat$Task == "Recall_Score")
Recall.ph = cast(IOC_data3, Username ~ Direction, mean)

##Start with main effect of direction
temp = t.test(IOC2$F, IOC2$B, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

mean(IOC2$F)
mean(IOC2$S)

sd(IOC2$F)
sd(IOC2$S)

##Now do interaction
##Interaction
#F vs F
temp = t.test(JOL.ph$F, Recall.ph$F, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#B vs B
temp = t.test(JOL.ph$B, Recall.ph$B, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

mean(JOL.ph$B)
mean(Recall.ph$B)

sd(JOL.ph$B)
sd(Recall.ph$B)

#S vs S
temp = t.test(JOL.ph$S, Recall.ph$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

mean(JOL.ph$S)
mean(Recall.ph$S)

sd(JOL.ph$S)
sd(Recall.ph$S)

#U vs U
temp = t.test(JOL.ph$U, Recall.ph$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

mean(JOL.ph$U)
mean(Recall.ph$U)

sd(JOL.ph$U)
sd(Recall.ph$U)

####Get values for table####
##JOLs
##Start with JOLs
#Means
mean(JOL.ph$B)
mean(JOL.ph$F)
mean(JOL.ph$S)
mean(JOL.ph$U)

sd(JOL.ph$B)
sd(JOL.ph$F)
sd(JOL.ph$S)
sd(JOL.ph$U)

x = apply(JOL.ph[ , -1], 2, sd)
x = x / sqrt(length(unique(JOL.ph$Username)))
x = x * 1.96
x

##Now do recall
mean(Recall.ph$B)
mean(Recall.ph$F)
mean(Recall.ph$S)
mean(Recall.ph$U)

sd(Recall.ph$B)
sd(Recall.ph$F)
sd(Recall.ph$S)
sd(Recall.ph$U)

z = apply(Recall.ph[ , -1], 2, sd)
z = z / sqrt(length(unique(Recall.ph$Username)))
z = z * 1.96
z

##Pbic
JOL.ph$task = rep("JOL")
Recall.ph$task = rep("Study")

pbic = rbind(JOL.ph, Recall.ph)

colnames(pbic)[3] = "f"

modelb = ezANOVA(pbic,
                 dv = f,
                 wid = Username,
                 within = task,
                 type = 3,
                 detailed = T)
modelb
