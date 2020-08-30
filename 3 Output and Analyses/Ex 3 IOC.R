##Experiment 3 IOC
JOL = read.csv("Scored Output/JOL.csv")

##load libraries
library(reshape)
library(ez)

colnames(JOL)[7] = "Direction"

#Fix out of range scores
JOL$Response.JOL[JOL$Response.JOL > 100] = NA

##missing data
JOL = na.omit(JOL)

sub6 = JOL[ , c(1, 7, 9, 13)]

long.dat = melt(sub6, measure.vars = c("Response.JOL", "Recall_Score"))

colnames(long.dat)[3:4] = c("Task", "Score")

model1 = ezANOVA(long.dat,
                 dv = Score,
                 wid = Username,
                 within = .(Direction, Task),
                 type = 3,
                 detailed = T)
model1

model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

#Main effect of Direction
tapply(long.dat$Score, long.dat$Direction, mean)

tapply(long.dat$Score, long.dat$Task, mean)

##Post hocs here
IOC2 = cast(long.dat, Username ~ Direction, mean)

temp = t.test(IOC2$F, IOC2$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

mean(IOC2$F)
mean(IOC2$S)

sd(IOC2$F)
sd(IOC2$S)

##Interaction
tapply(long.dat$Score, list(long.dat$Task, long.dat$Direction), mean)

##Set up for post-hoc
#Get mean JOL by direction (interaction)
IOC_data2 = subset(long.dat, long.dat$Task == "Response.JOL")

JOL.ph = cast(IOC_data2, Username ~ Direction, mean)

IOC_data3 = subset(long.dat, long.dat$Task == "Recall_Score")
Recall.ph = cast(IOC_data3, Username ~ Direction, mean)

##Interaction
temp = t.test(JOL.ph$F, JOL.ph$B, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

mean(JOL.ph$F)
mean(Recall.ph$F)

sd(JOL.ph$S)
sd(Recall.ph$S)

##Get pbics for interaction
JOL.ph$task = rep("JOL")
Recall.ph$task = rep("Study")

pbic = rbind(JOL.ph, Recall.ph)

colnames(pbic)[3] = "f"

modelb = ezANOVA(pbic,
                 dv = U,
                 wid = Username,
                 within = task,
                 type = 3,
                 detailed = T)
modelb

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
