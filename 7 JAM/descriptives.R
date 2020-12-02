####Setup####
options(scipen = 999)

##Load in data
JAM = read.csv("JAM.csv")
JOL = read.csv("JOL.csv")
Read = read.csv("Read.csv")

##check data
summary(JAM)

JAM$Response.JOL[JAM$Response.JOL > 100] = NA

summary(JOL)

JOL$Response.JOL[JOL$Response.JOL > 100] = NA

##GET Recall as percent
JOL$Scored = JOL$Scored * 100
JAM$Scored = JAM$Scored * 100
Read$Scored = Read$Scored * 100

#Get ns
length(unique(JAM$Sub.ID)) #33
length(unique(JOL$Sub.ID)) #33
length(unique(Read$Sub.ID)) #35

tapply(JAM$Scored, JAM$Stimuli.Stimuli.Notes, mean, na.rm = T)
tapply(JOL$Scored, JOL$Stimuli.Stimuli.Notes, mean, na.rm = T)
tapply(Read$Scored, Read$Stimuli.Stimuli.Notes, mean, na.rm = T)

####Data screening####
##Okay, we got any weirdo's in the read unrelated? Seems a bit high
library(reshape)

#Fix some column names
colnames(Read)[8] = "Direction"
colnames(JOL)[8] = "Direction"
colnames(JAM)[8] = "Direction"

long.read = cast(Read, Sub.ID ~ Direction, mean, na.rm = T)

#Okay, one dude remembered 97% of all unrelated pairs and another was at floor... Cut them out!

Read = subset(Read,
              Read$Sub.ID != "5ebecb3f7899980b68a9898a")
Read = subset(Read,
              Read$Sub.ID != "w10089076_AG")
Read = subset(Read,
              Read$Sub.ID != "5ebc9542e7dea50e8451e07b")

#Okay, now check the JAM
long.JAM = cast(JAM, Sub.ID ~ Direction, mean, na.rm = T) #JAM data looks fine

##Now for JOL
long.JOL = cast(JOL, Sub.ID ~ Direction, mean, na.rm = T) #JOL data seems fine

##remake long read

long.read = cast(Read, Sub.ID ~ Direction, mean, na.rm = T)

##Okay,Get ns again
length(unique(JAM$Sub.ID)) #33
length(unique(JOL$Sub.ID)) #33
length(unique(Read$Sub.ID)) #32

#now get descriptives again
tapply(JAM$Scored, JAM$Direction, mean, na.rm = T)
tapply(JOL$Scored, JOL$Direction, mean, na.rm = T)
tapply(Read$Scored, Read$Direction, mean, na.rm = T)

sds = apply(long.read[ , -1], 2, sd)

se = sds / sqrt(32)
se * 1.96

##Get CIs again
apply(long.JAM[ , -1], 2, mean)
(apply(long.JAM[ , -1], 2, sd) / sqrt(length(unique(long.JAM$Sub.ID)))) * 1.96

apply(long.JOL[ , -1], 2, mean)
(apply(long.JOL[ , -1], 2, sd) / sqrt(length(unique(long.JOL$Sub.ID)))) * 1.96

apply(long.read[ , -1], 2, mean)
(apply(long.read[ , -1], 2, sd) / sqrt(length(unique(long.read$Sub.ID)))) * 1.96


####ANOVA####
library(ez)

##Get the data in the right format
JAM$Encoding = rep("JAM")
JOL$Encoding = rep("JOL")
Read$Encoding = rep("Read")

##Combine the data and get just the required columns
combined = rbind(JAM[ , c(2, 8, 15, 16)], JOL[ , c(2, 8, 15, 16)], Read[ , c(2, 8, 14, 15)])

##Do the ANOVA
model1 = ezANOVA(combined,
                 dv = Scored,
                 wid = Sub.ID,
                 within = Direction,
                 between = Encoding,
                 type = 3,
                 detailed = T)
model1

#Get MSE here
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

##Post-hocs
t.test(long.read$B, long.JOL$B, paired = F, p.adjust.methods = "none")

##Write subject level to csv
#write.csv(long.read, file = "READ2.csv", row.names = F)
#write.csv(long.JAM, file = "JAM2.csv", row.names = F)
#write.csv(long.JOL, file = "JOL2.csv", row.names = F)
