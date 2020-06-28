####Set up####
##load in data
JOL = read.csv("Scored Output/JOL2.csv")
RL = read.csv("Scored Output/Study.RL.csv")
VC = read.csv("Scored Output/VC.csv")

##load libraries
library(reshape)
library(ez)

####Data Screening####
summary(JOL)
summary(RL)
summary(VC)

##Fix column names
colnames(JOL)[7] = "Direction"
colnames(RL)[7] = "Direction"
colnames(VC)[7] = "Direction"

##remove out of range JOL scores
JOL$Response.JOL[JOL$Response.JOL > 100] = NA

####Check for outliers####
##get participant means
JOL2 = cast(JOL[ , c(1, 7, 13)], Username ~ Direction, mean, na.rm = T)
VC2 = cast(VC[ , c(1, 7, 13)], Username ~ Direction, mean, na.rm = T)
RL2 = cast(RL[ , c(1, 7, 12)], Username ~ Direction, mean, na.rm = T)

##Get z-scores
JOL2$B_z = scale(JOL2$B) 
JOL2$F_z = scale(JOL2$F)
JOL2$S_z = scale(JOL2$S)
JOL2$U_z = scale(JOL2$U)

# w10061361_cbj w10000169_hf w10057323_amw w10003113_hkb w10026703_cem	

RL2$B_z = scale(RL2$B)
RL2$F_z = scale(RL2$F)
RL2$S_z = scale(RL2$S)
RL2$U_z = scale(RL2$U)

VC2$B_z = scale(VC2$B)
VC2$F_z = scale(VC2$F)
VC2$S_z = scale(VC2$S)
VC2$U_z = scale(VC2$U)

#	w10052270 

##Remove outliers
JOL = subset(JOL,
             JOL$Username != "w10061361_cbj")
JOL = subset(JOL,
             JOL$Username != "w10000169_hf")
JOL = subset(JOL,
             JOL$Username != "w10057323_amw")
JOL = subset(JOL,
             JOL$Username != "w10003113_hkb") #only answered 0 or 100

VC = subset(VC,
       VC$Username != "w10052270")

####Get descriptives####
tapply(JOL$Recall_Score, JOL$Direction, mean, na.rm = T)
tapply(RL$Recall_Score, RL$Direction, mean, na.rm = T)
tapply(VC$Recall_Score, VC$Direction, mean, na.rm = T)
