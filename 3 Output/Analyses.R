####Set up####
#load in data
JOL = read.csv("Scored Output/JOL.csv")
Study = read.csv("Scored Output/Study.csv")
FREQ = read.csv("Scored Output/FREQ.csv")
VC = read.csv("Scored Output/VC.csv")
Study.RL = read.csv("Scored Output/Study.RL.csv")
JOL2 = read.csv("Scored Output/JOL2.csv")

#set up environment
options(scipen = 999)

library(reshape)
library(ez)

####Get descriptives####
summary(JOL)
summary(FREQ) #at least one out of range Frequency judgment
summary(Study)
summary(VC)
summary(Study.RL)
summary(JOL2)

#Fix some of the column names
colnames(FREQ)[9] = "Response.FREQ"
colnames(JOL)[7] = "Direction"
colnames(FREQ)[7] = "Direction"
colnames(Study)[7] = "Direction"
colnames(VC)[7] = "Direction"
colnames(Study.RL)[7] = "Direction"
colnames(JOL2)[7] = "Direction"

FREQ$Response.FREQ[FREQ$Response.FREQ > 100] = NA
JOL$Response.JOL[JOL$Response.JOL > 100] = NA
JOL2$Response.JOL[JOL2$Response.JOL > 100] = NA

####Get Recall and judgment means split by direction####
##Start with the JOL data
tapply(JOL$Response.JOL, JOL$Direction, mean, na.rm = T)
tap1 = tapply(JOL$Recall_Score, JOL$Direction, mean, na.rm = T)

#look at it by subject
tapply(JOL$Recall_Score, list(JOL$Username, JOL$Direction), mean, na.rm = T)

##Lets do the frequency judgments next
tapply(FREQ$Response.FREQ, FREQ$Direction, mean, na.rm = T)
tap2 = tapply(FREQ$Recall_Score, FREQ$Direction, mean, na.rm = T) #Recall is trending in the right direction

#look at it by subject
tapply(FREQ$Recall_Score, list(FREQ$Username, FREQ$Direction), mean, na.rm = T)

##Lets look at recall for the study condition
tap3 = tapply(Study$Recall_Score, Study$Direction, mean, na.rm = T)

#look at it by subject
tapply(Study$Recall_Score, list(Study$Username, Study$Direction), mean, na.rm = T)

#Look at the vowel counting task
tap4 = tapply(VC$Recall_Score, VC$Direction, mean, na.rm = T)

#Look at relational study task
tap5 = tapply(Study.RL$Recall_Score, Study.RL$Direction, mean, na.rm = T)

#Second JOL comparison
tap6 = tapply(JOL2$Recall_Score, JOL2$Direction, mean, na.rm = T)

#Compare recall across each task
tap1;tap2;tap3
tap6;tap5;tap4

#Get number of participants in each condition
length(unique(JOL$Username))
length(unique(FREQ$Username))
length(unique(Study$Username))
length(unique(VC$Username))
length(unique(Study.RL$Username))
length(unique(JOL2$Username))
