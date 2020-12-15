####Footnote analyses####
##Comparing No-JOL Conditions in ex2 and 3
Study = read.csv("Scored Output/Study.csv") ##ex 3
Study2 = read.csv("Scored Output/Study2.csv") ##ex 2

RL = read.csv("Scored Output/Study.RL.csv") ##Relational encoding
VC = read.csv("Scored Output/VC.csv") ##vowel counting

library(reshape)

####NO-JOL####
length(unique(Study$Username))
length(unique(Study2$Username))

#remove outliers
Study = subset(Study,
               Study$Username != "10008908")
Study = subset(Study,
               Study$Username != "w10057719_awt")

##arrange the data
Study$task = rep("Study1")
Study2$task = rep("Study2")

Study = Study[ , c(1, 12, 13)]
Study = Study[ , c(1, 3, 2)]

Study2 = Study2[ , c(1, 12, 13)]
Study2 = Study2[ , c(1, 3, 2)]

s1 = cast(Study, Username ~ task, mean)
s2 = cast(Study2, Username ~ task, mean)

mean(s1$Study1)
mean(s2$Study2)

##Run the t-test
temp = t.test(s1$Study1, s2$Study2, paired = F)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 

##get pbic
s1$source = rep("sona")
s2$source = rep("prolific")

colnames(s1)[2] = "study"
colnames(s2)[2] = "study"

pbic = rbind(s1, s2)

model = ezANOVA(pbic,
                dv = study,
                wid = Username,
                between = source,
                type = 3,
                detailed = T)
model


####Relational Counting####
length(unique(RL$Username))

##Arrange the data
RL$task = rep("RL1")

RL = RL[ , c(1, 12, 13)]
RL = RL[ , c(1, 3, 2)]

r1 = cast(RL, Username ~ task, mean)

##Figure out who took the study on Prolific -- Prolific IDs start with 5
r1$Username2 = substr(r1$Username, 1, 1)

table(r1$Username2)

r2 = subset(r1,
            r1$Username2 == "5")
r3 = subset(r1,
            r1$Username2 != "5")

r2$source = rep("Prolific")
r3$source = rep("Sona")

mean(r2$RL1)
mean(r3$RL1)

##Run the t-test
temp = t.test(r2$RL1, r3$RL1, paired = F, var.equal = T)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##get pbic
pbic = rbind(r3, r2)

length(unique(pbic$Username))

model = ezANOVA(pbic,
                dv = RL1,
                wid = Username,
                between = source,
                type = 3,
                detailed = T)
model


####Now do Vowel Counting####
length(unique(VC$Username))

##Arrange the data
VC$task = rep("VC1")

VC = VC[ , c(1, 12, 14)]
VC = VC[ , c(1, 3, 2)]

v1 = cast(VC, Username ~ task, mean)

##Figure out who took the study on Prolific -- Prolific IDs start with 5
v1$Username2 = substr(v1$Username, 1, 1)

table(v1$Username2)

v2 = subset(v1,
            v1$Username2 == "5")
v3 = subset(v1,
            v1$Username2 != "5")

v2$source = rep("Prolific")
v3$source = rep("Sona")

mean(v2$VC1)
mean(v3$VC1)

##Run the t-test
temp = t.test(v2$VC1, v3$VC1, paired = F)
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 

##Get pbics
pbic = rbind(v3, v2)

length(unique(pbic$Username))

model = ezANOVA(pbic,
                dv = VC1,
                wid = Username,
                between = source,
                type = 3,
                detailed = T)
model
