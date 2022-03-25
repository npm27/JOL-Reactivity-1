####Set up####
##libraries
library(readxl)
library(reshape)
library(psych)
library(Hmisc)

#READ IN DATA
JOL1 = read_xlsx("Correlations.xlsx", sheet = "JOL1")
JOL2 = read_xlsx("Correlations.xlsx", sheet = "JOL2")
JOL3 = read_xlsx("Correlations.xlsx", sheet = "JOL3")
JOL4 = read_xlsx("Correlations.xlsx", sheet = "JOL4")

JAM = read_xlsx("Correlations.xlsx", sheet = "JAM")
FREQ = read_xlsx("Correlations.xlsx", sheet = "FREQ")

##Combine
dat = rbind(JOL1, JOL2, JOL3, JOL4, JAM, FREQ)

#remove out of range scores
dat$Rating = as.numeric(dat$Rating)

dat$Rating[dat$Rating > 100] = NA

#fix the case
dat$Stimuli.Cue = tolower(dat$Stimuli.Cue)
dat$Stimuli.Answer = tolower(dat$Stimuli.Answer)

##get mean ratings for each item by task
dat2 = cast(dat[ , c(1, 2, 3, 5, 6, 4)], Stimuli.Answer ~ TASK, mean, na.rm = T)
  
cor(dat2)

##can we get p-values?
rcorr(as.matrix(dat2))

####Just compare within experiments
ex2 = subset(dat,
             dat$Experiment == "2")
ex3 = subset(dat,
             dat$Experiment == "3")

ex2 = cast(ex2[ , c(1, 2, 3, 5, 6, 4)], Stimuli.Answer ~ TASK, mean, na.rm = T)
ex3 = cast(ex3[ , c(1, 2, 3, 5, 6, 4)], Stimuli.Answer ~ TASK, mean, na.rm = T)

temp = cbind(ex2, ex3)
temp = temp[ , -c(1, 3, 4, 6)]

cor(ex2)
cor(ex3)
cor(temp)

rcorr(as.matrix(ex2))
rcorr(as.matrix(ex3))
rcorr(as.matrix(temp))

####Split it up by high-low (related, unrelated)####
##JAM
JAM_R = subset(JAM,
               JAM$Stimuli.Stimuli.Notes != "U")
JAM_U = subset(JAM,
               JAM$Stimuli.Stimuli.Notes == "U")

##JOL
JOL1_R = subset(JOL1,
               JOL1$Stimuli.Stimuli.Notes != "U")
JOL1_U = subset(JOL1,
               JOL1$Stimuli.Stimuli.Notes == "U")

JOL2_R = subset(JOL2,
                JOL2$Stimuli.Stimuli.Notes != "U")
JOL2_U = subset(JOL2,
                JOL2$Stimuli.Stimuli.Notes == "U")

JOL3_R = subset(JOL3,
                JOL3$Stimuli.Stimuli.Notes != "U")
JOL3_U = subset(JOL3,
                JOL3$Stimuli.Stimuli.Notes == "U")

JOL4_R = subset(JOL4,
                JOL4$Stimuli.Stimuli.Notes != "U")
JOL4_U = subset(JOL4,
                JOL4$Stimuli.Stimuli.Notes == "U")

##FREQ
FREQ_R = subset(FREQ,
                FREQ$Stimuli.Stimuli.Notes != "U")
FREQ_U = subset(FREQ,
                FREQ$Stimuli.Stimuli.Notes == "U")

####COMBINE####
dat3 = rbind(JOL1_R, JOL2_R, JOL3_R, JOL4_R, JAM_R, FREQ_R)
dat3$Rating = as.numeric(dat3$Rating)
dat3$Rating[dat3$Rating > 100] = NA

#fix the case
dat3$Stimuli.Cue = tolower(dat3$Stimuli.Cue)
dat3$Stimuli.Answer = tolower(dat3$Stimuli.Answer)

##get mean ratings for each item by task
dat4 = cast(dat3[ , c(1, 2, 3, 5, 6, 4)], Stimuli.Answer ~ TASK, mean, na.rm = T)

cor(dat4)

##can we get p-values?
rcorr(as.matrix(dat4))

##Just by experiment
ex2 = subset(dat3,
             dat3$Experiment == "2")
ex3 = subset(dat3,
             dat3$Experiment == "3")

ex2 = cast(ex2[ , c(1, 2, 3, 5, 6, 4)], Stimuli.Answer ~ TASK, mean, na.rm = T)
ex3 = cast(ex3[ , c(1, 2, 3, 5, 6, 4)], Stimuli.Answer ~ TASK, mean, na.rm = T)

temp = cbind(ex2, ex3)
temp = temp[ , -c(1, 3, 4, 6)]

cor(ex2)
cor(ex3)
cor(temp)

rcorr(as.matrix(ex2))
rcorr(as.matrix(ex3))
rcorr(as.matrix(temp))

temp$JOL = ex2$JOL

##write to file for plots
write.csv(temp, file = "related.csv", row.names = F)

##Now do unrelated
dat5 = rbind(JOL1_U, JOL2_U, JOL3_U, JOL4_U, JAM_U, FREQ_U)
dat5$Rating = as.numeric(dat5$Rating)
dat5$Rating[dat5$Rating > 100] = NA

#fix the case
dat5$Stimuli.Cue = tolower(dat5$Stimuli.Cue)
dat5$Stimuli.Answer = tolower(dat5$Stimuli.Answer)

##get mean ratings for each item by task
dat6 = cast(dat5[ , c(1, 2, 3, 5, 6, 4)], Stimuli.Answer ~ TASK, mean, na.rm = T)

cor(dat6)

##can we get p-values?
rcorr(as.matrix(dat6))

##Just by experiment
ex2 = subset(dat5,
             dat5$Experiment == "2")
ex3 = subset(dat5,
             dat5$Experiment == "3")

ex2 = cast(ex2[ , c(1, 2, 3, 5, 6, 4)], Stimuli.Answer ~ TASK, mean, na.rm = T)
ex3 = cast(ex3[ , c(1, 2, 3, 5, 6, 4)], Stimuli.Answer ~ TASK, mean, na.rm = T)

temp = cbind(ex2, ex3)
temp = temp[ , -c(1, 3, 4, 6)]

cor(ex2)
cor(ex3)
cor(temp)

rcorr(as.matrix(ex2))
rcorr(as.matrix(ex3))
rcorr(as.matrix(temp))

##write to file for plots
temp$JOL = ex2$JOL

##write to file for plots
write.csv(temp, file = "unrelated.csv", row.names = F)