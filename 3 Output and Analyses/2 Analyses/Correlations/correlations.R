####Set up####
##libraries
library(readxl)
library(reshape)
library(psych)

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

##run the correlation
cor(dat2[ , c(2:4)])
