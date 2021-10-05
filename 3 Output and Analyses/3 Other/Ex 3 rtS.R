####Set up####
##load in data
JOL = read.csv("JOL.csv")
Study = read.csv("Study.csv")
FREQ = read.csv("FREQ.csv")

##load libraries
library(reshape)
library(ez)

####Data Screening####
summary(JOL)
summary(Study)
summary(FREQ)

##fix column names
colnames(FREQ)[9] = "Response.FREQ"

colnames(JOL)[7] = "Direction"
colnames(FREQ)[7] = "Direction"
colnames(Study)[7] = "Direction"

FREQ = subset(FREQ,
              FREQ$Username != "w726337")

#remove outliers
Study = subset(Study,
               Study$Username != "10008908")
Study = subset(Study,
               Study$Username != "w10057719_awt")

##get final subject numbers
length(unique(Study$Username)) #39
length(unique(JOL$Username)) #40
length(unique(FREQ$Username)) #39

#get the data into the right format
sub6 = JOL[ , c(1, 7, 8)]
sub7 = FREQ[ , c(1, 7, 8)]
sub8 = Study[ , c(1, 7, 8)]

colnames(sub6)[3] = "RT"
colnames(sub7)[3] = "RT"
colnames(sub8)[3] = "RT"

#Add task coding
sub6$Task = rep("JOL")
sub7$Task = rep("freq")
sub8$Task = rep("STUDY")

anova.data1 = rbind(sub6, sub7, sub8)

tapply(anova.data1$RT, anova.data1$Task, mean)
tapply(anova.data1$RT, anova.data1$Direction, mean)
tapply(anova.data1$RT, list(anova.data1$Task, anova.data1$Direction), mean)

stuff = tapply(anova.data1$RT, list(anova.data1$Task, anova.data2$Direction), sd)

stuff / sqrt(length(unique(FREQ$Username)))


#Run the anova
model1 = ezANOVA(data = anova.data1,
                 dv = RT,
                 wid = Username,
                 between = Task,
                 within = Direction,
                 detailed = T,
                 type = 3)

model1

model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

#Post hoc for interaction
anova.data2 = anova.data1[ , c(1, 2, 4, 3)]

ph = cast(anova.data2[ , -2], Username ~ Task, mean)

freq2 = na.omit(ph$freq)
mean(freq2)
sd(freq2)

jol2 = na.omit(ph$JOL)
mean(jol2)
sd(jol2)

study2 = na.omit(ph$STUDY)
mean(study2)
sd(study2)

##t-tests
temp = t.test(freq2, study2, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##Get pbic for interaction
#set up data
ph1 = ph[ , c(1, 2)]
ph1$task = rep("freq")

ph2 = ph[ , c(1, 3)]
ph2$task = rep("jol")

colnames(ph1)[2] = "score"
colnames(ph2)[2] = "score"

ph3 = rbind(ph1, ph2)

ph3 = na.omit(ph3)

##run anova
model2 = ezANOVA(data = ph3,
                 dv = score,
                 wid = Username,
                 between = task,
                 detailed = T,
                 type = 3)

model2
