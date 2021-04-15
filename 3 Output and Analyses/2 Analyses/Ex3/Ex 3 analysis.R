####Set up####
##load in data
JOL = read.csv("JOL.csv")
Study = read.csv("Study.csv")
FREQ = read.csv("FREQ.csv")

##load libraries
library(reshape)
library(ez)
library(psychReport)

####Data Screening####
summary(JOL)
summary(Study)
summary(FREQ)

##fix column names
colnames(FREQ)[9] = "Response.FREQ"

colnames(JOL)[7] = "Direction"
colnames(FREQ)[7] = "Direction"
colnames(Study)[7] = "Direction"

##fix out of range scores
FREQ$Response.FREQ = as.numeric(as.character(FREQ$Response.FREQ))

FREQ$Response.FREQ[FREQ$Response.FREQ > 100] = NA
JOL$Response.JOL[JOL$Response.JOL > 100] = NA

##missing data
JOL = na.omit(JOL)
FREQ = na.omit(FREQ)

##check for outliers
##Start with JOL data
sub1 = JOL[ , c(1, 6, 7, 9)] #JOLs
sub2 = JOL[ , c(1, 6, 7, 13)] #Recall

sub1 = cast(sub1, Username~Direction, mean)
sub2 = cast(sub2, Username~Direction, mean)

#Check for JOL rating outliers
sub1$z_B = scale(sub1$B)
sub1$z_F = scale(sub1$F)
sub1$z_S = scale(sub1$S)
sub1$z_U = scale(sub1$U)

#Check for recall outliers #One outlier: W10060628
sub2$z_B = scale(sub2$B)
sub2$z_F = scale(sub2$F)
sub2$z_S = scale(sub2$S)
sub2$z_U = scale(sub2$U)

#Remove the outlier
#JOL = subset(JOL,
           #  JOL$Username != "W10060628")

##Now check the frequency data for outliers
##Start with JOL data
sub3 = FREQ[ , c(1, 6, 7, 9)] #FREQ
sub4 = FREQ[ , c(1, 6, 7, 13)] #Recall

sub3 = cast(sub3, Username~Direction, mean)
sub4 = cast(sub4, Username~Direction, mean)

#Check for FREQ rating outliers #one outlier: w996448_apb
sub3$z_B = scale(sub3$B)
sub3$z_F = scale(sub3$F)
sub3$z_S = scale(sub3$S)
sub3$z_U = scale(sub3$U)

#Check recall #one outler: w726337
sub4$z_B = scale(sub4$B)
sub4$z_F = scale(sub4$F)
sub4$z_S = scale(sub4$S)
sub4$z_U = scale(sub4$U)

#remove outliers
#FREQ = subset(FREQ,
   #           FREQ$Username != "w996448_apb")
FREQ = subset(FREQ,
              FREQ$Username != "w726337")

#Now do Study only
sub5 = Study[ , c(1, 6, 7, 12)] #Recall

sub5 = cast(sub5, Username~Direction, mean)

#Check recall #two outlers: 10008908 and w10057719_awt
sub5$z_B = scale(sub5$B)
sub5$z_F = scale(sub5$F)
sub5$z_S = scale(sub5$S)
sub5$z_U = scale(sub5$U)

#remove outliers
Study = subset(Study,
               Study$Username != "10008908")
Study = subset(Study,
               Study$Username != "w10057719_awt")

##get final subject numbers
length(unique(Study$Username)) #39
length(unique(JOL$Username)) #40
length(unique(FREQ$Username)) #39

####Descriptive Statistics####
#Recall Scores
tap1 = tapply(JOL$Recall_Score, JOL$Direction, mean)
tap2 = tapply(FREQ$Recall_Score, FREQ$Direction, mean)
tap3 = tapply(Study$Recall_Score, Study$Direction, mean)

#Judgment Scores
tap4 = tapply(JOL$Response.JOL, JOL$Direction, mean)
tap5 = tapply(FREQ$Response.FREQ, FREQ$Direction, mean)

#compare recall rates (JOLs, FREQ, Study)
tap1;tap2;tap3

#Illusion of competence (JOLs, Recall)
tap4;tap1

####ANOVA Time ####
##Testing differences in recall first
#get the data into the right format
sub6 = JOL[ , c(1, 6, 7, 13)]
sub7 = FREQ[ , c(1, 6, 7, 13)]
sub8 = Study[ , c(1, 6, 7, 12)]

#Add task coding
sub6$Task = rep(0)
sub7$Task = rep(1)
sub8$Task = rep(2)

#Put the datasets together
anova.data1 = rbind(sub6, sub7, sub8)

anova.data1$Task = factor(anova.data1$Task,
                          levels = c(0, 1, 2),
                          labels = c("JOL", "FREQ", "Study"))

####Encoding Task ANOVA####
model1 = ezANOVA(data = anova.data1,
                 dv = Recall_Score,
                 wid = Username,
                 between = Task,
                 within = .(Direction),
                 return_aov = T,
                 detailed = T,
                 type = 3)

model1 #Everything is significant

anovaLength = length(model1$ANOVA)
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

#look at the means
#tap1;tap2;tap3

##Set up for posthocs
#main effect of direction
Direction.Recall = cast(anova.data1[ , -4], Username ~ Direction, mean)

apply(Direction.Recall, 2, mean)
apply(Direction.Recall, 2, sd)

#main effect of task
anova.data2 = anova.data1[ , -2]
anova.data2 = anova.data2[ , c(1,3,2)]

Task.Recall = cast(anova.data2, Username ~ Task, mean)

apply(Task.Recall, 2, mean, na.rm = T)
apply(Task.Recall, 2, sd, na.rm = T)


Task.Recall.JOL = na.omit(Task.Recall[ , c(1, 2)])
Task.Recall.FREQ = na.omit(Task.Recall[ , c(1, 3)])
Task.Recall.Study = na.omit(Task.Recall[ , c(1, 4)])

#For interaction
JOL.Recall = cast(sub6[ , -4], Username ~ Direction, mean)
FREQ.Recall = cast(sub7[ , -4], Username ~ Direction, mean)
Study.Recall = cast(sub8[ , -4], Username ~ Direction, mean)

##Get means for main effects
#main effect of Direction
tapply(anova.data1$Recall_Score, anova.data1$Direction, mean)

#F vs B
temp = t.test(Direction.Recall$F, Direction.Recall$B, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#F vs S
temp = t.test(Direction.Recall$F, Direction.Recall$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#F vs U
temp = t.test(Direction.Recall$F, Direction.Recall$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#B vs S
temp = t.test(Direction.Recall$B, Direction.Recall$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#B vs U
temp = t.test(Direction.Recall$B, Direction.Recall$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#S vs U
temp = t.test(Direction.Recall$S, Direction.Recall$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#When collapsed across task type, all direction comparisons are significant.

#main effect of task
tapply(anova.data1$Recall_Score, anova.data1$Task, mean)

#JOL vs FREQ
temp = t.test(Task.Recall.JOL$JOL, Task.Recall.FREQ$FREQ, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

##Get pbic for this
Task.Recall.FREQ$task = rep("freq")
Task.Recall.JOL$task = rep("jol")

Task2 = Task.Recall.FREQ
Task3 = Task.Recall.JOL

colnames(Task2)[2] = "Score"
colnames(Task3)[2] = "Score"

task4 = rbind(Task2, Task3)
task4 = na.omit(task4)

model.thing = ezANOVA(task4,
        dv = Score,
        wid = Username,
        between = task,
        detailed = T,
        type = 3)

model.thing

#JOL vs Study
temp = t.test(Task.Recall.JOL$JOL, Task.Recall.Study$Study, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#FREQ vs Study
temp = t.test(Task.Recall.FREQ$FREQ, Task.Recall.Study$Study, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#Correct recall was significantly higher when participants were instructed to make judgments at study

#interaction
tapply(anova.data1$Recall_Score, list(anova.data1$Task, anova.data1$Direction), mean)

####Get means, sds, and CIs for Figures and Tables####
##JOLs
mean(JOL.Recall$B)
mean(JOL.Recall$F)
mean(JOL.Recall$S)
mean(JOL.Recall$U)

sd.b = sd(JOL.Recall$B)
sd.f = sd(JOL.Recall$F)
sd.s = sd(JOL.Recall$S)
sd.u = sd(JOL.Recall$U)

#Get CIs
CI.b = sd.b / sqrt(length(JOL.Recall$Username)) * 1.96
CI.f = sd.f / sqrt(length(JOL.Recall$Username)) * 1.96
CI.s = sd.s / sqrt(length(JOL.Recall$Username)) * 1.96
CI.u = sd.u / sqrt(length(JOL.Recall$Username)) * 1.96

#Upper
mean(JOL.Recall$B) + CI.b
mean(JOL.Recall$F) + CI.f
mean(JOL.Recall$S) + CI.s
mean(JOL.Recall$U) + CI.u

#Lower
mean(JOL.Recall$B) - CI.b
mean(JOL.Recall$F) - CI.f
mean(JOL.Recall$S) - CI.s
mean(JOL.Recall$U) - CI.u

#determine significance for table 4
temp = t.test(JOL.Recall$F, JOL.Recall$S, paired = F, p.adjust.methods = "bonferroni")
temp

##Now do Frequency
mean(FREQ.Recall$B)
mean(FREQ.Recall$F)
mean(FREQ.Recall$S)
mean(FREQ.Recall$U)

sd.b = sd(FREQ.Recall$B)
sd.f = sd(FREQ.Recall$F)
sd.s = sd(FREQ.Recall$S)
sd.u = sd(FREQ.Recall$U)

#Get CIs
CI.b = sd.b / sqrt(length(FREQ.Recall$Username)) * 1.96
CI.f = sd.f / sqrt(length(FREQ.Recall$Username)) * 1.96
CI.s = sd.s / sqrt(length(FREQ.Recall$Username)) * 1.96
CI.u = sd.u / sqrt(length(FREQ.Recall$Username)) * 1.96

#determine significance for table 4
temp = t.test(JOL.FREQ$F, JOL.FREQ$B, paired = F, p.adjust.methods = "bonferroni")
temp

#Upper
mean(FREQ.Recall$B) + CI.b
mean(FREQ.Recall$F) + CI.f
mean(FREQ.Recall$S) + CI.s
mean(FREQ.Recall$U) + CI.u

#Lower
mean(FREQ.Recall$B) - CI.b
mean(FREQ.Recall$F) - CI.f
mean(FREQ.Recall$S) - CI.s
mean(FREQ.Recall$U) - CI.u

##Now study condition
mean(Study.Recall$B)
mean(Study.Recall$F)
mean(Study.Recall$S)
mean(Study.Recall$U)

sd.b = sd(Study.Recall$B)
sd.f = sd(Study.Recall$F)
sd.s = sd(Study.Recall$S)
sd.u = sd(Study.Recall$U)

#Get CIs
CI.b = sd.b / sqrt(length(Study.Recall$Username)) * 1.96
CI.f = sd.f / sqrt(length(Study.Recall$Username)) * 1.96
CI.s = sd.s / sqrt(length(Study.Recall$Username)) * 1.96
CI.u = sd.u / sqrt(length(Study.Recall$Username)) * 1.96

#Upper
mean(Study.Recall$B) + CI.b
mean(Study.Recall$F) + CI.f
mean(Study.Recall$S) + CI.s
mean(Study.Recall$U) + CI.u

#Lower
mean(Study.Recall$B) - CI.b
mean(Study.Recall$F) - CI.f
mean(Study.Recall$S) - CI.s
mean(Study.Recall$U) - CI.u

#determine significance for table 4
temp = t.test(JOL.Study$F, JOL.Study$B, paired = F, p.adjust.methods = "bonferroni")
temp

##Forward pairs
#JOLs vs FREQ
temp = t.test(JOL.Recall$F, FREQ.Recall$F, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

##Need pbic
thing1 = FREQ.Recall[ , c(1, 5)]
thing2 = JOL.Recall[ , c(1, 5)]
thing0 = Study.Recall[ , c(1, 5)]

thing1$task = rep("FREQ")
thing2$task = rep("jol")
thing0$task = rep("study")

thing3 = rbind(thing1, thing0)

thingthing = ezANOVA(thing3,
        dv = U,
        wid = Username,
        between = task,
        type = 3,
        detailed = T)
thingthing

#JOLs vs Study
temp = t.test(JOL.Recall$F, Study.Recall$F, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#FREQ vs Study
temp = t.test(FREQ.Recall$F, Study.Recall$F, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

##Backward Pairs
#JOLs vs FREQ
temp = t.test(JOL.Recall$B, FREQ.Recall$B, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

#JOLs vs Study
temp = t.test(JOL.Recall$B, Study.Recall$B, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#FREQ vs Study
temp = t.test(FREQ.Recall$B, Study.Recall$B, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

##Symmetrical Pairs
#JOLs vs FREQ
temp = t.test(JOL.Recall$S, FREQ.Recall$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

#JOLs vs Study
temp = t.test(JOL.Recall$S, Study.Recall$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#FREQ vs Study
temp = t.test(FREQ.Recall$S, Study.Recall$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

##Unrelated pairs
#JOLs vs FREQ
temp = t.test(JOL.Recall$U, FREQ.Recall$U, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

#JOLs vs Study
temp = t.test(JOL.Recall$U, Study.Recall$U, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

#FREQ vs Study
temp = t.test(FREQ.Recall$U, Study.Recall$U, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

#When pairs are related: Making judgments enhances recall: No diff between JOLs and FREQ, but both result in better recall relative to study
#When pairs are unrelated: No differences between instruction types

####Illusion of competence ANOVA####
##Get the data in the correct format
IOC = JOL[ , c(1, 7, 9, 13)]

colnames(IOC)[3] = "JOL"
colnames(IOC)[4] = "Recall"

IOC2 = melt(IOC, measure.vars = c("JOL", "Recall"))

colnames(IOC2)[3] = "Task"
colnames(IOC2)[4] = "Score"

##Run the ANOVA
ezANOVA(data = IOC2,
        wid = Username,
        between = .(Task, Direction),
        dv = Score,
        type = 3)

#IOC appears to replicate -- main effect of task, main effect of direction, and significant interaction

##Take a look at the means
tap4; tap1 #Only difference is that JOLs are calibrated for unrelated pairs

##Set up for posthocs
#main effect of Direction
IOC.Direction = cast(IOC2[ , -3], Username ~ Direction, mean)

#Main effect of Task
IOC.Task = cast(IOC2[ , -2], Username ~ Task, mean)

#Interaction
sub9 = subset(IOC2,
              IOC2$Task == "JOL")
sub10 = subset(IOC2,
               IOC2$Task == "Recall")

JOLs = cast(sub9[ , -3], Username ~ Direction, mean)
Recall = cast(sub10[ , -3], Username ~ Direction, mean)

##Now the posthocs
#Main effect of Direction
##F vs B
temp = t.test(IOC.Direction$F, IOC.Direction$B, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#F vs S
temp = t.test(IOC.Direction$F, IOC.Direction$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#F vs U
temp = t.test(IOC.Direction$F, IOC.Direction$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#B vs S
temp = t.test(IOC.Direction$B, IOC.Direction$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#B vs U
temp = t.test(IOC.Direction$B, IOC.Direction$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#S vs U
temp = t.test(IOC.Direction$S, IOC.Direction$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#All comparisons are significant

##Main effect of Task
#JOL vs Recall
temp = t.test(IOC.Task$JOL, IOC.Task$Recall, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#JOL ratings are significantly higher than recall rates

#Pbic for JOL and frequency


##Interaction
#Forward Pairs
temp = t.test(JOLs$F, Recall$F, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant! We end up with underconfidence....

#Backward Pairs
temp = t.test(JOLs$B, Recall$B, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Significant!

#Symmetrical Pairs
temp = t.test(JOLs$S, Recall$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

#Unrelated Pairs
temp = t.test(JOLs$U, Recall$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

##Overconfidence for backward, underconfidence for forward, no differences for symmetrical or unrelated.

####Write data to .csv for making graphs####
##Reactivity data
#write.csv(anova.data1, file = "Diss Ex 1 Graphs/Reactivity.csv", row.names = F)

##IOC
#write.csv(IOC2, file = "Diss Ex 1 Graphs/IOC.csv", row.names = F)





