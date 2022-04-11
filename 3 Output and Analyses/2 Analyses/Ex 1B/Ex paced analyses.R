####JML EX 1 data####
##set up
#Read in data
JOL = read.csv("JOL_scored.csv")
Study = read.csv("Read_scored.csv")

#Load libraries
library(ez)
library(reshape)
library(psychReport)

options(scipen = 999)

##Let's take a look at the data
summary(JOL)
summary(Study)

colnames(JOL)[19] = "Response.JOL"
colnames(JOL)[6] = "Recall_Score"
colnames(Study)[6] = "Recall_Score"

#colnames(JOL)[1:3] = c("thing", "thing2", "thing3") #Rename multiple columns. Columns must be next to each other in the dataframe!

#get JOLs and Recall on same scale
JOL$Recall_Score = JOL$Recall_Score * 100
Study$Recall_Score = Study$Recall_Score * 100

#Fix out of range JOLs
JOL$Response.JOL = as.numeric(JOL$Response.JOL)

JOL$Response.JOL[JOL$Response.JOL > 100] = NA #> greater, < less, == equals, != not equal

#table(JOL$Response.JOL)

#Fix column names
colnames(JOL)[12] = "Direction"
colnames(Study)[12] = "Direction"

#how many participants are we starting w/?
length(unique(JOL$id)) #52
length(unique(Study$id)) #59

#Drop unused columns w/ indexing
JOL1 = JOL[ , -c(1, 3:5, 7:11, 13:18, 20:21)] #Will use this one for IOC
JOL2 = JOL[ , -c(1, 3:5, 7:11, 13:21)] #Will use this one for reactivity

JOL2$Task = rep("JOL")

Study = Study[ , -c(1, 3:5, 7:11, 13:18)]
Study$Task = rep("Study")

reactivity_data = rbind(Study, JOL2)

##get descriptives
##Jol
tapply(JOL1$Response.JOL, JOL1$Direction, mean, na.rm = T) #DV, IV (grouping variable), function, remove NAs
tapply(JOL2$Recall_Score, JOL2$Direction, mean, na.rm = T)

#Study
tapply(Study$Recall_Score, Study$Direction, mean, na.rm = T)

####Clean the data####
##First check for outliers
jols.ratings = cast(JOL1, id ~ Direction, mean, na.rm = T)
jols.ratings2 = scale(jols.ratings) #a few close ones, but no real outliers here

#JOL group recall by direction
jols = cast(JOL1[ , c(1, 3, 4, 2)], id ~ Direction, mean, na.rm = T)
jols2 = scale(jols) #one in the unrelated #w10122668_asr

#Study group recall by direction
study = cast(Study[ , c(1, 3, 4, 2)], id ~ Direction, mean, na.rm = T)
study2 = scale(study) #got two: w10122762_blm, W10129411_WB

##Remove the outliers
reactivity_data = subset(reactivity_data,
                         reactivity_data$id != "w10122668_asr")
reactivity_data = subset(reactivity_data,
                         reactivity_data$id != "w10122762_blm")
reactivity_data = subset(reactivity_data,
                         reactivity_data$id != "W10129411_WB")

#reactivity_data = subset(reactivity_data,
#                         reactivity_data$id != "W10129411_WB" & reactivity_data$id != "w10122762_blm"
#                         & reactivity_data$id != "w10122668_asr") #Can also combine it like this

#remove from IOC
JOL1 = subset(JOL1,
              JOL1$id != "w10122668_asr")

##remove anyone else w/ abysmal related recall (suggests not paying attention) or really high unrelated (suggests cheating)
reactivity_data = subset(reactivity_data,
                         reactivity_data$id != "w881414_BAC")
reactivity_data = subset(reactivity_data,
                         reactivity_data$id != "w10056248bs")
reactivity_data = subset(reactivity_data,
                         reactivity_data$id != "w10094784")
reactivity_data = subset(reactivity_data,
                         reactivity_data$id != "w10095166_RS")
reactivity_data = subset(reactivity_data,
                         reactivity_data$id != "w10107404_zeb")

#remove from IOC dataset
JOL1 = subset(JOL1,
              JOL1$id != "w881414_BAC")

#remove participants who didn't take JOL task seriously
reactivity_data = subset(reactivity_data,
                         reactivity_data$id != "w10050706")

JOL1 = subset(JOL1,
              JOL1$id != "w10050706")


#drop na's and get means again
JOL1 = na.omit(JOL1) #na omit removes missing data
reactivity_data = na.omit(reactivity_data)

##IOC
tapply(JOL1$Response.JOL, JOL1$Direction, mean)
tapply(JOL1$Recall_Score, JOL1$Direction, mean)

##Reactivity
tapply(reactivity_data$Recall_Score, list(reactivity_data$Task, reactivity_data$Direction), mean)

##set up the IOC data
colnames(JOL1)[4] = "JOL"
colnames(JOL1)[2] = "Recall"

IOC_data = melt(JOL1, measure.vars = c("JOL", "Recall"))

colnames(IOC_data)[3] = "Task"
colnames(IOC_data)[4] = "Score"

length(unique(IOC_data$id)) #49 Participants in the JOL group

####IOC ANOVA####
model1 = ezANOVA(data = IOC_data,
                 dv = Score,
                 wid = id,
                 within = .(Direction, Task),
                 return_aov = T,
                 type = 3,
                 detailed = T)
model1 #IOC replicates

#Get MSE here
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

####Post Hocs####
##Get the means
#Pair Direction
tapply(IOC_data$Score, IOC_data$Direction, mean)

#Task
tapply(IOC_data$Score, IOC_data$Task, mean)

#IOC Interaction
tapply(IOC_data$Score, list(IOC_data$Task, IOC_data$Direction), mean)

##Now do the t-tests
#set up data
#get mean jol/recall by direction (main effect of direction)
IOC2 = cast(IOC_data, id ~ Direction, mean)

#Get mean JOL by direction (interaction)
IOC_data2 = subset(IOC_data, IOC_data$Task == "JOL")

JOL.ph = cast(IOC_data2, id ~ Direction, mean)

IOC_data3 = subset(IOC_data, IOC_data$Task == "Recall")
Recall.ph = cast(IOC_data3, id ~ Direction, mean)

##Main effect of Direction post hoc
temp = t.test(IOC2$F, IOC2$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##Make pbic model
model_dat = IOC2[, c(1,3,4)]

model_dat = melt(model_dat, ids = "id")

colnames(model_dat)[2:3] = c("Direction", "Score")

modela = ezANOVA(data = model_dat,
                 dv = Score,
                 wid = id,
                 between = Direction,
                 type = 3,
                 detailed = T)
modela

##Need to get sds for cohens d
apply(IOC2, 2, sd)
apply(IOC2, 2, mean)

##Main effect of Task type
task.data = cast(IOC_data, id ~ Task, mean)

##Start with JOLs
#Means
mean(JOL.ph$B)
mean(JOL.ph$F)
mean(JOL.ph$S)
mean(JOL.ph$U)

##Get sds and CIs for Table 1
b = sd(JOL.ph$B)
f = sd(JOL.ph$F)
s = sd(JOL.ph$S)
u = sd(JOL.ph$U)

b;f;s;u

#get se
b2 = b / sqrt(length(JOL.ph$Username))
f2 = f / sqrt(length(JOL.ph$Username))
s2 = s / sqrt(length(JOL.ph$Username))
u2 = u / sqrt(length(JOL.ph$Username))

#Get CI
b2 * 1.96; f2 * 1.96; s2 * 1.96; u2 *1.96

#Get Upper
b2 * 1.96 + mean(JOL.ph$B); f2 * 1.96 + mean(JOL.ph$F); s2 * 1.96 + mean(JOL.ph$S); u2 * 1.96 + mean(JOL.ph$U)

#Get Lower
mean(JOL.ph$B) - b2 * 1.96; mean(JOL.ph$F) - f2 * 1.96; mean(JOL.ph$S) - s2 * 1.96; mean(JOL.ph$U) - u2 * 1.96

##Run the t-tests
temp = t.test(JOL.ph$S, JOL.ph$F, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##Do it again for recall
#Means
mean(Recall.ph$B)
mean(Recall.ph$F)
mean(Recall.ph$S)
mean(Recall.ph$U)

##Get sds and CIs for Table 1
b = sd(Recall.ph$B)
f = sd(Recall.ph$F)
s = sd(Recall.ph$S)
u = sd(Recall.ph$U)

b;f;s;u

#get se
b2 = b / sqrt(length(Recall.ph$Username))
f2 = f / sqrt(length(Recall.ph$Username))
s2 = s / sqrt(length(Recall.ph$Username))
u2 = u / sqrt(length(Recall.ph$Username))

#Get CI
b2 * 1.96; f2 * 1.96; s2 * 1.96; u2 *1.96

#Get Upper
b2 * 1.96 + mean(Recall.ph$B); f2 * 1.96 + mean(Recall.ph$F); s2 * 1.96 + mean(Recall.ph$S); u2 * 1.96 + mean(Recall.ph$U)

#Get Lower
mean(Recall.ph$B) - b2 * 1.96; mean(Recall.ph$F) - f2 * 1.96; mean(Recall.ph$S) - s2 * 1.96; mean(Recall.ph$U) - u2 * 1.96

##Run the t-tests
temp = t.test(Recall.ph$F, Recall.ph$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#Main effect of Direction
#F vs B
temp = t.test(IOC2$F, IOC2$B, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#F vs S
temp = t.test(IOC2$F, IOC2$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

mean(IOC2$F)
sd(IOC2$F)

mean(IOC2$S)
sd(IOC2$S)

#F vs U
temp = t.test(IOC2$F, IOC2$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#B vs S
temp = t.test(IOC2$B, IOC2$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#B vs U
temp = t.test(IOC2$B, IOC2$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#S vs U
temp = t.test(IOC2$S, IOC2$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

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

#S vs S
temp = t.test(JOL.ph$S, Recall.ph$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#U vs U
temp = t.test(JOL.ph$U, Recall.ph$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

####Reactivity ANOVA####
model2 = ezANOVA(data = reactivity_data,
                 dv = Recall_Score,
                 wid = id,
                 between = Task,
                 return_aov = T,
                 within = .(Direction),
                 type = 3,
                 detailed = T)
model2 #significant everything!

length(unique(reactivity_data$id)) #102 total participants

#get MSE here
model2$ANOVA$MSE = model2$ANOVA$SSd/model2$ANOVA$DFd
model2$ANOVA$MSE

aovEffectSize(model2, effectSize = "pes")

###post-hocs here
##Get means
#Main effect of task
tapply(reactivity_data$Recall_Score, reactivity_data$Task, mean)

#Main effect of direction
tapply(reactivity_data$Recall_Score, reactivity_data$Direction, mean)

#Interaction
tapply(reactivity_data$Recall_Score, list(reactivity_data$Task, reactivity_data$Direction), mean)

##Set up data for t-tests
reactivity2 = cast(reactivity_data[ , c(1, 3, 4, 2)], id ~ Direction, mean)

reactivity3 = subset(reactivity_data,
                     reactivity_data$Task == "JOL")
reactivity4 = subset(reactivity_data,
                     reactivity_data$Task == "Study")

jol.ph = cast(reactivity3[ , c(1, 3, 4, 2)], id ~ Direction, mean)
study.ph = cast(reactivity4[ , c(1, 3, 4, 2)], id ~ Direction, mean)

##Sds for direction main effect
apply(reactivity2, 2, sd)

####Get values for Figs and Tables####
##JOL Condition
#Means
mean(jol.ph$B)
mean(jol.ph$F)
mean(jol.ph$S)
mean(jol.ph$U)

##Get sds and CIs for Table 1
b = sd(jol.ph$B)
f = sd(jol.ph$F)
s = sd(jol.ph$S)
u = sd(jol.ph$U)

b;f;s;u

#get se
b2 = b / sqrt(length(jol.ph$id))
f2 = f / sqrt(length(jol.ph$id))
s2 = s / sqrt(length(jol.ph$id))
u2 = u / sqrt(length(jol.ph$id))

#Get CI
b2 * 1.96; f2 * 1.96; s2 * 1.96; u2 *1.96

#Get Upper
b2 * 1.96 + mean(jol.ph$B); f2 * 1.96 + mean(jol.ph$F); s2 * 1.96 + mean(jol.ph$S); u2 * 1.96 + mean(jol.ph$U)

#Get Lower
mean(jol.ph$B) - b2 * 1.96; mean(jol.ph$F) - f2 * 1.96; mean(jol.ph$S) - s2 * 1.96; mean(jol.ph$U) - u2 * 1.96

##Study condition
#Means
mean(study.ph$B)
mean(study.ph$F)
mean(study.ph$S)
mean(study.ph$U)

##Get sds and CIs for Table 1
b = sd(study.ph$B)
f = sd(study.ph$F)
s = sd(study.ph$S)
u = sd(study.ph$U)

b;f;s;u

#get se
b2 = b / sqrt(length(study.ph$id))
f2 = f / sqrt(length(study.ph$id))
s2 = s / sqrt(length(study.ph$id))
u2 = u / sqrt(length(study.ph$id))

#Get CI
b2 * 1.96; f2 * 1.96; s2 * 1.96; u2 *1.96

#Get Upper
b2 * 1.96 + mean(study.ph$B); f2 * 1.96 + mean(study.ph$F); s2 * 1.96 + mean(study.ph$S); u2 * 1.96 + mean(study.ph$U)

#Get Lower
mean(study.ph$B) - b2 * 1.96; mean(study.ph$F) - f2 * 1.96; mean(study.ph$S) - s2 * 1.96; mean(study.ph$U) - u2 * 1.96

#Test whether comparisons in table 2 are significant
temp = t.test(study.ph$U, study.ph$S, paired = T, p.adjust.methods = "bonferroni")
temp

##Run the t-tests
#Main effect of Direction
#F vs B
temp = t.test(reactivity2$F, reactivity2$B, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#F vs S
temp = t.test(reactivity2$F, reactivity2$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#F vs U
temp = t.test(reactivity2$F, reactivity2$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#B vs S
temp = t.test(reactivity2$B, reactivity2$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#B vs U
temp = t.test(reactivity2$B, reactivity2$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#S vs U
temp = t.test(reactivity2$S, reactivity2$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##Interaction
#F vs F
temp = t.test(jol.ph$F, study.ph$F, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#B vs B
temp = t.test(jol.ph$B, study.ph$B, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#S vs S
temp = t.test(jol.ph$S, study.ph$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#U vs U
temp = t.test(jol.ph$U, study.ph$U, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-sig

##Need pbic
jol.ph$task = rep("JOL")
study.ph$task = rep("Study")

pbic = rbind(jol.ph, study.ph)

modelb = ezANOVA(pbic,
                 dv = U,
                 wid = id,
                 between = task,
                 type = 3,
                 detailed = T)
modelb

##Check sds for JOLs in related vs unrelated
tapply(JOL$Response.JOL, JOL$Direction, sd, na.rm = T)
