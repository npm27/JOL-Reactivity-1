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
library(psychReport)

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
                 return_aov = T,
                 detailed = T)
model1

#Get MSE here
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

aovEffectSize(model1, effectSize = "pes")

##Post-hocs -- 3way
t.test(long.read$B, long.JOL$B, paired = F, p.adjust.methods = "none", var.equal = T)

##Forward
temp = t.test(long.JOL$F, long.JAM$F, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

##effect size
mean(long.JAM$F);mean(long.read$F)
sd(long.JAM$F);sd(long.read$F)

##pbic
pbic1 = long.JAM[ , c(1, 3)]
pbic2 = long.JOL[ , c(1, 3)]

pbic1$task = rep("JAM")
pbic2$task = rep("JOL")

pbic3 = rbind(pbic1, pbic2)

model = ezANOVA(pbic3,
                wid = Sub.ID,
                between = task,
                dv = F,
                detailed = T,
                type = 3)

model

##symmetrical
temp = t.test(long.JOL$S, long.read$S, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

##eff size
mean(long.JOL$S);mean(long.read$S)
sd(long.JOL$S);sd(long.read$S)

temp = t.test(long.JOL$S, long.JAM$S, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

##pbic
pbic1 = long.JAM[ , c(1, 4)]
pbic2 = long.JOL[ , c(1, 4)]

pbic1$task = rep("JAM")
pbic2$task = rep("JOL")

pbic3 = rbind(pbic1, pbic2)

model = ezANOVA(pbic3,
                wid = Sub.ID,
                between = task,
                dv = S,
                detailed = T,
                type = 3)

model

##backward
temp = t.test(long.JOL$B, long.JAM$B, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

temp = t.test(long.JOL$B, long.read$B, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

##eff size
mean(long.JOL$B);mean(long.read$B)
sd(long.JOL$B);sd(long.read$B)

temp = t.test(long.read$F, long.read$S, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

##EFF SIZE 
mean(long.JAM$B);mean(long.read$B)
sd(long.JAM$B);sd(long.read$B)

##Effect size for table
##Jams
y = apply(long.JAM, 2, mean)
apply(long.JAM, 2, sd)
x = apply(long.JAM, 2, sd) 

x2 = x / sqrt(length(unique(long.JAM$Sub.ID)))
x3 = x2 * 1.96

upper = y + x3
lower = y - x3

upper
lower

#CONTROL
apply(long.read, 2, mean)
apply(long.read, 2, sd)

y = apply(long.read, 2, mean)
apply(long.read, 2, sd)
x = apply(long.read, 2, sd) 

x2 = x / sqrt(length(unique(long.read$Sub.ID)))
x3 = x2 * 1.96

upper = y + x3
lower = y - x3

upper
lower

##JOL
y = apply(long.JOL, 2, mean)
apply(long.JOL, 2, sd)
x = apply(long.JOL, 2, sd) 

x2 = x / sqrt(length(unique(long.JOL$Sub.ID)))
x3 = x2 * 1.96

upper = y + x3
lower = y - x3

upper
lower

##pbics
pbic1 = long.JAM[ , c(1, 2)]
pbic2 = long.JOL[ , c(1, 2)]
pbic4 = long.read[ , c(1, 2)]

pbic1$task = rep("JAM")
pbic2$task = rep("JOL")
pbic4$task = rep("read")

pbic3 = rbind(pbic1, pbic2)

model = ezANOVA(pbic3,
                wid = Sub.ID,
                between = task,
                dv = B,
                detailed = T,
                type = 3)

model

pbic5 = rbind(pbic2, pbic4)

model = ezANOVA(pbic5,
                wid = Sub.ID,
                between = task,
                dv = B,
                detailed = T,
                type = 3)

model

##Unrelated
temp = t.test(long.JOL$U, long.read$U, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

##PBIC
pbic1 = long.JAM[ , c(1, 5)]
pbic2 = long.JOL[ , c(1, 5)]
pbic4 = long.read[ , c(1, 5)]

pbic1$task = rep("JAM")
pbic2$task = rep("JOL")
pbic4$task = rep("read")

pbic5 = rbind(pbic2, pbic4)

model = ezANOVA(pbic5,
                wid = Sub.ID,
                between = task,
                dv = U,
                detailed = T,
                type = 3)

model

##main effect of pair type
tapply(combined$Scored, combined$Direction, mean)

##main effect of encoding group
tapply(combined$Scored, combined$Encoding, mean)

##interaction
tapply(combined$Scored, list(combined$Encoding, combined$Direction), mean)

##Write subject level to csv
#write.csv(long.read, file = "READ2.csv", row.names = F)
#write.csv(long.JAM, file = "JAM2.csv", row.names = F)
#write.csv(long.JOL, file = "JOL2.csv", row.names = F)

combined = combined[ , c(1, 2, 4, 3)]

##Do post-hoc testing
#Direction
reactivity.direction = cast(combined, Sub.ID ~ Direction, mean)

temp = t.test(reactivity.direction$F, reactivity.direction$S, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

mean(reactivity.direction$F); mean(reactivity.direction$S)
sd(reactivity.direction$F); sd(reactivity.direction$S)

#Encoding
reactivity.encoding = cast(combined, Sub.ID ~ Encoding, mean)

temp = t.test(reactivity.encoding$JAM, reactivity.encoding$Read, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

mean(reactivity.encoding$JAM, na.rm = T); mean(reactivity.encoding$Read, na.rm = T)
sd(reactivity.encoding$JAM, na.rm = T); sd(reactivity.encoding$Read, na.rm = T)

temp = t.test(reactivity.encoding$JAM, reactivity.encoding$JOL, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

##Get pbic
pbic1 = reactivity.encoding[ , c(1, 2)]
pbic2 = reactivity.encoding[ , c(1, 3)]

colnames(pbic1)[2] = "Score"
colnames(pbic2)[2] = "Score"

pbic1$task = rep("JOL")
pbic2$task = rep("JAM")

pbic3 = rbind(pbic1, pbic2)

pbic3 = na.omit(pbic3)

model = ezANOVA(pbic3,
                wid = Sub.ID,
                between = task,
                dv = Score,
                detailed = T,
                type = 3)

model

####Illusion of competence####
JOL2 = JOL[ , c(2, 8, 10, 15)]

IOC = melt(JOL2, measure.vars = c("Response.JOL", "Scored"))

colnames(IOC)[3:4] = c("Task", "Score")

IOC = na.omit(IOC)

model2 = ezANOVA(IOC,
                 dv = Score,
                 wid = Sub.ID,
                 within = .(Task, Direction),
                 type = 3,
                 return_aov = T,
                 detailed = T)

model2$ANOVA$MSE = model2$ANOVA$SSd/model2$ANOVA$DFd

aovEffectSize(model2, effectSize = "pes")

model2

##Get means for effects
tapply(IOC$Score, IOC$Direction, mean)
tapply(IOC$Score, IOC$Task, mean)
tapply(IOC$Score, list(IOC$Task, IOC$Direction), mean)

####IOC Posthocs####
##Direction
IOC_direction = cast(IOC, Sub.ID ~ Direction, mean)

temp = t.test(IOC_direction$F, IOC_direction$S, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

mean(IOC_direction$F); mean(IOC_direction$S)
sd(IOC_direction$F); sd(IOC_direction$S)

##interaction
unique(IOC$Task)

IOC_JOLs = subset(IOC, IOC$Task == "Response.JOL")
IOC_Recall = subset(IOC, IOC$Task == "Scored")

IOC_JOLs2 = cast(IOC_JOLs, Sub.ID ~ Direction, mean)
IOC_Recall2 = cast(IOC_Recall, Sub.ID ~ Direction, mean)

temp = t.test(IOC_JOLs2$F, IOC_Recall2$F, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

temp = t.test(IOC_JOLs2$B, IOC_Recall2$B, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

mean(IOC_JOLs2$B); mean(IOC_Recall2$B)
sd(IOC_JOLs2$B); sd(IOC_Recall2$B)

temp = t.test(IOC_JOLs2$S, IOC_Recall2$S, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

mean(IOC_JOLs2$S); mean(IOC_Recall2$S)
sd(IOC_JOLs2$S); sd(IOC_Recall2$S)

temp = t.test(IOC_JOLs2$U, IOC_Recall2$U, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

mean(IOC_JOLs2$U); mean(IOC_Recall2$U)
sd(IOC_JOLs2$U); sd(IOC_Recall2$U)

##Get CIs for table
(apply(IOC_JOLs2, 2, sd) / sqrt(length(unique(IOC_JOLs$Sub.ID)))) * 1.96

#Get means and sds for eff size
apply(IOC_JOLs2, 2, mean)
apply(IOC_JOLs2, 2, sd)

##Get it means and CIs for fig
y = apply(IOC_JOLs2, 2, mean)
apply(IOC_JOLs2, 2, sd)
x = apply(IOC_JOLs2, 2, sd) 

x2 = x / sqrt(length(unique(IOC_JOLs2$Sub.ID)))
x3 = x2 * 1.96

upper = y + x3
lower = y - x3

upper
lower

##Do it again for recall
apply(IOC_Recall2, 2, mean)
apply(IOC_Recall2, 2, sd)

##Get pbic for forward pairs
pbic1 = IOC_JOLs2[ , c(1, 3)]
pbic2 = IOC_Recall2[ , c(1, 3)]

pbic1$task = rep("JOL")
pbic2$task = rep("Recall")

pbic3 = rbind(pbic1, pbic2)

model = ezANOVA(pbic3,
                wid = Sub.ID,
                dv = F,
                within = task,
                detailed = T,
                type = 3)
model

####Differences as a function of sample source####
combined$key = substring(combined$Sub.ID, 1, 1)

prolific =  subset(combined,
                   combined$key == "5")
sona = subset(combined,
              combined$key != "5")

table(prolific$Sub.ID)
table(sona$Sub.ID)

prolific$source = rep("prolific")
sona$source = rep("sona")

combined2 = rbind(prolific, sona)

unique(prolific$Encoding)
length(unique(prolific$Sub.ID))

##get ns
JAM_p = subset(prolific,
               prolific$Encoding == "JAM")
length(unique(JAM_p$Sub.ID)) #10

JOL_p = subset(prolific,
               prolific$Encoding == "JOL")
length(unique(JOL_p$Sub.ID)) #11

read_p = subset(prolific,
                prolific$Encoding == "Read") 
length(unique(read_p$Sub.ID)) #7

##Get data in the right shape for t-tests
prolific2 = cast(prolific[ , -c(5,6)], Sub.ID ~ Encoding, mean)
sona2 = cast(sona[ , -c(5,6)], Sub.ID ~ Encoding, mean)

##run some t tests
#JAM
temp = t.test(prolific2$JAM, sona2$JAM, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

mean(prolific2$JAM, na.rm = T)
mean(sona2$JAM, na.rm = T)

#JOL
temp = t.test(prolific2$JOL, sona2$JOL, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

mean(prolific2$JOL, na.rm = T)
mean(sona2$JOL, na.rm = T)

#Control
temp = t.test(prolific2$Read, sona2$Read, paired = F, p.adjust.methods = "Bonferroni", var.equal = T)
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92
temp

mean(prolific2$Read, na.rm = T)
mean(sona2$Read, na.rm = T)

##Get pbics
pbic = prolific2[ , c(1, 3)]
pbic2 = sona2[ , c(1, 3)]

pbic$task = rep("prolific")
pbic2$task = rep("sona")

pbic3 = rbind(pbic, pbic2)

pbic3 = na.omit(pbic3)

ezANOVA(pbic3,
        dv = JOL,
        wid = Sub.ID,
        between = task,
        type = 3,
        detailed = T)

pbic = prolific2[ , c(1, 4)]
pbic2 = sona2[ , c(1, 4)]

pbic$task = rep("prolific")
pbic2$task = rep("sona")

pbic3 = rbind(pbic, pbic2)

pbic3 = na.omit(pbic3)

ezANOVA(pbic3,
        dv = Read,
        wid = Sub.ID,
        between = task,
        type = 3,
        detailed = T)

##get sds
tapply(JOL$Response.JOL, JOL$Direction, sd, na.rm = T)
tapply(as.numeric(JAM$Response.JOL), JAM$Direction, sd, na.rm = T)

sd(IOC_JOLs2$U)
