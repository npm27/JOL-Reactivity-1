####This is now Experiment 4####
library(reshape)
library(ez)

JOL = read.csv("JOL2.csv")
RL = read.csv("Study.RL.csv")
VC = read.csv("VC.csv")
Study = read.csv("Study2.csv")
options(scipen = 999)

##Fix column names
colnames(JOL)[7] = "Direction"
colnames(RL)[7] = "Direction"
colnames(VC)[7] = "Direction"
colnames(Study)[7] = "Direction"

##remove out of range JOL scores
JOL$Response.JOL[JOL$Response.JOL > 100] = NA

####ANOVA TIME####
##Combine datasets
JOL3 = JOL[ , c(1, 7, 8)]
JOL3$Encoding = rep("JOL")

VC3 = VC[ , c(1, 7, 8)]
VC3$Encoding = rep("VC")

RL3 = RL[ , c(1, 7, 8)]
RL3$Encoding = rep("RL")

Study3 = Study[ , c(1, 7, 8)]
Study3$Encoding = rep("Study")

colnames(JOL3)[3] = "rt"
colnames(VC3)[3] = "rt"
colnames(RL3)[3] = "rt"
colnames(Study3)[3] = "rt"

Anova.data = rbind(JOL3, VC3, RL3, Study3)

tapply(Anova.data$rt, Anova.data$Direction, mean)
tapply(Anova.data$rt, Anova.data$Encoding, mean)
tapply(Anova.data$rt, list(Anova.data$Encoding, Anova.data$Direction), mean)

model1 = ezANOVA(Anova.data,
                 within = Direction,
                 between = Encoding,
                 wid = Username,
                 dv = rt,
                 type = 3,
                 detailed = TRUE)
model1

##Get MSE
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

##Set up
##set up for post-hoc
jol4 = cast(JOL3[ , -4], Username ~ Direction, mean)
study4 = cast(Study3[ , -4], Username ~ Direction, mean)
RL4 = cast(RL3[ , -4], Username ~ Direction, mean)
VC4 = cast(VC3[ , -4], Username ~ Direction, mean)

VC4$task = rep("vc")
RL4$task = rep("RL")
jol4$task = rep("jol")
study4$task = rep("Study")

encoding = rbind(VC4, RL4, jol4, study4)

mean(encoding$B)

##Effect of Direction
#F VS B
temp = t.test(encoding$F, encoding$B, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #NON
#F VS S
temp = t.test(encoding$F, encoding$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non

#F VS U
temp = t.test(encoding$F, encoding$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#B VS S
temp = t.test(encoding$B, encoding$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #non

#B VS U
temp = t.test(encoding$B, encoding$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig ##compute d for this one

mean(encoding$B)
sd(encoding$B)

mean(encoding$U)
sd(encoding$U)

#S VS U
temp = t.test(encoding$S, encoding$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##pBIC
sub1 = encoding[ , c(1, 3, 6)]
sub2 = encoding[ , c(1, 4, 6)]

colnames(sub1)[2] = "score"
colnames(sub2)[2] = "score"

sub1$direction = rep("f")
sub2$direction = rep("s")

sub3 = rbind(sub1, sub2)


modelb = ezANOVA(sub3,
                 dv = score,
                 wid = Username,
                 between = direction,
                 type = 3,
                 detailed = T)
modelb


##Main effect of encoding
Anova.data2 = Anova.data[ , -2]
Anova.data2 = Anova.data2[ , c(1, 3, 2)]

anova.data3 = cast(Anova.data2, Username ~ Encoding, mean)

Encoding = anova.data3

##JOL vs VC
temp = t.test(Encoding$JOL, Encoding$VC, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#JOL vs RL
temp = t.test(Encoding$JOL, Encoding$RL, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

#JOL vs Study
temp = t.test(Encoding$JOL, Encoding$Study, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#vc VS RL
temp = t.test(Encoding$VC, Encoding$RL, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#VC vs Study
temp = t.test(Encoding$VC, Encoding$Study, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#RL vs Study
temp = t.test(Encoding$RL, Encoding$Study, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig #GET d for this one

##PBIC
sub3 = Encoding[ , c(1:2)]
sub4 = Encoding[ , c(1,3)]

sub3$task = rep("jol")
sub4$task = rep("rl")

colnames(sub3)[2] = "score"
colnames(sub4)[2] = "score"

sub5 = rbind(sub3, sub4)
sub5 = na.omit(sub5)

modelb = ezANOVA(sub5,
                 dv = score,
                 wid = Username,
                 between = task,
                 type = 3,
                 detailed = T)
modelb

####Get cohen's d####
mean(Encoding$RL)
sd(Encoding$RL)

mean(Encoding$Study)
sd(Encoding$Study)

mean(encoding$U)
sd(encoding$U)

mean(encoding$B)
sd(encoding$B)
