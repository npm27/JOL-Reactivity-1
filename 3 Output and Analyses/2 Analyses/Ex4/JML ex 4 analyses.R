##This is the JML Ex 2 Analysis
####Set up####
##load in data
JOL = read.csv("JOL2.csv")
RL = read.csv("Study.RL.csv")
VC = read.csv("VC.csv")
Study = read.csv("Study2.csv")

##load libraries
library(reshape)
library(ez)
library(psychReport)

####Data Screening####
summary(JOL)
summary(RL)
summary(VC)
summary(Study)

##Fix column names
colnames(JOL)[7] = "Direction"
colnames(RL)[7] = "Direction"
colnames(VC)[7] = "Direction"
colnames(Study)[7] = "Direction"

##remove out of range JOL scores
JOL$Response.JOL[JOL$Response.JOL > 100] = NA

####Check for outliers####
##get participant means
JOL2 = cast(JOL[ , c(1, 7, 13)], Username ~ Direction, mean, na.rm = T)
VC2 = cast(VC[ , c(1, 7, 13)], Username ~ Direction, mean, na.rm = T)
RL2 = cast(RL[ , c(1, 7, 12)], Username ~ Direction, mean, na.rm = T)
Study2 = cast(Study[ , c(1, 7, 12)], Username ~ Direction, mean, na.rm = T)

##Get z-scores
JOL2$B_z = scale(JOL2$B)
JOL2$F_z = scale(JOL2$F)
JOL2$S_z = scale(JOL2$S)
JOL2$U_z = scale(JOL2$U)

RL2$B_z = scale(RL2$B)
RL2$F_z = scale(RL2$F)
RL2$S_z = scale(RL2$S)
RL2$U_z = scale(RL2$U)

VC2$B_z = scale(VC2$B)
VC2$F_z = scale(VC2$F)
VC2$S_z = scale(VC2$S)
VC2$U_z = scale(VC2$U)

Study2$B_z = scale(Study2$B)
Study2$F_z = scale(Study2$F)
Study2$S_z = scale(Study2$S)
Study2$U_z = scale(Study2$U)

####Get descriptives####
tapply(JOL$Recall_Score, JOL$Direction, mean, na.rm = T)
tapply(RL$Recall_Score, RL$Direction, mean, na.rm = T)
tapply(VC$Recall_Score, VC$Direction, mean, na.rm = T)
tapply(Study$Recall_Score, Study$Direction, mean, na.rm = T)

####ANOVA TIME####
##Combine datasets
JOL3 = JOL[ , c(1, 6, 7, 13)]
JOL3$Encoding = rep("JOL")

VC3 = VC[ , c(1, 6, 7, 13)]
VC3$Encoding = rep("VC")

RL3 = RL[ , c(1, 6, 7, 12)]
RL3$Encoding = rep("RL")

Study3 = Study[ , c(1, 6, 7, 12)]
Study3$Encoding = rep("Study")

Anova.data = rbind(JOL3, VC3, RL3, Study3)

length(unique(Anova.data$Username))

##Run the ANOVA
model1 = ezANOVA(Anova.data,
                 within = .(Direction, Stimuli.Shuffle),
                 between = Encoding,
                 wid = Username,
                 dv = Recall_Score,
                 type = 3,
                 return_aov = T,
                 detailed = TRUE)
model1 ##Everything came out significant!

aovEffectSize(model1, effectSize = "pes")

##Get MSE
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

####interaction between block order and direction####
tapply(Anova.data$Recall_Score, list(Anova.data$Stimuli.Shuffle, Anova.data$Direction), mean, na.rm = T)

##sET UP FOR T.TESTS
b1 = subset(Anova.data,
            Anova.data$Stimuli.Shuffle == "Study1")
b2 = subset(Anova.data,
            Anova.data$Stimuli.Shuffle == "Study2")

block1 = cast(b1[ , -5], Username ~ Direction, mean, na.rm = T)
block2 = cast(b2[ , -5], Username ~ Direction, mean, na.rm = T)

mean(block1$B)
mean(block2$B)

##T-tests
temp = t.test(block1$F, block2$F, paired = T, p.adjust.methods = "none")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

##Get pbic
b.data = subset(Anova.data,
                Anova.data$Direction == "B")
f.data = subset(Anova.data,
                Anova.data$Direction == "F")

PBICMODEL = ezANOVA(data = f.data,
                    dv = Recall_Score,
                    within = Stimuli.Shuffle,
                    wid = Username,
                    detailed = T,
                    type = 3)
PBICMODEL

####Post-hocs####
##Main Effect of Direction
#Get the means
tapply(Anova.data$Recall_Score, Anova.data$Direction, mean)

##Set up data for t-tests and cohen's d/CIs for table
Direction = cast(Anova.data[ , -5], Username ~ Direction, mean)

#Get the sds
bd = sd(Direction$B)
fd = sd(Direction$F)
sd = sd(Direction$S)
ud = sd(Direction$U)

bd;fd;sd;ud

##Get the CIs
#Start with se
bd2 = bd / sqrt(length(Direction$Username))
fd2 = fd / sqrt(length(Direction$Username))
sd2 = sd / sqrt(length(Direction$Username))
ud2 = ud / sqrt(length(Direction$Username))

#CI
bd2 * 1.96; fd2 * 1.96; sd2 * 1.96; ud2 *1.96

#Get Upper
bd2 * 1.96 + mean(Direction$B); fd2 * 1.96 + mean(Direction$F); sd2 * 1.96 + mean(Direction$S); ud2 * 1.96 + mean(Direction$U)

#Get Lower
mean(Direction$B) - bd2 * 1.96; mean(Direction$F) - fd2 * 1.96; mean(Direction$S) - sd2 * 1.96; mean(Direction$U) - ud2 * 1.96

##t-tests
#F vs B
temp = t.test(Direction$F, Direction$B, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#F vs S
temp = t.test(Direction$F, Direction$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#F vs U
temp = t.test(Direction$F, Direction$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#B vs S
temp = t.test(Direction$B, Direction$S, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

#B vs U
temp = t.test(Direction$B, Direction$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

mean(Direction$B)
sd(Direction$B)

mean(Direction$U)
sd(Direction$U)

#S vs U
temp = t.test(Direction$S, Direction$U, paired = T, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##Main Effect of Encoding
tapply(Anova.data$Recall_Score, Anova.data$Encoding, mean)

##Set up data for t-tests and cohen's d/CIs for table
Anova.data2 = Anova.data[ , c(1, 2, 4, 3)] #Reorder columns

Encoding = cast(Anova.data2, Username ~ Encoding, mean)

#Get the sds
jl = sd(Encoding$JOL, na.rm = T)
vc = sd(Encoding$VC, na.rm = T)
rl = sd(Encoding$RL, na.rm = T)
st = sd(Encoding$Study, na.rm = T)

jl;vc;rl;st

#Get the CI
#Start with se
jl2 = jl / sqrt(length(Encoding$Username))
vc2 = vc / sqrt(length(Encoding$Username))
rl2 = rl / sqrt(length(Encoding$Username))
st2 = st / sqrt(length(Encoding$Username))

#CI
jl2 * 1.96; vc2 * 1.96; rl2 * 1.96; st2 *1.96

#Get Upper
jl2 * 1.96 + mean(Encoding$JOL, na.rm = T); vc2 * 1.96 + mean(Encoding$VC, na.rm = T); rl2 * 1.96 + mean(Encoding$RL, na.rm = T); st2 * 1.96 + mean(Encoding$Study, na.rm = T)

#Get Lower
mean(Encoding$JOL, na.rm = T) - jl2 * 1.96; mean(Encoding$VC, na.rm = T) - vc2 * 1.96; mean(Encoding$RL, na.rm = T) - rl2 * 1.96; mean(Encoding$Study, na.rm = T) - st2 * 1.96

##Now time for t-tests
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
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

#RL vs Study
temp = t.test(Encoding$RL, Encoding$Study, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##Interaction
tapply(Anova.data$Recall_Score, list(Anova.data$Encoding, Anova.data$Direction), mean)

##Set up data for interaction
JOL3 = JOL3[ , -5]
RL3 = RL3[ , -5]
VC3 = VC3[ , -5]
Study3 = Study3[ , -5]

JOL4 = cast(JOL3, Username ~ Direction, mean)
RL4 = cast(RL3, Username ~ Direction, mean)
VC4 = cast(VC3, Username ~ Direction, mean)
Study4 = cast(Study3, Username ~ Direction, mean)

####Get Sds for cohen's d####
sd_jol = apply(JOL4, 2, sd)
sd_RL = apply(RL4, 2, sd)
sd_VC = apply(VC4, 2, sd)
sd_ST = apply(Study4, 2, sd)

sd_jol;sd_RL;sd_VC;sd_ST

#Get means
m_jol = apply(JOL4, 2, mean)
m_RL = apply(RL4, 2, mean)
m_VC = apply(VC4, 2, mean)
m_ST = apply(Study4, 2, mean)

#Get se's
se_jol = sd_jol / sqrt(nrow(JOL4))
se_RL = sd_RL / sqrt(nrow(RL4))
se_VC = sd_VC / sqrt(nrow(VC4))
se_ST = sd_ST / sqrt(nrow(Study4))

#Get CI's ##These are for the table
ci_jol = se_jol * 1.96
ci_RL = se_RL * 1.96
ci_VC = se_VC * 1.96
ci_ST = se_ST * 1.96

##Get Upper
u_jol = ci_jol + m_jol
u_RL = ci_RL + m_RL
u_VC = ci_VC + m_VC
u_ST = ci_ST + m_ST

##Get Lower
l_jol = m_jol - ci_jol
l_RL = m_RL - ci_RL
l_VC = m_VC - ci_VC
l_ST = m_ST - ci_ST

####Now run t-tests for interaction####
###Forward
##Jol vs RL
temp = t.test(JOL4$F, RL4$F, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

##JOL vs Study
temp = t.test(JOL4$F, Study4$F, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##Jol vs VC
temp = t.test(JOL4$F, VC4$F, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##RL vs Study
temp = t.test(RL4$F, Study4$F, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##RL vs VC
temp = t.test(RL4$F, VC4$F, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##VC vs Study
temp = t.test(VC4$F, Study4$F, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

###Backward
##Jol vs RL
temp = t.test(JOL4$B, RL4$B, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

##JOL vs Study
temp = t.test(JOL4$B, Study4$B, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##Jol vs VC
temp = t.test(JOL4$B, VC4$B, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##RL vs Study
temp = t.test(RL4$B, Study4$B, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##RL vs VC
temp = t.test(RL4$B, VC4$B, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##VC vs Study
temp = t.test(VC4$B, Study4$B, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

###Symmetrical
##Jol vs RL
temp = t.test(JOL4$S, RL4$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Non-Sig

##JOL vs Study
temp = t.test(JOL4$S, Study4$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##Jol vs VC
temp = t.test(JOL4$S, VC4$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##RL vs Study
temp = t.test(RL4$S, Study4$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##RL vs VC
temp = t.test(RL4$S, VC4$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##VC vs Study
temp = t.test(VC4$S, Study4$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

p.adjust(temp$p.value, method = "bonferroni", n = 3)

###unrelated
##Jol vs RL
temp = t.test(JOL4$U, RL4$U, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #SIG!

##JOL vs Study
temp = t.test(JOL4$U, Study4$U, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #NON-Sig

##Jol vs VC
temp = t.test(JOL4$U, VC4$U, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #NON-Sig

##RL vs Study
temp = t.test(RL4$U, Study4$U, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##RL vs VC
temp = t.test(RL4$U, VC4$U, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #Sig

##VC vs Study
temp = t.test(VC4$U, Study4$U, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92 #NON-Sig

####Reactivity pbic stuff####
##Direction


##Study group
pbic = rbind(VC4, Study4)

encoding.s = Encoding[ , c(1, 4)]
encoding.v = Encoding[ , c(1, 5)]
encoding.j = Encoding[ , c(1, 2)]
encoding.r = Encoding[ , c(1, 3)]

colnames(encoding.s)[2] = "Score"
colnames(encoding.v)[2] = "Score"
colnames(encoding.r)[2] = "Score"
colnames(encoding.j)[2] = "Score"

encoding.s$task = rep("study")
encoding.v$task = rep("vowel")
encoding.r$task = rep("rl")
encoding.j$task = rep("jol")

dat1 = rbind(encoding.j, encoding.r)
dat2 = rbind(encoding.v, encoding.s)

dat1 = na.omit(dat1)
dat2 = na.omit(dat2)

modela = ezANOVA(dat2,
                 dv = Score,
                 wid = Username,
                 between = task,
                 type = 3,
                 detailed = T)
modela


##Interaction
JOL4$task = rep("jol")
RL4$task = rep("RL")
VC4$task = rep("vc")
Study4$task = rep("study")

pbic = rbind(JOL4, RL4)
pbic2 = rbind(Study4, VC4)
pbic3 = rbind(VC4, JOL4)

colnames(pbic)[3] = "f"

modelb = ezANOVA(pbic,
                 dv = f,
                 wid = Username,
                 between = task,
                 type = 3,
                 detailed = T)
modelb

#check sds
tapply(JOL$Response.JOL, JOL$Direction, sd, na.rm = T)