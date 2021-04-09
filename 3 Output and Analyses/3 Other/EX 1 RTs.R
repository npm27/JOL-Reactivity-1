####ex 1 rt stuff####
library(reshape)
library(ez)

JOL = read.csv("Scored Output/JOL3.csv")
Study = read.csv("Scored Output/Study3.csv")
options(scipen = 999)

##Let's take a look at the data
summary(JOL)
summary(Study)

#Fix out of range JOLs
JOL$Response.JOL[JOL$Response.JOL > 100] = NA

#Fix column names
colnames(JOL)[7] = "Direction"
colnames(Study)[7] = "Direction"

length(unique(JOL$Username))
length(unique(Study$Username))

#Drop unused columns
JOL2 = JOL[ , -c(2:6, 9:12)] #Will use this one for reactivity

JOL2$Task = rep("JOL")

Study = Study[ , -c(2:6, 9:11)]
Study$Task = rep("Study")

colnames(Study)[2:3] = c("Direction", "RT")
colnames(JOL2)[2:3] = c("Direction", "RT")

reactivity_data = rbind(Study, JOL2)

##get descriptives
tapply(reactivity_data$RT, list(reactivity_data$Task, reactivity_data$Direction), mean)

tapply(reactivity_data$RT, reactivity_data$Task, mean)

tapply(reactivity_data$RT, reactivity_data$Direction, mean)

stuff = tapply(reactivity_data$RT, list(reactivity_data$Task, reactivity_data$Direction), sd)
stuff / sqrt(length(unique(JOL$Username)))
stuff / sqrt(length(unique(Study$Username)))

####Run an ANOVA####
model1 = ezANOVA(reactivity_data,
        dv = RT,
        wid = Username,
        between = Task,
        within = Direction,
        type = 3,
        detailed = T)
model1

#Get MSE here
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

##set up for post-hoc
jol = subset(reactivity_data,
             reactivity_data$Task == "JOL")
study = subset(reactivity_data,
             reactivity_data$Task == "Study")

jol3 = cast(JOL2[ , -c(4:5)], Username ~ Direction, mean)
study3 = cast(Study[ , -c(4:5)], Username ~ Direction, mean)

##T-tests
temp = t.test(jol3$B, study3$B, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(jol3$F, study3$F, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(jol3$S, study3$S, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

temp = t.test(jol3$U, study3$U, paired = F, p.adjust.methods = "bonferroni")
temp
round(temp$p.value, 3)
temp$statistic
(temp$conf.int[2] - temp$conf.int[1]) / 3.92

####Okay, put everything back together and re-run with subject level means####
library(data.table)
jol3$task = rep("jol")

jol4 = melt(jol3,
            measured = c("B", "F", "S", "U"))
colnames(jol4)[3:4] = c("Direction", "RT")

study3$task = rep("study")

study4 = melt(study3,
            measured = c("B", "F", "S", "U"))
colnames(study4)[3:4] = c("Direction", "RT")

reactivity2 = rbind(study4, jol4)

reactivity2$RT2 = reactivity2$RT / 1000

mean(reactivity2$RT)
mean(reactivity2$RT2)

##Rerun the model
model1 = ezANOVA(reactivity2,
                 dv = RT,
                 wid = Username,
                 between = task,
                 within = Direction,
                 type = 3,
                 detailed = T)
model1

#Get MSE here
model1$ANOVA$MSE = model1$ANOVA$SSd/model1$ANOVA$DFd
model1$ANOVA$MSE

reactivity3 = rbind(jol3, study3)

#write.csv(reactivity3, file = "Ex 1 RTs.csv", row.names = F)
