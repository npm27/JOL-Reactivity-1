####Score JAM data####
##Read in data
setwd("C:/Users/nickm.000/Documents/JOL reactivity/3 Output/JAM/Judgments") #get the correct directory

#Get the files names
files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$Username))

#Now do the study only condition
setwd('..')

setwd("C:/Users/nickm.000/Documents/JOL reactivity/3 Output/JAM/Read") #get the correct directory

#Get the files names
files2 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat2 = do.call(rbind, lapply(files2, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat2$Username))

#Now do the vowel counting task
setwd('..')

####Clean up the data files####
##Separate out the JOL and JAM data
dat3 = subset(dat,
              dat$Condition.Number > 8) ##JAM
dat = subset(dat,
             dat$Condition.Number < 9) #JOL

length(unique(dat3$Username)) #27 JAM
length(unique(dat$Username)) #33 JOL
length(unique(dat2$Username)) #35 Read

##Drop unused columns
dat = dat[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:32,  34)] #JOL data
dat2 = dat2[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:33)] #Study condition
dat3 = dat3[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:32,  34)] #JAM condition

#Next, remove buffer trials
dat = subset(dat,
             dat$Stimuli.Stimuli.Notes != "Buffer")
dat2 = subset(dat2,
              dat2$Stimuli.Stimuli.Notes != "Buffer")
dat3 = subset(dat3,
              dat3$Stimuli.Stimuli.Notes != "Buffer")

#Now remove instruction trials
dat = subset(dat,
             dat$Procedure.Trial.Type != "Instruct")
dat2 = subset(dat2,
              dat2$Procedure.Trial.Type != "Instruct")
dat3 = subset(dat3,
              dat3$Procedure.Trial.Type != "Instruct")

#Now remove filler task
dat = subset(dat,
             dat$Procedure.Trial.Type != "FreeRecall")
dat2 = subset(dat2,
              dat2$Procedure.Trial.Type != "FreeRecall")
dat3 = subset(dat3,
              dat3$Procedure.Trial.Type != "FreeRecall")

####Score the data####
library(lrd)

dat.JOL = subset(dat,
                 dat$Procedure.Trial.Type == "JOL")
dat.Recall = subset(dat,
                    dat$Procedure.Trial.Type == "Test")

#get JOLs and Recall in the same order
dat.JOL = dat.JOL[order(dat.JOL$Stimuli.Cue), ]
dat.JOL = dat.JOL[order(dat.JOL$Condition.Number), ]
dat.JOL = dat.JOL[order(dat.JOL$Stimuli.Shuffle), ]

dat.Recall = dat.Recall[order(dat.Recall$Stimuli.Cue), ]
dat.Recall = dat.Recall[order(dat.Recall$Condition.Number), ]
dat.Recall = dat.Recall[order(dat.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat.R = dat.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
JOL = cbind(dat.JOL, dat.R)
JOL = JOL[ , -c(11, 13:14)]

colnames(JOL)[11] = "JOL.RT"

JOL = JOL[ , -c(9:10)]
JOL = JOL[ , -2]

length(unique(JOL$Username))

##Now do the same for the study only condition
#Start by subsetting out the recall and study trials for each dataset
dat2.Study = subset(dat2,
                    dat2$Procedure.Trial.Type == "Study")
dat2.Recall = subset(dat2,
                     dat2$Procedure.Trial.Type == "Test")

#get Study and Recall in the same order
dat2.Study = dat2.Study[order(dat2.Study$Stimuli.Cue), ]
dat2.Study = dat2.Study[order(dat2.Study$Condition.Number), ]
dat2.Study = dat2.Study[order(dat2.Study$Stimuli.Shuffle), ]

dat2.Recall = dat2.Recall[order(dat2.Recall$Stimuli.Cue), ]
dat2.Recall = dat2.Recall[order(dat2.Recall$Condition.Number), ]
dat2.Recall = dat2.Recall[order(dat2.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat2.R = dat2.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
Study = cbind(dat2.Study, dat2.R)
Study = Study[ , -c(11, 13:14)]

colnames(Study)[11] = "Study.RT"

Study = Study[ , -c(9:10)]
Study = Study[ , -2]

length(unique(Study$Username))

##Now do the frequency judgments
#Start by subsetting out the recall and JOL data for each dataset
dat3.JAM = subset(dat3,
                   dat3$Procedure.Trial.Type == "JAM")
dat3.Recall = subset(dat3,
                     dat3$Procedure.Trial.Type == "Test")

#length(unique(dat3.Recall$Username))

#get JOLs and Recall in the same order
dat3.JAM = dat3.JAM[order(dat3.JAM$Stimuli.Cue), ]
dat3.JAM = dat3.JAM[order(dat3.JAM$Condition.Number), ]
dat3.JAM = dat3.JAM[order(dat3.JAM$Stimuli.Shuffle), ]

dat3.Recall = dat3.Recall[order(dat3.Recall$Stimuli.Cue), ]
dat3.Recall = dat3.Recall[order(dat3.Recall$Condition.Number), ]
dat3.Recall = dat3.Recall[order(dat3.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat3.R = dat3.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
JAM = cbind(dat3.JAM, dat3.R)
JAM = JAM[ , -c(11, 13:14)]

colnames(JAM)[11] = "JAM.RT"

JAM = JAM[ , -c(9:10)]
JAM = JAM[ , -2]

length(unique(JAM$Username))

####Use lrd to score the data####
##Start w/ JOL data
JOL$Response.Response = tolower(JOL$Response.Response)
JOL$Stimuli.Answer = tolower(JOL$Stimuli.Answer)

##There are technically 4 different datasets, so I guess I need to subset these before scoring:
unique(JOL$Condition.Number)

JOL1 = subset(JOL,
              JOL$Condition.Number == 5)

JOL1 = JOL1[order(JOL1$Username), ]

JOL1$trial_num = rep(1:160)

output1 = prop_correct_cued(JOL1, responses = "Response.Response", key = "Stimuli.Answer",
                  key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                  cutoff = 2)
JOL2 = subset(JOL,
              JOL$Condition.Number == 6)

JOL2 = JOL2[order(JOL2$Username), ]

JOL2$trial_num = rep(1:160)

output2 = prop_correct_cued(JOL2, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

JOL3 = subset(JOL,
              JOL$Condition.Number == 7)

JOL3 = JOL3[order(JOL3$Username), ]

JOL3$trial_num = rep(1:160)

output3 = prop_correct_cued(JOL3, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

JOL4 = subset(JOL,
              JOL$Condition.Number == 8)

JOL4 = JOL4[order(JOL4$Username), ]

JOL4$trial_num = rep(1:160)

output4 = prop_correct_cued(JOL4, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

##Put all the JOL output back together
output1a = output1$DF_Scored
output2a = output2$DF_Scored
output3a = output3$DF_Scored
output4a = output4$DF_Scored

JOL_Scored = rbind(output1a, output2a, output3a, output4a)

####Okay, now do the JAM data####
JAM$Response.Response = tolower(JAM$Response.Response)
JAM$Stimuli.Answer = tolower(JAM$Stimuli.Answer)

##There are technically 4 different datasets, so I guess I need to subset these before scoring:
unique(JAM$Condition.Number)

JAM1 = subset(JAM,
              JAM$Condition.Number == 9)

JAM1 = JAM1[order(JAM1$Username), ]

JAM1$trial_num = rep(1:160)

output1 = prop_correct_cued(JAM1, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)
JAM2 = subset(JAM,
              JAM$Condition.Number == 10)

JAM2 = JAM2[order(JAM2$Username), ]

JAM2$trial_num = rep(1:160)

output2 = prop_correct_cued(JAM2, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

JAM3 = subset(JAM,
              JAM$Condition.Number == 11)

JAM3 = JAM3[order(JAM3$Username), ]

JAM3$trial_num = rep(1:160)

output3 = prop_correct_cued(JAM3, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

JAM4 = subset(JAM,
              JAM$Condition.Number == 12)

JAM4 = JAM4[order(JAM4$Username), ]

JAM4$trial_num = rep(1:160)

output4 = prop_correct_cued(JAM4, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

##Put all the JAM output back together
output1a = output1$DF_Scored
output2a = output2$DF_Scored
output3a = output3$DF_Scored
output4a = output4$DF_Scored

JAM_Scored = rbind(output1a, output2a, output3a, output4a)

####Okay, now for the read control####
Study$Response.Response = tolower(Study$Response.Response)
Study$Stimuli.Answer = tolower(Study$Stimuli.Answer)

##There are technically 4 different datasets, so I guess I need to subset these before scoring:
unique(Study$Condition.Number)

Study1 = subset(Study,
              Study$Condition.Number == 1)

Study1 = Study1[order(Study1$Username), ]

Study1$trial_num = rep(1:160)

output1 = prop_correct_cued(Study1, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)
Study2 = subset(Study,
              Study$Condition.Number == 2)

Study2 = Study2[order(Study2$Username), ]

Study2$trial_num = rep(1:160)

output2 = prop_correct_cued(Study2, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

Study3 = subset(Study,
              Study$Condition.Number == 3)

Study3 = Study3[order(Study3$Username), ]

Study3$trial_num = rep(1:160)

output3 = prop_correct_cued(Study3, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

Study4 = subset(Study,
              Study$Condition.Number == 4)

Study4 = Study4[order(Study4$Username), ]

Study4$trial_num = rep(1:160)

#write.csv(Study4, file = "test.csv", row.names = F)

output4 = prop_correct_cued(Study4, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

##Put all the Study output back together
output1a = output1$DF_Scored
output2a = output2$DF_Scored
output3a = output3$DF_Scored
output4a = output4$DF_Scored

#write.csv(output4a, file = "test.csv", row.names = F)

Study_Scored = rbind(output1a, output2a, output3a, output4a)

####Write all the output to .csv####
#write.csv(Study_Scored, file = "Read.csv", row.names = F)
#write.csv(JAM_Scored, file = "JAM.csv", row.names = F)
#write.csv(JOL_Scored, file = "JOL.csv", row.names = F)
