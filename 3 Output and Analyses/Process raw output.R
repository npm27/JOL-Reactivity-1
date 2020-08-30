####This script will be used to read in everything and set data up for processing####
##Start by gathering all of the data
#JOL and Frequency Judgments
setwd("C:/Users/nickm.000/Documents/JOL reactivity/3 Output/EX 1/Raw Collector Judgments") #get the correct directory

#Get the files names
files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$Username))

#Now do the study only condition
setwd('..')
setwd("C:/Users/nickm.000/Documents/JOL reactivity/3 Output/EX 1/Raw Collector Study") #get the correct directory

#Get the files names
files2 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat2 = do.call(rbind, lapply(files2, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat2$Username))

#Now do the vowel counting task
setwd('..')
setwd("C:/Users/nickm.000/Documents/JOL reactivity/3 Output/EX 1/Raw Collector Vowel") #get the correct directory

#Get the files names
files3 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat4 = do.call(rbind, lapply(files3, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat4$Username))

#Now do the relational task
setwd('..')
setwd("C:/Users/nickm.000/Documents/JOL reactivity/3 Output/EX 1/Raw Collector Relational Study") #get the correct directory

#Get the files names
files4 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat5 = do.call(rbind, lapply(files4, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat5$Username))

#Now do the second JOL group
setwd('..')
setwd("C:/Users/nickm.000/Documents/JOL reactivity/3 Output/EX 1/Raw Collector JOLs part 2") #get the correct directory

#Get the files names
files5 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat6 = do.call(rbind, lapply(files5, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat6$Username))

#Now do the third JOL group
setwd('..')
setwd("C:/Users/nickm.000/Documents/JOL reactivity/3 Output/EX 1/Raw Collector JOLs part 3") #get the correct directory

#Get the files names
files6 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat7 = do.call(rbind, lapply(files6, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat7$Username))

#Now do the third study only condition (Ex 1, pairs with JOL 3)
setwd('..')
setwd("C:/Users/nickm.000/Documents/JOL reactivity/3 Output/EX 1/Raw Collector Study part 3") #get the correct directory

#Get the files names
files7 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat8 = do.call(rbind, lapply(files7, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat8$Username))

##Now do the study condition for Ex 2
setwd('..')
setwd("C:/Users/nickm.000/Documents/JOL reactivity/3 Output/EX 1/Raw Collector Study part 2") #get the correct directory

#Get the files names
files8 = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat9 = do.call(rbind, lapply(files8, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat9$Username))

#Now move back to the 3 output folder
#This is where I'll store the combined final output for scoring
setwd('..')

####Clean up the data files####
##Let's separate out the frequency judgment data from the JOLs
dat3 = subset(dat,
              dat$Condition.Number > 8) ##Frequency
dat = subset(dat,
             dat$Condition.Number < 9) ##JOLs

##Drop unused columns
dat = dat[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:32,  34)] #JOL data
dat2 = dat2[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:33)] #Study condition
dat3 = dat3[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:32,  34)] #Frequency condition
dat4 = dat4[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:32,  34)] #Vowel condition
dat5 = dat5[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:33)] #Relational Study
dat6 = dat6[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:32,  34)] #JOL data for group 2
dat7 = dat7[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:32,  34)] #JOL data for group 3
dat8 = dat8[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:33)] #Study condition group 3
dat9 = dat9[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:33)] #Study condition group 2

#Next, remove buffer trials
dat = subset(dat,
             dat$Stimuli.Stimuli.Notes != "Buffer")
dat2 = subset(dat2,
             dat2$Stimuli.Stimuli.Notes != "Buffer")
dat3 = subset(dat3,
             dat3$Stimuli.Stimuli.Notes != "Buffer")
dat4 = subset(dat4,
              dat4$Stimuli.Stimuli.Notes != "Buffer")
dat5 = subset(dat5,
              dat5$Stimuli.Stimuli.Notes != "Buffer")
dat6 = subset(dat6,
              dat6$Stimuli.Stimuli.Notes != "Buffer")
dat7 = subset(dat7,
              dat7$Stimuli.Stimuli.Notes != "Buffer")
dat8 = subset(dat8,
              dat8$Stimuli.Stimuli.Notes != "Buffer")
dat9 = subset(dat9,
              dat9$Stimuli.Stimuli.Notes != "Buffer")

#Now remove instruction trials
dat = subset(dat,
             dat$Procedure.Trial.Type != "Instruct")
dat2 = subset(dat2,
             dat2$Procedure.Trial.Type != "Instruct")
dat3 = subset(dat3,
             dat3$Procedure.Trial.Type != "Instruct")
dat4 = subset(dat4,
              dat4$Procedure.Trial.Type != "Instruct")
dat5 = subset(dat5,
              dat5$Procedure.Trial.Type != "Instruct")
dat6 = subset(dat6,
              dat6$Procedure.Trial.Type != "Instruct")
dat7 = subset(dat7,
              dat7$Procedure.Trial.Type != "Instruct")
dat8 = subset(dat8,
              dat8$Procedure.Trial.Type != "Instruct")
dat9 = subset(dat9,
              dat9$Procedure.Trial.Type != "Instruct")

#Now remove filler task
dat = subset(dat,
             dat$Procedure.Trial.Type != "FreeRecall")
dat2 = subset(dat2,
             dat2$Procedure.Trial.Type != "FreeRecall")
dat3 = subset(dat3,
             dat3$Procedure.Trial.Type != "FreeRecall")
dat4 = subset(dat4,
              dat4$Procedure.Trial.Type != "FreeRecall")
dat5 = subset(dat5,
              dat5$Procedure.Trial.Type != "FreeRecall")
dat6 = subset(dat6,
              dat6$Procedure.Trial.Type != "FreeRecall")
dat7 = subset(dat7,
              dat7$Procedure.Trial.Type != "FreeRecall")
dat8 = subset(dat8,
              dat8$Procedure.Trial.Type != "FreeRecall")
dat9 = subset(dat9,
              dat9$Procedure.Trial.Type != "FreeRecall")

####Set the data up for scoring####
#Start by subsetting out the recall and JOL data for each dataset
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

##Now do the frequency judgments
#Start by subsetting out the recall and JOL data for each dataset
dat3.FREQ = subset(dat3,
                 dat3$Procedure.Trial.Type == "FREQ")
dat3.Recall = subset(dat3,
                    dat3$Procedure.Trial.Type == "Test")

#get JOLs and Recall in the same order
dat3.FREQ = dat3.FREQ[order(dat3.FREQ$Stimuli.Cue), ]
dat3.FREQ = dat3.FREQ[order(dat3.FREQ$Condition.Number), ]
dat3.FREQ = dat3.FREQ[order(dat3.FREQ$Stimuli.Shuffle), ]

dat3.Recall = dat3.Recall[order(dat3.Recall$Stimuli.Cue), ]
dat3.Recall = dat3.Recall[order(dat3.Recall$Condition.Number), ]
dat3.Recall = dat3.Recall[order(dat3.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat3.R = dat3.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
FREQ = cbind(dat3.FREQ, dat3.R)
FREQ = FREQ[ , -c(11, 13:14)]

colnames(FREQ)[11] = "FREQ.RT"

FREQ = FREQ[ , -c(9:10)]
FREQ = FREQ[ , -2]

##Now do the Vowel Counting
#Start by subsetting out the recall and JOL data for each dataset
dat4.VC = subset(dat4,
                   dat4$Procedure.Trial.Type == "VC")
dat4.Recall = subset(dat4,
                     dat4$Procedure.Trial.Type == "Test")

#get JOLs and Recall in the same order
dat4.VC = dat4.VC[order(dat4.VC$Stimuli.Cue), ]
dat4.VC = dat4.VC[order(dat4.VC$Condition.Number), ]
dat4.VC = dat4.VC[order(dat4.VC$Stimuli.Shuffle), ]

dat4.Recall = dat4.Recall[order(dat4.Recall$Stimuli.Cue), ]
dat4.Recall = dat4.Recall[order(dat4.Recall$Condition.Number), ]
dat4.Recall = dat4.Recall[order(dat4.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat4.R = dat4.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
VC = cbind(dat4.VC, dat4.R)
VC = VC[ , -c(11, 13:14)]

colnames(VC)[11] = "VC.RT"

VC = VC[ , -c(9:10)]
VC = VC[ , -2]

##Now for the relational study task
#Start by subsetting out the recall and study trials for each dataset
dat5.Study = subset(dat5,
                    dat5$Procedure.Trial.Type == "Study")
dat5.Recall = subset(dat5,
                     dat5$Procedure.Trial.Type == "Test")

#get Study and Recall in the same order
dat5.Study = dat5.Study[order(dat5.Study$Stimuli.Cue), ]
dat5.Study = dat5.Study[order(dat5.Study$Condition.Number), ]
dat5.Study = dat5.Study[order(dat5.Study$Stimuli.Shuffle), ]

dat5.Recall = dat5.Recall[order(dat5.Recall$Stimuli.Cue), ]
dat5.Recall = dat5.Recall[order(dat5.Recall$Condition.Number), ]
dat5.Recall = dat5.Recall[order(dat5.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat5.R = dat5.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
Study.RL = cbind(dat5.Study, dat5.R)
Study.RL = Study.RL[ , -c(11, 13:14)]

colnames(Study.RL)[11] = "Study.RT"

Study.RL = Study.RL[ , -c(9:10)]
Study.RL = Study.RL[ , -2]

##Now do the second JOL comparison
#Start by subsetting out the recall and JOL data for each dataset
dat6.JOL = subset(dat6,
                 dat6$Procedure.Trial.Type == "JOL")
dat6.Recall = subset(dat6,
                    dat6$Procedure.Trial.Type == "Test")

#get JOLs and Recall in the same order
dat6.JOL = dat6.JOL[order(dat6.JOL$Stimuli.Cue), ]
dat6.JOL = dat6.JOL[order(dat6.JOL$Condition.Number), ]
dat6.JOL = dat6.JOL[order(dat6.JOL$Stimuli.Shuffle), ]

dat6.Recall = dat6.Recall[order(dat6.Recall$Stimuli.Cue), ]
dat6.Recall = dat6.Recall[order(dat6.Recall$Condition.Number), ]
dat6.Recall = dat6.Recall[order(dat6.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat6.R = dat6.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
JOL2 = cbind(dat6.JOL, dat6.R)
JOL2 = JOL2[ , -c(11, 13:14)]

colnames(JOL2)[11] = "JOL.RT"

JOL2 = JOL2[ , -c(9:10)]
JOL2 = JOL2[ , -2]

##Now do the third JOL comparison
#Start by subsetting out the recall and JOL data for each dataset
dat7.JOL = subset(dat7,
                  dat7$Procedure.Trial.Type == "JOL")
dat7.Recall = subset(dat7,
                     dat7$Procedure.Trial.Type == "Test")

#get JOLs and Recall in the same order
dat7.JOL = dat7.JOL[order(dat7.JOL$Stimuli.Cue), ]
dat7.JOL = dat7.JOL[order(dat7.JOL$Condition.Number), ]
dat7.JOL = dat7.JOL[order(dat7.JOL$Stimuli.Shuffle), ]

dat7.Recall = dat7.Recall[order(dat7.Recall$Stimuli.Cue), ]
dat7.Recall = dat7.Recall[order(dat7.Recall$Condition.Number), ]
dat7.Recall = dat7.Recall[order(dat7.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat7.R = dat7.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
JOL3 = cbind(dat7.JOL, dat7.R)
JOL3 = JOL3[ , -c(11, 13:14)]

colnames(JOL3)[11] = "JOL.RT"

JOL3 = JOL3[ , -c(9:10)]
JOL3 = JOL3[ , -2]

##Now do the same for the study 3 condition
#Start by subsetting out the recall and study trials for each dataset
dat8.Study = subset(dat8,
                    dat8$Procedure.Trial.Type == "Study")
dat8.Recall = subset(dat8,
                     dat8$Procedure.Trial.Type == "Test")

#get Study and Recall in the same order
dat8.Study = dat8.Study[order(dat8.Study$Stimuli.Cue), ]
dat8.Study = dat8.Study[order(dat8.Study$Condition.Number), ]
dat8.Study = dat8.Study[order(dat8.Study$Stimuli.Shuffle), ]

dat8.Recall = dat8.Recall[order(dat8.Recall$Stimuli.Cue), ]
dat8.Recall = dat8.Recall[order(dat8.Recall$Condition.Number), ]
dat8.Recall = dat8.Recall[order(dat8.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat8.R = dat8.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
Study3 = cbind(dat8.Study, dat8.R)
Study3 = Study3[ , -c(11, 13:14)]

colnames(Study3)[11] = "Study.RT"

Study3 = Study3[ , -c(9:10)]
Study3 = Study3[ , -2]

##Now do the same for the study 2 condition
#Start by subsetting out the recall and study trials for each dataset
dat9.Study = subset(dat9,
                    dat9$Procedure.Trial.Type == "Study")
dat9.Recall = subset(dat9,
                     dat9$Procedure.Trial.Type == "Test")

#get Study and Recall in the same order
dat9.Study = dat9.Study[order(dat9.Study$Stimuli.Cue), ]
dat9.Study = dat9.Study[order(dat9.Study$Condition.Number), ]
dat9.Study = dat9.Study[order(dat9.Study$Stimuli.Shuffle), ]

dat9.Recall = dat9.Recall[order(dat9.Recall$Stimuli.Cue), ]
dat9.Recall = dat9.Recall[order(dat9.Recall$Condition.Number), ]
dat9.Recall = dat9.Recall[order(dat9.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat9.R = dat9.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
Study2 = cbind(dat9.Study, dat9.R)
Study2 = Study2[ , -c(11, 13:14)]

colnames(Study2)[11] = "Study.RT"

Study2 = Study2[ , -c(9:10)]
Study2 = Study2[ , -2]

####Score the recall data####
library(lrd)

##Start with the JOL data
JOL.key = tolower(JOL$Stimuli.Answer)
JOL.Response = tolower(JOL$Response.Response)
ID = JOL$Username

match1 = percent_match(JOL.Response, key = JOL.key, id = ID)
score_recall(match1, set.cutoff = 0.75)

scored1 = read.csv("output.csv")

JOL$Recall_Score = scored1$scored * 100

##Now do the study condition
Study.key = tolower(Study$Stimuli.Answer)
Study.Response = tolower(Study$Response.Response)
ID = Study$Username

match2 = percent_match(Study.Response, key = Study.key, id = ID)
score_recall(match2, set.cutoff = 0.75)

scored2 = read.csv("output.csv")

Study$Recall_Score = scored2$scored * 100

##Now do the Frequency condition
FREQ.key = tolower(FREQ$Stimuli.Answer)
FREQ.Response = tolower(FREQ$Response.Response)
ID = FREQ$Username

match3 = percent_match(FREQ.Response, key = FREQ.key, id = ID)
score_recall(match3, set.cutoff = 0.75)

scored3 = read.csv("output.csv")

FREQ$Recall_Score = scored3$scored * 100

##Now do the Vowel Counting Task
VC.key = tolower(VC$Stimuli.Answer)
VC.Response = tolower(VC$Response.Response)
ID = VC$Username

match4 = percent_match(VC.Response, key = VC.key, id = ID)
score_recall(match4, set.cutoff = 0.75)

scored4 = read.csv("output.csv")

VC$Recall_Score = scored4$scored * 100

##Now do the RELATIONAL study conditoin
Study.RL.key = tolower(Study.RL$Stimuli.Answer)
Study.RL.Response = tolower(Study.RL$Response.Response)
ID = Study.RL$Username

match5 = percent_match(Study.RL.Response, key = Study.RL.key, id = ID)
score_recall(match5, set.cutoff = 0.75)

scored5 = read.csv("output.csv")

Study.RL$Recall_Score = scored5$scored * 100

##Now do the second JOL comparison
JOL2.key = tolower(JOL2$Stimuli.Answer)
JOL2.Response = tolower(JOL2$Response.Response)
ID = JOL2$Username

match6 = percent_match(JOL2.Response, key = JOL2.key, id = ID)
score_recall(match6, set.cutoff = 0.75)

scored6 = read.csv("output.csv")

JOL2$Recall_Score = scored6$scored * 100

##Now do the third JOL comparison
JOL3.key = tolower(JOL3$Stimuli.Answer)
JOL3.Response = tolower(JOL3$Response.Response)
ID = JOL3$Username

match7 = percent_match(JOL3.Response, key = JOL3.key, id = ID)
score_recall(match7, set.cutoff = 0.75)

scored7 = read.csv("output.csv")

JOL3$Recall_Score = scored7$scored * 100

##Now do the study 3 condition
Study3.key = tolower(Study3$Stimuli.Answer)
Study3.Response = tolower(Study3$Response.Response)
ID = Study3$Username

match8 = percent_match(Study3.Response, key = Study3.key, id = ID)
score_recall(match8, set.cutoff = 0.75)

scored8 = read.csv("output.csv")

Study3$Recall_Score = scored8$scored * 100

##Now do the study 2 condition
Study2.key = tolower(Study2$Stimuli.Answer)
Study2.Response = tolower(Study2$Response.Response)
ID = Study2$Username

match9 = percent_match(Study2.Response, key = Study2.key, id = ID)
score_recall(match9, set.cutoff = 0.75)

scored9 = read.csv("output.csv")

Study2$Recall_Score = scored9$scored * 100

####Write combined raw output to data file####
write.csv(JOL, file = "Scored Output/JOL.csv", row.names = F)
write.csv(Study, file = "Scored Output/Study.csv", row.names = F)
write.csv(FREQ, file = "Scored Output/Freq.csv", row.names = F)
write.csv(VC, file = "Scored Output/VC.csv", row.names = F)
write.csv(Study.RL, file = "Scored Output/Study.RL.csv", row.names = F)
write.csv(JOL2, file = "Scored Output/JOL2.csv", row.names = F)
write.csv(JOL3, file = "Scored Output/JOL3.csv", row.names = F)
write.csv(Study3, file = "Scored Output/Study3.csv", row.names = F)
write.csv(Study2, file = "Scored Output/Study2.csv", row.names = F)