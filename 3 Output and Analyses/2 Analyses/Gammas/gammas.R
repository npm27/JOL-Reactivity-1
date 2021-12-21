####Set up####
##libraries
library(reshape)
library(Hmisc)

##load in the data
ex1 = read.csv("Data/JOL ex1.csv")
ex2 = read.csv("Data/JOL ex2.csv")
ex3 = read.csv("Data/JOL ex3.csv")
ex4 = read.csv("Data/JOL ex4.csv")

##clean the data
ex1$Response.JOL = as.numeric(ex1$Response.JOL)
ex2$Response.JOL = as.numeric(ex2$Response.JOL)
ex3$Response.JOL = as.numeric(ex3$Response.JOL)
ex4$Response.JOL = as.numeric(ex4$Response.JOL)

ex1$Response.JOL[ex1$Response.JOL > 100] = NA
ex2$Response.JOL[ex2$Response.JOL > 100] = NA
ex3$Response.JOL[ex3$Response.JOL > 100] = NA
ex4$Response.JOL[ex4$Response.JOL > 100] = NA

##drop unneeded columns and remove NAs
ex1 = ex1[ , c(1, 7, 9, 13)]
ex1 = na.omit(ex1)

ex2 = ex2[ , c(2, 8, 10, 15)]
ex2 = na.omit(ex2)

ex3 = ex3[ , c(1, 7, 9, 13)]
ex3 = na.omit(ex3)

ex4 = ex4[ , c(1, 7, 9, 13)]
ex4 = na.omit(ex4)

##fix column names
colnames(ex1)[2] = "Direction"
colnames(ex2)[2] = "Direction"
colnames(ex3)[2] = "Direction"
colnames(ex4)[2] = "Direction"

####compute gammas####
###Ex 1
ex1_F = subset(ex1,
               ex1$Direction == "F")
ex1_B = subset(ex1,
               ex1$Direction == "B")
ex1_S = subset(ex1,
               ex1$Direction == "S")
ex1_U = subset(ex1,
               ex1$Direction == "U")

##Forward
empty = data.frame()

for (i in unique(ex1_F$Username)){
  
  temp = subset(ex1_F, ex1_F$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex1_F = empty

##Backward
empty = data.frame()

for (i in unique(ex1_B$Username)){
  
  temp = subset(ex1_B, ex1_B$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex1_B = empty

##Symmetrical
empty = data.frame()

for (i in unique(ex1_S$Username)){
  
  temp = subset(ex1_S, ex1_S$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex1_S = empty

##Unrelated
empty = data.frame()

for (i in unique(ex1_U$Username)){
  
  temp = subset(ex1_U, ex1_U$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex1_U = empty

###Ex 2
ex2_F = subset(ex2,
               ex2$Direction == "F")
ex2_B = subset(ex2,
               ex2$Direction == "B")
ex2_S = subset(ex2,
               ex2$Direction == "S")
ex2_U = subset(ex2,
               ex2$Direction == "U")

##Forward
empty = data.frame()

for (i in unique(ex2_F$Sub.ID)){
  
  temp = subset(ex2_F, ex2_F$Sub.ID == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Scored, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex2_F = empty

##Backward
empty = data.frame()

for (i in unique(ex2_B$Sub.ID)){
  
  temp = subset(ex2_B, ex2_B$Sub.ID == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Scored, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex2_B = empty

##Symmetrical
empty = data.frame()

for (i in unique(ex2_S$Sub.ID)){
  
  temp = subset(ex2_S, ex2_S$Sub.ID == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Scored, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex2_S = empty

##Unrelated
empty = data.frame()

for (i in unique(ex2_U$Sub.ID)){
  
  temp = subset(ex2_U, ex2_U$Sub.ID == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Scored, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex2_U = empty

###Ex 3
ex3_F = subset(ex3,
               ex3$Direction == "F")
ex3_B = subset(ex3,
               ex3$Direction == "B")
ex3_S = subset(ex3,
               ex3$Direction == "S")
ex3_U = subset(ex3,
               ex3$Direction == "U")

##Forward
empty = data.frame()

for (i in unique(ex3_F$Username)){
  
  temp = subset(ex3_F, ex3_F$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex3_F = empty

##Backward
empty = data.frame()

for (i in unique(ex3_B$Username)){
  
  temp = subset(ex3_B, ex3_B$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex3_B = empty

##Symmetrical
empty = data.frame()

for (i in unique(ex3_S$Username)){
  
  temp = subset(ex3_S, ex3_S$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex3_S = empty

##Unrelated
empty = data.frame()

for (i in unique(ex3_U$Username)){
  
  temp = subset(ex3_U, ex3_U$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex3_U = empty

###Ex 4
ex4_F = subset(ex4,
               ex4$Direction == "F")
ex4_B = subset(ex4,
               ex4$Direction == "B")
ex4_S = subset(ex4,
               ex4$Direction == "S")
ex4_U = subset(ex4,
               ex4$Direction == "U")

##Forward
empty = data.frame()

for (i in unique(ex4_F$Username)){
  
  temp = subset(ex4_F, ex4_F$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex4_F = empty

##Backward
empty = data.frame()

for (i in unique(ex4_B$Username)){
  
  temp = subset(ex4_B, ex4_B$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex4_B = empty

##Symmetrical
empty = data.frame()

for (i in unique(ex4_S$Username)){
  
  temp = subset(ex4_S, ex4_S$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex4_S = empty

##Unrelated
empty = data.frame()

for (i in unique(ex4_U$Username)){
  
  temp = subset(ex4_U, ex4_U$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_ex4_U = empty

####Get means for Table A5####
mean(Gammas_ex1_F$g, na.rm = T);mean(Gammas_ex1_B$g, na.rm = T);mean(Gammas_ex1_S$g, na.rm = T);mean(Gammas_ex1_U$g, na.rm = T)
mean(Gammas_ex2_F$g, na.rm = T);mean(Gammas_ex2_B$g, na.rm = T);mean(Gammas_ex2_S$g, na.rm = T);mean(Gammas_ex2_U$g, na.rm = T)
mean(Gammas_ex3_F$g, na.rm = T);mean(Gammas_ex3_B$g, na.rm = T);mean(Gammas_ex3_S$g, na.rm = T);mean(Gammas_ex3_U$g, na.rm = T)
mean(Gammas_ex4_F$g, na.rm = T);mean(Gammas_ex4_B$g, na.rm = T);mean(Gammas_ex4_S$g, na.rm = T);mean(Gammas_ex4_U$g, na.rm = T)

####Get the 95% CIs####
###ex1
(sd(Gammas_ex1_F$g, na.rm = T) / sqrt(nrow(Gammas_ex1_F))) * 1.96
(sd(Gammas_ex1_B$g, na.rm = T) / sqrt(nrow(Gammas_ex1_B))) * 1.96
(sd(Gammas_ex1_S$g, na.rm = T) / sqrt(nrow(Gammas_ex1_S))) * 1.96
(sd(Gammas_ex1_U$g, na.rm = T) / sqrt(nrow(Gammas_ex1_U))) * 1.96

###ex2
(sd(Gammas_ex2_F$g, na.rm = T) / sqrt(nrow(Gammas_ex2_F))) * 1.96
(sd(Gammas_ex2_B$g, na.rm = T) / sqrt(nrow(Gammas_ex2_B))) * 1.96
(sd(Gammas_ex2_S$g, na.rm = T) / sqrt(nrow(Gammas_ex2_S))) * 1.96
(sd(Gammas_ex2_U$g, na.rm = T) / sqrt(nrow(Gammas_ex2_U))) * 1.96

###ex3
(sd(Gammas_ex3_F$g, na.rm = T) / sqrt(nrow(Gammas_ex3_F))) * 1.96
(sd(Gammas_ex3_B$g, na.rm = T) / sqrt(nrow(Gammas_ex3_B))) * 1.96
(sd(Gammas_ex3_S$g, na.rm = T) / sqrt(nrow(Gammas_ex3_S))) * 1.96
(sd(Gammas_ex3_U$g, na.rm = T) / sqrt(nrow(Gammas_ex3_U))) * 1.96

###ex4
(sd(Gammas_ex4_F$g, na.rm = T) / sqrt(nrow(Gammas_ex4_F))) * 1.96
(sd(Gammas_ex4_B$g, na.rm = T) / sqrt(nrow(Gammas_ex4_B))) * 1.96
(sd(Gammas_ex4_S$g, na.rm = T) / sqrt(nrow(Gammas_ex4_S))) * 1.96
(sd(Gammas_ex4_U$g, na.rm = T) / sqrt(nrow(Gammas_ex4_U))) * 1.96
