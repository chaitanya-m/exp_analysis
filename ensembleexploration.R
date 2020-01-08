# first read the real and synthetic csvs
library(readr)
source("hatexploration.r")

mevsyn <- read_csv("/home/c/exp_dir_results/output/outmetaefdtvfdtsynthetic")
mevsynE <- mevsyn[mevsyn$X2=="E",]

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn1.tex", c(12,10)) # LevBag without levbag (should equal Oza)

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn2.tex", c(17,16)) # OzaBag
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn3.tex", c(13,11)) # LevBag
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn4.tex", c(19,18)) # OzaBagADWIN
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn5.tex", c(9,8)) # BOLE

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn6.tex", c(6,5)) # ADOB
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn7.tex", c(21,20)) # OzaBoost
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn8.tex", c(23,22)) # OzaBoostAdwin
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn9.tex", c(15,14)) # OnlineSmoothBoost
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn10.tex", c(4,3)) #  ARF 


#=============================================================================

mevreal <- read_csv("/home/c/exp_dir_results/output/outmetaefdtvfdtreal")
mevrealE <- mevreal[mevreal$X2=="E",]

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal1.tex", c(12,10)) # LevBag without levbag (should equal Oza)

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal2.tex", c(17,16)) # OzaBag
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal3.tex", c(13,11)) # LevBag
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal4.tex", c(19,18)) # OzaBagADWIN
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal5.tex", c(9,8)) # BOLE

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal6.tex", c(6,5)) # ADOB
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal7.tex", c(21,20)) # OzaBoost
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal8.tex", c(23,22)) # OzaBoostAdwin
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal9.tex", c(15,14)) # OnlineSmoothBoost
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal10.tex", c(4,3)) #  ARF 


#==============================================================================

mevrealshuf <- read_csv("/home/c/exp_dir_results/output/outmetaefdtvfdtrealshuf")
mevrealshufE <- mevrealshuf[mevrealshuf$X2=="E",]

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf1.tex", c(12,10)) # LevBag without levbag (should equal Oza)

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf2.tex", c(17,16)) # OzaBag
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf3.tex", c(13,11)) # LevBag
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf4.tex", c(19,18)) # OzaBagADWIN
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf5.tex", c(9,8)) # BOLE

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf6.tex", c(6,5)) # ADOB
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf7.tex", c(21,20)) # OzaBoost
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf8.tex", c(23,22)) # OzaBoostAdwin
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf9.tex", c(15,14)) # OnlineSmoothBoost
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf10.tex", c(4,3)) #  ARF 



#==============================================================================



outTableRealShuf <- read_csv("/home/c/exp_dir_results/output/outTableRealShuf.csv")
outTableSyntheticShuf <- read_csv("/home/c/exp_dir_results/output/outTableSyntheticShuf.csv")
outTableRealStandard <- read_csv("/home/c/exp_dir_results/output/outTableRealStandard.csv")

nrow(outTableRealShuf)

length(colnames(outTableRealShuf))
length(names(outTableRealShuf))

colnames(outTableRealShuf)[names(outTableRealShuf) == "X1"] <- "Learners"
colnames(outTableRealShuf)[names(outTableRealShuf) == "X2"] <- "Performance"

colnames(outTableRealStandard)[names(outTableRealStandard) == "X1"] <- "Learners"
colnames(outTableRealStandard)[names(outTableRealStandard) == "X2"] <- "Performance"

colnames(outTableSyntheticShuf)[names(outTableSyntheticShuf) == "X1"] <- "Learners"
colnames(outTableSyntheticShuf)[names(outTableSyntheticShuf) == "X2"] <- "Performance"

rshuf <- outTableRealShuf
rstan <- outTableRealStandard
sshuf <- outTableSyntheticShuf
# Just EFDT and an exponential shuffle
#rshuf <- outTableRealShuf[c(59,60,87,88),]
#rstan <- outTableRealStandard[c(59,60,87,88),]
#sshuf <- outTableSyntheticShuf[c(59,60,87,88),]


# # Just EFDT, VFDT, LevBag
# rshuf <- outTableRealShuf[c(25:28,87,88, 103, 104),]
# rstan <- outTableRealStandard[c(25:28,87,88, 103, 104),]
# sshuf <- outTableSyntheticShuf[c(25:28,87,88, 103, 104),]

# Just trees with most decay trees removed 
# outTableRealShuf <- outTableRealShuf[-c(57:74, 77:80, 1:56),]
# outTableRealStandard <- outTableRealStandard[-c(57:74, 77:80, 1:56),]
# outTableSyntheticShuf <- outTableSyntheticShuf[-c(57:74, 77:80, 1:56),]



# Add the additional results

#outTableReal <- outTableReal[-c(57:68),] # remove Decay

#outTableReal <- rbind(outTableReal, outTableReal2)
#outTableReal <- outTableReal[-c(77:78),] # remove HATBoost

# Let's rank columns by error, so we have a ranking of learners

# First for Error data





source("hatexploration.r")





sshufE <- sshuf[sshuf$Performance=="E",]

compareTwo(sshufE,"ens0.tex", c(46,48))

compareTwo(sshufE,"ens1.tex", c(11,10)) # BOLE
compareTwo(sshufE,"ens2.tex", c(14,13)) # LevBag
compareTwo(sshufE,"ens3.tex", c(17,16)) # SmoothBoost
compareTwo(sshufE,"ens4.tex", c(20,19)) # OzaBag
compareTwo(sshufE,"ens5.tex", c(23,22)) # OzaBagAdwin
compareTwo(sshufE,"ens6.tex", c(25,24)) # OzaBoost
compareTwo(sshufE,"ens7.tex", c(28,27)) # OzaBoostAdwin
compareTwo(sshufE,"ens8.tex", c(3,2)) # ADOB
compareTwo(sshufE,"ens9.tex", c(8,7)) # ARF
compareTwo(sshufE,"ens10.tex", c(6,5)) # ARF Parameterized




comp <- list()

comp[[1]] <- sshuf[c(48,46),] #just the rows to compare

comp[[2]] <- sshuf[c(6,7),] #just the rows to compare
comp[[3]] <- sshuf[c(10,11),] #just the rows to compare
comp[[4]] <- sshuf[c(10,11),] #just the rows to compare
comp[[5]] <- sshuf[c(13,14),] #just the rows to compare

comp[[6]] <- sshuf[c(16,17),] #just the rows to compare
comp[[7]] <- sshuf[c(19,20),] #just the rows to compare
comp[[8]] <- sshuf[c(22,23),] #just the rows to compare
comp[[9]] <- sshuf[c(24,25),] #just the rows to compare
comp[[10]] <- sshuf[c(27,28),] #just the rows to compare

for (index in 1:10){
  df <- comp[[index]]
  df$countWins <- apply(df, 1, function(x) length(which(x=="1")))
  comp[[index]]<-df
}








rankTableRealStandardE <- rstan[rstan$Performance=="E",]
rankTableRealShufE <- rshuf[rshuf$Performance=="E",]
rankTableSyntheticShufE <- sshuf[sshuf$Performance=="E",]


rankTableRealStandardE[,-(1:2)] <- apply(rankTableRealStandardE[,-(1:2)],2, function(x){rank(x, ties.method="average")})
rankTableRealShufE[,-(1:2)] <- apply(rankTableRealShufE[,-(1:2)],2, function(x){rank(x, ties.method="average")})
rankTableSyntheticShufE[,-(1:2)] <- apply(rankTableSyntheticShufE[,-(1:2)],2, function(x){rank(x, ties.method="average")})








rankTableRealStandardE$mean = rowMeans(rankTableRealStandardE[,-(1:2)])
rankTableRealShufE$mean = rowMeans(rankTableRealShufE[,-(1:2)])
rankTableSyntheticShufE$mean = rowMeans(rankTableSyntheticShufE[,-(1:2)])

rankTableRealStandardE$wins = rowMeans(rankTableRealStandardE[,-(1:2)])
rankTableRealShufE$wins = rowMeans(rankTableRealShufE[,-(1:2)])
rankTableSyntheticShufE$wins = rowMeans(rankTableSyntheticShufE[,-(1:2)])

ranksRealStandardE <- rankTableRealStandardE[,c("Learners", "mean")]
ranksRealShufE <- rankTableRealShufE[,c("Learners", "mean")]
ranksSyntheticShufE <- rankTableSyntheticShufE[,c("Learners", "mean")]


# Then for CPU time
# rankTableRealStandardT <- outTableRealStandard[outTableRealStandard$Performance=="T",]
# rankTableRealShufT <- outTableRealShuf[outTableRealShuf$Performance=="T",]
# rankTableSyntheticShufT <- outTableSyntheticShuf[outTableSyntheticShuf$Performance=="T",]
# 
# rankTableRealStandardT[,-(1:2)] <- apply(rankTableRealStandardT[,-(1:2)],2, function(x){rank(x, ties.method="average")})
# rankTableRealShufT[,-(1:2)] <- apply(rankTableRealShufT[,-(1:2)],2, function(x){rank(x, ties.method="average")})
# rankTableSyntheticShufT[,-(1:2)] <- apply(rankTableSyntheticShufT[,-(1:2)],2, function(x){rank(x, ties.method="average")})
# 
# rankTableRealStandardT$mean = rowMeans(rankTableRealStandardT[,-(1:2)])
# rankTableRealShufT$mean = rowMeans(rankTableRealShufT[,-(1:2)])
# rankTableSyntheticShufT$mean = rowMeans(rankTableSyntheticShufT[,-(1:2)])
# 


# ranksRealStandardT <- rankTableRealStandardT[,c("Learners", "mean")]
# ranksRealShufT <- rankTableRealShufT[,c("Learners", "mean")]
# ranksSyntheticShufT <- rankTableSyntheticShufT[,c("Learners", "mean")]



#write.csv(rankTableRealE[,c("Learners", "mean")],"rankTableRealE.csv")
#write.csv(outTableReal,"outTableReal.csv")








