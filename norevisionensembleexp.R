# first read the real and synthetic csvs
library(readr)
source("hatexploration.r")

# mev is metaefdtvfdt. syn is synthetic (with drift), real is uci streams, realshuf is uci shuffled

mevsyn <- read_csv("/home/c/exp_dir_results/output/outmetaefdtvfdtsynthetic")
mevsynE <- mevsyn[mevsyn$X2=="E",]
mevsynE <- mevsynE[-c(12,3,4),]
# drop line 12 with no revision efdt, as that is now a separate bunch of experiments

mevreal <- read_csv("/home/c/exp_dir_results/output/outmetaefdtvfdtreal")
mevrealE <- mevreal[mevreal$X2=="E",]
mevrealE <- mevrealE[-c(12,3,4),]

mevrealshuf <- read_csv("/home/c/exp_dir_results/output/outmetaefdtvfdtrealshuf")
mevrealshufE <- mevrealshuf[mevrealshuf$X2=="E",]
mevrealshufE <- mevrealshufE[-c(12,3,4),]

# Okay, now the noRevision results have all been taken out.

# Let's also remove the repetitive datasets
mevsynE <- mevsynE[,-c(8,11,14,15,16,22,23,24)]



# note: already tested and showed that levbag without levbag equals ozabag
#=============================================================================
# Now, redo tables and analysis without noRevisionEFDT
#=============================================================================

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn2.tex", c(17,16)) # OzaBagADWIN
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn3.tex", c(13,12)) # OnlineSmoothBoost
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn4.tex", c(19,18)) # OzaBoost
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn5.tex", c(9,8)) # LevBagNoADWIN

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn6.tex", c(2,1)) # ARF Optimised Parametrization
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn7.tex", c(21,20)) #  OzaBoostAdwin
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn8.tex", c(23,22)) # Plain
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn9.tex", c(15,14)) # OzaBag
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn10.tex", c(4,3)) #  ADOB

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn11.tex", c(11,10)) #  LevBag

#=============================================================================


compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal2.tex", c(17,16)) # OzaBagADWIN
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal3.tex", c(13,12)) # OnlineSmoothBoost
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal4.tex", c(19,18)) # OzaBoost
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal5.tex", c(9,8)) # LevBagNoADWIN

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal6.tex", c(2,1)) # ARF Optimised Parametrization
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal7.tex", c(21,20)) #  OzaBoostAdwin
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal8.tex", c(23,22)) # Plain
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal9.tex", c(15,14)) # OzaBag
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal10.tex", c(4,3)) #  ADOB

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal11.tex", c(11,10)) #  LevBag


#==============================================================================

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf2.tex", c(17,16)) # OzaBagADWIN
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf3.tex", c(13,12)) # OnlineSmoothBoost
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf4.tex", c(19,18)) # OzaBoost
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf5.tex", c(9,8)) # LevBagNoADWIN

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf6.tex", c(2,1)) # ARF Optimised Parametrization
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf7.tex", c(21,20)) #  OzaBoostAdwin
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf8.tex", c(23,22)) # Plain
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf9.tex", c(15,14)) # OzaBag
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf10.tex", c(4,3)) #  ADOB

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf11.tex", c(11,10)) #  LevBag



#==============================================================================
# Now let's do ranks
#==============================================================================




rankTableRealStandardE <- as.data.frame(apply(mevrealE[,-c(1:2)],2, function(x){rank(x, ties.method="average")}))
rankTableRealShufE <- as.data.frame(apply(mevrealshufE[,-c(1:2)],2, function(x){rank(x, ties.method="average")}))
#rankTableSyntheticShufE <- as.data.frame(apply(mevsynE[c(13,14),-c(1:2)],2, function(x){rank(x, ties.method="average")}))
rankTableSyntheticShufE <- as.data.frame(apply(mevsynE[,-c(1:2)],2, function(x){rank(x, ties.method="average")}))

#rankTableSyntheticShufE <- rankTableSyntheticShufE[c(12,13,14),]

rankTableRealStandardE$mean = rowMeans(rankTableRealStandardE)
rankTableRealShufE$mean = rowMeans(rankTableRealShufE)
rankTableSyntheticShufE$mean = rowMeans(rankTableSyntheticShufE)

ranksRealStandardE <- cbind.data.frame(mevrealE$X1, rankTableRealStandardE$mean)
ranksRealShufE <- cbind.data.frame(mevrealshufE$X1, rankTableRealShufE$mean)
ranksSyntheticShufE <- cbind.data.frame(mevsynE$X1, rankTableSyntheticShufE$mean)


rankTableRealStandardE$wins = rowMeans(rankTableRealStandardE[,-(1:2)])
rankTableRealShufE$wins = rowMeans(rankTableRealShufE[,-(1:2)])
rankTableSyntheticShufE$wins = rowMeans(rankTableSyntheticShufE[,-(1:2)])

file.show("/home/c/papers/ensemble/mevrealshuf3.tex")

#==============================================================================
#==============================================================================
#==============================================================================
# meta efdt no revision
#==============================================================================
#==============================================================================
#==============================================================================

menrsyn <- read_csv("/home/c/exp_dir_results/output/outmetaeftdnorevisionsynthetic")
menrsynE <- menrsyn[menrsyn$X2=="E",]

menrreal <- read_csv("/home/c/exp_dir_results/output/outmetaeftdnorevisionreal")
menrrealE <- menrreal[menrreal$X2=="E",]

menrrealshuf <- read_csv("/home/c/exp_dir_results/output/outmetaeftdnorevisionrealshuf")
menrrealshufE <- menrrealshuf[menrrealshuf$X2=="E",]

# Let's also remove the repetitive datasets
menrsynE <- menrsynE[,-c(8,11,14,15,16,22,23,24)]

jointsynE <- rbind(mevsynE, menrsynE)

compareTwo(jointsynE,"/home/c/papers/ensemble/menrsyn2.tex", c(17,32)) # OzaBagADWIN
compareTwo(jointsynE,"/home/c/papers/ensemble/menrsyn3.tex", c(11,29)) # LevBag

# There isn't sufficient evidence to show EFDT NoRevision has an effect


rankTableRealStandardE <- as.data.frame(apply(menrrealE[,-c(1:2)],2, function(x){rank(x, ties.method="average")}))
rankTableRealShufE <- as.data.frame(apply(menrrealshufE[,-c(1:2)],2, function(x){rank(x, ties.method="average")}))
#rankTableSyntheticShufE <- as.data.frame(apply(menrsynE[c(13,14),-c(1:2)],2, function(x){rank(x, ties.method="average")}))
rankTableSyntheticShufE <- as.data.frame(apply(menrsynE[,-c(1:2)],2, function(x){rank(x, ties.method="average")}))

#rankTableSyntheticShufE <- rankTableSyntheticShufE[c(12,13,14),]

rankTableRealStandardE$mean = rowMeans(rankTableRealStandardE)
rankTableRealShufE$mean = rowMeans(rankTableRealShufE)
rankTableSyntheticShufE$mean = rowMeans(rankTableSyntheticShufE)

ranksRealStandardE <- cbind.data.frame(menrrealE$X1, rankTableRealStandardE$mean)
ranksRealShufE <- cbind.data.frame(menrrealshufE$X1, rankTableRealShufE$mean)
ranksSyntheticShufE <- cbind.data.frame(menrsynE$X1, rankTableSyntheticShufE$mean)


rankTableRealStandardE$wins = rowMeans(rankTableRealStandardE[,-(1:2)])
rankTableRealShufE$wins = rowMeans(rankTableRealShufE[,-(1:2)])
rankTableSyntheticShufE$wins = rowMeans(rankTableSyntheticShufE[,-(1:2)])


# these are rank comparisons of EFDT-NoRev making bagging ensembles rise relative to boosting ensembles. 
# But in the rest of the paper you compare two at a time. So the noRev results will have to be added as new rows to existing
# tables. New comparisons will have to be done. Showing VFDT ensembles vs EFDTNoRev and showing an improvment over EFDT 
# should suffice to be convincing.

