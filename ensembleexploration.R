# first read the real and synthetic csvs
library(readr)
source("compareLearners.r")

# mev is metaefdtvfdt. syn is synthetic (with drift), real is uci streams, realshuf is uci shuffled

mevsyn <- read_csv("/home/c/exp_dir_results/output/outmetaefdtvfdtsynthetic")
mevsynE <- mevsyn[mevsyn$X2=="E",]
mevsynE <- mevsynE[-c(12,3,4),]

mevsynT <- mevsyn[mevsyn$X2=="T",]
mevsynT <- mevsynT[-c(12,3,4),]
# drop line 12 with no revision efdt, as that is now a separate bunch of experiments
# also drop old ARF with old MOA default parameters (MOA has new default parameters)

mevreal <- read_csv("/home/c/exp_dir_results/output/outmetaefdtvfdtreal")
mevrealE <- mevreal[mevreal$X2=="E",]
mevrealE <- mevrealE[-c(12,3,4),]

mevrealT <- mevreal[mevreal$X2=="T",]
mevrealT <- mevrealT[-c(12,3,4),]

mevrealshuf <- read_csv("/home/c/exp_dir_results/output/outmetaefdtvfdtrealshuf")
mevrealshufE <- mevrealshuf[mevrealshuf$X2=="E",]
mevrealshufE <- mevrealshufE[-c(12,3,4),]

mevrealshufT <- mevrealshuf[mevrealshuf$X2=="T",]
mevrealshufT <- mevrealshufT[-c(12,3,4),]

# Okay, now the noRevision results have all been taken out.

# Let's also remove the repetitive datasets
mevsynE <- mevsynE[,-c(8,11,14,15,16,22,23,24)]
mevsynT <- mevsynT[,-c(8,11,14,15,16,22,23,24)]



col1 <- colnames(mevsynE)[3:26]
col2 <- c("recurrent---agrawal","recurrent---randomtree","recurrent---sea", "recurrent---stagger", 
          "hyperplane---1", "hyperplane---2","hyperplane---3","hyperplane---4",
          "led---drift",
          "rbf---drift-1","rbf---drift-2","rbf---drift-3","rbf---drift-4",
          "waveform---drift",
          "recurrent---abrupt---222","recurrent---abrupt---322","recurrent---abrupt---332","recurrent---abrupt---333","recurrent---abrupt---334",
          "recurrent---abrupt---335","recurrent---abrupt---422","recurrent---abrupt---444","recurrent---abrupt---522","recurrent---abrupt---555")
#col3 <- rep(" \\", 24)
#col3

dfkeysyn <- data.frame(col1,col2)
dfkeysyn

write.table(dfkeysyn, "/home/c/papers/ensemble/syntheticStreamsKey.tex", quote=FALSE, col.names = FALSE, sep = ' & ', eol = " \\\\\n",)

colnames(mevsynE)[3:26] <- col2
colnames(mevsynT)[3:26] <- col2


col2real <- c("airlines", "aws---price-discretized", "chess", "covertype", "covpokelec", "fonts", 
          "hhar", "kdd", "localization", "miniboone", "nbaiot", "nswelec", "pamap2", 
          "poker", "pucrio", "sensor---home-activity", "sensor---CO-discretized", "skin", 
          "tnelec", "wisdm")

colnames(mevrealE)[3:22] <- col2real 
colnames(mevrealshufE)[3:22] <- col2real

colnames(mevrealT)[3:22] <- col2real 
colnames(mevrealshufT)[3:22] <- col2real


# note: already tested and showed that levbag without levbag equals ozabag
#=============================================================================
# Now, redo tables and analysis without noRevisionEFDT
#=============================================================================

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn0.tex", c(15,14)) # OzaBag
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn1.tex", c(17,16)) # OzaBagADWIN

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn2.tex", c(9,8)) # LevBagNoADWIN
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn3.tex", c(11,10)) #  LevBag

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn4.tex", c(2,1)) # ARF Optimised Parametrization

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn5.tex", c(19,18)) # OzaBoost
compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn6.tex", c(21,20)) #  OzaBoostAdwin

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn7.tex", c(4,3)) #  ADOB

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn8.tex", c(13,12)) # OnlineSmoothBoost

compareTwo(mevsynE,"/home/c/papers/ensemble/mevsyn9.tex", c(23,22)) # Plain


#=============================================================================

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal0.tex", c(15,14)) # OzaBag
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal1.tex", c(17,16)) # OzaBagADWIN

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal2.tex", c(9,8)) # LevBagNoADWIN
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal3.tex", c(11,10)) #  LevBag

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal4.tex", c(2,1)) # ARF Optimised Parametrization

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal5.tex", c(19,18)) # OzaBoost
compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal6.tex", c(21,20)) #  OzaBoostAdwin

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal7.tex", c(4,3)) #  ADOB

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal8.tex", c(13,12)) # OnlineSmoothBoost

compareTwo(mevrealE,"/home/c/papers/ensemble/mevreal9.tex", c(23,22)) # Plain



#==============================================================================


compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf0.tex", c(15,14)) # OzaBag
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf1.tex", c(17,16)) # OzaBagADWIN

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf2.tex", c(9,8)) # LevBagNoADWIN
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf3.tex", c(11,10)) #  LevBag

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf4.tex", c(2,1)) # ARF Optimised Parametrization

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf5.tex", c(19,18)) # OzaBoost
compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf6.tex", c(21,20)) #  OzaBoostAdwin

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf7.tex", c(4,3)) #  ADOB

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf8.tex", c(13,12)) # OnlineSmoothBoost

compareTwo(mevrealshufE,"/home/c/papers/ensemble/mevrealshuf9.tex", c(23,22)) # Plain




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


#==============================================================================
# cpu times
#==============================================================================


# synthetic times

mevsynT <- mevsynT[-c(5,6,7),-2]
# remove buggy ARF, and BOLE (BOLE can be reintroduced if needed)

colnames(mevsynT)[1] <- ""


row1 = c("ARF EFDT", "ARF VFDT", "ADOB EFDT", "ADOB VFDT", "LevBagNoAdwin EFDT", "LevBagNoAdwin VFDT", 
         "LeveragingBag EFDT", "LeveragingBag VFDT", "OnlineSmoothBoost EFDT", "OnlineSmoothBoost EFDT",
         "OzaBag EFDT", "OzaBag VFDT", "OzaBagAdwin EFDT", "OzaBagAdwin VFDT", "OzaBoost EFDT", "OzaBoost VFDT",
         "OzaBoostAdwin EFDT", "OzaBoostAdwin VFDT", "Plain EFDT", "Plain VFDT")

mevsynT[,1] <- row1

mevsynT <- round(mevsynT[,-1], digits=0)

write.table(mevsynT, "/home/c/papers/ensemble/syntheticCPUtimes.tex", quote=FALSE, col.names = TRUE, sep = ' & ', eol = " \\\\\n",)

# real data times

mevrealT <- mevrealT[-c(5,6,7),-2]
colnames(mevrealT)[1] <- ""
mevrealT[,1] <- row1
mevrealT <- round(mevrealT[,-1], digits=0)

write.table(mevrealT, "/home/c/papers/ensemble/realCPUtimes.tex", quote=FALSE, col.names = TRUE, sep = ' & ', eol = " \\\\\n",)


# real shuf times
mevrealshufT <- mevrealshufT[-c(5,6,7),-2]
# remove buggy ARF, and BOLE (BOLE can be reintroduced if needed)
colnames(mevrealshufT)[1] <- ""
mevrealshufT[,1] <- row1
mevrealshufT <- round(mevrealshufT[,-1], digits=0)

write.table(mevrealshufT, "/home/c/papers/ensemble/realshufCPUtimes.tex", quote=FALSE, col.names = TRUE, sep = ' & ', eol = " \\\\\n",)


