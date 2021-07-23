# first read the real and synthetic csvs
library(readr)
library(plyr)
setwd("~/exp_analysis")
source("compareLearners.r")

# mev is metaefdtvfdt. syn is synthetic (with drift), real is uci streams, realshuf is uci shuffled

mevsyn <- read_csv("/home/c/results2/output/outmetaefdtvfdtsynshuf")
mevreal <- read_csv("/home/c/results2/output/outmetaefdtvfdtreal")
mevreal50 <- read_csv("/home/c/results2/output/outmetaefdtvfdtreal50")
mevrealshuf <- read_csv("/home/c/results2/output/outmetaefdtvfdtrealshuf")

clean_df<-function(df){
  
  
  # drop line 12 with no revision efdt, as that is now a separate bunch of experiments
  # also drop old ARF with old MOA default parameters (MOA has new default parameters)
  df<-df[-c(12,3,4),]
  
  # Let's also remove the repetitive datasets - too many hyperplanes. keep the ones parameterized similarly to RBF.
  #df<-df[,-c(11,14)] # already removed now from source
  
  return(df)
}

mevsynE <- clean_df(mevsyn[mevsyn$X2=="E",])
mevsynT <- clean_df(mevsyn[mevsyn$X2=="T",])
mevsynV <- clean_df(mevsyn[mevsyn$X2=="V",])
mevsynL <- clean_df(mevsyn[mevsyn$X2=="L",])

mevrealE <- clean_df(mevreal[mevreal$X2=="E",])
mevrealT <- clean_df(mevreal[mevreal$X2=="T",])
mevrealL <- clean_df(mevreal[mevreal$X2=="L",])

mevreal50E <- clean_df(mevreal50[mevreal50$X2=="E",])
mevreal50T <- clean_df(mevreal50[mevreal50$X2=="T",])
mevreal50L <- clean_df(mevreal50[mevreal50$X2=="L",])

mevrealshufE <- clean_df(mevrealshuf[mevrealshuf$X2=="E",])
mevrealshufT <- clean_df(mevrealshuf[mevrealshuf$X2=="T",])
mevrealshufV <- clean_df(mevrealshuf[mevrealshuf$X2=="V",])
mevrealshufL <- clean_df(mevrealshuf[mevrealshuf$X2=="L",])



# mevsynE <- mevsyn[mevsyn$X2=="E",]
# mevsynE <- mevsynE[-c(12,3,4),]
# 
# mevsynT <- mevsyn[mevsyn$X2=="T",]
# mevsynT <- mevsynT[-c(12,3,4),]




# mevrealE <- mevreal[mevreal$X2=="E",]
# mevrealE <- mevrealE[-c(12,3,4),]
# 
# mevrealT <- mevreal[mevreal$X2=="T",]
# mevrealT <- mevrealT[-c(12,3,4),]

# mevrealshufE <- mevrealshuf[mevrealshuf$X2=="E",]
# mevrealshufE <- mevrealshufE[-c(12,3,4),]
# 
# mevrealshufT <- mevrealshuf[mevrealshuf$X2=="T",]
# mevrealshufT <- mevrealshufT[-c(12,3,4),]

# Okay, now the noRevision results have all been taken out.


# Let's also remove the repetitive datasets - too many hyperplanes. keep the ones parameterized similarly to RBF.
#mevsynE <- mevsynE[,-c(11,14)]
#mevsynT <- mevsynT[,-c(11,14)]



col1 <- colnames(mevsynE)[3:26]
col2 <- syntheticDataStreams
#col3 <- rep(" \\", 24)
#col3

dfkeysyn <- data.frame(col1,col2)
dfkeysyn

write.table(dfkeysyn, "/home/c/papers/ensemble/syntheticStreamsKey.tex", quote=FALSE, col.names = FALSE, sep = ' & ', eol = " \\\\\n",)

colnames(mevsynE)[3:26] <- col2
colnames(mevsynT)[3:26] <- col2
colnames(mevsynV)[3:26] <- col2
colnames(mevsynL)[3:26] <- col2


col2real <- c("airlines", "aws---price-discretized", "chess", "covertype", "covpokelec", "fonts", 
          "hhar", "kdd", "localization", "miniboone", "nbaiot", "nswelec", "pamap2", 
          "poker", "pucrio", "sensor---home-activity", "sensor---CO-discretized", "skin", 
          "tnelec", "wisdm")

colnames(mevrealE)[3:22] <- col2real 
colnames(mevreal50E)[3:22] <- col2real 
colnames(mevrealshufE)[3:22] <- col2real

colnames(mevrealT)[3:22] <- col2real 
colnames(mevreal50T)[3:22] <- col2real 
colnames(mevrealshufT)[3:22] <- col2real

colnames(mevrealshufV)[3:22] <- col2real

colnames(mevrealL)[3:22] <- col2real
colnames(mevreal50L)[3:22] <- col2real 
colnames(mevrealshufL)[3:22] <- col2real

learners <- c("OzaBag","OzaBagAdwin", "LevBag without Adwin", "LevBag", "ARF", "OzaBoost", "OzaBoostAdwin", "ADOB", "BOLE", "OnlineSmoothBoost", "Plain (no ensemble)")

# note: already tested and showed that levbag without levbag equals ozabag
#=============================================================================
# Now, redo tables and analysis without noRevisionEFDT
#=============================================================================

real_results_df = data.frame()

dfs <- list(mevrealE, NA, NA)

real_results_df<-rbind(real_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal0.tex", c(15,14))) # OzaBag
real_results_df<-rbind(real_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal1.tex", c(17,16))) # OzaBagADWIN

real_results_df<-rbind(real_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal2.tex", c(9,8))) # LevBagNoADWIN
real_results_df<-rbind(real_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal3.tex", c(11,10))) #  LevBag

real_results_df<-rbind(real_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal4.tex", c(2,1))) # ARF Optimised Parametrization

real_results_df<-rbind(real_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal5.tex", c(19,18))) # OzaBoost
real_results_df<-rbind(real_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal6.tex", c(21,20))) #  OzaBoostAdwin

real_results_df<-rbind(real_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal7.tex", c(4,3))) #  ADOB
real_results_df<-rbind(real_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal8.tex", c(7,6))) #  BOLE


real_results_df<-rbind(real_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal9.tex", c(13,12))) # OnlineSmoothBoost

real_results_df<-rbind(real_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal10.tex", c(23,22))) # Plain
real_results_df







real50_results_df = data.frame()

dfs <- list(mevreal50E, NA, mevreal50L)

real50_results_df<-rbind(real50_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal500.tex", c(15,14))) # OzaBag
real50_results_df<-rbind(real50_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal501.tex", c(17,16))) # OzaBagADWIN

real50_results_df<-rbind(real50_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal502.tex", c(9,8))) # LevBagNoADWIN
real50_results_df<-rbind(real50_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal503.tex", c(11,10))) #  LevBag

real50_results_df<-rbind(real50_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal504.tex", c(2,1))) # ARF Optimised Parametrization

real50_results_df<-rbind(real50_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal505.tex", c(19,18))) # OzaBoost
real50_results_df<-rbind(real50_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal506.tex", c(21,20))) #  OzaBoostAdwin

real50_results_df<-rbind(real50_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal507.tex", c(4,3))) #  ADOB
real50_results_df<-rbind(real50_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal508.tex", c(7,6))) #  BOLE


real50_results_df<-rbind(real50_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal509.tex", c(13,12))) # OnlineSmoothBoost

real50_results_df<-rbind(real50_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevreal5010.tex", c(23,22))) # Plain
real50_results_df


real_comparison_df = data.frame()

real_comparison_df<-cbind(real_results_df[,3],real50_results_df[,3])
rownames(real_comparison_df) = c("OzaBag", "OzaBagAdwin", "LevBagNoAdwin", "LevBag","ARF", "OzaBoost", "OzaBoostAdwin", "ADOB", "BOLE", "OnlineSmoothBoost", "Plain")
colnames(real_comparison_df) = c("50-tree p-values", "50-tree p-values")






warnings()
#=============================================================================


synthetic_results_df = data.frame()

dfs <- list(mevsynE, mevsynV, NA)


synthetic_results_df<-rbind(synthetic_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevsyn0.tex", c(15,14))) # OzaBag
synthetic_results_df<-rbind(synthetic_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevsyn1.tex", c(17,16))) # OzaBagADWIN

synthetic_results_df<-rbind(synthetic_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevsyn2.tex", c(9,8))) # LevBagNoADWIN
synthetic_results_df<-rbind(synthetic_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevsyn3.tex", c(11,10))) #  LevBag

synthetic_results_df<-rbind(synthetic_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevsyn4.tex", c(2,1))) # ARF Optimised Parametrization

synthetic_results_df<-rbind(synthetic_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevsyn5.tex", c(19,18))) # OzaBoost
synthetic_results_df<-rbind(synthetic_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevsyn6.tex", c(21,20))) #  OzaBoostAdwin

synthetic_results_df<-rbind(synthetic_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevsyn7.tex", c(4,3))) #  ADOB
synthetic_results_df<-rbind(synthetic_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevsyn8.tex", c(7,6))) #  BOLE

synthetic_results_df<-rbind(synthetic_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevsyn9.tex", c(13,12))) # OnlineSmoothBoost

synthetic_results_df<-rbind(synthetic_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevsyn10.tex", c(23,22))) # Plain

synthetic_results_df

#==============================================================================

realshuf_results_df = data.frame()

dfs <- list(mevrealshufE, mevrealshufV,NA)

realshuf_results_df<-rbind(realshuf_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevrealshuf0.tex", c(15,14))) # OzaBag
realshuf_results_df<-rbind(realshuf_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevrealshuf1.tex", c(17,16))) # OzaBagADWIN

realshuf_results_df<-rbind(realshuf_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevrealshuf2.tex", c(9,8))) # LevBagNoADWIN
realshuf_results_df<-rbind(realshuf_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevrealshuf3.tex", c(11,10))) #  LevBag

realshuf_results_df<-rbind(realshuf_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevrealshuf4.tex", c(2,1))) # ARF Optimised Parametrization

realshuf_results_df<-rbind(realshuf_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevrealshuf5.tex", c(19,18))) # OzaBoost
realshuf_results_df<-rbind(realshuf_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevrealshuf6.tex", c(21,20))) #  OzaBoostAdwin

realshuf_results_df<-rbind(realshuf_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevrealshuf7.tex", c(4,3))) #  ADOB
realshuf_results_df<-rbind(realshuf_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevrealshuf8.tex", c(7,6))) #  BOLE

realshuf_results_df<-rbind(realshuf_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevrealshuf9.tex", c(13,12))) # OnlineSmoothBoost

realshuf_results_df<-rbind(realshuf_results_df, compareTwo(dfs,"/home/c/papers/ensemble/mevrealshuf10.tex", c(23,22))) # Plain

realshuf_results_df

#==============================================================================
# summary
#==============================================================================

summary_df <- cbind(learners, real_results_df, realshuf_results_df, synthetic_results_df)
write.table(summary_df, "/home/c/papers/ensemble/summary.tex", sep=" & ", quote=FALSE, col.names = FALSE, row.names = FALSE, eol = " \\\\\n") 

#==============================================================================
# cpu times
#==============================================================================


# synthetic times

mevsynT <- mevsynT[-c(5),-2]
# remove buggy ARF

colnames(mevsynT) <- seq(0,24)
colnames(mevsynT)[1] <- "& \\textit{Learners}  $\\downarrow$ \\; \\textit{Streams} $\\rightarrow$"
#  "& $Learners \downarrow Streams \rightarrow$"

learners = c("ARF EFDT", "ARF VFDT", "ADOB EFDT", "ADOB VFDT", "BOLE EFDT", "BOLE VFDT", "LevBagNoAdwin EFDT", "LevBagNoAdwin VFDT", 
             "LeveragingBag EFDT", "LeveragingBag VFDT", "OnlineSmoothBoost EFDT", "OnlineSmoothBoost EFDT",
             "OzaBag EFDT", "OzaBag VFDT", "OzaBagAdwin EFDT", "OzaBagAdwin VFDT", "OzaBoost EFDT", "OzaBoost VFDT",
             "OzaBoostAdwin EFDT", "OzaBoostAdwin VFDT", "Plain EFDT", "Plain VFDT")

mevsynT[,1] <- learners

mevsynT[,-1] <- round(mevsynT[,-1], digits=0) # round to integers


write.table(mevsynT, "/home/c/papers/ensemble/syntheticCPUtimes.tex", quote=FALSE, col.names = TRUE, sep = ' & ', eol = " \\\\\\hline\n",)

# real data times

mevrealT <- mevrealT[-c(5),-2]
colnames(mevrealT) <- seq(0,20)
colnames(mevrealT)[1] <- colnames(mevsynT)[1]

mevrealT[,1] <- learners
mevrealT[,-1] <- round(mevrealT[,-1], digits=0)

write.table(mevrealT, "/home/c/papers/ensemble/realCPUtimes.tex", quote=FALSE, col.names = TRUE, sep = ' & ', eol = " \\\\\\hline\n",)


# real shuf times
mevrealshufT <- mevrealshufT[-c(5),-2]
# remove buggy ARF, and BOLE (BOLE can be reintroduced if needed)
colnames(mevrealshufT) <- seq(0,20)
colnames(mevrealshufT)[1] <- colnames(mevsynT)[1]
mevrealshufT[,1] <- learners
mevrealshufT[,-1] <- round(mevrealshufT[,-1], digits=0)

write.table(mevrealshufT, "/home/c/papers/ensemble/realshufCPUtimes.tex", quote=FALSE, col.names = TRUE, sep = ' & ', eol = " \\\\\\hline\n",)



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

menrsyn <- read_csv("/home/c/results2/output/outmetaeftdnorevisionsynthetic")
menrsynE <- menrsyn[menrsyn$X2=="E",]

menrreal <- read_csv("/home/c/results2/output/outmetaeftdnorevisionreal")
menrrealE <- menrreal[menrreal$X2=="E",]

menrrealshuf <- read_csv("/home/c/results2/output/outmetaeftdnorevisionrealshuf")
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


