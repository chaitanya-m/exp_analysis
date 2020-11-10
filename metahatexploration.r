# first read the real and synthetic csvs

#closeAllConnections()
library(readr)
source("compareLearners.r")



tidydfsynthetic <- function(df){
  
  nrow(df)
  length(colnames(df))
  length(names(df))
  
  colnames(df)[names(df) == "X1"] <- "Learners"
  colnames(df)[names(df) == "X2"] <- "Performance"
  
  dfE <- df[df$Performance=="E",]
  rownames(dfE) <- NULL # renumber the rows without skips
  
  # Let's also remove the repetitive datasets - too many hyperplanes. keep the ones parameterized similarly to RBF.
  #dfE <- dfE[,-c(11,14)]
  
  col1 <- colnames(dfE)[3:26]
  col2 <- syntheticDataStreams
  
  dfkeysyn <- data.frame(col1,col2)
  
  colnames(dfE)[3:26] <- col2
  
  return(dfE[,-2]) # without error symbol
}

tidydfreal <- function(df){
  
  nrow(df)
  length(colnames(df))
  length(names(df))
  
  colnames(df)[names(df) == "X1"] <- "Learners"
  colnames(df)[names(df) == "X2"] <- "Performance"
  
  dfE <- df[df$Performance=="E",]
  rownames(dfE) <- NULL # renumber the rows without skips
  
  # Let's also remove the repetitive datasets - too many hyperplanes. keep the ones parameterized similarly to RBF.
  #dfE <- dfE[,-c(11,14)]
  
  col1 <- colnames(dfE)[3:22]
  col2 <- realDataStreams
  
  dfkeysyn <- data.frame(col1,col2)
  
  colnames(dfE)[3:22] <- col2
  
  return(dfE[,-2]) # without error symbol
  
}

#print(dfhathateagersynE[,1])
#================================================================================


# This is MOA version of HAT with all unspecified features

dfefdthatsyn<- read_csv("/home/c/exp_dir_results/output/outefdthatsynthetic")
dfefdthatreal<- read_csv("/home/c/exp_dir_results/output/outefdthatreal")
dfefdthatrealshuf<- read_csv("/home/c/exp_dir_results/output/outefdthatrealshuf")

dfhathateagersyn<- read_csv("/home/c/exp_dir_results/output/outhathateagersynthetic")
dfhathateagerreal<- read_csv("/home/c/exp_dir_results/output/outhathateagerreal")
dfhathateagerrealshuf<- read_csv("/home/c/exp_dir_results/output/outhathateagerrealshuf")

compare_efdthatsyn <- compareAll( rbind( tidydfsynthetic(dfefdthatsyn), tidydfsynthetic(dfhathateagersyn)[2,] ) ) 
compare_efdthatreal <- compareAll( rbind( tidydfreal(dfefdthatreal), tidydfreal(dfhathateagerreal)[2,]    ) ) 
compare_efdthatrealshuf <- compareAll( rbind( tidydfreal(dfefdthatrealshuf), tidydfreal(dfhathateagerrealshuf)[2,]) )

compare_hathateagersyn <- compareAll(tidydfsynthetic(dfhathateagersyn)) 
compare_hathateagerreal <- compareAll(tidydfreal(dfhathateagerreal)) 
compare_hathateagerrealshuf <- compareAll(tidydfreal(dfhathateagerrealshuf)) 

compareTwo(tidydfsynthetic(dfefdthatsyn),"/home/c/papers/1progressReport/content/tables/efdthatsyn.tex", c(1,2))
compareTwo(tidydfreal(dfefdthatreal),"/home/c/papers/1progressReport/content/tables/efdthatreal.tex", c(1,2))
compareTwo(tidydfreal(dfefdthatrealshuf),"/home/c/papers/1progressReport/content/tables/efdthatrealshuf.tex", c(1,2))

compareTwo(tidydfsynthetic(dfhathateagersyn),"/home/c/papers/1progressReport/content/tables/hathateagersyn.tex", c(1,2))
compareTwo(tidydfreal(dfhathateagerreal),"/home/c/papers/1progressReport/content/tables/hathateagerreal.tex", c(1,2))
compareTwo(tidydfreal(dfhathateagerrealshuf),"/home/c/papers/1progressReport/content/tables/hathateagerrealshuf.tex", c(1,2))








#=================================================================================
# This is my version of HAT.
omhs<- read_csv("/home/c/exp_dir_results/output/outmetahatsynthetic")

nrow(omhs)

length(colnames(omhs))
length(names(omhs))

colnames(omhs)[names(omhs) == "X1"] <- "Learners"
colnames(omhs)[names(omhs) == "X2"] <- "Performance"

omhsE <- omhs[omhs$Performance=="E",]
rownames(omhsE) <- NULL # renumber the rows without skips

# Let's also remove the repetitive datasets - too many hyperplanes. keep the ones parameterized similarly to RBF.
#omhsE <- omhsE[,-c(11,14)]

col1 <- colnames(omhsE)[3:26]
col2 <- syntheticDataStreams

dfkeysyn <- data.frame(col1,col2)

colnames(omhsE)[3:26] <- col2

#print(omhsE[,1])



allComparisons <- compareAll(omhsE[,-2]) # without error symbol

#allComparisons <- dplyr::filter(allComparisons, !grepl('EFDT|HATEFDT', Learner1), grepl('HATEFDT', Learner2))
#allComparisons <- dplyr::filter(allComparisons, grepl('OzaBoost', Learner1),grepl('OzaBoost', Learner2))
#allComparisons <- dplyr::filter(allComparisons, !grepl('HATEFDT|EFDT', Learner1))


write.table(allComparisons, "hat-efdt-ensemble.csv", quote=FALSE, col.names = TRUE, sep = ',', row.names = FALSE)#, eol = " \\\\\n",)

#================================================
