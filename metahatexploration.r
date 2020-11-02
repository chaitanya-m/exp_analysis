# first read the real and synthetic csvs

#closeAllConnections()
library(readr)
source("compareLearners.r")

# This is MOA version of HAT with all unspecified features

dfhathateagersyn<- read_csv("/home/c/exp_dir_results/output/outhathateagersynthetic")



nrow(dfhathateagersyn)

length(colnames(dfhathateagersyn))
length(names(dfhathateagersyn))

colnames(dfhathateagersyn)[names(dfhathateagersyn) == "X1"] <- "Learners"
colnames(dfhathateagersyn)[names(dfhathateagersyn) == "X2"] <- "Performance"

dfhathateagersynE <- dfhathateagersyn[dfhathateagersyn$Performance=="E",]
rownames(dfhathateagersynE) <- NULL # renumber the rows without skips

# Let's also remove the repetitive datasets - too many hyperplanes. keep the ones parameterized similarly to RBF.
#dfhathateagersynE <- dfhathateagersynE[,-c(11,14)]

col1 <- colnames(dfhathateagersynE)[3:26]
col2 <- syntheticDataStreams

dfkeysyn <- data.frame(col1,col2)

colnames(dfhathateagersynE)[3:26] <- col2

#print(dfhathateagersynE[,1])



allComparisons <- compareAll(dfhathateagersynE[,-2]) # without error symbol


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
