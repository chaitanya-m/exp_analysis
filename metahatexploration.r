# first read the real and synthetic csvs

#closeAllConnections()
library(readr)
source("compareLearners.r")

omhs<- read_csv("/home/c/exp_dir_results/outmetahatsyn0")

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
write.table(allComparisons, "hat-efdt-ensemble.csv", quote=FALSE, col.names = TRUE, sep = ',', row.names = FALSE)#, eol = " \\\\\n",)
