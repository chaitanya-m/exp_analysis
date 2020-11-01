# first read the real and synthetic csvs

#closeAllConnections()
library(readr)
source("compareLearners.r")

ots<- read_csv("/home/c/exp_dir_results/outtreesyn0")

nrow(ots)

length(colnames(ots))
length(names(ots))

colnames(ots)[names(ots) == "X1"] <- "Learners"
colnames(ots)[names(ots) == "X2"] <- "Performance"

otsE <- ots[ots$Performance=="E",]
rownames(otsE) <- NULL # renumber the rows without skips

# Let's also remove the repetitive datasets - too many hyperplanes. keep the ones parameterized similarly to RBF.
#otsE <- otsE[,-c(11,14)]

col1 <- colnames(otsE)[3:26]
col2 <- syntheticDataStreams

dfkeysyn <- data.frame(col1,col2)

colnames(otsE)[3:26] <- col2

#print(otsE[,1])



allComparisons <- compareAll(otsE[,-2]) # without error symbol

write.table(allComparisons, "trees.csv", quote=FALSE, col.names = TRUE, sep = ',', row.names = FALSE)#, eol = " \\\\\n",)

