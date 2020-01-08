# first read the real and synthetic csvs
library(readr)

outTableRealShuf <- read_csv("output/outTableRealShuf.csv")
outTableSyntheticShuf <- read_csv("output/outTableSyntheticShuf.csv")
outTableRealStandard <- read_csv("output/outTableRealStandard.csv")

nrow(outTableRealShuf)

length(colnames(outTableRealShuf))
length(names(outTableRealShuf))

additionalResultsRealShuf <- read_csv("output/outrealshuf.csv")
additionalResultsSyntheticShuf <- read_csv("output/outsyntheticshuf.csv")
additionalResultsRealStandard <- read_csv("output/outrealstandard.csv")

outTableRealShuf2 <- rbind(outTableRealShuf, additionalResultsRealShuf)
outTableSyntheticShuf2 <- rbind(outTableSyntheticShuf, additionalResultsSyntheticShuf)
outTableRealStandard2<- rbind(outTableRealStandard, additionalResultsRealStandard)

write.csv(outTableRealShuf2, "output/outTableRealShuf.csv", row.names=FALSE)
write.csv(outTableSyntheticShuf2, "output/outTableSyntheticShuf.csv",row.names=FALSE)
write.csv(outTableRealStandard2, "output/outTableRealStandard.csv",row.names=FALSE)


colnames(outTableRealShuf)[names(outTableRealShuf) == "X1"] <- "Learners"
colnames(outTableRealShuf)[names(outTableRealShuf) == "X2"] <- "Performance"

colnames(outTableRealStandard)[names(outTableRealStandard) == "X1"] <- "Learners"
colnames(outTableRealStandard)[names(outTableRealStandard) == "X2"] <- "Performance"

colnames(outTableSyntheticShuf)[names(outTableSyntheticShuf) == "X1"] <- "Learners"
colnames(outTableSyntheticShuf)[names(outTableSyntheticShuf) == "X2"] <- "Performance"

