# first read the real and synthetic csvs
library(readr)
library(ggplot2)
library(gridExtra)

syn <- read_csv("output/outhatsyn")
nrow(outTableRealShuf)
length(colnames(syn))
length(names(syn))

colnames(syn)[names(syn) == "X1"] <- "Learners"
colnames(syn)[names(syn) == "X2"] <- "Performance"

# Just the best and HATOriginal
synE <- syn[syn$Performance=="E",]
#synE <- synE[c(8,12),]
#synE <- synE[c(4,12),]
#synE <- synE[c(7,12),]
#synE <- synE[c(5,11),]
#synE <- synE[c(8,11),]
#synE <- synE[c(7,11),]
#synE <- synE[c(6,11),]
#synE <- synE[c(6,12),]
#synE <- synE[c(4,12),]
#synE <- synE[c(1,12),]
#synE <- synE[c(11,12),]
#synE <- synE[c(11,12),]
synE411 <- synE[c(4,11),]
x <- t(synE411[,-2])
rownames(x)[1] <- ""
write.table(x,"/home/c/papers/kddhatvote/synE411.csv", sep=",", quote=FALSE, col.names = TRUE)

synE511 <- synE[c(5,11),]
write.csv(synE511,"/home/c/papers/kddhatvote/synE511.csv")

synE1011 <- synE[c(10,11),]
write.csv(synE1011,"/home/c/papers/kddhatvote/synE1011.csv")



#png("test.png", height=35*nrow(synE0), width=800*nrow(synE0))
#p<-tableGrob(synE0)
#grid.arrange(p)
#dev.off()


# Count no of wins
#winE <- aggregate(by = "",synE, FUN="max")
synE0 <- synE1011
winE = synE0[,-(1:2)]
for (colname in names(winE)){
  #print(name)
  #print(winE[,name])
  for(rowname in row.names(winE)){
    winE[rowname,colname] = (winE[rowname,colname] == max(winE[,colname]))
    #print(winE[rowname,colname] == max(winE[,colname]))
  }
  #winE[,name] == max(winE[name])
  #which.max(winE[name])
  #print(max(winE[name]))
}

winE$sum = rowSums(winE)
print(winE$sum)

warnings()
#winE[which(apply(winE, 2, function(x) x == max(x,na.rm=TRUE)))] <- -1
#winE[,]


#for (i in names(winE)){
#  print(synE$i)
#}



#winE <- apply(synE[,-(1:2)],2,min)

#ranksynE <-synE
#ranksynE[,-(1:2)] <- apply(ranksynE[,-(1:2)],2, function(x){rank(x, ties.method="average")})

#rankTableRealStandardE$mean = rowMeans(rankTableRealStandardE[,-(1:2)])
#ranksynE$mean = rowMeans(ranksynE[,-(1:2)])
#ranksRealStandardE <- rankTableRealStandardE[,c("Learners", "mean")]
#synmeanE <- ranksynE[,c("Learners", "mean")]




real <- read_csv("output/outhatrealstan")
nrow(real)
length(colnames(real))
length(names(real))

colnames(real)[names(real) == "X1"] <- "Learners"
colnames(real)[names(real) == "X2"] <- "Performance"

# Just the best and HATOriginal
realE <- real[real$Performance=="E",]
realE <- realE[c(4,11,12),c(1:2,3, 4, 6, 7,10,14, 16,21)]





rankrealE <-realE
rankrealE[,-(1:2)] <- apply(rankrealE[,-(1:2)],2, function(x){rank(x, ties.method="average")})

#rankTableRealStandardE$mean = rowMeans(rankTableRealStandardE[,-(1:2)])
rankrealE$mean = rowMeans(rankrealE[,-(1:2)])

#ranksRealStandardE <- rankTableRealStandardE[,c("Learners", "mean")]
realmeanE <- rankrealE[,c("Learners", "mean")]






# ranksRealShufT <- rankTableRealShufT[,c("Learners", "mean")]
# ranksSyntheticShufT <- rankTableSyntheticShufT[,c("Learners", "mean")]



#write.csv(rankTableRealE[,c("Learners", "mean")],"rankTableRealE.csv")
#write.csv(outTableReal,"outTableReal.csv")








