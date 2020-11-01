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


compareAll <- function(fulldf){
    print(fulldf[,1])
  
    for (row1 in 1:nrow(fulldf)) {
        for (row2 in 1:nrow(fulldf)) {
            if(row1 != row2){
                #print("======================================================================")
              
                # just the rows to compare
              
                compare_df <- fulldf[c(row1,row2),]
                print(compare_df[,1])
                #print(compare_df)
                compare_df <- compare_df[,-1]         # without column names
                
                #print(compare_df[c(1,2),c(1,2)])
                #print(row.names(compare_df))
                #print(paste("Comparing ", compare_df[1,0], " and ", compare_df[2,0]))
                
                # column-wise minima
                all_min <- apply(compare_df,2,FUN=min)

                # find all occurrences of minimum in each column
                minima<-list()
                for (col in 1:ncol(compare_df)) { 
                  minima[[col]]<-which(all_min[col]==compare_df[,col])
                }
                
                #print(all_min)
                #print(minima)
                # now add up unique wins per row
                uniquewins<-list()
                uniqueminima<-minima[lapply(minima, length) == 1] # Only one min implies unique minimum
                #print(uniqueminima)
                for(row in 1:nrow(compare_df)){
                  uniquewins[[row]]<-sum(uniqueminima==row)       # Sum up the number of times this row wins
                }
                
                #print(uniquewins)

                uniqueWinsRow1 <- uniquewins[[1]]
                uniqueWinsRow2 <- uniquewins[[2]]
                
                #print(uniqueWinsRow1)
                #print(uniqueWinsRow2)
                if(uniqueWinsRow1 > 0 || uniqueWinsRow2 > 0){
                  binomtest<-binom.test(uniqueWinsRow2, uniqueWinsRow2 + uniqueWinsRow1, (1/2), alternative = "greater")
                  print(binomtest$p.value)
                }
                # print(c(fulldf[row1,1],fulldf[row2,1]))
                
                print("======================================================================")
                
            }
        }
    }
}

compareAll(otsE[,-2]) # without error symbol


#write.table(dfkeysyn, "/home/c/papers/tree/syntheticStreamsKey.tex", quote=FALSE, col.names = FALSE, sep = ' & ', eol = " \\\\\n",)




# Just HAT and HAT -A
#comparisonTable(c(19,5),otsE,"/home/c/papers/tree/table1.tex")
compareTwo(otsE,"/home/c/papers/tree/table1.tex", c(19,5))

# Just HAT -A -B and HAT -A (multiple alternates also vote)
compareTwo(otsE,"/home/c/papers/tree/table2.tex", c(4,5))

# HAT -A -B -H and HAT -A -B (multiple alternates also vote, but single leaf alternates do not, and just multiple alternates vote)
compareTwo(otsE,"/home/c/papers/tree/table3.tex", c(3,4))


# HAT -A -B -H and HAT -A -B -H -I (multiple alternates also vote, but single leaf alternates do not; and leaf weighting on)
compareTwo(otsE,"/home/c/papers/tree/table4.tex", c(3,2))
#table <- otsE[c(3,2),]

# HAT and HAT -E (getweightseen instead of nodeTime)
compareTwo(otsE,"/home/c/papers/tree/table5.tex", c(19,17))
#table <- otsE[c(19,17),]

# HAT and HAT -C (resplitting on nominals)
compareTwo(otsE,"/home/c/papers/tree/table6.tex", c(19,15))
#table <- otsE[c(19,15),]

# HAT and HAT -D (no averaging infogain)
compareTwo(otsE,"/home/c/papers/tree/table7.tex", c(19,16))
#table <- otsE[c(19,16),]

# HAT -CDE and HAT -CDE -F(vfdt behaviors, and top level replacement behavior)
compareTwo(otsE,"/home/c/papers/tree/table8.tex", c(14,12))
#table <- otsE[c(14,12),]


# HAT -CDE and HAT -CDE -G(vfdt behaviors, and alternate replacement behavior)
compareTwo(otsE,"/home/c/papers/tree/table9.tex", c(14,13))
#table <- otsE[c(14,13),]

#	-l (trees.HAT -C -D -E -A -B -H -I) and 7	-l (trees.HAT -C -D -E -A -B -H -I -F -G) (without and with alternate replacement behavior)
compareTwo(otsE,"/home/c/papers/tree/table10.tex", c(7,6))
#table <- otsE[c(7,6),]

#	HAT and HAT -C -D -E (without and with VFDT undocumented behaviors)
compareTwo(otsE,"/home/c/papers/tree/table11.tex", c(19,14))
#table <- otsE[c(19,14),]

# Just HAT and HAT -C -D -E -A -B -H -I -F -G
compareTwo(otsE,"/home/c/papers/tree/table12.tex", c(19,6))




#======================================================================================================================================
ovv<- read_csv("/home/c/exp_dir_results/output/outvfdtvariants")

nrow(ovv)

length(colnames(ovv))
length(names(ovv))

colnames(ovv)[names(ovv) == "X1"] <- "Learners"
colnames(ovv)[names(ovv) == "X2"] <- "Performance"

ovvE <- ovv[ovv$Performance=="E",]
rownames(ovvE) <- NULL # renumber the rows without skips

# Let's also remove the repetitive datasets - too many hyperplanes. keep the ones parameterized similarly to RBF.
ovvE <- ovvE[,-c(11,14)]

colnames(ovvE)[3:26] <- col2



# VFDT and VFDT -C (resplitting)
compareTwo(ovvE,"/home/c/papers/tree/table101.tex", c(6,3))
#table <- ovvE[c(5,2),]

# VFDT and VFDT -D (no infogain averaging)
compareTwo(ovvE,"/home/c/papers/tree/table102.tex", c(6,4))
#table <- ovvE[c(5,3),]

# VFDT and VFDT -E (weight at leaf instead of nodetime)
compareTwo(ovvE,"/home/c/papers/tree/table103.tex", c(6,5))
#table <- ovvE[c(5,4),]

# VFDT and VFDT -C -D -E
compareTwo(ovvE,"/home/c/papers/tree/table104.tex", c(6,1))
#table <- ovvE[c(5,1),]

# VFDT and VFDT -C -J (clear node instead of resplitting)
compareTwo(ovvE,"/home/c/papers/tree/table105.tex", c(6,2))
#table <- ovvE[c(5,1),]


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






#########################################################################################################
#########################################################################################################
# comparisonTable <- function(compareRows, fulltable, output) {
# 
#   table <- fulltable[compareRows,] #just the rows to compare
#   
#   
#   
#   
#   
#   wins <- table[,-c(1,2)][2,] < table[,-c(1,2)][1,]  
# 
#   # number of cases in which 2nd row is lower than 1st
#   table$wins <- c(length(wins[wins==FALSE]),length(wins[wins==TRUE])) 
#   binom.test(table$wins[2],table$wins[2] + table$wins[1], (1/2), alternative = "greater")
#   x <- t(table[,-2])
#   
#   
#     
#   # Find out the row and column index for maximum and minimum value
#   max_pos <- matrix(c(1:nrow(df), apply(df, 1, which.max)), ncol=2)
#   min_pos <- matrix(c(1:nrow(df), apply(df, 1, which.min)), ncol=2)
#   # Replace them
#   df[max_pos] <- df[max_pos] - 0.1
#   df[min_pos] <- df[min_pos] + 0.1
#   df
#   
# 
# 
#   rownames(x)[1] <- "Generators"
#   x[1,1] <- col1name
#   x[1,2] <- col2name
#   df <- data.frame(x)
#   df <- cbind(newColName = rownames(df), df)
#   rownames(df) <- NULL
#   colnames(df) <- as.character(unlist(df[1,]))
#   df <- df[-1,]
#   
#   fdf <- condformat(df)
#   for(j in 2:3){
#     fdf <- rule_text_bold(fdf, !!j, df[,!!j] == apply(df[,2:3], 1, FUN=min))
#   }
#   #fdf <- rule_text_bold(fdf, 2, df[,2] == apply(df[,2:3], 1, FUN=min))
#   #fdf <- rule_text_bold(fdf, 3, df[,3] == apply(df[,2:3], 1, FUN=min))
#   #for loop doesn't work with this for some reason  
#   # the expression is the condition, that the value in the column under consideration is the row min
#   return(fdf)
# 
# }
#=======================================================

# 
# dft<-dft[-1,]
# y<-length(compareRows)
# apply(dft, 1, which.min)
# 
# min_pos <- matrix(c(1:nrow(df), apply(dft, 1, which.min)), ncol=y)
# sum(min_pos==2)
# 
# 
# 
# fdf<-theme_kable(fdf, "latex", booktabs = T)#%>%kable_styling(latex_options = "striped")
# fdf
# #pfdf<-condformat2latex(fdf)
# #print(pfdf)
# #warnings()
# sink(output)
# condformat2latex(fdf)
# sink()  
# 
# fdf
# file.show(output)  
# 
# #View(x[,2])
# #write.table(x,"/home/c/papers/tree/table1.csv", sep=",", quote=FALSE, col.names = FALSE)
# 

