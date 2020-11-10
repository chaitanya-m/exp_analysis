syntheticDataStreams <- c("recurrent---agrawal","recurrent---led","recurrent---randomtree","recurrent---sea", "recurrent---stagger","recurrent---waveform", 
  "hyperplane---1", "hyperplane---2","hyperplane---3","hyperplane---4",
  "rbf---drift-1","rbf---drift-2","rbf---drift-3","rbf---drift-4",
  "recurrent---abrupt---222","recurrent---abrupt---322","recurrent---abrupt---332","recurrent---abrupt---333","recurrent---abrupt---334",
  "recurrent---abrupt---335","recurrent---abrupt---422","recurrent---abrupt---444","recurrent---abrupt---522","recurrent---abrupt---555")

realDataStreams <- c("airlines", "aws---price-discretized", "chess", "covertype", "covpokelec", "fonts", 
              "hhar", "kdd", "localization", "miniboone", "nbaiot", "nswelec", "pamap2", 
              "poker", "pucrio", "sensor---home-activity", "sensor---CO-discretized", "skin", 
              "tnelec", "wisdm")

compareTwo <- function(fulldf, output, compareRows){
  
  df <- fulldf[compareRows,] #just the rows to compare
  dft <- t(df[,c(-2)]) #transpose
  colnames(dft) <- as.character(dft[1,]) # first row as column names
  dft<-dft[-1,] # get rid of first row
  dft2<-apply(dft, 2, function(x) as.numeric(as.character(x))) # convert data to numeric in order to round
  rownames(dft2)<-rownames(dft)# apply loses rownames
  colnames(dft2)<-colnames(dft)
  dft2<-round(dft2,5) # round
  
  # find the first occurrence of minimum in each row
  #first_min_occurrence_pos <- matrix(c(1:nrow(dft2), apply(dft2, 1, which.min)), ncol=y) 
  
  # find the minimum in each row
  all_min <- apply(dft2, 1, min)
  
  # find all occurrences of minimum in each row
  minima<-list()
  for (row in 1:nrow(dft2)) { 
    minima[[row]]<-which(all_min[row]==dft2[row,])
  }
  #minima_matrix <- cbind(c(1:nrow(dft2)), minima)
  
  # convert back to character
  dft <- data.frame(apply(dft2, 2, function(x) as.character(as.numeric(x))), stringsAsFactors = FALSE) 
  rownames(dft)<-rownames(dft2)# apply loses rownames
  colnames(dft)<-colnames(dft2)
  #all minimums in bold, joint minima in cursive bold
  minima
  minima[[1]][2]
  
  # all minima are bold, and if there is more than one minimum in a row, it is italicized
  for(row in 1:nrow(dft)){
    dft[row,minima[[row]]]<-paste("\\textbf{",dft[row,minima[[row]]],"}", sep="")
    if(length(minima[[row]])>1){
      for (minindex in minima[[row]]){
        dft[row,minindex]<-paste("\\textit{",dft[row,minindex],"}", sep="")
      }
    }
  }
  
  # now add up unique wins per column
  uniquewins<-list()
  uniqueminima<-minima[lapply(minima, length) == 1]
  for(col in 1:ncol(dft)){
    uniquewins[[col]]<-sum(uniqueminima==col)
  }
  
  # now add up unique wins per column
  uniquewins<-list()
  uniqueminima<-minima[lapply(minima, length) == 1]
  #nonuniqueminima<-table(unlist(lapply(minima, unique))) # for multiple minima
  for(col in 1:ncol(dft)){
    uniquewins[[col]]<-sum(uniqueminima==col)
  }
  
  #only for 1 on 1 comparisons... test statistics
  #formatting
  uniquewinsPrint<-list(paste("\\textbf{", uniquewins[[1]],"}", sep=""),paste("\\textbf{", uniquewins[[2]], "}",sep=""))
  
  
  if(uniquewins[[2]] + uniquewins[[1]] > 0){
    
    binomtest<-binom.test(uniquewins[[2]],uniquewins[[2]] + uniquewins[[1]], (1/2), alternative = "greater")
    binomtest[["p.value"]]
    binomtest[["conf.int"]]
    list(binomtest[["p.value"]],binomtest[["conf.int"]])
    
    pvalue<-paste("p-value:", round(binomtest[["p.value"]],5))
    if(round(binomtest[["p.value"]],5) == 0){pvalue<-paste("p-value:", "$<$", "0.00001")}
    
    confint<-paste("Confidence Interval: ", round(binomtest[["conf.int"]][1],5), "---", round(binomtest[["conf.int"]][2],5))
    
    #formatting
    dft<-rbind(dft,"\\bottomrule"=list("",""))
    dft<-rbind(dft,"\\begin{tabularx}{\\linewidth}{Xr}
		A \\textbf{bold} value indicates higher accuracy, and \\textit{\\textbf{bold italics}} indicate a tie.  & \\textbf{Unique Wins}
		\\end{tabularx}" = uniquewinsPrint)
    dft<-rbind(dft,"\\cmidrule[0.4pt](lr){2-3}"=list("",""))
    dft<-rbind(dft, "\\begin{tabularx}{\\linewidth}{Xr} The test is a one-tailed binomial test to determine the probability that the strategy in the rightmost column would achieve so many wins if wins and losses were equiprobable. & \\textbf{Test Statistics} \\end{tabularx}" 
               = list(paste("\\textbf{", pvalue, "}",sep=""), paste("\\textbf{", confint,"}",sep="")))
  }
  write.table(dft,output, sep=" & ", quote=FALSE, col.names = FALSE,eol = " \\\\\n")
  #file.show(output)  
  
}



compareAll <- function(fulldf){
  #print(fulldf[,1])
  fullcomparison_df <- data.frame(Learner1=character(), Learner2=character(), Learner2wins=integer(), Totalwins=integer(), pvalue=double(), stringsAsFactors=FALSE)
  print(fullcomparison_df)
  for (row1 in 1:nrow(fulldf)) {
    for (row2 in 1:nrow(fulldf)) {
      if(row1 != row2){
        #print("======================================================================")
        
        # just the rows to compare
        
        compare_df <- fulldf[c(row1,row2),]
        #print(compare_df[,1])
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
          #print(binomtest$p.value)
        }
        # print(c(fulldf[row1,1],fulldf[row2,1]))
        fullcomparison_df <- rbind(fullcomparison_df, 
                                   data.frame(Learner1=fulldf[[row1,1]], Learner2=fulldf[[row2,1]], 
                                              Learner2wins=uniqueWinsRow2, 
                                              Totalwins=uniqueWinsRow2 + uniqueWinsRow1, 
                                              pvalue=binomtest$p.value))
        #print("======================================================================")
        
      }
    }
  }
  
  return(fullcomparison_df)
  
  
}
