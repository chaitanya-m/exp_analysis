syntheticDataStreams <- c("recurrent---agrawal","recurrent---led","recurrent---randomtree","recurrent---sea", "recurrent---stagger","recurrent---waveform", 
  "hyperplane---1", "hyperplane---2","hyperplane---3","hyperplane---4",
  "rbf---drift-1","rbf---drift-2","rbf---drift-3","rbf---drift-4",
  "recurrent---abrupt---222","recurrent---abrupt---322","recurrent---abrupt---332","recurrent---abrupt---333","recurrent---abrupt---334",
  "recurrent---abrupt---335","recurrent---abrupt---422","recurrent---abrupt---444","recurrent---abrupt---522","recurrent---abrupt---555")

realDataStreams <- c("airlines", "aws---price-discretized", "chess", "covertype", "covpokelec", "fonts", 
              "hhar", "kdd", "localization", "miniboone", "nbaiot", "nswelec", "pamap2", 
              "poker", "pucrio", "sensor---home-activity", "sensor---CO-discretized", "skin", 
              "tnelec", "wisdm")

compareTwo <- function(dfs, output, compareRows){
  
  dfError<-dfs[[1]]
  dfVar<-dfs[[2]] 
  dfLeaves<-dfs[[3]]

  transpose_and_round <- function(df, precision) {

    
    if(is.na(df)[[1]] == TRUE){
      return(NA)
    }
    
    df0 <- df[compareRows,] #just the rows to compare
    dft0 <- t(df0[,c(-2)]) #transpose, remove error/time/stddev/leaves indicator from table
    colnames(dft0) <- as.character(dft0[1,]) # first row as column names
    dft0<-dft0[-1,] # get rid of first row
    dft1<-apply(dft0, 2, function(x) as.numeric(as.character(x))) # convert data to numeric in order to round
    rownames(dft1)<-rownames(dft0) # apply loses rownames
    colnames(dft1)<-colnames(dft0)
    dft1<-round(dft1,precision) # round
    return(dft1)
  }
  
  dftE2 <- transpose_and_round(dfError, 5)
  dftV2 <- transpose_and_round(dfVar, 5)
  dftL2 <- transpose_and_round(dfLeaves, 0)
  # find the first occurrence of minimum in each row
  #first_min_occurrence_pos <- matrix(c(1:nrow(dftE2), apply(dftE2, 1, which.min)), ncol=y) 
  
  # find the minimum in each row
  all_min <- apply(dftE2, 1, min)
  
  # find all occurrences of minimum in each row for error
  minima<-list()
  for (row in 1:nrow(dftE2)) { 
    minima[[row]]<-which(all_min[row]==dftE2[row,])
  }
  #minima_matrix <- cbind(c(1:nrow(dftE2)), minima)
  
  # convert back to character
  to_character <- function(dft2){
    dft <- data.frame(apply(dft2, 2, function(x) as.character(as.numeric(x))), stringsAsFactors = FALSE)
    rownames(dft)<-rownames(dft2)# apply loses rownames
    colnames(dft)<-colnames(dft2)
    return(dft)
  }
  dftE <- to_character(dftE2)
  dftV <- if (is.na(dftV2)[[1]]) NA else to_character(dftV2)
  dftL <- if (is.na(dftL2)[[1]]) NA else to_character(dftL2)
  
  if(!is.na(dftL)[[1]]){
    x<-count(dftL[dftL=="-1"])
    if (nrow(x)>0){# x['freq'][[1]] > 0) { # leaves values are -1, ie not present, so make the df an NA
      dftL <- NA
    }    
  }
    
    

    
  # if( > 0){
  #   print(x[1,1])    
  # }

  #dftL[dftL=="-1"]<-""
  
  
  
  #all minimums in bold, joint minima in cursive bold
  #minima
  #minima[[1]][2]
  
  # all minima are bold, and if there is more than one minimum in a row, it is italicized
  for(row in 1:nrow(dftE)){
    dftE[row,minima[[row]]]<-paste("\\textbf{",dftE[row,minima[[row]]],"}", sep="")
    if(length(minima[[row]])>1){
      for (minindex in minima[[row]]){
        dftE[row,minindex]<-paste("\\textit{",dftE[row,minindex],"}", sep="")
      }
    }
  }
  

  # now add up unique wins per column
  uniquewins<-list()
  uniqueminima<-minima[lapply(minima, length) == 1]
  for(col in 1:ncol(dftE)){
    uniquewins[[col]]<-sum(uniqueminima==col)
  }
  
  # now add up unique wins per column
  uniquewins<-list()
  uniqueminima<-minima[lapply(minima, length) == 1]
  #nonuniqueminima<-table(unlist(lapply(minima, unique))) # for multiple minima
  for(col in 1:ncol(dftE)){
    uniquewins[[col]]<-sum(uniqueminima==col)
  }
  

  
  # create the df to print, concatenate error with std dev and leaves
  dfPrint <- dftE
  #dfPrint <- replace(dfPrint, dfPrint=="-1", "")
  
  for(row in 1:nrow(dfPrint)){

    
    if (is.na(dftL)[[1]] && is.na(dftV)[[1]]){
    }   
        
    else if (is.na(dftL)[[1]]){ # no leaf count for ARF
      dfPrint[row, 1] <- paste(dfPrint[row,1], dftV[row,1], sep=" & ")
      dfPrint[row, 2] <- paste(dfPrint[row,2], dftV[row,2], sep=" & ")      
    }
    else if (is.na(dftV)[[1]]){ # no variance for unshuffled streams
      dfPrint[row, 1] <- paste(dfPrint[row,1], dftL[row,1], sep=" & ")
      dfPrint[row, 2] <- paste(dfPrint[row,2], dftL[row,2], sep=" & ")      
    }
    else{
      dfPrint[row, 1] <- paste(dfPrint[row,1], dftV[row,1], dftL[row,1], sep=" & ")
      dfPrint[row, 2] <- paste(dfPrint[row,2], dftV[row,2], dftL[row,2], sep=" & ")
    }
  }
  
  if(uniquewins[[2]] + uniquewins[[1]] > 0){
    
    binomtest<-binom.test(uniquewins[[2]],uniquewins[[2]] + uniquewins[[1]], (1/2), alternative = "greater")
    binomtest[["p.value"]]
    binomtest[["conf.int"]]
    list(binomtest[["p.value"]],binomtest[["conf.int"]])
    
    pvalue<-paste("p-value:", round(binomtest[["p.value"]],5))
    if(round(binomtest[["p.value"]],5) == 0){pvalue<-paste("p-value:", "$<$", "0.00001")}
    
    confint<-paste("Confidence Interval: ", round(binomtest[["conf.int"]][1],5), "---", round(binomtest[["conf.int"]][2],5))
    
    #formatting
    
    
    if (is.na(dftV) && is.na(dftL)){
      #only for 1 on 1 comparisons... test statistics
      uniquewinsPrint<-list(paste("\\multicolumn{1}{l?}{\\textbf{", uniquewins[[1]],"}}",sep=""),paste("\\multicolumn{1}{l}{\\textbf{", uniquewins[[2]], "}}",sep=""))
      dfPrint<-rbind(dfPrint,"\\midrule\n\\textbf{Unique Wins}" = uniquewinsPrint) # this is a rowname col + 2 columns in dfPrint... being configured to fit a latex table with 5 columns
      dfPrint<-rbind(dfPrint,"\\midrule\\begin{tabularx}{\\linewidth}{Xr} A \\textbf{bold} value indicates higher accuracy, and \\textit{\\textbf{bold italics}} indicate a tie.  \\\\ \\\\ The test is a one-tailed binomial test to determine the probability that the strategy in the rightmost column would achieve so many wins if wins and losses were equiprobable. & \\textbf{Test Statistics} \\end{tabularx}" 
                     =   list(paste("\\textbf{", pvalue, "}",sep=""), paste("\\textbf{", confint,"}",sep="")))
      
    }    
    
    else if (is.na(dftV) || is.na(dftL)){
      #only for 1 on 1 comparisons... test statistics
      uniquewinsPrint<-list(paste("\\multicolumn{2}{l?}{\\textbf{", uniquewins[[1]],"}}",sep=""),paste("\\multicolumn{2}{l}{\\textbf{", uniquewins[[2]], "}}",sep=""))
      dfPrint<-rbind(dfPrint,"\\midrule\n\\textbf{Unique Wins}" = uniquewinsPrint) # this is a rowname col + 2 columns in dfPrint... being configured to fit a latex table with 5 columns
      dfPrint<-rbind(dfPrint,"\\midrule\\multicolumn{2}{p{10cm}}{\\begin{tabular}{p{9cm}} A \\textbf{bold} value indicates higher accuracy, and \\textit{\\textbf{bold italics}} indicate a tie.  \\\\ \\\\ The test is a one-tailed binomial test to determine the probability that the strategy in the rightmost column would achieve so many wins if wins and losses were equiprobable. \\end{tabular}}  
                   & \\textbf{Test Statistics}" 
                     =   list(paste("\\textbf{", pvalue, "}",sep=""), paste("\\textbf{", confint,"}",sep="")))

    } else {
      #only for 1 on 1 comparisons... test statistics
      uniquewinsPrint<-list(paste("\\multicolumn{3}{l?}{\\textbf{", uniquewins[[1]],"}}",sep=""),paste("\\multicolumn{3}{l}{\\textbf{", uniquewins[[2]], "}}",sep=""))
      dfPrint<-rbind(dfPrint,"\\midrule\n\\textbf{Unique Wins}" = uniquewinsPrint) # this is a rowname col + 2 columns in dfPrint... being configured to fit a latex table with 7 columns
      dfPrint<-rbind(dfPrint,"\\midrule\\multicolumn{4}{p{10cm}}{\\begin{tabular}{p{9cm}} A \\textbf{bold} value indicates higher accuracy, and \\textit{\\textbf{bold italics}} indicate a tie.  \\\\ \\\\ The test is a one-tailed binomial test to determine the probability that the strategy in the rightmost column would achieve so many wins if wins and losses were equiprobable. \\end{tabular}}  
                   & \\textbf{Test Statistics}" 
                     =   list(paste("\\textbf{", pvalue, "}",sep=""), paste("\\textbf{", confint,"}",sep="")))
    }
    
        #dfPrint<-rbind(dfPrint,"\\midrule\n\\multicolumn{4}{p{10cm}}{A \\textbf{bold} value indicates higher accuracy, and \\textit{\\textbf{bold italics}} indicate a tie.}"=list("\\multicolumn{1}{l}{}","\\multicolumn{1}{l}{}\\\\\n") )
    #dfPrint<-rbind(dfPrint, "\\multicolumn{1}{l}{\\begin{tabularx}{0.6\\textwidth}{Xr} The test is a one-tailed binomial test to determine the probability that the strategy in the rightmost column would achieve so many wins if wins and losses were equiprobable. \\end{tabularx}} \\multicolumn{4}{l}{} \\textbf{Test Statistics}" 
    #           = list(paste("\\textbf{", pvalue, "}",sep=""), paste("\\textbf{", confint,"}",sep="")))

    #dfPrint<-rbind(dfPrint, "\\multicolumn{4}{p{8.0cm}}{The test is a one-tailed binomial test to determine the probability that the strategy in the rightmost column would achieve so many wins if wins and losses were equiprobable.} = list("\\multicolumn{1}{l}{}","\\multicolumn{1}{l}{}")
    #dfPrint<-rbind(dfPrint,"\\cmidrule[0.4pt](lr){2-3}"=list("",""))
    #dfPrint<-rbind(dfPrint, "\begin{tabularx}{\\linewidth}{Xr} The test is a one-tailed binomial test to determine the probability that the strategy in the rightmost column would achieve so many wins if wins and losses were equiprobable. & \\textbf{Test Statistics} \\end{tabularx}" 
    #           = list(paste("\\textbf{", pvalue, "}",sep=""), paste("\\textbf{", confint,"}",sep="")))
  }
  
  
  write.table(dfPrint, output, sep=" & ", quote=FALSE, col.names = FALSE,eol = " \\\\\n") # lists of row elements will be separated by &
  #dfPrint
  #file.show(output)  
  
}



compareAll <- function(dfError){
  #print(dfError[,1])
  fullcomparison_df <- data.frame(Learner1=character(), Learner2=character(), Learner2wins=integer(), Totalwins=integer(), pvalue=double(), stringsAsFactors=FALSE)
  print(fullcomparison_df)
  for (row1 in 1:nrow(dfError)) {
    for (row2 in 1:nrow(dfError)) {
      if(row1 != row2){
        #print("======================================================================")
        
        # just the rows to compare
        
        compare_df <- dfError[c(row1,row2),]
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
        # print(c(dfError[row1,1],dfError[row2,1]))
        fullcomparison_df <- rbind(fullcomparison_df, 
                                   data.frame(Learner1=dfError[[row1,1]], Learner2=dfError[[row2,1]], 
                                              Learner2wins=uniqueWinsRow2, 
                                              Totalwins=uniqueWinsRow2 + uniqueWinsRow1, 
                                              pvalue=binomtest$p.value))
        #print("======================================================================")
        
      }
    }
  }
  
  return(fullcomparison_df)
  
  
}
