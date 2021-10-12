#figure out denominators

library(MASS)

#read in accuracy data from experiment 2 (smith et al)
df <- read.csv(file = "StandingData.csv", stringsAsFactors = F)
df <- df[1:30 , 1:9]

fractionlist <- list()
denoms <- list()
maxdenom <- numeric(8)

for (i in 2:9){
  
  #convert each column of decimals from spreadsheet to fractions
  tempfractions <- fractions(df[, i], max = 100)
  fractionlist[[i-1]] <- tempfractions
  
  #get just the denominators from the fractions
  denoms[[i-1]] <- as.numeric(sapply(strsplit(attr(tempfractions,"fracs"), "/"), function(x) x[2]))
  
  #figure out what the largest denominator is from that column
  maxdenom[i-1] <- max(denoms[[i-1]], na.rm = T)
  
}

