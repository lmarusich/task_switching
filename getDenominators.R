#figure out denominators

library(MASS)

df <- read.csv(file = "StandingData.csv", stringsAsFactors = F)
df <- df[1:30 , 1:9]

fractions <- list()
denoms <- list()
maxdenom <- numeric(8)

for (i in 2:9){
  
  tempmultiplier <- fractions(df[, i], max = 100)
  fractions[[i-1]] <- tempmultiplier
  
  denoms[[i-1]] <- as.numeric(sapply(strsplit(attr(tempmultiplier,"fracs"), "/"), function(x) x[2]))
  maxdenom[i-1] <- max(denoms[[i-1]], na.rm = T)
  
}

