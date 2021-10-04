require(tidyverse)

#play with trial sequences

#stimuli:
#yellow square
#yellow triangle
#blue square
#blue triangle

#just use "Shape_A, Shape_B, Col_A, Col_B" so the same sequence can be used with different 
#stimulus, response mappings (I think)

colorseq <- rep(c("Col_A","Col_B"), each = 4)
shapeseq <- rep(c("Shape_A","Shape_B","Shape_A","Shape_B"), each = 2)
taskseq <- rep(c("color","shape"),times = 4)
congruent <- rep(c(1,0,0,1), each = 2)

df <- cbind(colorseq, shapeseq, taskseq, congruent)

df48 <- data.frame(rbind(df,df,df,df,df,df))
balance = 0
iterations <- 1
while(balance < 1){
  
  tempbuffer <- sample(48, size = 1)
  tempseq <- sample(48)
  
  tempdf <- df48[c(tempbuffer, tempseq), ]
  tempdf <- tempdf %>% mutate(prevtask = lag(taskseq), switch = taskseq == prevtask)
  
  if (sum(tempdf$switch, na.rm = T) == 24){
    balance = 1
  }
  
  iterations <- iterations + 1
}
print(iterations)
result <- tempdf
print(result)


                   