## Laura's code
library(plyr)
library(dplyr)
library(purrr)
library(tidyverse)
#check and recode switching and congruency

#switching happens when what they respond to changes, so if they respond to shape and then respond to color, for example
#congruent means the response for both shape and color for a given stim is the same (depends on condition)

#can check switching just by looking at sequence of dashed vs. solid

#checking congruency will require knowing the condition mappings

#when done, need to check balance

####FIRST, divide up the data into participants, then blocks
raw_dat <- read.csv("task-switching-replication.csv")
#should be 54 subs, 8 blocks each, 49 trials each (including buffer trial)
sub_list <- list()

N <- length(unique(raw_dat$participant))

for (i in 1:N){
  sub_list[[i]] <- list()
  tempdat <- raw_dat %>% filter(participant == i) %>%
    select(-c(session,trialType,date, utcTime))
  for (j in 1:8){
    sub_list[[i]][[j]] <- tempdat[((j-1)*49 + 1):((j-1)*49 + 49), ]
 
  }
}

###SECOND, read in condition information, do some checking, fix congruency

conditions <- read.csv(file = "task switching counter balancing - Sheet1.csv", stringsAsFactors = F)

#does every block have 49 trials?
all(sapply(sub_list, function(x) sapply(x, function(y) dim(y)[1])) == 49)

#does every block start with a buffer?
all(sapply(sub_list, function(x) sapply(x, function(y) y$switchTrialType[1] == "buffer")))

#more checks
for (i in 1:N){
  for (j in 1:8){
    tempdat <- sub_list[[i]][[j]]
    tempcondition <- tempdat$condition[1] 
    
    #is the initial posture right?
    firstposture <- conditions[conditions$co == tempcondition, 2]
    if ((j < 5) & (tempdat$posture[1] == firstposture)){
      #good
    } else if ((j > 4) & (tempdat$posture[1] != firstposture)){
      #good
    } else {
      print(paste(i,j))
    }
    
    #is the switching right?
    for (k in 2:49){
      if (tempdat$cueType[k] == tempdat$cueType[k-1]){
        if(tempdat$switchTrialType[k] == "noswitch"){
          next
        } else {
          print(paste(i,j,k))
        }
      } else {
        if(tempdat$switchTrialType[k] == "switch"){
          next
        } else {
          print(paste(i,j,k))
        }
      }
    }
    
    
    #ok, now do congruency

     tempdat <- tempdat %>% 
      mutate(new_congruency = case_when(
        shapeType == conditions$shape.left[tempcondition] & shapeColor == conditions$color.left[tempcondition] ~ "congruent",
        shapeType == conditions$shape.right[tempcondition] & shapeColor == conditions$color.right[tempcondition] ~ "congruent",
        shapeType == conditions$shape.right[tempcondition] & shapeColor == conditions$color.left[tempcondition] ~ "incongruent",
        shapeType == conditions$shape.left[tempcondition]  & shapeColor == conditions$color.right[tempcondition] ~ "incongruent",
    ))

     sub_list[[i]][[j]] <- tempdat
     
     #is new congruency balanced?
     congr.count <- tempdat %>% filter(switchTrialType != "buffer") %>%
       group_by(new_congruency) %>%
       summarize(n = n())
     
     if (congr.count$n[1] != 24){
       print(paste(i,j))
     }
     
    
  }
}

#put giant dataframe back together
list1 <- lapply(sub_list, function(x) ldply(x, rbind))
corrected.df <- ldply(list1,rbind)

## Jon's  code

#Get the working directory
workingdir <- getwd()

task_switching_raw <- read.csv(paste0(workingdir, 
                                      "/task-switching-replication.csv"))
head(task_switching_raw)


condition <- read.csv(paste0(workingdir,
                            "/task switching counter balancing - Sheet1.csv"),
                      stringsAsFactors = F)

condition$condition <- condition$co
head(condition)

# order <- seq(1:dim(task_switching_raw)[1])
# 
# task_switching_raw$order.num <- order

task_switching_merged <- merge(task_switching_raw, 
                               condition, 
                               by = "condition",
                               all = TRUE,
                               sort = F)


 # task_switching_merged_sorted <- arrange(task_switching_merged, 
 #                                         order.num
 #                                         )

task_switching_raw$shapeType == task_switching_merged_sorted$shapeType
  
task_switching_joined <- 
join(task_switching_raw, 
      condition, 
      by = "condition",
      type = 'full')
  
head(task_switching_merged)
#Re-calc con/incon based on condition 
task_switching_recoded <- task_switching_joined %>% 
  mutate(new_congruency = 
         case_when(
          shapeType == shape.left  & shapeColor == color.left ~ "congruent",
          shapeType == shape.right & shapeColor == color.right ~ "congruent",
          TRUE ~ "incongruent")  #If it doesn't match the above two if conditions
        )


# 
# task_switching_recoded_sorted <- arrange(task_switching_recoded, 
#                                          order.num
#                                          )
#Compare recoding
sum(task_switching_recoded$new_congruency == corrected.df$new_congruency)

#Compare shapeType
sum(task_switching_recoded$shapeType == corrected.df$shapeType)

#Compare shapeColor
sum(task_switching_recoded$shapeColor == corrected.df$shapeColor)

sum(task_switching_recoded$shapeColor == corrected.df$shapeColor)

write.csv(task_switching_recoded, "jon_recoded.csv")
write.csv(corrected.df, "laura_recoded.csv")

#Quick calcs
# corrected.df$correct_bin <- recode(corrected.df$correct, 
#                                           "no"  = 0,
#                                           "yes" = 1)
# means <- corrected.df %>% 
#   group_by(posture , new_congruency)  %>%
#   summarize(Participant = mean(participant),
#             Accuracy    = mean(correct_bin),
#             RT = mean(reactionTime, na.rm = T))
