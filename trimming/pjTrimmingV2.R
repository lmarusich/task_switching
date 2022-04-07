pjRecursiveTrim2 <- function(dataSet,dv, splitvars){
  # Version 1.2
  # removed zscore columns
  # added METHOD functionality 
  #       method="recursive" for the modified recursive algorithm
  #       method="nonrecursive" for the non recursive algorithm
  
  # Version 1.1
  #   -fixed proportion trimmed to percentage trimmed (v1.1)
  # CREATED MY MICHAEL CHAN-REYNOLDS @ TRENT UNIVERSITY
  # PLEASE ME KNOW RIGHT AWAY IF YOU FIND ANY PROBLEMS
  
  #dataSet = the data.frame with all of the data in long format
  #dv is the "name" of the variable in the dataSet to be trimmed 
  #splitvars is c("the", "variables", "in", "dataSet", "to", "split", "on")
  
  #LOAD VALUES FROM VAN SELST & JOLICOEUR (1994)
  sampleSize = c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100)
  nonRecursiveCriterion= c(1.458,1.68,1.841,1.961,2.05,2.12,2.173,2.22,2.246,2.274,2.31,2.326,2.339,2.352,2.365,2.378,2.391,2.3948,2.3986,2.4024,2.4062,2.41,2.4141,2.4182,2.4223,2.4264,2.4305,2.432975,2.43545,2.437925,2.4404,2.442875,2.44535,2.447825,2.4503,2.452775,2.45525,2.457725,2.4602,2.462675,2.46515,2.467625,2.4701,2.472575,2.47505,2.477525,2.48,2.4804,2.4808,2.4812,2.4816,2.482,2.4824,2.4828,2.4832,2.4836,2.484,2.4844,2.4848,2.4852,2.4856,2.486,2.4864,2.4868,2.4872,2.4876,2.488,2.4884,2.4888,2.4892,2.4896,2.49,2.4904,2.4908,2.4912,2.4916,2.492,2.4924,2.4928,2.4932,2.4936,2.494,2.4944,2.4948,2.4952,2.4956,2.496,2.4964,2.4968,2.4972,2.4976,2.498,2.4984,2.4988,2.4992,2.4996,2.5)
  modifiedRecursiveCriterion = c(8,6.2,5.3,4.8,4.475,4.25,4.11,4,3.92,3.85,3.8,3.75,3.728,3.706,3.684,3.662,3.64,3.631,3.622,3.613,3.604,3.595,3.586,3.577,3.568,3.559,3.55,3.548,3.546,3.544,3.542,3.54,3.538,3.536,3.534,3.532,3.53,3.528,3.526,3.524,3.522,3.52,3.518,3.516,3.514,3.512,3.51,3.5098,3.5096,3.5094,3.5092,3.509,3.5088,3.5086,3.5084,3.5082,3.508,3.5078,3.5076,3.5074,3.5072,3.507,3.5068,3.5066,3.5064,3.5062,3.506,3.5058,3.5056,3.5054,3.5052,3.505,3.5048,3.5046,3.5044,3.5042,3.504,3.5038,3.5036,3.5034,3.5032,3.503,3.5028,3.5026,3.5024,3.5022,3.502,3.5018,3.5016,3.5014,3.5012,3.501,3.5008,3.5006,3.5004,3.5002,3.5)
  criterion = data.frame(sampleSize,nonRecursiveCriterion,modifiedRecursiveCriterion)
  
  
  #REMOVE INCOMPLETE ROWS (TRIALS) WITH MISSING DATA
  originalDataSize = dim(dataSet)[1]
  dataSet = dataSet[complete.cases(dataSet),] #only use data with complete rows!
  completeCasesDataSize = dim(dataSet)[1]
  if(originalDataSize > completeCasesDataSize)
    {
      print(c((originalDataSize > completeCasesDataSize), "  Rows with missing data were removed."))
    }
 
  
  Ncells = 0

        #IF MORE THAN ONE SPLIT VARIABLE
        if (length(splitvars) > 1){
          uniqueGroups = unique(dataSet[,splitvars[1]])  #GET THE FIRST SPLIT VARIABLE
          remainingVars = splitvars[2:length(splitvars)]  #GET THE REMAINING SPLIT VARIABLES
          count = 0
          for(i in 1: length(uniqueGroups)){  #FOR EACH LEVEL OF THE FIRST SPLIT VARIABLE RUN THE FUNCTION AGAIN FOR THE REMAINING SPLIT VARIABLES
            count = count+1
            tempList = pjRecursiveTrim2(dataSet[dataSet[as.character(splitvars[1])]==as.character(uniqueGroups[i]),],dv,remainingVars)
            if(count==1){  #IF IT IS THE FIRST TIME THROUGH CREATE NEW LIST
              trimmedData = tempList[[1]]
              Ncells = tempList[[5]]
            }
            else{
                 trimmedData = rbind(trimmedData,tempList[[1]])
                Ncells = Ncells+tempList[[5]]
            } #end else
          } # end for
        } #end if
        else {  #IF IT IS THE LAST TIME THROUGH SPLITTING - I.E. ONLY ONE VARIABLE TO SPLIT
          count = 0
          if(length(splitvars) == 1){
            uniqueGroups = unique(dataSet[,splitvars[1]]) #get the levels of the variable
            for(i in 1: length(uniqueGroups)){
              count = count+1
              Ncells = Ncells+1
              continue = 1 #flag to continue recursion
              reducedDataSet = dataSet[dataSet[as.character(splitvars[1])]==as.character(uniqueGroups[i]),]
              if(count==1)
              {
                while(continue >= 1)
                {
                  nSize = dim(reducedDataSet)[1]
                  if(nSize > 100) { nSize = 100}
                  if(nSize >= 4){  #IF THE SAMPLE IS LESS THAN 4 DON'T TRIM - AT LEAST THAT IS MY RECOLLECTION
                      for(i in 1:dim(criterion)[1]){
                        if(criterion$sampleSize[i]==nSize){critValue =criterion$modifiedRecursiveCriterion[i]}
                      }
                      reducedDataSet$zscore = abs(scale(reducedDataSet[,dv]))
                      #reducedDataSet$zscore2 = scale(reducedDataSet[,dv])
                      extremeScore = reducedDataSet[which.max(reducedDataSet$zscore),dv]
                      extremeScoreRemoved = reducedDataSet[-which.max(reducedDataSet$zscore),]
                      meanScore = mean(extremeScoreRemoved[,dv])
                      sdScore = sd(extremeScoreRemoved[,dv])
                      zExtreme = abs((extremeScore-meanScore)/sdScore)
                      if(round(zExtreme,3) >= critValue)
                      {
                        reducedDataSet = extremeScoreRemoved
                        continue = 1
                      }
                      else{continue = 0
                      #print(c(nSize, zExtreme, critValue))
                      }
                  }
                  else{ print("Sample size is less than 4.  No trimming has taken place.")
                    continue=0}
                }
                newData=reducedDataSet
              }
              else #IF IT IS THE SECOND OR GREATER LEVEL OF A VARIABLE
              {
                while(continue >= 1)
                {
                  nSize = dim(reducedDataSet)[1]
                  if(nSize > 100) { nSize = 100}
                  if(nSize >= 4){
                      for(i in 1:dim(criterion)[1]){
                        if(criterion$sampleSize[i]==nSize){critValue =criterion$modifiedRecursiveCriterion[i]}
                      }
                      reducedDataSet$zscore = abs(scale(reducedDataSet[,dv]))
                      #reducedDataSet$zscore2 = scale(reducedDataSet[,dv])
                      extremeScore = reducedDataSet[which.max(reducedDataSet$zscore),dv]
                      extremeScoreRemoved = reducedDataSet[-which.max(reducedDataSet$zscore),]
                      meanScore = mean(extremeScoreRemoved[,dv])
                      sdScore = sd(extremeScoreRemoved[,dv])
                      zExtreme = abs((extremeScore-meanScore)/sdScore)
                      
                      
                      if(round(zExtreme,3) >= critValue)
                      {
                        reducedDataSet = extremeScoreRemoved
                        continue = 1
                      }
                      else{continue = 0
                      #print(c(nSize, zExtreme, critValue))
                      }
                  }
                  else{ print("Sample size is less than 4.  No trimming has taken place.")
                    continue=0}
                }
                newData=rbind(newData,reducedDataSet)
              }
            }
            trimmedData = newData
          }
        }
  
  
  trimmedData$zscore <-NULL
  totalN = completeCasesDataSize
  rejected = (completeCasesDataSize -dim(trimmedData)[1])
  percentTrimmed = ((completeCasesDataSize -dim(trimmedData)[1])/completeCasesDataSize)*100
  returnedData = list(trimmedData, totalN, rejected, percentTrimmed, Ncells)
  return(returnedData)
  }