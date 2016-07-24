rankall <- function( outcome, num="best"){
  
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  usedata <- data.frame(hospital=data[,2], statename=data[,7], HeartAttack=as.numeric(data[,11]), HeartFailure=as.numeric(data[,17]), Pneumonia=as.numeric(data[,23]))
  states <- unique(usedata$statename)
  
  if(num=="best"){
    num <- 1
  }
  
  ## Check that state and outcome are valid
  if(outcome != "heart failure" && outcome != "heart attack" && outcome != "pneumonia"){
    return("invalid outcome")
  }
  
  i <- 1
  resultlist <- data.frame(stateid=character(),hospital=character())
 
  while (i <= length(states)){
    
    state <- states[i]
   
    
    statedata <- usedata[usedata$statename==states[i],]
  
    ## Return hospital name in that state with lowest 30-day death rate
    if(outcome == "heart attack"){
      statedata <- statedata[!is.na(statedata[,3]),]
      if(num == "worst"){
        num <- length(statedata$hospital)
      }
      statedata <- statedata[order(statedata[,3], statedata$hospital),]
    }
    if(outcome == "heart failure"){
      statedata <- statedata[!is.na(statedata[,4]),]
      if(num == "worst"){
        num <- length(statedata$hospital)
      }
      statedata <- statedata[order(statedata[,4], statedata$hospital),]
    }
    if(outcome == "pneumonia"){
      statedata <- statedata[!is.na(statedata[,5]),]
      if(num == "worst"){
        num <- length(statedata$hospital)
      }
      statedata <- statedata[order(statedata[,5], statedata$hospital),]
    }
    resultitem <- data.frame(stateid=states[i], hospital=statedata[num,1])
    print(resultitem)
    resultlist <- rbind(resultlist, resultitem)
    i <- i+1
  }
  return(resultlist)
}