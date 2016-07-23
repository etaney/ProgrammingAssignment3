rankhospital <- function(state, outcome, num){
  
  ##options(warn=-1)
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  usedata <- data.frame(hospital=data[,2], statename=data[,7], HeartAttack=as.numeric(data[,11]), HeartFailure=as.numeric(data[,17]), Pneumonia=as.numeric(data[,23]))
  
  if(num=="best"){
    num <- 1
  }
  
  ## Check that state and outcome are valid
  if(sum(usedata$statename==state)==0){
    return("invalid state")
  }
  if(outcome != "heart failure" && outcome != "heart attack" && outcome != "pneumonia"){
    return("invalid outcome")
  }
  
  statedata <- usedata[usedata$statename==state,]
  
  ## Return hospital name in that state with lowest 30-day death rate
  if(outcome == "heart attack"){
    statedata <- statedata[!is.na(statedata[,3]),]
    if(num == "worst"){
      num <- length(statedata$hospital)
    }
    statedata <- statedata[order(statedata[,3], statedata$hospital),]
    return(statedata[num,1])
  }
  if(outcome == "heart failure"){
    statedata <- statedata[!is.na(statedata[,4]),]
    if(num == "worst"){
      num <- length(statedata$hospital)
    }
    statedata <- statedata[order(statedata[,4], statedata$hospital),]
    return(statedata[num,1])
  }
  if(outcome == "pneumonia"){
    statedata <- statedata[!is.na(statedata[,5]),]
    if(num == "worst"){
      num <- length(statedata$hospital)
    }
    statedata <- statedata[order(statedata[,5], statedata$hospital),]
    return(statedata[num,1])
  }
  ##options(warn=0)
  
}