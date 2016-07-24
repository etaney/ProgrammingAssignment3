
best <- function(state, outcome){
  options(warn=-1)
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  usedata <- data.frame(hospital=data[,2], statename=data[,7], HeartAttack=as.numeric(data[,11]), HeartFailure=as.numeric(data[,17]), Pneumonia=as.numeric(data[,23]))
  usedata[outcome] <- as.numeric(usedata[outcome])
  ##print("I read the data")
  
  
  ## Check that state and outcome are valid
  if(sum(usedata$statename==state)==0){
    return("invalid state")
  }
  if(outcome != "heart failure" && outcome != "heart attack" && outcome != "pneumonia"){
    return("invalid outcome")
  }
  else{
  ##  print("Valid Data")
  }
  
  statedata <- usedata[usedata$statename==state,]
  
  ## Return hospital name in that state with lowest 30-day death rate
  if(outcome == "heart failure"){
    statedata <- statedata[!is.na(statedata[,4]),]
    return(statedata$hospital[statedata[,4] == min(statedata[,4])])
  }
  if(outcome == "heart attack"){
    statedata <- statedata[!is.na(statedata[,3]),]
    return(statedata$hospital[statedata[,3] == min(statedata[,3])])
  }
  if(outcome == "pneumonia"){
    statedata <- statedata[!is.na(statedata[,5]),]
    return(statedata$hospital[statedata[,5] == min(statedata[,5])])
  }
  options(warn=0)
  
}