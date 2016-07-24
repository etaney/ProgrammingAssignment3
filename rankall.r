rankall <- function(outcome, num="best"){
  
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.string="Not Available", stringsAsFactors=FALSE)
  usedata <- data.frame(hospital=data[,2], statename=data[,7], HeartAttack=data[,11], HeartFailure=data[,17], Pneumonia=data[,23], stringsAsFactors=FALSE)
  colnames(usedata) <- c("hospital", "statename", "heart attack", "heart failure", "pneumonia")
  usedata[,outcome] <- as.numeric(usedata[,outcome])
  states <- sort(unique(usedata$statename))
  
  if(num=="best"){
    numnum <- 1
  }
  if(num != "best" && num !="worst"){
    numnum <- num
  }
  
  ## Check that state and outcome are valid
  if(outcome != "heart failure" && outcome != "heart attack" && outcome != "pneumonia"){
    return("invalid outcome")
  }
  
  i <- 1
  resultlist <- data.frame(hospital=character(),stateid=character())
 
  while (i <= length(states)){
    
    state <- states[i]
   
    statedata <- usedata[usedata$statename==states[i],]
    
      statedata <- statedata[order(statedata[outcome], statedata$hospital,na.last=NA),]
      if(num == "worst"){
        numnum <- nrow(statedata)
      }
      resultitem <- data.frame(hospital=statedata[numnum,1],stateid=states[i])
   

    resultlist <- rbind(resultlist, resultitem)
    
    i <- i+1
  }
  return(resultlist)
}