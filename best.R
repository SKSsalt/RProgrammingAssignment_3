best <- function(state, outcome) {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcomevector <- c("heart attack" , "heart failure" , "pneumonia")

	## Check that state and outcome are valid
	if(!(outcome %in% outcomevector)){
		stop("invalid outcome")
	}

	if(!(state %in% data$State)){
		stop("invalid state")
	}

	## rate
	dataState <- subset(data, data$State == state)

	if(outcome == outcomevector[1]){
      	outMin <- min(dataState[,11], na.rm=TRUE)
    	outcomeData <- subset(dataState, dataState[,11] == outMin)
    	result <- outcomeData[2]
    	return(result)
	}
	else if (outcome == outcomevector[2]){
      	outMin <- min(dataState[,17], na.rm=TRUE)
        outcomeData <- subset(dataState, dataState[,17] == outMin)
      	result <- outcomeData[2]
      	return(result)
	}
	else {
      	outMin <- min(dataState[,23], na.rm=TRUE)
        outcomeData <- subset(dataState, dataState[,23] == outMin)
      	result <- outcomeData[2]
      	return(result)
	}
}