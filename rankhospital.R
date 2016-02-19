rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcomevector <- c("heart attack" , "heart failure" , "pneumonia")
	
	## Check that state and outcome are valid
	if(!(state %in% data$State)){
		stop("invalid state")
	}
	
	if(!(outcome %in% outcomevector)){
		stop("invalid outcome")
	}

	## Set the outcome column
	if(outcome == outcomevector[1]){
      	outcomeColumn <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	}
	else if (outcome == outcomevector[2]){
      	outcomeColumn <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	}
	else {
      	outcomeColumn <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	}
	
	## Data arrangement
	data_state_outcome <- data[,c("State","Hospital.Name",outcomeColumn)]
	ordered_data_state_outcome <- order(data_state_outcome[outcomeColumn], data_state_outcome$Hospital.Name, na.last=NA)
   
	## Return hospital name
    if (num == "best") {
     #   return(as.character(data_state_outcome$Hospital.Name[ordered_data_state_outcome[1]]))
		return(as.character(ordered_data_state_outcome[1]))
    } 
	else if (num == "worst") {
	return(as.character(data_state_outcome$Hospital.Name[ordered_data_state_outcome[length(ordered_data_state_outcome)]]))
    } 
	else if (is.numeric(num)) {
		if(is.numeric(num) <= length(ordered_data_state_outcome)){
			return(as.character(data_state_outcome$Hospital.Name[ordered_data_state_outcome[num]]))
		}
		else{
			return('NA')
		}
    }
	else{
		stop("invalid number")
	}

## 30-day death rate
}