rankall <- function(outcome, num = "best"){
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcomevector <- c("heart attack" , "heart failure" , "pneumonia")

	## Check that outcome are valid
	if(!(outcome %in% outcomevector)){
		stop("invalid outcome")
	}
	
	if(outcome == outcomevector[1]){
      	outcomeColumn <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	}
	else if (outcome == outcomevector[2]){
      	outcomeColumn <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
	}
	else {
      	outcomeColumn <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	}
	
	## For each state, find the hospital of the given rank
	# Create a subset of relevant data
	data <- data[,c("State","Hospital.Name",outcomeColumn)]
	data <- data[!is.na(data[outcomeColumn]),]
	
	# Sort data: State -> outcome -> Hospital Name
	data <- data[order(data$State, data[outcomeColumn], data$Hospital.Name),]
	
	# aggregate by state, choosing the row that corresponds to the rank num
	data_state <- aggregate(data, by=list(data$State), function(x) {
					if (!is.numeric(num)) {
						if (num == "best") {
							num <- 1
						} 
						else if (num == "worst") {
							num <- length(x)
						} 
						else {
							stop("invalid number")
						} 
					}
					x[num]
				})

	rank <- data_state[,c(3,1)]
	names(rank) <- c("hospital","state")

  return(rank)
}