
best <- function(state, outcome) {

## Examples
## best("TX", "heart failure")

## Read outcome data

	outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	outcome_data[, 11] <- as.numeric(outcome_data[, 11])
	valid_outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
	valid_states <- levels(factor(outcome_data[, 7]))

## Check that state and outcome are valid
	##print(state)
	##print(valid_states)

	##print(outcome)
	##print(valid_outcomes)

	isValidState <- function ( s ){
		for (vs in valid_states){
			if (s == vs) return(TRUE)
		}
		FALSE
	}

	isValidOutcome <- function ( o ){
		for (vo in valid_outcomes)
			if ( o == vo ) return(TRUE)
		FALSE
	}

	if (!isValidState(state))
		stop(c("invalid state"))

	if (!isValidOutcome(outcome))
		stop(c("invalid outcome" ))

## Return hospital name in that state with lowest 30-day death
## rate
stateh <- outcome_data[,7] == state
valid_values<-!is.na(outcome_data[,11][stateh])
valid_rates <- outcome_data[,11][stateh][valid_values]
min_death<-min(valid_rates)
c(outcome_data[,2][valid_values],outcome_data[,11][valid_values])


}