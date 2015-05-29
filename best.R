
best <- function(state, outcome) {

## Examples

## Read outcome data
	outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	options( warn = -1 )
	outcome_data[, 11] <- as.numeric(outcome_data[, 11])
	outcome_data[, 17] <- as.numeric(outcome_data[, 17])
	outcome_data[, 23] <- as.numeric(outcome_data[, 23])
	outcome_data[, 13] <- as.numeric(outcome_data[, 13])
	outcome_data[, 19] <- as.numeric(outcome_data[, 19])
	outcome_data[, 25] <- as.numeric(outcome_data[, 25])
	options( warn = 0 )

## Check that state and outcome are valid
	valid_outcomes <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
	valid_states <- levels(factor(outcome_data[, 7]))

	isValidState <- function ( s ){
		for (vs in valid_states){
			if (s == vs) return(TRUE)
		}
		FALSE
	}

	isValidOutcome <- function ( o ){
		for (vo in valid_outcomes){			
			if (!is.na(valid_outcomes[o]))
				if ( valid_outcomes[o] == vo ) return(TRUE)
		}
		FALSE
	}

	if (!isValidState(state))
		stop(c("invalid state"))

	if (!isValidOutcome(outcome))
		stop(c("invalid outcome" ))

## Return hospital name in that state with lowest 30-day death
## rate
	stateh <- outcome_data[,7] == state ##Hospitales que aplican a la consulta
  voutcome<-valid_outcomes[outcome] ##Tipo de afeccion 
  bads<-is.na(outcome_data[stateh,voutcome])## Hospitales sin datos disponibles
  statehdf <-data.frame(outcome_data[stateh,2][!bads], ##Tabla de hospitales que aplican 
                        outcome_data[stateh,voutcome][!bads], 
                        stringsAsFactors = FALSE )
  statehdf[statehdf[,2]==min(statehdf[,2]),1]
#   outcome_data[stateh,11]
# 	valid_values<-!is.na(outcome_data[,valid_outcomes[outcome]][stateh])
# 	valid_rates <- outcome_data[,valid_outcomes[outcome]][stateh][valid_values]
# 	min_death<-min(valid_rates)
# 	valid_h <- outcome_data[,valid_outcomes[outcome]][stateh][valid_values] == min_death
# 	print(outcome_data[,2][stateh][valid_h])
# 	return(sort(outcome_data[,2][stateh][valid_h])[1])
}