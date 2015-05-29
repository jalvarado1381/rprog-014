rankhospital <- function(state, outcome, num = "best") {
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
  
  quantity_stateh <- outcome_data[,7] == state
  if (sum > quantity_stateh) return(NA)
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  
}