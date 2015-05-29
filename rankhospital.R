rankhospital <- function(state, outcome, num = "best") {
  ## Examples
  #   > rankhospital("TX", "heart failure", 4)
  #   [1] "DETAR HOSPITAL NAVARRO"
  #   > rankhospital("MD", "heart attack", "worst")
  #   [1] "HARFORD MEMORIAL HOSPITAL"
  #   > rankhospital("MN", "heart attack", 5000)
  #   [1] NA
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
  
  #Evaluating ranking values
  stateh <- outcome_data[,7] == state ##Hospitales del estado en cuestion que aplican a la consulta
  voutcome<-valid_outcomes[outcome] ##Tipo de afeccion a ser evaluada
  bads<-is.na(outcome_data[stateh,voutcome]) ##Identificacion de hospitales sin datos disponibles
  quantity_stateh <- sum(!bads) ##Cantidad de hospitales que realmente aplican
  
  if (num =="best") num = 1
  else if (num == "worst") {num = quantity_stateh}
  else if (num > quantity_stateh) { return(NA); }
  
  statehdf <-data.frame(hospital=outcome_data[stateh,2][!bads], ##Tabla de hospitales que aplican 
                        rate=outcome_data[stateh,voutcome][!bads], 
                        stringsAsFactors = FALSE )
  statehdf_ordered<-statehdf[order(statehdf$rate),]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

  statehdf_ordered[num,]$hospital
  
}