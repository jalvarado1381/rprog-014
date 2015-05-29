rankall <- function(outcome, num = "best") {
  
  ##Example
  #head(rankall("heart attack", 20), 10)
  #tail(rankall("pneumonia", "worst"), 3)
  
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  options( warn = -1 )
  outcome_data[, 11] <- as.numeric(outcome_data[, 11])
  outcome_data[, 17] <- as.numeric(outcome_data[, 17])
  outcome_data[, 23] <- as.numeric(outcome_data[, 23])  
  options( warn = 0 )
  states <- levels(factor(outcome_data[, 7]))
  ranked_states <- data.frame()
  
  ## Check that num and outcome are valid
  
  valid_outcomes <- c("heart attack" = 11, "heart failure"=17, "pneumonia"=23)  
  
  isValidOutcome <- function ( o ){
    for (vo in valid_outcomes){    	
      if (!is.na(valid_outcomes[o]))
        if ( valid_outcomes[o] == vo ) return(TRUE)
    }
    FALSE
  }
  
  if (!isValidOutcome(outcome))
    stop(c("invalid outcome" ))
  
  voutcome <- valid_outcomes[outcome] ##Tipo de afeccion a ser evaluada
  
  for ( s in states ){
    
    stateh <- outcome_data[,7] == s ## Identificacion de Hospitales en el estado
    bads<-is.na(outcome_data[stateh,voutcome]) ##Identificacion de hospitales sin datos disponibles
    quantity_stateh <- sum(!bads) ##Calcular la cantidad de hospitales que realmente aplican
    num1 = num
    
    ## Defining rankings
    if (num1 == "best") {
      num1 = 1
    }
    else if (num1 == "worst") {num1 = quantity_stateh}
    else if (num1 > quantity_stateh) { num1 = NA }
    
    statehdf <-data.frame(hospital=outcome_data[stateh,2][!bads], ##Tabla de hospitales que aplican 
                          rate=outcome_data[stateh,voutcome][!bads], 
                          stringsAsFactors = FALSE )
    
    statehdf_ordered<-statehdf[order(statehdf$rate),]
    print(c(quantity_stateh, num1,statehdf_ordered[num,]$hospital,s))
    ranked_states[s,1] <- c(statehdf_ordered[num,]$hospital)
    ranked_states[s,2] <- c(s)
    
  }

  
  ## For each state, find the hospital of the given rank

  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  names(ranked_states)<-c("hospital","state")
  ranked_states
}
