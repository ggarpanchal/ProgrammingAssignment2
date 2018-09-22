## coursera : R programming Assignment3:2

best <- function(state, outcome = c("heart attack", "heart failure", "pneumonia")) {
  
  ## Read outcome data
  outcome_data_read <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  all_State <- unique(outcome_data_read$State)
  
  ##if else loop check given state & outcome is valid when both are valid then call find_best function
  if (match(state , all_State, nomatch = "0") != 0){
      
      ##if the given state name is right then find given outcome is right or not ..?
      outcome_temp = c("heart attack", "heart failure", "pneumonia")
      if(match(outcome , outcome_temp, nomatch = "0") != 0){
          
          ##if given outcome name is right then finction go ahead
          ## call the find_best function with required input oubject
          find_best(x = outcome_data_read, y = state, outcome)
        
      }else{
        ##this is stop function for given outcome is wrong
        stop("invalid outcome")
      }
  }else{
    ##this is stop function for given state name is wrong
    stop("invalid state")
  }
}

## this function find the best hospital

find_best <- function(x, y, z){

  state_vise_outcome <- x[x$State == y,]
  state_vise_outcome <- state_vise_outcome[with(state_vise_outcome,order(Hospital.Name)),]
  if(z == "heart attack"){
    state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(
                                      state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    T1 <- state_vise_outcome$Hospital.Name[which
                                     (state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(
                                       state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack ,na.rm = T) )[1]]
    return(T1)
  }  
  if(z == "heart failure"){
    state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(
                                      state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    T2 <- state_vise_outcome$Hospital.Name[which
                                     (state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(
                                       state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure ,na.rm = T) )[1]]
    return(T2)
  } 
  if(z == "pneumonia"){
    state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(
                                      state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) 
    T3 <- state_vise_outcome$Hospital.Name[which
                                     (state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(
                                       state_vise_outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia ,na.rm = T) )[1]]
     return(T3)
  } 
}
  
