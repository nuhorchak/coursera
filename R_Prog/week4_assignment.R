library(readr)
library(tidyverse)

hospital_data <- read_csv("R_Prog/hospital-data.csv")
outcome_of_care_measures <- read.csv("R_Prog/outcome-of-care-measures.csv", colClasses = "character")

#reading the data in as specified, introduces the following two columns as characters, therefore
#they must be converted to compater numeric values
outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- 
  as.numeric(outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <-
  as.numeric(outcome_of_care_measures$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)

outcome_of_care_measures[, 11] <- as.numeric(outcome_of_care_measures[, 11])
hist(outcome_of_care_measures[, 11])

# Write a function called best that take two arguments: 
#   the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file 
# and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 
# 30-day mortality for the specified outcome in that state

best <- function(state, outcome){
  #error handling
  if(!state %in% outcome_of_care_measures$State){
    stop("invalid state")
  }
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% outcome_list){
    stop("invalid outcome")
  }
  #conditional logic
  if(outcome == "heart attack"){
      outcome_of_care_measures %>% 
        select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack") %>% 
        filter(State == state) %>% 
        filter(!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) %>% 
        arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name) -> name
  }else if (outcome == "heart failure"){
    outcome_of_care_measures %>% 
      select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure") %>% 
      filter(State == state) %>% 
      filter(!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)) %>% 
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name) -> name
  }else{
    outcome_of_care_measures %>% 
      select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") %>% 
      filter(State == state) %>% 
      filter(!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)) %>% 
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name) -> name
  }
  return(name$Hospital.Name[1])
  #return(name)
}


# Write a function called rankhospital that takes three arguments: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument

rankhospital <- function(state, outcome, num = "best"){
  #error handling
  if(!state %in% outcome_of_care_measures$State){
    stop("invalid state")
  }
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% outcome_list){
    stop("invalid outcome")
  }
  #conditional logic
  if(outcome == "heart attack"){
    outcome_of_care_measures %>% 
      select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack") %>% 
      filter(State == state) %>% 
      filter(!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) %>% 
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name) -> name
  }else if (outcome == "heart failure"){
    outcome_of_care_measures %>% 
      select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure") %>% 
      filter(State == state) %>% 
      filter(!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)) %>% 
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name) -> name
  }else{
    outcome_of_care_measures %>% 
      select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") %>% 
      filter(State == state) %>% 
      filter(!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)) %>% 
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name) -> name
  }
  if(num=="best"){
    hospital <- dplyr::first(name$Hospital.Name)
  }else if(num=="worst"){
    hospital <- dplyr::last(name$Hospital.Name)
  }else{
    hospital <- dplyr::nth(name$Hospital.Name, num)
  }
  return(hospital)
}


# Write a function called rankall that takes two arguments: an outcome name (outcome) 
# and a hospital ranking (num). The function reads the outcome-of-care-measures.csv 
# file and returns a 2-column data frame containing the hospital in each state that has the 
# ranking specified in num. For example the function call rankall("heart attack", "best") 
# would return a data frame containing the names of the hospitals that are the best in their 
# respective states for 30-day heart attack death rates. 
# The function should return a value for every state (some may be NA). 
# The first column in the data frame is named hospital, which containsthe hospital name, 
# and the second column is named state, which contains the 2-character abbreviation for
# the state name

rankall <- function(outcome, num = "best"){
  #error handling
  outcome_list <- c("heart attack", "heart failure", "pneumonia")
  if(!outcome %in% outcome_list){
    stop("invalid outcome")
  }
  #conditional logic
  if(outcome == "heart attack"){
    outcome_of_care_measures %>% 
      select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack") %>% 
      filter(!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) %>% 
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name) -> name
  }else if (outcome == "heart failure"){
    outcome_of_care_measures %>% 
      select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure") %>% 
      filter(!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)) %>% 
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name) -> name
  }else{
    outcome_of_care_measures %>% 
      select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") %>% 
      filter(!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)) %>% 
      arrange(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name) -> name
  }
  if(num=="best"){
    hospital <- dplyr::first(name$Hospital.Name)
  }else if(num=="worst"){
    hospital <- dplyr::last(name$Hospital.Name)
  }else{
    hospital <- dplyr::nth(name$Hospital.Name, num)
  }
  return(hospital)
}



##### test

outcome_of_care_measures %>%
  select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") %>% 
  filter(!is.na(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)) %>% 
  group_by(State, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) -> rankall_test
