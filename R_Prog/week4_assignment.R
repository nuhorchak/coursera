library(readr)
library(tidyverse)

hospital_data <- read_csv("hospital-data.csv")
outcome_of_care_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


outcome_of_care_measures[, 11] <- as.numeric(outcome_of_care_measures[, 11])
hist(outcome_of_care_measures[, 11])

# Write a function called best that take two arguments: 
#   the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file 
# and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 
# 30-day mortality for the specified outcome in that state

best <- function(state, outcome){
  if(outcome == "heart attack"){
    name <- outcome_of_care_measures %>% 
      select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack") %>% 
      filter("State" == state) %>% top_n(1) %>% select("Hospital.Name")
  }else if (outcome == "heart failure"){
    name <- outcome_of_care_measures %>% 
      select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure") %>% 
      filter("State" == state)%>% top_n(1) %>% select("Hospital.Name")
  }else{
    name <- outcome_of_care_measures %>% 
      select("State", "Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") %>% 
      filter("State" == state) %>% top_n(1) %>% select("Hospital.Name")
  }
  return(name[[1]])
}

best("TX", "heart attack")
