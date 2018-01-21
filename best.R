## Coursera R Programming
## Week 4; Programming Assignment 3
## Michael Assink
## 1/17/2018

## best(state, outcome) 
##    is a function which will return the name of the hospital
##    in the state provided with the lowest 30-day death rate
##    it will validate that state and outcome are valid

source("rankhospital.R")

best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate

  
  # rankhospital defaults to num="best"
  return(rankhospital(state, outcome)) # , "best"))
}