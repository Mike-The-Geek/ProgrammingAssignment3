## Coursera R Programming
## Week 4; Programming Assignment 3 part 3
## Michael Assink
## 1/20/2018

## rankall(state, outcome, num) 
##    is a function which will return the name of the hospital
##    in the state provided with the ranking for the provided outcome
##    which matches the provided num variable
##
##    it will validate that state and outcome are valid
##    valid inputs for num:  "best", "worst" or an int greater than 0
##    if num is > lowest ranked hospital, NA is returned


# using the plyr package as recommended by Igreski
# https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-sortFunctionsExample.md

library(plyr)

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that outcome is valid
  ## Read outcome data
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  
  # read in file in current working directory
  hc_ooc <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available", stringsAsFactors=FALSE)
  
  ## begin validation

  # stop if invalid outcome
  # \heart attack", \heart failure", or \pneumonia".
  if(!( outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")) {
    stop("invalid outcome")
  }
  
  if (num == "best") { 
    num <- 1
  }
  
  # best is now 1, validate "worst" or (positive && num mod 1 == 0   [is.integer]
  #  
  if (!(num == "worst" || (num > 0 && num %% 1 == 0) )) {
    stop("invalid num, requires positive integer, 'best', or 'worst'")
  }
  ## end validation
  
  

  ## we'll use subset with a different column
  ## [2] "Hospital.Name"                                            
  ## [7] "State" 
  ## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  ## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  ## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
  if (outcome == "heart attack") {
    allstates <- subset(hc_ooc, select = c(2, 7, 11))
  }
  if (outcome == "heart failure") {
    allstates <- subset(hc_ooc, select = c(2, 7, 17))
  }
  if (outcome == "pneumonia") {
    allstates <- subset(hc_ooc, select = c(2, 7, 23))
  }
  
  # rename these to something simple
  names(allstates) <- c("hospital", "state", "measure")
  
  # leaving the measures list as character messes up the ranking
  allstates$measure <- as.numeric(as.character(allstates$measure))
  
  # eliminate NAs
  allstates <- allstates[complete.cases(allstates),]
  
  # worst = last
  if (num == "worst") { num <- nrow(allstates)}
  
  # go ahead and bail if we don't have enough rows 
  if (num > nrow(allstates)) {
    return(NA)
  }

  # using order(), reorder the frame
  ranked <- allstates[order(allstates[3], allstates[1]) , ]

  # return the hospital name of the numteenth row  
  return(ranked[num , 1])
}