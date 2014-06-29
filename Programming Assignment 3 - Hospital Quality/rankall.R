rankall <- function(outcome, num = "best") {
        ## Read outcome data
        
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        #read in the desired data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #create a list of states and initialize a character array to hold the
        #required hospital names
        state <- levels(factor(data[, 7]))
        hospital <- vector(mode="character") 
        
        for (i in seq(state)) {
                hospital[i] <- rankhospital(state[i], outcome, num)
        }
        data.frame(hospital, state)
}
