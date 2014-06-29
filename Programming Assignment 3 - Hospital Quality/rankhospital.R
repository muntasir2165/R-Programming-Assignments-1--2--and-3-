rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        #read in the desired data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        #check if the state and outcomes are valid
        states <- data[ , 7]
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        if ((state %in% states) == FALSE) {
                stop(print("invalid state"))
        }
        else if ((outcome %in% outcomes) == FALSE) {
                stop(print("invalid outcome"))
        }
        
        #get the subset of the data with the desired state
        new_data <- subset(data, State == state)
        
        #get the desired outcome column from the data file
        if (outcome == "heart attack") {
                outcome_column <- 11
        }
        else if (outcome == "heart failure") {
                outcome_column <- 17
        }
        else {
                outcome_column <- 23
        }
        
        #if num is greater that the number of hospitals in the desired state,
        # return NA
        if (is.numeric(num) == TRUE) {
                if (length(data[,2]) < num) {
                        return(NA)
                }
        }
        
        #get rid of the NA's in the desired outcome column
        new_data[, outcome_column] <- as.numeric(new_data[,outcome_column])
        bad <- is.na(new_data[, outcome_column])
        desired_data <- new_data[!bad, ]
        
        #arrange the modified dataframe in ascending order of the outcome values
        outcome_column_name <- names(desired_data)[outcome_column]
        hospital_column_name <- names(desired_data)[2]
        index <- with(desired_data, order(desired_data[outcome_column_name], desired_data[hospital_column_name]))
        ordered_desired_data <- desired_data[index, ]
        
        #if nume is either "best" or "worst", then interpret it to the
        #corresponding numerical value
        if (is.character(num) == TRUE) {
                if (num == "best") {
                        num = 1
                }
                else if (num == "worst") {
                        num = length(ordered_desired_data[, outcome_column])
                }
        }
        #return the hospital name with the outcome ranking of num
        ordered_desired_data[num, 2]
}
