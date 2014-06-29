complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        filename <- vector(mode="character", length=length(id))
        for(i in seq_along(id)) {
                x <- id[i]
                id_string <- toString(x)
                if (x >= 1 && x <= 9) {  
                        monitor <- paste("00", id_string, ".csv", sep="")
                }
                else if (x >= 10 && x <= 99) {
                        id_string <- toString(x)
                        monitor <- paste("0", id_string, ".csv", sep="")
                }
                else {
                        id_string <- toString(x)
                        monitor <- paste(id_string, ".csv", sep="") 
                }
                filename[i] <- monitor      
                #print(monitor)
        }

        #accumulator <- 0
        nobs <- vector(mode="integer", length=length(id)) 
        for(i in seq(filename)) {
                airquality <- read.csv(filename[i])
                good <- complete.cases(airquality)
                airquality <- airquality[good, ]
                nobs[i] <- nrow(airquality)
                #accumulator <- accumulator + sum(airquality[pollutant], na.rm = TRUE)
        }
        result = data.frame(id, nobs)
        result
}