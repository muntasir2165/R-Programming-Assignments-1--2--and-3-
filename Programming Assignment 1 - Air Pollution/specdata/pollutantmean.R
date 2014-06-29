pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
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
        
        accumulator <- 0
        total <- 0  
        for(i in filename) {
                airquality <- read.csv(i)
                good <- complete.cases(airquality[pollutant])
                #good <- complete.cases(airquality[[pollutant]])
                airquality <- airquality[good, ]
                total <- total + nrow(airquality)
                #accumulator <- accumulator + sum(airquality[pollutant], na.rm = TRUE)
                # We need [[]] around pollutant instead of [] since airquality[pollutant]
                # is a data.frame but we need a vector here. Please note that using either
                #[[]] or [] gives the same results as the test cases but only [[]] successfully
                #passes the submit() script
                accumulator <- accumulator + sum(airquality[[pollutant]], na.rm = TRUE)
        }
        accumulator/total
}
