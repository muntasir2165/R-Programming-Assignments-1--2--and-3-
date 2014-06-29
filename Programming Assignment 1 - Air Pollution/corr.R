corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        id = 1:332
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
        }
        #print(filename)
        
        #empty vector
        result <-vector(mode="numeric", length=0)
        for(i in seq(filename)) {
                airquality <- read.csv(filename[i])
                good <- complete.cases(airquality)
                airquality <- airquality[good, ]
                if (nrow(airquality) > threshold) {
                        # We need [[]] around pollutant instead of [] since airquality["sulfate"]
                        # is a data.frame but we need a vector here. Hence, [[]]. Please note that using either
                        #[[]] or [] gives the same results as the test cases
                        correlation <- cor(airquality[["sulfate"]], airquality[["nitrate"]])
                        result <- append(result, correlation)
                        #print(correlation)
                }
               }
        result
}
