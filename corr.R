corrle <- function(directory ,threshold = 0){                                ## Function for calculating the correlation between two variables 
                list_1 <- list.files(path = directory , pattern =".csv")     ##Make a list containing the path of the files we want to calculate the link in and the type of these files
                df <- complete(directory)                                    ##Set file path and return to a variable
                ds<- df[df["nub"] > threshold,] $id                          ##select id
                corrr <- numeric()                                           ##Define a variable that denotes the association and make it a numeric value
                for(i  in  ds){                                              ##Make a for loop to specify the variables to calculate the correlation
                  mydata <- read.csv(list_1[i])                              ##Storing files in a variable in order to calculate the correlation
                  dff <- mydata[complete.cases(mydata), ]                    ##Determine the state of the files
                  corrr <- c(corrr , cor(dff$sulfate , dff$nitrate))         ##Calculating the correlation between two variables (columns within the data)
                }
                return(corrr)
}