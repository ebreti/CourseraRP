pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  dirOK <- FALSE
  if(file.exists(directory)) {
    if(file.info(directory)$isdir) {
      dirOK <- TRUE
    }
  }
  if(!dirOK) {
    print(paste("not a directory ::",directory))
  }
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  pollOK <- FALSE
  target <- c("sulfate", "nitrate")
  if(pollutant %in% target) {
    pollOK <- TRUE
  } else {
    print(paste("pollutant not mesured ::",pollutant))
  }
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  if(dirOK){
    fileNF <- vector(mode="character")
    measures <- vector(mode="numeric")
    measures[1] <- NA
    for(idmon in id) {
      idm3 <- paste("000",idmon,sep="")
      file <- paste(substr(idm3, nchar(idm3)-3+1, nchar(idm3)),".csv",sep="")
      fullname <- paste(directory,file,sep="/")
      if(file.exists(fullname)) {
        monitor_data <- read.csv(fullname)
        measures <- c(measures, monitor_data[[pollutant]])
      } else {
        fileNF[length(fileNF)+1] <- file
      }
    }
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)

    print(mean(measures,na.rm=TRUE))
    if(length(fileNF) > 0) {
      print("monitors not found")
      print(fileNF)
    }
  }
}