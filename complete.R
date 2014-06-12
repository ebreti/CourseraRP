complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  idwk <- numeric(1)
  idwk <- NA
  nobs <- 0
  result <- data.frame(idwk, nobs)
  dirOK <- FALSE
  if(file.exists(directory)) {
    if(file.info(directory)$isdir) {
      dirOK <- TRUE
    }
  }
  if(!dirOK) {
    print(paste("not a directory ::",directory))
  }

  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  if(dirOK){
    fileNF <- vector(mode="character")
    for(idmon in id) {
      idm3 <- paste("000",idmon,sep="")
      file <- paste(substr(idm3, nchar(idm3)-3+1, nchar(idm3)),".csv",sep="")
      fullname <- paste(directory,file,sep="/")
      if(file.exists(fullname)) {
        monitor_data <- read.csv(fullname)
        if(! is.na(result$idwk[nrow(result)])) {
          idwk <- NA
          nobs <- 0
          result <- rbind(result, data.frame(idwk, nobs)) 
        }
        result$idwk[nrow(result)] <- idmon
        for(line in 1:nrow(monitor_data)) {
          if(! is.na(monitor_data$sulfate[line]) && ! is.na(monitor_data$nitrate[line])) {
            nobs <- nobs + 1
          }
        }
        result$nobs[nrow(result)] <- nobs
      } else {
        fileNF[length(fileNF)+1] <- file
      }
    }
  }  
    ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  colnames(result) <- c("id", "nobs")
  result
}
