corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  id <- 1:332
  idwk <- id
  corr <- rep(0, length(idwk))
  uset <- rep(FALSE, length(idwk))
  result <- data.frame(idwk, corr, uset)
  dirOK <- FALSE
  if(file.exists(directory)) {
    if(file.info(directory)$isdir) {
      dirOK <- TRUE
    }
  }
  if(!dirOK) {
    print(paste("not a directory ::",directory))
  } else {
    fileNF <- vector(mode="character")
    for(idmon in id) {
      idm3 <- paste("000",idmon,sep="")
      file <- paste(substr(idm3, nchar(idm3)-3+1, nchar(idm3)),".csv",sep="")
      fullname <- paste(directory,file,sep="/")
      if(file.exists(fullname)) {
        monitor_data <- read.csv(fullname)
        sulfate <- numeric(1)
        nitrate <- numeric(1)
        usec <- FALSE
        specific <- data.frame(sulfate, nitrate, usec)
        for(line in 1:nrow(monitor_data)) {
          if(! is.na(monitor_data$sulfate[line]) && ! is.na(monitor_data$nitrate[line])) {
            sulfate <- monitor_data$sulfate[line]
            nitrate <- monitor_data$nitrate[line]
            usec <- c(TRUE)
            specific <- rbind(specific, data.frame(sulfate, nitrate, usec))
          }
        }
        specific <- subset(specific, specific$usec)
        if(nrow(subset(specific, specific$usec)) > threshold) {
          result$corr[idmon] <- cor(specific$sulfate, specific$nitrate)
          result$uset[idmon] <- TRUE
        }
      } else {
        fileNF[length(fileNF)+1] <- file
      }
    }
  }  
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  result <- subset(result, result$uset)
  result$corr
}
