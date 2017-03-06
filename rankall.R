rankall <- function(outcome, num = "best"){
  ## Read outcome data
  Data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospDf   <- as.data.frame(cbind(Data[, 2],  
                                  Data[, 7],  
                                  Data[, 11],  
                                  Data[, 17],  
                                  Data[, 23]), 
                            stringsAsFactors = FALSE)
  colnames(hospDf) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  hospDf[, eval(outcome)] <- as.numeric(hospDf[, eval(outcome)])
  
  ## Check that state and outcome are valid
  
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(num)) {
    byState <- with(hospDf, split(hospDf, state))
    ordered  <- list()
    for (i in seq_along(byState)){
      byState[[i]] <- byState[[i]][order(byState[[i]][, eval(outcome)], 
                                         byState[[i]][, "hospital"]), ]
      ordered[[i]]  <- c(byState[[i]][num, "hospital"], byState[[i]][, "state"][1])
    }
    result <- do.call(rbind, ordered)
    output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  } else if (!is.numeric(num)) {
    if (num == "best") {
      byState <- with(hospDf, split(hospDf, state))
      ordered  <- list()
      for (i in seq_along(byState)){
        byState[[i]] <- byState[[i]][order(byState[[i]][, eval(outcome)], 
                                           byState[[i]][, "hospital"]), ]
        ordered[[i]]  <- c(byState[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else if (num == "worst") {
      byState <- with(hospDf, split(hospDf, state))
      ordered  <- list()
      for (i in seq_along(byState)){
        byState[[i]] <- byState[[i]][order(byState[[i]][, eval(outcome)], 
                                           byState[[i]][, "hospital"], 
                                           decreasing = TRUE), ]
        ordered[[i]]  <- c(byState[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    } else {
      stop('invalid num')
    }
  }
  return(output)
}