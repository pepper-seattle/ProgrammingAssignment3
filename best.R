best <- function(state, outcome) {
  ## Read outcome data
  Data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Create a data frame with all necessary information
  hospDf <- as.data.frame(cbind(Data[, 2],
                                Data[, 7],
                                Data[, 11],
                                Data[, 17],
                                Data[, 23]),
                          stringsAsFactors = FALSE)
  
  colnames(hospDf) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% hospDf[, "state"]) {
    stop('invalid state')
  } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
    stop('invalid outcome')
  } else {
    st <- which(hospDf[, "state"] == state)
    extract <- hospDf[st, ]
    nm <- as.numeric(extract[, eval(outcome)])
    min_val <- min(nm, na.rm = TRUE)
    result <- extract[, "hospital"][which(nm == min_val)]
    output <- result[order(result)]
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  return(output)
}