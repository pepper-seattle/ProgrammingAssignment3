rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  Data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  hospDf   <- as.data.frame(cbind(Data[, 2],  
                                  Data[, 7],  
                                  Data[, 11],  
                                  Data[, 17],  
                                  Data[, 23]), 
                            stringsAsFactors = FALSE)
  colnames(hospDf) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if (!state %in% hospDf[, "state"]) {
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(rank)) {
    st <- which(hospDf[, "state"] == state)
    extract <- hospDf[st, ]                     
    extract[, eval(outcome)] <- as.numeric(extract[, eval(outcome)])
    extract <- extract[order(extract[, eval(outcome)], extract[, "hospital"]), ]
    output <- extract[, "hospital"][rank]
  } else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      st <- which(hospDf[, "state"] == state)
      extract <- hospDf[st, ]    
      extract[, eval(outcome)] <- as.numeric(extract[, eval(outcome)])
      extract <- extract[order(extract[, eval(outcome)], extract[, "hospital"], decreasing = TRUE), ]
      output <- extract[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(output)
}