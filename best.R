best <- function(state, outcome) {

  ## Read the outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses="character")

  ## Select only columns of interest
  data <- data[,c(2,7,11,17,23)]

  ## Check that state is valid
  if(!(state %in% levels(factor(data$State)))){
    stop("invalid state")
  }

  ## Check that the outcome is valid
  if(!(outcome == "heart attack" ||
       outcome == "heart failure" ||
       outcome == "pneumonia"
  )){
       stop("invalid outcome")
  }
  ## Filter for only state of interest
  data <- data[data$State==state,]
  data <- data[,c(1,3,4,5)]

  ## Filter data for only outcome of interest
  str(data)
  if(outcome == "heart attack"){
    data <- data[,c(1,2)]
  } else if(outcome == "heart failure"){
    data <- data[,c(1,3)]
  } else if(outcome == "pneumonia"){
    data <- data[,c(1,4)]
  }
  names(data)[2] <- "Deaths"
  data[,2] <- suppressWarnings(as.numeric(data[,2]))
  data <- data[!is.na(data$Deaths),]

  data = data[order(data$Deaths, data$Hospital.Name),]

  ## Return hospital name in that state with lowest 30-day death rate
  return (data$Hospital.Name[1])
}
