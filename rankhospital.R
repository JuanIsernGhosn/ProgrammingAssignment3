rankhospital <- function(state, outcome, n) {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if(state %in% data$State){
        data <- data[which(data$State == state),]
    } else {
        stop("invalid state")
    }
    
    if(identical(outcome,"heart attack")){
        coln <- 11
    } else if (identical(outcome,"heart failure")){
        coln <- 17
    } else if (identical(outcome,"pneumonia")){
        coln <- 23
    } else {
        stop("invalid outcome")
    }
    
    data[, coln] <- suppressWarnings(as.numeric(data[,coln]))
    
    data <- data[!is.na(data[,coln]),]
    
    result <- data[order(data[,coln],data[,2]),]
    
    if(identical(n,"best")){
        result[1,2]
    } else if (identical(n,"worst")){
        result[nrow(result),2]
    } else if (is.numeric(n)){
        result[n,2]
    } else {
        stop("invalid number")
    }
}
    
    