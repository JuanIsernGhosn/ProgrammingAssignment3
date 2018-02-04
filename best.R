best <- function(state, outcome) {
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
    
    data <- data[!is.na(suppressWarnings(as.numeric(data[,coln]))),]
    
    result <- data$Hospital.Name[as.numeric(data[,coln]) == min(as.numeric(data[,coln]))]
    
    sort(result)[1]
    
}