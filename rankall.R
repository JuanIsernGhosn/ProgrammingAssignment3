rankall <- function(outcome,  num = "best") {
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if(identical(outcome,"heart attack")){
        coln <- 11
    } else if (identical(outcome,"heart failure")){
        coln <- 17
    } else if (identical(outcome,"pneumonia")){
        coln <- 23
    } else {
        stop("invalid outcome")
    }
    
    data[, coln] <- suppressWarnings(as.numeric(data[, coln]))
    
    data <- split(x = data,f = data$State)
    
    result <- data.frame(hospital = character(), state = character())
    
    for (stateframe in data) {
        stateframe <- stateframe[!is.na(stateframe[,coln]),]
        stateframe <- stateframe[order(stateframe[,coln],stateframe[,2]),]
        
        if(identical(num,"best")){
            result <- rbind(result,stateframe[1,c(2,7)]) 
        } else if (identical(num,"worst")){
            result <- rbind(result,stateframe[nrow(stateframe),c(2,7)]) 
        } else if (is.numeric(num)){
            result <- rbind(result,stateframe[num,c(2,7)]) 
        } else {
            stop("invalid number")
        }
    }
    
    result 
}
