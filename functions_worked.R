best <- function(state, outcome) {
    ## read outcome data
    ## check that the state and outcome are valid
    if (!is.element(state, state.abb)) {
        stop("invalid state")
    } 
    if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    ## return hospital name in that state w/ lowest 30-day death rate
    switch(outcome,
           "heart attack" = {colnum = 11},
           "heart failure" = {colnum = 17},
           "pneumonia" = {colnum = 23}, 
           stop("invalid outcome"))
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- data[, c(2, 7, colnum)]
    names(data) <- c("hospital", "state", "outcome")
    data <-  data[data$state == state, c("hospital", "outcome")]
    data[, "outcome"] = as.numeric(data[, "outcome"])
    data <- data[complete.cases(data), ]
    data <- data[order(data[, "outcome"], data[, "hospital"]), ]
    data[1, ]
    # data <- lapply(data, function(dat) dat[order(dat[, "outcome"], dat[, "hospital"]), ])
}


rankhospital <- function(state, outcome, num = "best") {
    if (!is.element(state, state.abb)) {
        stop("invalid state")
    } 
    if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    switch(outcome,
           "heart attack" = {colnum = 11},
           "heart failure" = {colnum = 17},
           "pneumonia" = {colnum = 23}, 
           stop("invalid outcome"))
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- data[, c(2, 7, colnum)]
    names(data) <- c("hospital", "state", "outcome")
    # sort by states
    # note that, below is an inefficient way of sorting
    # for efficient sorting based on outcome then on hospital name,
    # see the function below
    data <-  data[data$state == state, c("hospital", "outcome")]
    data[, "outcome"] <- as.numeric(data[, "outcome"])
    data <- data[complete.cases(data), ]
    data <- data[order(data[, "outcome"]), ]
    data <- split(data, data$outcome)
    data <- lapply(data, function(arg) arg[order(arg[, "hospital"]), ])
    data <- do.call("rbind", data)
    data <- data[, c(1, 2)]
    rownames(data) <- NULL
    switch(num,
           "best" = {num = 1},
           "worst" = {num = nrow(data)})
    if (!is.numeric(num)){
        stop("invalid num")
    }
    data[num, ]
}

rankall <- function(outcome, num = "best") {
    if (!is.element(outcome, c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }
    switch(outcome,
           "heart attack" = {colnum = 11},
           "heart failure" = {colnum = 17},
           "pneumonia" = {colnum = 23}, 
           stop("invalid outcome"))
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data <- data[, c(2, 7, colnum)]
    names(data) <- c("hospital", "state", "outcome")
    # sort by states
    data[, "outcome"] <- as.numeric(data[, "outcome"])
    data <- data[complete.cases(data), ]
    data <- split(data, data$state)
    # efficient ranking using order
    data <- lapply(data, function(dat) dat[order(dat[, "outcome"], dat[, "hospital"]), ])

    # read num
    if (num == "best") {
        num = 1
        output <- c()
        for (i in 1:length(data)){
            output <- rbind(output, as.data.frame(data[[i]][1, ]))
        }
    } 
    else if (num == "worst") { 
        num <- sapply(data, function(dat) nrow(dat)) 
        output <- c()
        for (i in 1:length(data)){
            # print("print below: ")
            # print(data[[i]][num[i], ])
            output <- rbind(output, as.data.frame(data[[i]][num[i], ]))
        }
    }
    else if (!is.numeric(num)) {
        stop("invalid num")
    }
    else {
        output <- c()
        for (i in 1:length(data)){
            if(is.na(data[[i]][num, 2])){
                data[[i]][num, 2] = data[[i]]$state[1]
            }
            output <- rbind(output, as.data.frame(data[[i]][num, ]))
        }
    }
    output
    # data <- sapply(data, function(dat, pos = 20) dat[pos, ])
}


