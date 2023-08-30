library(dplyr)

outcome.data <- mutate(read.csv("outcome-of-care-measures.csv", colClasses = "character"),
                      State,
                      Hospital.Name,
                      "heart attack" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),
                      "heart failure" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),
                      "pneumonia" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),
                      .keep = "none")

plot.outcome.hist <- function(outcome) hist(outcomeData[, outcome])

states <- outcome.data$State %>% unique %>% sort

validate.state <- function(state) stopifnot("invalid state" = state %in% states)
validate.outcome <- function(outcome) stopifnot("invalid outcome" = outcome %in% names(outcome.data))

hospital.ranks <- function(state, outcome) {
    validate.state(state)
    validate.outcome(outcome)
    
    outcome.data %>%
        subset(State == state) %>%
        select(Hospital.Name, as.character(outcome)) %>%
        na.omit %>%
        arrange(.data[[outcome]], Hospital.Name) %>%
        mutate(Rank = row_number())
}

best <- function(state, outcome) {
    hospital.ranks(state, outcome)[1, 1]
}

rankhospital <- function(state, outcome, num = "best") {
    hospitalRanks <- hospital.ranks(state, outcome)
    
    nrows <- nrow(hospitalRanks)

    if (is.character(num)) num <- switch(num, "best" = 1, "worst" = nrows)
    if(is.numeric(num) && num > nrows) return(NA)
    
    subset(hospitalRanks, Rank == num)
}

find.hospital.by.rank <- function(myState, myRank, myData) {
    stateHospitals <- filter(myData, State == myState)
    if (is.character(myRank)) myRank <- switch(myRank, "best" = 1, "worst" = nrow(stateHospitals))
    hospitalName <- filter(stateHospitals, Rank == myRank)$Hospital.Name
    if(length(hospitalName) == 0) NA else hospitalName
}

rankall <- function(outcome, num = "best") {
    validate.outcome(outcome)

    outcomeData2 <- outcome.data %>%
        select(State, Hospital.Name, as.character(outcome)) %>%
        na.omit %>%
        arrange(State, .data[[outcome]], Hospital.Name) %>%
        group_by(State) %>%
        mutate(Rank = row_number()) %>%
        ungroup()
    
    cbind(
        hospital = sapply(states, find.hospital.by.rank, num, outcomeData2),
        state = states
        ) %>% as.data.frame
        
}
