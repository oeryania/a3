library(dplyr)

# intilize data by reading outcome-of-care-measures.csv (downloaded from https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip)
outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") %>%
                    mutate(State, Hospital.Name,
                      # hospital outcomes that we care about
                      "heart attack" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),
                      "heart failure" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),
                      "pneumonia" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),
                      .keep = "none") # drop all other columns

# input: outcome | output: histogram for specified outcome
plot.outcome.hist <- function(outcome) hist(outcome.data[, outcome])

# input: none | output: list of all states, sorted alphabetically
states <- outcome.data$State %>% unique %>% sort

# input: state | output: error if state is not in data
validate.state <- function(state) stopifnot("invalid state" = state %in% states)

# input: outcome | output: error if outcome is not in data
validate.outcome <- function(outcome) stopifnot("invalid outcome" = outcome %in% names(outcome.data))

# input: state, outcome | output: list of all hospitals and their rankings for input
hospital.ranks <- function(state, outcome) {
    validate.state(state)
    validate.outcome(outcome)
    
    outcome.data %>%
        filter(State == state) %>%
        select(Hospital.Name, as.character(outcome)) %>%
        na.omit %>%
        arrange(.data[[outcome]], Hospital.Name) %>%
        mutate(Rank = row_number())
}

# input: state, rank, data with state, hospital name, and rank | output: hospital name with the specified ranking for a given state
find.hospital.by.rank <- function(myState, myRank, myData) {
    stateHospitals <- filter(myData, State == myState)
    if (is.character(myRank)) myRank <- switch(myRank, "best" = 1, "worst" = nrow(stateHospitals))
    hospitalName <- filter(stateHospitals, Rank == myRank)$Hospital.Name
    if(length(hospitalName) == 0) NA else hospitalName
}

# input: state, outcome | output: best hospital for input
best <- function(state, outcome) {
    hospital.ranks(state, outcome)[1, 1]
}

# input: state, outcome, rank | output: hospital name for specified input
rankhospital <- function(state, outcome, num = "best") {
    hospitalRanks <- hospital.ranks(state, outcome)
    
    nrows <- nrow(hospitalRanks)

    if (is.character(num)) num <- switch(num, "best" = 1, "worst" = nrows)
    else if(is.numeric(num) && num > nrows) return(NA)
    
    filter(hospitalRanks, Rank == num)$Hospital.Name
}

# input: hospital outcome, rank | output: list of all states, along with the hospital name with in that state with given rank
rankall <- function(outcome, num = "best") {
    validate.outcome(outcome)

    outcomeDataRanked <- outcome.data %>%
        select(State, Hospital.Name, as.character(outcome)) %>%
        na.omit %>%
        arrange(State, .data[[outcome]], Hospital.Name) %>%
        group_by(State) %>%
        mutate(Rank = row_number()) %>%
        ungroup()
    
    cbind(
        hospital = sapply(states, find.hospital.by.rank, num, outcomeDataRanked),
        state = states
        ) %>% as.data.frame
}
