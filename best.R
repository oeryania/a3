library(dplyr)

outcome.data <- mutate(read.csv("outcome-of-care-measures.csv", colClasses = "character"),
                      State,
                      Hospital.Name,
                      "heart attack" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),
                      "heart failure" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),
                      "pneumonia" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),
                      .keep = "none")

plot.outcome.hist <- function(outcome) hist(outcomeData[, outcome])

hospital.ranks <- function(state, outcome) {
    stopifnot("invalid state" = state %in% outcome.data$State)
    stopifnot("invalid outcome" = outcome %in% names(outcome.data))
    outcomeData %>%
        subset(State == state) %>%
        select(Hospital.Name, as.character(outcome)) %>%
        na.omit %>%
        arrange(.data[[outcome]], Hospital.Name) %>%
        cbind(Rank = seq(nrow(.)))
}

best <- function(state, outcome) {
    hospital.ranks(state, outcome)[1, 1]
}

rankhospital <- function(state, outcome, num = "best") {
    hospitalRanks <- hospital.ranks(state, outcome)
    if (is.character(num)) num <- switch(num, "best" = 1, "worst" = nrow(hospitalRanks))
    subset(hospitalRanks, Rank == num)
}