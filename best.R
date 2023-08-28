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
    outcomeDataFilteredOrdered <- outcomeData %>%
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
    subset(hospital.ranks(state, outcome), Rank == num)
}