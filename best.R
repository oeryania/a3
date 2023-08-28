library(dplyr)

outcome.data <- mutate(read.csv("outcome-of-care-measures.csv", colClasses = "character"),
                      State,
                      Hospital.Name,
                      "heart attack" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),
                      "heart failure" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),
                      "pneumonia" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),
                      .keep = "none")

plot.outcome.hist <- function(outcome) hist(outcomeData[, outcome])

best <- function(state, outcome) {
    stopifnot("invalid state" = state %in% outcome.data$State)
    stopifnot("invalid outcome" = outcome %in% names(outcome.data))
    outcome.data.filtered <- outcomeData %>%
        subset(State == state) %>%
        select(Hospital.Name, as.character(outcome)) %>%
        na.omit
    outcome.data.filtered.ranked <- cbind(outcome.data.filtered, Rank = dense_rank(outcome.data.filtered[,2]))
    outcome.data.filtered.ranked.ordered <- arrange(outcome.data.filtered.ranked, Rank, Hospital.Name)
    outcome.data.filtered.ranked.ordered[1, 1]
}