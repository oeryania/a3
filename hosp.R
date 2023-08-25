library(dplyr)

outcomeFilename <- "outcome-of-care-measures.csv"
varPrefix <- "Hospital.30.Day.Death..Mortality..Rates.from."

best <- function(state, outcome) {
    outcomes <- read.csv(outcomeFilename, colClasses = "character")
    varName <- paste(varPrefix, varSuffix(outcome), sep = "")
    outcomesForState <- filter(outcomes, State == state)
    if (nrow(outcomesForState) == 0) {
        cat(sep = "", 'Error in best("', state, '", "', outcome, '") : invalid state')
        opt <- options(show.error.messages = FALSE)
        on.exit(options(opt))
        stop()
    }
    outcomeForState <- tryCatch( outcomesForState[, varName],
        error = function(e) {
            cat(sep = "", 'Error in best("', state, '", "', outcome, '") : invalid outcome')
            opt <- options(show.error.messages = FALSE)
            on.exit(options(opt))
            stop()
        }
    )
    suppressWarnings(outcomeData <- as.numeric(outcomeForState))
    max(outcomeData, na.rm = TRUE)
}

varSuffix <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = ".")
}
