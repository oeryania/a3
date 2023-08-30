# Assignment Description:
#
# Download the file ProgAssignment3-data.zip from:
# https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip
# The data for this assignment come from the Hospital Compare web site
# (http://hospitalcompare.hhs.gov) run by the U.S. Department of Health and
# Human Services. The purpose of the web site is to provide data and information
# about the quality of care at over 4,000 Medicare-certified hospitals in the
# U.S. This dataset essentially covers all major U.S. hospitals. This dataset is
# used for a variety of purposes, including determining whether hospitals should
# be fined for not providing high quality care to patients (see
# http://goo.gl/jAXFX for some background on this particular topic). The
# Hospital Compare web site contains a lot of data and we will only look at a
# small subset for this assignment. The zip file for this assignment contains
# three files • outcome-of-care-measures.csv: Contains information about 30-day
# mortality and readmission rates for heart attacks, heart failure, and
# pneumonia for over 4,000 hospitals. • hospital-data.csv: Contains information
# about each hospital. • Hospital_Revised_Flatfiles.pdf: Descriptions of the
# variables in each file (i.e the code book). A description of the variables in
# each of the files is in the included PDF file named
# Hospital_Revised_Flatfiles.pdf. This document contains information about many
# other files that are not included with this programming assignment. You will
# want to focus on the variables for Number 19 (“Outcome of Care Measures.csv”)
# and Number 11 (“Hospital Data.csv”).

library(dplyr)

############################## INITIALIZE DATA #################################

# intilize outcome data from csv, selecting only the relevant columns
outcome.data <- read.csv("outcome-of-care-measures.csv", colClasses = "character") %>%
                    mutate(State, Hospital.Name,
                      "heart attack" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)),
                      "heart failure" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)),
                      "pneumonia" = suppressWarnings(as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)),
                      .keep = "none") # drop all other columns

# input: none | output: list of all states, sorted alphabetically
states <- outcome.data$State %>% unique %>% sort

############################## HELPER FUNCTIONS ################################

# input: state | output: error if state is not in data
validate.state <- function(state) stopifnot("invalid state" = state %in% states)

# input: outcome | output: error if outcome is not in data
validate.outcome <- function(outcome) stopifnot("invalid outcome" = outcome %in% names(outcome.data))

# input: state, outcome | output: hospitals for input with their rank
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

# input: state, rank, data with state, hospital, rank | output: hospital name for input
find.hospital.by.rank <- function(myState, myRank, myData) {
    stateHospitals <- filter(myData, State == myState)
    if (is.character(myRank)) myRank <- switch(myRank, "best" = 1, "worst" = nrow(stateHospitals))
    hospitalName <- filter(stateHospitals, Rank == myRank)$Hospital.Name
    if(length(hospitalName) == 0) NA else hospitalName
}

########################### ASSIGNMENT FUNCTIONS ###############################

# ANSWER 1 | input: outcome | output: histogram for input
plot.outcome.hist <- function(outcome) hist(outcome.data[, outcome])

# ANSWER 2 | input: state, outcome | output: best hospital for input
best <- function(state, outcome) {
    hospital.ranks(state, outcome)[1, 1]
}

# ANSWER 3 | input: state, outcome, rank | output: hospital name for input
rankhospital <- function(state, outcome, num = "best") {
    hospitalRanks <- hospital.ranks(state, outcome)
    
    nrows <- nrow(hospitalRanks)

    if (is.character(num)) num <- switch(num, "best" = 1, "worst" = nrows)
    else if(is.numeric(num) && num > nrows) return(NA)
    
    filter(hospitalRanks, Rank == num)$Hospital.Name
}

# ANSWER 4 | input: outcome, rank | output: state and hospital name for input
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
