# Load packages ----------------------------------------------------------------
library(tidyverse)

# Import data ------------------------------------------------------------------
setwd("~/Documents/OpenCourse/Coursera/Data_Science/2_R_Programming/week4")
data_folder <- "ProgAssignment3-data"
hospital_data<- read_csv(file.path(data_folder, "hospital-data.csv"), 
                         na = c("Not Available"))
outcome_data <- read_csv(file.path(data_folder, "outcome-of-care-measures.csv"),
                         na = c("Not Available"))

# Define functions -------------------------------------------------------------
# best.R
best <- function(state, outcome) {
  state <- toupper(state)
  outcome <- tolower(outcome)
  states <- unique(outcome_data$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!state %in% states) {
    stop("invalid state")
  }
  
  if (!outcome %in% outcomes) {
    stop("invalid outcome")
  }
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="", collapse=" ")
  }
  
  outcome_col <- paste("Hospital 30-Day Death (Mortality) Rates from",
                        simpleCap(outcome))

  outcome_data %>%
    filter(State == state) %>%
    filter(.[outcome_col] == min(.[outcome_col], na.rm = TRUE)) %>%
    arrange(`Hospital Name`) %>%
    select(`Hospital Name`) %>% 
    .[1]
}

# rankhospital.R
rankhospital <- function(state, outcome, num = "best") {
  state <- toupper(state)
  outcome <- tolower(outcome)
  states <- unique(outcome_data$State)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!state %in% states) {
    stop("invalid state")
  }
  
  if (!outcome %in% outcomes) {
    stop("invalid outcome")
  }
  
  if (num != "best" && num != "worst" && num %% 1 != 0) {
    stop("invalid num")
  }
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="", collapse=" ")
  }
  
  outcome_col <- paste("Hospital 30-Day Death (Mortality) Rates from",
                       simpleCap(outcome))
  outcome_col <- as.name(outcome_col)
  
  ranked <- outcome_data %>%
    filter(State == state) %>%
    select(`Hospital Name`, !!outcome_col) %>%
    na.omit() %>%
    arrange(!!outcome_col, `Hospital Name`)
  
  if (num == "best") {
    return(as.character(ranked[1, 1]))
  } else if (num == "worst") {
    return(as.character(ranked[nrow(ranked), 1]))
  } else {
    return(as.character(ranked[num, 1]))
  }
}

# rankall.R
rankall <- function(outcome, num) {
  outcome <- tolower(outcome)
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!outcome %in% outcomes) {
    stop("invalid outcome")
  }
  
  if (num != "best" && num != "worst" && num %% 1 != 0) {
    stop("invalid num")
  }
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2), sep="", collapse=" ")
  }
  
  outcome_col <- paste("Hospital 30-Day Death (Mortality) Rates from",
                       simpleCap(outcome))
  outcome_col <- as.name(outcome_col)
  
  outcome_data %>%
    select(hospital = `Hospital Name`, outcome = !!outcome_col, state = `State`) %>%
    na.omit() %>%
    group_by(state) %>%
    arrange(outcome, hospital) %>%
    filter(row_number() == ifelse(num == "best", 1, ifelse(num == "worst", n(), num))) %>%
    arrange(state)
}