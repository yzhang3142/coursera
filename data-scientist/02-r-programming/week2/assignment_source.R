library(tidyverse)

pollutantmean <- function(directory, pollutant, id = 1:332) {
  pollutant <- as.name(pollutant) # coerce the argument to a name
  pollutant_sum <- 0
  pollutant_n <- 0
  
  lapply(
    lapply(id, function(i) {
      data <- read_csv(file.path(directory,
                                 paste0(formatC(i, width=3, flag="0"), ".csv")),
                       col_types = cols(
                         Date = col_date(),
                         sulfate = col_double(),
                         nitrate = col_double(),
                         ID = col_integer()
                       ))
      data %>%
        select(!!pollutant) %>%
        filter(!is.na(!!pollutant)) %>%
        summarise(sum = sum(!!pollutant), n = n())
    }), 
    function(data) {
      pollutant_sum <<- pollutant_sum + data$sum
      pollutant_n <<- pollutant_n + data$n
    }
  )
  
  pollutant_sum / pollutant_n
}

complete <- function(directory, id = 1:332) {
  counts <- data.frame(id = id, nobs = 0)
  
  counts$nobs <- sapply(id, function(i) {
    data <- read_csv(file.path(directory,
                               paste0(formatC(i, width=3, flag="0"), ".csv")),
                     col_types = cols(
                       Date = col_date(),
                       sulfate = col_double(),
                       nitrate = col_double(),
                       ID = col_integer()
                     ))
    nrow(na.omit(data))
  })
  
  counts
}

corr <- function(directory, threshold = 0) {
  files <- list.files(directory)
  
  crs <- sapply(files, function(file) {
    data <- read_csv(file.path(directory, file),
                     col_types = cols(
                       Date = col_date(),
                       sulfate = col_double(),
                       nitrate = col_double(),
                       ID = col_integer()
                     )) %>% 
      na.omit()
      
    if (nrow(data) < threshold) {
      NA
    } else {
      cor(data$sulfate, data$nitrate)
    }
  })
  
  crs <- crs[!is.na(crs)]
  
  if (length(crs) == 0) {
    numeric(0)
  } else {
    crs
  }
}