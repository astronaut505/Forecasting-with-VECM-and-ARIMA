library(xts)
install.packages("tseries")
library(vars)
library(tidyverse)
library(knitr)
library(urca)
library(tseries)
l
file_path <- file.choose()
data <- read.csv(file_path)

# Loop through all time series and perform the ADF test
for(i in 1:10) {
  adf.test(data[,i])
}

# Initialize variables to hold the cointegrated pair
pair_found <- FALSE
pair <- c()

# Loop through all pairs of time series and perform the cointegration test
for(i in 1:9) {
  for(j in (i+1):10) {
    test <- cointegrationTest(data[,i], data[,j], output=FALSE)
    if(test@test$p.value < 0.05) {
      pair_found <- TRUE
      pair <- c(i, j)
      break
    }
  }
  if(pair_found) {
    break
  }
}

if(pair_found) {
  cat("Cointegrated pair found:", colnames(data)[pair], "\n")
} else {
  cat("No cointegrated pair found.\n")
}
