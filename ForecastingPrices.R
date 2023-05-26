install.packages("vars")
install.packages("tidyverse")
install.packages("knitr")
install.packages("urca")
install.packages("tseries")
install.packages("xts")


library(vars)
library(tidyverse)
library(knitr)
library(urca)
library(tseries)
library(xts)

data <- read.csv("TSA_2023.csv")


# Perform ADF test for each variable
adf_results <- lapply(data[-1], adf.test)

# Extract the test statistics and p-values
adf_stats <- sapply(adf_results, function(x) x$statistic)
adf_pvalues <- sapply(adf_results, function(x) x$p.value)

# Print the ADF test results for each variable
for (i in seq_along(adf_results)) {
  cat("ADF Test Results for", colnames(data)[i+1], ":\n")
  cat("Test statistic:", adf_stats[i], "\n")
  cat("p-value:", adf_pvalues[i], "\n\n")
}

#Alright, looks like only x4 is likely to be stationary.
#We can check for ACF and PACF, or seasonality. And so on.