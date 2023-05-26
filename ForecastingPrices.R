install.packages("vars")
install.packages("tidyverse")
install.packages("knitr")
install.packages("urca")
install.packages("tseries")
install.packages("xts")
install.packages("forecast")


library(forecast)
library(vars)
library(tidyverse)
library(knitr)
library(urca)
library(tseries)
library(xts)

data <- read.csv("TSA_2023.csv")
data$Date <- as.Date(data[, 1])


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

# Perform the ADF test for x4
x4 <- data$x4
adf_result_x4 <- adf.test(x4)

# Check the ADF test results for x4
cat("ADF Test Results for x4:\n")
cat("Test statistic:", adf_result_x4$statistic, "\n")
cat("p-value:", adf_result_x4$p.value, "\n\n")


#The test statistic indicates the strength of evidence
#against the null hypothesis of non-stationarity. 
#In this case, the test statistic of -3.453917 suggests
#strong evidence in favor of stationarity.
#So, We will give it a change, checking for seasonalities.

# Create a time series object with the date index
x4_ts <- ts(data[, 2], start = min(data$Date), end = max(data$Date), frequency = 12)

# Perform seasonal decomposition of x4
decomposition <- decompose(x4_ts)

# Plot the seasonal decomposition
plot(decomposition)

# Check for seasonality using seasonal plots
seasonplot(x4_ts)
ggseasonplot(x4_ts)

# Examine the autocorrelation function (ACF) and partial autocorrelation function (PACF)
acf(x4_ts)
pacf(x4_ts)

