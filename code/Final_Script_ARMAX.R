##################################################################
############### ARMA-X ANALYSIS  ##################################

# Isaac Graber
# Adrien Currat
# Mathéo Bourgeois



##############  NIKKEI DATA #####################################



##### READING AND PROCESSING DATA

# Load the CSV file into R
nikkei_data <- read.csv("~/Downloads/NikkeiDailyData1.csv", sep = ";", header = TRUE)

# Print the structure and column names of the dataset
str(nikkei_data)
names(nikkei_data)

# Clean the 'Close' column by removing non-numeric characters and converting it to numeric
nikkei_data$Close <- as.numeric(gsub("[^0-9.]", "", nikkei_data$Close))

# Remove rows where 'Close' is NA after conversion
nikkei_data <- nikkei_data[!is.na(nikkei_data$Close), ]



##### MAKE THE DATA STATIONARY 

# Apply log transformation and then compute the first difference
nikkei_data_log_diff <- diff(log(nikkei_data$Close), differences = 1)

# Create a new DataFrame aligned to the differenced data (removing the first date due to differencing)
adjusted_dates <- nikkei_data$Date[-1]
adjusted_dummy <- nikkei_data$Dummy[-1]
adjusted_damage <- nikkei_data$Damage[-1]
adjusted_dummy_over_median <- nikkei_data$Dummy_over_0.5[-1]
adjusted_damage_over_median <- nikkei_data$Damage_over_0.5[-1]
adjusted_dummy_over_0.75_quantile <- nikkei_data$Dummy_over_0.75[-1]
adjusted_damage_over_0.75_quantile <- nikkei_data$Damage_over_0.75[-1]
adjusted_dummy_over_0.90_quantile <- nikkei_data$Dummy_over_0.90[-1]
adjusted_damage_over_0.90_quantile <- nikkei_data$Damage_over_0.90[-1]

data_frame_aligned <- data.frame(
  Date = adjusted_dates,   # Adjusted date column
  Log_Diff = nikkei_data_log_diff,  # Log differenced 'Close' prices
  Dummy = adjusted_dummy, # Adjusted dummy variable column
  Damage = adjusted_damage, # Adjusted damage variable column
  Dummy_over_median = adjusted_dummy_over_median, # Adjusted dummy over median variable column
  Damage_over_median = adjusted_damage_over_median, # Adjusted damage over median variable column
  Dummy_over_0.75_quantile = adjusted_dummy_over_0.75_quantile, # Adjusted dummy over 0.75 quantile variable column
  Damage_over_0.75_quantile = adjusted_damage_over_0.75_quantile, # Adjusted damage over 0.75 quantile variable column
  Dummy_over_0.90_quantile = adjusted_dummy_over_0.90_quantile, # Adjusted dummy over 0.90 quantile variable column
  Damage_over_0.90_quantile = adjusted_damage_over_0.90_quantile # Adjusted damage over 0.90 quantile variable column
)

# Convert the 'Date' column to Date type
data_frame_aligned$Date <- as.Date(data_frame_aligned$Date, format = "%d.%m.%Y")

# Identify rows where 'Close' column has missing values
missing_rows_in_logdiff <- which(is.na(data_frame_aligned$Log_Diff))
print(paste("Missing values in 'LogDiff' are in rows: ", toString(missing_rows_in_logdiff)))

# Exclude rows with NA in Log_Diff using base R
data_frame_cleaned <- data_frame_aligned[!is.na(data_frame_aligned$Log_Diff), ]

# Print the structure to confirm rows are dropped
str(data_frame_cleaned)

# Plotting the log differenced data as a line graph
plot(nikkei_data_log_diff,
     type = "l",
     main = "Log Differenced Nikkei Data",
     xlab = "Number of days",
     ylab = "Log Differenced Values")



##### TESTING FOR STATIONARITY

# Install and load packages for time series analysis
library(tseries)
library(forecast)
library(carData)

# Perform the Augmented Dickey-Fuller test on the log-differenced data
adf_result <- adf.test(nikkei_data_log_diff, alternative = "stationary")
print(adf_result)
# The p-value (0.01) is less than the threshold (0.05), we can reject the null hypothesis and conclude that the data is stationary.



##### IDENTIFY AR AND MA ORDERS USING PACF AND ACF

# Visualize Autocorrelation and Partial Autocorrelation
acf(nikkei_data_log_diff)
pacf(nikkei_data_log_diff)

# Automatically fit an ARIMA model based on the AIC criterion
fit <- auto.arima(nikkei_data_log_diff, ic = "aic")
summary(fit)
# We should implement a model with AR component of order two and MA component of order two: ARMA(2,2)



##### FIT AN ARIMA(2,0,2) MODEL USING THE DUMMY VARIABLE AS X

arimax_model <- Arima(data_frame_aligned$Log_Diff,
                      order=c(2,0,2), include.mean=FALSE,
                      xreg=data_frame_aligned$Dummy)
summary(arimax_model)
# Effect of x is counter intuitive and very low

# Extract coefficients and standard errors from the model summary
coefficients <- c(ar1 = -0.4862, ar2 = -0.9103, ma1 = 0.4541, ma2 = 0.9381, xreg = 0.0009)
std_errors <- c(ar1 = 0.039, ar2 = 0.0376, ma1 = 0.0308, ma2 = 0.0336, xreg = 0.0018)
# Calculate t-statistics and corresponding p-values
t_statistics <- coefficients / std_errors
p_values <- 2 * pt(abs(t_statistics), df = length(nikkei_data_log_diff) - 5, lower.tail = FALSE)
# Print p-values to evaluate the statistical significance of the model's coefficients
print("P-values for the coefficients:")
print(p_values)

# P-value < 0.05 for all MA and AR coefficients, but the p-value for the X coefficient is > 0.05
# These negative coefficients on the AR part show the cyclical component of the serie, suggesting a larger effect of lag 2 in the value of today than lag 1 
# The coefficient on the dummy isn't statistically significant
# MA coefficient positive suggest that a past underestimation (positive errors) conduct to an increase in percentage of value
# and overestimation suggest that a past overestimation (negative errors) conduct to a decrease.



##### FIT AN ARIMA(2,0,2) MODEL USING THE DAMAGE VARIABLE AS X (54 observations)

arimax_model_damage <- Arima(data_frame_cleaned$Log_Diff,
                             order=c(2,0,2), include.mean=FALSE,
                             xreg=data_frame_cleaned$Damage)
summary(arimax_model_damage)
# NaNs produced, let's log the damage variable

# Transform the damage variable
data_frame_cleaned$Log_Damage <- log(data_frame_cleaned$Damage + 1)

##### FIT AN ARIMA(2,0,2) MODEL USING THE TRANSFORMED DAMAGE VARIABLE AS X (54 observations)

arimax_model_log_damage <- Arima(data_frame_cleaned$Log_Diff,
                                 order=c(2,0,2), include.mean=FALSE,
                                 xreg=data_frame_cleaned$Log_Damage)
summary(arimax_model_log_damage)

# Extract coefficients and standard errors from the model summary
coefficients <- c(ar1 = -0.4621, ar2 = -0.9096, ma1 = 0.4542, ma2 = 0.9374, xreg = 1e-04)
std_errors <- c(ar1 = 0.0392, ar2 = 0.0380, ma1 = 0.0311, ma2 = 0.0341, xreg = 1e-04)
# Calculate t-statistics and corresponding p-values
t_statistics <- coefficients / std_errors
p_values <- 2 * pt(abs(t_statistics), df = length(nikkei_data_log_diff) - 5, lower.tail = FALSE)
# Print p-values to evaluate the statistical significance of the model's coefficients
print("P-values for the coefficients:")
print(p_values)
# P-value < 0.05 for all MA and AR coefficients, but the p-value for the X coefficient is > 0.05



##### FIT AN ARIMA(2,0,2) MODEL USING THE DUMMY VARIABLES OVER THE MEDIAN AS X (27 observations)

arimax_model_dummy_over_median <- Arima(data_frame_cleaned$Log_Diff,
                                        order=c(2,0,2), include.mean=FALSE,
                                        xreg=data_frame_cleaned$Dummy_over_median)
summary(arimax_model_dummy_over_median)

# Extract coefficients and standard errors from the model summary
coefficients <- c(ar1 = -0.4621, ar2 = -0.9096, ma1 = 0.4541, ma2 = 0.9374, xreg = 0.0021)
std_errors <- c(ar1 = 0.0391, ar2 = 0.0379, ma1 = 0.0310, ma2 = 0.0340, xreg = 0.0026)
# Calculate t-statistics and corresponding p-values
t_statistics <- coefficients / std_errors
p_values <- 2 * pt(abs(t_statistics), df = length(nikkei_data_log_diff) - 5, lower.tail = FALSE)
# Print p-values to evaluate the statistical significance of the model's coefficients
print("P-values for the coefficients:")
print(p_values)
# P-value < 0.05 for all MA and AR coefficients, but the p-value for the X coefficient is > 0.05



##### FIT AN ARIMA(2,0,2) MODEL USING THE DAMAGE VARIABLES OVER THE MEDIAN AS X (27 observations)

arimax_model_damage_over_median <- Arima(data_frame_cleaned$Log_Diff,
                                         order=c(2,0,2), include.mean=FALSE,
                                         xreg=data_frame_cleaned$Damage_over_median)
summary(arimax_model_damage_over_median)
# NaNs produced, let's log the damage variable

# Transform the damage variable
data_frame_cleaned$Log_Damage_over_median <- log(data_frame_cleaned$Damage_over_median + 1)

##### FIT AN ARIMA(2,0,2) MODEL USING THE TRANSFORMED DAMAGE VARIABLES OVER THE MEDIAN AS X (27 observations)

arimax_model_logdamage_over_median <- Arima(data_frame_cleaned$Log_Diff,
                                            order=c(2,0,2), include.mean=FALSE,
                                            xreg=data_frame_cleaned$Log_Damage_over_median)
summary(arimax_model_logdamage_over_median)

# Extract coefficients and standard errors from the model summary
coefficients <- c(ar1 = -0.4621, ar2 = -0.9094, ma1 = 0.4540, ma2 = 0.9372, xreg = 1e-04)
std_errors <- c(ar1 = 0.0390, ar2 = 0.0380, ma1 = 0.0309, ma2 = 0.0340, xreg = 2e-04)
# Calculate t-statistics and corresponding p-values
t_statistics <- coefficients / std_errors
p_values <- 2 * pt(abs(t_statistics), df = length(nikkei_data_log_diff) - 5, lower.tail = FALSE)
# Print p-values to evaluate the statistical significance of the model's coefficients
print("P-values for the coefficients:")
print(p_values)
# P-value < 0.05 for all MA and AR coefficients, but the p-value for the X coefficient is > 0.05



##### FIT AN ARIMA(2,0,2) MODEL USING THE DUMMY VARIABLES OVER THE 0.75 QUANTILE AS X (13 observations)

arimax_model_dummy_over_0.75_quantile  <- Arima(data_frame_cleaned$Log_Diff,
                                                order=c(2,0,2), include.mean=FALSE,
                                                xreg=data_frame_cleaned$Dummy_over_0.75_quantile)
summary(arimax_model_dummy_over_0.75_quantile)

# Extract coefficients and standard errors from the model summary
coefficients <- c(ar1 = -0.4627, ar2 = -0.9078, ma1 = 0.4544, ma2 = 0.9358, xreg = 0.0025)
std_errors <- c(ar1 = 0.0386, ar2 = 0.0377, ma1 = 0.0307, ma2 = 0.0339, xreg = 0.0037)
# Calculate t-statistics and corresponding p-values
t_statistics <- coefficients / std_errors
p_values <- 2 * pt(abs(t_statistics), df = length(nikkei_data_log_diff) - 5, lower.tail = FALSE)
# Print p-values to evaluate the statistical significance of the model's coefficients
print("P-values for the coefficients:")
print(p_values)
# P-value < 0.05 for all MA and AR coefficients, but the p-value for the X coefficient is > 0.05



##### FIT AN ARIMA(2,0,2) MODEL USING THE TRANSFORMED DAMAGE VARIABLES OVER THE 0.75 QUANTILE AS X (13 observations)

# Transform the damage
data_frame_cleaned$Log_Damage_over_0.75_quantile <- log(data_frame_cleaned$Damage_over_0.75_quantile + 1)

arimax_model_logdamage_over_0.75_quantile <- Arima(data_frame_cleaned$Log_Diff,
                                                   order=c(2,0,2), include.mean=FALSE,
                                                   xreg=data_frame_cleaned$Log_Damage_over_0.75_quantile)
summary(arimax_model_logdamage_over_0.75_quantile)

# Extract coefficients and standard errors from the model summary
coefficients <- c(ar1 = -0.4635, ar2 = -0.9056, ma1 = 0.4548, ma2 = 0.9338, xreg = 1e-04)
std_errors <- c(ar1 = 0.0386, ar2 = 0.0384, ma1 = 0.0308, ma2 = 0.0345, xreg = 2e-04)
# Calculate t-statistics and corresponding p-values
t_statistics <- coefficients / std_errors
p_values <- 2 * pt(abs(t_statistics), df = length(nikkei_data_log_diff) - 5, lower.tail = FALSE)
# Print p-values to evaluate the statistical significance of the model's coefficients
print("P-values for the coefficients:")
print(p_values)
# P-value < 0.05 for all MA and AR coefficients, but the p-value for the X coefficient is > 0.05



##### FIT AN ARIMA(2,0,2) MODEL USING THE DUMMY VARIABLES OVER THE 0.90 QUANTILE AS X (6 observations)

arimax_model_dummy_over_0.90_quantile  <- Arima(data_frame_cleaned$Log_Diff,
                                                order=c(2,0,2), include.mean=FALSE,
                                                xreg=data_frame_cleaned$Dummy_over_0.90_quantile)
summary(arimax_model_dummy_over_0.90_quantile)

# Extract coefficients and standard errors from the model summary
coefficients <- c(ar1 = -0.4627, ar2 = -0.9069, ma1 = 0.4543, ma2 = 0.9353, xreg = -0.0003)
std_errors <- c(ar1 = 0.0378, ar2 = 0.0374, ma1 = 0.0300, ma2 = 0.0335, xreg = 0.0054)
# Calculate t-statistics and corresponding p-values
t_statistics <- coefficients / std_errors
p_values <- 2 * pt(abs(t_statistics), df = length(nikkei_data_log_diff) - 5, lower.tail = FALSE)
# Print p-values to evaluate the statistical significance of the model's coefficients
print("P-values for the coefficients:")
print(p_values)
# P-value < 0.05 for all MA and AR coefficients, but the p-value for the X coefficient is > 0.05


##### FIT AN ARIMA(2,0,2) MODEL USING THE TRANSFORMED DAMAGE VARIABLES OVER THE 0.90 QUANTILE AS X (6 observations)

# Transform the damage
data_frame_cleaned$Log_Damage_over_0.90_quantile <- log(data_frame_cleaned$Damage_over_0.90_quantile + 1)

arimax_model_logdamage_over_0.90_quantile <- Arima(data_frame_cleaned$Log_Diff,
                                                   order=c(2,0,2), include.mean=FALSE,
                                                   xreg=data_frame_cleaned$Log_Damage_over_0.90_quantile)
summary(arimax_model_logdamage_over_0.90_quantile)

# Extract coefficients and standard errors from the model summary
coefficients <- c(ar1 = -0.4622, ar2 = -0.9102, ma1 = 0.4541, ma2 = 0.9381, xreg = -1e-04)
std_errors <- c(ar1 = 0.0381, ar2 = 0.0371, ma1 = 0.0301, ma2 = 0.0332, xreg = 3e-04)
# Calculate t-statistics and corresponding p-values
t_statistics <- coefficients / std_errors
p_values <- 2 * pt(abs(t_statistics), df = length(nikkei_data_log_diff) - 5, lower.tail = FALSE)
# Print p-values to evaluate the statistical significance of the model's coefficients
print("P-values for the coefficients:")
print(p_values)
# P-value < 0.05 for all MA and AR coefficients, but the p-value for the X coefficient is > 0.05





##############  TOPIX TRANSPORTATION DATA #####################################

# Reading the CSV file Transportation
transportation_data <- read.csv("~/Downloads/Topix_Transportation.csv", sep = ",", header = TRUE)

print(transportation_data$Close) # see the column closed prices

# Conversion of "close" column to numeric type
transportation_data$Close <- as.numeric(as.character(transportation_data$Close))


##########  STATIONARITY OF THE DATA  ##############

# Log transformation and First Differencing
transportation_data_log_diff <- diff(log(transportation_data$Close), differences = 1)

# It computes the natural logarithm of the "Close" and calculates the difference
# between consecutive log-transformed data points. This differencing is used
# to make the time-series stationary.

# Create a new DataFrame aligned to the differenced data (removing the first date due to differencing)
adjusted_dates <- transportation_data$Date[-1]
adjusted_dummy <- transportation_data$Dummy[-1]
adjusted_dummy_over_0.5 <- transportation_data$Dummy_over_0.5[-1]
adjusted_dummy_over_0.75 <- transportation_data$Dummy_over_0.75[-1]
adjusted_dummy_over_0.90 <- transportation_data$Dummy_over_0.90[-1]

data_frame_aligned <- data.frame(
  Date = adjusted_dates,   # Adjusted date column
  Log_Diff = transportation_data_log_diff,  # Log differenced 'Close' prices
  Dummy = adjusted_dummy, # Adjusted dummy variable column
  Dummy_over_0.5 = adjusted_dummy_over_0.5, # Adjusted dummy over median variable column
  Dummy_over_0.75 = adjusted_dummy_over_0.75, # Adjusted dummy over 0.75 quantile variable column
  Dummy_over_0.90 = adjusted_dummy_over_0.90 # Adjusted dummy over 0.90 quantile variable column
)
summary(data_frame_aligned)

# Convert the "Date" column to Date type
data_frame_aligned$Date <- as.Date(data_frame_aligned$Date, format = "%d.%m.%Y")
str(data_frame_aligned$Date) # verify the conversion

# Identify missing values in "Close" column
missing_rows_in_logdiff <- which(is.na(data_frame_aligned$Log_Diff))
print(paste("Missing values in 'LogDiff' are in rows: ", toString(missing_rows_in_logdiff)))

# Remove rows with NA in log_Diff
data_frame_cleaned <- data_frame_aligned[!is.na(data_frame_aligned$Log_Diff), ]
str(data_frame_cleaned) # print the structure to confirm rows are dropped

# Plotting the log-differenced data
plot(transportation_data_log_diff,
     type = "l",
     main = "Log Differenced Transportation Data",
     xlab = "Number of days",
     ylab = "Log Differenced Values")


##########  TESTING FOR STATIONARITY  ##############

install.packages("tseries")
library(tseries)

# Remove NA values
transportation_data_log_diff <- na.omit(transportation_data_log_diff)

# Augmented Dickey-Fuller (ADF) test for stationarity
adf_result <- adf.test(transportation_data_log_diff, alternative = "stationary")
print(adf_result)
# The p-value (0.01) is less than the threshold (0.05). We can reject the 
# null hypothesis and conclude that the data is stationary.


##########  IDENTIFY AR AND MA ORDERS  ##############

library(forecast)
install.packages("AEC")
library(AEC)

# ACF to identify AR orders
acf(transportation_data_log_diff)

# PACF to identify MA orders
pacf(transportation_data_log_diff)

# Akaike Information criterion
fit <- auto.arima(transportation_data_log_diff, ic = "aic")
summary(fit)
# The model is an ARIMA model with 0 AR terms and 0 MA terms.


##########  FIT ARMA-X MODEL  ##############

# In this section, we fit a ARMA-X model to see the effect of shocks (or dummy)
# on the log-differenced series of Topix_transportation closing prices.

print(transportation_data$Dummy) # Examine the dummy variable

index <- which(data_frame_cleaned$Dummy == 1)[1] # check data_framed cleaned index
first_row_with_dummy_one <- data_frame_cleaned[index, ] # extract the row using the index
# It locates and extracts the first instance in the cleaned data set where the
# dummy variable is 1.


##### FIT AN ARIMA(0,0,0) MODEL USING THE DUMMY VARIABLES AS X (54 observations)
arimax_model_dummy <- Arima(data_frame_cleaned$Log_Diff,
                      order=c(0,0,0), include.mean=FALSE,
                      xreg=data_frame_cleaned$Dummy)
summary(arimax_model_dummy)

coefficients <- c(xreg = 0.0008)
std_errors <- c(xreg = 0.0017)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# X have p-values larger than 0.05 --> statistically not significant



##### FIT AN ARIMA(0,0,0) MODEL USING THE DUMMY VARIABLES OVER THE MEDIAN AS X (27 observations)

arimax_model_dummy_over_median <- Arima(data_frame_cleaned$Log_Diff,
                                        order=c(0,0,0), include.mean=FALSE,
                                        xreg=data_frame_cleaned$Dummy_over_0.5)
summary(arimax_model_dummy_over_median)

coefficients <- c(xreg = 0.0024)
std_errors <- c(xreg = 0.0025)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# X have p-values larger than 0.05 --> statistically not significant



##### FIT AN ARIMA(0,0,0) MODEL USING THE DUMMY VARIABLES OVER THE 0.75 PERCENTILE AS X (13 observations)

arimax_model_dummy_over_0.75 <- Arima(data_frame_cleaned$Log_Diff,
                                        order=c(0,0,0), include.mean=FALSE,
                                        xreg=data_frame_cleaned$Dummy_over_0.75)
summary(arimax_model_dummy_over_0.75)

coefficients <- c(xreg = 0.0006)
std_errors <- c(xreg = 0.0036)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# X have p-values larger than 0.05 --> statistically not significant



##### FIT AN ARIMA(0,0,0) MODEL USING THE DUMMY VARIABLES OVER THE 0.90 PERCENTILE AS X (13 observations)

arimax_model_dummy_over_0.90 <- Arima(data_frame_cleaned$Log_Diff,
                                        order=c(0,0,0), include.mean=FALSE,
                                        xreg=data_frame_cleaned$Dummy_over_0.90)
summary(arimax_model_dummy_over_0.90)

coefficients <- c(xreg = -0.0009)
std_errors <- c(xreg = 0.0052)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# X have p-values larger than 0.05 --> statistically not significant



##############  TOPIX ELECTRIC POWER & GAS DATA #####################################

# Reading the CSV file Transportation
Utilities_data <- read.csv("~/Downloads/Topix_Utilities.csv", sep = ",", header = TRUE)

print(Utilities_data$Close) # see the column closed prices

# Conversion of "close" column to numeric type
Utilities_data$Close <- as.numeric(as.character(Utilities_data$Close))



##########  STATIONARITY OF THE DATA  ##############

# Log transformation and First Differencing
Utilities_data_log_diff <- diff(log(Utilities_data$Close), differences = 1)

# It computes the natural logarithm of the "Close" and calculates the difference
# between consecutive log-transformed data points. This differencing is used
# to make the time-series stationary.

# Create a new DataFrame aligned to the differenced data (removing the first date due to differencing)
adjusted_dates <- Utilities_data$Date[-1]
adjusted_dummy <- Utilities_data$Dummy[-1]
adjusted_dummy_over_0.5 <- Utilities_data$Dummy_over_0.5[-1]
adjusted_dummy_over_0.75 <- Utilities_data$Dummy_over_0.75[-1]
adjusted_dummy_over_0.90 <- Utilities_data$Dummy_over_0.90[-1]

data_frame_aligned <- data.frame(
  Date = adjusted_dates,   # Adjusted date column
  Log_Diff = Utilities_data_log_diff,  # Log differenced 'Close' prices
  Dummy = adjusted_dummy, # Adjusted dummy variable column
  Dummy_over_0.5 = adjusted_dummy_over_0.5, # Adjusted dummy over median variable column
  Dummy_over_0.75 = adjusted_dummy_over_0.75, # Adjusted dummy over 0.75 quantile variable column
  Dummy_over_0.90 = adjusted_dummy_over_0.90 # Adjusted dummy over 0.90 quantile variable column
)
summary(data_frame_aligned)

# Convert the "Date" column to Date type
data_frame_aligned$Date <- as.Date(data_frame_aligned$Date, format = "%d.%m.%Y")
str(data_frame_aligned$Date) # verify the conversion

# Identify missing values in "Close" column
missing_rows_in_logdiff <- which(is.na(data_frame_aligned$Log_Diff))
print(paste("Missing values in 'LogDiff' are in rows: ", toString(missing_rows_in_logdiff)))

# Remove rows with NA in log_Diff
data_frame_cleaned <- data_frame_aligned[!is.na(data_frame_aligned$Log_Diff), ]
str(data_frame_cleaned) # print the structure to confirm rows are dropped

# Plotting the log-differenced data
plot(Utilities_data_log_diff,
     type = "l",
     main = "Log Differenced Real Estate Data",
     xlab = "Number of days",
     ylab = "Log Differenced Values")



##########  TESTING FOR STATIONARITY  ##############

install.packages("tseries")
library(tseries)

# Remove NA values
Utilities_data_log_diff <- na.omit(Utilities_data_log_diff)

# Augmented Dickey-Fuller (ADF) test for stationarity
adf_result <- adf.test(Utilities_data_log_diff, alternative = "stationary")
print(adf_result)
# The p-value (0.01) is less than the threshold (0.05). We can reject the 
# null hypothesis and conclude that the data is stationary.



##########  IDENTIFY AR AND MA ORDERS  ##############

library(forecast)
install.packages("AEC")
library(AEC)

# ACF to identify AR orders
acf(Real_Estate_data_log_diff)

# PACF to identify MA orders
pacf(Real_Estate_data_log_diff)

# Akaike Information criterion
fit <- auto.arima(Utilities_data_log_diff, ic = "aic")
summary(fit)
# The model is an ARIMA model with 0 AR terms and 1 MA terms.



##########  FIT ARMA-X MODEL  ##############

# In this section, we fit a ARMA-X model to see the effect of shocks (or dummy)
# on the log-differenced series of Topix_transportation closing prices.

print(Utilities_data$Dummy) # Examine the dummy variable

index <- which(data_frame_cleaned$Dummy == 1)[1] # check data_framed cleaned index
first_row_with_dummy_one <- data_frame_cleaned[index, ] # extract the row using the index
# It locates and extracts the first instance in the cleaned data set where the
# dummy variable is 1.

# Fit an ARIMA (0,0,1) model with dummy variable as an exogenous regressor
arimax_model <- Arima(data_frame_cleaned$Log_Diff,
                      order=c(0,0,1), include.mean=FALSE,
                      xreg=data_frame_cleaned$Dummy)
summary(arimax_model)


coefficients <- c(ma1 = 0.0449, xreg = 0.0018)
std_errors <- c(ma1 = 0.0178, xreg = 0.0020)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# MA1 has p-values below 0.05 --> statistically significant
# X have p-values larger than 0.05 --> statistically not significant


# Fit an ARIMA (0,0,1) model with dummy variable over the median as an exogenous regressor
arimax_model_dummy_over_0.5 <- Arima(data_frame_cleaned$Log_Diff,
                      order=c(0,0,1), include.mean=FALSE,
                      xreg=data_frame_cleaned$Dummy_over_0.5)
summary(arimax_model_dummy_over_0.5)


coefficients <- c(ma1 = 0.0455, xreg = 0.0043)
std_errors <- c(ma1 = 0.0178, xreg = 0.0028)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# MA1 has p-values below 0.05 --> statistically significant
# X have p-values larger than 0.05 --> statistically not significant


# Fit an ARIMA (0,0,1) model with dummy variable over the 0.75 percentile as an exogenous regressor
arimax_model_dummy_over_0.75 <- Arima(data_frame_cleaned$Log_Diff,
                                     order=c(0,0,1), include.mean=FALSE,
                                     xreg=data_frame_cleaned$Dummy_over_0.75)
summary(arimax_model_dummy_over_0.75)


coefficients <- c(ma1 = 0.0450, xreg = 0.0025)
std_errors <- c(ma1 = 0.0178, xreg = 0.0041)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# MA1 has p-values below 0.05 --> statistically significant
# X have p-values larger than 0.05 --> statistically not significant


# Fit an ARIMA (0,0,1) model with dummy variable over the 0.90 percentile as an exogenous regressor
arimax_model_dummy_over_0.90 <- Arima(data_frame_cleaned$Log_Diff,
                                      order=c(0,0,1), include.mean=FALSE,
                                      xreg=data_frame_cleaned$Dummy_over_0.90)
summary(arimax_model_dummy_over_0.90)


coefficients <- c(ma1 = 0.0448, xreg = -4e-04)
std_errors <- c(ma1 = 0.0179, xreg = 6e-03)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# MA1 has p-values below 0.05 --> statistically significant
# X have p-values larger than 0.05 --> statistically not significant


##############  TOPIX FINANCIALS (EX BANKS) DATA #####################################


# Reading the CSV file Transportation
insurance_data <- read.csv("~/Downloads/Topix_Insurance.csv", sep = ",", header = TRUE)

print(insurance_data$Close) # see the column closed prices

# Conversion of "close" column to numeric type
insurance_data$Close <- as.numeric(as.character(insurance_data$Close))



##########  STATIONARITY OF THE DATA  ##############

# Log transformation and First Differencing
insurance_data_log_diff <- diff(log(insurance_data$Close), differences = 1)

# It computes the natural logarithm of the "Close" and calculates the difference
# between consecutive log-transformed data points. This differencing is used
# to make the time-series stationary.

# Create a new DataFrame aligned to the differenced data (removing the first date due to differencing)
adjusted_dates <- insurance_data$Date[-1]
adjusted_dummy <- insurance_data$Dummy[-1]
adjusted_dummy_over_0.5 <- insurance_data$Dummy_over_0.5[-1]
adjusted_dummy_over_0.75 <- insurance_data$Dummy_over_0.75[-1]
adjusted_dummy_over_0.90 <- insurance_data$Dummy_over_0.90[-1]

data_frame_aligned <- data.frame(
  Date = adjusted_dates,   # Adjusted date column
  Log_Diff = insurance_data_log_diff,  # Log differenced 'Close' prices
  Dummy = adjusted_dummy, # Adjusted dummy variable column
  Dummy_over_0.5 = adjusted_dummy_over_0.5, # Adjusted dummy over median variable column
  Dummy_over_0.75 = adjusted_dummy_over_0.75, # Adjusted dummy over 0.75 quantile variable column
  Dummy_over_0.90 = adjusted_dummy_over_0.90 # Adjusted dummy over 0.90 quantile variable column
)
summary(data_frame_aligned)


# Convert the "Date" column to Date type
data_frame_aligned$Date <- as.Date(data_frame_aligned$Date, format = "%d.%m.%Y")
str(data_frame_aligned$Date) # verify the conversion

# Identify missing values in "Close" column
missing_rows_in_logdiff <- which(is.na(data_frame_aligned$Log_Diff))
print(paste("Missing values in 'LogDiff' are in rows: ", toString(missing_rows_in_logdiff)))

# Remove rows with NA in log_Diff
data_frame_cleaned <- data_frame_aligned[!is.na(data_frame_aligned$Log_Diff), ]
str(data_frame_cleaned) # print the structure to confirm rows are dropped

# Plotting the log-differenced data
plot(insurance_data_log_diff,
     type = "l",
     main = "Log Differenced Insurance Data",
     xlab = "Number of days",
     ylab = "Log Differenced Values")



##########  TESTING FOR STATIONARITY  ##############

install.packages("tseries")
library(tseries)

# Remove NA values
insurance_data_log_diff <- na.omit(insurance_data_log_diff)

# Augmented Dickey-Fuller (ADF) test for stationarity
adf_result <- adf.test(insurance_data_log_diff, alternative = "stationary")
print(adf_result)
# The p-value (0.01) is less than the threshold (0.05). We can reject the 
# null hypothesis and conclude that the data is stationary.



##########  IDENTIFY AR AND MA ORDERS  ##############

library(forecast)
install.packages("AEC")
library(AEC)

# ACF to identify AR orders
acf(insurance_data_log_diff)

# PACF to identify MA orders
pacf(insurance_data_log_diff)

# Akaike Information criterion
fit <- auto.arima(insurance_data_log_diff, ic = "aic")
summary(fit)
# The model is an ARIMA model with 0 AR terms and 0 MA terms.



##########  FIT ARMA-X MODEL  ##############

# In this section, we fit a ARMA-X model to see the effect of shocks (or dummy)
# on the log-differenced series of Topix_transportation closing prices.

print(insurance_data$Dummy) # Examine the dummy variable

index <- which(data_frame_cleaned$Dummy == 1)[1] # check data_framed cleaned index
first_row_with_dummy_one <- data_frame_cleaned[index, ] # extract the row using the index
# It locates and extracts the first instance in the cleaned data set where the
# dummy variable is 1.

# Fit an ARIMA (0,0,0) model with dummy variable as an exogenous regressor
arimax_model <- Arima(data_frame_cleaned$Log_Diff,
                      order=c(0,0,0), include.mean=FALSE,
                      xreg=data_frame_cleaned$Dummy)
summary(arimax_model)

coefficients <- c(xreg = -0.0014)
std_errors <- c(xreg = 0.0023)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# X have p-values larger than 0.05 --> statistically not significant


# Fit an ARIMA (0,0,0) model with dummy variable over the median as an exogenous regressor
arimax_model_dummy_over_0.5 <- Arima(data_frame_cleaned$Log_Diff,
                      order=c(0,0,0), include.mean=FALSE,
                      xreg=data_frame_cleaned$Dummy_over_0.5)
summary(arimax_model_dummy_over_0.5)

coefficients <- c(xreg = -0.0004)
std_errors <- c(xreg = 0.0032)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# X have p-values larger than 0.05 --> statistically not significant



# Fit an ARIMA (0,0,0) model with dummy variable over the 0.75 percentile as an exogenous regressor
arimax_model_dummy_over_0.75 <- Arima(data_frame_cleaned$Log_Diff,
                      order=c(0,0,0), include.mean=FALSE,
                      xreg=data_frame_cleaned$Dummy_over_0.75)
summary(arimax_model_dummy_over_0.75)

coefficients <- c(xreg = -0.0041)
std_errors <- c(xreg = 0.0047)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# X have p-values larger than 0.05 --> statistically not significant


# Fit an ARIMA (0,0,0) model with dummy variable over the 0.90 percentile as an exogenous regressor
arimax_model_dummy_over_0.90 <- Arima(data_frame_cleaned$Log_Diff,
                                      order=c(0,0,0), include.mean=FALSE,
                                      xreg=data_frame_cleaned$Dummy_over_0.90)
summary(arimax_model_dummy_over_0.90)

coefficients <- c(xreg = -0.0097)
std_errors <- c(xreg = 0.0069)

# Compute t-statistics
t_statistics <- coefficients / std_errors

# Degrees of freedom (depends on the sample size and the number of parameters estimated)
df <- 100  # Replace 100 with your actual degrees of freedom

# Calculate p-values
p_values <- 2 * pt(abs(t_statistics), df, lower.tail = FALSE)
print(p_values)

# X have p-values larger than 0.05 --> statistically not significant
