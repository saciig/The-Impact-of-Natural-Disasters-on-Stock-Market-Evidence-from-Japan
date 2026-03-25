##################################################################
############### GARCH ANALYSIS  ##################################

# Isaac Graber
# Adrien Currat
# Mathéo Bourgeois


# Install and load necessary packages
install.packages("rugarch")
library(rugarch)
library(forecast)
library(tseries)


##############  NIKKEI DATA #####################################

# Load the CSV file into R
nikkei_data <- read.csv("~/Desktop/MacroProject/NikkeiDailyData1.csv", sep = ";", header = TRUE)

# Convert the Date column to Date type
nikkei_data$Date <- as.Date(nikkei_data$Date, format="%d.%m.%Y")

# Convert relevant columns to numeric
nikkei_data$Adj.Close <- as.numeric(gsub(",", "", nikkei_data$Adj.Close))
nikkei_data$Dummy <- as.numeric(nikkei_data$Dummy)

# Calculate daily log returns and align lengths
log_returns <- diff(log(nikkei_data$Adj.Close))
nikkei_data <- nikkei_data[-1, ]  # Remove the first row to match the length of log_returns
nikkei_data$Log_Returns <- log_returns

# Remove rows with NA values in Log_Returns
nikkei_data <- na.omit(nikkei_data)


##############  Identify the orders of the GARCH model with information criteria


# Function to fit GARCH models with different orders and return the AIC and BIC
fit_garch_models <- function(data, p_max, q_max, external_regressors) {
  results <- data.frame(p = integer(), q = integer(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)
  
  for (p in 0:p_max) {
    for (q in 0:q_max) {
      spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(p, q), external.regressors = external_regressors),
        mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
        distribution.model = "norm"
      )
      fit <- tryCatch(
        {
          ugarchfit(spec = spec, data = data)
        },
        error = function(e) {
          return(NULL)
        },
        warning = function(w) {
          return(NULL)
        }
      )
      
      if (!is.null(fit)) {
        aic <- infocriteria(fit)[1]
        bic <- infocriteria(fit)[2]
        if (!is.nan(aic) && !is.nan(bic)) {
          results <- rbind(results, data.frame(p = p, q = q, AIC = aic, BIC = bic))
        }
      }
    }
  }
  
  return(results)
}

# Fit GARCH models with different orders (e.g., p, q from 0 to 3)
garch_results <- fit_garch_models(nikkei_data$Log_Returns, 3, 3, matrix(nikkei_data$Dummy, ncol=1))

# Print the results
print(garch_results)

# Select the best model based on AIC and BIC
best_aic_model <- garch_results[which.min(garch_results$AIC),]
best_bic_model <- garch_results[which.min(garch_results$BIC),]

print(paste("Best model based on AIC: GARCH(", best_aic_model$p, ",", best_aic_model$q, ")", sep = ""))
print(paste("Best model based on BIC: GARCH(", best_bic_model$p, ",", best_bic_model$q, ")", sep = ""))

# It finds the model with the lowest AIC and BIC values and print the orders of the best models based
# on each criterion. The GARCH (1,1) is the best model based on both AIC and BIC criteria. 


##############  Dummy


# Define the GARCH(1,1) model specification
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(nikkei_data$Dummy, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = nikkei_data$Log_Returns)

# Print the summary of the fit
print(fit)

# Interpretation : The coefficient is 0.000011 and the p-value is 0.380333. 
# The coefficient is not statistically significant, indicating that natural disasters
# do not have a significant direct impact on market volatility.


##############  Dummy over 0.5

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(nikkei_data$Dummy_over_0.5, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = nikkei_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000040 and the p-value is 0.078337 
# The coefficient is not statistically significant at 5% level, but it is close.


##############  Dummy over 0.75

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(nikkei_data$Dummy_over_0.75, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = nikkei_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000092 and the p-value is 0.033583 
# The coefficient is statistically significant at 5% level, indicating that severe natural disasters
# have a significant impact on market volatility (however effect not significant with robust sd.)


# Now we can quantify the increase in the volatility

# Extract the conditional variances
cond_var <- sigma(fit)^2

# Calculate the average conditional variance in the absence of severe natural disasters
avg_cond_var_no_disaster <- mean(cond_var[nikkei_data$Dummy_over_0.75 == 0])

# Calculate the coefficient for the dummy variable (gamma)
gamma <- coef(fit)["vxreg1"]

# Compute the new conditional variance with the impact of severe natural disasters
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma

# Compute the volatilities (standard deviations)
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)

# Calculate the increase in volatility due to the disaster
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The volatility increases by a factor of 1.23 when 
# a severe natural disaster occurs. 


##############  Dummy over 0.90

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(nikkei_data$Dummy_over_0.90, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = nikkei_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000148 and the p-value is 0.053175 
# The coefficient is close to be statistically significant at 5% level, indicating that severe natural disasters
# might have an impact on market volatility (however effect not significant with robust sd.)

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[nikkei_data$Dummy_over_0.90 == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The volatility increases by a factor of 1.35 when 
# a severe natural disaster occurs. Severe natural disasters introduce considerable
# uncertainty and risk into the market, leading to much higher volatility.



##############  TOPIX TRANSPORTATION DATA #####################################

# Load the CSV file into R
transportation_data <- read.csv("~/Desktop/MacroProject/Topix_Transportation.csv", sep = ",", header = TRUE)

# Convert the Date column to Date type
transportation_data$Date <- as.Date(transportation_data$Date, format="%Y-%m-%d")

# Convert relevant columns to numeric
transportation_data$Adj.Close <- as.numeric(transportation_data$Adj.Close)

# Remove rows with NA values in the relevant columns
transportation_data <- na.omit(transportation_data)

# Calculate daily log returns
log_returns <- diff(log(transportation_data$Adj.Close))
transportation_data <- transportation_data[-1, ]  # Remove the first row to match the length of log_returns
transportation_data$Log_Returns <- log_returns

# Remove any additional rows with NA values in Log_Returns
transportation_data <- na.omit(transportation_data)


##############  Identify the orders of the GARCH model with information criteria


# Function to fit GARCH models with different orders and return the AIC and BIC
fit_garch_models <- function(data, p_max, q_max, external_regressors) {
  results <- data.frame(p = integer(), q = integer(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)
  
  for (p in 0:p_max) {
    for (q in 0:q_max) {
      spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(p, q), external.regressors = external_regressors),
        mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
        distribution.model = "norm"
      )
      fit <- tryCatch(
        {
          ugarchfit(spec = spec, data = data)
        },
        error = function(e) {
          return(NULL)
        },
        warning = function(w) {
          return(NULL)
        }
      )
      
      if (!is.null(fit)) {
        aic <- infocriteria(fit)[1]
        bic <- infocriteria(fit)[2]
        if (!is.nan(aic) && !is.nan(bic)) {
          results <- rbind(results, data.frame(p = p, q = q, AIC = aic, BIC = bic))
        }
      }
    }
  }
  
  return(results)
}

# Fit GARCH models with different orders (e.g., p, q from 0 to 3)
garch_results <- fit_garch_models(transportation_data$Log_Returns, 3, 3, matrix(transportation_data$Dummy, ncol=1))

# Print the results
print(garch_results)

# Select the best model based on AIC and BIC
best_aic_model <- garch_results[which.min(garch_results$AIC),]
best_bic_model <- garch_results[which.min(garch_results$BIC),]

print(paste("Best model based on AIC: GARCH(", best_aic_model$p, ",", best_aic_model$q, ")", sep = ""))
print(paste("Best model based on BIC: GARCH(", best_bic_model$p, ",", best_bic_model$q, ")", sep = ""))

# The GARCH(2,3) is the best according to AIC, while the GARCH(1,1) is the best according to BIC. 
# We choose the GARCH(1,1) for model simplicity. 


##############  Dummy

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(transportation_data$Dummy, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = transportation_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000062 and the p-value is 0.004202 
# The coefficient is statistically significant at 5% level, indicating that natural disasters
# significantly increase market volatility (however effect not significant with robust sd.)

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[transportation_data$Dummy == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The volatility increases by a factor of 1.17 when 
# a natural disaster occurs. This means the volatility increased by 17%. 


####### Dummy over 0.5

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(transportation_data$Dummy_over_0.5, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = transportation_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000181 and the p-value is 0.001725 
# The coefficient is statistically significant at 5% level, indicating that natural disasters
# significantly increase market volatility (however effect not significant with robust sd.)

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[transportation_data$Dummy_over_0.5 == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The conditional variance increases by a factor of 1.45 when 
# a natural disaster above the median occurs. This means the volatility increased by 45%.


####### Dummy over 0.75

spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(transportation_data$Dummy_over_0.75, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = transportation_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000467 and the p-value is 0.000097 
# The coefficient is statistically significant at 5% level, indicating that severe natural disasters
# significantly increase market volatility (however effect not significant with robust sd.)

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[transportation_data$Dummy_over_0.75 == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The conditional variance increases by a factor of 1.96 when 
# a severe natural disaster occurs.This means that the volatility increased by 96%.


### Dynamic multiplier associated with a shock over the 0.75 quantile on transportation sector 

params <- coef(fit)
omega <- params["omega"]
alpha1 <- params["alpha1"]
beta1 <- params["beta1"]
vxreg1 <- params["vxreg1"]

# Calculate initial variance based on model parameters
initial_var <- omega / (1 - alpha1 - beta1)

# Define the function to simulate conditional variances
simulate_conditional_variance <- function(n_periods, omega, alpha1, beta1, vxreg1, initial_var) {
  sigma2 <- numeric(n_periods)
  sigma2[1] <- initial_var  # Start with the initial variance
  
  # Initialize dummy as zero and only trigger at the shock period
  dummy <- numeric(n_periods)
  dummy[2] <- 1  # Apply shock at period 2
  
  # Compute future variances
  for (t in 2:n_periods) {
    sigma2[t] <- omega + alpha1 * sigma2[t-1] + beta1 * sigma2[t-1] + vxreg1 * dummy[t]
    cat(sprintf("t: %d, sigma^2: %f\n", t, sigma2[t]))  
  }
  
  return(sqrt(sigma2))  
}

# Simulation settings
n_periods <-60
volatility_response <- simulate_conditional_variance(n_periods, omega, alpha1, beta1, vxreg1, initial_var)

# Plot the simulated dynamic multiplier
plot(volatility_response, type = "b", pch = 19, col = "blue", 
     ylim = c(min(volatility_response)*0.99, max(volatility_response)*1.01),
     xlab = "Time (periods)", ylab = "Conditional Volatility",
     main = "Dynamic multiplier, Volatility Response to a shock in t=2, TRANSPORTATION sector")
abline(h = sqrt(initial_var), col = "red", lty = 2)

# It takes about 50 days to reach again the unconditional volatility after a shock in t=2.


####### Dummy over 0.90


spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(transportation_data$Dummy_over_0.90, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = transportation_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000895 and the p-value is 0.003390 
# The coefficient is statistically significant at 5% level, indicating that very severe natural disasters
# significantly increase market volatility (however effect not significant with robust sd.)

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[transportation_data$Dummy_over_0.90 == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The volatility increased by a factor of 2.53 when 
# a severe natural disaster occurs.This means that the volatility increased by 153%.



##############  TOPIX ELECTRIC POWER & GAS DATA #####################################


# Load the CSV file into R
utilities_data <- read.csv("~/Desktop/MacroProject/Topix_Utilities.csv", sep = ",", header = TRUE)

# Convert the Date column to Date type
utilities_data$Date <- as.Date(utilities_data$Date, format="%d.%m.%Y")

# Convert relevant columns to numeric
utilities_data$Adj.Close <- as.numeric(utilities_data$Adj.Close)

# Remove rows with NA values in the relevant columns
utilities_data <- na.omit(utilities_data)

# Calculate daily log returns
log_returns <- diff(log(utilities_data$Adj.Close))
utilities_data <- utilities_data[-1, ]  # Remove the first row to match the length of log_returns
utilities_data$Log_Returns <- log_returns

# Remove any additional rows with NA values in Log_Returns
utilities_data <- na.omit(utilities_data)


##############  Identify the orders of the GARCH model with information criteria


# Function to fit GARCH models with different orders and return the AIC and BIC
fit_garch_models <- function(data, p_max, q_max, external_regressors) {
  results <- data.frame(p = integer(), q = integer(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)
  
  for (p in 0:p_max) {
    for (q in 0:q_max) {
      spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(p, q), external.regressors = external_regressors),
        mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
        distribution.model = "norm"
      )
      fit <- tryCatch(
        {
          ugarchfit(spec = spec, data = data)
        },
        error = function(e) {
          return(NULL)
        },
        warning = function(w) {
          return(NULL)
        }
      )
      
      if (!is.null(fit)) {
        aic <- infocriteria(fit)[1]
        bic <- infocriteria(fit)[2]
        if (!is.nan(aic) && !is.nan(bic)) {
          results <- rbind(results, data.frame(p = p, q = q, AIC = aic, BIC = bic))
        }
      }
    }
  }
  
  return(results)
}

# Fit GARCH models with different orders (e.g., p, q from 0 to 3)
garch_results <- fit_garch_models(utilities_data$Log_Returns, 3, 3, matrix(utilities_data$Dummy, ncol=1))

# Print the results
print(garch_results)

# Select the best model based on AIC and BIC
best_aic_model <- garch_results[which.min(garch_results$AIC),]
best_bic_model <- garch_results[which.min(garch_results$BIC),]

print(paste("Best model based on AIC: GARCH(", best_aic_model$p, ",", best_aic_model$q, ")", sep = ""))
print(paste("Best model based on BIC: GARCH(", best_bic_model$p, ",", best_bic_model$q, ")", sep = ""))

# The GARCH(1,3) is the best according to AIC, while the GARCH(1,1) is the best according to BIC. 
# We choose again the GARCH(1,1) for model simplicity. 



####### Dummy


spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(utilities_data$Dummy, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = utilities_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000088 and the p-value is 0.000008 
# The coefficient is statistically significant at 5% level, indicating that natural disasters
# significantly increase market volatility (however effect not significant with robust sd.)

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[utilities_data$Dummy == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The volatility increases by a factor of 1.18 when 
# a natural disaster occurs. This means the volatility increased by 18%.


####### Dummy over 0.5


spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(utilities_data$Dummy_over_0.5, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = utilities_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000136 and the p-value is 0.000005
# The coefficient is statistically significant at 5% level, indicating that natural disasters
# significantly increase market volatility (however effect not significant with robust sd.)

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[utilities_data$Dummy_over_0.5 == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The volatility increases by a factor of 1.26 when 
# a natural disaster occurs. This means the volatility increased by 26%.


####### Dummy over 0.75


spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(utilities_data$Dummy_over_0.75, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = utilities_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000215 and the p-value is 0.000015
# The coefficient is statistically significant at 5% level, indicating that natural disasters
# significantly increase market volatility (however effect not significant with robust sd.)

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[utilities_data$Dummy_over_0.75 == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The conditional variance increases by a factor of 1.40 when 
# a severe natural disaster occurs. This means the volatility increased by 40%.


### Dynamic multiplier associated with a shock over the 0.75 quantile on ELECTRIC POWER & GAS sector

params <- coef(fit)
omega <- params["omega"]
alpha1 <- params["alpha1"]
beta1 <- params["beta1"]
vxreg1 <- params["vxreg1"]

# Calculate initial variance based on model parameters
initial_var <- omega / (1 - alpha1 - beta1)

# Define the function to simulate conditional variances
simulate_conditional_variance <- function(n_periods, omega, alpha1, beta1, vxreg1, initial_var) {
  sigma2 <- numeric(n_periods)
  sigma2[1] <- initial_var  # Start with the initial variance
  
  # Initialize dummy as zero and only trigger at the shock period
  dummy <- numeric(n_periods)
  dummy[2] <- 1  # Apply shock at period 2
  
  # Compute future variances
  for (t in 2:n_periods) {
    sigma2[t] <- omega + alpha1 * sigma2[t-1] + beta1 * sigma2[t-1] + vxreg1 * dummy[t]
    cat(sprintf("t: %d, sigma^2: %f\n", t, sigma2[t]))  
  }
  
  return(sqrt(sigma2))  
}

# Simulation settings
n_periods <-200
volatility_response <- simulate_conditional_variance(n_periods, omega, alpha1, beta1, vxreg1, initial_var)

# Plot the simulated IRF
plot(volatility_response, type = "b", pch = 19, col = "blue", 
     ylim = c(min(volatility_response)*0.99, max(volatility_response)*1.01),
     xlab = "Time (periods)", ylab = "Conditional Volatility",
     main = "Dynamic multiplier, Volatility Response to a Shock in t=2 ELECTRIC POWER & GAS sector")
abline(h = sqrt(initial_var), col = "red", lty = 2)

# The process take about 200 days to come back to initial level.
# Even if the ELECTRIC POWER & GAS sector is less affected by the initial shock, this sector has bigger persistence of the volatility than the transportation sector 
# beta1= 0.8552864 in the ELECTRIC POWER & GAS sector and in the TRANSPORTATION sector, it was 0.7910667


####### Dummy over 0.90


spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(utilities_data$Dummy_over_0.90, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = utilities_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000378 and the p-value is 0.000067
# The coefficient is statistically significant at 5% level, indicating that natural disasters
# significantly increase market volatility (however effect not significant with robust sd.)

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[utilities_data$Dummy_over_0.75 == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The volatility increases by a factor of 1.64 when 
# a severe natural disaster occurs. This means the volatility increased by 64%.



##############  TOPIX FINANCIALS (EX BANKS) DATA #####################################


# Load the CSV file into R
insurance_data <- read.csv("~/Desktop/MacroProject/Topix_Insurance.csv", sep = ",", header = TRUE)

# Convert the Date column to Date type with the correct format
insurance_data$Date <- as.Date(insurance_data$Date, format="%Y-%m-%d")

# Convert relevant columns to numeric
insurance_data$Adj.Close <- as.numeric(insurance_data$Adj.Close)

# Remove rows with NA values in the relevant columns
insurance_data <- na.omit(insurance_data)

# Calculate daily log returns
log_returns <- diff(log(insurance_data$Adj.Close))
insurance_data <- insurance_data[-1, ]  # Remove the first row to match the length of log_returns
insurance_data$Log_Returns <- log_returns

# Remove any additional rows with NA values in Log_Returns
insurance_data <- na.omit(insurance_data)


##############  Identify the orders of the GARCH model with information criteria


# Function to fit GARCH models with different orders and return the AIC and BIC
fit_garch_models <- function(data, p_max, q_max, external_regressors) {
  results <- data.frame(p = integer(), q = integer(), AIC = numeric(), BIC = numeric(), stringsAsFactors = FALSE)
  
  for (p in 0:p_max) {
    for (q in 0:q_max) {
      spec <- ugarchspec(
        variance.model = list(model = "sGARCH", garchOrder = c(p, q), external.regressors = external_regressors),
        mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
        distribution.model = "norm"
      )
      fit <- tryCatch(
        {
          ugarchfit(spec = spec, data = data)
        },
        error = function(e) {
          return(NULL)
        },
        warning = function(w) {
          return(NULL)
        }
      )
      
      if (!is.null(fit)) {
        aic <- infocriteria(fit)[1]
        bic <- infocriteria(fit)[2]
        if (!is.nan(aic) && !is.nan(bic)) {
          results <- rbind(results, data.frame(p = p, q = q, AIC = aic, BIC = bic))
        }
      }
    }
  }
  
  return(results)
}

# Fit GARCH models with different orders (e.g., p, q from 0 to 3)
garch_results <- fit_garch_models(insurance_data$Log_Returns, 3, 3, matrix(insurance_data$Dummy, ncol=1))

# Print the results
print(garch_results)

# Select the best model based on AIC and BIC
best_aic_model <- garch_results[which.min(garch_results$AIC),]
best_bic_model <- garch_results[which.min(garch_results$BIC),]

print(paste("Best model based on AIC: GARCH(", best_aic_model$p, ",", best_aic_model$q, ")", sep = ""))
print(paste("Best model based on BIC: GARCH(", best_bic_model$p, ",", best_bic_model$q, ")", sep = ""))

# The GARCH(1,2) is the best according to AIC, while the GARCH(1,1) is the best according to BIC. 
# We choose again the GARCH(1,1) for model simplicity. 


####### Dummy


spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(insurance_data$Dummy, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = insurance_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000048 and the p-value is 0.013827
# The coefficient is statistically significant at 5% level.

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[insurance_data$Dummy == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The volatility increases by a factor of 1.07 when 
# a natural disaster occurs. This means the volatility increased by 7%.


####### Dummy over 0.5


spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(insurance_data$Dummy_over_0.5, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = insurance_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000078 and the p-value is 0.009839
# The coefficient is statistically significant at 5% level.

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[insurance_data$Dummy_over_0.5 == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The volatility increases by a factor of 1.12 when 
# a natural disaster over the median occurs. This means the volatility increased by 12%.


####### Dummy over 0.75


spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(insurance_data$Dummy_over_0.75, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = insurance_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000121 and the p-value is 0.023323
# The coefficient is statistically significant at 5% level.

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[insurance_data$Dummy_over_0.75 == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The conditional variance increases by a factor of 1.19 when 
# a natural disaster occurs. This means the volatility increased by 19%.


### Dynamic multiplier associated with a shock over the 0.75 quantile on FINANCIALS sector

params <- coef(fit)
omega <- params["omega"]
alpha1 <- params["alpha1"]
beta1 <- params["beta1"]
vxreg1 <- params["vxreg1"]

# Calculate initial variance based on model parameters
initial_var <- omega / (1 - alpha1 - beta1)

# Define the function to simulate conditional variances
simulate_conditional_variance <- function(n_periods, omega, alpha1, beta1, vxreg1, initial_var) {
  sigma2 <- numeric(n_periods)
  sigma2[1] <- initial_var  # Start with the initial variance
  
  # Initialize dummy as zero and only trigger at the shock period
  dummy <- numeric(n_periods)
  dummy[2] <- 1  # Apply shock at period 2
  
  # Compute future variances
  for (t in 2:n_periods) {
    sigma2[t] <- omega + alpha1 * sigma2[t-1] + beta1 * sigma2[t-1] + vxreg1 * dummy[t]
    cat(sprintf("t: %d, sigma^2: %f\n", t, sigma2[t]))  
  }
  return(sqrt(sigma2))  
}

# Simulation settings
n_periods <-150
volatility_response <- simulate_conditional_variance(n_periods, omega, alpha1, beta1, vxreg1, initial_var)

# Plot the simulated IRF
plot(volatility_response, type = "b", pch = 19, col = "blue", 
     ylim = c(min(volatility_response)*0.99, max(volatility_response)*1.01),
     xlab = "Time (periods)", ylab = "Conditional Volatility",
     main = "Dynamic multiplier, Volatility Response to a Shock in t=2, FINANCIAL sector")
abline(h = sqrt(initial_var), col = "red", lty = 2)

# The process take about 150 days to comeback to initial level
# Even if the FINANCIAL sector is the least affected by the initial shock, this sector has the biggest persistence of the volatility
# beta1= 0.8801025 in the FINANCIAL sector, in TRANSPORTATION sector, it was 0.7910667 and 0.8552864 in the ELECTRIC POWER & GAS sector


####### Dummy over 0.90


spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1), external.regressors = matrix(insurance_data$Dummy_over_0.90, ncol=1)),
  mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "norm"
)

fit <- ugarchfit(spec = spec, data = insurance_data$Log_Returns)
print(fit)

# Interpretation : The coefficient is 0.000409 and the p-value is 0.002317
# The coefficient is statistically significant at 5% level.

cond_var <- sigma(fit)^2
avg_cond_var_no_disaster <- mean(cond_var[insurance_data$Dummy_over_0.90 == 0])
gamma <- coef(fit)["vxreg1"]
avg_cond_var_with_disaster <- avg_cond_var_no_disaster + gamma
vol_no_disaster <- sqrt(avg_cond_var_no_disaster)
vol_with_disaster <- sqrt(avg_cond_var_with_disaster)
increase_in_volatility <- vol_with_disaster / vol_no_disaster
increase_in_volatility

# Interpretation : The conditional variance increases by a factor of 1.55 when 
# a natural disaster occurs. This means that the volatility increased by 55%.
