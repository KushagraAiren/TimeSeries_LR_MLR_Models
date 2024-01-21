# Installing and loading necessary libraries
# install.packages(c("ggplot2","forecast", "TSA"))
# install.packages("lubridate")
# install.packages("zoo")

library(ggplot2)
library(forecast)
library(TSA)
library(lubridate)
library(stats)
library(dplyr)
library(zoo) 

# Loading the dataset into the variable weather_data
weather_data <- read.csv("weather_revised.csv")
print(head(weather_data))

# Keeping only the mean wind speed variable and date
weather_data <- weather_data[, c("date", "wdsp.Mean.Wind.Speed...knot.")]

# Updated dataframe
head(weather_data)

# Checking Null values if there are any
print(colSums(is.na(weather_data)))

# Printing summary of the dataset
print(summary(weather_data))

# Checking dataframe
print(str(weather_data))

# Converting 'date' to Date type as it is in chr Date Type
# Extracting day and month from the original date
day_month <- sub("(\\d+-\\w+)-\\d\\d", "\\1", weather_data$date)

# Adjusting the year part
year_part <- as.numeric(sub(".*-(\\d\\d)$", "\\1", weather_data$date))
cutoff_year <- 23
adjusted_year <- ifelse(year_part <= cutoff_year, 2000 + year_part, 1900 + year_part)

# Combining day, month, and adjusted year to create a full date string
full_date_str <- paste(day_month, adjusted_year, sep = "-")

# Converting to date
weather_data$date <- as.Date(full_date_str, format = "%d-%b-%Y")

# Updated Dataframe
head(weather_data)

# Validating Date by checking the maximum date in the dataset
max_date <- max(weather_data$date)
max_date

# Decomposing the time series to observe components
ts_wind_speed <- ts(weather_data$`wdsp.Mean.Wind.Speed...knot.`, start = c(1942, 1), frequency = 365)
decomposed <- stl(ts_wind_speed, s.window = "periodic")

# Plotting the original time series with a smoother trend line
ggplot(weather_data, aes(x = date, y = `wdsp.Mean.Wind.Speed...knot.`)) + 
  geom_line(color = "black", alpha = 0.5) +  
  geom_smooth(method = "loess", span = 0.2, color = "blue") +  # Smoothing line for trend
  labs(title = "Time Series Analysis of Mean Wind Speed (knots)",
       x = "Date", 
       y = "Mean Wind Speed (knots)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Validating Seasonality by plotting box plot
ggplot(weather_data, aes(x = as.factor(month(date)), y = wdsp.Mean.Wind.Speed...knot.)) +
  geom_boxplot(fill = "skyblue", color = "steelblue") +
  labs(title = "Seasonal Plot of wdsp(Mean Wind Speed - knot)",
       x = "Month",
       y = "wdsp(Mean Wind Speed - knot)") +
  theme_minimal()

# Plotting Seasonal decomposition of mean wind speed
autoplot(decomposed) + 
  labs(title = "Decomposed Time Series of Mean Wind Speed (knots)",
       x = "Date",
       y = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Splitting the data into train and test subset
train_data <- subset(weather_data, format(date, "%Y") %in% c("2019", "2020", "2021", "2022"))
test_data <- subset(weather_data, format(date, "%Y") == "2023")
str(train_data)
str(test_data)

#Validating train_data and test_data
head(train_data)
head(test_data)

# Applying Simple Moving Average model on Training Data
window_size <- 12
train_data$sma_wind_speed <- stats::filter(train_data$`wdsp.Mean.Wind.Speed...knot.`, rep(1/window_size, window_size), sides = 2)


# Plotting the SMA predictions with actual data
plot(train_data$date, train_data$`wdsp.Mean.Wind.Speed...knot.`, type = "l", col = "grey", xlab = "Date", ylab = "Wind Speed (knot)", main = "SMA vs Actual Data", cex = 1, pch = 50)
lines(train_data$date, train_data$sma_wind_speed, col = "red", lty = 2, lwd = 1)  # Dashed line for SMA

# Adding a legend to the plot
legend("topright", legend = c("Actual Data", "SMA"), col = c("grey", "red"), lty = c(1, 2))

test_data$sma_wind_speed <- stats::filter(test_data$wdsp.Mean.Wind.Speed...knot., rep(1/window_size, window_size), sides = 2)
# To evaluate the SMA model, comparing the SMA values with the actual wind speed values in the training data
mae_sma <- mean(abs(train_data$`wdsp.Mean.Wind.Speed...knot.` - train_data$sma_wind_speed), na.rm = TRUE)
cat("Mean Absolute Error (MAE) of SMA on Training Data:", mae_sma, "\n")

# Calculate Mean Squared Error (MSE)
mse_sma <- mean((test_data$`wdsp.Mean.Wind.Speed...knot.` - test_data$sma_wind_speed)^2, na.rm = TRUE)

# Calculate Root Mean Squared Error (RMSE)
rmse_sma <- sqrt(mse_sma)

calculate_mape <- function(actual, forecast) {
  actual_non_zero <- actual[actual != 0]
  forecast_non_zero <- forecast[actual != 0]
  mape <- mean(abs((actual_non_zero - forecast_non_zero) / actual_non_zero), na.rm = TRUE) * 100
  return(mape)
}

# Calculate MAPE for SMA
mape_sma <- calculate_mape(test_data$`wdsp.Mean.Wind.Speed...knot.`,  test_data$sma_wind_speed)

# Calculate Mean Absolute Error (MAE) on Test Data
mae_sma_test <- mean(abs(test_data$`wdsp.Mean.Wind.Speed...knot.` - test_data$sma_wind_speed), na.rm = TRUE)

# Print the results
cat("Mean Squared Error (MSE) for SMA:", mse_sma, "\n")
cat("Root Mean Squared Error (RMSE) for SMA:", rmse_sma, "\n")
cat("Mean Absolute Percentage Error (MAPE) for SMA:", mape_sma, "%\n")
cat("Mean Absolute Error (MAE) for SMA:", mae_sma_test, "\n")

# Dropping sma_wind_speed column
train_data <- train_data[ , !(names(train_data) %in% c("sma_wind_speed"))]

str(train_data)



# dropping null values if there are any
test_data <- na.omit(test_data)



# Aggregating Training Data by month
train_data <- train_data %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(mean_wind_speed = mean(`wdsp.Mean.Wind.Speed...knot.`, na.rm = TRUE))

# Aggregating Test Data by month
test_data <- test_data %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(mean_wind_speed = mean(`wdsp.Mean.Wind.Speed...knot.`, na.rm = TRUE))

head(train_data)
head(test_data)

# Converting training and test data to time series objects
ts_train <- ts(train_data$mean_wind_speed, start = c(2019, 1), frequency = 12)
ts_test <- ts(test_data$mean_wind_speed, start = c(2023, 1), frequency = 12)

# Fitting an ETS model on the training data
ets_model <- ets(ts_train)

# Forecasting using the fitted model
ets_forecast <- forecast(ets_model, h = length(ts_test))

# Extracting the forecast values
ets_predictions <- ets_forecast$mean

# Evaluating the model
mae_ets <- mean(abs(ts_test - ets_predictions), na.rm = TRUE)
cat("Mean Absolute Error (MAE) of ETS:", mae_ets, "\n")


#  Plotting the forecast along with the actual data for visualization
plot(ets_forecast, main = "ETS Model Forecast vs Actual")
lines(ts_test, col = "red", lty = 2) 
legend("bottomleft", legend = c("ETS Forecast", "Actual Data"), col = c("black", "red"), lty = 1:2)

# Evaluating the ETS model
mse_ets <- mean((ts_test - ets_predictions)^2, na.rm = TRUE)
cat("Mean Squared Error (MSE) of ETS:", mse_ets, "\n")

mape_ets <- mean(abs((ts_test - ets_predictions) / ts_test)) * 100
cat("Mean Absolute Percentage Error (MAPE) of ETS:", mape_ets, "%\n")

# Summary of the ETS model
summary(ets_model)

# Checking for stationarity in the training data
adf_test <- adf.test(ts_train)
p_value <- adf_test$p.value

print(adf_test)
print(p_value)

# Performing differencing as the data currently is non stationary
if (p_value > 0.05) {
  ts_diff <- diff(ts_train)
} else {
  ts_diff <- ts_train
}

# Checking for stationarity again with the diffrenced training data
adf_test <- adf.test(ts_diff)
p_value <- adf_test$p.value

print(adf_test)
print(p_value)

# Ploting ACF
acf(ts_diff, main = 'Autocorrelation Function (ACF)')

# Ploting PACF
pacf(ts_diff, main = 'Partial Autocorrelation Function (PACF)')

# Converting to time series object
ts_diff <- ts(ts_diff, frequency = 12, start = c(2019, 2))

# Fitting SARIMA model
sarima_model <- Arima(ts_diff, order = c(1, 0, 1), seasonal = c(1, 1, 0))

# Summary of the model
summary(sarima_model)

# Diagnostics Check
checkresiduals(sarima_model)

# Generating diagnostic plots
sarima_forecast <- forecast(sarima_model)
plot(sarima_forecast)


# Converting 'month' to a Date object
test_data$month <- as.yearmon(test_data$month)

# Converting test_data to a time series object
ts_test <- ts(test_data$mean_wind_speed, frequency = 12, start = c(2023, 1))

# Making forecasts
sarima_forecast <- forecast(sarima_model, h = length(ts_test))

# Plotting the forecasts
plot(sarima_forecast, main = 'SARIMA Forecast')
lines(ts_test, col = 'red', lty = 2)  # Add the actual values in red
legend('topright', legend = c('Forecast', 'Actual'), col = c('black', 'red'), lty = 1:2)

# Printing the forecast summary
print(sarima_forecast)

