#' ---
#' title: Time Series Coursework 1
#' author: James David 210928709
#' date: 17th March 2025
#' ---

# 1. Package Installation and Libraries ----------------------------------------------------
# Here I will install all packages that will be used in the rest of the project
install.packages("prophet")
install.packages("ggplot2")
install.packages("dplyr")
library("prophet")
library("ggplot2")
library("dplyr")
library("zoo")

# 2. Initial Exploration -----------------------------------------------------
# The following commands will bring up R documentation regarding specific data
# and packages. Doing this first has allowed me to get a better grasp of them
# before moving forward.
?prophet
?ldeaths
?make_future_dataframe
?zoo
?frequency
?ggplot2
?dplyr
?predict
browseVignettes(package = "dplyr")


# 3. Rough Code --------------------------------------------------------------------
# In this section I will be working on the code that is to be transferred to
# the RMD file

# 3.1 Summary Code ------------------------------------------------------------
# I am using this for a brief introduction of my data set.
ldeaths.df = data.frame(
    ds=zoo::as.yearmon(time(ldeaths)),
    y=ldeaths) # This creates the data frame for our time series
head(ldeaths.df,1) # This prints the first entry data frame
tail(ldeaths.df,1) # This prints the last entry of the data frame
frequency(ldeaths) # This shows the frequency of data collection in our case monthly
plot(ldeaths) # Finally this is a basic plot of the time series to give some context before analysis


# 3.2 Decomposition Code --------------------------------------------------
compositionldeaths = decompose(ldeaths)
plot(compositionldeaths)

# 3.3 Analysis using Prophet ----------------------------------------------
ldeaths.df = data.frame(
    ds=zoo::as.yearmon(time(ldeaths)),
    y=ldeaths)
model = prophet::prophet(ldeaths.df)
future = prophet::make_future_dataframe(model, periods=36, freq="month")
forecast = predict(model, future)
plot(model,forecast)


# 3.4 Prophet Forecasting Plot ------------------------------------------------
# This was an attempt to remodel the prophet data into a new graph
# i wasn't able to make this code output the result that I wanted so it will not
# be included in the rmd document but I have left it here to show my thought process.
cutoffdate = as.Date("1979-12-31")
ldeathstrainingperiod = ldeaths.df %>% filter(ldeaths.df$ds < train_cutoff)
ldeathstestperiod = ldeaths.df %>% filter(ldeaths.df$ds >= train_cutoff)
plot <- ggplot() +
    geom_point(data = df_train, aes(x = ds, y = y), color = "black", size = 1.5) +  # Training data
    geom_point(data = df_test, aes(x = ds, y = y), color = "red", size = 1.5) +    # Test data
    geom_line(data = forecast, aes(x = ds, y = yhat), color = "blue", size = 1) +  # Forecast line
    geom_ribbon(data = forecast, aes(x = ds, ymin = yhat_lower, ymax = yhat_upper),
                fill = "lightblue", alpha = 0.2) +  # Confidence interval
    geom_vline(xintercept = as.numeric(cutoffdate), linetype = "dashed", color = "red", size = 1) +  # Training/test split
    labs(title = "Prophet Forecast with Training/Test Data", x = "Date", y = "Deaths per Month") +
    theme_minimal()






