##TIMESERIESANALYSISUSINGARIMAMODELFORFORECASTINGINRPRACTICAL_ResearchGate
library(tseries)
library(forecast)
setwd("C:\\Users\\my lenovo\\Documents\\RStudio_Projects\\Supply Chain Data Science Project\\temperature")
temp<- read.csv("daily-minimum-temperatures-in-me.csv")
temp
##perform EDA on temp dataset
library(ggplot2)
library(gridExtra)

str(temp)
##view class for each column in dataset
class(temp$Date)
class(temp$Temperatures)

##convert date colunn from chr to date format
temp$Date <- as.Date(temp$Date)
class(temp$Date)
head(temp$Date)

##convert temperature from chr to date format
head(temp$Temperatures)
temp$Temperatures <- as.numeric(temp$Temperatures)
class(temp$Temperatures)

head(temp)
tail(temp)

p1 <- ggplot(temp, aes(x = Date, y = Temperatures)) +
  ylab("temperature") +
  xlab("dates") +
  geom_line()

p1

### to forecast using ARIMA for the next 365 days
library(forecast)
fit_temperatures <- auto.arima(temp$Temperatures)
fcast_temperatures <- forecast(fit_temperatures, h=365)
fcast_temperatures
head(fcast_temperatures)
plot(fcast_temperatures)
plot(fcast_temperatures$residuals)
qqnorm(fcast_temperatures$residuals)
acf(fcast_temperatures$residuals)

##ts.plot(temp,exp(fcast_temperatures$fcast_temperatures), log = "y", lty = c(1,3))


accuracy(fit_temperatures)

##temperature <- read.csv("temperature.csv", header = TRUE)
##temperature
##temperature <- ts(temperature, start = c(1981,1), frequency = 365)
##plot.ts(temperature)
##test data for stationarity using adf test
##H0: Unit root
##adf.test(temperature)
