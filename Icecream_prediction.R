## https://www.r-bloggers.com/2017/05/forecasting-arimax-model-exercises-part-5/

setwd("C:\\Users\\my lenovo\\Desktop\\MSc Operations and Supply Chain Management\\Modules\\Dissertation\\datasets\\Miscellaneous/")

df <- read.csv("Icecream.csv")

df
nrow(df)

library(ggplot2)
library(gridExtra)

p1 <- ggplot(df, aes(x = periods, y = cons)) +
  ylab("Consumption") +
  xlab("Periods") +
  geom_line() +
  expand_limits(x = 0, y = 0)

p1

p2 <- ggplot(df, aes(x = periods, y = income)) +
  ylab("Income") +
  xlab("Periods") +
  geom_line() +
  expand_limits(x = 0, y = 70)
p2

p3 <- ggplot(df, aes(x = periods, y = price)) +
  ylab("price") +
  xlab("Periods") +
  geom_line() +
  expand_limits(x = 0, y = 0.25)
p3

p4 <- ggplot(df, aes(x = periods, y = temp)) +
  ylab("temp") +
  xlab("Periods") +
  geom_line() +
  expand_limits(x = 0, y = 0)
p4

grid.arrange(p1, p2, p3, p4, ncol=1, nrow=4)

library(forecast)
fit_cons <- auto.arima(df$cons)
fcast_cons <- forecast(fit_cons, h = 6)
fcast_cons

##y_cons = rbind(df$cons,fcast_cons)
##y_cons
plot1 <- autoplot(fcast_cons)

accuracy(fit_cons)

# predict temp
fit_temp <- auto.arima(df$temp)
fcast_temp <- forecast(fit_temp, h = 6)
fcast_temp

autoplot(fcast_temp)


fit_cons_temp <- auto.arima(df$cons, xreg = df$temp)
fcast_temperature <- c(72.63688,68.88137,60.08721,48.81045,37.57558,28.91481)
fcast_cons_temp <- forecast(fit_cons_temp, xreg = fcast_temperature, h = 6)
fcast_cons_temp
plot2 <- autoplot(fcast_cons_temp)

grid.arrange(plot1, plot2, ncol=1, nrow=2)

summary(fcast_cons_temp)
summary(fcast_cons)

library(lmtest)
coeftest(fit_cons_temp)

temp_column <- matrix(df$temp, ncol = 1)
income <- c(NA, NA, df$income)
income_matrix <- embed(income, 3)
vars_matrix <- cbind(temp_column, income_matrix)
print(vars_matrix)

vars_matrix[, 1:4]

fit_vars_0 <- auto.arima(df$cons, xreg = vars_matrix[, 1:2])
fit_vars_1 <- auto.arima(df$cons, xreg = vars_matrix[, 1:3])
fit_vars_2 <- auto.arima(df$cons, xreg = vars_matrix[, 1:4])

summary(fit_vars_0)
summary(fit_vars_1)
summary(fit_vars_2)

print(fit_vars_0$aic)
print(fit_vars_1$aic)
print(fit_vars_2$aic)

#fit_inc <- auto.arima(df$income)
#fcast_inc <- forecast(fit_inc, h = 6)
#fcast_inc
#autoplot(fcast_inc)

expected_temp_income <- matrix(c(fcast_temperature, 91, 91, 93, 96, 96, 96),
                               ncol = 2, nrow = 6)
expected_temp_income
fcast_cons_temp_income <- forecast(fit_vars_0,
                                   xreg = expected_temp_income,
                                   h = 6)
autoplot(fcast_cons_temp_income)

accuracy(fcast_cons_temp_income)


# the model with two external regressors has the lowest 

# mean absolute scaled error (0.7290753)