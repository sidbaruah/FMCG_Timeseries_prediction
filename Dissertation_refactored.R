##transposed the original dataset to arrange months as rows and different retail stores as columns
##Combined the data of 4 years from 2018-2020 present in different sheets to a single worksheet and converted it to .csv

setwd("C:\\Users\\my lenovo\\Desktop\\MSc Operations and Supply Chain Management\\Modules\\Dissertation\\datasets\\US Sales and Inventory Datasets")

df_sales <- read.csv("US_Sales_Final_Dataset.csv")
df_sales

library(ggplot2)
library(gridExtra)
library(scales)
library(fpp)
library(dplyr)
library(lubridate) 
library(vars)
library(mFilter)
library(tseries)
library(forecast)
library(TSstudio)
library(xts)
library(dynlm)
library(lmtest)
# install.packages("fpp")
# install.packages("vars")
# install.packages("mFilter")
# install.packages("TSstudio")
# install.packages("dynlm")

##checking for any missing values
sum(is.na(df_sales))

##checking the structure of dataset
str(df_sales)

##since all data in the data-frame are characters, we convert all to integer in excel


##change date column from character to date format


df_sales$Months <- as.Date(df_sales$months, "%d-%m-%Y")
df_sales$Months

##plotting the different columns against Months column using ggplot to check consumer buying patterns before and during the pandemic
p_food_beverage <- ggplot(df_sales, aes(x = Months, y = Food_and_beverage_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line(color="Orange") + ggtitle("Food and Beverage Stores") +scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_food_beverage


p_Grocery <- ggplot(df_sales, aes(x = Months, y = Grocery_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line(color="Blue") + ggtitle("Grocery Stores") +scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_Grocery

p_Supermarkets <- ggplot(df_sales, aes(x = Months, y = Supermarkets_and_other_grocery_except_convenience_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line(color="Green") +ggtitle("Supermarkets, other grocery except convenience stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_Supermarkets

grid.arrange(p_food_beverage,p_Grocery,p_Supermarkets, nrow=3, ncol=1)

p_liquor <- ggplot(df_sales, aes(x = Months, y = Beer_wine_and_liquor_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Beer, wine and liquor stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_liquor

p_personalCare <- ggplot(df_sales, aes(x = Months, y = Health_and_personal_care_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line(color="Orange") +ggtitle("Health and personal care stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_personalCare

p_pharmacy <- ggplot(df_sales, aes(x = Months, y = Pharmacies_and_drug_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line(color="Blue") +ggtitle("Pharmacies and drug stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_pharmacy  

grid.arrange(p_personalCare,p_pharmacy, nrow=2, ncol=1)


p_gas <- ggplot(df_sales, aes(x = Months, y = Gasoline_stations)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Gasoline stations")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))

p_gas  

p_clothing <- ggplot(df_sales, aes(x = Months, y = Clothing_and_clothing_access_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line(color="Orange") +ggtitle("Clothing and clothing access stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_clothing


p_mensClothing <- ggplot(df_sales, aes(x = Months, y = Mens_clothing_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line(color="blue") +ggtitle("Men's clothing stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_mensClothing

p_WomensClothing <- ggplot(df_sales, aes(x = Months, y = Womens_clothing_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line(color="Red") +ggtitle("Women's clothing stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_WomensClothing


p_familyClothing <- ggplot(df_sales, aes(x = Months, y = Family_clothing_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Family clothing stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_familyClothing

grid.arrange(p_clothing,p_mensClothing, p_WomensClothing, p_familyClothing, nrow=4, ncol=1)


p_shoes <- ggplot(df_sales, aes(x = Months, y = Shoe_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Shoe_stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_shoes


p_jewellery <- ggplot(df_sales, aes(x = Months, y = Jewelry_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("jewellery_stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_jewellery


p_hobby <- ggplot(df_sales, aes(x = Months, y = Sporting_goods_hobby_musical_instrument_and_book_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Sporting goods. hobby, musical instrument and book-stores")
p_hobby

p_books <- ggplot(df_sales, aes(x = Months, y = Book_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Only Book-stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_books


p_generalMerchandise <- ggplot(df_sales, aes(x = Months, y = General_merchandise_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("General merchandise stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_generalMerchandise


p_department <-ggplot(df_sales, aes(x = Months, y = Department_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Department stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_department


p_nodiscountstore <-ggplot(df_sales, aes(x = Months, y = Department_stores_excluding_discount_department_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Department stores excluding discount department stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_nodiscountstore


p_discountstore <-ggplot(df_sales, aes(x = Months, y = Discount_department_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Discount department stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_discountstore

grid.arrange(p_generalMerchandise, p_department, nrow=2, ncol=1)
grid.arrange( p_nodiscountstore, p_discountstore, nrow=2, ncol=1)

## Find correlation of death n stringency with all retail store types to check how much retail stores get affected by stringency or the number of deaths.
## Create a correlation matrix

### TODO: Why pearson and not kendall? what do they measure and why are they different?
cor(df_sales[1:25,2:20],df_sales[1:25,22:23], method = "pearson")
cor(df_sales[26:40,2:20],df_sales[26:40,22:23], method = "pearson")

cor(df_sales[,2:20],df_sales[,22:23], method = "kendall")
cor(df_sales[,2:20],df_sales[,22:23], method = "spearman")

# ## ADF test to check for stationarity
adf.test(df_sales$Food_and_beverage_stores)
## from the ADF test, the p-value = 0.4593 > 0.05 means that we fail to reject the null hypothesis. That  means the data is NOT stationary.

##now to convert the variable into stationary, we can try to convert it to log / 1st difference / 2nd difference
#convert to log
ln_food_beverage <- log(df_sales$Food_and_beverage_stores)
ln_food_beverage
adf.test(ln_food_beverage)
# here, p = 0.4257 > 0.05, therefore not stationary

#Checking first difference
d1_food_beverage <- diff(ln_food_beverage)
d1_food_beverage
adf.test(d1_food_beverage)
#here, p= 0.01 < 0.05, so we reject the null hypothesis and the series is stationary.
plot(d1_food_beverage, type = "l")
#Performing ARIMA on this stationary variable
fit_d1_food_beverage <- auto.arima(d1_food_beverage)
fcast_d1_food_beverage <- forecast(fit_d1_food_beverage, h = 12)
fcast_d1_food_beverage
autoplot(fcast_d1_food_beverage)
summary(fcast_d1_food_beverage)






#Checking first difference for rest of the columns


d1_grocery <- diff(log(df_sales$Grocery_stores))
d1_grocery
adf.test(d1_grocery)
plot(d1_grocery, type = "l")
fit_d1_grocery <- auto.arima(d1_grocery)
fcast_d1_grocery <- forecast(fit_d1_grocery, h = 12)
fcast_d1_grocery
autoplot(fcast_d1_grocery)
summary(fcast_d1_grocery)

d1_supermarkets <- diff(log(df_sales$Supermarkets_and_other_grocery_except_convenience_stores))
adf.test(d1_supermarkets)
plot(d1_supermarkets, type = "l")
fit_d1_supermarkets <- auto.arima(d1_supermarkets)
fcast_d1_supermarkets <- forecast(fit_d1_supermarkets, h = 12)
fcast_d1_supermarkets
autoplot(fcast_d1_supermarkets)
summary(fcast_d1_supermarkets)


d1_gas<- diff(log(df_sales$Gasoline_stations))
adf.test(d1_gas)
plot(d1_gas, type = "l")



##try ADF, KPSS and BOX test across all variables and print output in a data-frame
names(df_sales[2:20])
multi_stat_tests<- function(df_sales){
  p <- ncol(df_sales)
  df_multi <- data.frame(var = names(df_sales),
                         # box.pvalue=sapply(df_sales, function(v) Box.test(ts(diff(v)),lag=20,type="Ljung-Box")$p.value),
                         kpss.pvalue=sapply(df_sales, function(v) kpss.test(ts(diff(v)))$p.value),
                         adf.pvalue=sapply(df_sales, function(v) adf.test(ts(diff(v)),alternative = "stationary")$p.value)
  )
  # df_multi$box <- df_multi$box.pvalue < 0.05
  df_multi$kpss <- df_multi$kpss.pvalue > 0.05
  df_multi$adf <- df_multi$adf.pvalue < 0.05
  row.names(df_multi) <- c()
  df_multi
}

multi_stat_tests(df_sales[2:20])
## TODO: Make sure if you want log or no log and diff = 1 ??



## convert all the variables to time-series
food_beverage.ts <- ts(df_sales$Food_and_beverage_stores, start = c(2018,1,1), frequency = 12)
grocery.ts <- ts(df_sales$Grocery_stores, start = c(2018,1,1), frequency = 12)
supermarkets.ts <- ts(df_sales$Supermarkets_and_other_grocery_except_convenience_stores, start = c(2018,1,1), frequency = 12)
liquor.ts <- ts(df_sales$Beer_wine_and_liquor_stores, start = c(2018,1,1), frequency = 12)
personalCare.ts <- ts(df_sales$Health_and_personal_care_stores, start = c(2018,1,1), frequency = 12)
pharmacy.ts <- ts(df_sales$Pharmacies_and_drug_stores, start = c(2018,1,1), frequency = 12)
gas.ts <- ts(df_sales$Gasoline_stations, start = c(2018,1,1), frequency = 12)
clothing.ts <- ts(df_sales$Clothing_and_clothing_access_stores, start = c(2018,1,1), frequency = 12)
mensClothing.ts <- ts(df_sales$Mens_clothing_stores, start = c(2018,1,1), frequency = 12)
womensClothing.ts <- ts(df_sales$Womens_clothing_stores, start = c(2018,1,1), frequency = 12)
familyClothing.ts <- ts(df_sales$Family_clothing_stores, start = c(2018,1,1), frequency = 12)
shoe.ts <- ts(df_sales$Shoe_stores, start = c(2018,1,1), frequency = 12)
jewelry.ts <- ts(df_sales$Jewelry_stores, start = c(2018,1,1), frequency = 12)
hobby.ts <- ts(df_sales$Sporting_goods_hobby_musical_instrument_and_book_stores, start = c(2018,1,1), frequency = 12)
books.ts <- ts(df_sales$Book_stores, start = c(2018,1,1), frequency = 12)
general_merchandise.ts <- ts(df_sales$General_merchandise_stores, start = c(2018,1,1), frequency = 12)
department.ts <- ts(df_sales$Department_stores, start = c(2018,1,1), frequency = 12)
nodiscount.ts <- ts(df_sales$Department_stores_excluding_discount_department_stores, start = c(2018,1,1), frequency = 12)
discount.ts <- ts(df_sales$Discount_department_stores, start = c(2018,1,1), frequency = 12)
deaths.ts <- ts(df_sales$Deaths_due_to_covid, start = c(2018,1,1), frequency = 12)
stringency.ts <- ts(df_sales$Monthly_average_stringency_index, start = c(2018,1,1), frequency = 12)








## splitting the dataset
## Split  data to test and train for all retail store types using TSstudio and then use ARIMA to check test accuracy

functionToSplit<-function(x){
  split_dataset<-ts_split(x, sample.out = 4) # this will create test data with last 4 values of the dataset. Rest will be train dat
  
}

## stringency index and death columns as a matrix and using inside xreg
stringency_column <- matrix(df_sales$Monthly_average_stringency_index, ncol = 1)
death_column <- matrix(df_sales$Deaths_due_to_covid, ncol=1)
stringency_death_matrix<- cbind(stringency_column,death_column)
stringency_death_matrix

# stringency train and test
stringency_train<-functionToSplit(stringency.ts)$train
stringency_test<-functionToSplit(stringency.ts)$test


#### setting benchmarking models for all retail store types: snaive forecast as baseline models
mean_food <- meanf(food_beverage.ts, h=12)
#autoplot(mean_food)
drift_food <- rwf(food_beverage.ts, h=12)
#autoplot(drift_food)
plot(mean_food,
     main="benchmark forecast for food_beverages for the next 12 months",col="black")

snaive_food<- snaive(food_beverage.ts, h=12)
#autoplot(snaive_food)

food_naive <- naive(food_beverage.ts,h=12)
#autoplot(food_naive)

lines(food_naive$mean, col="red",lwd =3)
lines(snaive_food$mean, col="blue")
lines(drift_food$mean, col="green")
lines(food_beverage.ts)
legend("topleft", lty = 1, col = c("lightblue","blue","red","green"),
       legend=c("Mean","Seasonal Naive", "Naive","Drift"), ncol=2,cex = 0.60)
## TODO: Double check this legend
## TODO: Add seasonal naive

models <- c()
rmse <- c()
mae <- c()

# TODO: Change to seasonal naive
## From the above, the best benchmarking/ baseline model to select is snaive model.  
## So, For all other retail types, we will apply the snaive model as benchmark.   
## We apply the model in the train set and evaluate its accuracy on the test set
snaive_benchmark_model<- function(x, filename){
  snaive_model<- snaive(x$train,h=4, level = c(95))
  print(summary(snaive_model))
  # path <- file.path("C:","Users","my lenovo", "Documents", "RStudio_Projects", "FMCG_Timeseries_prediction","Naive_benchmark",paste("naive_", filename, ".png", sep = ""))
  # png(file=path)
  plot(snaive_model, main= "Sales Forecast for supermarket", xlab="Months", ylab="Sales")
  # dev.off()
  print(accuracy(snaive_model,x$test))
  models <- c(models, "S-Naive")
  rmse <- c(rmse, accuracy(snaive_model)[2])
  mae <- c(mae, accuracy(snaive_model)[3])
  lines(snaive_model$mean, col = "blue")
  lines(x$test)
  legend("topleft", lty = 1, col = c("blue","green"),
         legend=c("Predicted data","Actual data"), ncol=1,cex = 0.60)
  checkresiduals(snaive_model)
}  

snaive_benchmark_model(functionToSplit(food_beverage.ts), "food_beverage")
snaive_benchmark_model(functionToSplit(grocery.ts), "grocery")
snaive_benchmark_model(functionToSplit(supermarkets.ts), "supermarket")
snaive_benchmark_model(functionToSplit(liquor.ts), "liquor")
snaive_benchmark_model(functionToSplit(personalCare.ts), "personalcare")
snaive_benchmark_model(functionToSplit(pharmacy.ts), "pharmacy")
snaive_benchmark_model(functionToSplit(gas.ts), "gas")
snaive_benchmark_model(functionToSplit(clothing.ts), "clothing")
snaive_benchmark_model(functionToSplit(mensClothing.ts), "mensClothing")
snaive_benchmark_model(functionToSplit(womensClothing.ts), "womensClothing")
snaive_benchmark_model(functionToSplit(familyClothing.ts), "familyClothing")
snaive_benchmark_model(functionToSplit(shoe.ts), "shoe")
snaive_benchmark_model(functionToSplit(jewelry.ts), "jewelry")
snaive_benchmark_model(functionToSplit(hobby.ts), "hobby")
snaive_benchmark_model(functionToSplit(books.ts), "books")
snaive_benchmark_model(functionToSplit(general_merchandise.ts), "generalMerchandise")
snaive_benchmark_model(functionToSplit(department.ts), "department")
snaive_benchmark_model(functionToSplit(nodiscount.ts), "Nodiscount")
snaive_benchmark_model(functionToSplit(discount.ts), "discount")


##Now we apply different time-series models to all the retail types to check for best fit model
##Models: Snaive model, Arima, Linear regression
##Please note that we have already applied Snaive above



# ARIMA model with stringency & death as exogenous variables
# TODO: If we are predicting next month, we will not have stringency death matrix
# TODO: It would be better if we use "what if" analysis. What if stringency is ... and then generate predictions
create_arima_model<- function(x){
  autoarima_fit <-auto.arima(x$train, d=1, xreg= stringency_death_matrix[1:36,1:2])
  fcast_autoarima_fit <- forecast(autoarima_fit,h=4, xreg=stringency_death_matrix[37:40,1:2], level= c(95))
  print(fcast_autoarima_fit) 
  
  print(summary(autoarima_fit))
  cat("Test dataset values: \n", x$test, "\n")
  
  print("Accuracy:")
  print(accuracy(fcast_autoarima_fit, x$test))
  
  models <- c(models, "ARIMA w/(S & D)")
  rmse <- c(rmse, accuracy(fcast_autoarima_fit)[2])
  mae <- c(mae, accuracy(fcast_autoarima_fit)[3])
  
  
  plot(x$test, ylim = c(60000,90000), main="Best fit arima", xlab= "Months", ylab= "Sales" )
  lines(fcast_autoarima_fit$mean, col = "blue")
  lines(x$test,col = "green")
  legend("topleft", lty = 1, col = c("blue","green"),
         legend=c("Best Fit - ARIMA","Actual data"), ncol=1,cex = 0.60)
  
  #checkresiduals(fcast_autoarima_fit)
  ## TODO: Add the actual data in the plot in a green line

}
?plot
create_arima_model(functionToSplit(food_beverage.ts))
create_arima_model(functionToSplit(grocery.ts))
create_arima_model(functionToSplit(supermarkets.ts))
create_arima_model(functionToSplit(liquor.ts))
create_arima_model(functionToSplit(personalCare.ts))
create_arima_model(functionToSplit(pharmacy.ts))
create_arima_model(functionToSplit(gas.ts))
create_arima_model(functionToSplit(clothing.ts))
create_arima_model(functionToSplit(mensClothing.ts))
create_arima_model(functionToSplit(womensClothing.ts))
create_arima_model(functionToSplit(familyClothing.ts))
create_arima_model(functionToSplit(shoe.ts))
create_arima_model(functionToSplit(jewelry.ts))
create_arima_model(functionToSplit(hobby.ts))
create_arima_model(functionToSplit(books.ts))
create_arima_model(functionToSplit(general_merchandise.ts))
create_arima_model(functionToSplit(department.ts))
create_arima_model(functionToSplit(nodiscount.ts))
create_arima_model(functionToSplit(discount.ts))


# ARIMA model with only stringency exogenous variables
# TODO: If we are predicting next month, we will not have stringency death matrix
# TODO: It would be better if we use "what if" analysis. What if stringency is ... and then generate predictions
create_arima_stringency_model<- function(x){
  autoarima_fit <-auto.arima(x$train, d=1, xreg=stringency_train)
  fcast_autoarima_fit <- forecast(autoarima_fit,h=4, xreg=stringency_test, level = c(95))
  fcast_autoarima_fit 
  print("Summary:")
  print(summary(fcast_autoarima_fit))
  cat("Test dataset values: \n", x$test, "\n")
  
  print("Accuracy:")
  print(accuracy(fcast_autoarima_fit, x$test))
  
  plot(fcast_autoarima_fit)
  lines(fcast_autoarima_fit$mean, col = "blue")
  lines(x$test,col = "green")
  legend("topleft", lty = 1, col = c("blue","green"),
         legend=c("Predicted data","Actual data"), ncol=1,cex = 0.60)
  models <- c(models, "ARIMA w/(S)")
  
  
  rmse <- c(rmse, accuracy(fcast_autoarima_fit)[2])
  mae <- c(mae, accuracy(fcast_autoarima_fit)[3])
  checkresiduals(fcast_autoarima_fit)
  ## TODO: Add the actual data in the plot in a green line
  # plot(x$test, ylim = c(60000,90000), main="Best fit arima", xlab= "Months", ylab= "Sales" )
  # lines(fcast_autoarima_fit$mean, col = "blue")
  # lines(x$test,col = "green")
  # legend("topleft", lty = 1, col = c("blue","green"),
  #        legend=c("Best Fit - ARIMA","Actual data"), ncol=1,cex = 0.60)
}

create_arima_stringency_model(functionToSplit(food_beverage.ts))
create_arima_stringency_model(functionToSplit(grocery.ts))
create_arima_stringency_model(functionToSplit(supermarkets.ts))
create_arima_stringency_model(functionToSplit(liquor.ts))
create_arima_stringency_model(functionToSplit(personalCare.ts))
create_arima_stringency_model(functionToSplit(pharmacy.ts))
create_arima_stringency_model(functionToSplit(gas.ts))
create_arima_stringency_model(functionToSplit(clothing.ts))
create_arima_stringency_model(functionToSplit(mensClothing.ts))
create_arima_stringency_model(functionToSplit(womensClothing.ts))
create_arima_stringency_model(functionToSplit(familyClothing.ts))
create_arima_stringency_model(functionToSplit(shoe.ts))
create_arima_stringency_model(functionToSplit(jewelry.ts))
create_arima_stringency_model(functionToSplit(hobby.ts))
create_arima_stringency_model(functionToSplit(books.ts))
create_arima_stringency_model(functionToSplit(general_merchandise.ts))
create_arima_stringency_model(functionToSplit(department.ts))
create_arima_stringency_model(functionToSplit(nodiscount.ts))
create_arima_stringency_model(functionToSplit(discount.ts))



##http://r-statistics.co/Linear-Regression.html: IMPORTANT!!! 
# For linear model comparison, the model with the lowest AIC and BIC score is preferred.

##train test split of stringency and death
stringency_xts<- as.xts(stringency.ts)
trainstringency_xts <- stringency_xts[1:36,]
teststringency_xts <- stringency_xts[37:40,]

deaths_xts<- as.xts(deaths.ts)
traindeaths_xts <- deaths_xts[1:36,]
testdeaths_xts <- deaths_xts[37:40,]


## create a function to build a linear regression model based on lags and no lags
models <- c()
rmse <- c()
mae <- c()

create_lm_model_with_lag<- function(x, folder_name){
  
  Data_ts_to_xts<- x
  trainData_ts_to_xts <- Data_ts_to_xts[1:36]
  testData_ts_to_xts <- Data_ts_to_xts[37:40]
  
  traindata <- matrix(0, nrow = 24, ncol = 5)
  traindata[,1] <- trainData_ts_to_xts[13:36]
  traindata[,2] <- trainstringency_xts[13:36]
  traindata[,3] <- traindeaths_xts[13:36,1]
  traindata[,4] <- trainData_ts_to_xts[12:35]
  traindata[,5] <- trainData_ts_to_xts[2:25]
  
  testdata <- matrix(0, nrow = 4, ncol = 5)
  testdata[,1] <- testData_ts_to_xts[1:4]
  testdata[,2] <- teststringency_xts[1:4,1]
  testdata[,3] <- testdeaths_xts[1:4,1]
  testdata[1,4] <- trainData_ts_to_xts[36]
  testdata[2:4,4] <- testData_ts_to_xts[1:3]
  testdata[,5] <- trainData_ts_to_xts[27:30]
  
  traindata <- as.data.frame(traindata)
  testdata <- as.data.frame(testdata)
  
  colnames(traindata) <- colnames(testdata) <- c("x","stringency","deaths","lag1", "lag2")
  
  
  ##MOdel 1 with stringency only
  lmodel_stringency_only<- lm(x ~ stringency, data = traindata)
  
  
  cat("Print Model Summary with Stringency only", "\n")
  print(summary(lmodel_stringency_only))
  
  cat("Print Accuracy of model with Stringency only", "\n")
  print(accuracy(lmodel_stringency_only))
  
  predictions1 <- predict(lmodel_stringency_only, newdata = testdata, interval = 'predict')
  RMSE <- sqrt(mean((testdata$x - predictions1)^2))
  MAE <- mean(abs(testdata$x - predictions1))
  
  models <- c(models, "LR with Stringency only")
  rmse <- c(rmse, RMSE)
  mae <- c(mae, MAE)
  
  cat("Test RMSE for LR with Stringency only",RMSE,"\n")
  cat("Test MAE for LR with Stringency only",MAE,"\n")
  
  
  
  ##MOdel 2 with stringency and death
  lmodel_retail_type<- lm(x ~ stringency + deaths, data = traindata)

  cat("Print Model Summary with S, D", "\n")
  print(summary(lmodel_retail_type))
  
  cat("Print Accuracy of model S, D", "\n")
  print(accuracy(lmodel_retail_type))

  predictions2 <- predict(lmodel_retail_type, newdata = testdata, interval = 'predict')
  RMSE <- sqrt(mean((testdata$x - predictions2)^2))
  MAE <- mean(abs(testdata$x - predictions2))
  
  models <- c(models, "LR S,D")
  rmse <- c(rmse, RMSE)
  mae <- c(mae, MAE)
  
  cat("Test RMSE for LR with S, D",RMSE,"\n")
  cat("Test MAE for LR with S, D",MAE,"\n")
  
  
  
  ##MOdel 3 with stringency and death and lag1 and lag12
  lmodel_retail_type_lag <- lm(x ~ stringency + deaths + lag1 + lag2, data = traindata)
  
  cat("Print Model Summary with lag, S, D","\n")
  print(summary(lmodel_retail_type_lag))
  
  cat("Print Accuracy of model with lag, S, D","\n")
  print(accuracy(lmodel_retail_type_lag))
  
  predictions_lag <- predict(lmodel_retail_type_lag, newdata = testdata, interval = 'predict')
  
  RMSE <- sqrt(mean((testdata$x - predictions_lag)^2))
  MAE <- mean(abs(testdata$x - predictions_lag))
  
  cat("Test RMSE for LR with lag, S, D",RMSE,"\n")
  cat("Test MAE for LR with lag, S, D",MAE,"\n")
  
  models <- c(models, "LR lag, S, D")
  rmse <- c(rmse, RMSE)
  mae <- c(mae, MAE)
  
  
  
  ##MOdel 4 with stringency and lag1 and lag12
  lmodel_stringency_lag <- lm(x ~ stringency + lag1 + lag2, data = traindata)
  
  cat("Print Model Summary with S & lag","\n")
  print(summary(lmodel_stringency_lag))
  
  cat("Print Accuracy of model with S & lag","\n")
  print(accuracy(lmodel_stringency_lag))
  
  predictions_lag2 <- predict(lmodel_stringency_lag, newdata = testdata, interval = 'predict')
  
  RMSE <- sqrt(mean((testdata$x - predictions_lag2)^2))
  MAE <- mean(abs(testdata$x - predictions_lag2))
  
  cat("Test RMSE for LR with S & lag",RMSE,"\n")
  cat("Test MAE for LR with S & lag",MAE,"\n")
  
  models <- c(models, "LR S & lag")
  rmse <- c(rmse, RMSE)
  mae <- c(mae, MAE)
  
  plot(testdata$x, ylim = c(60000,80000), main="Best fit arima", xlab= "Months", ylab= "Sales" )
  lines(testdata$x,col = "green")
  lines((as.data.frame(predictions1))$fit, col = "black")
  # lines((as.data.frame(predictions2))$fit, col = "orange")
  # lines((as.data.frame(predictions_lag))$fit, col = "blue")
  # lines((as.data.frame(predictions_lag2))$fit, col = "red")
  legend("topleft", lty = 1, col = c("black","green"),
         legend=c("Best Fit - Linear Model","Actual data"), ncol=1,cex = 0.60)
  
  # mypath <- file.path("C:","Users","my lenovo", "Documents", "RStudio_Projects", "FMCG_Timeseries_prediction",folder_name,paste("myplot_", i, ".jpg", sep = ""))
  # jpeg(file=mypath)
  # mytitle = paste("Lag ", i)
  plot(traindata$x)#, ylim = c(60000,80000))
  lines(traindata$x, col = "black")
  lines(testdata$x, col = "black")
  lines(predictions1, col = "green")
  lines(predictions2, col = "orange")
  lines(predictions_lag, col = "blue")
  lines(predictions_lag2, col = "red")
  # dev.off()
  # }
  
  legend("topleft", lty = 1, col = c("black","blue","green"),
         legend=c("Actual","Predicted with lag","Predicted with no lag"), ncol=1,cex = 0.60)
  
  result_matrix <- matrix(c(models, rmse, mae), ncol = 3, nrow = 4)
  colnames(result_matrix) <- c("models","test_rmse","test_mae")
  print(result_matrix) 
  
  ##residuals of all 4 models
  print(bgtest(x ~ stringency, order = 2, data = traindata))
  print(bgtest(x ~ stringency + deaths, order = 3, data = traindata))
  print(bgtest(x ~ stringency + deaths + lag1 + lag2, order = 5, data = traindata))
  print(bgtest(x ~ stringency + lag1 + lag2, order = 4, data = traindata))
  
  ##print predictions
  print(predictions1)
  print(predictions2)
  print(predictions_lag)
  print(predictions_lag2)
  
  

  
  }

create_lm_model_with_lag(food_beverage.ts, "food_lags")
create_lm_model_with_lag(grocery.ts, "grocery_lags")
create_lm_model_with_lag(supermarkets.ts, "supermarkets_lags")
create_lm_model_with_lag(liquor.ts, "liquor_lags")
create_lm_model_with_lag(personalCare.ts, "personalcare_lags")
create_lm_model_with_lag(pharmacy.ts, "pharmacy_lags")
create_lm_model_with_lag(gas.ts,"gas_lags")
create_lm_model_with_lag(clothing.ts, "clothing_lags")
create_lm_model_with_lag(mensClothing.ts, "mensClothing_lags")
create_lm_model_with_lag(womensClothing.ts, "womensClothing_lags")
create_lm_model_with_lag(familyClothing.ts, "familyClothing_lags")
create_lm_model_with_lag(shoe.ts, "shoe_lags")
create_lm_model_with_lag(jewelry.ts, "jewelry_lags")
create_lm_model_with_lag(hobby.ts, "hobby_lags")
create_lm_model_with_lag(books.ts, "books_lags")
create_lm_model_with_lag(general_merchandise.ts, "general_merchandise_lags")
create_lm_model_with_lag(department.ts, "department_lags")
create_lm_model_with_lag(nodiscount.ts, "nodiscount_lags")
create_lm_model_with_lag(discount.ts, "discount_lags")

plot_best_fit<- function(x, folder_name){
  
  # splitting
  Data_ts_to_xts<- x
  trainData_ts_to_xts <- Data_ts_to_xts[1:36]
  testData_ts_to_xts <- Data_ts_to_xts[37:40]
  
  traindata <- matrix(0, nrow = 24, ncol = 5)
  traindata[,1] <- trainData_ts_to_xts[13:36]
  traindata[,2] <- trainstringency_xts[13:36]
  traindata[,3] <- traindeaths_xts[13:36,1]
  traindata[,4] <- trainData_ts_to_xts[12:35]
  traindata[,5] <- trainData_ts_to_xts[2:25]
  
  testdata <- matrix(0, nrow = 4, ncol = 5)
  testdata[,1] <- testData_ts_to_xts[1:4]
  testdata[,2] <- teststringency_xts[1:4,1]
  testdata[,3] <- testdeaths_xts[1:4,1]
  testdata[1,4] <- trainData_ts_to_xts[36]
  testdata[2:4,4] <- testData_ts_to_xts[1:3]
  testdata[,5] <- trainData_ts_to_xts[27:30]
  
  traindata <- as.data.frame(traindata)
  testdata <- as.data.frame(testdata)
  
  colnames(traindata) <- colnames(testdata) <- c("x","stringency","deaths","lag1", "lag2")
  
  #snaive
  
  snaive_model<- snaive(traindata$x,h=4, level = c(95))
  
  plot(snaive_model, main= "Sales Forecast for supermarket", ylim=c(60000,80000),xlab="Months", ylab="Sales")
  
  lines(snaive_model$mean, col = "blue")
  
  
  # #Arima
  # autoarima_fit <-auto.arima(traindata$x, d=1, xreg= stringency_death_matrix[1:24,1:2])
  # fcast_autoarima_fit <- forecast(autoarima_fit,h=4, xreg=stringency_death_matrix[37:40,1:2], level= c(95))
  # 
  # lines(fcast_autoarima_fit$mean, col = "red")
 
  #Linear Regression

  lmodel_stringency_lag <- lm(x ~ stringency + lag1 + lag2, data = traindata)
  
  predictions_lag2 <- predict(lmodel_stringency_lag, newdata = testdata, interval = 'predict')
  
  lines((as.data.frame(predictions1))$fit, col = "black")
  
  #test data
 # lines((functionToSplit(x))$test,col = "green")
  lines(testdata$x, col="green")
  
  #legends
  legend("topleft", lty = 1, col = c("blue","green", "black"),
         legend=c("Snaive","Actual data","Best fit - Linear model"), ncol=1,cex = 0.60)
  
  
}
plot_best_fit(food_beverage.ts)

