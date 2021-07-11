##transposed the original dataset to arrange months as rows and different retail stores as columns
##Combined the data of 4 years from 2018-2020 present in different sheets to a single worksheet and converted it to .csv

setwd("C:\\Users\\my lenovo\\Desktop\\MSc Operations and Supply Chain Management\\Modules\\Dissertation\\datasets\\US Sales and Inventory Datasets")

df_sales <- read.csv("US_Sales_Final_Dataset.csv")
df_sales

library(ggplot2)
library(gridExtra)
library(scales)

##checking for any missing values
sum(is.na(df_sales))

##checking the structure of dataset
str(df_sales)

##since all data in the data-frame are characters, we convert all to integer in excel
library(dplyr)


##change date column from character to date format
library(lubridate)

df_sales$Months <- as.Date(df_sales$Months, "%Y-%m-%d")
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
  geom_line() + ggtitle("Grocery Stores") +scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_Grocery

p_Supermarkets <- ggplot(df_sales, aes(x = Months, y = Supermarkets_and_other_grocery_except_convenience_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Supermarkets, other grocery except convenience stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
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
  geom_line() +ggtitle("Health and personal care stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_personalCare

p_pharmacy <- ggplot(df_sales, aes(x = Months, y = Pharmacies_and_drug_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Pharmacies and drug stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
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
  geom_line() +ggtitle("Clothing and clothing access stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_clothing


p_mensClothing <- ggplot(df_sales, aes(x = Months, y = Mens_clothing_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Men's clothing stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
p_mensClothing

p_WomensClothing <- ggplot(df_sales, aes(x = Months, y = Womens_clothing_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Women's clothing stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
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

grid.arrange(p_generalMerchandise, p_department, p_nodiscountstore, p_discountstore, nrow=4, ncol=1)


## Find correlation of death n stringency with all retail store types to check how much retail stores get affected by stringency or the number of deaths.
## Create a correlation matrix
cor(df_sales[2:20],df_sales[22:23], method = "pearson")



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
                         box.pvalue=sapply(df_sales, function(v) Box.test(ts(v),lag=20,type="Ljung-Box")$p.value),
                         kpss.pvalue=sapply(df_sales, function(v) kpss.test(ts(v))$p.value),
                         adf.pvalue=sapply(df_sales, function(v) adf.test(ts(diff(log(v))),alternative = "stationary")$p.value)
  )
  df_multi$box <- df_multi$box.pvalue < 0.05
  df_multi$kpss <- df_multi$kpss.pvalue > 0.05
  df_multi$adf <- df_multi$adf.pvalue < 0.05
  row.names(df_multi) <- c()
  df_multi
}

multi_stat_tests(df_sales[2:20])




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


#### setting benchmarking models for all retail store types: naive forecast as baseline models

library(fpp)
mean_food <- meanf(food_beverage.ts, h=12)
autoplot(mean_food)
drift_food <- rwf(food_beverage.ts, h=12)
autoplot(drift_food)
plot(mean_food, 
     main="benchmark forecast for food_beverages for the next 12 months")
lines(naive_food$mean, col=2)
lines(drift_food$mean, col=3)
lines(food_beverage.ts)
legend("topright", lty = 1, col = c(4,2,3),
       legend=c("Mean","Seasonal Naive", "Naive"))

## From the above, the best benchmarking/ baseline model to select is naive model.  
## So, For all other retail types, we will apply the naive model as benchmark.    

naive_benchmark_model<- function(x, filename){
  naive_model<- naive(x,h=12)
  summary(naive_model)
  path <- file.path("C:","Users","my lenovo", "Documents", "RStudio_Projects", "FMCG_Timeseries_prediction","Naive_benchmark",paste("naive_", filename, ".png", sep = ""))
  png(file=path)
  plot(naive_model, main = filename)
  dev.off()
  
}    

naive_benchmark_model(discount.ts, "discount")
naive_benchmark_model(nodiscount.ts, "Nodiscount")
naive_benchmark_model(general_merchandise.ts, "generalMerchandise")
naive_benchmark_model(department.ts, "department")
naive_benchmark_model(food_beverage.ts, "food_beverage")
naive_benchmark_model(grocery.ts, "grocery")
naive_benchmark_model(supermarkets.ts, "supermarket")
naive_benchmark_model(liquor.ts, "liquor")
naive_benchmark_model(personalCare.ts, "personalcare")
naive_benchmark_model(pharmacy.ts, "pharmacy")
naive_benchmark_model(gas.ts, "gas")
naive_benchmark_model(clothing.ts, "clothing")
naive_benchmark_model(mensClothing.ts, "mensClothing")
naive_benchmark_model(womensClothing.ts, "womensClothing")
naive_benchmark_model(familyClothing.ts, "familyClothing")
naive_benchmark_model(hobby.ts, "hobby")
naive_benchmark_model(books.ts, "books")
naive_benchmark_model(shoe.ts, "shoe")
naive_benchmark_model(jewelry.ts, "jewelry")


##Now we apply different time-series models to all the retail types to check for best fit model
##Models: Snaive model, Arima, Linear regression

##Seasonal naive model for forecasts across all retail-store types    
naive_food<- snaive(food_beverage.ts, h=12)
naive_food
summary(naive_food)
autoplot(naive_food)


naive_grocery <- snaive(grocery.ts, h=12)
naive_grocery
summary(naive_grocery)
autoplot(naive_grocery)

naive_supermarket <- snaive(supermarkets.ts, h=12)
naive_supermarket
summary(naive_supermarket)
autoplot(naive_supermarket)

naive_liquor <- snaive(liquor.ts, h=12)
naive_liquor
summary(naive_liquor)
autoplot(naive_liquor)

naive_personalCare <- snaive(personalCare.ts, h=12)
naive_personalCare
summary(naive_personalCare)
autoplot(naive_personalCare)

naive_pharmacy <- snaive(pharmacy.ts, h=12)
naive_pharmacy
summary(naive_pharmacy)
autoplot(naive_pharmacy)

naive_gas <- snaive(gas.ts, h=12)
naive_gas
summary(naive_gas)
autoplot(naive_gas)

naive_gas <- snaive(gas.ts, h=12)
naive_gas
summary(naive_gas)
autoplot(naive_gas)

naive_clothing <- snaive(clothing.ts, h=12)
naive_clothing
summary(naive_clothing)
autoplot(naive_clothing)

naive_clothing <- snaive(clothing.ts, h=12)
naive_clothing
summary(naive_clothing)
autoplot(naive_clothing)

naive_mensClothing <- snaive(mensClothing.ts, h=12)
naive_mensClothing
summary(naive_mensClothing)
autoplot(naive_mensClothing)

naive_womensClothing <- snaive(womensClothing.ts, h=12)
naive_womensClothing
summary(naive_womensClothing)
autoplot(naive_womensClothing)

naive_familyClothing <- snaive(familyClothing.ts, h=12)
naive_familyClothing
summary(naive_familyClothing)
autoplot(naive_familyClothing)

naive_familyClothing <- snaive(familyClothing.ts, h=12)
naive_familyClothing
summary(naive_familyClothing)
autoplot(naive_familyClothing)

naive_shoe <- snaive(shoe.ts, h=12)
naive_shoe
summary(naive_shoe)
autoplot(naive_shoe)

naive_jewelry <- snaive(jewelry.ts, h=12)
naive_jewelry
summary(naive_jewelry)
autoplot(naive_jewelry)

naive_hobby <- snaive(hobby.ts, h=12)
naive_hobby
summary(naive_hobby)
autoplot(naive_hobby)

naive_books <- snaive(books.ts, h=12)
naive_books
summary(naive_books)
autoplot(naive_books)

naive_generalMerchandise <- snaive(general_merchandise.ts, h=12)
naive_generalMerchandise
summary(naive_generalMerchandise)
autoplot(naive_generalMerchandise)

naive_department <- snaive(department.ts, h=12)
naive_department
summary(naive_department)
autoplot(naive_department)

naive_nodiscount <- snaive(nodiscount.ts, h=12)
naive_nodiscount
summary(naive_nodiscount)
autoplot(naive_nodiscount)

naive_discount <- snaive(discount.ts, h=12)
naive_discount
summary(naive_discount)
autoplot(naive_discount)


##Function to autosave graphs of snaive
snaive_model<- function(x, filename){
  mypath <- file.path("C:","Users","my lenovo", "Documents", "RStudio_Projects", "FMCG_Timeseries_prediction","SeasonalNaive_Forecasts",paste("snaiveplot_", filename, ".jpg", sep = ""))
  jpeg(file=mypath)
  plot(x, main = filename)
  dev.off()
}

snaive_model(naive_discount, "discount")
snaive_model(naive_nodiscount, "Nodiscount")
snaive_model(naive_generalMerchandise, "generalMerchandise")
snaive_model(naive_department, "department")
snaive_model(naive_food, "food_beverage")
snaive_model(naive_grocery, "grocery")
snaive_model(naive_supermarket, "supermarket")
snaive_model(naive_liquor, "liquor")
snaive_model(naive_personalCare, "personalcare")
snaive_model(naive_pharmacy, "pharmacy")
snaive_model(naive_gas, "gas")
snaive_model(naive_clothing, "clothing")
snaive_model(naive_mensClothing, "mensClothing")
snaive_model(naive_womensClothing, "womensClothing")
snaive_model(naive_familyClothing, "familyClothing")
snaive_model(naive_hobby, "hobby")
snaive_model(naive_books, "books")
snaive_model(naive_shoe, "shoe")
snaive_model(naive_jewelry, "jewelry")



library(vars)
library(mFilter)
library(tseries)
library(forecast)



## stringency index and death columns as a matrix and using inside xreg
stringency_column <- matrix(df_sales$Monthly_average_stringency_index, ncol = 1)
death_column <- matrix(df_sales$Deaths_due_to_covid, ncol=1)
stringency_death_matrix<- cbind(stringency_column,death_column)
stringency_death_matrix

## Split  data to test and train for all retail store types using TSstudio and then use ARIMA to check test accuracy


library(TSstudio)
functionToSplit<-function(x){
  split_dataset<-ts_split(x, sample.out = 4) # this will create test data with last 4 values of the dataset. Rest will be train dat
  
}
print(functionToSplit(grocery.ts))
print(functionToSplit(grocery.ts)$train)
print(functionToSplit(grocery.ts)$test)

stringency_train<-functionToSplit(stringency.ts)$train
stringency_test<-functionToSplit(stringency.ts)$test



create_naive_model<- function(x){
  naive_fit <- snaive(x$train, h=4)
  naive_fit
  summary(naive_fit)
  accuracy(naive_fit,x$test)
  #autoplot(naive_fit)
}

create_naive_model(functionToSplit(food_beverage.ts))

create_arima_model<- function(x){
  autoarima_fit <-auto.arima(x$train, d=1, xreg= stringency_death_matrix[1:36,1:2])
  fcast_autoarima_fit <- forecast(autoarima_fit,h=4, xreg=stringency_death_matrix[37:40,1:2])
  fcast_autoarima_fit 
  print("Summary:")
  summary(fcast_autoarima_fit)
  cat("Test dataset values: \n", x$test, "\n")
  
  print("Accuracy:")
  accuracy(fcast_autoarima_fit, x$test)
  #autoplot(fcast_autoarima_discount)
}

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



##http://r-statistics.co/Linear-Regression.html: IMPORTANT!!! 
# For linear model comparison, the model with the lowest AIC and BIC score is preferred.

##perform linear regression for all store types taking the whole dataset. no splitting between test and train


library(xts)

##train test
stringency_xts<- as.xts(stringency.ts)
deaths_xts<- as.xts(deaths.ts)


## create a function to build linear regression model based on lags

create_lm_model_with_lag<- function(x, folder_name){
  
  trainData_ts_to_xts<- as.xts(x)
  
  lmodel_retail_type<- lm(x~stringency_xts)
  
  cat("Print Model Summary without lag", "\n")
  print(summary(lmodel_retail_type))
  
  cat("Print Accuracy of model without lag", "\n")
  print(accuracy(lmodel_retail_type))
  
  predictions <- predict(lmodel_retail_type)
  models <- c()
  rmse <- c()
  for(i in -12:12){
    
    stringency_lag <- stats::lag(stringency_xts,i)
    lmodel_retail_type_lag<- lm(x~stringency_lag)
    cat("Print Model Summary with lag = ", i,  "\n")
    print(summary(lmodel_retail_type_lag))
    cat("Print Accuracy of model with lag = ", i, "\n")
    print(accuracy(lmodel_retail_type_lag))
    
    predictions_lag <- predict(lmodel_retail_type_lag)
    
    models <- c(models, i)
    rmse <- c(rmse, accuracy(lmodel_retail_type_lag)[2])
    
    mypath <- file.path("C:","Users","my lenovo", "Documents", "RStudio_Projects", "FMCG_Timeseries_prediction",folder_name,paste("myplot_", i, ".jpg", sep = ""))
    jpeg(file=mypath)
    mytitle = paste("Lag ", i)
    plot(cbind(food,predictions,predictions_lag), main = mytitle)
    dev.off()
  }
  
  result_matrix <- matrix(c(models, rmse), ncol = 2, nrow = 25)
  colnames(result_matrix) <- c("models","rmse")
  print(result_matrix) 
  
}

create_lm_model_with_lag(food_beverage.ts, "food_lags")
create_lm_model_with_lag(grocery.ts, "grocery_lags")
create_lm_model_with_lag(supermarkets.ts, "supermarkets_lags")
create_lm_model_with_lag(liquor.ts, "liquor_lags")
create_lm_model_with_lag(personalCare.ts, "pcare_lags")
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





