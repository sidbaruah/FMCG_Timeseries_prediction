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

##plotting the different columns against Months column using ggplot
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



##Trying for ARIMA model for food_beverage

library(forecast)
fit_food_beverage <- arima(df_sales$Food_and_beverage_stores, order = c(3,1,1))
fcast_food_beverage <- forecast(fit_food_beverage, h = 12)
fcast_food_beverage
autoplot(fcast_food_beverage)
summary(fcast_food_beverage)
##ARIMAX model for food_beverage with stringency index and death as exogenous variables
#fit_food_beverage_stringency <- auto.arima(df_sales$Food_and_beverage_stores, xreg = df_sales$Monthly_average_stringency_index)

fit_stringency <- arima(df_sales$Monthly_average_stringency_index, order = c(3,1,1))
fcast_stringency <- as.data.frame(forecast(fit_stringency, h=12)) #Vector containing next 12 values of monthly average stringency index
fcast_stringency
str(fcast_stringency)

fit_death<- arima(df_sales$Deaths_due_to_covid, order = c(3,1,1))
fcast_death <- as.data.frame(forecast(fit_death, h=12))
fcast_death
str(fcast_death)



# fcast_food_beverage_stringency <- forecast(fit_food_beverage_stringency, h=12, xreg = fcast_stringencyNext12)
# autoplot(fcast_food_beverage_stringency)
# 
# deathsNext12 <- forecast(df_sales$Deaths_due_to_covid, h=12)  #Vector containing next 12 values of deaths due to covid
# 
# ##ARIMA model for food_beverage taking stringency index and death columns as a matrix and using inside xreg
stringency_column <- matrix(df_sales$Monthly_average_stringency_index, ncol = 1)
death_column <- matrix(df_sales$Deaths_due_to_covid, ncol=1)
stringency_death_matrix<- cbind(stringency_column,death_column)
stringency_death_matrix
fit_food_beverage_stringency_death<- Arima(df_sales$Food_and_beverage_stores, order = c(3,1,1),xreg = stringency_death_matrix[,1:2])
fit_food_beverage_stringency_death


expected_stringency_death_matrix <- matrix(c(52.65205,52.32921,52.67195,52.99716,53.06499,53.05524,53.03324,53.02577,53.02516,53.02645,53.02711,53.02725,33912.09,47477.72,52481.98,49976.30,46019.15,44369.01,44967.76,46114.13,46648.36,46514.05,46184.40,46014.05), ncol = 2, nrow = 12)
expected_stringency_death_matrix
fcast_food_beverage_stringency_death<- forecast(fit_food_beverage_stringency_death, xreg = expected_stringency_death_matrix, h=12)
# summary(fit_food_beverage_stringency_death)
# coeftest(fit_food_beverage_stringency_death)


plot(fcast_food_beverage_stringency_death)

#n<- nrow(df_sales)
#train_indices <- 1:round(0.85 * n) #get train indices
train <- df_sales[1:36,] #split test data based on row numbers
train
#test_indices <- (round(0.6 * n) + 1):n #get test indices
test <- df_sales[37:40,] #split test data based on row numbers
test

# make arima model based on train data and validate with test data
fit_train<- Arima(train$Food_and_beverage_stores, order = c(3,12,1), xreg = stringency_death_matrix[1:36,1:2])
fcast_test <- forecast(fit_train, xreg = stringency_death_matrix[37:40,1:2], h = 4)
fcast_test
autoplot(fcast_test)
summary(fcast_test)


## ADF test to check for stationarity
adf.test(df_sales$Food_and_beverage_stores)
## from the ADF test, the p-value = 0.4593 > 0.05 means that we fail to reject the null hypothesis. That  means the data is NOT stationary.


##now to convert the varuable into stationary, we can try to convert it to log / 1st difference / 2nd difference
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


#Checking first difference for rest of the columns and doing ARIMA


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



##try ADF, KPSS and BOX test across all variables and print output in a data-frame
names(df_sales[2:20])
multi_stat_tests<- function(df_sales){
  p <- ncol(df_sales)
  df_multi <- data.frame(var = names(df_sales),
                         # box.pvalue=sapply(df_sales, function(v) Box.test(ts(v),lag=20,type="Ljung-Box")$p.value),
                         # kpss.pvalue=sapply(df_sales, function(v) kpss.test(ts(v))$p.value),
                         adf.pvalue=sapply(df_sales, function(v) adf.test(ts(diff(log(v))),alternative = "stationary")$p.value)
  )
  # df_multi$box <- df_multi$box.pvalue < 0.05
  # df_multi$kpss <- df_multi$kpss.pvalue > 0.05
  df_multi$adf <- df_multi$adf.pvalue < 0.05
  row.names(df_multi) <- c()
  df_multi
}

multi_stat_tests(df_sales[2:20])
####



