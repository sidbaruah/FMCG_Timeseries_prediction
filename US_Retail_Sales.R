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
  geom_line() +ggtitle("Shoe_stores")+scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 month"))
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




