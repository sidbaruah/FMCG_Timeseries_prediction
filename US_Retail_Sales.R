##transposed the original dataset to arrange months as rows and different retail stores as columns
##Combined the data of 4 years from 2018-2020 present in different sheets to a single worksheet and converted it to .csv

setwd("C:\\Users\\my lenovo\\Desktop\\MSc Operations and Supply Chain Management\\Modules\\Dissertation\\datasets\\US Sales and Inventory Datasets")

df_sales <- read.csv("US_Sales_Final_Dataset.csv")
df_sales

library(ggplot2)
library(gridExtra)

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

##plotting the different columns against Months column
p_food_beverage <- ggplot(df_sales, aes(x = Months, y = Food_and_beverage_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() + ggtitle("Food and Beverage Stores")

print(p_food_beverage)

p_Grocery <- ggplot(df_sales, aes(x = Months, y = Grocery_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() + ggtitle("Grocery Stores")
p_Grocery

p_Supermarkets <- ggplot(df_sales, aes(x = Months, y = Supermarkets_and_other_grocery_except_convenience_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Supermarkets, other grocery except convenience stores")
p_Supermarkets

grid.arrange(p_food_beverage,p_Grocery,p_Supermarkets, nrow=3, ncol=1)

p_liquor <- ggplot(df_sales, aes(x = Months, y = Beer_wine_and_liquor_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Beer, wine and liquor stores")
p_liquor

p_personalCare <- ggplot(df_sales, aes(x = Months, y = Health_and_personal_care_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Health and personal care stores")
p_personalCare

p_pharmacy <- ggplot(df_sales, aes(x = Months, y = Pharmacies_and_drug_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Pharmacies and drug stores")
p_pharmacy  

grid.arrange(p_personalCare,p_pharmacy, nrow=2, ncol=1)


p_gas <- ggplot(df_sales, aes(x = Months, y = Gasoline_stations)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Gasoline stations")
p_gas  

p_clothing <- ggplot(df_sales, aes(x = Months, y = Clothing_and_clothing_access_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Clothing and clothing access stores")
p_clothing


p_mensClothing <- ggplot(df_sales, aes(x = Months, y = Mens_clothing_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Men's clothing stores")
p_mensClothing

p_WomensClothing <- ggplot(df_sales, aes(x = Months, y = Womens_clothing_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Women's clothing stores")
p_WomensClothing


p_familyClothing <- ggplot(df_sales, aes(x = Months, y = Family_clothing_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Family clothing stores")
p_familyClothing

grid.arrange(p_clothing,p_mensClothing, p_WomensClothing, p_familyClothing, nrow=4, ncol=1)


p_shoes <- ggplot(df_sales, aes(x = Months, y = Shoe_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Shoe_stores")
p_shoes


p_jewellery <- ggplot(df_sales, aes(x = Months, y = Jewelry_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Shoe_stores")
p_jewellery


p_hobby <- ggplot(df_sales, aes(x = Months, y = Sporting_goods_hobby_musical_instrument_and_book_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Sporting goods. hobby, musical instrument and book-stores")
p_hobby

p_books <- ggplot(df_sales, aes(x = Months, y = Book_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Only Book-stores")
p_books


p_generalMerchandise <- ggplot(df_sales, aes(x = Months, y = General_merchandise_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("General merchandise stores")
p_generalMerchandise


p_department <-ggplot(df_sales, aes(x = Months, y = Department_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Department stores")
p_department


p_nodiscountstore <-ggplot(df_sales, aes(x = Months, y = Department_stores_excluding_discount_department_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Department stores excluding discount department stores")
p_nodiscountstore


p_discountstore <-ggplot(df_sales, aes(x = Months, y = Discount_department_stores)) +
  ylab("Sales") +
  xlab("Periods") +
  geom_line() +ggtitle("Discount department stores")
p_discountstore

grid.arrange(p_generalMerchandise, p_department, p_nodiscountstore, p_discountstore, nrow=4, ncol=1)



