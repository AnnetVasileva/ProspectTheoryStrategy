# Load Packages
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(zoo)
library(xts)

# Function: Transforming returns

Value_function <- function(i) {
  if(i >= 0) {
    j = i^0.88
  } else {
    j = -1.5*(-i)^0.88
  }
  return(j)
}

# Function: Transforming probabilities

w_pos <- function(z) {
  l = z^0.61/(z^0.61 + (1-z)^0.61)^(1/0.61)
  return(l)
}

w_neg <- function(k) {
  h = k^0.69/(k^0.69 + (1-k)^0.69)^(1/0.69)
  return(h)
}

# set working directory

setwd("~/rdata/lajdinovic/Personal")

# Read the csv table

df = read_excel("OEX_const_prices.xlsx", sheet = 3)
df2 = read_excel("sp100ConMatrix.xlsx")

# Take away the N/A columns

df = df[ , colSums(is.na(df)) < nrow(df)]

# Create a vector with the names of the stocks

names = rbind(colnames(df[1,]))
names_vector = rbind(names[seq(1, ncol(names), 2)])

# Create a matrix with the prices, and add the stock tickers as the names

price = (df[seq(2, ncol(df), 2)])
price_matrix = rbind(price)
colnames(price_matrix) = names_vector

# Create a vector with the dates

d = cbind(df[1])
colnames(d) = "Date"
dates = transform(d, Date = as.numeric(Date))

# Add the column to our price_matrix and delete the first row (Px_last)

matrix = cbind(dates, price_matrix)
matrix_final = matrix[-1,]
matrix_final$Date <- as.Date(matrix_final$Date, origin = "1899-12-30")
matrix_final[2:ncol(matrix_final)] <- sapply(matrix_final[2:ncol(matrix_final)],as.numeric)


# Use zoo package 
date <- matrix_final$Date
matrix_final_wthDate <- matrix_final[2:189]
data <- xts(matrix_final_wthDate, order.by = as.Date(date, "%m/%d/%Y"))
data <- na.locf(data)
data2 <- diff(data, differences = 1, log = TRUE)
data2 <- data2[-1]
data2[is.na(data2)] <- 0
data3 <- data2

for (i in 1 : nrow(data2)) {
        for (j in 1 : ncol(data2)) {
          data3[i,j] <- Value_function(data2[i,j])
        }
  return(data3)
}

#Convert data from xts to data frame

date2 <- date[-1]
ym2 <- as.yearmon(date2)
data4 <- cbind(date2, as.data.frame(data3))
rownames(data4) <- NULL
data5 <- gather(data4, "Company", "Subj.Return", 2:ncol(data4))
data6 <- mutate(data5, ym = as.yearmon(data5$date2))

# sort the returns from least to highest in a month for each security


data7 <- data6 %>% group_by(Company, ym) %>% arrange(Subj.Return, .by_group = TRUE)
data8 <- filter(data7, Subj.Return != 0)
count <- data8 %>% group_by(ym, Company) %>% tally()
data9 <- inner_join(data8, count, by = c("Company", "ym"))
data10 <- mutate(data9, prob = 1/n)
data11 <- mutate(data10, Sign = ifelse(Subj.Return >= 0, "Pos", "Neg"))
#Here try to arrange it conditional on "pos" or "neg". Ascending for neg and descending for pos.We will then merge it with 99.
data12 <- data11 %>% group_by(Company, ym, Sign) %>% arrange(case_when(Sign == "Neg" ~ Subj.Return, Sign == "Pos" ~ -Subj.Return), .by_group = T)

data13 <- data12 %>% group_by(Sign, ym, Company) %>% mutate(CumSum = cumsum(prob))
data14 <- data13 %>% ungroup()
data15 <- mutate(data14, Cum.Subj.Prob = ifelse(Sign == 'Pos', w_pos(CumSum), w_neg(CumSum)))
#Here you need to add substraction to create subj.prob from sum.subj.prob
data16 <- data15 %>% group_by(Sign, ym, Company) %>% mutate(Lag.Prob = lag(Cum.Subj.Prob, default = 0)) %>% ungroup()
data17 <- data16 %>% mutate(Subj.Prob = Cum.Subj.Prob - Lag.Prob)

data18 <- mutate(data17, TK = Subj.Return*Subj.Prob)
data19 <- data18 %>% group_by(Company, ym) %>% summarise(TK = sum(TK))
data19$Company <- str_remove(data19$Company, " Equity")
                

#GET DATA FOR COMPANIES THAT WERE IN S&P INDEX AT THAT POINT

# Convert dates to propriate format for df2 and replace all NA by 0
df2$Dates <- as_date(df2$Dates)
df2$Dates <- as.yearmon(df2$Dates, "%m/%Y")
df2 <- rename(df2, ym = Dates)
df2[is.na(df2)] <- 0

# Edit composition data
df2_2 <- gather(df2, "Company", "Marker", 2:ncol(df2))
df2_3 <- filter(df2_2, Marker == 1)

#Join data that is needed
data <- inner_join(data19, df2_3, by = c("ym", "Company"))
data_2 <- data %>% arrange(ym,TK)