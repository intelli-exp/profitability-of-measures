require(tibble)
require(dplyr)
require(magrittr)
library(Hmisc)
library(lubridate)

read_stock <- function(path="data/Stocks/a.us.txt",from_date=as.Date("1800/1/1"),to_date=as.Date("5000/11/10"),features=NULL,...){
  
  stock_data <- read.csv(path)
  stock_data <- as.tbl(stock_data)
  stock_data$Date <- as.Date(stock_data$Date)
  if(from_date)
  stock_data %<>% filter(between(Date,from_date,to_date))
  if(!is.null(features)){
    stock_data  <- stock_data[features]
  }
  return(stock_data)
}

read_stock()
