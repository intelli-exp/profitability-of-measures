#features: Time(t), Day of week, etc
library(glmnet)
library(Rlab)
library(Hmisc)
library(zoo)
#Ticker Selection
# source("processing_data/input.R")
# ticker <- sample( list.files("data/Stocks"),1)
# if(nrow(x)<20){
#   next}
# ticker = paste0("data/Stocks/",ticker)
# x <- read_stock(ticker,from_date = as.Date("2015-01-01"),to_date=as.Date("2015-08-31"))
# 
# n<-1
# xc <- x$Close
# x$Close_diff <- c(rep(NA,n),diff(x$Close,n))
# m <- mean(x$Close_diff,na.rm=T)
# s <- 3*sd(x$Close_diff,na.rm=T)/4
# x$rn <- rnorm(mean = m,sd=s,n=nrow(x))

continue_signal <- function (x,steps,num,samp=FALSE){
  if(samp){
    x %<>% filter(in_sample)
  }
  len <- length(x$Close)
  n=1
  x$Close_diff <- c(rep(NA,n),diff(x$Close,n))
  
  
  m <- mean(x$Close_diff,na.rm=T)
  s <- sd(x$Close_diff,na.rm=T)
  #x$rn <- rnorm(mean = m,sd=s,n=nrow(x))
 

  plt <- ggplot()
  vals <- c()
  paths <- c()
  for(i in 1:num){
    move <-  rnorm(mean = m,sd=s,n=steps)
    last<-x$Close %>% .[length(.)]
    
    paths %<>% c(.,move+last)
    signal <- c(x$Close,last+ cumsum(move))
    vals %<>% c(signal %>% .[length(.)])
    df <- data.frame(index = 1:(len+steps),
                     in_sample = c( rep(TRUE,len),rep(FALSE,steps)),
                     y=signal )
    plt<- plt + geom_line(data=df,aes(x=index, y=y,color=in_sample))
    
    #print(plt)
  }
   #ggplot() +   geom_density(aes(x=vals))
   mat <- matrix(paths,nrow=num)
  return(mat)
}
# 
# paths <- continue_signal(x,22,100)
# 
# 
# 
# rollapply(x$Close,FUN=function(x){
#   
# },partial=TRUE,align="right",width=nrow(x))
# 
# 
# library(ggplot2)
#  ggplot(x) + geom_density(aes(x=Close))
#  ggplot(x) + geom_density(aes(x=Close_diff,color="actual")) + geom_density(aes(x=rn,color="std"))
# ggplot(x) + geom_line(aes(x=1:nrow(x),y=Close_diff,color="red")) + geom_line(aes(x=1:nrow(x),y=rn))
# ggplot(x) + geom_line(aes(x=1:nrow(x),y=rn))

# replicate(
# 
# {
# low = sample(1:(length(tt)-1),1)
# high = sample((low+1):length(tt),1)
# cor(tt[low:high],actual[low:high])
# }
# ,n = 10)


