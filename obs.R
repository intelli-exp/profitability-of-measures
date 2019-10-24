library(dplyr)
library(zoo)

obs <- function(capital,actual,x){
  cummin <- rollapply(x,FUN=min,partial=T,align="right",width=length(x))
  df <- data.frame(t=1:length(x),x=x,cummin=cummin)
  df$dist_cummin <- df$x - df$cummin
  pmax <- max(which(df$dist_cummin == max(df$dist_cummin)))
  pmin <- min(which(df$cummin == df[pmax,]$cummin))
  df$opt <- FALSE
  df[c(pmax,pmin),]$opt <- T
  
  df$spec <- FALSE
  df$spec[c(pmin,pmax)] <- TRUE
 #  plt1<-ggplot(df) + geom_point(aes(x=t,y=x)) + geom_point(data= df %>% filter(spec),aes(x=t,y=x),color="red")

   df$actual <- actual
  # plt2<-ggplot(df) + geom_point(aes(x=t,y=actual)) + geom_point(data= df %>% filter(spec),aes(x=t,y=actual),color="red")
   
  # grid.arrange(plt1,plt2,ncol=1) %>% print
  
  own <- 0
  buy_price <- actual[pmin]
  sell_price <-actual[pmax]
  
  #buy
  own <- floor(capital/buy_price)
  capital <- capital - own*buy_price
  #sell
  capital <- capital + own*sell_price
  own <- 0
  return(capital)
}

library(Hmisc)
mobs <- function(capital,actual,x){
  df <- data.frame(t=1:length(x),x=x,x_prev = Lag(x,1),x_next=Lag(x,-1))
  df$is_max = x> df$x_prev & x> df$x_next
  df$is_min = x<df$x_prev & x<df$x_next
  df[1,]$is_min <- df[1,]$x < df[2,]$x
  lngth <- nrow(df)
  df[lngth,]$is_max <- df[lngth,]$x > df[lngth-1,]$x
  df$point <- df$is_max | df$is_min
  
  
  # ggplot(df) + geom_point(aes(x=t,y=x)) +
  #   geom_point(data= df %>% filter(is_min),aes(x=t,y=x,color="min"))+
  # geom_point(data= df %>% filter(is_max),aes(x=t,y=x,color="max"))
  df$actual <- actual
  df_pts <- df%>% filter(point)
  own <- 0
  for( i in 1:nrow(df_pts)){
    act <- df_pts[i,]
    if(act$is_min & floor(capital/act$actual) >0){
      buy <-  floor(capital/act$actual)
      own <- own+ buy
      capital <- capital - own*act$actual
      #print(paste("buy",buy,"at",act$x))
      buy=0
    }else if(act$is_max & own >0){
      sell <- own*act$actual
      #print(paste("sell",sell,"at",act$x))
      own <- 0
      capital <- capital + sell
      sell <- 0
    }
    #print(paste("capital:",capital))
  }
  return(capital)
  
}



