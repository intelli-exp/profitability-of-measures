#features: Time(t), Day of week, etc
library(glmnet)
library(Rlab)
library(Hmisc)
library(zoo)


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
