t <- seq(0,12*pi,length.out=100)
y <- sin(0.5*t)
plot(y)
y1 <- y
y2 <- y + rnorm(length(y),0,0.2) #ifelse(y==max(y),1, ifelse(y< (min(y)+0.005),-1,0))
y3 <- ifelse(y>0,1,-1)
y4 <- sin(0.5*t + pi/2)

library(fpp2)
#y2<-ma(y2,2)

len <- length(t)
df <- data.frame(t=rep(t,4),y=rep(y,4),yhat=c(y1,y2,y3,y4),forecast=factor(c(rep(1,len),rep(2,len),rep(3,len),rep(4,len))))

ggplot(data=df) + geom_point(aes(x=t,y=y)) +geom_line(aes(x=t,y=yhat,color=forecast)) + facet_wrap(~forecast)
library(Hmisc)


sign  <- function(x){
  ifelse(x>0,1,-1)
}

cor_sign <- function(x,y){
  cor(sign(x),sign(y))
}

mae <- function(y,yh){
  return( mean(abs(y-yh))) 
}

mae(y,y1)
mae(y,y2)
mae(y,y3)
mae(y,y4)

cor_sign(y,y1) %>% round(2)
cor_sign(y,y2) %>% round(2)
cor_sign(y,y3) %>% round(2)
cor_sign(y,y4) %>% round(2)


library(dtw)
dtwDist(matrix(y,nrow=1),matrix(y1,nrow=1)) %>% round(2)
dtwDist(matrix(y,nrow=1),matrix(y2,nrow=1)) %>% round(2)
dtwDist(matrix(y,nrow=1),matrix(y3,nrow=1)) %>% round(2)
dtwDist(matrix(y,nrow=1),matrix(y4,nrow=1)) %>% round(2)
  
