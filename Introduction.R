library(dplyr)
x <- (15+cos(seq(0,(4*pi),length.out = 100) )) %>%
  c(., seq(16,11,length.out=100),10+cos(seq(0,(4*pi),length.out = 100) ) )

y <- (15+cos(pi+seq(0,(4*pi),length.out = 100) )) %>%
  c(., seq(14,9,length.out=100),10+cos(pi+seq(0,(4*pi),length.out = 100) ) )

xd <- diff(x,1)
yd <- diff(y,1)

plot(x)
library(ggplot2)
plt1 <- ggplot() + geom_linerange(aes(x=t,ymax=max(x),ymin=min(x)),
                                  color=if_else(togeth,"green","red"))+
  geom_line(aes(x=1:length(x),y=x)) +
  geom_hline(yintercept = mean(x),linetype="dotted") + ylab("Actual") + ggtitle("cor(actual,pred)=0.79")
  

plt2 <- ggplot() + 
  geom_linerange(aes(x=t,ymax=max(y),ymin=min(y)),
                 color=if_else(togeth,"green","red"))+
  geom_line(aes(x=1:length(y),y=y)) + 
  geom_hline(yintercept = mean(y),linetype="dotted") +  ylab("Prediction") 

plt3 <- ggplot() + 
  geom_linerange(aes(x=td,ymax=max(xd),ymin=min(xd)),
                 color=if_else(togeth_d,"green","red")) +
  geom_line(aes(x=1:length(xd),y=xd)) + 
  geom_hline(yintercept = mean(xd),linetype="dotted") + ylab("D(Actual)") +
   ggtitle("cor(D(actual),D(pred))=-0.80") + xlab("t")

plt4 <- ggplot() +  
  geom_linerange(aes(x=td,ymax=max(yd),ymin=min(yd)),
                 color=if_else(togeth_d,"green","red")) +
  geom_line(aes(x=1:length(yd),y=yd)) + 
  geom_hline(yintercept = mean(yd),linetype="dotted") + ylab("D(Prediction)") + xlab("t")

library(gridExtra)
grid.arrange(plt1,plt3,plt2,plt4,nrow=2,ncol=2)

t <- 1:length(x)
td <- 1:length(xd)
togeth <- (x > mean(x)) & (y > mean(y)) | (x < mean(x)) & (y < mean(y)) 
togeth_d <-  (xd > mean(xd)) & (yd > mean(yd)) | (xd < mean(xd)) & (yd < mean(yd)) 

geom_rect(t[togeth],y=)

cor(x,y)
cor(xd,yd)
