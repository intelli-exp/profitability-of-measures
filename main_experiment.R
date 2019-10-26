source("processing_data/input.R")
source("processing_data/apply.R")
source("utilities/value_computation.R")
source("utilities/random_walks_gaussian.R")
library(glmnet)
library(Rlab)
library(Hmisc)
library(zoo)
library(ggplot2)

### 1. EXPERIMENT PREPARATION


#Ticker Selection
tickers <-   list.files("data/Stocks/") %>% paste0("data/Stocks/",.)# %>% sample(1000)

#this step has already been precomputed and saved as proper_tickers.rds
#The purpose is to remove tickers that dont have enough data in the considered period
# proper_tickers <- filter_tickers(tickers,ff = function(x){
#   total_days <- x %>% filter(between(Date,as.Date("2015-01-01"),as.Date("2015-08-31"))) %>% nrow()
#   test_days <-  x %>% filter(between(Date,as.Date("2015-08-01"),as.Date("2015-08-31"))) %>% nrow()
#   return(test_days ==21 & total_days>100)
# },paths=TRUE)
proper_tickers <-  readRDS("proper_tickers.rds")

#Set random seed for reproducibility and select 100 random stocks
set.seed(111);
selected_tickers <- sample(proper_tickers,100,replace = FALSE)


#Vectors and lists to be filled with results
prediction_results = list()
metrics_results = list()
errors = list()
pcd_times <- c()
best_times<-c()
best_walks_lst <- list()
pcd_all <- list()
pc_all <- list()
value_all <- list()

counter<<-1 #stock counter

### 2. EXECUTION OF THE EXPERIMENT


#The main experiment repeated for N randomly selected stocks (N=100)
for(ticker in selected_tickers){
  
      print(paste0("Running main experiment...",counter,"/",length(selected_tickers)))
      counter <<- counter+1
      tryCatch({
        
    

      x <- read_stock(ticker,from_date = as.Date("2015-01-01"),to_date=as.Date("2015-08-31"))

      x$in_sample <- FALSE
      take=which( x$Date < as.Date("2015-08-01")) %>% max()
      x$in_sample <- FALSE
      x[1:take,]$in_sample <- TRUE
      
     
      crash=FALSE
      tryCatch({
        walks<-continue_signal(x,21,100,samp=TRUE)
      }
        ,
        error = function(e){crash=TRUE}
      )
      if(crash==TRUE){
        next
      }
      
      #Costs
      actual <- x %>% filter(!in_sample) %>% pull(Close)
      t1 <- Sys.time()
      capital<-1000
      
      
      ##Estimate Actual Values
      
      all_walks_gain <- apply(walks,1,function(x){
        mobs(capital,actual,x)
      }) %>% {./capital}
      best_times %<>% c(.,Sys.time()-t1)
      
      
      value_all[[length(value_all)+1]] <- all_walks_gain
      best_walks_lst[[length(best_walks_lst)+1]] <- all_walks_gain
      best_walk <- walks[which(max(all_walks_gain) ==all_walks_gain)[1] ,]
      best_walk_gain <-  mobs(capital,actual,best_walk ) /capital
      

      ##### Estimate Performance measures
    
      
      MAE <- apply(walks,1,function(x){mean(abs(x-actual))})
      min_MAE <- which(MAE == min(MAE))
      x$min_mae_walk =  c( rep(NA,nrow(x) - dim(walks)[2]) , walks[min_MAE,])
      
      MAPE <- apply(walks,1,function(x){mean(abs((x-actual)/x))})
      min_MAPE <- which(MAPE == min(MAPE))
      x$min_mape_walk =  c( rep(NA,nrow(x) - dim(walks)[2]) , walks[min_MAPE,])
      
      MAAPE <- apply(walks,1,function(x){mean(abs(atan((x-actual)/x)))})
      min_MAAPE <- which(MAAPE == min(MAAPE))
      x$min_maape_walk =  c( rep(NA,nrow(x) - dim(walks)[2]) , walks[min_MAAPE,])

      PC <- apply(walks,1,function(x){cor(x,actual)})
      max_PC <- which(PC == max(PC))
      x$max_pc_walk =  c( rep(NA,nrow(x) - dim(walks)[2]) , walks[max_PC,])
      
      pc_all[[length(pc_all)+1]] <- PC
      
      t1 <- Sys.time()
      PCD <- apply(walks,1,function(x){cor(diff(x),diff(actual))})
      pcd_times %<>% c(.,Sys.time()-t1)
      
      pcd_all[[length(pcd_all)+1]] <- PCD
      
      max_PCD <- which(PCD == max(PCD))
      x$max_pcd_walk =  c( rep(NA,nrow(x) - dim(walks)[2]) , walks[max_PCD,])
      
      ##Compute Profitability of Performance Measures
      
      mae_gain <- mobs(capital,actual,na.omit(x$min_mae_walk) ) /capital
      mape_gain <- mobs(capital,actual,na.omit(x$min_mape_walk) ) /capital
      maape_gain <- mobs(capital,actual,na.omit(x$min_mape_walk) ) /capital
      
      pc_gain <- mobs(capital,actual,na.omit(x$max_pc_walk) ) /capital
      pcd_gain <- mobs(capital,actual,na.omit(x$max_pcd_walk) ) /capital

      avg_gain <- mobs(capital,actual,walks %>% apply(2,mean))
      perfect_gain <- mobs(capital,actual,actual)/capital
      
      de=data.frame(pcd=max(PCD),pc=max(PC),mae=min(MAE),mape=min(MAPE),maape=min(MAAPE))
      row.names(de)=ticker
      errors[[length(errors)+1]] <- de
      
      df = data.frame(perfect=perfect_gain,best_walk=best_walk_gain,pcd=pcd_gain,pc=pc_gain,mae=mae_gain,mape=mape_gain,maape=maape_gain)
      row.names(df) = ticker
      metrics_results[[length(metrics_results)+1]] <- df
 
      },error=function(e){print(e)})
}

### 3.EXPERIMENTAL RESULTS


names(best_walks_lst) <- selected_tickers %>% gsub(pattern="data/Stocks/",replacement = "") %>% gsub(pattern=".us.txt",replacement="")

#Compute Victories per metric
res<-metrics_results %>% bind_rows %>%  select(-one_of("perfect","best_walk"))
row.names(res) <- metrics_results %>% lapply(row.names)
lapply(1:nrow(res),function(i){
  ind <- res[i,] == max(res[i,])
  names(res[i,])[ind]
}) %>% unlist -> victories

#Section 4.1 Table 1
victories %>% table


#Estimate performance of random selection method
#For profitability and capital at the end of random trading session
num_profitable <- value_all %>% lapply(function(x){sum(x>=1)}) %>% unlist

replicate(1000, {
  num_profitable %>% {./100} %>% lapply(function(x){
    rbern(n = 1,prob=x)
  }) %>% unlist() %>% sum()
}) -> random_trials
random_results <- summary(random_trials)

replicate(1000, {
  value_all %>%  lapply(function(x){
    sample(x,1)*100
  }) %>% unlist  %>% sum()
}) -> random_capital

random_capital_stats <- summary(random_capital)

#Section 3.3 Figure 3
ggplot() + geom_density(aes(x=num_profitable)) + xlab("Profitable Walks") + ylab("Probability Density")


library(reshape2)
res<-metrics_results %>% bind_rows %>%  select(-one_of("perfect"))
res$ticker <- row.names(res)
df<-res %>% melt("ticker")%T>% {colnames(.)<-c("ticker","metric","gain")}
df %>% group_by(ticker) %>% arrange(gain) %>% mutate(ord = n():1) %>% ungroup() ->df





#Section 4.1 Table 2
df  %>%  mutate(pos = gain >= 1) %>% group_by(metric) %>%
  summarise(success = sum(pos)/n()) %>%  rbind(data.frame(metric="random",success=random_results[["Mean"]]/100)) %>%
  arrange(desc(success))

#Section 4.1 Table 3
df %>%  mutate(cap = 100*gain) %>% group_by(metric) %>%
  summarise(capital=sum(cap))  %>% rbind(data.frame(metric="random",capital=random_capital_stats[["Mean"]])) %>%
  arrange(desc(capital))


#Section 4.1 Figure 4
df$ticker %>% unique   %>% .[1:7] -> sample_tickers
plt <- ggplot(df %>% filter(ticker %in% sample_tickers)) +
  geom_col(aes(x=ticker,y=gain,fill=metric,group=ord),position="dodge") +
  geom_hline(yintercept = (1),linetype="dotted") + ylab("Financial Gain") + xlab("Ticker")
print(plt)

#for interactive plot run
# library(plotly)
# plt %>% ggplotly()


  
  
#Execution Time Analysis
#############
ggplot() +  geom_density(aes(x=pcd_times))+ geom_density(aes(x=best_times))  -> plt
pcd_times %>% summary
best_times %>% summary
pcd_times %>% sum %>% round(3) %>%  {.*1000}  %>% paste0(" ms")
best_times %>% sum %>% paste0("s")


#Section 4.2 Table 3
(best_times/pcd_times) %>% summary


data.frame( PCD= summary(pcd_times)%>%as.numeric,Actual_Gain=summary(best_times) %>% as.numeric())  %>% round(3) %>% {.*1000} %>%
  lapply(function(x){paste0(x," ms")}) %>% as.data.frame %T>%
  {rownames(.) <- paste0(names(summary(pcd_times))," Time")}


  


#Regression Analysis

pcds<- pcd_all %>% unlist()
vals <- value_all %>% unlist()


pcd_plots <- list()
pc_plots <- list() 

stats <- c("sigma","r.squared","adj.r.squared","fstatistic","cov.unscaled")

pc_lm_results = data.frame(ticker=character(0),rmse=numeric(0),r.squared=numeric(0),adj.r.squared=numeric(0))
pcd_lm_results= data.frame(ticker=character(0),rmse=numeric(0),r.squared=numeric(0),adj.r.squared=numeric(0))

#run linear regression for five random stocks

for(i in 1:5){
  
  df <- data.frame(pc =pc_all[[i]],pcd=pcd_all[[i]],value=value_all[[i]])
  pcd_lm <- lm(value ~ pcd,df)
  pc_lm <- lm(value~ pc,df)
  
  
  df$pc_pred <- predict(pc_lm,df)
  df$pcd_pred <- predict(pcd_lm,df)
  
  
  pcd_lm_results %<>% rbind( data.frame(ticker=names(best_walks_lst)[i] ,rmse=sqrt(mean((df$pcd_pred - df$value)**2)), r.squared = summary(pcd_lm)$r.squared, adj.r.squared= summary(pcd_lm)$adj.r.squared ))
  pc_lm_results %<>% rbind(data.frame(ticker=names(best_walks_lst)[i] ,rmse=sqrt(mean((df$pc_pred - df$value)**2)),r.squared = summary(pc_lm)$r.squared, adj.r.squared= summary(pc_lm)$adj.r.squared ))
  
  

  
  rmse_pc_value <- sqrt(mean((df$pc_pred - df$value)**2))
  rmse_pcd_value <- sqrt(mean((df$pcd_pred - df$value)**2))
  
  
  plt<- local({
    
    i<-i
    print(i)
    ggplot() + geom_point(aes(x=pcd_all[[i]],y=value_all[[i]])) +
    geom_smooth(method='lm',formula=y~x,aes(x=pcd_all[[i]],y=value_all[[i]])) + xlab("PCD") + ylab("Actual Value") +
    ggtitle(names(best_walks_lst)[i]) #+ geom_point(aes(x=df$pcd,y=df$pcd_pred,color="red"))
  })
  pcd_plots[[i]] <- plt
  
  
  pc_plots[[i]]<- local({
    i<-i
    ggplot() + geom_point(aes(x=pc_all[[i]],y=value_all[[i]])) +
      geom_smooth(method='lm',formula=y~x,aes(x=pc_all[[i]],y=value_all[[i]])) + xlab("PC") + ylab("Actual Value") +
      ggtitle(names(best_walks_lst)[i]) 
  }) 
}

library(gridExtra)

#Section 4.3 Figure 5
grid.arrange(pcd_plots[[1]],pcd_plots[[2]],pcd_plots[[3]],pcd_plots[[4]],pcd_plots[[5]],
             pc_plots[[1]],pc_plots[[2]],pc_plots[[3]],pc_plots[[4]],pc_plots[[5]],
             nrow=2)

#Section 4.3 Table 5 for PC and PCD
pc_lm_results
pcd_lm_results


##Stocks in Appendix
selected_tickers %>% gsub(pattern="data/Stocks/",replacement = "") %>% gsub(pattern=".us.txt",replacement="") -> ticks
text <-  ticks %>% toupper() %>% sort() %>% paste0(collapse=", ")
print(text)

