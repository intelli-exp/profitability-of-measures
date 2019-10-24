#set.seed(40); 

source("obs.R")
source("random_walks_gaussian.R")
source("processing_data/apply.R")



filter_tickers <- function(tickers,ff=function(x){x},paths=FALSE,...){
  companies <- tickers
  l<- lapply(companies,function(x){
    tryCatch({
      data <- x %>% read_stock(.,...)
      cond <- data %>% ff(.,...)
      if(cond & paths)
        return(x)
      else if(cond & !paths)
        return(data)
      else
        return(NA)
    },error=function(e){print("Warning: Ignoring file with zero rows.")
      return(NA)})
  })
  
  
  if(paths)
    return(l[!is.na(l)]%>% unlist)
  else
    return(l %T>% {names(.)<- tickers} %>% .[!is.na(.)])
}

tickers <-   list.files("data/Stocks/") %>% paste0("data/Stocks/",.)# %>% sample(1000)

# proper_tickers <- filter_tickers(tickers,ff = function(x){
#   total_days <- x %>% filter(between(Date,as.Date("2015-01-01"),as.Date("2015-08-31"))) %>% nrow()
#   test_days <-  x %>% filter(between(Date,as.Date("2015-08-01"),as.Date("2015-08-31"))) %>% nrow()
#   return(test_days ==21 & total_days>100)
# },paths=TRUE)

proper_tickers <-  readRDS("~/Desktop/Projects/stock-market-forecasting/proper_tickers.rds")
proper_tickers  %>% unique %>% length
set.seed(111);
selected_tickers <- sample(proper_tickers,100,replace = FALSE)
prediction_results = list()
metrics_results = list()
errors = list()

proper_tickers <- readRDS("~/Desktop/Projects/stock-market-forecasting/proper_tickers.rds")
pcd_times <- c()
best_times<-c()
best_walks_lst <- list()

pcd_all <- list()
pc_all <- list()
value_all <- list()

counter<<-1
for(ticker in selected_tickers){
  
      print(paste0(counter,"/",length(selected_tickers)))
      counter <<- counter+1
      tryCatch({
        
    
      #features: Time(t), Day of week, etc
      library(glmnet)
      library(Rlab)
      library(Hmisc)
      library(zoo)
      #Ticker Selection
      source("processing_data/input.R")
      x <- read_stock(ticker,from_date = as.Date("2015-01-01"),to_date=as.Date("2015-08-31"))

      x$in_sample <- FALSE
      take=which( x$Date < as.Date("2015-08-01")) %>% max()
      x$in_sample <- FALSE
      x[1:take,]$in_sample <- TRUE
      
     
      boom=FALSE
      tryCatch({
        walks<-continue_signal(x,21,100,samp=TRUE)
      }
        ,
        error = function(e){boom=TRUE}
      )
      if(boom==TRUE){
        next
      }
      
      #Costs
      library(Hmisc)
      actual <- x %>% filter(!in_sample) %>% pull(Close)
      library(ggplot2)
      t1 <- Sys.time()
      capital<-1000
      
      
      all_walks_gain <- apply(walks,1,function(x){
        mobs(capital,actual,x)
      }) %>% {./capital}
      best_times %<>% c(.,Sys.time()-t1)
      
      #new
      value_all[[length(value_all)+1]] <- all_walks_gain
      
      
      best_walks_lst[[length(best_walks_lst)+1]] <- all_walks_gain
      
      
      best_walk <- walks[which(max(all_walks_gain) ==all_walks_gain)[1] ,]
      
      
      best_walk_gain <-  mobs(capital,actual,best_walk ) /capital
      

      #####
    
      
      MAE <- apply(walks,1,function(x){mean(abs(x-actual))})
      min_MAE <- which(MAE == min(MAE))
      x$min_mae_walk =  c( rep(NA,nrow(x) - dim(walks)[2]) , walks[min_MAE,])
      
      MAPE <- apply(walks,1,function(x){mean(abs((x-actual)/x))})
      min_MAPE <- which(MAPE == min(MAPE))
      x$min_mape_walk =  c( rep(NA,nrow(x) - dim(walks)[2]) , walks[min_MAPE,])
      
      MAAPE <- apply(walks,1,function(x){mean(abs(atan((x-actual)/x)))})
      min_MAAPE <- which(MAAPE == min(MAAPE))
      x$min_maape_walk =  c( rep(NA,nrow(x) - dim(walks)[2]) , walks[min_MAAPE,])
      ###
      
      
      PC <- apply(walks,1,function(x){cor(x,actual)})
      max_PC <- which(PC == max(PC))
      x$max_pc_walk =  c( rep(NA,nrow(x) - dim(walks)[2]) , walks[max_PC,])
      
      #new
      pc_all[[length(pc_all)+1]] <- PC
      
      t1 <- Sys.time()
      PCD <- apply(walks,1,function(x){cor(diff(x),diff(actual))})
      pcd_times %<>% c(.,Sys.time()-t1)
      
      #new
      pcd_all[[length(pcd_all)+1]] <- PCD
      
      
      max_PCD <- which(PCD == max(PCD))
      x$max_pcd_walk =  c( rep(NA,nrow(x) - dim(walks)[2]) , walks[max_PCD,])
      
      PCD2 <- apply(walks,1,function(x){cor(diff(x,differences = 2),diff(actual,differences = 2))})
      max_PCD2 <- which(PCD2 == max(PCD2))
      x$max_pcd2_walk =  c( rep(NA,nrow(x) - dim(walks)[2]) , walks[max_PCD2,])
      
      
   
      
      
   
      mae_gain <- mobs(capital,actual,na.omit(x$min_mae_walk) ) /capital
      mape_gain <- mobs(capital,actual,na.omit(x$min_mape_walk) ) /capital
      maape_gain <- mobs(capital,actual,na.omit(x$min_mape_walk) ) /capital
      
      pc_gain <- mobs(capital,actual,na.omit(x$max_pc_walk) ) /capital
      pcd_gain <- mobs(capital,actual,na.omit(x$max_pcd_walk) ) /capital
      pcd2_gain <- mobs(capital,actual,na.omit(x$max_pcd2_walk) ) /capital
      
      avg_gain <- mobs(capital,actual,walks %>% apply(2,mean))
      perfect_gain <- mobs(capital,actual,actual)/capital
      
      de=data.frame(pcd=max(PCD),pc=max(PC),mae=min(MAE),mape=min(MAPE),maape=min(MAAPE),pcd2=max(PCD2))
      row.names(de)=ticker
      errors[[length(errors)+1]] <- de
      
      df = data.frame(perfect=perfect_gain,best_walk=best_walk_gain,pcd=pcd_gain,pc=pc_gain,mae=mae_gain,mape=mape_gain,maape=maape_gain)
      row.names(df) = ticker
      metrics_results[[length(metrics_results)+1]] <- df
      x$average_walk = c( rep(NA,nrow(x) - dim(walks)[2]) , walks %>% apply(2,mean))
      x$min_walk = c( rep(NA,nrow(x) - dim(walks)[2]) , walks %>% apply(2,min))
      x$max_walk = c( rep(NA,nrow(x) - dim(walks)[2]) , walks %>% apply(2,max))
      
      x$sd_walk = c( rep(NA,nrow(x) - dim(walks)[2]) , walks %>% apply(2,sd))
      
      # ggplot(x) + geom_line(aes(x=Date,y=Close))   +
      #   geom_line(aes(x=Date,y=average_walk,color="Average Path"))+
      #   geom_ribbon(aes(x=Date,ymin=min_walk,ymax=max_walk),alpha=0.2,fill="blue")
      # 
      },error=function(e){print(e)})
}

names(best_walks_lst) <- selected_tickers %>% gsub(pattern="data/Stocks/",replacement = "") %>% gsub(pattern=".us.txt",replacement="")


#NEW
pcds<- pcd_all %>% unlist()
vals <- value_all %>% unlist()


pcd_plots <- list()
pc_plots <- list() 

stats <- c("sigma","r.squared","adj.r.squared","fstatistic","cov.unscaled")

pc_lm_results = data.frame(ticker=character(0),rmse=numeric(0),r.squared=numeric(0),adj.r.squared=numeric(0))
pcd_lm_results= data.frame(ticker=character(0),rmse=numeric(0),r.squared=numeric(0),adj.r.squared=numeric(0))

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
grid.arrange(pcd_plots[[1]],pcd_plots[[2]],pcd_plots[[3]],pcd_plots[[4]],pcd_plots[[5]],
             pc_plots[[1]],pc_plots[[2]],pc_plots[[3]],pc_plots[[4]],pc_plots[[5]],
             nrow=2)

    ggplot(mapping=aes(x=pcds,y=vals)) +geom_point()+ geom_smooth(method='lm',formula=y~x)

##

data.frame(best_walks_lst) %>% lapply(function(x){sum(x>=1)}) %>% unlist -> wins

replicate(1000, {
wins %>% {./100} %>% lapply(function(x){
  rbern(n = 1,prob=x)
}) %>% unlist() %>% sum()
}) -> trials
summary(trials)

theoretic <- rnorm( n=1000,mean(trials),sd(trials))

ggplot() + geom_density(aes(x=trials,color="Results for 1000"))+ geom_density(aes(x=theoretic,color="Generated with Normal Distribution"),linetype="dashed")  + xlab("Profitable Stocks with Random Selection")
ggplot() + geom_density(aes(x=wins)) + xlab("Profitable Walks") + ylab("Probability Density")


replicate({ best_walks_lst %>% lapply(function(x){100*sample(x,1)}) %>% unlist %>% sum},n=1000) -> caps

ggplot() + geom_density(aes(x=caps))
caps %>% summary
##

res<-metrics_results %>% bind_rows %>%  select(-one_of("perfect","best_walk","pcd2"))
row.names(res) <- metrics_results %>% lapply(row.names)


#Extract Names
row.names(res) %>% gsub(pattern="data/Stocks/",replacement = "") %>% gsub(pattern=".us.txt",replacement="") -> ticks
text <-  ticks %>% toupper() %>% sort() %>% paste0(collapse=", ")
text

#Victories

 lapply(1:nrow(res),function(i){
  ind <- res[i,] == max(res[i,])
  print(ind)
    names(res[i,])[ind]
}) %>% unlist -> victories
victories %>% table

#Time analysis
#############
  ggplot() +  geom_density(aes(x=pcd_times))+ geom_density(aes(x=best_times))  -> plt
  pcd_times %>% summary
  best_times %>% summary
  (best_times/pcd_times) %>% summary
  
  pcd_times %>% sum %>% round(3) %>%  {.*1000}  %>% paste0(" ms")
  best_times %>% sum
  
  
  #PCD
  data.frame( PCD= summary(pcd_times)%>%as.numeric,Actual_Gain=summary(best_times) %>% as.numeric())  %>% round(3) %>% {.*1000} %>%
    lapply(function(x){paste0(x," ms")}) %>% as.data.frame %T>%
    {rownames(.) <- paste0(names(summary(pcd_times))," Time")}

  res<-metrics_results %>% bind_rows
  rownames(res) <- metrics_results %>% lapply(rownames)
  rownames(res) %<>% gsub(pattern = "data/Stocks/",replacement = "") %>%gsub(pattern = ".us.txt",replacement = "")
  res$ticker <- rownames(res) #factor(rownames(res),levels = c("perfect","best_walk","pcd","pc","mae","mape","maape","nbmae"),ordered = TRUE)
  res %<>% select(-one_of("perfect","pcd2"))
  
  
##PCD vs PC
# 
# res$pcd[ errs$pcd > 0.5] %>% {.>=1} %>% table
# res$pcd[ errs$pcd < 0.5] %>% {.>=1} %>% table
# 
# df <- data.frame(th=numeric(0),pr=numeric(0),rec=numeric(0))
# #for(th in seq(0.4,0.9,length.out=100)){
#   tn_fn <- res$pcd[ errs$pcd < 0.39] %>% {.>=1} %>% table
#   fp_tp <- res$pcd[ errs$pcd >= 0.39] %>% {.>=1} %>% table
#   (precision <- fp_tp[length(fp_tp)] / sum(fp_tp))
#   (recall <- fp_tp[2] / sum(fp_tp[2] + tn_fn[2]))
#   df %<>% rbind(data.frame(th=th,pr=precision,rec=recall))
# #}
# 
# 
# 
# errs <- errors %>% bind_rows
# cor(errs$pcd,res$pcd)
# 
# 
# ymax <- max(c(res$pcd,res$pc))
# ymin <- min(c(res$pcd,res$pc))
# xmin <- min(c(errs$pcd,errs$pc))
# xmax <- max(c(errs$pcd,errs$pc))
# 
# plt1<-ggplot() + geom_point(aes(x=errs$pcd,res$pcd)) +
#   geom_point(aes(x=errs$pcd[selection_2],res$pcd[selection_2]),color="red") + xlab("PCD") + ylab("Gain") +
#   geom_hline(yintercept = (1),linetype="dotted") + xlim(xmin,xmax) + ylim(ymin,ymax)
# 
# selection <- errs$pc > 0.5 & res$pc < 1 #19, 54, 35
# selection_2 <- errs$pcd >0.5 & res$pcd <1 #2, 52, 7
# 
# plt2<-ggplot() + geom_point(aes(x=errs$pc,res$pc)) +
#   geom_point(aes(x=errs$pc[selection],res$pc[selection]),color="red")+xlab("PC") +
#   ylab("Gain")  + geom_hline(yintercept = (1),linetype="dotted") + xlim(xmin,xmax) + ylim(ymin,ymax)
# 
# library(gridExtra)
# grid.arrange(plt2,plt1,nrow=1)



####Finance example gain


library(reshape2)
df<-res %>% melt("ticker")%T>% {colnames(.)<-c("ticker","metric","gain")}

df %>% group_by(ticker) %>% arrange(gain) %>% mutate(ord = n():1) %>% ungroup() ->df


#Success Rate
df %>%  mutate(cap = 100*gain) %>% group_by(metric) %>% summarise(capital=sum(cap))  %>% arrange(desc(capital))
df %>%   mutate(pos = gain >= 1) %>% group_by(metric) %>% summarise(success = sum(pos)/n()) %>% arrange(desc(success))


library(plotly)
 df$ticker %>% unique   %>% .[1:7] -> selected_tickers
 plt <- ggplot(df %>% filter(ticker %in% selected_tickers)) +
   geom_col(aes(x=ticker,y=gain,fill=metric,group=ord),position="dodge") +
   geom_hline(yintercept = (1),linetype="dotted") + ylab("Financial Gain") + xlab("Ticker")
    
#for interactivity
#plt %>% ggplotly()

 ggplot(df %>% filter(metric %in% c("best_walk","pcd")))+ geom_density(aes(x=gain,color=metric))


 res<-metrics_results %>% bind_rows %>%  select(-one_of("perfect","best_walk"))

 row.names(res) <- metrics_results %>% lapply(row.names)

 lapply(1:nrow(res),function(i){
   ind <- res[i,] == max(res[i,])
   print(ind)
   names(res[i,])[ind]
 }) %>% unlist -> victories
 victories %>% table

 res %>% summary

