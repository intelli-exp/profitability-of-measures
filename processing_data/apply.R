source("processing_data/input.R")
library(zoo)
library(Hmisc)


#MAP


map_tickers  <- function(tickers,ff=function(x,window,...){x},...){
  
  companies <- paste0("data/Stocks/", tickers,".us.txt")
  lapply(companies,function(x){
    
    x %>% read_stock(.,...) %>% ff(.,...)  }
  )%T>% {names(.) <- tickers}

}

map_data <- function(data,ff=function(x){x},...){
  lapply(data,function(x){
    x %>%  ff(.,...)
  })  
}

#FILTER

filter_tickers <- function(tickers,ff=function(x){x},paths=FALSE,...){
  companies <- paste0("data/Stocks/", tickers,".us.txt")
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
  },error=function(e){print(e)
    return(NA)})
    })
    
    
  if(paths)
    return(l[!is.na(l)]%>% unlist)
  else
    return(l %T>% {names(.)<- tickers} %>% .[!is.na(.)])
}


filter_data <- function(data,ff=function(x){x},...){
  l<- lapply(data,function(x){
    
    cond <- x %>% ff(.,...)
    if(cond )
      return(x)
    else
      return(NA)
  })
  return(l[!is.na(l)])
}

#MAP WINDOW


map_tickers_roll <- function(tickers,ff=function(x,...){x},width=10,connector=function(x){x},...){
  companies <- paste0("data/Stocks/", tickers,".us.txt")
  lapply(companies,function(x){
    x %<>% read_stock(.,...)
    indices <- 1:nrow(x)
    roll_indices <- rollapply(indices,width=width,function(x){x}) %>% t() %>% as.data.frame %>% as.list()
    #result <- roll_indices %>% lapply(FUN = function(i,ff=ff){ff(x[i,],...)},...) %>% connector
    result <- list()
    for( i in roll_indices){
      result[[length(result)+1]] <- ff(x[i,],...)
    }
    result %<>% connector
    return(result)
  }) %T>% {names(.) <- tickers}
}

map_data_roll <-  function(data,ff=function(x,...){x},width=10,connector=function(x){x},...){
  
  lapply(data,function(x){
    indices <- 1:nrow(x)
    roll_indices <- rollapply(indices,width=width,function(x){x}) %>% t() %>% as.data.frame %>% as.list()
    #result <- roll_indices %>% lapply(FUN = function(i,ff=ff){ff(x[i,],...)},...) %>% connector
    result <- list()
    for( i in roll_indices){
      result[[length(result)+1]] <- ff(x[i,],...)
    }
    result %<>% connector
    return(result)
  }) 
}


map_tickers_roll_month <- function(tickers,ff=function(x,...){x},months=3,connector=function(x){x},...){
  
  
  companies <- paste0("data/Stocks/", tickers,".us.txt")
  lapply(companies,function(x){
    x %<>% read_stock(.,...)
    x$Month <- format( as.POSIXct( x$Date ),"%m")
    x$Month_lag_1 <- Lag( x$Month,1);x$Month_lag_1[1] <- x$Month[1]
    x$new_month <- x$Month != x$Month_lag_1
    x$month_id <- cumsum(x$new_month)
    #window_apply <- function(month_ids,ff,...){
    #  return(ff(x %>% filter(month_id %in% month_ids),...))
    #}
    
    roll_months <- rollapply(x$month_id %>% unique,width=months,function(x){x}) %>% t() %>% as.data.frame %>% as.list()
    result <- list()
    for( months_window in roll_months){
      result[[length(result)+1]] <- ff(x %>% filter(month_id %in% months_window),...)
    }
    result %<>% connector
    return(result)
  }) %T>% {names(.) <- tickers}
}

map_data_roll_month <- function(data,ff=function(x,...){x},months=3,connector=function(x){x},...){
  
  lapply(data,function(x){
    x$Month <- format( as.POSIXct( x$Date ),"%m")
    x$Month_lag_1 <- Lag( x$Month,1);x$Month_lag_1[1] <- x$Month[1]
    x$new_month <- x$Month != x$Month_lag_1
    x$month_id <- cumsum(x$new_month)
    #window_apply <- function(month_ids,ff,...){
    #  return(ff(x %>% filter(month_id %in% month_ids),...))
    #}
    
    roll_months <- rollapply(x$month_id %>% unique,width=months,function(x){x}) %>% t() %>% as.data.frame %>% as.list()
    result <- list()
    for( months_window in roll_months){
      result[[length(result)+1]] <- ff(x %>% filter(month_id %in% months_window),...)
    }
    result %<>% connector
    return(result)
  }) 
}



map_tickers_slice <- function(tickers,ff=function(x,...){x},...,width,connector){
  
  
  companies <- paste0("data/Stocks/", tickers,".us.txt")
  lapply(companies,function(x){
    x %<>% read_stock(.,...) 
    x$index <- 1:nrow(x)
    x$slice <- ceil( x$index/ width)
    x %>% split(x$slice) %>% lapply(function(x,...){ff(x,...)}) %>% connector
    
    
  }) %T>% {names(.) <- tickers} 
}

map_data_slice <- function(data,ff=function(x,...){x},...,width,connector){
  
  
  
  lapply(data,function(x){
    x$index <- 1:nrow(x)
    x$slice <- ceil( x$index/ width)
    x %>% split(x$slice) %>% lapply(function(x,...){ff(x,...)}) %>% connector
  }) 
}




map_tickers_slice_month <-   function(tickers,ff=function(x,...){x},...,months,connector=function(x){x}){
  
  companies <- paste0("data/Stocks/", tickers,".us.txt")
  mclapply(companies,function(x){
    x %<>% read_stock(.,...) 

    x$Month <- format( as.POSIXct( x$Date ),"%m")
    x$Month_lag_1 <- Lag( x$Month,1);x$Month_lag_1[1] <- x$Month[1]
    x$new_month <- x$Month != x$Month_lag_1
    x$month_id <- cumsum(x$new_month) +1
    
    x$slice <- ceil(x$month_id/ months)
    x %>% split(x$slice) %>% lapply(function(x,...){ff(x,...)}) %>%  connector
    
    
  },mc.cores = 3L) %T>% {names(.) <- tickers} 
}


map_data_slice_month <-   function(data,ff=function(x,...){x},...,months,connector=function(x){x}){
  lapply(data,function(x){
    
    x$Month <- format( as.POSIXct( x$Date ),"%m")
    x$Month_lag_1 <- Lag( x$Month,1);x$Month_lag_1[1] <- x$Month[1]
    x$new_month <- x$Month != x$Month_lag_1
    x$month_id <- cumsum(x$new_month) +1
    
    x$slice <- ceil(x$month_id/ months)
    x %>% split(x$slice) %>% lapply(function(x,...){ff(x,...)}) %>%  connector
    
    
  }) %T>% {names(.) <- tickers} 
}
