read_promet <- function(path){
  data <- read.csv(path)
  data$X <- as.Date(data$X, "%Y-%m-%d")
  data$Dan <- sapply(data$X, weekdays, abbreviate = TRUE)
  data$Mjesec <- sapply(data$X, function(date){
    format(date, format = "%m")
  })
  data$Godina <- sapply(data$X, function(date){
    format(date, format = "%Y")
  }
  )
  data$Quarter <- sapply(data$Mjesec, function(month){
    floor((strtoi(month, base=10L) + 2) / 3)
  })
  return(data)
}


get_promet <- function(promet, year, quarter){
  return(mean(subset(promet, promet$Quarter == quarter & promet$Godina == year)$UKUPNO))
}

check <- function(stock, perc, year, quarter){
  if(quarter == 0){
    first_date <- paste(as.character(year), "-01-01", sep = "")
    last_date <- paste(as.character(year), "-12-31", sep = "") 
  }
  else if(quarter == 1){
    first_date <- paste(as.character(year), "-01-01", sep = "")
    last_date <- paste(as.character(year), "-03-31", sep = "") 
  }
  else if(quarter == 2){
    first_date <- paste(as.character(year), "-04-01", sep = "")
    last_date <- paste(as.character(year), "-06-30", sep = "") 
  }
  else if(quarter == 3){
    first_date <- paste(as.character(year), "-07-01", sep = "")
    last_date <- paste(as.character(year), "-12-31", sep = "") 
  }
  else if(quarter == 4){
    first_date <- paste(as.character(year), "-10-01", sep = "")
    last_date <- paste(as.character(year), "-12-31", sep = "") 
  }
  stock <- subset(stock, stock["Date"] >= first_date & stock["Date"] <= last_date)
  stock <- stock[order(nrow(stock):1),]
  first_price <- stock["Price"][[1]][1]
  first_date <- stock["Date"][[1]][1]
  
  #print(first_date)
  #print(first_price)
  #print(nrow(stock))
  for(i in 1:nrow(stock)){
    price <- stock["Price"][[1]][i]
    date <- stock["Date"][[1]][i]
    #print(price)
    #print(date)
    if(((price - first_price) / first_price) * 100 >= perc){
      return(1)
    }
  }
  return(0)
}

avg_price <- function(stock, year, quarter){
  if(quarter == 1){
    first_date <- paste(as.character(year), "-01-01", sep = "")
    last_date <- paste(as.character(year), "-03-31", sep = "") 
  }
  else if(quarter == 2){
    first_date <- paste(as.character(year), "-04-01", sep = "")
    last_date <- paste(as.character(year), "-06-30", sep = "") 
  }
  else if(quarter == 3){
    first_date <- paste(as.character(year), "-07-01", sep = "")
    last_date <- paste(as.character(year), "-9-30", sep = "") 
  }
  else if(quarter == 4){
    first_date <- paste(as.character(year), "-10-01", sep = "")
    last_date <- paste(as.character(year), "-12-31", sep = "") 
  }
  stock <- subset(stock, stock["Date"] >= first_date & stock["Date"] <= last_date)
  stock <- stock[order(nrow(stock):1),]
  sum_price <- 0
  for(i in 1:nrow(stock)){
    price <- stock["Price"][[1]][i]
    sum_price <- sum_price + price
  }
  return(sum_price / nrow(stock))
}

avg_price_year <- function(stock, year){
  first_date <- paste(as.character(year), "-01-01", sep = "")
  last_date <- paste(as.character(year), "-12-31", sep = "") 
  stock <- subset(stock, stock["Date"] >= first_date & stock["Date"] <= last_date)
  stock <- stock[order(nrow(stock):1),]
  sum_price <- 0
  for(i in 1:nrow(stock)){
    price <- stock["Price"][[1]][i]
    sum_price <- sum_price + price
  }
  return(sum_price / nrow(stock))
}