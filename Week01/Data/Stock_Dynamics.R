### STOCK DYNAMICS
 IBM <- read.csv("/Users/craigzimber/Documents/R/The_Analytics_Edge/IBMStock.csv", 
                header = TRUE, stringsAsFactors = FALSE)
 GE <- read.csv("/Users/craigzimber/Documents/R/The_Analytics_Edge/GEStock.csv", 
                header = TRUE, stringsAsFactors = FALSE)
 ProcterGamble <- read.csv("/Users/craigzimber/Documents/R/The_Analytics_Edge/ProcterGambleStock.csv", 
                header = TRUE, stringsAsFactors = FALSE)
 CocaCola <- read.csv("/Users/craigzimber/Documents/R/The_Analytics_Edge/CocaColaStock.csv", 
                header = TRUE, stringsAsFactors = FALSE)
 Boeing <- read.csv("/Users/craigzimber/Documents/R/The_Analytics_Edge/BoeingStock.csv", 
                header = TRUE, stringsAsFactors = FALSE)

 str(IBM)

 IBM$Date <- as.Date(IBM$Date, "%m/%d/%y")
 GE$Date <- as.Date(GE$Date, "%m/%d/%y")
 CocaCola$Date <- as.Date(CocaCola$Date, "%m/%d/%y")
 ProcterGamble$Date <- as.Date(ProcterGamble$Date, "%m/%d/%y")
 Boeing$Date <- as.Date(Boeing$Date, "%m/%d/%y")
 
# Our five datasets all have the same number of observations. How many observations 
# are there in each data set?
 dim(IBM)
# What is the earliest year in our datasets? 
 head(IBM)
# What is the latest year in our datasets? 
 tail(IBM)
# What is the mean stock price of IBM over this time period?
 mean(IBM$StockPrice)
# What is the minimum stock price of General Electric 
# (GE) over this time period?
 min(GE$StockPrice)
# What is the maximum stock price of Coca-Cola over 
# this time period? 
 max(CocaCola$StockPrice)
# What is the median stock price of Boeing over this time period?
 median(Boeing$StockPrice)
# What is the standard deviation of the stock price of 
# Procter & Gamble over this time period?
 sd(ProcterGamble$StockPrice)
 
# Using the plot function, plot the Date on the x-axis and the 
# StockPrice on the y-axis, for Coca-Cola.
 plot(CocaCola$Date, CocaCola$StockPrice, type="l",col="red")
 lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
 abline(v=as.Date(c("2000-03-01")), lwd=2, col="grey")
 abline(v=as.Date(c("1983-01-01")), lwd=2, col="grey")
 
 
 plot(CocaCola$Date[401:432], CocaCola$StockPrice[401:432], type="l", col="red", ylim=c(0,210))
 lines(ProcterGamble$Date[401:432], ProcterGamble$StockPrice[401:432], col="blue")
 lines(IBM$Date[401:432], IBM$StockPrice[401:432], col="green")
 lines(GE$Date[401:432], GE$StockPrice[401:432], col="black")
 lines(Boeing$Date[401:432], Boeing$StockPrice[401:432], col="yellow")
 abline(v=as.Date(c("2004-01-01")), lwd=2, col="grey")
 abline(v=as.Date(c("2005-12-31")), lwd=2, col="grey")
 
 
 plot(as.numeric(tapply(IBM$StockPrice, months(IBM$Date), mean) > mean(IBM$StockPrice)),
      type="l", col="red", ylim=c(0,2))
 lines(as.numeric(tapply(ProcterGamble$StockPrice, 
                              months(ProcterGamble$Date), mean) > 
                               mean(ProcterGamble$StockPrice)),col="blue")
 
      
 
 
 
 sort(tapply(GE$StockPrice, months(GE$Date), mean))
 sort(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))
 
 
 
 months(IBM$Date)
 months(IBM$Date)
 
 
 
