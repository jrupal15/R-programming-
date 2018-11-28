library(quantmod)
library(ggplot2)
library(dplyr)
AAPL <- getSymbols("AAPL", src = "yahoo", from = "2017-01-01", to = "2018-04-01", auto.assign = FALSE)
AAPL
MSFT <- getSymbols("MSFT", src = "yahoo", from = "2017-01-01", to = "2018-04-01", auto.assign = FALSE)
MSFT
GOOG <- getSymbols("GOOG", src = "yahoo", from = "2017-01-01", to = "2018-04-01", auto.assign = FALSE)
GOOG
# prices for AAPL, MSFT, and GOOG
stocks <- as.xts(data.frame(AAPL = AAPL[, "AAPL.Close"], MSFT = MSFT[, "MSFT.Close"], 
                            GOOG = GOOG[, "GOOG.Close"]))
stocks
# a plot showing all series as lines
plot(as.zoo(stocks), screens = 1, lty = 1:3, xlab = "Date", ylab = "Price")
legend("right", c("AAPL", "MSFT", "GOOG"), lty = 1:3, cex = 0.8)

ggplot(AAPL, aes(x = index(AAPL), y = AAPL[,6])) + geom_line(color = "darkblue") + ggtitle("Apple prices series") + xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")
ggplot(MSFT, aes(x = index(AAPL), y = MSFT[,6])) + geom_line(color = "darkblue") + ggtitle("Microsoft prices series") + xlab("Date") + ylab("Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

#Moving Average
candleChart(AAPL, up.col = "blue", dn.col = "red", theme = "white")
addSMA(n = 50)

candleChart(AAPL, up.col = "black", dn.col = "blue", theme = "white", subset = "2017-01-04/")
addSMA(n = c(20, 50, 200))

chart_Series(AAPL,multi.col=TRUE,theme_classic(),subset= "2015-1::2015-6")+add_BBands()

#Return
Op(AAPL)
Ad(AAPL)
drA <- dailyReturn(AAPL)
drM <- dailyReturn(MSFT)
drG <- dailyReturn(GOOG)
return <- as.xts(data.frame(drA,drM,drG))
return
plot(as.zoo(return), screens = 1, lty = 1:3, xlab = "Date", ylab = "Price")
legend("right", c("drA", "drM", "drG"), lty = 1:3, cex = 0.8)

drA2 <- dailyReturn(AAPL,subset =  "2015-1::2015-6" )
drM2 <- dailyReturn(MSFT,subset =  "2015-1::2015-6" )
drG2 <- dailyReturn(GOOG,subset =  "2015-1::2015-6" )
return2 <- as.xts(data.frame(drA2,drM2,drG2))
return2
plot(as.zoo(return2), screens = 1, lty = 1:3, xlab = "Date", ylab = "Price")
legend("topright", c("DR APPL", "DR MSFT", "DR GOOG"), lty = 1:3, cex = 0.8)

plot(weeklyReturn(GOOG, subset="2016::"), main="Weekly return of Google")
candleChart(GOOG, subset="2018::2018-03-31", name="Google", theme="white")
chartSeries(GOOG, subset="2016::2016-12-31", type="line", name="Google", theme="white")
addBBands()
addRSI()

