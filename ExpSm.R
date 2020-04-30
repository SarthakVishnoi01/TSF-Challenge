india <- as.data.frame(read.csv("~/Desktop/COVID-19/India_Time_Series.csv", sep = ",", colClasses=c("character","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")))
tail(india)
india$Date <- as.Date(india$Date , format = "%d-%b")
library(xts)
india.xts <- xts(india, order.by=india$Date)
india.xts <- india.xts[, colnames(india.xts) != "Date"]
startDate <- as.Date("12-Mar", format = "%d-%b")
newIndia.xts <- subset(india.xts, index(india.xts)>=startDate)
head(newIndia.xts$Total.Confirmed)
tbu <- as.numeric(newIndia.xts$Total.Confirmed)
dates <- seq(startDate, as.Date(startDate+length(tbu) - 1), by="days")
# plot(dates, tbu, type='b', ylim = c(0,48000), xlim = as.Date(c("2020-03-12", "2020-05-08")), col='black', xlab="Date", ylab="Total number of cases", main = "Observed")

library(forecast)
library(lubridate)

# ses1 <- ses(tbu)
# summary(ses1)
# plot(ses1)

# plot(holt1, xaxt='n')
# axis(1, at=seq(1, 90, by=10) , las=2, labels=seq(as.Date('2020-01-30'), as.Date('2020-04-12')+weeks(30), length.out=9) )


date1 <- as.Date("22-Mar", format = "%d-%b")
newIndia1 <- subset(india.xts, index(india.xts)>=startDate & index(india.xts)<date1)
tbu1 <- as.numeric(newIndia1$Total.Confirmed)
dates1 <- seq(startDate, as.Date(startDate+length(tbu1) - 1), by="days")
holt2 <- holt(tbu1)
# summary(holt2)
plot(holt2, ylim = c(0,48000), xlim= c(0,60), xlab = "Days since March 12", ylab = "Total Number of Cases")

date2 <- as.Date("01-Apr", format = "%d-%b")
newIndia2 <- subset(india.xts, index(india.xts)>=startDate & index(india.xts)<date2)
tbu2 <- as.numeric(newIndia2$Total.Confirmed)
dates1 <- seq(startDate, as.Date(startDate+length(tbu2) - 1), by="days")
holt3 <- holt(tbu2)
# summary(holt3)
par (new = TRUE)
plot(holt3, ylim = c(0,48000), xlim= c(0,60))

date3 <- as.Date("11-Apr", format = "%d-%b")
newIndia3 <- subset(india.xts, index(india.xts)>=startDate & index(india.xts)<date3)
tbu3 <- as.numeric(newIndia3$Total.Confirmed)
dates3 <- seq(startDate, as.Date(startDate+length(tbu3) - 1), by="days")
holt4 <- holt(tbu3)
# summary(holt4)
par (new = TRUE)
plot(holt4, ylim = c(0,48000), xlim= c(0,60))

date4 <- as.Date("25-Apr", format = "%d-%b")
newIndia4 <- subset(india.xts, index(india.xts)>=startDate & index(india.xts)<date4)
tbu4 <- as.numeric(newIndia4$Total.Confirmed)
dates4 <- seq(startDate, as.Date(startDate+length(tbu4) - 1), by="days")
holt5 <- holt(tbu4)
# summary(holt5)
par (new = TRUE)
plot(holt5, ylim = c(0,48000), xlim= c(0,60))

legend("bottomleft", 
       legend = c("Actual number of cases", "Forecasts", "80% confidence interval", "95% confidence interval"), 
       col = c(rgb(0,0,0),rgb(0,0,1), rgb(0.5,0.5,0.8), rgb(0.69,0.68,0.79)),
       pch = c(19, 19, 19, 19, 19),
       bty = "n",
       pt.cex = 1,
       cex = 0.6,
       text.col = "black", 
       horiz = F,
       inset = c(0.1, 0.1, 0.1, 0.1, 0.1)
)

# par (new = TRUE)
# holt1 <- holt(tbu)
# summary(holt1)
# plot(holt1, ylim = c(0,48000), xlim = c(0,60), xlab="Date", ylab="Total number of cases", main = "Observed")