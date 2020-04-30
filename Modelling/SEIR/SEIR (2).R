# install.packages("deSolve")
library (deSolve)
seir_model = function (current_timepoint, state_values, parameters)
{
  # create state variables (local variables)
  S = state_values [1]        # susceptibles
  E = state_values [2]        # exposed
  I = state_values [3]        # infectious
  R = state_values [4]        # recovered
  
  with (
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivatives
      dS = (-beta * S * I)
      dE = (beta * S * I) - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I)
      
      # combine results
      results = c (dS, dE, dI, dR)
      list (results)
    }
  )
}

# contact_rate = 14.781                     # number of contacts per day -> depends on social patterns
# transmission_probability = 2.1*10^-8       # transmission probability
infectious_period = 2.9                 # infectious period
latent_period = 5.2                     # latent/incubation period

# beta_value = contact_rate * transmission_probability     # effective contract rate
gamma_value = 1 / infectious_period
delta_value = 1 / latent_period
Ro = 3.68
beta_value = gamma_value*Ro
# Ro = beta_value / gamma_value

parameter_list = c(beta = beta_value, gamma = gamma_value, delta = delta_value)

W = 966000000           # susceptible hosts -> 70% of the population
X = 0                 # exposed hosts
Y = 82                 # infected hosts -> number of people infected due to travel abroad in initial days
Z = 0                 # recovered hosts
N = W + X + Y + Z     # total population

initial_values = c(S = W/N, E = X/N, I = Y/N, R = Z/N)

timepoints = seq (0, 150, by=1)

output = lsoda (initial_values, timepoints, seir_model, parameter_list)

head(output)
# plot (S ~ time, data = output, type='b', col = 'blue')
# plot (E ~ time, data = output, type='b', col = 'pink')
# plot (I ~ time, data = output, type='b', col = 'red')
# plot (R ~ time, data = output, type='b', col = 'green')

india <- as.data.frame(read.csv("~/Desktop/COVID-19/TSF-Challenge-master/Data/India_Time_Series.csv", sep = ",", colClasses=c("character","numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")))
head(india)
india$Date <- as.Date(india$Date , format = "%d-%b")
library(xts)
india.xts <- xts(india, order.by=india$Date)
india.xts <- india.xts[, colnames(india.xts) != "Date"]
startDate <- as.Date("12-Mar", format = "%d-%b")
newIndia.xts <- subset(india.xts, index(india.xts)>=startDate)
head(newIndia.xts$Total.Confirmed)

tbu1<- as.numeric(newIndia.xts$Total.Confirmed)/N
length(tbu1)

startDate = as.Date("12-Mar", format="%d-%b")
endDate = as.Date(startDate+151-1)
dates <- seq(startDate, endDate, by="days")
dates1 <- seq(startDate, as.Date(startDate+32-1), by="days")

plot(dates, output[,"S"], type='b', ylim = c(0,1), xlim = as.Date(c("2020-03-12", "2020-08-28")), col='blue', xlab="Date", ylab="S, E, I, R, actual", main = "SEIR epidemic model")
par (new = TRUE)
plot(dates, output[,"E"], type='b', ylim = c(0,1), xlim = as.Date(c("2020-03-12", "2020-08-28")), col='pink', xlab="", ylab="")
par (new = TRUE)
plot(dates, output[,"I"], type='b', ylim = c(0,1), xlim = as.Date(c("2020-03-12", "2020-08-28")), col='red', xlab="", ylab="")
par (new = TRUE)
plot(dates, output[,"R"], type='b', ylim = c(0,1), xlim = as.Date(c("2020-03-12", "2020-08-28")), col='green', xlab="", ylab="")
par (new = TRUE)
plot(dates1, tbu1, type='b', ylim = c(0,1), xlim = as.Date(c("2020-03-12", "2020-08-28")), col='black', xlab="", ylab="")

legend("bottomleft", 
       legend = c("S", "E", "I", "R", "actual"), 
       col = c(rgb(0,0,1),rgb(0.8,0.6,0.7), rgb(1,0,0), rgb(0,1,0), rgb(0,0,0)), 
       pch = c(19, 19, 19, 19, 19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.6, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1, 0.1, 0.1, 0.1))

# susceptible hosts over time
# plot (S ~ time, data = output, type='b', ylim = c(0,1), col = 'blue', ylab = 'S, E, I, R', main = 'SEIR epidemic') 
# remain on same frame
# par (new = TRUE)
# exposed hosts over time
# plot (E ~ time, data = output, type='b', ylim = c(0,1), col = 'pink', ylab = '', axes = FALSE)
# remain on same frame
# par (new = TRUE)
# infectious hosts over time
# plot (I ~ time, data = output, type='b', ylim = c(0,1), col = 'red', ylab = '', axes = FALSE)
# remain on same frame
# par (new = TRUE)
# recovered hosts over time
# plot (R ~ time, data = output, type='b', ylim = c(0,1), col = 'green', ylab = '', axes = FALSE)

plot(dates, output[,"I"], type='b', ylim = c(0, 0.00005), xlim = as.Date(c("2020-03-12", "2020-04-20")), col='red', xlab="", ylab="")
par (new = TRUE)
plot(dates1, tbu1, type='b', ylim = c(0, 0.00005), xlim = as.Date(c("2020-03-12", "2020-04-20")), col='black', xlab="", ylab="")
