# install.packages("deSolve")
library (deSolve)
seir_model = function (current_timepoint, state_values, parameters)
{
  # create state variables (local variables)
  Sa = state_values [1]        # susceptibles
  Ss = state_values [2]
  Ea = state_values [3]        # exposed
  Es = state_values [4]
  Ia = state_values [5]        # infectious
  Is = state_values [6]
  Ra = state_values [7]        # recovered
  Rs = state_values [8]
  
  with (
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivatives
      dSa = (-beta1 * Sa * Ia - beta2 * Sa * Is)
      dSs = (-beta3 * Ss * Is - beta4 * Ss * Ia)
      dEa = (beta1 * Sa * Ia + beta2 * Sa * Is) - (delta * Ea)
      dEs = (beta3 * Ss * Is + beta4 * Ss * Ia) - (delta * Es)
      dIa = (delta * Ea) - (gamma * Ia)
      dIs = (delta * Es) - (gamma * Is)
      dRa = (gamma * Ia)
      dRs = (gamma * Is)
      
      # combine results
      results = c (dSa, dSs, dEa, dEs, dIa, dIs, dRa, dRs)
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
Ro = 2.34
beta_value = gamma_value*Ro
# Ro = beta_value / gamma_value
betatoo = 1.34/2.9
parameter_list = c(beta1 = beta_value, beta2 = betatoo, beta3 = 2.7/2.9, beta4 = betatoo, gamma = gamma_value, delta = delta_value)

W = 966000000           # susceptible hosts -> 70% of the population
X = 0                 # exposed hosts
Y = 82                 # infected hosts -> number of people infected due to travel abroad in initial days
Z = 0                 # recovered hosts
N = W + X + Y + Z     # total population

initial_values = c (Sa = 0.915*W/N, Ss = 0.085*W/N, Ea = 0.915*X/N, Es = 0.085*X/N, Ia = 0.915*Y/N, Is = 0.085*Y/N, Ra = 0.915*Z/N, Rs = 0.085*Z/N)

timepoints = seq (0, 200, by=1)

output = lsoda (initial_values, timepoints, seir_model, parameter_list)

head(output)
# plot (S ~ time, data = output, type='b', col = 'blue')
# plot (E ~ time, data = output, type='b', col = 'pink')
# plot (I ~ time, data = output, type='b', col = 'red')
# plot (R ~ time, data = output, type='b', col = 'green')

S <- output[,"Sa"]+output[,"Ss"]
E <- output[,"Ea"]+output[,"Es"]
I <- output[,"Ia"]+output[,"Is"]
R <- output[,"Ra"]+output[,"Rs"]

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
endDate = as.Date(startDate+200)
dates <- seq(startDate, endDate, by="days")
dates1 <- seq(startDate, as.Date(startDate+32-1), by="days")

plot(dates, S, type='b', ylim = c(0,1), xlim = as.Date(c("2020-03-12", "2020-09-28")), col='blue', xlab="Date", ylab="S, E, I, R", main = "SEIR epidemic model")
par (new = TRUE)
plot(dates, E, type='b', ylim = c(0,1), xlim = as.Date(c("2020-03-12", "2020-09-28")), col='pink', xlab="", ylab="")
par (new = TRUE)
plot(dates, I, type='b', ylim = c(0,1), xlim = as.Date(c("2020-03-12", "2020-09-28")), col='red', xlab="", ylab="")
par (new = TRUE)
plot(dates, R, type='b', ylim = c(0,1), xlim = as.Date(c("2020-03-12", "2020-09-28")), col='green', xlab="", ylab="")
par (new = TRUE)
plot(dates1, tbu1, type='b', ylim = c(0,1), xlim = as.Date(c("2020-03-12", "2020-09-28")), col='black', xlab="", ylab="")

#plot(dates, I, type='b', ylim = c(0, 0.00001), xlim = as.Date(c("2020-01-30", "2020-06-28")), col='red', xlab="", ylab="")
#par (new = TRUE)
#plot(dates1, tbu1, type='b', ylim = c(0, 0.00001), xlim = as.Date(c("2020-01-30", "2020-06-28")), col='black', xlab="", ylab="")

barplot(
  t(output[seq(1,length(dates),by=10),-1]), 
  names.arg = seq(1,length(dates),by=10),
  xlab='Time',main='Population structured epidemic modelling', 
  space=0,
  col=c(
    rgb(0.5,0.5,1),
    rgb(0,0,1),
    rgb(0.9,0.5,0.6),
    rgb(1,0,0.5),
    rgb(1,0.5,0.5),
    rgb(1,0,0),
    rgb(0.5,1,0.5),
    rgb(0,1,0)
  ),
  legend = colnames(output[seq(1,length(dates),by=10),-1]),
  args.legend=list(bg="transparent")
)
