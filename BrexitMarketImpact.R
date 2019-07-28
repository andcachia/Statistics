############ 
#
# The functionality in this class allows us to identify major events in Brexit, and determine the significance of their impact on various financial markets.
# This class is split into a number of different sections, each section performing the required pre-processing until finally performing the hypothesis testing.
#
###########
 
require(TTR)
Sys.setlocale("LC_ALL","C")

############ Google Trends SVI Adjustment
#
# Here we scale the Google Trends daily data appropriately by comparing it to the corresponding weekly data
#
###########

daily = read.csv("/home/andrew/pCloudDrive/Uni/Statistics/Assignment/201819_ICS5115_CACHIA_ANDREW_211494M_assignment_code/Data/GoogleTrends_Brexit_Modified.csv", header = TRUE, skip = 2, sep=",", stringsAsFactors=FALSE);
weekly = read.csv("/home/andrew/pCloudDrive/Uni/Statistics/Assignment/201819_ICS5115_CACHIA_ANDREW_211494M_assignment_code/Data/Google Trends Brexit Weekly.csv", header = TRUE, skip = 2, sep=",", stringsAsFactors=FALSE);
colnames(daily)[2] <- "SVI"
colnames(weekly)[2] <- "SVI"

# Loop through weeks, and aggregate corresponding daily values for that week
for (i in 0:(nrow(weekly)-1)) {
  current = i*7
  currentEnd = current+7
  if (currentEnd > nrow(daily)) currentEnd = nrow(daily)
  daily_volume_sum = sum(daily$SVI[(current+1):(currentEnd)])
  weekly$daily_aggregate[i+1]<-daily_volume_sum
}

# Calcualte adjustment factor for that week by dividing aggregated daily data from the weekly SVI
weekly$adjustment_factor <- weekly$SVI / weekly$daily_aggregate
daily$VolumeAdjusted = 0

# Adjust each day's value by the corresponding weekly adjustment factor
for (i in 0:(nrow(weekly)-1)) {
  current = (i*7)+1
  currentEnd = current+7
  if (currentEnd > nrow(daily)) currentEnd = nrow(daily)
  daily$VolumeAdjusted[current:currentEnd] = weekly$adjustment_factor[(i+1)] * daily$SVI[current:currentEnd]
}

# Plot the transformation of the time series
old.par <- par(mfrow=c(3,1))
plot(weekly$SVI, type = "l",  col="red", main = "Google Trends Weekly")
plot(daily$SVI, type = "l",  col="red", main = "Google Trends Daily Before Adjustment")
plot(daily$VolumeAdjusted, type = "l", col="red", main = "Google Trends Daily After Adjustment")
par(old.par)

################################## 

############ Creating Time Series
#
# Here we extract the time series from the data, correlate them and calculate the Google Trends RSI
#
###########

# Read raw data from csv files
gbpEurData = read.csv("/home/andrew/pCloudDrive/Uni/Statistics/Assignment/201819_ICS5115_CACHIA_ANDREW_211494M_assignment_code/Data/GBP_EUR Historical Data.csv", header = TRUE, sep=",");#, row.names="Date");
gbpEurData$DayOfWeek <- weekdays(as.Date(gbpEurData$Date, format = "%b %d, %Y"))

# Use data calcualted in section above
googleTrendsData = daily
googleTrendsData$DayOfWeek <- weekdays(as.Date(googleTrendsData$Day, format = "%Y-%m-%d"))

# Remove all weekend observations
gbpEurFiltered = gbpEurData[which(gbpEurData$DayOfWeek %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
googleTrendsFiltered = googleTrendsData[which(googleTrendsData$DayOfWeek %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]

gbpEur = gbpEurFiltered[2]
googleTrends = googleTrendsFiltered[3]

# Plot both time series superimposed
plot(scale(gbpEur), type = "l",  col="blue", main = "Time Series Superimposed")
lines(scale(googleTrends), type = "l", col="red")

# Calculate correlation
corr = ccf(gbpEur,googleTrends)

# Calculate RSI for Google Trends
rsiWindow = 14
rsiGoogleTrends = RSI(googleTrends, n=rsiWindow)

plot(rsiGoogleTrends, type = "l", col="red", ylim=c(0,100), main = "Google Trends RSI")
abline(h = 70, lty=2)
abline(h = 30, lty=2)

################################################

######################## Generate Absolute Returns Function

# Transform price time series into returns
generateAbsoluteReturns <- function(prices){
  # Initialize Vector
  returns <- vector()
  returns[1] = 0
  
  # De-trend financial time series - convert to returns
  for (i in 2:length(prices)){
    returns[i] = (prices[i] - prices[i-1]) / prices[i-1]
  }
  
  # Take absolute returns
  absReturns = abs(returns)
  
  return(absReturns)
}

################################################

######################## Determining Events and Hypothesis Testing


# Transform price time series into returns
runTest <- function(timeSeriesName,prices){
  
  absReturns = generateAbsoluteReturns(prices)
  
  # Plot returns and rsi
  plot(scale(absReturns), type = "l", main = paste(timeSeriesName, "Returns vs RSI", sep=" "))
  lines(scale(rsiGoogleTrends), type = "l", col="red")
  
  sampleWindow <- vector()
  eventWindow <- vector()
  
  for (i in 1:rsiWindow) {
    sampleWindow <- c(sampleWindow, absReturns[i])
  }
  
  flagEvent = FALSE
  eventCount = 3
  eventDates <- vector()
  
  # Loop through RSI values, when RSI is over 70 mark as an event and place financial data in event window.
  # All other data should be in the sample window
  for (i in (rsiWindow+1):length(rsiGoogleTrends)) {
    if (rsiGoogleTrends[i] > 70){
      flagEvent = TRUE
      eventCount = 3
      eventDates <- c(eventDates, googleTrendsFiltered$Day[i])
    }
    
    if (flagEvent == TRUE){
      eventWindow <- c(eventWindow, absReturns[i])
      eventCount = eventCount - 1
      if (eventCount == 0) flagEvent = FALSE
    } 
    else{
      sampleWindow <- c(sampleWindow, absReturns[i])
    }
  }
  
  meanExpectedReturn = mean(sampleWindow)
  stdDevExpectedReturn = sd(sampleWindow)
  
  cumulativeAbnormalReturns = 0
  
  # Sum all abnormal returns to determine CAR
  for (i in 1:length(eventWindow)) {
    cumulativeAbnormalReturns = cumulativeAbnormalReturns + (eventWindow[i] - meanExpectedReturn)
  }
  
  # Calculate t statistic
  stdDevCAReturns = sqrt(length(eventWindow) * stdDevExpectedReturn * stdDevExpectedReturn)
  tValue = cumulativeAbnormalReturns / stdDevCAReturns
  
  # Find bounds to determine significance
  tcrit=qt(c(.05, .95), df=length(eventWindow)-1)
  
  # Plot t statistic curve
  N = length(eventWindow)
  dum=seq(-9, 9, length=10^4)#For the plot
  
  plot(dum, dt(dum, df=(N-1)), type='l', xlab='t', ylab='f(t)', main = paste(timeSeriesName, "T-Stat", sep=" "))
  abline(v=tValue, col='blue', lty=2)
  abline(v=tcrit, col='red', lty=2)
  abline(v=-tcrit, col='red', lty=2)
  
  print(timeSeriesName)
  print(paste("T Stat bounds:", tcrit, sep=" "))
  print(paste("T Value:", tValue, sep=" "))
  
  return(tValue)
}

# Run test for GBP/EUR time series
tValue = runTest("GbpEur", gbpEur$Price)

################################################

######################## Testing Using Cryptocurrencies


# Read raw data from csv files
btcData = read.csv("/home/andrew/pCloudDrive/Uni/Statistics/Assignment/201819_ICS5115_CACHIA_ANDREW_211494M_assignment_code/Data/btc.csv", header = TRUE, sep=",");
btcData$DayOfWeek <- weekdays(as.Date(btcData$Date, format = "%b %d, %Y"))

ethData = read.csv("/home/andrew/pCloudDrive/Uni/Statistics/Assignment/201819_ICS5115_CACHIA_ANDREW_211494M_assignment_code/Data/eth.csv", header = TRUE, sep=",");
ethData$DayOfWeek <- weekdays(as.Date(ethData$Date, format = "%b %d, %Y"))

# Remove all weekend observations
btcFiltered = btcData[which(btcData$DayOfWeek %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]
ethFiltered = ethData[which(ethData$DayOfWeek %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')), ]

btcTValue = runTest("BTC", btcFiltered$Close)
ethTValue = runTest("ETH", ethFiltered$Close)