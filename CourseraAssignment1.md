#load needed libraries 
library(ggplot2)
library(dplyr)
library(tidyverse)
library(chron)
library(ggpubr)

#1.Reading the dataset 

setwd("C:\\Users\\maknickel\\Desktop")
data <- read.csv("activity.csv", header = TRUE)
dframe <- data.frame(data)  
dframe

#2. plotting the steps per day

##creating a sum of the steps per day 

dframe$daynum <- as.numeric(as.factor(dframe$date))
dframe


dframe$dailysteps <- 0
dframe$steps1 <- dframe$steps

##removing NA values to make the data easier to work with 

for(i in 1:nrow(dframe))
{
  if(is.na(dframe$steps[i]))
  {
    dframe$steps1[i] <- 0
  }
}

##calculating the steps per day
day <- 1
for(i in 1:nrow(dframe))
{
  if(day == dframe$daynum[i])
  {
      if(dframe$interval[i] == 0)
      {
        dframe$dailysteps[i] <- dframe$steps1[i]
      }
      else
      {
        dframe$dailysteps[i] <- dframe$dailysteps[i-1] + dframe$steps1[i]
      }
  }
  else
  {
    day <- dframe$daynum[i]
    dframe$dailysteps[i] <- dframe$steps1[i]
  }
}
dframe

##making the plot 
datashort <- filter(dframe, dframe$interval == 2355)
p1 <- hist(datashort$dailysteps)

#3.Mean and median of steps taken per day 

##mean 

meansteps <- mean(datashort$dailysteps)
meansteps

##median
medsteps <- median(datashort$dailysteps)
medsteps

#4.Timeseries plot of average number of steps taken 

##calculate the average steps per interval

avsteps1 <- data.frame(matrix(integer(), nrow = 471, ncol = 2))
colnames(avsteps1) <- c('interval', 'steps')
avsteps1$interval <- seq(5, length.out = nrow(avsteps1), by = 5)
avsteps <- rbind(c(0,0), avsteps1)
avsteps$steps <- 0

for(i in 1:nrow(dframe))
{
  for(j in 1:nrow(avsteps))
  {
    if(dframe$interval[i] == avsteps$interval[j])
    {
      avsteps$steps[j] <- dframe$steps1[i] + avsteps$steps[j]
    }
  }
}
avsteps$steps <- avsteps$steps/61

##plot the new data 

p2 <- ggplot(avsteps, aes(x = interval, y = steps)) + geom_line()
p2

#5. Interval with the max steps 

moststeps <- slice_max(avsteps, steps)
moststeps

#6. Code to describe and show a strategy for inputting missing data 

## In order to input missing data simply perform the following steps 
## 1. read the new data into a data frame with a new name, we will use the name newdata (see part 1 of this script)
## 2a. if adding to the top: data <- rbind(newdata, data) (and when adding to the bottom simply reverse the order in the brackets following the rbind command)
## 2b. if adding to the middle: data <- add_row(steps, date, interval, .before = desired row number)
## 3. rerun the script to get the data you need
## if trying to change a specific cell simply indicate the cell desired: data[row, col] <- newdata

#7. Missing values 

## calculate rows with NA and fill with averages from #4.

NAs <- sum(!complete.cases(dframe))
NAs

filled <- dframe

for(i in 1:nrow(filled))
{
  if(is.na(filled$steps[i]))
    {
        filled$steps[i] <- avsteps$steps[(filled$interval[i]/5+1)]
    }
}

##make a histogram of the total steps each day (filled)
##calculating the steps per day (filled)
day <- 1
for(i in 1:nrow(filled))
{
  if(day == filled$daynum[i])
  {
    if(filled$interval[i] == 0)
    {
      filled$dailysteps[i] <- filled$steps[i]
    }
    else
    {
      filled$dailysteps[i] <- filled$dailysteps[i-1] + filled$steps[i]
    }
  }
  else
  {
    day <- filled$daynum[i]
    filled$dailysteps[i] <- filled$steps1[i]
  }
}
filled

##making the plot (filled)
datashort2 <- filter(filled, filled$interval == 2355)
p3 <- hist(datashort2$dailysteps)
p3 

##Mean and median of steps taken per day (filled)

##mean 

meansteps2 <- mean(datashort2$dailysteps)
meansteps2

##median
medsteps2 <- median(datashort2$dailysteps)
medsteps2

##Median steps remains the same, mean steps increases by approx 1000 steps.
##The number of days with steps in the 5000 - 10 000 bin increases and these 
##values are removed from the 0 - 5000 bin (approx 10% transfer).

#8. Weekend walking 

##update the dataset with the new data

for (i in 1:nrow(filled))
{
  if(is.weekend(filled$date[i]))
  {
    filled$datefactor <- "weekend"
  }
  else
  {
    filled$datefactor <- "weekday"   
  }
}

##make datasets with the weekend/day average data
avstepswd1 <- data.frame(matrix(integer(), nrow = 471, ncol = 2))
colnames(avstepswd1) <- c('interval', 'steps')
avstepswd1$interval <- seq(5, length.out = nrow(avsteps1), by = 5)
avstepsweekend <- rbind(c(0,0), avstepswd1)
avstepsweekend$steps <- 0

for(i in 1:nrow(filled))
{
  if(is.weekend(filled$date[i]))
  {
    for(j in 1:nrow(avstepsweekend))
    {
      if(filled$interval[i] == avstepsweekend$interval[j])
      {
        avstepsweekend$steps[j] <- filled$steps1[i] + avstepsweekend$steps[j]
      }
    }
  }
}

endcount <- 0 
date <- ""
for(i in 1:nrow(filled))
{
  if(is.weekend(filled$date[i]))
    {
        if(date != filled$date[i])
        {
          date <- filled$date[i]
          endcount <- endcount + 1 
        }
    }
}
endcount
avstepsweekend$steps <- avstepsweekend$steps/endcount

avstepswy1 <- data.frame(matrix(integer(), nrow = 471, ncol = 2))
colnames(avstepswy1) <- c('interval', 'steps')
avstepswy1$interval <- seq(5, length.out = nrow(avsteps1), by = 5)
avstepsweekday <- rbind(c(0,0), avstepswy1)
avstepsweekday$steps <- 0

for(i in 1:nrow(filled))
{
  if(!is.weekend(filled$date[i]))
  {
    for(j in 1:nrow(avstepsweekday))
    {
      if(filled$interval[i] == avstepsweekday$interval[j])
      {
        avstepsweekday$steps[j] <- filled$steps1[i] + avstepsweekday$steps[j]
      }
    }
  }
}
avstepsweekday
endcount2 <- 0 
date2 <- ""
for(i in 1:nrow(filled))
{
  if(!is.weekend(filled$date[i]))
  {
    if(date2 != filled$date[i])
    {
      date2 <- filled$date[i]
      endcount2 <- endcount2 + 1 
    }
  }
}
endcount2
avstepsweekday$steps <- avstepsweekday$steps/endcount2

#plot the steps/interval together 
p4 <- ggplot(avstepsweekend, aes(x = interval, y = steps)) + geom_line()
p5 <- ggplot(avstepsweekday, aes(x = interval, y = steps)) + geom_line()
p6 <- ggarrange(p4, p5, labels = c("weekend", "weekday"), ncol = 1, nrow = 2)
p6 


