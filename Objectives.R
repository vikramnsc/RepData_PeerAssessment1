library(dplyr)
library(lubridate)
library(ggplot2)
library(knitr)
library(markdown)

# Objective1:Reading in Data


Data_assignment<-read.csv(file= "activity.csv", header = TRUE, sep=",")

str(Data_assignment)
head(Data_assignment)
tail(Data_assignment)

if (FALSE) {
  ymd<- NULL
     day <- NULL
 month <- NULL
 select<- NULL
 
}

##Data Wrangling with column of dates
Data_assignment$Dateform<- ymd(Data_assignment$date)
Data_assignment$Day<-day(Data_assignment$Dateform)
Data_assignment$Month<-month(Data_assignment$Dateform)
Data_assignment$weekend <- as.factor(ifelse(weekdays(Data_assignment$Dateform)=="Saturday" | weekdays(Data_assignment$Dateform)=="Sunday","weekend","weekday"))
Data_assignment$dayofweek <- as.factor(weekdays(Data_assignment$Dateform))

View(Data_assignment)

str(Data_assignment)
head(Data_assignment)
tail(Data_assignment)

# Objective 2: Create Histogram of the total number of steps taken each day


Steps_Day <- Data_assignment%>% group_by(Dateform) %>% summarise(Steps_Day = sum(steps,na.rm = TRUE))
View(Steps_Day)
Plot1<-qplot(Steps_Day,data=Steps_Day,na.rm=TRUE,binwidth=1000,
xlab= "Total Steps taken per day", 
ylab="Frequency using binwith 1000",main = "Histogram of the Total number of Steps per Day")

Plot1 

dev.copy(png,'Plot1.png')
dev.off()

#Objective3: Mean and Median number of steps taken each day

Steps_Day_mean_median<- Steps_Day%>% summarise(average_steps = mean(Steps_Day,na.rm = TRUE),
median_steps=median(Steps_Day,na.rm = TRUE))

Steps_Day_mean_median

#Objective4: Create Time series plot of the average number of steps taken

TimeSeries_Steps<- Data_assignment %>%  group_by(interval) %>% 
summarize(Steps_mean=mean(steps, na.rm=TRUE))

View(TimeSeries_Steps)
head(TimeSeries_Steps)
tail(TimeSeries_Steps)

Plot2<-ggplot(TimeSeries_Steps, aes(interval, Steps_mean)) + geom_line()

Plot2

dev.copy(png, 'Plot2.png')
dev.off()

#Objective 5: To calculate the 5-minute interval, contains the 
#              maximum number of steps?

Int_max_steps<- TimeSeries_Steps[TimeSeries_Steps$Steps_mean==max(TimeSeries_Steps$Steps_mean),]
Int_max_steps



#Objective 6: Calculate and report the total number of 
#missing values in the dataset (i.e. the total number of rows with NAs)

Data_without_NA <- na.omit(Data_assignment)
Rows_NA<- nrow(Data_assignment)-nrow(Data_without_NA)
Rows_NA

#Objective 6.1: Impute missing values
#I will replace the missing values with the mean of the number of steps for the 
#5-minute interval across all the days and generate the dataset with imputed values

names(TimeSeries_Steps)[2] <- "Mean_Steps"
Dataset_imputed <- merge(Data_assignment, Data_without_NA)
is.na(Dataset_imputed$steps)

Dataset_imputed$steps[is.na(Dataset_imputed$steps)] <- Dataset_imputed$Mean_Steps[is.na(Dataset_imputed$Mean_Steps)]

summary(Dataset_imputed)
str(Dataset_imputed)

View(Dataset_imputed)
head(Dataset_imputed)
tail(Dataset_imputed)


#Objective 6.2: Create Histogram for total number of steps taken each day from the imputed data.

SumSteps_Day_newdata <- Dataset_imputed %>%  group_by(Dateform) %>% 
summarize(Total_steps_day=sum(steps))

summary(SumSteps_Day_newdata)
str(SumSteps_Day_newdata)

Plot3<-hist(SumSteps_Day_newdata$Total_steps_day, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks=50)
Plot3

dev.copy(png, 'Plot3.png')
dev.off()


#Objective 6.3: Compare the mean and median of Old and New data

NewData<-summary(Dataset_imputed$steps)
OldData<-summary(Data_assignment$steps, na.rm=TRUE)


NewData
OldData

#Objective7: Are there differences in activity patterns between weekdays and weekends?
#Dataset_imputed already has been created using the Dataset_assignment data
#which has been converted to the appropriate columns of Date class and the 
#weekday and weekend factors included.

str(Dataset_imputed)

#Objective 7.1: Make a panel plot containing a time series plot (i.e. type = “l”) 
#of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all weekday days or weekend days (y-axis).


Average_Steps<- Dataset_imputed %>% group_by(interval,weekend) %>%   
  summarise(Average_Steps = mean(steps))

summary(Average_Steps)


Plot4<-ggplot(Average_Steps, aes(x=interval, y=Average_Steps, color=weekend)) + geom_line(linetype=1) +
facet_grid(weekend~.)+ xlab("Interval") + ylab("Mean_Steps") +
ggtitle("Comparison of Average Number of Steps")

Plot4

dev.copy(png, "Plot4.png")
dev.off()

