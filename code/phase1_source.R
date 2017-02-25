rm(list=ls())

weather <- read.csv(file = "weather.csv", header = TRUE, stringsAsFactors = TRUE)

paste("Total rows in dataset: ", nrow(weather),
      " & total values in dataset: ", (nrow(weather) * ncol(weather)))

columnsWithNAs <- function(x){
  TotalsNAs <- colSums(is.na(x))
  TotalsNAs <- as.data.frame(TotalsNAs)
  for(i in 1:ncol(x)){
    TotalsNAs[i,2] <- typeof(x[,i])
    TotalsNAs[i,3] <- class(x[,i])
  }
  colnames(TotalsNAs)[2] <- c("typeof")
  colnames(TotalsNAs)[3] <- c("class")
  return(TotalsNAs)
}

#paste("Column-wise total number of NA values in the dataset are as follows:")
#columnsWithNAs(weather)

paste("Total NA values in entire dataset:", sum(is.na(weather)))

#Cloning the original dataset
weather2 = weather

paste("Data loss if na.omit() is performed on dataset:",
      (nrow(weather2) - nrow(na.omit(weather2))),"rows")

#Adding extra columns by splitting CET column (Date) into Year, Month, Date
datetxt <- as.Date(weather2$CET)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")))

weather2$CET_Year = df$year
weather2$CET_Month = df$month
weather2$CET_Date = df$day
rm(df, datetxt)

#Giving Month names to the month CET_Month having digit months 
#for the use in plots as labels
monthName<-factor("CET_Month", levels=month.abb)
monthNames<-month.abb
for(i in 1:length(levels(monthName))){
  for( j in 1:nrow(weather2)){
    if(weather2$CET_Month[j]==i){
      monthName[j]<-monthNames[i]
    }
  }
}

weather2$CET_Month <- monthName

rm(i, j, monthName, monthNames)

levels(weather2$Events)

levels(weather2$Events)[1] <- "Normal"
paste("Total missing values in CloudCover column",sum(is.na(weather2$CloudCover)))

weather2$CloudCover[is.na(weather2$CloudCover) ] <- -1
weather2$CloudCover <- as.factor(weather2$CloudCover)


colsOfInterest <- c("CET", "CET_Year", "CET_Month", "CET_Date", "Mean.TemperatureC", 
                    "Mean.Humidity", "MeanDew.PointC", "Mean.VisibilityKm", 
                    "Precipitationmm", "CloudCover", "Events")

weather2ForAnalysis <- weather2[,colsOfInterest]

paste("Variables(columns) of interest for data analysis: ")
colsOfInterest
rm(colsOfInterest)

paste("Column-wise NA totals in the final dataset that we are using for analysis: ")
columnsWithNAs(weather2ForAnalysis)

paste("Omitting rows containing NAs from final dataset:", 
      (nrow(weather2ForAnalysis)-nrow(na.omit(weather2ForAnalysis))),"rows omitting")

weather2ForAnalysis <-  na.omit(weather2ForAnalysis)

weather2ForAnalysis$CET <- as.Date(weather2ForAnalysis$CET)


summary(weather2ForAnalysis)

require('ggplot2')

ggplot(weather2ForAnalysis, aes(CET_Year, Mean.VisibilityKm, col = CET_Year)) + 
  geom_point(alpha=.3, color='darkorange') + 
  geom_smooth(fill = 'blue', color='darkblue',alpha=.2, size=1) + 
  xlab("Year") + ylab("Mean Visibility in Km") + 
  ggtitle("Mean Visibility in Km over Time")


trimmed <- weather2ForAnalysis[weather2ForAnalysis$CET_Year>2011,]

trimmed$CET_Month <- as.factor(trimmed$CET_Month)
trimmed$CET_Year <- as.factor(trimmed$CET_Year)

averageofMVKm <- aggregate(trimmed$Mean.VisibilityKm, list(trimmed$CET_Month, trimmed$CET_Year), mean)
colnames(averageofMVKm)<-c("CET_Month", "CET_Year", "Avg.Mean.VisibilityKm")

ggplot(averageofMVKm, aes(CET_Year, Avg.Mean.VisibilityKm, col = CET_Year)) + 
  geom_point() + 
  facet_wrap(~CET_Month) +
  xlab("Month") + ylab("Average of Mean Visibility in Km") + 
  ggtitle("Average of Mean Visibility by Month and Year after 2011")


ggplot(trimmed, aes(CET, Mean.VisibilityKm, col = CloudCover)) + 
  geom_line() + 
  geom_smooth() + 
  facet_wrap(~CloudCover) +
  xlab("Year") + ylab("Mean Visibility in Km") + 
  ggtitle("Mean Visibility over Time and by CloudCover after 2011")

ggplot(trimmed, aes(CET, Mean.VisibilityKm, col = Events)) + 
  geom_line() + 
  geom_smooth() + 
  facet_wrap(~Events) +
  xlab("Year") + ylab("Mean Visibility in Km") + 
  ggtitle("Mean Visibility over Time and by Events after 2011")