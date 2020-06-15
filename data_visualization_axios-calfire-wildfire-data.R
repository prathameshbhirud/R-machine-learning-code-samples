#install.packages("readr")
library(readr)
fireData = read.csv("C:/Prathamesh/Sample Codes/R/RTVS-docs-master/examples/datasets/forest/axios-calfire-wildfire-data.csv", header = TRUE)
dim(fireData)
head(fireData)

#install.packages("lubridate")
#install.packages("dplyr")
library(lubridate)
library(dplyr)

#change dates from character to numeric
#fireData = fireData %>% mutate(start = as.Date(start, "%m-%d-%Y"), end = as.Date(end, "%m-%d-%Y"))

#extract information from the start column to create new month and year variables.
fireData = fireData %>% mutate(Month = month(start, label = TRUE), Year = year(start))

#calculate duration (end - start)
fireData <- fireData %>% mutate(duration = end - start)

#number of acres burned by wildfires over time
fireData = fireData %>% mutate(acres_cumulative = cumsum(acres))

#Visualizations

#area
ggplot(data = fireData) + aes(acres) + geom_histogram(bins = 30) + theme_classic() + scale_x_continuous(labels = scales::comma)

#duration
ggplot(data = fireData) + aes(duration) + geom_histogram(bins = 30) + theme_classic() + scale_x_continuous(labels = scales::comma)

#filter out negative values from duartion
fireData = fireData %>% filter(duration > 0)

month_summary = fireData %>% group_by(month) %>% summarise(fires = n())