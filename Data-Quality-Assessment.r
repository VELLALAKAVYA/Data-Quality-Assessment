#Global Options
rm(list = ls())
options(scipen = 999)
options(stringsAsFactors = FALSE)
memory.limit(size = 20000000000) #Allowing R to use maximum memory

#Loading required packages 
library(tidyverse)   #Visualize the data
library(Hmisc)       #imputing missing values
library(pastecs)     #Five number summary
library(plyr)        #Splitting data library(e1071)
library(dplyr)       #Data manipulation, (subset,select,apply,..and more)
library(summarytools) #provides tools to neatly sumarize data 

//////////////////////////////////////------- IMPORTING THE DATA --------////////////////////
#Reading csv file from the path
inputData <- read.csv("D:/Kavya/Data/daily_data_500_cus.csv")
df <- inputData #Keeping the dataset in df for modifications


/////////////////////////////////////------- VIEWING THE DATA --------/////////////////////
dim(df)   #Number of columns and rows in dataset
str(df)   #Structure of data 
head(df)  #Display the first five rows of dataset

######### ---- Separating Numeric and Categporicla columns form the dataset ----######
#Identifying the numeric columns from data 
nums  <- df %>% select(-contains("FLAG"), -contains("ID"))  #Excluding ID columns from data
numeric_cols = subset(nums, select=-c(CUS_MOBILE_NO,CUS_VALUE_SEGMENT_DAILY))  #Excluding mobile number column from data which is not sueful for analysis

#Identifying the categorical columns from data
categorical_cols <- df %>% select(contains("FLAG"),  split(names(df),sapply(df, function(x) paste(class(x), collapse=" ")))$factor) 

///////////////////////////////////////////////////------ SUMMARY STATISTICS OF NUMERIC DATA ------////////////////////////////
#Descriptive Statistics of Numerical data using pastecs library
summ <- stat.desc(numeric_cols) 
#Transpose is the simplest way to reshape dataset, row names beocme variable(column) names
summary <- t(summ)

#Remoivng the statistics form stat.desc which are not considered here for quality assessments  
summary_stats <- subset(summary, select = -c(nbr.val, nbr.null, nbr.na, range, sum, SE.mean, CI.mean.0.95, coef.var))
#Writing the summary_statistics into csv file 
write.csv(summary_stats, "summary_numeric_cols.csv", row.names = FALSE)

/////////////////////////////////////------- DATA CLEANING --------/////////////////////

//////////////////-------- NUMERIC VARIABLES  ------////////////////

#Identifying count of missing observations in numeric columns
which(is.na(numeric_cols)) #which funciton will return the position of the elements having missing values in data 

#Finding NA(missing values) percentage in the data
sum(is.na(df))/prod(dim(df))

#////Identifying OUTLIERS in Numerical variables from data
nums <- names(numeric_cols) #Keeping numeric_cols in x 
outlie <- function(x, num=3){
outlier <- x[x>mean(x)+3*sd(x) | x < mean(x) -3*sd(x)]
length(outlier)
}
outliers <-lapply(numeric_cols, function(x) if(is.numeric(x)) outlie(x))
#Getting extreme values for each Numerical variable in a dataframe outliers_count
outliers_count <- as.data.frame(do.call(rbind, outliers)) 

#///Identifying EXTREMES VALUES for Numerical variables from data
nums <- names(numeric_cols) #Keeping numeric_cols in x 
ext <- function(x, num=5){
	extreme <- x[x > mean(x)+ 5*sd(x) | x < mean(x) - 5*sd(x)]
	length(extreme)
}
extremes <-lapply(numeric_cols, function(x) if(is.numeric(x)) ext(x))
#Getting extreme values for each Numerical variable in a dataframe extremes_count
extremes_count <- as.data.frame(do.call(rbind, extremes))

#////////////////////------- CATEGORICAL VARIABLES ------///////////////////
#Identifying the categorical columns from data 
categorical<-select_if(df, is.factor)
sum(is.na(categorical))/prod(dim(categorical))

#Fore categorical variables calculating frequency, percentage frequency and cumulative frequency
cols <- names(categorical_cols)
#freq() generates a table of frequencies with counts and proportions
freq(df[cols], report.nas = FALSE, headings= FALSE) #if no need of % about missing data,use report.nas as FASLE 






