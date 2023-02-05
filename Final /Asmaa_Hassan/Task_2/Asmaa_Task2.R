################################################################################
################################################################################
#####################  Install packages & call libraries  ######################
################################################################################
################################################################################



install.packages("RVAideMemoire")
install.packages("car")
install.packages("corrplot")
install.packages("tidyverse")
install.packages("dplyr") 
install.packages("ggplot2") 
install.packages(c('tibble', 'dplyr', 'readr'))
install.packages("moments")
install.packages("datarium")
install.packages("qqplotr")


library(datarium)
library(qqplotr)
library(RVAideMemoire)
library(car)
library(corrplot)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(datarium)
library(readxl)
library(e1071)
library(moments)

################################################################################
################################################################################
##########################   Exploration of Data Set  ##########################
################################################################################
################################################################################

##Read the data and explore the top values
dataSet<- read_excel("GDP Indicators.xlsx")
head(dataSet)


##see the dataset varibles' type
str(dataSet)

##Check If the Dataset has missing values 
is.null(dataSet)

##Check The Outliers in the Numeric columns
boxplot(dataSet$`GDP growth`,dataSet$`Adjusted net national income`,
        dataSet$`Exports of goods and services`,dataSet$`Imports of goods and services`)

#Round the numerical columns in the dataset to two digits
dataSet$`GDP growth`=round(dataSet$`GDP growth`,digits = 2)
dataSet$`Adjusted net national income`=round(dataSet$`Adjusted net national income`,digits = 2)
dataSet$`Exports of goods and services`=round(dataSet$`Exports of goods and services`,digits = 2)
dataSet$`Imports of goods and services`=round(dataSet$`Imports of goods and services`,digits = 2)
dataSet

#Sort the coulmn with outliers to check if the outlier is Natural variation
sort(dataSet$`GDP growth`,decreasing = TRUE)
sort(dataSet$`Adjusted net national income`,decreasing = TRUE)
sort(dataSet$`Exports of goods and services`,decreasing = TRUE)
sort(dataSet$`Imports of goods and services`,decreasing = TRUE)

################################################################################
################################################################################
##############   Comprehensive Descriptive Statistical Analysis   ##############
################################################################################
################################################################################

##Group the countries to developed and developing countries
dataSet=dataSet %>% mutate(CountryStatus = 
                             case_when( 
                                      Country == "United States" ~ "Developed",
                                      Country == "France" ~ "Developed",
                                      Country == "Germany" ~ "Developed",
                                      Country == "Switzerland" ~ "Developed",
                                      Country == "Netherlands" ~ "Developed",
                                      Country == "Japan" ~ "Developed",
                                      Country == "Egypt, Arab Rep." ~ "Developing",
                                      Country == "Brazil" ~ "Developing",
                                      Country == "South Africa" ~ "Developing",
                                      Country == "Rwanda" ~ "Developing",
                                      Country == "Saudi Arabia" ~ "Developing",
                                      Country == "India" ~ "Developing"))


##Show dataset after grouping the countries into developed and developing countries
head(dataSet)

#Calculate the mean for GDP growth,net national income ,Exports of goods and services,
#and Imports of goods and services 
#the calculation based on developed countries and developing countries as group

##Calculate the mean based on the Country status
aggregate(dataSet[, 3:6], list(dataSet$'CountryStatus'), mean)

#Calculate the Median for GDP growth,net national income ,Exports of goods and services,
#and Imports of goods and services 
#the calculation based on developed countries and developing countries as group

##Calculate the median based on the Country status
aggregate(dataSet[, 3:6], list(dataSet$'CountryStatus'), median)

#Calculate the Mode for GDP growth,net national income ,Exports of goods and services,
#and Imports of goods and services

##Calculate the mode based on the Country status
Mode <- function(dataSet) {
  ux <- unique(dataSet)
  ux[which.max(tabulate(match(dataSet, ux)))]
}
aggregate(dataSet[, 3:6], list(dataSet$'CountryStatus'), Mode)


#Calculate the Standard deviation for GDP growth,net national income ,Exports of goods and services,
#and Imports of goods and services 

##Calculate the Standard deviation based on the Country status
aggregate(dataSet[,3:6],list(dataSet$'CountryStatus'), sd)


#calculate skewness

#A skewness value greater than 1 or less than -1 indicates a highly skewed distribution. 
#A value between 0.5 and 1 or -0.5 and -1 is moderately skewed.
#A value between -0.5 and 0.5 indicates that the distribution is fairly symmetrical

#Calculate skewness
aggregate(dataSet[,3:6],list(dataSet$'CountryStatus'), skewness)

#calculate kurtosis
#A kurtosis greater than three will indicate Positive Kurtosis.
#a kurtosis less than three will mean a negative kurtosis. 

#Calculate kurtosis
aggregate(dataSet[,3:6],list(dataSet$'CountryStatus'), kurtosis)

###############################################################################
checkPointData<-dataSet
################################################################################
################################################################################
############################  Correlation Analysis   ###########################
################################################################################
################################################################################
#creating new variables for developed and developing countries
#developed countries:
dataSetDeveloped<-subset(dataSet,CountryStatus=='Developed')
#developing countries
dataSetDeveloping<-subset(dataSet,CountryStatus=='Developing')

#Select the Numerical columns developed countries
dataSetDevelopedNum = subset(dataSetDeveloped, select = -c(Country,Time,CountryStatus) )
str(dataSetDevelopedNum)

#Select the Numerical columns developing countries
dataSetDevelopingNum = subset(dataSetDeveloping, select = -c(Country,Time,CountryStatus) )
str(dataSetDevelopingNum)


# Pearson correlation between 2 variables for developed countries
#The Pearson correlation is computed by default with the cor() function. 
cor(dataSetDevelopedNum$`GDP growth`, dataSetDevelopedNum$`Adjusted net national income`)

# Pearson correlation between 2 variables for developing countries
#The Pearson correlation is computed by default with the cor() function. 
cor(dataSetDevelopingNum$`GDP growth`, dataSetDevelopingNum$`Adjusted net national income`)


#Correlation Matrix: Correlations for All Variables in developed countries
# correlation for all variables and rounded to 2 decimals
round(cor(dataSetDevelopedNum), digits = 2)

#Correlation Matrix: Correlations for All Variables in developing countries
# correlation for all variables and rounded to 2 decimals
round(cor(dataSetDevelopingNum), digits = 2)


# You will use library(corrplot) for this task
# type = "upper" shows only upper side of the matrix

# Improved correlation matrix for developed countries
corrplot(cor(dataSetDevelopedNum), method = "number", type = "upper")

# Improved correlation matrix for developing countries
corrplot(cor(dataSetDevelopingNum), method = "number", type = "upper")

################################################################################
################################################################################
############################   Normality of Data    ###########################
################################################################################
################################################################################

# and one country  ????????
############################    Normality of Data    ###########################
##check the normality for the data
#Q-Q plot (Visual Method)
dad<-dataSet
dataSet<-dad
# Visually confirm if the plot appears to be on a normal curve
ggplot(mapping = aes(sample=dataSet$`GDP growth`)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("GDP growth") + 
  ylab("Sample")


ggplot(mapping = aes(sample = dataSet$`Adjusted net national income`)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("national income") + 
  ylab("Sample")

ggplot(mapping = aes(sample = dataSet$`Imports of goods and services`)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Imports of goods and services") + 
  ylab("Sample")


####################
##Shapiro-Wilk Test (Statistical Test)
shapiro.test(dataSet$`GDP growth`)
shapiro.test(dataSet$`Adjusted net national income`)
shapiro.test(dataSet$`Imports of goods and services`)

##############################################################
## For Report display

#create histogram for original distribution for th data before normlize 
hist(dataSet$`GDP growth`, col='steelblue', main='Original')
hist(dataSet$`Adjusted net national income`, col='steelblue', main='Original')
hist(dataSet$`Imports of goods and services`, col='steelblue', main='Original')


################################################
#Handle Non-Normal Data
#################################################
# Distribution of `GDP growth`variable
install.packages("ggpubr")
library(ggpubr)


#UsingCube Root Transformation: Transform the values from x to x1/3

# Normalize the numerical column by using the cube method

dataSet$`GDP growth`<-(dataSet$`GDP growth`)^(1/3)
dataSet$`Adjusted net national income`<-(dataSet$`Adjusted net national income`)^(1/3)
dataSet$`Imports of goods and services`<-(dataSet$`Imports of goods and services`)^(1/3)

#check the p-value after normalize the data
shapiro.test(dataSet$`GDP growth`)
shapiro.test(dataSet$`Adjusted net national income`)
shapiro.test(dataSet$`Imports of goods and services`)




##############################################################
## for report display

#create histogram for Cube-transformed distribution 
hist(dataSet$`GDP growth`, col='coral2', main='Transformed')
hist(dataSet$`Adjusted net national income`, col='coral2', main='Transformed')
hist(dataSet$`Imports of goods and services`, col='coral2', main='Transformed')


################################################################################
#save a copy of normlise data
checkpointNormnalData<-dataSet
################################################################################
################################################################################
############################   Regression Analysis   ###########################
################################################################################
################################################################################
##############################################################################

#Load the data after normalize and extract the numeral variables 
dataSetReg<-dataSet
head(dataSetReg)

#developed countries:
dataSetRegDeveloped<-subset(dataSetReg,CountryStatus=='Developed')
#developing countries
dataSetRegDeveloping<-subset(dataSetReg,CountryStatus=='Developing')



#Step 1:check the variables and data types for each variable 
str(dataSetReg)

#Step 2: Define the objective of the regression analysis
#subset the numerical variables for developed countries
dataSetRegDeveloped_reduced<-dataSetRegDeveloped[ ,c('GDP growth','Exports of goods and services',
                                   'Imports of goods and services','Adjusted net national income')]

#subset the numerical variables for developing countries
dataSetRegDeveloping_reduced<-dataSetRegDeveloping[ ,c('GDP growth','Exports of goods and services',
                                                     'Imports of goods and services','Adjusted net national income')]



#Then a correlation matrix between all variables in developed countries
cor(dataSetRegDeveloped_reduced,use="complete.obs")
#Then a correlation matrix between all variables in developing countries
cor(dataSetRegDeveloping_reduced,use="complete.obs")

#visualize a correlation matrix for developed countries
corrplot(cor(dataSetRegDeveloped_reduced,use="complete.obs"))

#visualize a correlation matrix for developing countries
corrplot(cor(dataSetRegDeveloping_reduced,use="complete.obs"))


#Step 3:Perform the linear regression analysis for devloped countries
#use lm function 
#fit the model
#To write the formula option:
#Y = GDP growth
#X = Adjusted net national income

#developed countries
model_Developed_1 <-lm(dataSetRegDeveloped_reduced$`GDP growth` ~ dataSetRegDeveloped_reduced$`Adjusted net national income`,
                       dataSetRegDeveloped_reduced)
summary.lm(model_Developed_1)

# formela 
############GDP growth =  0.68877  +0.37386 × djusted net national income


#Step 3:Perform the linear regression analysis for developing countries
#use lm function 
#fit the model
#To write the formula option:
#Y = GDP growth
#X = Adjusted net national income

#developing countries
model_Developing_1 <-lm(dataSetRegDeveloping_reduced$`GDP growth` ~ dataSetRegDeveloping_reduced$`Adjusted net national income`,
                        dataSetRegDeveloping_reduced)
summary.lm(model_Developing_1)
# formela 
############GDP growth =  0.73998   +0.73998  × djusted net national income

#to visualize the fitted regression line first we should draw the scatter plot , for developed countries
plot(dataSetRegDeveloped_reduced$`GDP growth` ~ dataSetRegDeveloped_reduced$`Adjusted net national income`,
     dataSetRegDeveloped_reduced,col = "blue",
     main = "Regression:Developed countries",
     xlab = "GDP growth",
     ylab = "National Income growth")

#adding the regression line to the plot
abline(model_Developed_1, col="red")

#to visualize the fitted regression line first we should draw the scatter plot , for developing countries
plot(dataSetRegDeveloping_reduced$`GDP growth` ~ dataSetRegDeveloping_reduced$`Adjusted net national income`, 
     dataSetRegDeveloping_reduced,col = "blue",
     main = "Regression:Developing countries",
     xlab = "GDP growth",
     ylab = "National Income growth")

#adding the regression line to the plot
abline(model_Developing_1, col="red")


#Step 4: Make sure the fitted model meets SLR assumptions
#check the following 4 assumptions

#1. Linearity
#The relationship between X and Y must be linear.
#Check this assumption by examining a scatter plot of x and y
#the variables have a linear relationship

#2. Residuals’ Independence for developed countries
plot(model_Developed_1, 1)

#2. Residuals’ Independence for developing countries
plot(model_Developing_1, 1)

#3. Normality of residuals for developed countries
plot(model_Developed_1, 2)

#3. Normality of residuals for developing countries
plot(model_Developing_1, 2)

#4. Equal variances of the residuals (Homoscedasticity) for developed countries
plot(model_Developed_1, 3)

#4. Equal variances of the residuals (Homoscedasticity) for developing countries
plot(model_Developing_1, 3)


#Step 5: Report the result for developed countries
#All 4 assumptions were approved, and we can confirm our fitted regression line is:

#GDP growth =  0.65823+ 0.45248× djusted net national income.


#Step 5: Report the result for developing countries
#All 4 assumptions were approved, and we can confirm our fitted regression line is:

#GDP growth =  0.75083 + 0.53283 × djusted net national income.


################################################################################
#Section 2: Multiple Linear Regression (MLR)
#Step 1: Load the data into R and briefly look at the data:
#Load the data
dataSetReg<- checkpointNormnalData
head(dataSetReg2)

#Step 2: Define the objective of the regression analysis
dataSetReg_reduced<-dataSetReg[ ,c('Time', 'GDP growth', 'Adjusted net national income','Imports of goods and services','Exports of goods and services')]

#Then a correlation matrix between all variables
cor(dataSetReg_reduced,use="complete.obs")

#visualize a correlation matrix
corrplot(cor(dataSetReg_reduced,use="complete.obs"))

#Step 3: Perform the linear regression analysis
#To write the formula option:
#Y = GDP growth
#X1 = national Income
#X2= Imports of goods and services
model_2 <-lm(dataSetReg_reduced$'GDP growth' ~ dataSetReg_reduced$`Adjusted net national income` +
               dataSetReg_reduced$`Imports of goods and services`, dataSetReg_reduced)
summary.lm(model_2)

#To write the formula option:
#Y = GDP growth
#X1 = national Income
#X2= Imports of goods and services
#X3=Exports of goods and services
model_3 <-lm(dataSetReg_reduced$`GDP growth` ~ dataSetReg_reduced$`Adjusted net national income` + 
               dataSetReg_reduced$`Imports of goods and services` +dataSetReg_reduced$`Exports of goods and services`,
               dataSetReg_reduced)
summary.lm(model_3)


#To write the formula option:
#Y = GDP growth
#X1 = national Income
#X2= Imports of goods and services
#X3=Exports of goods and services
#X4= Time
model_4 <-lm(dataSetReg_reduced$`GDP growth` ~ dataSetReg_reduced$`Adjusted net national income` + 
               dataSetReg_reduced$`Imports of goods and services` +dataSetReg_reduced$`Exports of goods and services`+
               dataSetReg_reduced$Time, dataSetReg_reduced)
summary.lm(model_4)

#Step 4: Make sure the fitted model meets MLR assumption
#1. Linearity:
data.frame(colnames(dataSetReg_reduced))
dataSetReg_reduced$`GDP growth` ~ dataSetReg_reduced$`Adjusted net national income` + 
  dataSetReg_reduced$`Imports of goods and services` +dataSetReg_reduced$`Exports of goods and services`+dataSetReg_reduced$Time
pairs(dataSetReg_reduced[,c(2,3,5,4,1)], lower.panel = NULL, pch = 19,cex = 0.2)

#2.Residuals’ Independence
plot(model_4, 1)

#3.Normality of residuals
plot(model_4, 2)

#4.Equal variances of the residuals (Homoscedasticit)
plot(model_4, 3)

#5. No multicollinearity
library(car)
vif(model_4)

#Step 5: Report the results
#GDP growth = 9.856770  + 0.539939 × Adjusted net national income+
#                   0.044978 x Imports of goods and services + 0.011646 xExports of goods and services - 0.004670 x Time

################################################################################
################################################################################
############################  Time Series Analysis   ###########################
################################################################################
################################################################################

##2.5.2 Holt-Winters Exponential Smoothing (additive model with increasing or decreasing trend and no seasonalit
#load packeges 
install.packages("TTR")
install.packages("forecast")

#call library
library("TTR")
library("forecast")
dataSetTime<-checkPointData
#Reading Time Series Data
head(dataSetTime)
dataSetTime
#choose one developed  country for time series Germany
dataSetGermany<-subset(dataSetTime,Country=='Germany')
dataSetGermany

#choose one developing  country for time series Egypt, Arab Rep.
dataSetEgypt<-subset(dataSetTime,Country=='Egypt, Arab Rep.')
dataSetEgypt

#This step is to store the data in a time series object in R
#First for GDP

#Germany
dataSetTimeSeriesGermany<- ts(dataSetGermany$`GDP growth`,start = 2001)
dataSetTimeSeriesGermany

#Egypt
dataSetTimeSeriesEgypt<- ts(dataSetEgypt$`GDP growth`,start = 2001)
dataSetTimeSeriesEgypt

#Plotting Time Series 
#GDP
#Germany
plot.ts(dataSetTimeSeriesGermany)

#Egypt
plot.ts(dataSetTimeSeriesEgypt)

#####  Decomposing Non-Seasonal Data ####
#library("TTR")
# Apply SMA for Germany
dataSetTimeSeriesGermanySMA3 <- SMA(dataSetTimeSeriesGermany,n=3)
#plot the smoothed Time Series
plot.ts(dataSetTimeSeriesGermanySMA3)


# Apply SMA for Egypt
dataSetTimeSeriesEgyptSMA3 <- SMA(dataSetTimeSeriesEgypt,n=3)
#plot the smoothed Time Series
plot.ts(dataSetTimeSeriesEgyptSMA3)


#Forecasts using Exponential Smoothing
#Holt winter for Germany
dataSetTimeSeriesGermanyforecasts <- HoltWinters(dataSetTimeSeriesGermany, gamma=FALSE,start = 2001)
dataSetTimeSeriesGermanyforecasts

#Holt winter for Egypt
dataSetTimeSeriesEgyptforecasts <- HoltWinters(dataSetTimeSeriesEgypt, gamma=FALSE,start = 2001)
dataSetTimeSeriesEgyptforecasts

#plot the original time series against the forecasts
#for Germany
plot(dataSetTimeSeriesGermanyforecasts)

#for Egypt
plot(dataSetTimeSeriesEgyptforecasts)

#fitted variable Germany
dataSetTimeSeriesGermanyforecasts$fitted

#fitted variable Egypt
dataSetTimeSeriesEgyptforecasts$fitted

#calculate the sum of squared errors for the in-sample forecasterrors
#Germany
dataSetTimeSeriesGermanyforecasts$SSE

#Egypt
dataSetTimeSeriesEgyptforecasts$SSE


#Holt winters
#specify the initial values of the level and the slope b of the trend component
#Germany
HoltWinters(dataSetTimeSeriesGermany, gamma=FALSE, l.start=-2.08, b.start=-1.88)

#Egypt
HoltWinters(dataSetTimeSeriesEgypt, gamma=FALSE, l.start=1.24, b.start=-1.15)

#
library("forecast")
#Forecast for Germany
dataSetTimeSeriesGermanyforecasts2 <- forecast(dataSetTimeSeriesGermanyforecasts, h=10,start = 2001)
dataSetTimeSeriesGermanyforecasts2

#Forecast for Egypt
dataSetTimeSeriesEgyptforecasts2 <- forecast(dataSetTimeSeriesEgyptforecasts, h=10,start = 2001)
dataSetTimeSeriesEgyptforecasts2

#show the forecast for Germany
plot(dataSetTimeSeriesGermanyforecasts2)

#show the forecast for Egypt
plot(dataSetTimeSeriesEgyptforecasts2)

#make a correlogram 
#Germany
acf(dataSetTimeSeriesGermanyforecasts2$residuals, lag.max=20 , na.action = na.pass)
#The Ljung-Box test for Germany
Box.test(dataSetTimeSeriesGermanyforecasts2$residuals, lag=16, type="Ljung-Box")

#Egypt
acf(dataSetTimeSeriesEgyptforecasts2$residuals, lag.max=20 , na.action = na.pass)
#The Ljung-Box test for Egypt
Box.test(dataSetTimeSeriesEgyptforecasts2$residuals, lag=16, type="Ljung-Box")


#Forecast Errors function
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

#plot.ts Germany
plot.ts(dataSetTimeSeriesGermanyforecasts2$residuals)

#Germany
#Finding missing values
dataSetTimeSeriesGermanyforecasts2$residuals <-
  dataSetTimeSeriesGermanyforecasts2$residuals[!is.na(dataSetTimeSeriesGermanyforecasts2$residuals)]
#plotForecastErrors
plotForecastErrors(dataSetTimeSeriesGermanyforecasts2$residuals)

#plot.ts Egypt
plot.ts(dataSetTimeSeriesEgyptforecasts2$residuals)


#Egypt
#Finding missing values
dataSetTimeSeriesEgyptforecasts2$residuals <-
  dataSetTimeSeriesEgyptforecasts2$residuals[!is.na(dataSetTimeSeriesEgyptforecasts2$residuals)]
#plotForecastErrors
plotForecastErrors(dataSetTimeSeriesEgyptforecasts2$residuals)




################################################################################
################################################################################
############################  Comparative Analysis  ###########################
################################################################################
################################################################################


#Assessing the Normality of Data
#read the data
dataSethypo<-checkpointNormnalData
head(dataSethypo)

# Visually confirm if the plot appears to be on a normal curve
ggplot(mapping = aes(sample=dataSethypo$`GDP growth`)) +
  stat_qq_point(size = 2,color = "blue")+
  stat_qq_line(color="orange") +xlab("Theoretical") + ylab("Sample")

#Shapiro-Wilk Test (Statistical Test) to confirm  with numbers
shapiro.test(dataSethypo$`GDP growth`)
shapiro.test(dataSethypo$`Adjusted net national income`)
shapiro.test(dataSethypo$`Imports of goods and services`)


#read the dataset
dataSethypo

#check the outliers
boxplot(dataSethypo$`GDP growth`,dataSethypo$`Adjusted net national income`,dataSethypo$`Imports of goods and services`)


# Lets inspect the Data Frame
head(dataSethypo)
#
summary(dataSethypo)
#Double check the number of indecators  in the sample
dim(dataSethypo)
## Plot the distribution of the GDP growth
hist(dataSethypo$`GDP growth`)
#check the mean and standard deviation
mean(as.numeric(dataSethypo$`GDP growth`),na.rm=TRUE)
sd(as.numeric(dataSethypo$`GDP growth`),na.rm=TRUE)

#one-tailed hypothesis test
#H0 GDP is always greater than 0% 
#if p value is great or equal to 0.05 we accepet this hyposs


#one-tailed hypothesis test
t.test(dataSethypo$`GDP growth`, mu=0, alternative="less")

#boxplot GDP
boxplot(dataSethypo$`GDP growth`)

#Independent Two Sample T-Test
#add a new column showing the export growth in each country and spliting it by 
dataSethypo=dataSethypo %>% mutate(ExportStatus = 
                                     case_when( 
                                       dataSet$`Exports of goods and services`  >7 ~ "High",
                                       dataSet$`Exports of goods and services`  < 2 ~ "Low",
                                       TRUE ~ "Meduim")
                                   
)

#Null Hypothesis is 𝐻0: GDP Growth Developed= GDP Growth Developing
#Alternative Hypothesis is:𝐻1:GDP Growth Developed ≠ GDP Growth Developing


# convert country status status into  factor
dataSethypo$CountryStatus <- as.factor(dataSethypo$CountryStatus)

# convert export status into  factor
dataSethypo$ExportStatus<-as.factor(dataSethypo$ExportStatus)

# Use boxplot to compare developing  and developed countries
boxplot(dataSethypo$`GDP growth` ~ dataSethypo$CountryStatus, data=dataSethypo, names=c("Developed", "Developing"),
        xlab="Developed or Developing", ylab="GDP Growth",
        main="GDP Growth For Developed & Developing Countries")

# we can now use the two sample t test using the same function
t.test(dataSethypo$`GDP growth` ~ dataSethypo$CountryStatus, dataSethypo)


###################################################################################
#ANOVA
#read the data
dataSethypo<-checkpointNormnalData
head(dataSethypo)

#add a new column showing the export growth in each country and spliting it by 
dataSethypo=dataSethypo %>% mutate(ExportStatus = 
                                     case_when( 
                                       dataSet$`Exports of goods and services`  >7 ~ "High",
                                       dataSet$`Exports of goods and services`  < 2 ~ "Low",
                                       TRUE ~ "Meduim")
                                   
)

#Show the export growth situation
barplot(prop.table(table(dataSethypo$ExportStatus)),
        col = rainbow(3),
        ylim = c(0, 0.7),
        main = "Export Distribution")

# check the outlier
boxplot(dataSethypo$`GDP growth`)

#check the GDP is normally distributed
shapiro.test(dataSethypo$`GDP growth`)


#checked Variances 
bartlett.test(dataSethypo$`GDP growth` ~ dataSethypo$ExportStatus, data=dataSethypo)

#One way ANOVA test
oneway.test(dataSethypo$`GDP growth` ~ dataSethypo$ExportStatus,data=dataSethypo, var.equal = TRUE)













