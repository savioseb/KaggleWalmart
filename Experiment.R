## Libraries used in the Project
library(ggplot2)
library(reshape2)
library(scales)
library(lubridate)
library(e1071)
## to be able to plot in grids
library(grid)
## to be able to plot in grids
library(gridExtra)

options(scipen=10000)

## Loading the datasets
train <- read.csv("Data/train.csv")
stores <- read.csv("Data/stores.csv")
features <- read.csv("Data/features.csv")
test <- read.csv("Data/test.csv")

## Fixing the Data
train$Date <- as.Date(train$Date)
features$Date <- as.Date(features$Date)
test$Date <- as.Date( test$Date)


############################################
## Analyzing features
############################################
summary( features )


############# FEATURES: MARKDOWN 1

## to understand where these missing data lie, we will plot a
## heat map
ggplot( features , aes(x = Date, y = Store)) + 
  geom_tile(aes(fill = MarkDown1)) +
  scale_fill_gradient(low="yellow", high="red" , labels = comma , name="Mark Down1") +
  scale_y_continuous(name="Store")


############# FEATURES : CPI

meanCPIAcrossStores <- tapply( features$CPI , features$Date , FUN = mean )
meanCPIAcrossStoresDF <- data.frame( meanCPIAcrossStores )
meanCPIAcrossStoresDF$Date <- as.Date( rownames( meanCPIAcrossStoresDF ) )

## heatmap to undersatnd where CPI is missing
missingCPIHeatMap <- ggplot( features , aes(x = Date, y = Store)) + 
  geom_tile(aes(fill = CPI)) +
  scale_fill_gradient(low="yellow", high="red" , labels = comma , name="CPI") +
  scale_y_continuous(name="Store" ) +
  theme( legend.position = "bottom" )
## trend of Average CPI Across All Stores
avgCPiIndexTrend <- ggplot( meanCPIAcrossStoresDF , aes(x = Date, y = meanCPIAcrossStores ) ) + 
  geom_line() +
  scale_y_continuous(name="MEAN CPI Across All Stores" ) +
  stat_smooth( method = "lm" )
grid.arrange(missingCPIHeatMap , avgCPiIndexTrend , ncol = 1 )



## CPI MEAN, MAX MIN & Range tabulated into a Data Frame
CPI_Mean <- tapply( features$CPI , features$Store , FUN = mean , na.rm= T )
CPI_Max <- tapply( features$CPI , features$Store , FUN = max , na.rm= T )
CPI_Min <- tapply( features$CPI , features$Store , FUN = min , na.rm= T )
CPI_DF <- data.frame( CPI_Mean , CPI_Max , CPI_Min )
CPI_DF$Range <- CPI_DF$CPI_Max - CPI_DF$CPI_Min 
## Checking if there is a difference between Max and Min
## - if no change then no rows
CPI_DF[ CPI_DF$Range != 0 ,]


## Removing CPI Columns not needed anymore
CPI_DF$CPI_Min = CPI_DF$CPI_Max = CPI_DF$Range = NULL
## Creating Store Column with the RowNAME
CPI_DF$Store <- as.integer( rownames( CPI_DF ) )
## Imputing CPI
## merging the dataframes by Store
features1 <- merge( features , CPI_DF , by="Store" )
## removing old CPI Column
features1$CPI = NULL
## Renaming CPI_Mean to CPI
colnames( features1 )[12] <- "CPI"

## checking if the imputation was done correctly
ggplot( features1 , aes(x = Date, y = Store)) + 
  geom_tile(aes(fill = CPI)) +
  scale_fill_gradient(low="yellow", high="red" , labels = comma , name="CPI") +
  scale_y_continuous(name="Store")

## overwriting old features with features1 (with no missing values for CPI)
features <- features1
## removing unneccesary data elements
rm( CPI_DF , CPI_Max , CPI_Mean , CPI_Min , features1 )


ggplot( features , aes(x = Date, y = Store)) + 
  geom_tile(aes(fill = )) +
  scale_fill_gradient(low="yellow", high="red" , labels = comma , name="CPI") +
  scale_y_continuous(name="Store")



############# FEATURES : Unemployment
## heatmap to undersatnd where Unemployment is missing
oldUnemploymentData <- ggplot( features , aes(x = Date, y = Store)) + 
  geom_tile(aes(fill = Unemployment)) +
  scale_fill_gradient(low="yellow", high="red" , labels = comma , name="Unemployment") +
  scale_y_continuous(name="Store")

## Unemployment MEAN, MAX MIN & Range tabulated into a Data Frame
Unemployment_Mean <- 
  tapply( features$Unemployment , features$Store , FUN = mean , na.rm = T )
Unemployment_Max <- 
  tapply( features$Unemployment , features$Store , FUN = max , na.rm = T )
Unemployment_Min <- 
  tapply( features$Unemployment , features$Store , FUN = min , na.rm = T )
Unemployment_DF <- 
  data.frame( Unemployment_Mean , Unemployment_Max , Unemployment_Min )
Unemployment_DF$Range <- Unemployment_DF$Unemployment_Max - Unemployment_DF$Unemployment_Min 
## Checking if there is a difference between Max and Min
## - if no change then no rows
Unemployment_DF[ Unemployment_DF$Range > 0 ,]


## Removing CPI Columns not needed anymore
CPI_DF$CPI_Min = CPI_DF$CPI_Max = CPI_DF$Range = NULL
## Creating Store Column with the RowNAME
CPI_DF$Store <- as.integer( rownames( CPI_DF ) )
## Imputing CPI
## merging the dataframes by Store
features1 <- merge( features , CPI_DF , by="Store" )
## removing old CPI Column
features1$CPI = NULL
## Renaming CPI_Mean to CPI
colnames( features1 )[12] <- "CPI"

## checking if the imputation was done correctly
ggplot( features1 , aes(x = Date, y = Store)) + 
  geom_tile(aes(fill = CPI)) +
  scale_fill_gradient(low="yellow", high="red" , labels = comma , name="CPI") +
  scale_y_continuous(name="Store")

## overwriting old features with features1 (with no missing values for CPI)
features <- features1
## removing unneccesary data elements
rm( CPI_DF , CPI_Max , CPI_Mean , CPI_Min , features1 )


ggplot( features , aes(x = Date, y = Store)) + 
  geom_tile(aes(fill = )) +
  scale_fill_gradient(low="yellow", high="red" , labels = comma , name="CPI") +
  scale_y_continuous(name="Store")









###################################################################
#################################################################
############### MERGING DATASETS #######################
################################################################

## Merging the datasets
trainStoresMerge <- merge(train , stores , by = "Store")
trainStoresFeaturesMerge <- merge( trainStoresMerge , features , by = c( "Store" , "Date" ) )
colnames(trainStoresFeaturesMerge)[5] <- "IsHoliday"
trainStoresFeaturesMerge$IsHoliday.y <- NULL
testStoresMerge <- merge(test , stores , by = "Store")
testStoresFeaturesMerge <- merge( testStoresMerge , features , by = c( "Store" , "Date" ) )
colnames(testStoresFeaturesMerge)[5] <- "IsHoliday"
testStoresFeaturesMerge$IsHoliday.y <- NULL


## removing intermediate datasets
rm(trainStoresMerge )
rm( testStoresMerge )


# unique store department sales
#-----------------------------------------------------------------------------
## running the sum function for each store & department
storeDeptTotalSales <- tapply( 
  trainStoresFeaturesMerge$Weekly_Sales , 
  trainStoresFeaturesMerge[, c("Store","Dept")]  , 
  FUN = sum )
## Converting the matrix to a dataframe
storeDeptTotalSalesDataFrame <- as.data.frame( storeDeptTotalSales )
## Setting the Store Number into the table so we can analyze it further
storeDeptTotalSalesDataFrame$Store <- as.integer( rownames( storeDeptTotalSalesDataFrame ) )
## Move Store to the 1st column in the dataframe
storeDeptTotalSalesDataFrame <- storeDeptTotalSalesDataFrame[ , c(ncol(storeDeptTotalSalesDataFrame) , 1:ncol(storeDeptTotalSalesDataFrame)-1 )]
## Melting the columns into rows to enable analysis
storeDeptTotalSalesDataFrame <- melt(storeDeptTotalSalesDataFrame , id="Store" )
## removing the NA variables - where the department does not exist in a store
storeDeptTotalSalesDataFrame <- storeDeptTotalSalesDataFrame[ complete.cases(storeDeptTotalSalesDataFrame),]
## Renaming the Columns in the Dataframe
colnames( storeDeptTotalSalesDataFrame )[2:3] <- c("Dept" , "TotalSales" )
## Changing the Dept Type from String to Numeric
storeDeptTotalSalesDataFrame$Dept <- as.integer(storeDeptTotalSalesDataFrame$Dept)
## Freeing Memory - Removing the intermediate Matrix
rm(storeDeptTotalSales)

summary( storeDeptTotalSalesDataFrame)

## Generating a Heatmap of the Department's Total Sales in each of the 45 stores
ggplot( storeDeptTotalSalesDataFrame , aes(x = Store, y = Dept)) + 
  geom_tile(aes(fill = TotalSales)) +
  scale_fill_gradient(low="yellow", high="red" , labels = comma , name="Total Sales") +
  scale_y_continuous(name="Department")

## removing dataframe to free up memory
rm( storeDeptTotalSalesDataFrame )


## Total Sales vs. Store Size - plotting the relationship
## -------------------------------------------------------------------
StoreTotalSales <- 
  tapply(
    trainStoresFeaturesMerge$Weekly_Sales, 
    trainStoresFeaturesMerge$Store, 
    FUN = sum)
## converting the table to a DataFrame
stores$TotalSales <- StoreTotalSales
stores$TotalSalesInMillion <- stores$TotalSales/1000000
## Plotting the Total Sales vs. Store Size
ggplot( stores , aes(x=Size , y=TotalSalesInMillion , color = Type ) ) + 
  geom_point( size=3) +  
  scale_y_continuous(name="Total Sales in Millions" ) + 
  scale_color_brewer(palette = "Dark2", name="Store Type" )
  
## box plot to show the summary statistics of the Type of Stores
ggplot(data=stores, 
       aes(x=Type, y=TotalSalesInMillion, fill=Type) ) + 
  geom_boxplot(outlier.shape = 15, outlier.size = 4) +
  ## to show how the individual store sales are distributed
  geom_jitter() +
  scale_y_continuous(name="Total Sales in Millions" ) +
  scale_fill_brewer(name = "Store Type" , palette = "Dark2")

## calculating the summary statistics for each Type
tabledTypeWiseSummaryStatistics <- 
  tapply(stores$TotalSalesInMillion , stores$Type , summary)

## Changing the Labels of the tabled Summary Statistics for printing
attributes(tabledTypeWiseSummaryStatistics)$dimnames[[1]] <- 
  c( 
    "Type A Store Summary Statistics" , 
    "Type B Store Summary Statistics" , 
    "Type C Store Summary Statistics" )

## Printing Summary Statistics for each Type of Store (based on Total Sales)
tabledTypeWiseSummaryStatistics

rm(StoreTotalSales , tabledTypeWiseSummaryStatistics )



## Total Sales Per Week - Time Series
## ----------------------------------------------------------

# function to handle lag
lagpad <- function(x, k) {
  if( k > 0 ) {
    # It should actually be NA in the rep function but for my purposes 0 is acceptable
    c(rep(0, k), x)[1 : length(x)] 
  } else {
    # It should actually be NA in the rep function but for my purposes 0 is acceptable
    c(x[ (abs(k)+1) : length(x)] , rep(0, abs(k) ) )  
  }
}


## Running tapply with sum to find the total sales per week
totalSalesPerWeek <- tapply( trainStoresFeaturesMerge$Weekly_Sales , trainStoresFeaturesMerge$Date , FUN = sum )
## Converting table to Data Frame
totalSalesPerWeekDataFrame <- as.data.frame( totalSalesPerWeek )
## Converting date from String-Factor to Date Type
totalSalesPerWeekDataFrame$Date <- as.Date( rownames(totalSalesPerWeekDataFrame ) )
## Renaming the Column to "TotalSales"
colnames(totalSalesPerWeekDataFrame)[1] <- "TotalSales"
## Calculating the Total sales in Millions
totalSalesPerWeekDataFrame$TotalSalesInMillion = totalSalesPerWeekDataFrame$TotalSales/1000000

## Getting the holiday List
## Extracting the Holiday List
holidayDateTable <- table(trainStoresFeaturesMerge$Date , trainStoresFeaturesMerge$IsHoliday)
## Converting from Table to Data Frame
holidayDateTableDataFrame <- as.data.frame( holidayDateTable)
## Extracting the Holidays
holidayDateTableDataFrame <- subset( holidayDateTableDataFrame,holidayDateTableDataFrame$Var2==T)
## Marking the Holdiays in the dataset
holidayDateTableDataFrame$IsHoliday <- ifelse( holidayDateTableDataFrame$Freq >0 , 1 , 0 )
## Converting Date from String-Factor to Date Type
holidayDateTableDataFrame$Date <- as.Date( holidayDateTableDataFrame$Var1 )
# creating the holiday season lag - before and after the holiday week - 2 weeks
holidayDateTableDataFrame$Week1BeforeHoliday <- lagpad(holidayDateTableDataFrame$IsHoliday , -1)
holidayDateTableDataFrame$Week2BeforeHoliday <- lagpad(holidayDateTableDataFrame$IsHoliday , -2)
holidayDateTableDataFrame$Week1AfterHoliday <- lagpad(holidayDateTableDataFrame$IsHoliday , 1)
holidayDateTableDataFrame$Week2AfterHoliday <- lagpad(holidayDateTableDataFrame$IsHoliday , 2)
## Creating an ordered Holiday Season Type
holidayDateTableDataFrame$HolidaySeasonType = 
  ifelse( holidayDateTableDataFrame$Week1BeforeHoliday == 1 , "1 Week Before Holiday" ,
          ifelse( holidayDateTableDataFrame$Week2BeforeHoliday == 1 , "2 Weeks Before Holiday" ,
                  ifelse( holidayDateTableDataFrame$Week1AfterHoliday == 1 , "1 Week After Holiday" ,
                          ifelse( holidayDateTableDataFrame$Week2AfterHoliday == 1 , "2 Weeks After Holiday" ,
                                  ifelse( holidayDateTableDataFrame$IsHoliday == 1 , "Holiday Week" , "No Holiday" )))))
holidayDateTableDataFrame$HolidaySeasonType = factor(
  holidayDateTableDataFrame$HolidaySeasonType , 
  ordered=TRUE , 
  levels=c( 
    "No Holiday" ,
    "2 Weeks Before Holiday" ,
    "1 Week Before Holiday" ,
    "Holiday Week" ,
    "1 Week After Holiday" ,
    "2 Weeks After Holiday"
    )
  )



## Creating a variable to mark if it is a Holiday season - 
## that is 2 weeks before + holdiay week + 2 weeks after
holidayDateTableDataFrame$IsHolidaySeason <- 
  holidayDateTableDataFrame$IsHoliday +
  holidayDateTableDataFrame$Week1BeforeHoliday * .6 +
  holidayDateTableDataFrame$Week2BeforeHoliday *.2   +
  holidayDateTableDataFrame$Week1AfterHoliday * .6 +
  holidayDateTableDataFrame$Week2AfterHoliday *.2


## Removing unneccesary Columns
holidayDateTableDataFrame$Var1 = 
  holidayDateTableDataFrame$Var2 = 
  holidayDateTableDataFrame$Freq = NULL
## Clearing Memory - removing intermediate Tables
rm(holidayDateTable , totalSalesPerWeek)

## Mering the sales per week and holiday list 
totalSalesPerWeekDataFrame <- merge( totalSalesPerWeekDataFrame, holidayDateTableDataFrame , by=2)

# Plotting Sales Per Week
ggplot( totalSalesPerWeekDataFrame , aes(x=Date , y=TotalSalesInMillion , color = HolidaySeasonType ) ) + 
  geom_line( aes(group=1) , size=1) + 
  geom_point(size = 3) +
  scale_y_continuous(name="Total Sales in Millions" ) +
  scale_color_brewer(palette="Dark2") 


#####################################
# Creating separate holiday dummy variables
holidayDateTableDataFrame$IsHolidayDefined <- 
  ifelse( holidayDateTableDataFrame$IsHoliday == 1 & month( holidayDateTableDataFrame$Date ) == 2 ,
          2, ifelse( holidayDateTableDataFrame$IsHoliday == 1 & month( holidayDateTableDataFrame$Date ) == 9 ,
                     9 , ifelse( holidayDateTableDataFrame$IsHoliday == 1 & month( holidayDateTableDataFrame$Date ) == 11 ,
                                 11 , ifelse( holidayDateTableDataFrame$IsHoliday == 1 & month( holidayDateTableDataFrame$Date ) == 12 , 
                                              12 , 0 ) ) ) )
# creating the holiday season lag - before and after the holiday week - 2 weeks
holidayDateTableDataFrame$Week1BeforeHoliday <- lagpad(holidayDateTableDataFrame$IsHolidayDefined , -1)
holidayDateTableDataFrame$Week2BeforeHoliday <- lagpad(holidayDateTableDataFrame$IsHolidayDefined , -2)
holidayDateTableDataFrame$Week1AfterHoliday <- lagpad(holidayDateTableDataFrame$IsHolidayDefined , 1)
holidayDateTableDataFrame$Week2AfterHoliday <- lagpad(holidayDateTableDataFrame$IsHolidayDefined , 2)

holidayDateTableDataFrame$HolidaySeasonId <- as.factor(
  holidayDateTableDataFrame$Week1BeforeHoliday + 
  holidayDateTableDataFrame$Week2BeforeHoliday +
  holidayDateTableDataFrame$IsHolidayDefined +
  holidayDateTableDataFrame$Week1AfterHoliday +
  holidayDateTableDataFrame$Week2AfterHoliday )


## Creating an ordered Holiday Season Type for Super Bowl
holidayDateTableDataFrame$HolidaySeasonType <- 
  ifelse( holidayDateTableDataFrame$Week1BeforeHoliday == 2 , "1 Week Before Super Bowl" ,
          ifelse( holidayDateTableDataFrame$Week2BeforeHoliday == 2 , "2 Weeks Before Super Bowl" ,
                  ifelse( holidayDateTableDataFrame$Week1AfterHoliday == 2 , "1 Week After Super Bowl" ,
                          ifelse( holidayDateTableDataFrame$Week2AfterHoliday == 2 , "2 Weeks After Super Bowl" ,
                                  ifelse( holidayDateTableDataFrame$IsHolidayDefined == 2 , "Super Bowl" , "No Holiday" )))))

## Creating an ordered Holiday Season Type for Labor Day
holidayDateTableDataFrame$HolidaySeasonType <- 
  ifelse( holidayDateTableDataFrame$Week1BeforeHoliday == 9 , "1 Week Before Labor Day" ,
          ifelse( holidayDateTableDataFrame$Week2BeforeHoliday == 9 , "2 Weeks Before Labor Day" ,
                  ifelse( holidayDateTableDataFrame$Week1AfterHoliday == 9 , "1 Week After Labor Day" ,
                          ifelse( holidayDateTableDataFrame$Week2AfterHoliday == 9 , "2 Weeks After Labor Day" ,
                                  ifelse( holidayDateTableDataFrame$IsHolidayDefined == 9 , "Labor Day" , holidayDateTableDataFrame$HolidaySeasonType ) ) ) ) )

## Creating an ordered Holiday Season Type for Thanksgiving
holidayDateTableDataFrame$HolidaySeasonType <- 
  ifelse( holidayDateTableDataFrame$Week1BeforeHoliday == 11 , "1 Week Before Thanksgiving" ,
          ifelse( holidayDateTableDataFrame$Week2BeforeHoliday == 11 , "2 Weeks Before Thanksgiving" ,
                  ifelse( holidayDateTableDataFrame$Week1AfterHoliday == 11 , "1 Week After Thanksgiving" ,
                          ifelse( holidayDateTableDataFrame$Week2AfterHoliday == 11 , "2 Weeks After Thanksgiving" ,
                                  ifelse( holidayDateTableDataFrame$IsHolidayDefined == 11 , "Thanksgiving" , holidayDateTableDataFrame$HolidaySeasonType ) ) ) ) )

## Creating an ordered Holiday Season Type for Christmas
holidayDateTableDataFrame$HolidaySeasonType <- 
  ifelse( holidayDateTableDataFrame$Week1BeforeHoliday == 12 , "1 Week Before Christmas" ,
          ifelse( holidayDateTableDataFrame$Week2BeforeHoliday == 12 , "2 Weeks Before Christmas" ,
                  ifelse( holidayDateTableDataFrame$Week1AfterHoliday == 12 , "1 Week After Christmas" ,
                          ifelse( holidayDateTableDataFrame$Week2AfterHoliday == 12 , "2 Weeks After Christmas" ,
                                  ifelse( holidayDateTableDataFrame$IsHolidayDefined == 12 , "Christmas" , holidayDateTableDataFrame$HolidaySeasonType ) ) ) ) )

holidayDateTableDataFrame$HolidaySeasonType = factor(
  holidayDateTableDataFrame$HolidaySeasonType , 
  ordered=TRUE , 
  levels=c( 
    "No Holiday" ,
    "2 Weeks Before Super Bowl" ,
    "1 Week Before Super Bowl" ,
    "Super Bowl" ,
    "1 Week After Super Bowl" ,
    "2 Weeks After Super Bowl" ,
    "2 Weeks Before Labor Day" ,
    "1 Week Before Labor Day" ,
    "Labor Day" ,
    "1 Week After Labor Day" ,
    "2 Weeks After Labor Day" ,
    "2 Weeks Before Thanksgiving" ,
    "1 Week Before Thanksgiving" ,
    "Thanksgiving" ,
    "1 Week After Thanksgiving" ,
    "2 Weeks After Thanksgiving" ,
    "2 Weeks Before Christmas" ,
    "1 Week Before Christmas" ,
    "Christmas" ,
    "1 Week After Christmas" ,
    "2 Weeks After Christmas"
  )
)


## Mering the sales per week and holiday list 
totalSalesPerWeekDataFrame$HolidaySeasonType <- holidayDateTableDataFrame$HolidaySeasonType
## Merging the season Type with sales per week
totalSalesPerWeekDataFrame$HolidaySeasonId <- 
  holidayDateTableDataFrame$HolidaySeasonId

## Subsetting only the holidays
totalSalesPerWeekDataFrameDuringHolidays <- 
  subset( totalSalesPerWeekDataFrame , 
          totalSalesPerWeekDataFrame$Date >= '2010-08-27' & 
            totalSalesPerWeekDataFrame$Date <= '2011-02-25' )

## Plotting the subset of totalSalesPerWeekDataFrame
ggplot( totalSalesPerWeekDataFrameDuringHolidays , 
        aes(x=Date , y=TotalSalesInMillion , 
            color = HolidaySeasonId ) ) + 
  geom_line( aes(group=1) ) + 
  geom_point(size = 2) +
  scale_y_continuous(name="Total Sales in Millions" )  +
  scale_color_brewer(
    palette="Dark2" , 
    name = "Holiday Season" ,
    labels = c( 
      "No Holiday" , 
      "Super Bowl" ,
      "Labor Day" , 
      "Thanksgiving" ,
      "Christmas"
      )
    ) 

## removing the following columns because it may cause 
## multi-collinearity issues once merged with the main data and
## building a model with that
holidayDateTableDataFrame$Week1BeforeHoliday = NULL
holidayDateTableDataFrame$Week2BeforeHoliday = NULL
holidayDateTableDataFrame$Week1AfterHoliday = NULL
holidayDateTableDataFrame$Week2AfterHoliday = NULL
holidayDateTableDataFrame$IsHoliday = NULL
holidayDateTableDataFrame$IsHolidayDefined = NULL

rm( totalSalesPerWeekDataFrame , totalSalesPerWeekDataFrameDuringHolidays , trainStoresMerge )


#################################################################
## 3.3.4 Store-Department-wise Sales per Week - Time Series
################################################################
ggplot(trainStoresFeaturesMerge , 
       aes(x=Date , y = Weekly_Sales , color = Dept ) ) +
  geom_point() +
  scale_y_continuous(name="Weekly Sales" )

ggplot( train, aes( x = Weekly_Sales ) ) +
  geom_histogram(binwidth=5000 ) + 
  ## Vertical line indicating the mean value
  geom_vline( aes( xintercept = mean( Weekly_Sales ) ), color="red" ) +
  scale_y_continuous( "Frequency of Occurance" ) +
  scale_x_continuous( "Weekly Sales" )

train0 <- subset( train, train$Weekly_Sales <= 0 )


paste0( round( nrow(train0)/nrow( train )*100 , 1) , "%" )

abs( min( train0$Weekly_Sales ) )

## Create subset of train data set
train <- subset( train , train$Weekly_Sales > 0 )
## Make Log Transformation for Weekly_Sales
train$Log_Weekly_Sales <- log(train$Weekly_Sales)
## remove train0
rm( train0 )


## Merging the datasets
trainStoresMerge <- merge(train , stores , by = "Store")
trainStoresFeaturesMerge <- merge( trainStoresMerge , features , by = c( "Store" , "Date" ) )
colnames(trainStoresFeaturesMerge)[5] <- "IsHoliday"
trainStoresFeaturesMerge$IsHoliday.y <- NULL
testStoresMerge <- merge(test , stores , by = "Store")
testStoresFeaturesMerge <- merge( testStoresMerge , features , by = c( "Store" , "Date" ) )
colnames(testStoresFeaturesMerge)[5] <- "IsHoliday"
testStoresFeaturesMerge$IsHoliday.y <- NULL





summary(train$Weekly_Sales)
summary( train$Log_Weekly_Sales )


## plotting the log( Weekly_Sales ) histogram
ggplot( train, aes( x = Log_Weekly_Sales ) ) +
  geom_histogram(binwidth=.2 ) + 
  ## Vertical line indicating the mean value
  geom_vline( aes( xintercept = mean( Log_Weekly_Sales ) ), color="red" ) +
  scale_y_continuous( "Frequency of Occurance" ) +
  scale_x_continuous( "Weekly Sales" )






## Function to calculate the Standard Error
## x: the vector of numerical values
## returns the standard error of the vector
standardError <- function( x ) {
  sd( x )/sqrt( length( x ) )
}


## Calculating Detailed Summary Statistics for Weekly_Sales
Weekly_Sales <- c( 
  mean( train$Weekly_Sales ) ,
  standardError(  train$Weekly_Sales ) ,
  median( train$Weekly_Sales) ,
  sd( train$Weekly_Sales ) ,
  var( train$Weekly_Sales ) , 
  kurtosis( train$Weekly_Sales ) ,
  skewness( train$Weekly_Sales ) ,
  range( train$Weekly_Sales )[2]-range( train$Weekly_Sales )[1] ,
  min( train$Weekly_Sales ) ,
  max( train$Weekly_Sales ) ,
  sum( train$Weekly_Sales ),
  length( train$Weekly_Sales ) 
)

## Calculating Detailed Summary Statistics for Weekly_Sales
Log_Weekly_Sales <- c( 
  mean( train$Log_Weekly_Sales , na.rm = T ) ,
  standardError(  train$Log_Weekly_Sales ) ,
  median( train$Log_Weekly_Sales) ,
  sd( train$Log_Weekly_Sales ) ,
  var( train$Log_Weekly_Sales ) , 
  kurtosis( train$Log_Weekly_Sales ) ,
  skewness( train$Log_Weekly_Sales ) ,
  range( train$Log_Weekly_Sales )[2]-range( train$Log_Weekly_Sales )[1] ,
  min( train$Log_Weekly_Sales ) ,
  max( train$Log_Weekly_Sales ) ,
  sum( train$Log_Weekly_Sales ),
  length( train$Log_Weekly_Sales ) 
)

Description <- c(
  "Mean",
  "Standard Error" ,
  "Median" , 
  "Standard Deviation" ,
  "Variance" , 
  "Kurtosis" ,
  "Skewness" ,
  "Range" ,
  "Min" ,
  "Max" ,
  "Sum" ,
  "Count" 
)

Weekly_Sales
Description

Detailed_Summary_Statistics_on_Weekly_Sales <- 
  data.frame( 
    Description=Description , 
    Weekly_Sales = Weekly_Sales ,
    Log_Weekly_Sales = Log_Weekly_Sales
    )
## printing out the detailed summary statistics 
print( Detailed_Summary_Statistics_on_Weekly_Sales , row.names = F )

rm( 
  Description, 
  Log_Weekly_Sales , 
  Weekly_Sales , 
  Detailed_Summary_Statistics_on_Weekly_Sales
  )


############# Stores Dataset
summary(stores$Size)

## plotting the Size histogram
ggplot( stores , aes( x = Size ) ) +
  geom_histogram(binwidth=2000 ) + 
  ## Vertical line indicating the mean value
  geom_vline( aes( xintercept = mean( Size ) ), color="red" ) +
  scale_y_continuous( "Frequency of Occurance" ) +
  scale_x_continuous( "Store Size" )


## box plot to show the summary statistics of the Type of Stores and their sizes
ggplot(data=stores, 
       aes(x=Type, y=Size, fill=Type) ) + 
  geom_boxplot(outlier.shape = 15, outlier.size = 4) +
  ## to show how the individual store sizes are distributed
  geom_jitter() +
  scale_y_continuous( name="Store Size" ) + 
  scale_fill_brewer( name = "Store Type" , palette = "Dark2")

## Standard Deviation of each type of Store
tapply( stores$Size , stores$Type , sd )
## Median of each Type of Store
tapply( stores$Size , stores$Type , median )
## Mean of each type of Store
tapply( stores$Size , stores$Type , mean )






## Function to calculate Week
## date - the date for which the Week Number should be calculated
## returns week Number
weekNumber <- function( date ) {
  d1 <- as.Date( paste0( year(date) , "-01-01" ) )
  as.integer((date-d1)/7)+1
}

## Adding the Week Number to the holidayDateTableDataFrame
holidayDateTableDataFrame$WeekNumber <- weekNumber( holidayDateTableDataFrame$Date )
## Adding Month to holidayDateTableDataFrame
holidayDateTableDataFrame$Month <- month( holidayDateTableDataFrame$Date )


## Merging holidayDateTableDataFrame with trainStoresFeaturesMerge
trainStoresFeaturesMerge <- merge( trainStoresFeaturesMerge , holidayDateTableDataFrame , by = "Date" )

summary(trainStoresFeaturesMerge)







####################################################################
## Hypothesis development
####################################################################
## Extracting observations where IsHoliday = False
Hyp_NotHoliday <- subset( trainStoresFeaturesMerge , IsHoliday == FALSE );
Hyp_Holiday <- subset( trainStoresFeaturesMerge , IsHoliday == TRUE );

## Number of sample elements to collect from population 
## should be <10% of holiday Week Population
ndiff <- 2500
## Seeding to ensure the randomness can be repeated
set.seed(1101)
## Getting a sample elements (ndiff) (<10% of Holiday Weeks)
Holiday_Sample <- sample( Hyp_Holiday$Log_Weekly_Sales , ndiff )
NotHoliday_Sample <- sample( Hyp_NotHoliday$Log_Weekly_Sales , ndiff )


## combining both the sample into one x-Axis Variable
xVar <- c(NotHoliday_Sample , Holiday_Sample )
## Creating the color Variable
colorVar <- as.factor(c(rep(1, ndiff), rep(2, ndiff ) ) )
## creating the dataframe
sampleDensityDf <- data.frame( xVar ,  colorVar )
## the density plot showing the 
## Not Holiday and Holiday values of Log(Weekly_Sales)
plottingDensity <- ggplot( sampleDensityDf , aes(x = xVar, fill = colorVar) ) + 
  geom_density( alpha = .2 ) +
  scale_x_continuous( "log(Weekly_Sales)" ) +
  scale_fill_discrete( 
    name = "Sample" , labels=c( "Not Holiday", "Holiday" ) ) +
  scale_y_continuous( "Density" ) +
  theme( legend.position = "bottom" )
## box plot to show the Density Distribution
boxPlotDensity <- ggplot( sampleDensityDf , aes( colorVar , xVar ) ) + 
  geom_boxplot( aes( fill = colorVar ) ) + 
  scale_y_continuous( "log(Weekly_Sales)" ) +
  scale_fill_discrete( 
    name = "Sample" , labels=c( "Not Holiday", "Holiday" ) ) +
  scale_x_discrete( "Sample" ) +
  theme( legend.position = "bottom" )
## arranging the plots next to each other
grid.arrange( plottingDensity , boxPlotDensity , nrow = 1 )
## removing plots from memory
rm( xVar , colorVar , sampleDensityDf , plottingDensity , boxPlotDensity, Hyp_Holiday , Hyp_NotHoliday)


## Calculating the Difference
Diff_Log_Weekly_Sales = Holiday_Sample - NotHoliday_Sample
## Printing Top 5 values of diff
head(Diff_Log_Weekly_Sales)
## Calculating the Test Statistic
xBar <- mean(Diff_Log_Weekly_Sales)
xBar
## Calculating the Test Statistic
zScore <- xBar / standardError(Diff_Log_Weekly_Sales)
zScore
## Calculating p-value
pValue <- 1-pnorm( zScore )
pValue
## removing variables not needed anymore
rm( pValue , zScore , xBar , Diff_Log_Weekly_Sales , Holiday_Sample , NotHoliday_Sample , ndiff )



#########################################################################
#######################
## ThanksGiving & Christmas Season Hypothesis Testing
##############################################################


Hyp_NotHoliday <- subset( trainStoresFeaturesMerge , 
                          HolidaySeasonId != 11 & HolidaySeasonId != 12  );
Hyp_Holiday <- subset( trainStoresFeaturesMerge , 
                       HolidaySeasonId == 11 | HolidaySeasonId == 12);

## Getting the Number of rows in each dataset
nrow( Hyp_NotHoliday )
nrow( Hyp_Holiday )


## Number of sample elements to collect from population 
## should be <10% of holiday Week Population
ndiff <- 5000
## Seeding to ensure the randomness can be repeated
set.seed(1101)
## Getting a sample of elements (ndiff) (<10% of Holiday Weeks)
Holiday_Sample <- sample( Hyp_Holiday$Log_Weekly_Sales , ndiff )
head(Holiday_Sample)
NotHoliday_Sample <- sample( Hyp_NotHoliday$Log_Weekly_Sales , ndiff )
head(NotHoliday_Sample)


## combining both the sample into one x-Axis Variable
xVar <- c(NotHoliday_Sample , Holiday_Sample )
## Creating the color Variable
colorVar <- as.factor(c(rep(1, ndiff), rep(2, ndiff ) ) )
## creating the dataframe
sampleDensityDf <- data.frame( xVar ,  colorVar )
## the density plot showing the 
## Not Holiday and Holiday values of Log(Weekly_Sales)
plottingDensity <- ggplot( sampleDensityDf , aes(x = xVar, fill = colorVar) ) + 
  geom_density( alpha = .2 ) +
  scale_x_continuous( "log(Weekly_Sales)" ) +
  scale_fill_discrete( 
    name = "Sample" , labels=c( "Not Holiday", "Holiday" ) ) +
  scale_y_continuous( "Density" ) +
  theme( legend.position = "bottom" )
## box plot to show the Density Distribution
boxPlotDensity <- ggplot( sampleDensityDf , aes( colorVar , xVar ) ) + 
  geom_boxplot( aes( fill = colorVar ) ) + 
  scale_y_continuous( "log(Weekly_Sales)" ) +
  scale_fill_discrete( 
    name = "Sample" , labels=c( "Not Holiday", "Holiday" ) ) +
  scale_x_discrete( "Sample"  , labels=c( "Not Holiday", "Holiday" ) ) +
  theme( legend.position = "bottom" )
## arranging the plots next to each other
grid.arrange( plottingDensity , boxPlotDensity , nrow = 1 )
## removing plots from memory
rm( xVar , colorVar , sampleDensityDf , plottingDensity , 
    boxPlotDensity, Hyp_Holiday , Hyp_NotHoliday)




## Calculating the Difference
Diff_Log_Weekly_Sales = Holiday_Sample - NotHoliday_Sample
## Printing Top 5 values of diff
head(Diff_Log_Weekly_Sales)
## Calculating the Test Statistic
xBar <- mean(Diff_Log_Weekly_Sales)
xBar
## Calculating the Test Statistic
zScore <- xBar / standardError(Diff_Log_Weekly_Sales)
zScore
## Calculating p-value
## 1-pnorm() because we are doing a one-sided test - greater than
pValue <- 1-pnorm( zScore ) 
pValue

## removing variables not needed anymore
rm( pValue , zScore , xBar , Diff_Log_Weekly_Sales , Holiday_Sample , NotHoliday_Sample , ndiff )





# Model Building

trainStoresFeaturesMerge$TotalSales = trainStoresFeaturesMerge$TotalSalesInMillion = NULL
str(trainStoresFeaturesMerge)

## removing HolidaySeasonId before building model 
## because we already have another factor variable
trainStoresFeaturesMerge$HolidaySeasonId = NULL


modelLog <- lm( 
  formula = Log_Weekly_Sales ~ . -Date -Weekly_Sales , 
  data = trainStoresFeaturesMerge )


model_Weekly_Sales = lm( 
  formula = Weekly_Sales ~
    Store + Dept + Type + IsHoliday +  Size +
    Temperature + Fuel_Price + 
    #MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 +
    CPI + Unemployment +
    HolidaySeasonType + IsHolidaySeason + 
    WeekNumber + Month , data = trainStoresFeaturesMerge )

summary(model_Weekly_Sales)
