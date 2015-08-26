library(ggplot2)
library(reshape2)
library(scales)
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
rm(trainStoresMerge , train)
rm( test , testStoresMerge , features )


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

rm(StoreTotalSales)



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


#####################################


## Subsetting only the holidays
totalSalesPerWeekDataFrameDuringHolidays <- 
  subset( totalSalesPerWeekDataFrame , 
          totalSalesPerWeekDataFrame$Date >= '2010-08-27' & 
            totalSalesPerWeekDataFrame$Date <= '2011-02-25' )
ggplot( totalSalesPerWeekDataFrameDuringHolidays , aes(x=Date , y=TotalSalesInMillion , color = HolidaySeasonType ) ) + 
  geom_line( aes(group=1) , size=1) + 
  geom_point(size = 3) +
  scale_y_continuous(name="Total Sales in Millions" )








# Model Building
#model <- lm( Weekly_Sales ~ . , data = train )






