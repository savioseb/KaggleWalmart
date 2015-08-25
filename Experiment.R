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


#unique store department
storeDept <- train[,c(1,2)]
uniqueStoreDept <- unique(storeDept)
hist(uniqueStoreDept$Store,breaks = 45)


## Total Sales vs. Store Size - plotting the relationship
## calculating the sum of all the store sales
StoreTotalSales <- tapply(trainStoresFeaturesMerge$Weekly_Sales, trainStoresFeaturesMerge$Store, FUN = sum)
## converting the table to a DataFrame
stores$TotalSales <- StoreTotalSales
stores$TotalSalesInMillion <- stores$TotalSales/1000000
## Plotting the Total Sales vs. Store Size
ggplot( stores , aes(x=Size , y=TotalSales , color = Type ) ) + 
  geom_point() +  
  scale_y_continuous(name="Total Sales in Millions" )



## Total Sales Per Week
totalSalesPerWeek <- tapply( trainStoresFeaturesMerge$Weekly_Sales , trainStoresFeaturesMerge$Date , FUN = sum )
totalSalesPerWeekDataFrame <- as.data.frame( totalSalesPerWeek )
totalSalesPerWeekDataFrame$Date <- as.Date( rownames(totalSalesPerWeekDataFrame ) )
colnames(totalSalesPerWeekDataFrame)[1] <- "TotalSales"
totalSalesPerWeekDataFrame$TotalSalesInMillion = totalSalesPerWeekDataFrame$TotalSales/1000000

## Getting the holiday List
holidayDateTable <- table(trainStoresFeaturesMerge$Date , trainStoresFeaturesMerge$IsHoliday)
holidayDateTableDataFrame <- as.data.frame( holidayDateTable)
holidayDateTableDataFrame <- subset( holidayDateTableDataFrame,holidayDateTableDataFrame$Var2==T)
holidayDateTableDataFrame$IsHoliday <- ifelse( holidayDateTableDataFrame$Freq >0 , 1 , 0 )
holidayDateTableDataFrame$Date <- as.Date( holidayDateTableDataFrame$Var1 )
holidayDateTableDataFrame <- holidayDateTableDataFrame[,c(4,5)]
# creating the holiday season lag
holidayDateTableDataFrame$Week1BeforeHoliday <- lagpad(holidayDateTableDataFrame$IsHoliday , -1)
holidayDateTableDataFrame$Week2BeforeHoliday <- lagpad(holidayDateTableDataFrame$IsHoliday , -2)
holidayDateTableDataFrame$Week1AfterHoliday <- lagpad(holidayDateTableDataFrame$IsHoliday , 1)
holidayDateTableDataFrame$Week2AfterHoliday <- lagpad(holidayDateTableDataFrame$IsHoliday , 2)
holidayDateTableDataFrame$IsSpecial <- 
  holidayDateTableDataFrame$IsHoliday +
  holidayDateTableDataFrame$Week1BeforeHoliday +
  holidayDateTableDataFrame$Week2BeforeHoliday +
  holidayDateTableDataFrame$Week1AfterHoliday +
  holidayDateTableDataFrame$Week2AfterHoliday

rm(holidayDateTable , totalSalesPerWeek)

## Mering the sales per week and holiday list 
totalSalesPerWeekDataFrame <- merge( totalSalesPerWeekDataFrame, holidayDateTableDataFrame , by=2)

# Plotting Sales Per Week
ggplot( totalSalesPerWeekDataFrame , aes(x=Date , y=TotalSalesInMillion , color = IsSpecial ) ) + 
  geom_line() + 
  geom_point() +
  scale_y_continuous(name="Total Sales in Millions" )




# Model Building
model <- lm( Weekly_Sales ~ . , data = train )


# function to handle lag
lagpad <- function(x, k) {
  if( k > 0 ) {
    # It should actually be NA in the rep function but for my purposes 0 is acceptable
    c(rep(0, k), x)[1 : length(x)] 
  } else {
    c(x[ (abs(k)+1) : length(x)] , rep(0, abs(k) ) )  
  }
}



