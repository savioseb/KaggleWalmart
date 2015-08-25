## Loading the datasets
train <- read.csv("Data/train.csv")
stores <- read.csv("Data/stores.csv")
features <- read.csv("Data/features.csv")
test <- read.csv("Data/test.csv")

## Fixing the Data
train$Date <- as.Date(train$Date)
features$Date <- as.Date(features$Date)
test$Date <- as.Date( test$Date)

trainStoresMerge <- merge(train , stores , by = "Store")
merge( trainStoresMerge , features , by = c( "Store" , "Date" ) )


#unique store department
storeDept <- train[,c(1,2)]
uniqueStoreDept <- unique(storeDept)
hist(uniqueStoreDept$Store,breaks = 45)


# Model Building
model <- lm( Weekly_Sales ~ . , data = train )



