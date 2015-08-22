## Loading the datasets
train <- read.csv("Data/train.csv")
stores <- read.csv("Data/stores.csv")
features <- read.csv("Data/features.csv")
test <- read.csv("Data/test.csv")

## Fixing the Data
train$Date <- as.Date(train$Date)
trainStoresMerge <- merge(train , stores , by = "Store")
features$Date <- as.Date(features$Date)
test$Date <- as.Date( test$Date)
merge( trainStoresMerge , features , by = c( "Store" , "Date" ) )


# Model Building
model <- lm( Weekly_Sales ~ . , data = train )



