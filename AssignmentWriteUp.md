## Practical Machine Learning: Assignment Write-Up

### Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

### Data Sources

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

### Load and Clean the Data

#### read the data from the downloaded csv files into data frames, ensuring invalid or blank fields are conveted to NA
trainData <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
testData <- read.csv("pml-testing.csv", na.strings = c("NA", "#DIV/0!", ""))

#### Ascertain the column names and structure of the data
names(trainData)
str(trainData)

#### check distinct values for the "classe" column
table(trainData$classe)

#### Remove the first 6 columns as they're info only, not measures
trainData <- trainData[,-c(1:6)]
testData <- testData[,-c(1:6)]
dim(trainData)

#### Filter the data further to take only the rows without NAs
trainData <- trainData[,colSums(is.na(trainData)) == 0]
testData <- testData[,colSums(is.na(testData)) == 0]

## Partition and train the training dataset

#### Load the caret package and partition the data 70/30 between training and cross validation
library(caret)
set.seed(123)
inTrain <- createDataPartition(y = trainData$classe, p = 0.7, list = FALSE)
training <- trainData[inTrain,]
crossval <- trainData[-inTrain,]
dim(training)
dim(crossval)

#### Train two models on the training data, using the rpart and random forest methods
library(rpart)
library(randomForest)
partModel <- train(classe ~ ., method="rpart", data=training, trControl = trainControl(method="cv")) 
rfModel <- randomForest(classe ~ ., data=training, trControl = trainControl(method="cv"), ntree=100)

## Run predictions on the cross validation dataset

#### Predict the outcome for each model using the cross-validation data
predPartModel <- predict(partModel, newdata=crossval)
predRfModel <- predict(rfModel, newdata=crossval)

#### Determine the accuracy of each using confusion matrixes, and calculate the out-of-sample error
confMatrixPartModel <- confusionMatrix(predPartModel, crossval$classe)
print(confMatrixPartModel)

confMatrixRfModel <- confusionMatrix(predRfModel, crossval$classe)
print(confMatrixRfModel)

#### Run the tests agianst the test dataset
predTestData <- predict(rfModel, newdata=testData)






