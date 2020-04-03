#title: "PracticalMachineLearningProject"
#Author: "Saydaliev"
#Date: "4/3/2020
  #Github link:https://github.com/Saydaliev/Practical-Machine-Learning/tree/master
#Rpubs link: https://rpubs.com/Saydaliev/593975

## Background
#Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

### Load Packages, set seed

library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(e1071)
set.seed(5316)
### Loading Data, Working with Data Frame, and Cleaning Data
#After reading data we will check the dimension of the datasets with 'dim' and the variables names with 'names'

getwd()
trainDT <- read.csv("pml-training.csv",na.strings = c("NA","#DIV/0!",""))
testDT  <- read.csv("pml-testing.csv", na.strings = c("NA","#DIV/0!",""))
dim(trainDT)
dim(testDT)
names(trainDT)


### Data Cleaning 
#We want to remove variables with missing observations from the training set -- these can negatively impact the predictive modeling. We remove from the training and the test set variables that don't contain the relevant accelerometry data, just to tidy the dataframes. From the output from `names`, we see that these variables are in the first seven columns of the dataframe.

trainDT <- trainDT[, colSums(is.na(trainDT)) == 0] 
testDT <- testDT[, colSums(is.na(testDT)) == 0]  
trainDT <- trainDT[, -c(1:7)]
testDT <- testDT[, -c(1:7)]
### Subsetting the training data

#Test the training set into the training set used for model training and a cross validation set. We use a 80/20 split.

inTrain <- createDataPartition(trainDT$classe, p=0.8, list=FALSE)
newTraining = trainDT[ inTrain,]
newTesting = trainDT[-inTrain,]
### Model Building

#Random forests are a robust method for classifying factor variables such as **classe**, the outcome variable, because of their efficiency at selecting the most important predictive variables and handling of covariates and outliers.

#We first build the predictive model using our new subsetted training data.

fitForest <- randomForest(classe ~., data=newTraining, type="class")
#Then we test the fit on the testing subset of the training data and calculate the accuracy using `confusionMatrix`.

resultForest <- predict(fitForest, newTesting)
confusionMatrix(newTesting$classe, resultForest)
#As we can see, the accuracy of this model is about 99.67%, indicating accurate prediction. 

### Predicting the test dataset

#Finally, we can use our model to predict class levels in the given test data.

finalResult <- predict(fitForest, testDT)
print(finalResult)

### Figures

#A tree model can be useful in visualizing the classification scheme.

treeTest <- rpart(classe ~ ., data=trainDT, method="class")
prp(treeTest)
