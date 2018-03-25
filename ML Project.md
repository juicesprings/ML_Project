---
title: 'Peer-graded Assignment: Prediction Assignment Writeup'
output:
  word_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

##Data

The training data for this project are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here:

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 
##Getting and Cleaning Data

1. Load relevant libraries

```{r}
library(caret)
library(randomForest)
library(knitr)
library(ggplot2)
library(rpart)
library(rpart.plot)
```

2. Get Data

```{r}
# Download
rainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# Load to R
trainingData <- read.csv(url(trainUrl), na.strings = c("NA", "#DIV/0!", ""))
testingData <- read.csv(url(testUrl), na.strings = c("NA", "#DIV/0!", ""))
```

3. Explore

```{r}
dim(trainingData)
str(trainingData)
head(trainingData)
summary(trainingData)

dim(testingData)
str(testingData)
head(testingData)
summary(testingData)
```

3. Clean

```{r}
# Remove bad data
trainingData <- trainingData[, colSums(is.na(trainingData)) == 0]
testingData <- testingData[, colSums(is.na(testingData)) == 0]

# Remove unrelated variables
trainingData <- trainingData[, -c(1:7)]
testingData <- testingData[, -c(1:7)]
```

4. Prepare Data

```{r}
# partioning the training set into two different dataset

traningPartitionData <- createDataPartition(trainingData$classe,  p = 0.7, list = F)
trainingDataSet <- trainingData[traningPartitionData, ]
testingDataSet <- trainingData[-traningPartitionData, ]
dim(trainingData); dim(testingDataSet)
```


## Model 1 - Decision Tree

```{r}
decisionTreeModel <- rpart(classe ~ ., data = trainingDataSet, method = "class")
decisionTreePrediction <- predict(decisionTreeModel, testingDataSet, type = "class")

# Plot Decision Tree
rpart.plot(decisionTreeModel, main = "Decision Tree", under = T, faclen = 0)

# Using confusion matrix to test results
confusionMatrix(decisionTreePrediction, testingDataSet$classe)
```

## Model 2 - Random Forest

```{r}
randomForestModel <- randomForest(classe ~. , data = trainingDataSet, method = "class")
randomForestPrediction <- predict(randomForestModel, testingDataSet, type = "class")

confusionMatrix(randomForestPrediction, testingDataSet$classe)
```

## Conclusion

Rndom Forest model, accuracy is 0.9988 which is better than Classification Tree model which is 0.6612

```{r}
predictionFinal <- predict(randomForestModel, testingDataSet, type = "class")
```

