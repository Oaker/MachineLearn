---
title: "Machine learning CP"
author: "Sergey Chernov"
date: "Tuesday, March 10, 2015"
output: html_document
---

##Include library

```{r}

require(parallel)
require(doParallel)

library(caret)
library(randomForest)
library(parallel)
library(doParallel)
library(rpart)
```

##Introduction

Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

##Task

The goal of this project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. We use some of the collected variables to predict with. We uses our prediction model to predict 20 different test cases.


##Load data
```{r}

setwd("E://program//Machine//data")
tr<-read.csv("pml-training.csv")
test<-read.csv("pml-testing.csv")

```

Creating training set and validation set
Also we cleaned data. We removed variables with NA values and removed not digital variables.

```{r}

inTrain <- createDataPartition(y=tr$classe,
                              p=0.7, list=FALSE)

b<-sapply(tr, f<-function(a) {length(which(is.na(a)))})
type<-sapply(tr, f<-function(a) {(class(a)=="integer")||(class(a)=="numeric")})
training <- tr[inTrain,]
testing <- tr[-inTrain,]
trdata<-training[,(b==0)&type]
trdata<-trdata[,-c(1:4)]

```

We trained model with using "random forest" method.
Also we shrink number of trees because we have been problem with perfomance.

```{r}
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
ctrl <- trainControl(classProbs=TRUE,
                     savePredictions=TRUE,
                     allowParallel=TRUE
                     )
system.time(model<-train(training$classe ~.,data=trdata,method="rf",trControl=ctrl,ntree=10))
stopCluster(cl)
```

Test model and evaluate the accuracy.

```{r}

pr<-predict(model,testing)
confusionMatrix(testing$classe,pr)
```

Predict test cases from assignement.

```{r}
pr2<-predict(model,test)
```

Save files for submiting. 
 
```{r}
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(pr2)

```

