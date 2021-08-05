---
title: "Prediction Assignment Writeup"
author: "Fritzi G. Gironella"
date: "5 Aug 2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 

In this project, data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants are to be used. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: <http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har> (see the section on the Weight Lifting Exercise Dataset).

The training data for this project are available here:
<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv>

The test data are available here:
<https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv>

The data for this project come from this source:
<http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har>

The goal of the project is to predict the manner in which they did the exercise. This is the **classe** variable in the training set. Any of the other variables may be used to predict with. This report describes how the model was built, how cross validation was used, the expected out of sample error, and why some model parameters were chosen. The prediction model will be used to predict 20 different test cases. 

## Loading and preprocessing the data

This project will be using the **caret** package.

```{r}
library(caret)
```

This is the code to load the data from the *.csv files in the URLs:

```{r load_data_train}
# download and read training dataset
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","pml-training.csv")
dat_train <- read.csv("pml-training.csv", na.strings = c("NA","","#DIV/0!"))
str(dat_train)
```

```{r load_data_test}
# download and read test dataset
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","pml-testing.csv")
dat_test <- read.csv("pml-testing.csv", na.strings = c("NA","","#DIV/0!"))
str(dat_test)
```

The training set and test set are further pre-processed by deleting parameters (i.e. columns) that are empty.

```{r na_data}
# determining empty columns
na_cols <- which(is.na(dat_train[1,]))
colnames(dat_train[,na_cols])
# deleting empty columns from training set
dat_train <- dat_train[,-na_cols]
# deleting empty columns from test set
dat_test <- dat_test[,-na_cols]
```

The training dataset will be partitioned to get a separate dataset for validation:

```{r dat_train_v}
# create data partition
set.seed(72387)
part_train <- createDataPartition(y = dat_train$classe, p = 0.7, list = FALSE)
dat_train_t <- dat_train[part_train,]
dat_train_v <- dat_train[-part_train,]
```

## Model Fitting and Prediction

I chose to build a Random Forest model to predict the **classe** variable. As a first model, all motion sensor variables, i.e. disregarding the first seven columns of the dataset, will be used as predictors or features in the Random Forest model.

```{r rf_model_1, eval = FALSE}
# fitting a random forest model
set.seed(72387)
rf_model_1 <- train(classe ~ ., data = dat_train_t[,8:60], method = "rf", prox = TRUE)
```

```{r load_rf_model_1, echo = FALSE}
# saved random forest model is loaded.
load("rf_model_1.rda")
rf_model_1
```

The Random Forest model fit will now be used to predict the **classe** variable of the training set **predict_t**. The in-sample evaluation will be used to test if the model is biased or underfitting.

```{r predict_t}
# predicting test set
set.seed(72387)
predict_t <- predict(rf_model_1, dat_train_t)
# evaluating training set prediction vs actual
sum(predict_t == dat_train_t$classe) / length(dat_train_t$classe)
# matrix of prediction vs actual in training set
table(predict_t, dat_train_t$classe)
```

The in-sample accuracy of 100% might be an indication that the model is overfitting. This can further be checked by evaluating the accuracy of the predictions done in the validation set **predict_v**. 

```{r predict_v}
# predicting validation set
set.seed(72387)
predict_v <- predict(rf_model_1, dat_train_v)
# evaluating validation set prediction vs actual
sum(predict_v == dat_train_v$classe) / length(dat_train_v$classe)
# matrix of prediction vs actual in validation set
table(predict_v, dat_train_v$classe)
```

The out-of-sample accuracy of 99% indicates that the Random Forest model is a good predictor of the **classe** variable. No further improvements will be done in this model.

## Model Performance

To evaluate the performance of the Random Forest model, it will now be used to predict the **classe** variable of the test set **predict_test**.

```{r eval_test}
# predicting test set
set.seed(72387)
predict_test <- predict(rf_model_1, dat_test)
# presenting the prediction of the test set in a table format
data.frame(predict_test, dat_test$problem_id)
```
