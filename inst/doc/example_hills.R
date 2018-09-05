## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- echo = FALSE, message = FALSE, warning = FALSE---------------------
library(SAFE)
library(DALEX)
library(changepoint.np)
library(strucchange)
library(randomForest)

## ---- warning = FALSE----------------------------------------------------
set.seed(111)
hills_rf_model <- randomForest(time ~ ., data = hills)
explainer <- explain(hills_rf_model, data = hills[,c(1,2)], y = hills[,3])

## ------------------------------------------------------------------------
data <- transform_variables(explainer, hills[,c(1,2)], package = "strucchange", type = "linear", plot = TRUE)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(head(data))

## ---- fig.width=6--------------------------------------------------------
data <- cbind(hills[,3], data)
colnames(data)[1] <- 'time'

set.seed(111)
model_rf <- randomForest(time ~ ., data = data)
varImpPlot(model_rf)

model_lm <- lm(time ~ ., data = data)
summary(model_lm)

