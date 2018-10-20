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
library(ggplot2)
library(utils)
library(stringr)
library(iml)
library(factorMerger)

## ---- warning = FALSE----------------------------------------------------
model_lm1 <- lm(time ~ ., data = hills[1:12,])
explainer_lm1 <- explain(model_lm1, data = hills[13:24,1:2], y = hills[13:24,3], label = "lm1")
set.seed(111)
model_rf1 <- randomForest(time ~ ., data = hills[1:12,])
explainer_rf1 <- explain(model_rf1, data = hills[13:24,1:2], y = hills[13:24,3], label = "rf1")

## ---- fig.width=6--------------------------------------------------------
trans_prop <- transform_propositions(explainer_rf1, package = "strucchange", type = "linear", plot = TRUE, interactions = FALSE)
data1 <- transform_data(hills[13:24,1:2], trans_prop, keep_old = FALSE) #tylko nowe zmienne
data1_old <- transform_data(hills[13:24,1:2], trans_prop, keep_old = TRUE) #nowe + stare zmienne

data1 <- cbind(hills[13:24,3], data1)
colnames(data1)[1] <- 'time'
data1_old <- cbind(hills[13:24,3], data1_old)
colnames(data1_old)[1] <- 'time'

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(head(data1_old))

## ------------------------------------------------------------------------
data2 <- transform_data(hills[25:nrow(hills),1:2], trans_prop, keep_old = FALSE)
data2_old <- transform_data(hills[25:nrow(hills),1:2], trans_prop, keep_old = TRUE)

model_lm2 <- lm(time ~ ., data = data1)
explainer_lm2 <- explain(model_lm2, data = data2, y = hills[25:nrow(hills),3], label = "lm2")

model_lm2_old <- lm(time ~ ., data = data1_old)
explainer_lm2_old <- explain(model_lm2_old, data = data2_old, y = hills[25:nrow(hills),3], label = "lm2_old")

set.seed(111)
model_rf2 <- randomForest(time ~ ., data = data1)
explainer_rf2 <- explain(model_rf2, data2, hills[25:nrow(hills),3], label = "rf2")

set.seed(111)
model_rf2_old <- randomForest(time ~ ., data = data1_old)
explainer_rf2_old <- explain(model_rf2_old, data2_old, hills[25:nrow(hills),3], label = "rf2_old")

## ---- echo = FALSE, fig.width=7, fig.height=6----------------------------
mp_lm1 <- model_performance(explainer_lm1)
vi_lm1 <- variable_importance(explainer_lm1, type = "difference")

mp_rf1 <- model_performance(explainer_rf1)
vi_rf1 <- variable_importance(explainer_rf1, type = "difference")

mp_lm2 <- model_performance(explainer_lm2)
vi_lm2 <- variable_importance(explainer_lm2, type = "difference")

mp_lm2_old <- model_performance(explainer_lm2_old)
vi_lm2_old <- variable_importance(explainer_lm2_old, type = "difference")

mp_rf2 <- model_performance(explainer_rf2)
vi_rf2 <- variable_importance(explainer_rf2, type = "difference")

mp_rf2_old <- model_performance(explainer_rf2_old)
vi_rf2_old <- variable_importance(explainer_rf2_old, type = "difference")


plot(mp_lm1, mp_rf1, mp_lm2, mp_lm2_old, mp_rf2, mp_rf2_old, geom = "boxplot")
plot(vi_lm1, vi_rf1, vi_lm2, vi_lm2_old, vi_rf2, vi_rf2_old)

## ------------------------------------------------------------------------
summary(model_lm1)
summary(model_lm2)

