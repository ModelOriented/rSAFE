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
library(gridExtra)
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

## ---- fig.width=6, warning = FALSE---------------------------------------
trans_prop <- transform_propositions(explainer_rf1, package = "strucchange", type = "linear", plot = TRUE)

data1_new <- transform_data(hills[13:24,1:2], hills[13:24,3], trans_prop, which_variables = "only_new")
data1_all <- transform_data(hills[13:24,1:2], hills[13:24,3], trans_prop, which_variables = "all")
data1_aic <- transform_data(hills[13:24,1:2], hills[13:24,3], trans_prop, which_variables = "aic")

data1_new <- cbind(hills[13:24,3], data1_new)
colnames(data1_new)[1] <- 'time'
data1_all <- cbind(hills[13:24,3], data1_all)
colnames(data1_all)[1] <- 'time'
data1_aic <- cbind(hills[13:24,3], data1_aic)
colnames(data1_aic)[1] <- 'time'

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(head(data1_all))

## ---- warning = FALSE----------------------------------------------------
data2_new <- transform_data(hills[25:nrow(hills),1:2], hills[25:nrow(hills),3], trans_prop, which_variables = "only_new")
data2_all <- transform_data(hills[25:nrow(hills),1:2], hills[25:nrow(hills),3], trans_prop, which_variables = "all")
data2_aic <- transform_data(hills[25:nrow(hills),1:2], hills[25:nrow(hills),3], trans_prop, which_variables = "all")
data2_aic <- data2_aic[,names(data2_aic) %in% names(data1_aic)]

model_lm2_new <- lm(time ~ ., data = data1_new)
explainer_lm2_new <- explain(model_lm2_new, data = data2_new, y = hills[25:nrow(hills),3], label = "lm2_new")

model_lm2_all <- lm(time ~ ., data = data1_all)
explainer_lm2_all <- explain(model_lm2_all, data = data2_all, y = hills[25:nrow(hills),3], label = "lm2_all")

model_lm2_aic <- lm(time ~ ., data = data1_aic)
explainer_lm2_aic <- explain(model_lm2_aic, data = data2_aic, y = hills[25:nrow(hills),3], label = "lm2_aic")

set.seed(111)
model_rf2_new <- randomForest(time ~ ., data = data1_new)
explainer_rf2_new <- explain(model_rf2_new, data2_new, hills[25:nrow(hills),3], label = "rf2_new")

set.seed(111)
model_rf2_all <- randomForest(time ~ ., data = data1_all)
explainer_rf2_all <- explain(model_rf2_all, data2_all, hills[25:nrow(hills),3], label = "rf2_all")

set.seed(111)
model_rf2_aic <- randomForest(time ~ ., data = data1_aic)
explainer_rf2_aic <- explain(model_rf2_aic, data2_aic, hills[25:nrow(hills),3], label = "rf2_aic")

## ---- echo = FALSE, warning = FALSE--------------------------------------
mp_lm1 <- model_performance(explainer_lm1)
vi_lm1 <- variable_importance(explainer_lm1, type = "difference")

mp_rf1 <- model_performance(explainer_rf1)
vi_rf1 <- variable_importance(explainer_rf1, type = "difference")

mp_lm2_new <- model_performance(explainer_lm2_new)
vi_lm2_new <- variable_importance(explainer_lm2_new, type = "difference")

mp_lm2_all <- model_performance(explainer_lm2_all)
vi_lm2_all <- variable_importance(explainer_lm2_all, type = "difference")

mp_lm2_aic <- model_performance(explainer_lm2_aic)
vi_lm2_aic <- variable_importance(explainer_lm2_aic, type = "difference")

mp_rf2_new <- model_performance(explainer_rf2_new)
vi_rf2_new <- variable_importance(explainer_rf2_new, type = "difference")

mp_rf2_all <- model_performance(explainer_rf2_all)
vi_rf2_all <- variable_importance(explainer_rf2_all, type = "difference")

mp_rf2_aic <- model_performance(explainer_rf2_aic)
vi_rf2_aic <- variable_importance(explainer_rf2_aic, type = "difference")

## ---- echo = FALSE, fig.width=7, fig.height=6, warning = FALSE-----------
plot(mp_lm1, mp_rf1, mp_lm2_new, mp_lm2_all, mp_lm2_aic, mp_rf2_new, mp_rf2_all, mp_rf2_aic, geom = "boxplot")

## ---- echo = FALSE, fig.width=7, fig.height=6, warning = FALSE-----------
grid.arrange(
  plot(vi_lm1, vi_lm2_new, vi_lm2_all, vi_lm2_aic),
  plot(vi_rf1, vi_rf2_new, vi_rf2_all, vi_rf2_aic), ncol = 2)

## ------------------------------------------------------------------------
summary(model_lm1)
summary(model_lm2_aic)

