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
apartments_rf_model <- randomForest(m2.price ~ ., data = apartments)
explainer <- explain(apartments_rf_model, data = apartmentsTest[1:7000,2:6], y = apartmentsTest$m2.price[1:7000])

## ------------------------------------------------------------------------
data1 <- transform_variables(explainer, apartmentsTest[7001:9000,2:6], package = "changepoint.np", type = "constant", plot = TRUE)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(head(data1))

## ---- fig.width=6--------------------------------------------------------
data1 <- cbind(apartmentsTest[7001:9000, 1], data1)
colnames(data1)[1] <- 'm2.price'

set.seed(111)
model1_rf <- randomForest(m2.price ~ ., data = data1)
varImpPlot(model1_rf)

model1_lm <- lm(m2.price ~ ., data = data1)
summary(model1_lm)

## ------------------------------------------------------------------------
data2 <- transform_variables(explainer, apartmentsTest[7001:9000,2:6], package = "strucchange", type = "constant", plot = TRUE)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(head(data2))

## ---- fig.width=6--------------------------------------------------------
data2 <- cbind(apartmentsTest[7001:9000, 1], data2)
colnames(data2)[1] <- 'm2.price'

set.seed(111)
model2_rf <- randomForest(m2.price ~ ., data = data2)
varImpPlot(model2_rf)

model2_lm <- lm(m2.price ~ ., data = data2)
summary(model2_lm)

## ------------------------------------------------------------------------
data3 <- transform_variables(explainer, apartmentsTest[7001:9000,2:6], package = "changepoint.np", type = "linear", plot = TRUE)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(head(data3))

## ---- fig.width=6--------------------------------------------------------
data3 <- cbind(apartmentsTest[7001:9000, 1], data3)
colnames(data3)[1] <- 'm2.price'

set.seed(111)
model3_rf <- randomForest(m2.price ~ ., data = data3)
varImpPlot(model3_rf)

model3_lm <- lm(m2.price ~ ., data = data3)
summary(model3_lm)

## ------------------------------------------------------------------------
data4 <- transform_variables(explainer, apartmentsTest[7001:9000,2:6], package = "strucchange", type = "linear", plot = TRUE)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(head(data4))

## ---- fig.width=6--------------------------------------------------------
data4 <- cbind(apartmentsTest[7001:9000, 1], data4)
colnames(data4)[1] <- 'm2.price'

set.seed(111)
model4_rf <- randomForest(m2.price ~ ., data = data4)
varImpPlot(model4_rf)

model4_lm <- lm(m2.price ~ ., data = data4)
summary(model4_lm)

