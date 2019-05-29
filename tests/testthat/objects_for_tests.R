library(SAFE)
library(DALEX)
library(randomForest)

set.seed(111)
model_rf <- randomForest(m2.price ~ ., data = apartments)
explainer_rf <- explain(model_rf, data = apartmentsTest[1:3000,2:6], y = apartmentsTest[1:3000,1], label = "rf1")

v1 <- c(2,2,2,2,6,6,6,6,6)
