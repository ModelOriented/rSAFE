library(SAFE)
library(DALEX)
library(randomForest)

set.seed(111)
model_rf <- randomForest(m2.price ~ ., data = apartments)
explainer_rf <- explain(model_rf, data = apartmentsTest[1:2000,2:6], y = apartmentsTest[1:2000,1])
safe_extractor <- safe_extraction(explainer_rf, inter_threshold = 0.01)
data1 <- safely_transform_data(safe_extractor, apartmentsTest[2001:4000,], verbose = FALSE)

apartments_modified <- apartments
levels(apartments_modified$district) <- c(levels(apartments_modified$district), "other")
apartments_modified[apartments_modified$district != "Srodmiescie",]$district <- "other"
apartments_modified$district <- droplevels(apartments_modified$district)
set.seed(111)
model_rf_modified <- randomForest(m2.price ~ ., data = apartments_modified)
explainer_rf_modified <- explain(model_rf_modified, data = apartments_modified[1:500,2:6], y = apartments_modified[1:500,1])
#safe_extractor_modified <- safe_extraction(explainer_rf_modified, inter_threshold = 0.01)
safe_extractor_modified <- safe_extraction(explainer_rf_modified, interactions = FALSE)

set.seed(111)
model_rf_hr <- randomForest(factor(status) ~ ., data = HR)
explainer_rf_hr <- explain(model_rf_hr, data = HR[1:3000,1:5], y = HR[1:3000,6],
                         predict_function = function(model, x) predict(model, x, type = "prob")[,1])
safe_extractor_hr <- safe_extraction(explainer_rf_hr, no_segments = 3, verbose = FALSE)
data_hr <- safely_transform_data(safe_extractor_hr, HR[1:500,], verbose = FALSE)




