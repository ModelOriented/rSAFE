#' Selecting variables
#'
#' The safely_select_variables() function selects variables from dataset returned
#' by transform_data(). For each original variable:
#' 1) if encoding = "categorical" then exactly one variable is chosen
#' - either original one or transformed one. The choice is based on the AIC value
#' for linear model (regression) or logistic regression (classification).
#' 2) if encoding = "one-hot" then backward feature selection is performed
#' on all features
#'
#' @param safe_extractor object containing information about variables transformations created with safe_extractor() function
#' @param data data, with already transformed variables or not, containing response variable or not
#' @param y vector of responses, must be given if data does not contain it
#' @param which_y numeric or character (optional), must be given if data contains response values
#' @param encoding method of representing factor variables, one of: "categorical", "one-hot"
#' @param verbose logical, if progress bar is to be printed
#'
#' @return vector of variables names, selected based on AIC values
#'
#' @export
#'


safely_select_variables <- function(safe_extractor, data, y = NULL, which_y = NULL, encoding = "categorical", verbose = TRUE) {
  #one can either give y explicitly or specify which column in data is response column
  #data can be transformed or not

  if (is.null(y) & is.null(which_y)) {
    stop("Specify either y or which_y argument!")
  }

  if (! is.null(which_y)) {

    #checking correctness of which_y argument
    tryCatch(
      {
        y <- data[,which_y]
      },
      error = function(cond) {
        print("The 'y' variable is not in the dataset!")
        message(cond)
      }
    )
    if (is.character(which_y)) {
      #which_y is a column name
      data <- data[, colnames(data) != which_y]
    } else {
      #which_y is a column index
      data <- data[, -which_y]
    }

  }


  #checking whether data is already transformed or not
  vars <- names(safe_extractor$vars)
  #checking if there are variables different than the ones specified in safe_extractor$vars
  #assuming that in the dataset there are only variables that were present in the initial black box model
  diff_var <- setdiff(colnames(data), vars)


  #1) encoding = "categorical"
  if (encoding == "categorical") {

    if (length(diff_var) == 0) {
      #there are only original variables, no transformations have been done
      #we will do it now
      data <- safely_transform_data(data, safe_extractor, encoding = "categorical", verbose = FALSE)
    }
    #now data is supposed to contain also transformed variables

    var_best <- vars #set of variables, each is either original or transformed, initially all are original

    #fitting white box model to order variables in terms of significance
    #for regression problems -> linear regression
    #for classification problem -> logistic regression
    if (is.factor(y)) {
      #-> classification
      #binary model for first factor level
      #TOODOO: better solution, maybe different model?
      model <- stats::glm((y == levels(y)[1]) ~ ., data = as.data.frame(data[,var_best]), family = binomial(link = 'logit'))
    } else {
      #-> regression
      model <- stats::lm(y ~ ., data = as.data.frame(data[,var_best])) #as.data.frame() - in case of var_best being just one variable
    }
    #cut fragment above if we do not use p-values to order variables

    #var_ordered <- summary(model)$coefficients[,4] #p-values for all variables
    #order in which variables are to be tested - starting from the one with the lowest p-value
    #var_ordered <- vars[order(var_ordered)]
    #problem with categorical variables!!!


    if (verbose == TRUE) {
      #progress bar
      pb <- utils::txtProgressBar(min = 0, max = length(vars), style = 3)
    }

    if (is.factor(y)) {

      for (v in vars) {
        if (paste0(v, "_new") %in% colnames(data)) { #checking if there is transformed variable
          var_checked <- c(setdiff(var_best, v), paste0(v, "_new"))
          model_best <- stats::glm((y == levels(y)[1]) ~ ., data = as.data.frame(data[, var_best]), family = binomial(link = 'logit'))
          model_checked <- stats::glm((y == levels(y)[1]) ~ ., data = as.data.frame(data[, var_checked]), family = binomial(link = 'logit'))
          if (AIC(model_checked) < AIC(model_best)) {
            var_best <- var_checked
          }
        }

        if (verbose == TRUE) {
          utils::setTxtProgressBar(pb, which(vars == v))
        }
      }

    } else {

      for (v in vars) {
        if (paste0(v, "_new") %in% colnames(data)) { #checking if there is transformed variable
          var_checked <- c(setdiff(var_best, v), paste0(v, "_new"))
          model_best <- stats::lm(y ~ ., data = as.data.frame(data[, var_best]))
          model_checked <- stats::lm(y ~ ., data = as.data.frame(data[, var_checked]))
          if (AIC(model_checked) < AIC(model_best)) { #comparing AIC to choose better set of variables
            var_best <- var_checked
          }
        }

        if (verbose == TRUE) {
          utils::setTxtProgressBar(pb, which(vars == v))
        }
      }

    }


    #closing progress bar
    if (verbose == TRUE) {
      close(pb)
    }


  #2) encoding == "one-hot"
  } else {

    if (length(diff_var) == 0) {
      #there are only original variables, no transformations have been done
      #we will do it now
      data <- safely_transform_data(data, safe_extractor, encoding = "one-hot", verbose = FALSE)
    }
    #now data is supposed to contain also transformed variables

    var_best <- colnames(data) #set of variables in transformed dataset

    if (is.factor(y)) {
      #-> classification
      #binary model for first factor level
      #TOODOO: better solution, maybe different model?
      model <- stats::glm((y == levels(y)[1]) ~ ., data = as.data.frame(data), family = binomial(link = 'logit'))
    } else {
      #-> regression
      model <- stats::lm(y ~ ., data = as.data.frame(data)) #as.data.frame() - in case of var_best being just one variable
    }

    model <- stats::step(model, direction = 'backward', trace = (verbose==TRUE))
    var_best <- c(attr(model$terms, "term.labels"))


  }




  return(var_best)



}



# customize_data <- function(data, safe_extractor, model_whitebox = NULL, select_variables = TRUE, one_hot = FALSE, which_one_hot = NULL) {
#
#   if (select_variables == TRUE & is.null(model_whitebox)) {
#     stop("Pass a white box model as an argument in order to perform feature selection.")
#   }
#
#   #features selection
#   if (select_variables == TRUE) {
#
#     #safe_extractor$explainer$data might contain a response variable or not
#     if (! is.null(safe_extractor$explainer$y)) { #response variable is given in 'y' argument of explainer
#       #we merge data with responses
#       model_data <- cbind(data.frame("response_variable" = safe_extractor$explainer$y),
#                           data)
#     } else { #response variable is given in 'data' argument of explainer
#       #we search for column with responses
#       #after deleting original predictors and original predictors with suffix '_new' there should be exactly one column left
#       response_variable_name <- setdiff(setdiff(names(data),
#                                                 names(safe_extractor$vars)),
#                                         as.vector(sapply(names(safe_extractor$vars), function(x) paste0(x, "_new"))))
#       #checking if there is exactly one response variable
#       if (length(response_variable_name) != 1) {
#         stop("There is something wrong with variables in dataset.")
#       }
#
#       model_data <- data
#       colnames(model_data)[which(colnames(model_data) == response_variable_name)] <- "response_variable"
#
#     }
#
#     #original white-box model and variables
#     model_best <- model_whitebox
#     vars_best <- names(safe_extractor$vars)
#
#     #for each feature we check whether it is better to leave its original form or to transform it
#     #we start from fetaures that have the highest overall contribution strength
#     #and we compare mean of absolute values of contributions
#
#
#     #T O D O !!!
#
#
#
#
#
#     #updating given model by adding newly created variables
#     model_new <- stats::update(model_whitebox,
#                                as.formula(paste("response_variable ~", paste(colnames(model_data)[colnames(model_data) != "response_variable"], collapse = " + "))),
#                                           data = model_data)
#
#   }
#
#   return(model_new)
#
# }


