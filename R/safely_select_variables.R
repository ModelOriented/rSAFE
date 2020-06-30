#' @title Performing feature selection on the dataset with transformed variables
#'
#' @description The safely_select_variables() function selects variables from dataset returned
#' by safely_transform_data() function. For each original variable exactly one variable is chosen
#' - either original one or transformed one. The choice is based on the AIC value
#' for linear model (regression) or logistic regression (classification).
#'
#' @param safe_extractor object containing information about variables transformations created with safe_extraction() function
#' @param data data, original dataset or the one returned by safely_transform_data() function.
#' If data do not contain transformed variables then transformation is done inside this function using 'safe_extractor' argument.
#' Data may contain response variable or not - if it does then 'which_y' argument must be given,
#' otherwise 'y' argument should be provided.
#' @param y vector of responses, must be given if data does not contain it
#' @param which_y numeric or character (optional), must be given if data contains response values
#' @param class_pred numeric or character, used only in multi-classification problems.
#' If response vector has more than two levels, then 'class_pred' should indicate the class of interest
#' which will denote failure - all other classess will stand for success.
#' @param verbose logical, if progress bar is to be printed
#'
#' @return vector of variables names, selected based on AIC values
#'
#' @seealso \code{\link{safely_transform_data}}
#'
#' @examples
#'
#' library(DALEX)
#' library(randomForest)
#' library(rSAFE)
#'
#' data <- apartments[1:500,]
#' set.seed(111)
#' model_rf <- randomForest(m2.price ~ construction.year + surface + floor +
#'                            no.rooms + district, data = data)
#' explainer_rf <- explain(model_rf, data = data[,2:6], y = data[,1])
#' safe_extractor <- safe_extraction(explainer_rf, verbose = FALSE)
#' safely_select_variables(safe_extractor, data, which_y = "m2.price", verbose = FALSE)
#'
#' @export


safely_select_variables <- function(safe_extractor, data, y = NULL, which_y = NULL, class_pred = NULL, verbose = TRUE) {

  if (class(safe_extractor) != "safe_extractor") {
    stop(paste0("No applicable method for 'safely_select_variables' applied to an object of class '", class(safe_extractor), "'."))
  }
  if (is.null(data)) {
    stop("No data provided!")
  }
  if (is.null(y) & is.null(which_y)) {
    stop("Specify either y or which_y argument!")
  }
  if (! is.null(which_y)) {
    #checking correctness of which_y argument
    y <- tryCatch(
      {
        data[,which_y]
      },
      error = function(cond) {
        stop("The 'y' variable is not in the dataset!")
        message(cond)
      }
    )
    if (is.character(which_y)) {
      #which_y is a column name
      data <- data[, colnames(data) != which_y]
      data <- data[, colnames(data) != paste0(which_y, "_new")]
    } else {
      #which_y is a column index
      data <- data[, -which_y]
    }
  }

  #class of interest
  if (is.factor(y)) {
    if (! is.null(class_pred)) {
      if (is.character(class_pred)) {
        if (! class_pred %in% levels(y)) {
          cat("There is no such a level in response vector! Using first level instead.\n")
          class_pred <- levels(y)[1]
        }
      } else {
        if (! class_pred %in% 1:length(levels(y))) {
          cat("There is no such a level in response vector! Using first level instead.\n")
          class_pred <- levels(y)[1]
        }
      }
    } else {
      class_pred <- levels(y)[1]
    }
  }


  #checking whether data is already transformed or not
  term_names <- names(safe_extractor$variables_info)
  term_names <- term_names[term_names != which_y]
  term_names_new <- sapply(term_names, function(x) paste0(x, "_new"))
  #we check whether there is at least one transformed variable in given dataset
  term_names_new_present <- intersect(colnames(data), term_names_new)

  if (length(term_names_new_present) == 0) {
    #there are only original variables, no transformations have been done - we will do it now

    data <- safely_transform_data(safe_extractor, data, verbose = FALSE)
    term_names_new_present <- intersect(colnames(data), term_names_new)
  }
  #now data is supposed to contain also transformed variables
  var_best <- term_names #set of variables, each is either original or transformed, initially all are original

  if (verbose == TRUE) {
    #progress bar
    pb <- utils::txtProgressBar(min = 0, max = length(term_names), style = 3)
  }

  #fitting white box model to decide which of the original and transformed variable is better
  #for regression problems -> linear regression
  #for classification problem -> logistic regression
  if (is.factor(y)) {
    #-> classification
    #binary model for chosen factor level
    for (var_temp in term_names) {
      if (paste0(var_temp, "_new") %in% colnames(data)) { #checking if there is transformed variable
        var_checked <- c(setdiff(var_best, var_temp), paste0(var_temp, "_new"))
        model_best <- stats::glm((y == class_pred) ~ ., data = as.data.frame(data[, var_best]), family = binomial(link = 'logit'))
        model_checked <- stats::glm((y == class_pred) ~ ., data = as.data.frame(data[, var_checked]), family = binomial(link = 'logit'))
        if (stats::AIC(model_checked) < stats::AIC(model_best)) {
          var_best <- var_checked
        }
      }
      if (verbose == TRUE) {
        utils::setTxtProgressBar(pb, which(term_names == var_temp))
      }
    }

  } else {
    #-> regression
    for (var_temp in term_names) {
      if (paste0(var_temp, "_new") %in% colnames(data)) { #checking if there is transformed variable
        var_checked <- c(setdiff(var_best, var_temp), paste0(var_temp, "_new"))
        model_best <- stats::lm(y ~ ., data = as.data.frame(data[, var_best]))
        model_checked <- stats::lm(y ~ ., data = as.data.frame(data[, var_checked]))
        if (stats::AIC(model_checked) < stats::AIC(model_best)) { #comparing AIC to choose better set of variables
          var_best <- var_checked
        }
      }
      if (verbose == TRUE) {
        utils::setTxtProgressBar(pb, which(term_names == var_temp))
      }
    }

  }

  #closing progress bar
  if (verbose == TRUE) {
    close(pb)
  }

  return(var_best)

}



