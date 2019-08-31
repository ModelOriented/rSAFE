#' @title Creating SAFE extractor - an object used for surrogate-assisted feature extraction
#'
#' @description The safe_extraction() function creates a SAFE-extractor object which may be used later
#' for surrogate feature extraction.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param response_type character, type of response to be calculated, one of: "pdp", "ale".
#' If features are uncorrelated, one can use "pdp" type - otherwise "ale" is strongly recommended.
#' @param grid_points number of points on x-axis used for creating the PD/ALE plot, default 50
#' @param N number of observations from the dataset used for creating the PD/ALE plot, default 200
#' @param penalty penalty for introducing another changepoint,
#' one of "AIC", "BIC", "SIC", "MBIC", "Hannan-Quinn" or numeric non-negative value
#' @param nquantiles the number of quantiles used in integral approximation
#' @param no_segments numeric, a number of segments variable is to be divided into in case of founding no breakpoints
#' @param method the agglomeration method to be used in hierarchical clustering, one of:
#' "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"
#' @param B number of reference datasets used to calculate gap statistics
#' @param collapse a character string to separate original levels while combining them to the new one
#' @param interactions logical, if interactions between variables are to be taken into account
#' @param inter_param numeric, a positive value indicating which of single observation non-additive effects
#' are to be regarded as significant, the higher value the higher non-additive effect has to be to be taken
#' into account
#' @param inter_threshold numeric, a value from `[0,1]` interval indicating which interactions should be returned
#' as significant. It corresponds to the percentage of observations for which interaction measure is greater
#' than inter_param - if this percentage is less than inter_threshold then interaction effect is ignored.
#' @param verbose logical, if progress bar is to be printed
#'
#' @return safe_extractor object containing information about variables transformation
#'
#' @seealso \code{\link{safely_transform_categorical}}, \code{\link{safely_transform_continuous}}, \code{\link{safely_detect_interactions}}, \code{\link{safely_transform_data}}
#'
#' @importFrom graphics plot
#' @importFrom stats AIC aggregate binomial quantile
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
#' explainer_rf <- explain(model_rf, data = data[,2:6], y = data[,1], verbose = FALSE)
#' safe_extractor <- safe_extraction(explainer_rf, grid_points = 30, N = 100, verbose = FALSE)
#' print(safe_extractor)
#' plot(safe_extractor, variable = "construction.year")
#'
#' @export

safe_extraction <- function(explainer, response_type = "ale", grid_points = 50, N = 200, penalty = "MBIC", nquantiles = 10, no_segments = 2, method = "complete", B = 500, collapse = "_", interactions = FALSE, inter_param = 0.25, inter_threshold = 0.25, verbose = TRUE) {

  if (class(explainer) != "explainer") {
    stop(paste0("No applicable method for 'safe_extraction' applied to an object of class '", class(explainer), "'."))
  }
  if (! response_type %in% c("pdp", "ale")) {
    warning("Wrong type of response - using default one.")
    response_type <- "ale"
  }


  #explainer$data might contain also a response variable - we use model attributes to get only the predictors
  term_names <- attr(explainer$model$terms, "term.labels")
  if (is.null(term_names)) {
    term_names <- explainer$model$feature_names #xgboost
  }
  if (is.null(term_names)) {
    term_names <- colnames(explainer$data) #we take column names from dataset (the output variable should not be there)
  }

  p <- length(term_names) #number of variables in dataset
  n <- nrow(explainer$data) #number of observations in dataset

  #list with all variables and information on them
  variables_info <- vector("list", length = p)
  names(variables_info) <- term_names

  #checking if the feature is categorical or continuous
  for (var_temp in term_names) {
    if (is.factor(explainer$data[,var_temp])) {
      variables_info[[var_temp]] <- list(type = "categorical")
    } else {
      variables_info[[var_temp]] <- list(type = "continuous")
    }
  }

  # transformations proposition
  if (verbose == TRUE) {
    if (interactions == TRUE) {
      cat("Single variables processing...\n")
    }
    #progress bar - to let the user know how many variables have been already processed
    pb <- utils::txtProgressBar(min = 0, max = p, style = 3)
  }

  for (var_temp in term_names) {

    temp_info <- variables_info[[var_temp]] #information on currently considered variable

    if (temp_info$type == "categorical") {
      trans_prop <- safely_transform_categorical(explainer, var_temp, method, B, collapse)
      temp_info$clustering <- trans_prop$clustering
      temp_info$new_levels <- trans_prop$new_levels
    } else {
      trans_prop <- safely_transform_continuous(explainer, var_temp, response_type, grid_points, N, penalty, nquantiles, no_segments)
      temp_info$sv <- trans_prop$sv
      temp_info$break_points <- trans_prop$break_points
      temp_info$new_levels <- trans_prop$new_levels
    }

    #updating progress bar
    if (verbose == TRUE) {
      utils::setTxtProgressBar(pb, which(term_names == var_temp))
    }

    variables_info[[var_temp]] <- temp_info

  }

  #closing progress bar
  if (verbose == TRUE) {
    close(pb)
  }

  if (interactions == TRUE) {
    interaction_effects <- safely_detect_interactions(explainer, inter_param, inter_threshold, verbose)
  } else {
    interaction_effects <- NULL
  }


  safe_extractor <- list(explainer = explainer,
                         variables_info = variables_info,
                         interaction_effects = interaction_effects)
  class(safe_extractor) <- "safe_extractor"

  return(safe_extractor)

}


#' @title Plotting transformations of the SAFE extractor object
#'
#' @param x safe_extractor object containing information about variables transformations created with safe_extraction() function
#' @param ... other parameters
#' @param variable character, name of the variable to be plotted
#'
#' @return a plot object
#'
#' @export
#'
#' @import ggplot2
plot.safe_extractor <- function(x, ..., variable = NULL) {

  if (is.null(variable)) { #argument 'variable' not specified
    stop("Give a variable as an argument!")
  }
  if (! variable %in% names(x$variables_info)) { #wrong variable name
    stop("Wrong variable name!")
  }

  temp_info <- x$variables_info[[variable]]
  if (temp_info$type == "continuous") {
    p <- plot_continuous(temp_info, variable)
  } else {
    p <- plot_categorical(temp_info, variable)
  }

  return(p)

}


#' @title Printing summary of the SAFE extractor object
#'
#' @param x safe_extractor object containing information about variables transformations created with safe_extraction() function
#' @param ... other parameters
#' @param variable character, name of the variable to be plotted. If this argument
#' is not specified then transformations for all variables are printed
#'
#' @return NULL
#'
#' @export
print.safe_extractor <- function(x, ..., variable = NULL) {

  if (is.null(variable)) { #if 'variable' argument is not specified then transformations for all variables are printed
    for (var_temp in names(x$variables_info)) {
      print.safe_extractor(x, variable = var_temp)
    }
  } else {

    if (! variable %in% names(x$variables_info)) {
      stop("Wrong variable name!") #wrong variable name
    }

    cat(paste0("Variable '", variable, "'"))

    var_info <- x$variables_info[[variable]]
    if (is.null(var_info$new_levels)) {
      cat(" - no transformation suggested.\n")
    } else {
      if (var_info$type == "continuous") {
        cat(" - selected intervals:\n")
        cat(paste0(sapply(var_info$new_levels,
                          function(x) paste0("\t", x, "\n"))))
      } else {
        b <- var_info$new_levels
        b <- aggregate(b[,1], by = list(b[,2]), function(x) paste(x))
        cat(" - created levels:\n")
        for (i in 1:nrow(b)) {
          cat("\t")
          cat(cat(b$x[[i]], sep = ", "), "-> ", b$Group.1[i], "\n")
        }
      }
    }
  }

}




