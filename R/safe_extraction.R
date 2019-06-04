#' @title Creating SAFE extractor - an object used for surrogate-assisted feature extraction
#'
#' @description The safe_extraction() function creates a SAFE-extractor object which may be used later
#' for surrogate feature extraction.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param response_type character, type of response to be calculated, one of: "pdp", "ale".
#' If features are uncorrelated, one can use "pdp" type - otherwise "ale" is strongly recommended.
#' @param penalty penalty for introducing another changepoint,
#' one of "AIC", "BIC", "SIC", "MBIC", "Hannan-Quinn" or numeric non-negative value
#' @param no_segments numeric, a number of segments variable is to be divided into in case of founding no breakpoints
#' @param interactions logical, if interactions between variables are to be taken into account
#' @param inter_param numeric, a positive value indicating which of single observation non-additive effects
#' are to be regarded as significant, the higher value the higher non-additive effect has to be to be taken
#' into account
#' @param inter_threshold numeric, a value from `[0,1]` interval indicating which interactions should be returned
#' as significant. It corresponds to the percentage of observations for which interaction measure is greater
#' than inter_param - if this percentage is less than inter_threshold then interaction effect is ignored.
#' @param verbose logical, if progress bar is to be printed
#'
#' @seealso \code{\link{safely_transform_factor}}, \code{\link{safely_transform_continuous}}, \code{\link{safely_detect_interactions}}
#'
#' @return safe_extractor object containing information about variables transformation
#' @importFrom graphics plot
#' @importFrom stats AIC aggregate binomial quantile
#' @export

safe_extraction <- function(explainer, response_type = "ale", penalty = "MBIC", no_segments = 2, interactions = TRUE, inter_param = 2, inter_threshold = 0.4, verbose = TRUE) {

  if (class(explainer) != "explainer") {
    stop(paste0("No applicable method for 'safe_extraction' applied to an object of class '", class(explainer), "'."))
  }
  if (! response_type %in% c("pdp", "ale")) {
    warning("Wrong type of response - using default one.")
    response_type <- "ale"
  }

  #explainer$data might contain also a response variable - we use model attributes to get the predictors
  term_names <- attr(explainer$model$terms, "term.labels")

  p <- length(term_names) #number of variables in dataset
  n <- nrow(explainer$data) #number of observations in dataset

  #list with all variables and information on them
  variables_info <- vector("list", length = p)
  names(variables_info) <- term_names

  #checking if the feature is categorical or numerical
  for (var_temp in term_names) {
    if (is.factor(explainer$data[1,var_temp])) {
      variables_info[[var_temp]] <- list(type = "categorical")
    } else {
      variables_info[[var_temp]] <- list(type = "numerical")
    }
  }

  # transformations proposition
  if (verbose == TRUE) {
    if (interactions == TRUE) {
      cat("Single variables processing...\n")
    }
    #progress bar - to let the user know how many variables have been already processed
    pb <- utils::txtProgressBar(min = 0, max = length(term_names), style = 3)
  }

  for (var_temp in term_names) {

    temp_info <- variables_info[[var_temp]] #information on currently considered variable

    if (temp_info$type == "categorical") {
      trans_prop <- safely_transform_factor(explainer, var_temp)
      temp_info$new_levels <- trans_prop$new_levels
    } else {
      trans_prop <- safely_transform_continuous(explainer, var_temp, response_type, penalty, no_segments)
      temp_info$break_points <- trans_prop$break_points
      temp_info$new_levels <- trans_prop$new_levels
    }
    temp_info$sv <- trans_prop$sv

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
  }


  safe_extractor <- list(explainer = explainer,
                         variables_info = variables_info,
                         interaction_effects = interaction_effects)
  class(safe_extractor) <- "safe_extractor"

  return(safe_extractor)

}


#' @title safe_extractor plot
#'
#' @param x safe_extractor object containing information about variables transformations created with safe_extraction() function
#' @param ... other parameters
#' @param variable character, name of the variable to be plotted
#'
#' @return a plot object
#'
#' @export
plot.safe_extractor <- function(x, ..., variable = NULL) {

  if (is.null(variable)) { #argument 'variable' not specified
    cat("Give a variable as an argument!")
    return(NULL)
  }
  if (! variable %in% names(x$variables_info)) { #wrong variable name
    return(NULL)
  }

  temp_info <- x$variables_info[[variable]]
  p <- plot(temp_info$sv)

  # adding breakpoints to the pdp/ale plot
  if (temp_info$type == "numerical") {
    temp_bp <- temp_info$break_points
    if (!is.null(temp_bp)) {
      for (i in 1:length(temp_bp)) {
        p <- p + ggplot2::geom_vline(xintercept = temp_bp[i], colour = "red", size = 1, linetype = "dotted")
      }
    }
  }

  return(p)

}


#' @title Printing safe_extractor summary
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
      return(NULL) #wrong variable name
    }

    cat(paste0("Variable '", variable, "'"))

    var_info <- x$variables_info[[variable]]

    if (var_info$type == "numerical") {

      if (is.null(var_info$break_points)) {
        cat(" - no transformation suggested.\n")
      } else {
        cat(" - selected intervals:\n")
        cat(paste0(sapply(var_info$new_levels,
                          function(x) paste0("\t", format(x, digits=2, nsmall=2), "\n"))))
      }

    } else {

      if (is.null(var_info$new_levels)) {
        cat(" - no transformation suggested.\n")
      } else {
        b <- var_info$new_levels
        b <- aggregate(b[,1], by = list(b[,2]), function(x) paste(x))
        cat(" - created levels:\n")
        for (i in 1:nrow(b)) {
          cat(paste0("\t", b[i,2], " -> ", b[i,1], "\n"))
        }
      }
    }
  }
}




