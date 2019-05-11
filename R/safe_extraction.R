#' Creating SAFE extractor - an object used for surrogate-assisted feature extraction
#'
#' The safe_extractiob() function creates a SAFE-extractor object which may be used later
#' for surrogate feature extraction.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param package character, a package used to compute breakpoints in case of continuous variables,
#' one of: "breakpoint", "strucchange"
#' @param response_type character, type of response to be calculated, one of: "pdp", "ale".
#' If features are uncorrelated, one can use "pdp" type - otherwise "ale" is strongly recommended.
#' @param no_segments numeric, a number of segments variable is to be divided into in case of founding no breakpoints
#' @param verbose logical, if progress bar is to be printed
#'
#' @return safe_extractor object containing information about variables transformation
#'
#' @export
#'
#'

safe_extraction <- function(explainer, package = "breakpoint", response_type = "ale", no_segments = 2, verbose = TRUE) {

  if (class(explainer) != "explainer") {
    stop(paste0("No applicable method for 'safe_extraction' applied to an object of class '", class(explainer), "'."))
  }
  if (! package %in% c("breakpoint", "strucchange")) {
    warning("Wrong package - using default one.")
    package <- "breakpoint"
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
  vars <- vector("list", length = p)
  names(vars) <- term_names

  #checking if the feature is categorical or numerical
  for (v in names(vars)) {
    if (is.factor(explainer$data[1,v])) {
      vars[[v]] <- list(type = "categorical")
    } else {
      vars[[v]] <- list(type = "numerical")
    }
  }


  # vars <- data.frame(variable = colnames(explainer$data),
  #                    type = sapply(explainer$data[1,], function(x) if(is.factor(x)) 'categorical' else 'numerical'))

  # transformations proposition
  if (verbose == TRUE) {
    #progress bar - to let the user know how many variables have been already processed
    pb <- utils::txtProgressBar(min = 0, max = length(names(vars)), style = 3)
  }

  for (v in names(vars)) {

    # if (verbose == TRUE) {
    #   cat(paste0("Variable processed: ", v, "\n"))
    # }

    if (vars[[v]]$type == "categorical") {
      trans_prop <- safely_transform_factor(v, explainer)
      vars[[v]]$new_levels <- trans_prop$new_levels
    } else {
      trans_prop <- safely_transform_continuous(v, explainer, package, response_type, no_segments)
      vars[[v]]$break_points <- trans_prop$break_points
      vars[[v]]$new_levels <- trans_prop$new_levels
    }
    vars[[v]]$sv <- trans_prop$sv

    #updating progress bar
    if (verbose == TRUE) {
      utils::setTxtProgressBar(pb, which(names(vars) == v))
    }

  }

  #closing progress bar
  if (verbose == TRUE) {
    close(pb)
  }




  safe_extractor <- list(explainer = explainer,
                         package = package,
                         vars = vars
                         )
  class(safe_extractor) <- "safe_extractor"

  return(safe_extractor)

}


#' @export

plot.safe_extractor <- function(safe_extractor, variable = NULL) {

  if (is.null(variable)) { #argument 'variable' not specified
    cat("Give a variable as an argument!")
    return(NULL)
  }
  if (! variable %in% names(safe_extractor$vars)) { #wrong variable name
    return(NULL)
  }

  var_info <- safe_extractor$vars[[variable]]
  p <- plot(var_info$sv)

  # adding breakpoints to the pdp/ale plot
  if (var_info$type == "numerical") {
    if (!is.null(var_info$break_points)) {
      for (i in 1:length(var_info$break_points)) {
        p <- p + ggplot2::geom_vline(xintercept = var_info$break_points[i], colour = "blue", size = 1, linetype = "dotted")
      }
    }
  }

  p

}

#' @export


print.safe_extractor <- function(safe_extractor, variable = NULL) {

  if (is.null(variable)) { #if 'variable' argument is not specified then transformations for all variables are printed
    for (v in names(safe_extractor$vars)) {
      print.safe_extractor(safe_extractor, v)
    }
  } else {

    if (! variable %in% names(safe_extractor$vars)) {
      return(NULL) #wrong variable name
    }

    cat(paste0("Variable '", variable, "'"))

    var_info <- safe_extractor$vars[[variable]]

    if (var_info$type == "numerical") {

      if (is.null(var_info$break_points)) {
        cat(" - no transformation suggested.\n")
      } else {
        # b <- safe_extractor$vars[[variable]]$break_points
        cat(" - selected intervals:\n")
        cat(paste0(sapply(var_info$new_levels,
                   function(x) paste0("\t", format(x, digits=2, nsmall=2), "\n"))))
        # improve printing numbers - TODO!!!


        # cat(paste0("\t(-Inf, ", format(b[1], digits=2, nsmall=2), ")\n"))
        # if (length(b) > 1) {
        #   for (i in 1:(length(b)-1)) {
        #     cat(paste0("\t[", format(b[i], digits=2, nsmall=2), ", ",
        #                format(b[i+1], digits=2, nsmall=2), ")\n"))
        #   }
        # }
        # cat(paste0("\t[", format(b[length(b)], digits=2, nsmall=2), ", Inf)\n"))
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




