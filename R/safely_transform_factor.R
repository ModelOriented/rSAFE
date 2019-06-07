#' @title Factor feature transformation using factorMerger package
#'
#' @description The safely_transform_factor() function calculates a transformation function
#' for the factor variable using the getOptimalPartitionDf() function from factorMerger package.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param variable a feature for which the transformation function is to be computed
#'
#' @return list of information on the transformation of given variable
#'
#' @export

safely_transform_factor <- function(explainer, variable) {

  if (class(explainer) != "explainer") {
    stop(paste0("No applicable method for 'safe_extraction' applied to an object of class '", class(explainer), "'."))
  }
  if (! variable %in% colnames(explainer$data)) {
    stop("Wrong variable name!")
  }

  #calculating average responses
  sv <- DALEX::variable_response(explainer, variable, type = "factor")
  new_levels <- factorMerger::getOptimalPartitionDf(sv)

  if (length(levels(new_levels$pred)) %in% c(1, nrow(new_levels))) { #no appropriate merge methods have been found
    new_levels <- NULL
  } else {
    colnames(new_levels) <- c(variable, paste0(variable, "_new"))
    rownames(new_levels) <- 1:nrow(new_levels)
  }

  return(list(sv = sv,
              new_levels = new_levels))

}






