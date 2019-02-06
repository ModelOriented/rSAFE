#' Factor feature transformation using factorMerger package
#'
#' The transform_factor() function calculates a transformation function
#' for the factor variable using the getOptimalPartitionDf() function from factorMerger package.
#'
#' @param variable a feature for which the transformation function is to be computed
#' @param explainer DALEX explainer created with explain() function
#'
#' @return list of transformation information on the given variable
#'
#' @export


transform_factor <- function(variable, explainer) {

  #calculating average responses of chosen type
  sv <- DALEX::variable_response(explainer, variable, type = "factor")
  new_levels <- factorMerger::getOptimalPartitionDf(sv)

  if (length(levels(new_levels$pred)) == 1) {
    new_levels <- NULL
  } else {
    colnames(new_levels) <- c(variable, paste0(variable, "_new"))
    rownames(new_levels) <- 1:nrow(new_levels)
  }

  return(list(sv = sv,
              new_levels = new_levels))


}



