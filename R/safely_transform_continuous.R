#' Continuous feature transformation using PDP plot
#'
#' The transform_continuous() function calculates a transformation function
#' for the continuous variable using a PDP plot from DALEX package.
#'
#' @param variable a feature for which the transformation function is to be computed
#' @param explainer DALEX explainer created with explain() function
#' @param package character, a package used to compute breakpoints, one of: "changepoint.np", "strucchange"
#' @param response_type character, type of response to be calculated, one of: "pdp", "ale".
#' If features are uncorrelated, one can use "pdp" type - otherwise "ale" is strongly recommended.
#'
#' @return list of transformation information on the given variable
#'
#' @export


safely_transform_continuous <- function(variable, explainer, package = "changepoint.np", response_type = "pdp") {

  #calculating average responses of chosen type
  sv <- DALEX::variable_response(explainer, variable, type = response_type)

  min_seg_length <- 3 #at least 3 observations in each segment

  #computing breakpoints
  if (package == "changepoint.np") {
    break_points <- changepoint::cpts(changepoint.np::cpt.np(sv$y, minseglen = min_seg_length))
  } else if (package == "strucchange") {
    break_points <- strucchange::breakpoints(ts(sv$y) ~ 1, h = min_seg_length)$breakpoints
  }

  if (length(break_points) == 0) {
    break_points <- NULL
    new_levels <- NULL
  } else {
    break_points <- sv$x[break_points]
    if (length(break_points) == 1) {
      new_levels <- c(paste0("(-Inf, ", break_points[1], ")"),
                      paste0("[", break_points[1], ", Inf)"))
    } else {
      new_levels <- c(paste0("(-Inf, ", break_points[1], ")"),
                      apply(matrix(c(break_points[1:(length(break_points)-1)],break_points[2:length(break_points)]), ncol = 2), 1, function(x) paste0("[", x[1], ", ", x[2], ")")),
                      paste0("[", break_points[length(break_points)], ", Inf)"))
    }
  }

  return(list(sv = sv,
              break_points = break_points,
              new_levels = new_levels))



}



