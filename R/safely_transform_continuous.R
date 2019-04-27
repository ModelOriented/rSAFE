#' Continuous feature transformation using PDP plot
#'
#' The safely_transform_continuous() function calculates a transformation function
#' for the continuous variable using a PDP plot obtained from black box model.
#'
#' @param variable a feature for which the transformation function is to be computed
#' @param explainer DALEX explainer created with explain() function
#' @param package character, a package used to compute breakpoints, one of: "changepoint.np", "strucchange"
#' @param response_type character, type of response to be calculated, one of: "pdp", "ale".
#' If features are uncorrelated, one can use "pdp" type - otherwise "ale" is strongly recommended.
#'
#' @return list of information on the transformation of given variable
#'
#' @export


safely_transform_continuous <- function(variable, explainer, package = "changepoint.np", response_type = "ale") {

  #calculating average responses of chosen type
  sv <- DALEX::variable_response(explainer, variable, type = response_type)

  min_seg_length <- 3 #at least 3 observations in each segment

  #computing breakpoints
  if (package == "changepoint.np") {
    break_points <- changepoint::cpts(changepoint.np::cpt.np(sv$y, minseglen = min_seg_length))
  } else {
    break_points <- strucchange::breakpoints(ts(sv$y) ~ 1, h = min_seg_length)$breakpoints
  }

  if (length(break_points) == 0) { #no significant changes have been found
    # break_points <- NULL
    # new_levels <- NULL
    #in this case we take a median as a breakpoint and create two intervals
    break_points <- median(explainer$data[[variable]])
    #we use prettyNum() function to nicely format numbers that will be displeyed during intervals printing
    new_levels <- c(paste0("(-Inf, ", prettyNum(break_points[1]), ")"),
                    paste0("[", prettyNum(break_points[1]), ", Inf)"))

  } else {
    break_points <- sv$x[break_points]
    if (length(break_points) == 1) {
      #we use prettyNum() function to nicely format numbers that will be displeyed during intervals printing
      new_levels <- c(paste0("(-Inf, ", prettyNum(break_points[1]), ")"),
                      paste0("[", prettyNum(break_points[1]), ", Inf)"))
    } else {
      #in case there are many breakpoints first we check if they still will be unique after applying prettyNum() function
      bp_pretty <- unique(prettyNum(break_points))
      if (length(bp_pretty) == length(break_points)) { #all 'pretty' breakpoints are unique so they can be used to create intervals
        new_levels <- c(paste0("(-Inf, ", prettyNum(break_points[1]), ")"),
                        apply(matrix(c(prettyNum(break_points[1:(length(break_points)-1)]),prettyNum(break_points[2:length(break_points)])), ncol = 2), 1, function(x) paste0("[", x[1], ", ", x[2], ")")),
                        paste0("[", prettyNum(break_points[length(break_points)]), ", Inf)"))
      } else { #not all the 'pretty' breakpoints are unique so we leave original formatting
        new_levels <- c(paste0("(-Inf, ", break_points[1], ")"),
                        apply(matrix(c(break_points[1:(length(break_points)-1)],break_points[2:length(break_points)]), ncol = 2), 1, function(x) paste0("[", x[1], ", ", x[2], ")")),
                        paste0("[", break_points[length(break_points)], ", Inf)"))
      }


    }
  }

  return(list(sv = sv,
              break_points = break_points,
              new_levels = new_levels))



}



