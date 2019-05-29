#' @title Continuous feature transformation using PDP plot
#'
#' @description The safely_transform_continuous() function calculates a transformation function
#' for the continuous variable using a PDP plot obtained from black box model.
#'
#' @param variable a feature for which the transformation function is to be computed
#' @param explainer DALEX explainer created with explain() function
#' @param response_type character, type of response to be calculated, one of: "pdp", "ale".
#' If features are uncorrelated, one can use "pdp" type - otherwise "ale" is strongly recommended.
#' @param penalty penalty for introducing another changepoint,
#' one of "AIC", "BIC", "SIC", "MBIC", "Hannan-Quinn" or numeric non-negative value
#' @param no_segments numeric, a number of segments variable is to be divided into in case of founding no breakpoints
#'
#' @return list of information on the transformation of given variable
#'
#' @export


safely_transform_continuous <- function(variable, explainer, response_type = "ale", penalty = "MBIC", no_segments = 2) {

  #calculating average responses of chosen type
  sv <- DALEX::variable_response(explainer, variable, type = response_type)

  #computing breakpoints
  break_points <- safely_detect_changepoints(sv$y, penalty, nquantiles = 10)

  if (length(break_points) == 0) { #no significant changes have been found
    #in this case we take a median as a breakpoint and create two intervals (as deafult)
    #or if no_segments arguments is specified then we take (no_segments-1) different quantiles and create no_segments intervals
    var_quantiles <- quantile(explainer$data[[variable]], probs = seq(0, 1, length.out = no_segments+1))
    break_points <- unique(as.vector(var_quantiles)[2:no_segments]) #removing first, last and repeating values
  } else {
    break_points <- sv$x[break_points]
  }

  #we use prettyNum() function to nicely format numbers that will be displeyed during intervals printing
  bp_pretty <- unique(prettyNum(break_points))

  if (length(break_points) == 1) {
    new_levels <- c(paste0("(-Inf, ", bp_pretty[1], "]"),
                    paste0("(", bp_pretty[1], ", Inf)"))
  } else {
    #in case there are many breakpoints first we check if they still will be unique after applying prettyNum() function
    if (length(bp_pretty) != length(break_points)) { #not all the 'pretty' breakpoints are unique so we go back to original formatting
      bp_pretty <- break_points
    }
    first_interval <- paste0("(-Inf, ", bp_pretty[1], "]")
    inner_interval_starts <- bp_pretty[1:(length(bp_pretty)-1)]
    inner_interval_ends <- bp_pretty[2:length(bp_pretty)]
    inner_intervals <- matrix(c(inner_interval_starts, inner_interval_ends), ncol = 2)
    inner_intervals <- apply(inner_intervals, 1, function(x) paste0("(", x[1], ", ", x[2], "]"))
    last_interval <- paste0("(", bp_pretty[length(bp_pretty)], ", Inf)")
    new_levels <- c(first_interval, inner_intervals, last_interval)
  }

  return(list(sv = sv,
              break_points = break_points,
              new_levels = new_levels))

}



