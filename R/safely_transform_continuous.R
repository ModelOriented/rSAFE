#' @title Calculating a Transformation of a Continuous Feature Using PDP/ALE Plot
#'
#' @description The safely_transform_continuous() function calculates a transformation function
#' for the continuous variable using a PD/ALE plot obtained from black box model.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param variable a feature for which the transformation function is to be computed
#' @param response_type character, type of response to be calculated, one of: "pdp", "ale".
#' If features are uncorrelated, one can use "pdp" type - otherwise "ale" is strongly recommended.
#' @param grid_points number of points on x-axis used for creating the PD/ALE plot, default 50
#' @param N number of observations from the dataset used for creating the PD/ALE plot, default 200
#' @param penalty penalty for introducing another changepoint,
#' one of "AIC", "BIC", "SIC", "MBIC", "Hannan-Quinn" or numeric non-negative value
#' @param nquantiles the number of quantiles used in integral approximation
#' @param no_segments numeric, a number of segments variable is to be divided into in case of founding no breakpoints
#'
#' @return list of information on the transformation of given variable
#'
#' @seealso \code{\link{safe_extraction}}, \code{\link{safely_detect_changepoints}}
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
#' safely_transform_continuous(explainer_rf, "construction.year")
#'
#' @importFrom ingredients accumulated_dependence partial_dependence
#'
#' @export

safely_transform_continuous <- function(explainer, variable, response_type = "ale", grid_points = 50, N = 200, penalty = "MBIC", nquantiles = 10, no_segments = 2) {

  if (class(explainer) != "explainer") {
    stop(paste0("No applicable method for 'safely_transform_continuous' applied to an object of class '", class(explainer), "'."))
  }
  if (! variable %in% colnames(explainer$data)) {
    stop("Wrong variable name!")
  }
  if (! response_type %in% c("pdp", "ale")) {
    warning("Wrong type of response - using default one.")
    response_type <- "ale"
  }
  if (!is.numeric(no_segments) | no_segments%%1!=0 | no_segments<2) {
    warning("Wrong number of segments - using default one.")
    no_segments <- 2
  }

  #calculating average responses of chosen type
  set.seed(123) #functions from ingredients contain sampling
  if (response_type == "ale") {
    sv <- accumulated_dependence(explainer, variables = variable, grid_points = grid_points, N = N)
  } else {
    sv <- partial_dependence(explainer, variables = variable, grid_points = grid_points, N = N)
  }

  #if the variable is a factor with two values (but is regarded as a continuous feature) we do not transform it
  if (length(sv$`_x_`) <= 2) {
    return(list(sv = sv,
                break_points = NULL,
                new_levels = NULL))
  }

  #computing breakpoints
  break_points <- safely_detect_changepoints(sv$`_yhat_`, penalty, nquantiles = nquantiles)

  if (length(break_points) == 0) { #no significant changes have been found
    #in this case we take a median as a breakpoint and create two intervals (as deafult)
    #or if no_segments arguments is specified then we take (no_segments-1) different quantiles and create no_segments intervals
    var_quantiles <- quantile(explainer$data[,variable], probs = seq(0, 1, length.out = no_segments+1))
    break_points <- unique(as.vector(var_quantiles)[2:no_segments]) #removing first, last and repeating values
  } else {
    break_points <- sv$`_x_`[break_points]
  }

  new_levels <- pretty_intervals(break_points)

  return(list(sv = sv,
              break_points = break_points,
              new_levels = new_levels))

}


pretty_intervals <- function(break_points) {

  if (is.null(break_points)) {
    stop("No data provided!")
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

  return(new_levels)

}

#' @importFrom ggplot2 geom_vline
plot_continuous <- function(temp_info, variable) {
  p <- plot(temp_info$sv)
  #adding breakpoints to the pdp/ale plot
  temp_bp <- temp_info$break_points
  if (!is.null(temp_bp)) {
    for (i in 1:length(temp_bp)) {
      p <- p + geom_vline(xintercept = temp_bp[i], colour = "red", size = 1, linetype = "dotted")
    }
  } else {
    cat(paste0("No transformation for '", variable, "'."))
  }
  return(p)
}











