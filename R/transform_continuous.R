#' Continuous feature transformation using PDP plot
#'
#' The transform_continuous() function calculates a transformation function
#' for the continuous variable using PDP plots from DALEX package.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param variable a feature for which the transformation function is to be computed
#' @param package a package used to compute breakpoints, one of: "changepoint.np", "strucchange"
#' @param type a transformation method, one of: "constant", "linear"
#' @param plot logical, if TRUE then plots breakpoints and segments means
#'
#' @return transformation function for the given variable
#'
#' @export


transform_continuous <- function(explainer, variable, package = "changepoint.np", type = "constant", plot = FALSE) {

  if (class(explainer) != "explainer") {
    stop(paste0("No applicable method for 'transform_step' applied to an object of class '", class(explainer), "'."))
  }

  if (! variable %in% colnames(explainer$data)) {
    stop(paste0("Variable '", variable, "' not found."))
  }

  if (is.factor(explainer$data[, which(colnames(explainer$data) == variable)])) {
    stop(paste0("Factor variable, use transform_factor() function."))
  }

  if (! package %in% c("changepoint.np", "strucchange")) {
    stop("Wrong package.")
  }

  if (! type %in% c("constant", "linear")) {
    stop("Wrong type.")
  }


  sv <- single_variable(explainer, variable, type = "pdp")

  min_seg_length <- 3 #at least 3 observations in each segment

  #computing breakpoints
  if (package == "changepoint.np") {
    break_points <- cpts(cpt.np(sv$y, minseglen = min_seg_length))
  } else if (package == "strucchange" & type == "constant") {
    break_points <- breakpoints(ts(sv$y) ~ 1, h = min_seg_length)$breakpoints
  } else {
    break_points <- breakpoints(ts(sv$y) ~ sv$x, h = min_seg_length)$breakpoints
  }

  n_groups <- length(break_points) + 1

  #computing segments bounds
  x_bounds <- rep(0, n_groups-1)
  if (length(x_bounds) > 0) {
    for (i in 1:(n_groups-1)) {
      x_bounds[i] <- sv$x[break_points[i]]
    }
  }


  y_split <- vector("list", length = n_groups)
  x_split <- vector("list", length = n_groups)
  if (n_groups == 1) {
    y_split[[1]] <- sv$y
    x_split[[1]] <- sv$x
  } else {
    for (i in 1:n_groups) {

      if (i == 1) {
        ind <- 1:break_points[i]
      } else if (i == n_groups) {
        ind <- (break_points[i-1]+1):length(sv$x)
      } else {
        ind <- (break_points[i-1]+1):break_points[i]
      }

      y_split[[i]] <- sv$y[ind]
      x_split[[i]] <- sv$x[ind]

    }
  }

  #transformation function coefficients
  if (type == "constant") {
    trans_coef <- rep(0, n_groups)
    for (i in 1:n_groups) {
      trans_coef[i] <- mean(y_split[[i]])
    }
  } else {
    trans_coef <- matrix(rep(0, 2*n_groups), ncol = 2)
    for (i in 1:n_groups) {
      lm_model <- lm(y_split[[i]] ~ x_split[[i]])
      trans_coef[i,] <- coef(lm_model)
    }
  }

  #transformation function
  transformation <- function (value) {
    if (n_groups == 1) {
      interv <- 1
    } else {
      interv <- which(value <= x_bounds)[1]
      if (is.na(interv)) {
        interv <- n_groups
      }
    }
    if (type == "constant") {
      return(trans_coef[interv])
    } else {
      return(trans_coef[interv,1] + trans_coef[interv,2] * value)
    }
  }


  if (plot == TRUE) {
    plot(sv$x, sv$y, type = "l", xlab = variable, ylab = "y")
    if (type == "constant") {
      for (i in 1:n_groups) {
        segments(x_split[[i]][1], trans_coef[i],
                 x_split[[i]][length(x_split[[i]])], trans_coef[i],
                 col = "red", lwd = 3)
      }
    } else {
      for (i in 1:n_groups) {
        segments(x_split[[i]][1], trans_coef[i,1] + trans_coef[i,2] * x_split[[i]][1],
                 x_split[[i]][length(x_split[[i]])], trans_coef[i,1] + trans_coef[i,2] * x_split[[i]][length(x_split[[i]])],
                 col = "red", lwd = 3)
      }
    }

    if (n_groups > 1) {
      for (i in 1:(n_groups-1)) {
        abline(v = x_bounds[i], col = "blue", lty = "dashed")
      }
    }

  }



  return(transformation)


}

