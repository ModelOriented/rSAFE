#' Continuous feature transformation using PDP plot
#'
#' The transform_continuous() function calculates a transformation function
#' for the continuous variable using a PDP plot from DALEX package.
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
    trans_coef <- as.factor(1:n_groups)
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
      return(as.factor(trans_coef[interv]))
    } else {
      return(trans_coef[interv,1] + trans_coef[interv,2] * value)
    }
  }


  #transformation propositions
  cat(paste0("Transformation for '", variable, "' variable:\n"))
  if (n_groups == 1) {
    if (type == "constant") {
      #cat(paste0("\t", trans_coef[1]))
      cat("No transformation.\n") #no transformation for 1 factor
      return(NULL)
    } else {
      if (trans_coef[1,1]<0) {
        sgn_coef <- " - "
      } else {
        sgn_coef <- " + "
      }
      cat(paste0("\t", trans_coef[1,2], " * ", variable, sgn_coef , abs(trans_coef[1,1])))
    }
  } else {
    for (i in 1:n_groups) {
      if (i == 1) {
        cat(paste0("\t<=", x_bounds[1], ":   "))
      } else if (i == n_groups) {
        cat(paste0("\t>", x_bounds[n_groups-1], ":   "))
      } else {
        cat(paste0("\t(", x_bounds[i-1], ", ", x_bounds[i], "]:   "))
      }
      if (type == "constant") {
        cat(paste0(as.numeric(trans_coef[i]), "\n"))
      } else {
        if (trans_coef[i,1]<0) {
          sgn_coef <- " - "
        } else {
          sgn_coef <- " + "
        }
        cat(paste0(trans_coef[i,2], " * ", variable, sgn_coef, abs(trans_coef[i,1]), "\n"))
      }
    }
  }




  #plot with breakpoints indicated
  if (plot == TRUE) {
    p <- ggplot(data = data.frame(cbind(sv$x, sv$y)), aes(x = sv$x, y = sv$y)) +
      geom_line() +
      labs(x = variable, y = "y")

    if (type != "constant") {
      for (i in 1:n_groups) {
        p <- p + geom_segment(data = data.frame(x1 = x_split[[i]][1], y1 = trans_coef[i,1] + trans_coef[i,2] * x_split[[i]][1],
                                                x2 = x_split[[i]][length(x_split[[i]])], y2 = trans_coef[i,1] + trans_coef[i,2] * x_split[[i]][length(x_split[[i]])]),
                              aes(x = x1, y = y1, xend = x2, yend = y2), colour = "red", size = 1.5)
      }
    }

    if (n_groups > 1) {
      for (i in 1:(n_groups-1)) {
        p <- p + geom_vline(xintercept = x_bounds[i], colour = "blue", size = 1, linetype = "dotted")
      }
    }

    plot(p)

  }


  #transformation function for given variable
  return(transformation)


}

