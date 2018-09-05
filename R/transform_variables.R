#' Features transformation using PDP plots
#'
#' The transform_variables() function creates new variables
#' using PDP plots from DALEX package and adds them to the original ones.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param data data for which features values are to be transformed
#' @param package a package used to compute breakpoints, one of: "changepoint.np", "strucchange"
#' @param type a transformation method, one of: "constant", "linear"
#' @param plot logical, if TRUE then plots breakpoints and segments means
#'
#' @return data with extra columns containing transformed variables
#'
#' @export


transform_variables <- function(explainer, data, package = "changepoint.np", type = "constant", plot = FALSE) {
  #only for continuous so far

  if (class(explainer) != "explainer") {
    stop(paste0("No applicable method for 'transform_step' applied to an object of class '", class(explainer), "'."))
  }

  if (! package %in% c("changepoint.np", "strucchange")) {
    stop("Wrong package.")
  }

  if (! type %in% c("constant", "linear")) {
    stop("Wrong type.")
  }


  n <- ncol(data)

  for (i in 1:n) {

    if (!is.factor(data[,i])) {
      trans_function <- transform_continuous(explainer, colnames(data)[i], package, type, plot)
      new_variable <- rep(0, nrow(data))
      for (j in 1:nrow(data)) {
        new_variable[j] <- trans_function(data[j,i])
      }
      data <- cbind(data, new_variable)
      colnames(data)[ncol(data)] <- paste0(colnames(data)[i], "_new")
    }

  }

  return(data)

}
