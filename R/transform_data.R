#' Features transformation using PDP plots
#'
#' The transform_data() function creates new variables
#' using PDP plots from DALEX package and adds them to the original ones.
#'
#' @param data data for which features values are to be transformed
#' @param propositions list of transformations functions and interactions returned by transform_propositons() function
#' @param which_variables allows user to decide which variables keep, one of: "all", "only_new", "aic"
#'
#' @return data with extra columns containing transformed variables
#'
#' @export


transform_data <- function(data, y, propositions, which_variables = "only_new") {

  n <- length(propositions$transformations)
  if (n == 0) {
    cat("No transformations available.")
    return(data)
  }

  ind <- data.frame('ind' = 1:nrow(data)) #column created to maintain rows order
  data <- cbind(ind, data)

  for (i in 1:n) {
    if (is.function(propositions$transformations[[i]])) { #continuous

      new_variable <- rep(0, nrow(data))
      which_var <- which(colnames(data) == names(propositions$transformations)[i])
      for (j in 1:nrow(data)) {
        new_variable[j] <- propositions$transformations[[i]](data[j,which_var])
      }
      if (is.factor(propositions$transformations[[i]](data[1,which_var]))) {
        new_variable <- as.factor(new_variable)
      }
      data <- cbind(data, new_variable)
      colnames(data)[ncol(data)] <- paste0(names(propositions$transformations)[i], "_new")

    } else { #factor
      data <- merge(x = data, y = propositions$transformations[[i]], by = names(propositions$transformations)[i])
    }

  }

  # if (user_interaction == TRUE) {
  #   cat("Do you want to keep the old variables?")
  #   ans <- menu(c("Yes.", "No."))
  #   if (ans == 2) {
  #     data <- data[,!names(data) %in% names(transformations)]
  #   }
  # }

  # if (keep_old == FALSE) { #deleting old variables from dataset
  #   data <- data[,!names(data) %in% names(propositions$transformations)]
  # }

  data <- data[order(data$ind), colnames(data) != "ind"] #restoring rows order after merge
  rownames(data) <- 1:nrow(data)

  if (which_variables == "only_new") { #deleting old variables from dataset
    data <- data[,!names(data) %in% names(propositions$transformations)]
  } else if (which_variables == "aic") {
    model_lm <- lm(y ~ ., data = cbind(data,y))
    sel <- stats::step(model_lm, direction = 'backward')
    data <- data[,names(data) %in% attr(sel$terms, "term.labels")]
  }


  return(data)

}
