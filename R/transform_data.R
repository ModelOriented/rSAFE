#' Features transformation using PDP plots
#'
#' The transform_data() function creates new variables
#' using PDP plots from DALEX package and adds them to the original ones.
#'
#' @param data data for which features values are to be transformed
#' @param propositions list of transformations functions and interactions returned by transform_propositons() function
#' @param user_interaction logical, TRUE allows the user to decide whether to keep the old variables after transformation
#' @param keep_old logical, if FALSE then old variables are removed from the dataset
#'
#' @return data with extra columns containing transformed variables
#'
#' @export


transform_data <- function(data, propositions, user_interaction = FALSE, keep_old = FALSE) {

  n <- length(propositions$transformations)
  if (n == 0) {
    cat("No transformations available.")
  }

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
      data <- merge(data, propositions$transformations[[i]], by = names(propositions$transformations)[i])
    }



  }

  # if (user_interaction == TRUE) {
  #   cat("Do you want to keep the old variables?")
  #   ans <- menu(c("Yes.", "No."))
  #   if (ans == 2) {
  #     data <- data[,!names(data) %in% names(transformations)]
  #   }
  # }
  if (keep_old == FALSE) {
    data <- data[,!names(data) %in% names(propositions$transformations)]
  }

  return(data)

}
