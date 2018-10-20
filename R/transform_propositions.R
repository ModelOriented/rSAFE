#' Features transformation using PDP plots
#'
#' The transform_propositions() function computes transformation functions
#' for all continuous variables using PDP plots from DALEX package.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param package a package used to compute breakpoints, one of: "changepoint.np", "strucchange"
#' @param type a transformation method, one of: "constant", "linear"
#' @param plot logical, if TRUE then plots breakpoints and segments means
#' @param user_interaction logical, TRUE allows the user to decide which of the transformations keep
#' @param interactions logical, if TRUE then H-statistics corresponding to interactions are computed for all pairs of variables
#'
#' @return list of transformations propositions and possible interactions between variables
#'
#' @export


transform_propositions <- function(explainer, package = "changepoint.np", type = "constant", plot = FALSE, user_interaction = FALSE, interactions = FALSE) {

  if (class(explainer) != "explainer") {
    stop(paste0("No applicable method for 'transform_step' applied to an object of class '", class(explainer), "'."))
  }

  if (! package %in% c("changepoint.np", "strucchange")) {
    stop("Wrong package.")
  }

  if (! type %in% c("constant", "linear")) {
    stop("Wrong type.")
  }


  n <- ncol(explainer$data)
  #prop <- vector("list", n) #list of transformations propositions
  #names(prop) <- colnames(explainer$data)
  prop <- vector("list", 2)
  prop[[1]] <- vector("list", n)
  names(prop) <- c("transformations", "interactions")

  cat("These are the propositions for variables transformations:\n")
  for (i in 1:n) {

    cat("\n")
    if (!is.factor(explainer$data[,i])) {
      prop$transformations[[i]] <- transform_continuous(explainer, colnames(explainer$data)[i], package, type, plot)
    } else {
      prop$transformations[[i]] <- transform_factor(explainer, colnames(explainer$data)[i], plot)
    }

  }

  names(prop$transformations) <- colnames(explainer$data)

  prop_null <- c()
  for (i in 1:length(prop$transformations)) {
    if (is.null(prop$transformations[[i]])) {
      prop_null <- c(prop_null, i)
    }
  }
  if (length(prop_null) > 0) {
    prop$transformations <- prop$transformations[-prop_null]
  }


  if (user_interaction == TRUE) {
    cat("\n\nDo you want to maintain the transformations?")
    ans <- menu(c("Yes.", "Only selected.", "No."))
    if (ans == 2) {
      cat("\nDecide which of the transformations you would like to keep by listing variables names, e.g. 'name1', 'name2'.")
      ans2 <- stringr::str_extract_all(readline(), "\'.+?\'")[[1]]
      if (length(ans2) == 0) {
        cat("Wrong answer.")
        prop$transformations <- NULL
      }
      which_keep <- c()
      for (v in ans2) {
        var_name <- substr(v, 2, nchar(v)-1)
        if (! var_name %in% names(prop$transformations)) {
          cat("Wrong variables names.")
          prop$transformations <- NULL
        }
        which_keep <- c(which_keep, var_name)
      }
      prop$transformations <- prop$transformations[which_keep]
    } else if (ans == 3) {
      prop$transformations <- NULL
    }
  }

  #interactions between variables
  if (interactions == TRUE & n > 1) {
    cat("\n\nConsider also adding to a model following interactions between variables (this may take a while):\n")
    predictor <- Predictor$new(explainer$model, data = explainer$data, y = explainer$y)
    prop$interactions <- matrix(rep(0, n*n), ncol = n) #matrix of H-statistics
    rownames(prop$interactions) <- colnames(explainer$data)
    colnames(prop$interactions) <- colnames(explainer$data)
    if (n == 2) {
      set.seed(111)
      interact <- Interaction$new(predictor)
      f <- unique(interact$results$.feature)
      for (j in 1:nrow(interact$results)) {
        if (! is.na(interact$results$.interaction[j])) {
          if (interact$results$.interaction[j] > prop$interactions[f[1], f[2]]) {
            prop$interactions[f[1], f[2]] <- interact$results$.interaction[j]
            prop$interactions[f[2], f[1]] <- interact$results$.interaction[j]
          }
        }

      }
    } else {
      for (i in 1:n) {
        set.seed(111)
        interact <- Interaction$new(predictor, feature = colnames(explainer$data)[i])
        for (j in 1:nrow(interact$results)) {
          if (! is.na(interact$results$.interaction[j])) {
            f <- strsplit(interact$results$.feature[j], ":")[[1]]
            if (interact$results$.interaction[j] > prop$interactions[f[1], f[2]]) {
              prop$interactions[f[1], f[2]] <- interact$results$.interaction[j]
              prop$interactions[f[2], f[1]] <- interact$results$.interaction[j]
            }
          }
        }
      }
    }

    interactions_detected <- FALSE
    for (i in 1:n) {
      for (j in 1:i) {
        if (prop$interactions[i,j] > 0.5) {
          interactions_detected <- TRUE
          cat(paste0(colnames(explainer$data)[i], ":", colnames(explainer$data)[j]))
          cat("\n")
        }
      }
    }
    if (interactions_detected == FALSE) {
      cat("\nNo interactions detected.")
    }
  }

  return(prop)

}
