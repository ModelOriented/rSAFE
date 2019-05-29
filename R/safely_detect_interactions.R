#' @title Detecting interactions via a permutation approach
#'
#' @description The safely_detect_interactions() function detects second-order interactions based on predictions
#' made by a surrogate model. For each pair of features it performs values permutation in order
#' to evaluate their non_additive effect.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param inter_param numeric, a positive value indicating which of single observation non-additive effects
#' are to be regarded as significant, the higher value the higher non-additive effect has to be to be taken
#' into account
#' @param inter_threshold numeric, a value from [0,1] interval indicating which interactions should be returned
#' as significant. It corresponds to the percentage of observations for which interaction measure is greater
#' than inter_param - if this percentage is less than inter_threshold then interaction effect is ignored.
#' @param verbose logical, if progress bar is to be printed
#'
#' @seealso \code{\link{interaction_measure}}
#'
#' @return dataframe object containing interactions effects greater than or equal to the specified inter_threshold
#'
#' @export


safely_detect_interactions <- function(explainer, inter_param = 2, inter_threshold = 0.4, verbose = TRUE) {

  term_names <- attr(explainer$model$terms, "term.labels")
  p <- length(term_names)

  #creating all 2-elements subsets of term_names vector
  no_pairs <- p*(p-1)/2
  interactions_strength <- as.data.frame(matrix(rep("", 2*no_pairs), ncol = 2))
  levels(interactions_strength[,1]) <- term_names
  levels(interactions_strength[,2]) <- term_names
  interaction_values <- as.data.frame(matrix(rep(0, no_pairs), ncol = 1))

  if (verbose == TRUE) {
    #progress bar - to let the user know how many interactions have been already processed
    cat("Interactions processing...\n")
    pb <- utils::txtProgressBar(min = 0, max = no_pairs, style = 3)
  }

  i <- 1
  for (j1 in 1:p) {
    if (j1 == p) {
      break
    }
    for (j2 in (j1+1):p) {
      temp_interaction <- interaction_measure(explainer, term_names[j1], term_names[j2], inter_param)
      #percentage of observations for which interaction measure is greater than inter_param
      interaction_values[i,] <- mean(temp_interaction)
      interactions_strength[i,] <- c(term_names[j1], term_names[j2])
      i <- i + 1
      #updating progress bar
      if (verbose == TRUE) {
        utils::setTxtProgressBar(pb, i)
      }
    }
  }

  #closing progress bar
  if (verbose == TRUE) {
    close(pb)
  }

  interactions_strength <- cbind(interactions_strength, interaction_values)
  colnames(interactions_strength) <- c("variable1", "variable2", "strength")
  interactions_strength <- interactions_strength[interactions_strength["strength"] >= inter_threshold,]
  if (nrow(interactions_strength) == 0) {
    interactions_strength <- NULL
  } else {
    rownames(interactions_strength) <- 1:nrow(interactions_strength)
  }
  return(interactions_strength)

}


#' @title Detecting interactions via a permutation approach
#'
#' @description The safely_detect_interactions() function detects second-order interactions based on predictions
#' made by a surrogate model. For each pair of features it performs values permutation in order
#' to evaluate their non_additive effect.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param var1 character, first of the names of variables for which wa want to evaluate
#' interaction effect
#' @param var2 character, second of the names of variables for which wa want to evaluate
#' interaction effect
#' @param inter_param numeric, a positive value indicating which of single observation non-additive effects
#' are to be regarded as significant, the higher value the higher non-additive effect has to be to be taken
#' into account
#'
#' @seealso \code{\link{safely_detect_interactions}}
#'
#' @return vector of logical values, each of them corresponding to one observation and
#' indicating whether for this observation interaction effect was detected
#'
#' @export
interaction_measure <- function(explainer, var1, var2, inter_param) {

  data <- explainer$data

  #permutations of var1 and var2 columns
  set.seed(123)
  var1_permutation <- sample(unlist(data[var1]))
  set.seed(123)
  var2_permutation <- sample(unlist(data[var2]))

  pred_function <- explainer$predict_function
  model <- explainer$model

  #predictions for original data
  y_pred <- pred_function(model, data)

  #the difference in predictions after var1 was permuted
  data_perm1 <- data
  data_perm1[var1] <- var1_permutation
  y_pred_perm1 <- pred_function(model, data_perm1)
  diff_pred_perm1 <- y_pred_perm1 - y_pred

  #the difference in predictions after var2 was permuted
  data_perm2 <- data
  data_perm2[var2] <- var2_permutation
  y_pred_perm2 <- pred_function(model, data_perm2)
  diff_pred_perm2 <- y_pred_perm2 - y_pred

  #the difference in predictions after both var1 and var2 were permuted
  data_perm12 <- data
  data_perm12[var1] <- var1_permutation
  data_perm12[var2] <- var2_permutation
  y_pred_perm12 <- pred_function(model, data_perm12)
  diff_pred_perm12 <- y_pred_perm12 - y_pred

  #a non-additive effect of var1 and var2 for all observations
  non_additive_effect <- diff_pred_perm12 - (diff_pred_perm1 + diff_pred_perm2)

  return(abs(non_additive_effect)>=inter_param*abs(diff_pred_perm1 + diff_pred_perm2))

}


