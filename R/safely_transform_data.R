#' @title Performing transformations on all features in the dataset
#'
#' @description The safely_transform_data() function creates new variables in dataset
#' using safe_extractor object.
#'
#' @param safe_extractor object containing information about variables transformations created with safe_extraction() function
#' @param data data for which features are to be transformed
#' @param verbose logical, if progress bar is to be printed
#'
#' @return data with extra columns containing newly created variables
#'
#' @seealso \code{\link{safe_extraction}}, \code{\link{safely_select_variables}}
#'
#' @examples
#'
#' library(DALEX)
#' library(randomForest)
#' library(SAFE)
#'
#' data <- apartments[1:500,]
#' set.seed(111)
#' model_rf <- randomForest(m2.price ~ construction.year + surface + floor +
#'                            no.rooms + district, data = data)
#' explainer_rf <- explain(model_rf, data = data[,2:6], y = data[,1])
#' safe_extractor <- safe_extraction(explainer_rf, verbose = FALSE)
#' safely_transform_data(safe_extractor, data, verbose = FALSE)
#'
#' @export

safely_transform_data <- function(safe_extractor, data, verbose = TRUE) {

  if (class(safe_extractor) != "safe_extractor") {
    stop(paste0("No applicable method for 'safely_transform_data' applied to an object of class '", class(safe_extractor), "'."))
  }
  if (is.null(data)) {
    stop("No data provided!")
  }

  row_ind <- data.frame(row_ind = 1:nrow(data)) #column created to maintain rows order after later merge
  data <- cbind(row_ind, data)
  term_names <- names(safe_extractor$variables_info)

  #in case some variables are missing in given dataset we omit them in transformations
  term_names <- intersect(term_names, colnames(data))

  if (verbose == TRUE) {
    #progress bar - to let the user know how many variables have been already processed
    pb <- utils::txtProgressBar(min = 0, max = length(term_names), style = 3)
  }

  for (var_temp in term_names) {

    temp_info <- safe_extractor$variables_info[[var_temp]] #information on variable extracted from safe_extractor object

    if (is.null(temp_info$new_levels)) { #no transformation available
      if (verbose == TRUE) {
        utils::setTxtProgressBar(pb, which(term_names == var_temp))
      }
      next
    }

    new_var_name <- paste0(var_temp, "_new") #name of the column containing new variable
    if (temp_info$type == "categorical") {

      data <- merge(data, temp_info$new_levels, by = var_temp)
      data[,new_var_name] <- as.factor(data[,new_var_name]) #variable as factor
      #adding levels which do not occur in the dataset
      levels(data[,new_var_name]) <- c(levels(data[,new_var_name]),
                                       setdiff(unique(temp_info$new_levels[,new_var_name]),
                                               levels(data[,new_var_name])))
    } else {

      #adding new column for transformed variable
      data[,new_var_name] <- sapply(data[,var_temp],
                                    function(x) which.max(x<=c(temp_info$break_points, Inf)))

      #intervals implied by breakpoints as factor names
      data[,new_var_name] <- sapply(data[,new_var_name],
                                    function(x) temp_info$new_levels[x])
      data[,new_var_name] <- as.factor(data[,new_var_name]) #variable as factor
      #adding levels which do not occur in the dataset
      levels(data[,new_var_name]) <- c(levels(data[,new_var_name]),
                                       setdiff(temp_info$new_levels, levels(data[,new_var_name])))

    }

    #updating progress bar
    if (verbose == TRUE) {
      utils::setTxtProgressBar(pb, which(term_names == var_temp))
    }

  }

  #closing progress bar
  if (verbose == TRUE) {
    close(pb)
  }

  data <- data[order(data$row_ind), colnames(data) != "row_ind"] #restoring rows order after merge
  rownames(data) <- 1:nrow(data) #reseting rownames

  #adding variables representing interactions
  interaction_effects <- safe_extractor$interaction_effects
  if (! is.null(interaction_effects)) {
    for (i in 1:nrow(interaction_effects)) {
      var1 <- interaction_effects$variable1[i]
      var2 <- interaction_effects$variable2[i]
      if (all(c(paste0(var1, "_new"), paste0(var2, "_new")) %in% colnames(data))) { #checking if both new variables are present in the dataset
        data[, paste0("interaction_", var1, "_", var2)] <-
          interaction(data[, c(paste0(var1, "_new"), paste0(var2, "_new"))])
      }
    }
  }

  return(data)

}

