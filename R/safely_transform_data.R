#' Performing transformations on all features in the dataset
#'
#' The safely_transform_data() function creates new variables in dataset
#' using safe_extractor object.
#'
#' @param safe_extractor object containing information about variables transformations created with safe_extraction() function
#' @param data data for which features are to be transformed
#' @param encoding method of representing factor variables, one of: "categorical", "one-hot"
#' @param verbose logical, if progress bar is to be printed
#'
#' @return data with extra columns containing newly created variables
#'
#' @export


safely_transform_data <- function(safe_extractor, data, encoding = "categorical", verbose = TRUE) {

  if (class(safe_extractor) != "safe_extractor") {
    stop(paste0("No applicable method for 'transform_data' applied to an object of class '", class(safe_extractor), "'."))
  }

  row_ind <- data.frame(row_ind = 1:nrow(data)) #column created to maintain rows order after merge
  data <- cbind(row_ind, data)


  if (verbose == TRUE) {
    #progress bar - to let the user know how many variables have been already processed
    pb <- utils::txtProgressBar(min = 0, max = length(names(safe_extractor$vars)), style = 3)
  }

  #for (v in colnames(data)) {
  for (v in names(safe_extractor$vars)) {

    # if (! v %in% names(safe_extractor$vars)) { #column with responses or just created new variable
    #   next
    # }

    var_info <- safe_extractor$vars[[v]] #information on variable extracted from safe_extractor object

    if (is.null(var_info$new_levels)) { #no transformation available
      #cat(paste0("No transformation for '", v, "' variable.\n"))
      if (verbose == TRUE) {
        utils::setTxtProgressBar(pb, which(names(safe_extractor$vars) == v))
      }
      next
    }
    # cat(paste0("Transforming '", v, "' variable.\n"))



    if (var_info$type == "categorical") {

      data <- merge(data, var_info$new_levels, by = v)
      data[,paste0(v, "_new")] <- as.factor(data[,paste0(v, "_new")]) #variable as factor
      #adding levels which do not occur in the dataset
      levels(data[,paste0(v, "_new")]) <- c(levels(data[,paste0(v, "_new")]),
                                            setdiff(unique(var_info$new_levels[,paste0(v, "_new")]),
                                                    levels(data[,paste0(v, "_new")])))


    } else {

      #adding new column for transformed variable
      data[,paste0(v, "_new")] <- sapply(data[,v],
                                         function(x) which.max(x<c(var_info$break_points, Inf)))

      if (encoding == "categorical") { #for categorical encoding - intervals implied by breakpoints as factor names
        data[,paste0(v, "_new")] <- sapply(data[,paste0(v, "_new")],
                                           function(x) var_info$new_levels[x])
      }
      data[,paste0(v, "_new")] <- as.factor(data[,paste0(v, "_new")]) #variable as factor
      #adding levels which do not occur in the dataset
      if (encoding == "categorical") {
        levels(data[,paste0(v, "_new")]) <- c(levels(data[,paste0(v, "_new")]),
                                              setdiff(var_info$new_levels,
                                                      levels(data[,paste0(v, "_new")])))
      } else {
        levels(data[,paste0(v, "_new")]) <- c(levels(data[,paste0(v, "_new")]),
                                              setdiff(1:length(var_info$new_levels),
                                                      levels(data[,paste0(v, "_new")])))
      }

    }


    #changing factor names in order to get proper colnames after one-hot encoding - TODO???

    #updating progress bar
    if (verbose == TRUE) {
      utils::setTxtProgressBar(pb, which(names(safe_extractor$vars) == v))
    }

  }

  #closing progress bar
  if (verbose == TRUE) {
    close(pb)
  }

  data <- data[order(data$row_ind), colnames(data) != "row_ind"] #restoring rows order after merge
  rownames(data) <- 1:nrow(data) #reseting rownames


  #transforming factor variables to one-hot encoding using recipe package
  #if intervals are factor levels then binary variables have names that do not look well - change it???
  if (encoding == "one-hot") {

    data_recipe <- recipes::recipe(~ ., data = data)
    ref_cell <- data_recipe %>%
      recipes::step_dummy(tidyselect::ends_with("_new")) %>%
      recipes::prep(training = data, retain = TRUE)
    data <- as.data.frame(recipes::bake(ref_cell, data))

  }



  return(data)

}



# transform_data <- function(data, y, propositions, which_variables = "only_new") {
#
#   if (which_variables == "only_new") { #deleting old variables from dataset
#     data <- data[,!names(data) %in% names(propositions$transformations)]
#   } else if (which_variables == "aic") {
#     model_lm <- lm(y ~ ., data = cbind(data,y))
#     sel <- stats::step(model_lm, direction = 'backward')
#     data <- data[,names(data) %in% attr(sel$terms, "term.labels")]
#   }
#
#
#   return(data)
#
# }
