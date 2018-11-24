#' Factor feature transformation using factorMerger package
#'
#' The transform_factor() function calculates a transformation function
#' for the factor variable using the getOptimalPartitionDf() function from factorMerger package.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param variable a feature for which the transformation function is to be computed
#' @param plot logical, if TRUE then plots the optimal partition
#'
#' @return transformation function for the given variable
#'
#' @export


transform_factor <- function(explainer, variable, plot = FALSE) {

  sv <- single_variable(explainer, variable, type = "factor")
  partition <- getOptimalPartitionDf(sv)

  cat(paste0("Transformation for '", variable, "' variable:\n"))
  if (length(levels(partition$pred)) == 1) {
    cat("No transformation.") #no transformation for 1 factor
    return(NULL)
  } else {
    colnames(partition) <- c(variable, paste0(variable, "_new"))
    rownames(partition) <- 1:nrow(partition)
    print(partition)
  }

  if (plot == TRUE) {
    #graphics.off()
    plot(sv) #not showing
  }


  return(partition)

}
