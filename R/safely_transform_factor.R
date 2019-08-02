#' @title Calculating a transformation of categorical feature using hierarchical clustering
#'
#' @description The safely_transform_factor() function calculates a transformation function
#' for the factor variable using predictions obtained from black box model and hierarchical clustering.
#' The gap statistic criterion is used to determine the optimal number of clusters.
#'
#' @param explainer DALEX explainer created with explain() function
#' @param variable a feature for which the transformation function is to be computed
#' @param method the agglomeration method to be used in hierarchical clustering, one of:
#' "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid"
#' @param B number of reference datasets used to calculate gap statistics
#' @param collapse a character string to separate original levels while combining them to the new one
#'
#' @return list of information on the transformation of given variable
#'
#' @seealso \code{\link{safe_extraction}}
#'
#' @examples
#'
#' library(DALEX)
#' library(randomForest)
#' library(rSAFE)
#'
#' data <- apartments[1:500,]
#' set.seed(111)
#' model_rf <- randomForest(m2.price ~ construction.year + surface + floor +
#'                            no.rooms + district, data = data)
#' explainer_rf <- explain(model_rf, data = data[,2:6], y = data[,1])
#' safely_transform_factor(explainer_rf, "district")
#'
#' @export

safely_transform_factor <- function(explainer, variable, method = "complete", B = 500, collapse = "_") {

  if (class(explainer) != "explainer") {
    stop(paste0("No applicable method for 'safely_transform_factor' applied to an object of class '", class(explainer), "'."))
  }
  if (! variable %in% colnames(explainer$data)) {
    stop("Wrong variable name!")
  }

  data <- explainer$data
  lev <- levels(factor(data[,variable]))
  n <- length(lev)

  preds_agg <- levels_mean_agg(explainer, variable)

  #WSS
  wss_result <- WSS_all(preds_agg, method = method)
  wss <- log(wss_result$wss)
  clustering <- wss_result$clustering

  if (n < 3) { #1 cluster and n clusters can be omitted in our problem
    return(list(clustering = clustering,
                new_levels = NULL))
  }

  #reference distribution
  ref_values <- matrix(rep(0, (n-2)*B), ncol = B)
  set.seed(123)
  for (b in 1:B) { #B reference datasets
    uni_data <- stats::runif(n, min = min(preds_agg), max = max(preds_agg))
    ref_values[,b] <- log(WSS_all(uni_data, method = method)$wss)
  }
  exp_log_Wk <- apply(ref_values, 1, mean)
  gap_estimated <- exp_log_Wk - wss #estimated gap statistic

  #standard deviation
  sd_k <- (ref_values - exp_log_Wk)^2
  sd_k <- sqrt(apply(sd_k, 1, mean))
  sd_k <- sd_k * sqrt(1+1/B) #accounting for the simulation error

  #smallest k for which gap_k >= gap_(k+1) - sd_(k+1)
  if (n == 3) {
    final_cluster_size <- 2 #the only "non-trivial" clustering
  } else {
    left_side <- gap_estimated[1:(n-3)]
    right_side <- (gap_estimated - sd_k)[2:(n-2)]
    #choosing smallest k
    all_k <- which(left_side >= right_side)
    #if no k satisfies the equality above we take (n-1) as an optimal number of clusters (merging two levels)
    if (length(all_k) == 0) {
      final_cluster_size <- n-1
    } else {
      final_cluster_size <- all_k[1]+1
    }
  }

  groups <- stats::cutree(clustering, final_cluster_size)
  new_levels <- data.frame(names(groups), groups)
  names(new_levels)[1] <- variable
  pred <- stats::aggregate(new_levels[variable], by = list(new_levels$groups), function(x) paste0(x, collapse = collapse))
  names(pred)[2] <- paste0(variable, "_new")
  new_levels <- merge(new_levels, pred, by.x = "groups", by.y = "Group.1")
  new_levels <- new_levels[, colnames(new_levels) != "groups"]

  return(list(clustering = clustering,
              new_levels = new_levels))

}



levels_mean_agg <- function(explainer, variable) {
  data <- explainer$data
  lev <- levels(factor(data[,variable]))
  trans <- explainer$link
  preds <- lapply(lev, function(cur_lev) {
    tmp <- data
    tmp[,variable] <- factor(cur_lev, levels = lev)
    data.frame(scores = trans(explainer$predict_function(explainer$model, tmp)),
               level = cur_lev)
  })
  preds_combined <- do.call(rbind, preds)

  preds_agg <- stats::aggregate(preds_combined$scores, by = list(preds_combined$level), mean)
  preds_agg_final <- preds_agg$x
  names(preds_agg_final) <- preds_agg$"Group.1"
  return(preds_agg_final)
}

sum_of_squares <- function(data) {
  center <- mean(data) #the center of the data
  return(sum((data-center)^2)) #sum of squared errors between every point and the center
}

WSS <- function(data, groups) {
  k <- max(groups)
  wss <- sapply(1:k, function(k) {
    cluster <- data[groups == k]
    return(sum_of_squares(cluster))
  })
  return(sum(wss))
}

WSS_all <- function(data, method) {
  n <- length(data)
  wss <- rep(0, n)
  dist_matrix <- stats::dist(data, method = "euclidean")
  clustering <- stats::hclust(dist_matrix, method = method)
  for (k in 1:n) {
    groups <- stats::cutree(clustering, k)
    wss[k] <- WSS(data, groups)
  }
  #omitting k=1 and k=n
  wss <- wss[-c(1,n)]
  return(list(wss = wss, clustering = clustering))
}

plot_categorical <- function(temp_info, variable) {
  dend <- stats::as.dendrogram(temp_info$clustering)
  if (!is.null(temp_info$new_levels)) {
    final_cluster_size <- length(unique(temp_info$new_levels[,2]))
  } else {
    final_cluster_size <- 1
  }
  hues <- seq(15, 375, length = final_cluster_size + 1)
  k_colors <- grDevices::hcl(h = hues, l = 65, c = 100, alpha = 1)[1:final_cluster_size]
  dend <- dendextend::set(dend, "labels_cex", 4)
  dend <- dendextend::set(dend, "branches_lwd", 0.5)
  dend <- dendextend::set(dend, "branches_k_color", k = final_cluster_size, value = k_colors)
  dend <- dendextend::set(dend, "labels_col", k = final_cluster_size, value = k_colors)
  max_height <- max(dendextend::get_branches_heights(dend))
  labels_track_height <- max_height/8
  if(max_height < 1) {
    offset_labels <- -max_height/100
  } else {
    offset_labels <- -0.1
  }
  gdend <- dendextend::as.ggdend(dend, type = "rectangle")
  gdend$labels$angle <- 0
  gdend$labels$hjust <- -0.1
  gdend$labels$vjust <- 0.5
  data <- dendextend::prepare.ggdend(gdend)
  data$labels$y <- data$labels$y + offset_labels

  p <- ggplot()
  p <- p + geom_segment(data = data$segments,
                        aes_string(x = "x", y = "y", xend = "xend", yend = "yend",
                                   colour = "col", linetype = "lty", size = "lwd"),
                        lineend = "square") +
    guides(linetype = FALSE, col = FALSE) +
    scale_size_identity() +
    scale_linetype_identity()
  p <- p + scale_colour_identity()
  p <- p + ggpubr::geom_exec(geom_text, data = data$labels,
                             x = "x", y = "y", label = "label", color = "col", size = "cex",
                             angle = "angle", hjust = "hjust", vjust = "vjust")
  p <- p + DALEX::theme_drwhy()
  p <- p + coord_flip() +
    scale_y_reverse() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    expand_limits(y = -labels_track_height) +
    labs(title = variable, x = '', y = '')

  if (is.null(temp_info$new_levels)) {
    cat(paste0("No transformation for '", variable, "'."))
  }
  return(p)
}





