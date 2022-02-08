#' @title Identifying Changes in a Series Using PELT Algorithm
#'
#' @description The safely_detect_changepoints() function calculates the optimal positioning
#' and number of changepoints for given data and penalty. It uses a PELT algorithm
#' with a nonparametric cost function based on the empirical distribution.
#' The implementation is inspired by the code available on https://github.com/rkillick/changepoint.
#'
#' @param data a vector within which you wish to find changepoints
#' @param penalty penalty for introducing another changepoint,
#' one of "AIC", "BIC", "SIC", "MBIC", "Hannan-Quinn" or numeric non-negative value
#' @param nquantiles the number of quantiles used in integral approximation
#'
#' @return a vector of optimal changepoint positions (last observations of each segment)
#'
#' @seealso \code{\link{safely_transform_continuous}}
#'
#' @examples
#'
#' library(rSAFE)
#'
#' data <- rep(c(2,7), each=4)
#' safely_detect_changepoints(data)
#'
#' set.seed(123)
#' data <- c(rnorm(15, 0), rnorm(20, 2), rnorm(30, 8))
#' safely_detect_changepoints(data)
#' safely_detect_changepoints(data, penalty = 25)
#' @export

safely_detect_changepoints <- function(data, penalty = "MBIC", nquantiles = 10) {

  if (is.null(data)) {
    cat("No data provided!")
    return(NULL)
  }

  n <- length(data)

  penalty_value <- penalty_value_choice(penalty, n)

  K <- nquantiles #uses K points to evaluate integral
  if (K > n) { #there are more quantiles than observations
    K <- n
  }
  sumstat <- matrix(0, K, n+1) #matrix in which cdf values will be stored
  data_sorted <- sort(data)
  x_K <- -1 + (2*(1:K)/K-1/K) #K equally spaced values from [-1,1]
  c_param <- -log(2*n-1) #parameters from lemma 3.1
  p_K <- 1/(1+exp(c_param*x_K))
  t_K <- rep(0, K)

  for (k in 1:K) {
    j <- as.integer((n-1)*p_K[k] + 1) #computing indexes corresponding to quantiles p_K
    t_K[k] <- data_sorted[j] #quantile of rank p_K[k]
    sumstat[k,-1] <- cumsum(data<t_K[k]) + 0.5*cumsum(data==t_K[k]) #sumstat[k,j] = F_hat(data[1:j], t_k) - value of empirical cdf for data x_(1:J) in t_k
  }

  #PELT algorithm
  cpts_final <- PELT_algorithm(data, penalty_value, sumstat, K)
  return(cpts_final)

}


penalty_value_choice <- function(penalty, n) {

  if (is.character(penalty)) {
    if (penalty=="MBIC") {
      penalty_value <- 3*log(n)
    } else if (penalty=="AIC") {
      penalty_value <- 2
    } else if ((penalty %in% c("BIC", "SIC"))) {
      penalty_value <- log(n)
    } else if (penalty=="Hannan-Quinn") {
      penalty_value <- 2*log(log(n))
    } else {
      penalty_value <- NULL
    }
  } else if (is.numeric(penalty)) {
    if (penalty<0) {
      penalty_value <- NULL
    } else {
      penalty_value <- penalty
    }
  }

  if (is.null(penalty_value)) {
    warning("Wrong penalty - using default one.")
    penalty_value <- 3*log(n)
  }

  return(penalty_value)

}


cost <- function(data, u, v, sumstat, K) { #function that evaluates cost of data[u:v]

  if (is.null(data)) {
    stop("No data provided in cost function!")
  }

  n <- length(data)
  if (u<1 | v>n | u>v) {
    stop("Wrong indexes u and v!")
  }

  F_hat <- rep(0, K)
  cost_temp <- rep(0, K)
  for (k in 1:K) {
    F_hat[k] <- (1/(v-u+1)) * (sumstat[k, v+1] - sumstat[k, u]) #cdf based on data[u:v] evaluated for k-th quantile
    cost_temp[k] <- (v-u+1) * (F_hat[k] * log(F_hat[k]) + (1-F_hat[k]) * log(1-F_hat[k])) #cost for k-th quantile
  }

  c_param <- -log(2*n-1)
  cost_final <- 2*c_param/K * sum(cost_temp, na.rm = TRUE)

  return(cost_final)

}

#' @importFrom sets set_union set
PELT_algorithm <- function(data, penalty_value, sumstat, K) {

  if (is.null(data)) {
    stop("No data provided in PELT_algorithm function!")
  }

  epsilon <- 0.000001 #needed later to compare floats

  n <- length(data)

  Q <- rep(0, n+1) #Q[i] = Q(data[1:(i-1)] = F(i-1) - minimum of the segmentation cost for data[1:(i-1)]
  Q[1] <- (-penalty_value) #Q[1] = F(0) = -beta

  cp <- vector("list", length = n+1)
  #cp[[i]] - set of optimal changepoints for data[1:(i-1)] for i=2,...,n+1,
  #cp[[1]]=NULL

  R <- sets::set(0)
  #R - set of points that could be the last optimal changepoint, updated in a loop
  #initially R={0}

  #computing minima of the segmentation cost Q[v+1]=F(v), starting from data[1:1] till data[1:n]
  for (v in 1:n) {
    u_index_min <- (-1)
    Q_min <- Inf
    for (u in R) { #searching for Q minimmum for data[1:v], with the last changepoint coming from R set
      Q_actual <- Q[u+1] + cost(data, u+1, v, sumstat, K) + penalty_value
      if (Q_actual < Q_min) { #if the better segmentation is found both Q value and u index are saved
        Q_min <- Q_actual
        u_index_min <- u
      }
    }

    #saving the optimal Q_min value and u_index_min index
    Q[v+1] <- Q_min
    cp[[v+1]] <- sets::set(u_index_min)
    #if u_index_min is the optimal last changepoint, then the optimal segmentation of data[1:u_index_min]
    #together with u_index_min give the optimal segmentation of data[1:v]
    if (! is.null(cp[[u_index_min+1]])) {
      cp[[v+1]] <- set_union(cp[[u_index_min+1]], cp[[v+1]])
    }

    #updating R set for next iterations - removing those points that can never be the last optimal changepoint
    for (tau in R) {
      if (Q[tau+1] + cost(data, tau+1, v, sumstat, K) > Q[v+1] + epsilon) {
        R <- R - set(tau)
      }
    }
    R <- set_union(R, v)
  }

  cpts_final <- cp[[n+1]] - set(0)

  return(sort(unlist(cpts_final)))

}






