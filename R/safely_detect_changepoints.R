#' @title Identyfying changes using a nonparametric cost function
#'
#' @description The safely_detect_changepoints() function calculates the optimal positioning
#' and number of changepoints for given data and penalty. It uses a PELT algorithm
#' with a nonparametric cost function based on the empirical distribution.
#'
#' @param data a vector within which you wish to find changepoints
#' @param penalty penalty for introducing another changepoint,
#' one of "AIC", "BIC", "SIC", "MBIC", "Hannan-Quinn" or numeric non-negative value
#' @param nquantiles the number of quantiles used in integral approximation
#'
#' @return a vector of optimal changepoint positions (last observations of each segment)
#'
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


#' @title Choosing penalty value
#'
#' @description The penalty_value_choice() function sets penalty value.
#'
#' @param penalty penalty for introducing another changepoint,
#' one of "AIC", "BIC", "SIC", "MBIC", "Hannan-Quinn" or numeric non-negative value
#' @param n integer, number of observations
#'
#' @return penalty value
#'
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


#' @title Evaluating a cost of specified data segment
#'
#' @description The cost() function evaluates cost for given data and both beginning and
#' end of the segment.
#'
#' @param data vector of numeric values
#' @param u index of the segment beginning
#' @param v index of the segment end
#' @param sumstat matrix of statistics used to calculate empirical distribution function
#' @param K the number of quantiles used in integral approximation, not greater than
#' the number of all observations in the dataset
#'
#' @return a cost of specified data segment
#'
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


#' @title PELT algorithm
#'
#' @description The PELT_algorithm function() performs PELT algorithm in order to find
#' optimal changepoints.
#'
#' @param data vector of numeric values
#' @param penalty_value penalty value returned by penalty_value_choice() function
#' @param sumstat matrix of statistics used to calculate empirical distribution function
#' @param K the number of quantiles used in integral approximation, not greater than
#' the number of all observations in the dataset
#'
PELT_algorithm <- function(data, penalty_value, sumstat, K) {

  if (is.null(data)) {
    stop("No data provided in PELT_algorithm function!")
  }

  epsilon <- 0.000001 #needed later to compare floats

  n <- length(data)

  Q <- rep(0, n+1) #Q[i] = Q(data[1:(i-1)] = F(i-1)
  Q[1] <- (-penalty_value) #F(0) = -beta

  cp <- matrix(0, n+1, n+1) #cp[i,j] - if j-th point was recorded as a changepoint in i-th step
  #cp(0) = NULL
  cp <- vector("list", length = n+1)

  R <- rep(0, n+1) #changepoints that could be last ones, updated during loop
  R[1] <- 1 #R_1 = {0}
  R <- sets::set(0)

  for (v in 1:n) {
    u_index_min <- (-1)
    Q_min <- Inf
    for (u in R) { #searching for Q minimmum
      Q_actual <- Q[u+1] + cost(data, u+1, v, sumstat, K) + penalty_value
      if (Q_actual < Q_min) {
        Q_min <- Q_actual
        u_index_min <- u
      }
    }

    Q[v+1] <- Q_min
    cp[[v+1]] <- sets::set(u_index_min)
    if (! is.null(cp[[u_index_min+1]])) {
      cp[[v+1]] <- sets::set_union(cp[[u_index_min+1]], cp[[v+1]])
    }

    #updating R set
    for (tau in R) {
      if (Q[tau+1] + cost(data, tau+1, v, sumstat, K) > Q[v+1] + epsilon) {
        R <- R - tau
      }
    }
    R <- sets::set_union(R, v)
  }

  cpts_final <- cp[[n+1]] - 0
  return(sort(unlist(cpts_final)))

}






