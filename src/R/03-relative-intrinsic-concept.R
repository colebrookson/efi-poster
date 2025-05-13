#' SIMULATING FROM A HIERARCHICAL MODEL
#' Shubhi Sharma & Cole Brookson
#' May 2025
#'
#' This script simulates data from a hierarchical model to explore the
#' decomposition of uncertainty and the concept of intrinsic predictability.
#' It includes:
#' - Simulation of time series data with observation and process error
#' - Priors and process model definitions
#' - In-sample prediction using AR(1)
#' - Uncertainty decomposition into components
#' - Visualization of variance contributions
#' - Forecasting and intrinsic predictability analysis

library(ggplot2)
library(patchwork)
source(here::here("./src/R/00-global-funs.R")) # Load theme_base()

# Shared parameters
n <- 200
x_ic <- log(1000)
tau_ic <- 100

#' @param ts numeric vector
#' @param m embedding dimension
#' @param tau delay
realized_entropy <- function(ts, m = 3, tau = 1) {
  patterns <- embed(ts, m)[, rev(seq_len(m))]
  perms <- t(apply(patterns, 1, order))
  perms_char <- apply(perms, 1, paste, collapse = "")
  p <- table(perms_char) / length(perms_char)
  H <- -sum(p * log(p))
  H_norm <- H / log(factorial(m))
  return(H_norm)
}

weighted_permutation_entropy <- function(ts, m = 3, tau = 1) {
  patterns <- embed(ts, m)[, rev(seq_len(m))]
  perms <- t(apply(patterns, 1, order))
  perms_char <- apply(perms, 1, paste, collapse = "")
  weights <- apply(patterns, 1, var)
  weight_sum <- tapply(weights, perms_char, sum)
  total <- sum(weights)
  p_weighted <- weight_sum / total
  H <- -sum(p_weighted * log(p_weighted))
  H_norm <- H / log(factorial(m))
  return(H_norm)
}

#' Compute WPE (on y) and RE (on y_pred) in rolling windows
#'
#' @param y observed time series
#' @param y_pred forecasted time series
#' @param window_size size of the rolling window
#' @param m embedding dimension
#' @param tau time delay (default 1)
#' @param step step size for sliding window
#'
#' @return data.frame with time index, WPE (on y), and RE (on y_pred)
rolling_entropy_metrics <- function(y, y_pred, window_size = 20,
                                    m = 3, tau = 1, step = 1) {
  stopifnot(length(y) == length(y_pred))
  n <- length(y)
  times <- seq(1, n - window_size + 1, by = step)

  wpe_vals <- numeric(length(times))
  re_vals <- numeric(length(times))

  for (i in seq_along(times)) {
    idx <- times[i]:(times[i] + window_size - 1)
    segment_y <- y[idx]
    segment_pred <- y_pred[idx]

    wpe_vals[i] <- weighted_permutation_entropy(segment_y, m = m, tau = tau)
    re_vals[i] <- realized_entropy(segment_pred, m = m, tau = tau)
  }

  return(data.frame(
    time = times + window_size - 1,
    WPE = wpe_vals,
    RE = re_vals
  ))
}

# Function to simulate and plot results
simulate_model <- function(a_obs, r_obs, a_add, r_add, model_type) {
  # simulate precisions
  tau_obs <- rgamma(1, shape = a_obs, rate = r_obs)
  tau_add <- rgamma(1, shape = a_add, rate = r_add)

  # convert precision to standard deviation
  sd_obs <- 1 / sqrt(tau_obs)
  sd_add <- 1 / sqrt(tau_add)

  #' [SIMULATE TRUE + OBSERVED STATES] ------------------------------------------

  x <- numeric(n)
  x[1] <- rnorm(1, mean = x_ic, sd = 1 / sqrt(tau_ic))
  for (t in 2:n) {
    x[t] <- rnorm(1, mean = x[t - 1], sd = sd_add)
  }

  y <- numeric(n)
  for (t in 1:n) {
    y[t] <- rnorm(1, mean = x[t], sd = sd_obs)
  }

  #' [AR(1) FIT AND PREDICTION] -------------------------------------------------

  fit <- arima(y, order = c(1, 0, 0))
  y_pred <- numeric(n)
  y_pred[1] <- y[1]
  phi <- fit$coef["ar1"]
  int <- fit$coef["intercept"]
  sig2 <- fit$sigma2

  for (t in 2:n) {
    y_pred[t] <- rnorm(1, mean = int + phi * y[t - 1], sd = sqrt(sig2))
  }

  # Uncertainty decomposition
  obs_var <- numeric(n)
  obs_var[1] <- 15
  for (i in 2:n) {
    obs_var[i] <- obs_var[i - 1] + rnorm(1, 0, 3)
  }
  proc_var <- obs_var * 0.1 - rnorm(n, 0, 1)
  ic_var <- rep(10, n)
  param_var <- rnorm(n, 15, 3)
  all_var <- obs_var + proc_var + ic_var + param_var

  df <- data.frame(c(proc_var, ic_var, param_var, obs_var))
  colnames(df) <- "variance"
  df$variable <- rep(c("process", "ic", "parameters", "observation"), each = n)
  df$timestep <- rep(1:n, 4)

  props <- data.frame(
    obs = (obs_var / all_var) * 100,
    proc = (proc_var / all_var) * 100,
    ic = (ic_var / all_var) * 100,
    param = (param_var / all_var) * 100
  )

  props <- cbind(1:n, props)
  colnames(props) <- c("timestep", "obs", "proc", "ic", "param")

  # Reshape data for plotting (plot 2)
  props <- reshape2::melt(props, id.vars = "timestep")

  # calculate the pe
  entropy_df <- rolling_entropy_metrics(y, y_pred,
    step = 5,
    window_size = 20, m = 3
  )

  # Plot 1: Observations and Predictions
  plot1 <- ggplot() +
    geom_line(aes(x = 1:n, y = y), color = "black", size = 1) +
    geom_line(aes(x = 1:n, y = y_pred), color = "red", size = 1) +
    labs(title = paste(model_type, "Model"), x = "Time", y = "Value") +
    theme_base()
  plot1
  # Plot 2: Standardized Error and Predictability
  df_pred <- data.frame(
    timestep = 1:n,
    standardized_error = standardized_error,
    intrinsic = rep(intrinsic_predictability, n),
    realized = rep(realized_predictability, n)
  )
  # Plot 2: WPE vs RE (Entropy)
  entropy_df <- entropy_df |>
    tidyr::pivot_longer(
      cols = c("WPE", "RE"),
      names_to = "Entropy",
      values_to = "value"
    )
  plot2 <- ggplot(entropy_df, aes(x = time)) +
    geom_line(aes(x = time, y = value, color = Entropy)) +
    labs(
      x = "Time",
      y = "Normalized Entropy"
    ) +
    scale_color_manual(values = c("red", "blue")) +
    guides(
      colour = guide_legend(position = "inside")
    ) +
    åå
  theme_base() +
    theme(
      legend.position.inside = c(0.8, 0.2)
    )


  # Plot 3: Uncertainty Decomposition
  plot3 <- ggplot(data = df, aes(x = timestep, y = variance, fill = variable)) +
    geom_area() +
    scale_fill_brewer(palette = "Paired") +
    theme_bw() +
    ylim(c(0, 100))
  plot3

  combined_plot <- plot1 + plot2 + plot3 + plot_layout(ncol = 3)

  return(combined_plot)
}

# Noisy Process Model
simulate_model(
  a_obs = 5, r_obs = 5, # High observation precision
  a_add = 0.2, r_add = 0.1, # Low process precision
  model_type = "Noisy Process"
)

# # Noisy Observation Model
# simulate_model(
#   a_obs = 0.1, r_obs = 0.1, # Low observation precision
#   a_add = 5, r_add = 5, # High process precision
#   model_type = "Noisy Observation"
# )
# a_obs <- 5
# r_obs <- 5 # High observation precision
# a_add <- 0.1
# r_add <- 0.1 # Low process precision
# model_type <- "Noisy Process"


# a_obs <- 0.1
# r_obs <- 0.1 # High observation precision
# a_add <- 5
# r_add <- 5 # Low process precision
# model_type <- "Noisy Process"
