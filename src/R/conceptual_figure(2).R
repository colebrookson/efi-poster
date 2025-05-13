# Simulate from a hiearchical model
library(ggplot2)
# Case 1 - high observation error
n <- 100
x_ic <- log(1000)
tau_ic <- 100

a_obs <- 3
r_obs <- 3
a_add <- 0.5
r_add <- 0.5

# log y.samp if y isn't logged!
init <- list()
y <- arima.sim(
  n = n, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
  sd = sqrt(0.1796)
)
y_samp <- sample(y, length(y), replace = TRUE)
init <- list(
  tau_add = 1 / var(diff((y_samp))),
  ## initial guess on process precision
  tau_obs = 5 / var((y_samp))
)
## initial guess on obs precision

x <- arima.sim(
  n = n, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
  sd = sqrt(0.001)
)

#### Priors
x[1] <- dnorm(x_ic, tau_ic)
tau_obs <- dgamma(a_obs, r_obs)
tau_add <- dgamma(a_add, r_add)

#### Process Model
for (t in 2:n) {
  x[t] <- rnorm(1, x[t - 1], tau_add)
}

#### Data Model
for (t in 2:n) {
  y[t] <- rnorm(1, x[t], tau_obs)
}

# In-sample prediction with a AR(1)
y_fit <- arima(y, c(1, 0, 0))
y_pred <- rep(0, length(y))
y_pred[1] <- y[1]
b1 <- y_fit$coef[1]
int <- y_fit$coef[2]
sig2 <- y_fit$sigma2
# Pred
for (t in 2:n) {
  y_pred[t] <- rnorm(1, int + y[t] * b1, sig2)
}

plot(y)
lines(y_pred, col = "red")
# Combine plot_data_1 and plot_data_2 into a single dataframe
plot_data_1 <- data.frame(
  timestep = rep(1:n, 2),
  value = c(y, y_pred),
  variable = rep(c("Observed", "Predicted"), each = n)
)
plot_data_combined <- rbind(
  cbind(plot_data_2, panel = "Panel 1"),
  cbind(plot_data_1, panel = "Panel 2")
)

# Plot using ggplot2 with facets for the panels
p_combined <- ggplot(plot_data_combined, aes(x = timestep, y = value, color = variable)) +
  geom_line() +
  scale_color_manual(
    "",
    values = c("#0072B2", "#D55E00"),
    labels = c("Observed", "Predicted")
  ) +
  labs(y = "Variable", x = "Time step") +
  theme_base(base_size = 18) +
  theme(
    legend.position = c(0.1, 0.1), # Position the legend inside the plot panel
    strip.text = element_blank(),
    legend.background = element_rect(fill = "white"),
    legend.key = element_rect(fill = "white"),
    axis.text.y = element_blank()
  ) +
  facet_wrap(~panel, ncol = 1, scales = "free_y")

# Print the combined plot
print(p_combined)
ggsave(
  filename = here::here("figs", "varaible_stack.png"),
  plot = p_combined,
  width = 8,
  height = 6,
  dpi = 300
) # Uncertainty decomposition

obs_var <- matrix(0, ncol = 1, nrow = n)
obs_var[1] <- 15

for (i in 2:n) {
  obs_var[i] <- obs_var[i - 1] + rnorm(1, 0, 3)
}

proc_var <- obs_var * .1 - rnorm(n, 0, 1)

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

props <- reshape2::melt(props, id.vars = "timestep")

# Plot
ggplot(data = df, aes(x = timestep, y = variance, fill = variable)) +
  geom_area() +
  scale_fill_brewer(palette = "Paired") +
  theme_bw()

# Forecast
pe <- matrix(0, ncol = 1, nrow = n)
pe[1] <- y_pred[1] - y[1] / 25
for (i in 2:n) {
  pe[i] <- (y_pred[i] - y[i]) / 25 + pe[i - 1] + rnorm(1, 0, 1)
}

intrinsic <- -.5 * pe
# Reshape the data for plotting
plot_data <- data.frame(
  timestep = rep(1:n, 2),
  value = c(intrinsic, pe),
  variable = rep(c("Intrinsic", "PE"), each = n)
)

# Plot using ggplot2
p2 <- ggplot(plot_data, aes(x = timestep, y = value, color = variable)) +
  geom_line() +
  scale_color_manual(
    values = c("black", "red"),
    labels = c("Realized Predictability", "Intrinsic Predictability")
  ) +
  labs(y = "Standardized Error (WPE vs. RE)", color = "Legend") +
  theme_base() +
  theme(
    legend.position = c(0.2, 0.1), # Position the legend inside the plot panel
    legend.background = element_rect(fill = "white", color = "black"),
    legend.key = element_rect(fill = "white"),
    axis.text.y = element_blank()
  )
