# Simulate from a hiearchical model
library(ggplot2)
# Case 1 - high observation error
n <- 100
x_ic <- log(1000)
tau_ic <- 100

a_obs <- 3
r_obs <- .5
a_add <- 2
r_add <- 2

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

plot(y, ylim = c(-.7, 3))
lines(y_pred, col = "red")
# Uncertainty decomposition

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
pe[1] <- y_pred[1] - y[1]/ 25 
for (i in 2:n) {
pe[i] <- (y_pred[i] - y[i])/25 + pe[i - 1] + rnorm(1, 0, 1)
}

intrinsic <- -.5 * pe
plot(intrinsic, type = "l", ylim = c(-8, 10))
lines(pe, col = "red")

