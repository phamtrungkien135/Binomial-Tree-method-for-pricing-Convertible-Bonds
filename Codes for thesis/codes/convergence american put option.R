rm(list = ls())

library(ggplot2)

vol <- 0.284345044546826
S0  <- 23750
K   <- 30000
T   <- 1/2
r   <- 0.07

price_american_put <- function(model, N) {
  dt <- T / N
  
  if (model == "CRR") {
    u <- exp(vol * sqrt(dt))
    d <- exp(-vol * sqrt(dt))
    p <- (exp(r * dt) - d) / (u - d)
  }
  
  if (model == "JR") {
    u <- exp((r - 0.5 * vol^2) * dt + vol * sqrt(dt))
    d <- exp((r - 0.5 * vol^2) * dt - vol * sqrt(dt))
    p <- 0.5
  }
  
  if (model == "Tian") {
    v <- exp(vol^2 * dt)
    u <- 0.5 * exp(r * dt) * v *
      (v + 1 + sqrt(v^2 + 2 * v - 3))
    d <- 0.5 * exp(r * dt) * v *
      (v + 1 - sqrt(v^2 + 2 * v - 3))
    p <- (exp(r * dt) - d) / (u - d)
  }
  
  if (model == "Haahtela") {
    u <- exp(r * dt) * (1 + sqrt(exp(vol^2 * dt) - 1))
    d <- exp(r * dt) * (1 - sqrt(exp(vol^2 * dt) - 1))
    p <- (exp(r * dt) - d) / (u - d)
  }
  
  stock <- matrix(0, N + 1, N + 1)
  
  for (i in 0:N) {
    for (j in 0:i) {
      stock[i + 1, j + 1] <- S0 * u^j * d^(i - j)
    }
  }
  
  option <- matrix(0, N + 1, N + 1)
  option[N + 1, ] <- pmax(K - stock[N + 1, ], 0)
  
  for (i in N:1) {
    for (j in 1:i) {
      continuation <- ((1 - p) * option[i + 1, j] +
                         p * option[i + 1, j + 1]) / exp(r * dt)
      
      exercise <- max(K - stock[i, j], 0)
      
      option[i, j] <- max(exercise, continuation)
    }
  }
  
  option[1, 1]
}

periods <- 3:2000
models  <- c("CRR", "JR", "Tian", "Haahtela")

all_data <- expand.grid(
  periods = periods,
  model = models
)

all_data$values <- mapply(
  price_american_put,
  model = all_data$model,
  N = all_data$periods
)

ggplot(all_data, aes(x = periods, y = values, color = model)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Convergence of American Put Price for 4 Binomial Models",
    x = "Periods",
    y = "Option Value",
    color = "Model"
  ) +
  theme_minimal()