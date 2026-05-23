rm(list = ls())
library(ggplot2)
# Input
vol <- 0.284345044546826
S0  <- 23750
K   <- 30000
T   <- 1/2
r   <- 0.07
N   <- 1000

models <- c("CRR", "JR", "Tian", "Haahtela")

# American put price and optimal exercise boundary
american_put <- function(model, S0, K, T, r, sigma, N) {
  
  dt <- T / N
  
  if (model == "CRR") {
    u <- exp(sigma * sqrt(dt))
    d <- 1 / u
    p <- (exp(r * dt) - d) / (u - d)
  }
  
  if (model == "JR") {
    u <- exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt))
    d <- exp((r - 0.5 * sigma^2) * dt - sigma * sqrt(dt))
    p <- 0.5
  }
  
  if (model == "Tian") {
    v <- exp(sigma^2 * dt)
    u <- 0.5 * exp(r * dt) * v * (v + 1 + sqrt(v^2 + 2 * v - 3))
    d <- 0.5 * exp(r * dt) * v * (v + 1 - sqrt(v^2 + 2 * v - 3))
    p <- (exp(r * dt) - d) / (u - d)
  }
  
  if (model == "Haahtela") {
    u <- exp(r * dt) * (1 + sqrt(exp(sigma^2 * dt) - 1))
    d <- exp(r * dt) * (1 - sqrt(exp(sigma^2 * dt) - 1))
    p <- (exp(r * dt) - d) / (u - d)
  }
  
  stock <- matrix(0, N + 1, N + 1)
  option <- matrix(0, N + 1, N + 1)
  exercise_flag <- matrix(FALSE, N + 1, N + 1)
  
  # Stock price tree
  for (i in 1:(N + 1)) {
    for (j in 1:i) {
      stock[i, j] <- S0 * u^(j - 1) * d^((i - 1) - (j - 1))
    }
  }
  
  # Terminal payoff
  option[N + 1, ] <- pmax(K - stock[N + 1, ], 0)
  
  # Backward induction
  for (i in N:1) {
    for (j in 1:i) {
      
      continuation <- ((1 - p) * option[i + 1, j] +
                         p * option[i + 1, j + 1]) / exp(r * dt)
      
      exercise <- max(K - stock[i, j], 0)
      
      option[i, j] <- max(exercise, continuation)
      
      exercise_flag[i, j] <- exercise > 0 && exercise >= continuation
    }
  }
  
  # Optimal exercise boundary
  boundary <- data.frame()
  
  for (i in 1:N) {
    idx <- which(exercise_flag[i, 1:i])
    
    if (length(idx) > 0) {
      boundary <- rbind(
        boundary,
        data.frame(
          time = (i - 1) * dt,
          optimal_exercise_price = max(stock[i, idx]),
          model = model
        )
      )
    }
  }
  
  list(
    model = model,
    price = option[1, 1],
    boundary = boundary
  )
}


# Run all models
results <- lapply(models, function(m) {
  american_put(m, S0, K, T, r, vol, N)
})

# Option prices
prices <- data.frame(
  Model = models,
  American_Put_Price = sapply(results, function(x) x$price)
)

print(prices)

# Boundary data
boundary_data <- do.call(rbind, lapply(results, function(x) x$boundary))

# Chọn bớt các điểm để hiển thị dấu chấm
point_data <- boundary_data[
  round(boundary_data$time, 3) %% 0.025 == 0,
]

# Plot optimal exercise boundary
ggplot(boundary_data, aes(x = time, y = optimal_exercise_price)) +
  geom_line(linewidth = 1, color = "black") +
  geom_point(
    data = point_data,
    aes(x = time, y = optimal_exercise_price),
    size = 2,
    color = "red"
  ) +
  facet_wrap(~ model, ncol = 2, scales = "free_y") +
  labs(
    title = "Optimal Exercise Boundary for American Put",
    x = "Time t (years)",
    y = "Optimal Exercise Stock Price"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5)
  )

