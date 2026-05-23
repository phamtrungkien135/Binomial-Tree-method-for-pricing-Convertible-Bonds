# =========================================================
# Input
# =========================================================
S0 <- 75
F  <- 100
cp <- 6
T  <- 5
r  <- 0.07
k  <- 0.03
sigma <- 0.20
m <- 1
b <- r

# =========================================================
# Convertible bond pricing - 4 binomial models
# =========================================================
price_cb <- function(model = c("CRR", "Tian", "JR", "Haahtela"), n = 100) {
  
  model <- match.arg(model)
  
  if (n %% T != 0) {
    stop("n must be a multiple of T.")
  }
  
  dt <- T / n
  coupon_steps <- seq(n / T, n, by = n / T)
  
  if (model == "CRR") {
    u  <- exp(sigma * sqrt(dt))
    d  <- 1 / u
    pi <- (exp(r * dt) - d) / (u - d)
  }
  
  if (model == "Tian") {
    v <- exp(sigma^2 * dt)
    M <- exp(r * dt)
    u <- 0.5 * M * v * (v + 1 + sqrt(v^2 + 2 * v - 3))
    d <- 0.5 * M * v * (v + 1 - sqrt(v^2 + 2 * v - 3))
    pi <- (exp(r * dt) - d) / (u - d)
  }
  
  if (model == "JR") {
    u  <- exp((r - 0.5 * sigma^2) * dt + sigma * sqrt(dt))
    d  <- exp((r - 0.5 * sigma^2) * dt - sigma * sqrt(dt))
    pi <- 0.5
  }
  
  if (model == "Haahtela") {
    u <- exp(r * dt) * (1 + sqrt(exp(sigma^2 * dt) -1))
	  d <- exp(r * dt) * (1 - sqrt(exp(sigma^2 * dt) -1))
	  pi <- (exp(r * dt) - d) / (u - d)
  }
  
  S  <- matrix(0, n + 1, n + 1)
  P  <- matrix(0, n + 1, n + 1)
  q  <- matrix(0, n + 1, n + 1)
  rr <- matrix(0, n + 1, n + 1)
  
  for (step in 0:n) {
    for (up in 0:step) {
      S[up + 1, step + 1] <- S0 * u^up * d^(step - up)
    }
  }
  
  maturity_coupon <- if (n %in% coupon_steps) cp else 0
  
  for (up in 0:n) {
    conv_val <- m * S[up + 1, n + 1]
    red_val  <- F + maturity_coupon
    
    P[up + 1, n + 1] <- max(conv_val, red_val)
    q[up + 1, n + 1] <- ifelse(conv_val >= red_val, 1, 0)
    rr[up + 1, n + 1] <- r + (1 - q[up + 1, n + 1]) * k
  }
  
  for (step in (n - 1):0) {
    coupon_next <- if ((step + 1) %in% coupon_steps) cp else 0
    
    for (up in 0:step) {
      conv_now <- m * S[up + 1, step + 1]
      
      cont_now <- coupon_next +
        pi * P[up + 2, step + 2] * exp(-rr[up + 2, step + 2] * dt) +
        (1 - pi) * P[up + 1, step + 2] * exp(-rr[up + 1, step + 2] * dt)
      
      P[up + 1, step + 1] <- max(conv_now, cont_now)
      
      q[up + 1, step + 1] <- ifelse(
        conv_now >= cont_now,
        1,
        pi * q[up + 2, step + 2] + (1 - pi) * q[up + 1, step + 2]
      )
      
      rr[up + 1, step + 1] <- r + (1 - q[up + 1, step + 1]) * k
    }
  }
  
  P[1, 1]
}

# =========================================================
# Convergence test
# =========================================================
models <- c("CRR", "Tian", "JR", "Haahtela")
n_grid <- seq(5, 2000, by = 5)

price_list <- list()

for (model in models) {
  prices <- numeric(length(n_grid))
  
  for (i in seq_along(n_grid)) {
    prices[i] <- price_cb(model = model, n = n_grid[i])
  }
  
  price_list[[model]] <- prices
  cat(model, "price at n =", max(n_grid), ":", tail(prices, 1), "\n")
}

# =========================================================
# Plot convergence
# =========================================================
old_par <- par(no.readonly = TRUE)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (model in models) {
  plot(
    n_grid, price_list[[model]],
    type = "l", lwd = 2,
    xlab = "Number of steps n",
    ylab = "Convertible bond price",
    main = paste("Convergence -", model)
  )
  grid()
}

par(old_par)