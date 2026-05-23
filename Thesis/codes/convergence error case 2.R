rm(list = ls())

# =========================
# Input
# =========================
S0    <- 75
F     <- 100
cp    <- 6
T     <- 5
r     <- 0.07
k     <- 0.03
sigma <- 0.20
m     <- 1
b     <- r

# =========================
# Convertible bond price
# =========================
price_cb <- function(model, n) {
  dt <- T / n
  
  if (model == "CRR") {
    u  <- exp(sigma * sqrt(dt))
    d  <- exp(-sigma * sqrt(dt))
    pi <- (exp(b * dt) - d) / (u - d)
  }
  
  if (model == "Tian") {
    v  <- exp(sigma^2 * dt)
    u  <- 0.5 * exp(b * dt) * v *
      (v + 1 + sqrt(v^2 + 2 * v - 3))
    d  <- 0.5 * exp(b * dt) * v *
      (v + 1 - sqrt(v^2 + 2 * v - 3))
    pi <- (exp(b * dt) - d) / (u - d)
  }
  
  if (model == "JR") {
    u  <- exp((b - 0.5 * sigma^2) * dt + sigma * sqrt(dt))
    d  <- exp((b - 0.5 * sigma^2) * dt - sigma * sqrt(dt))
    pi <- 0.5
  }
  
  if (model == "Haahtela") {
    u  <- exp(b * dt) * (1 + sqrt(exp(sigma^2 * dt) - 1))
    d  <- exp(b * dt) * (1 - sqrt(exp(sigma^2 * dt) - 1))
    pi <- (exp(b * dt) - d) / (u - d)
  }
  
  stock <- matrix(NA, n + 1, n + 1)
  price <- matrix(NA, n + 1, n + 1)
  q     <- matrix(NA, n + 1, n + 1)
  rate  <- matrix(NA, n + 1, n + 1)
  
  for (t in 0:n) {
    for (j in 0:t) {
      stock[j + 1, t + 1] <- S0 * u^(t - j) * d^j
    }
  }
  
  for (j in 0:n) {
    conversion <- m * stock[j + 1, n + 1]
    redemption <- F + cp
    
    price[j + 1, n + 1] <- max(conversion, redemption)
    q[j + 1, n + 1] <- ifelse(conversion >= redemption, 1, 0)
    rate[j + 1, n + 1] <- r + (1 - q[j + 1, n + 1]) * k
  }
  
  for (t in (n - 1):0) {
    coupon <- ifelse((t + 1) %% 1 == 0, cp, 0)
    
    for (j in 0:t) {
      conversion_now <- m * stock[j + 1, t + 1]
      
      continuation <- coupon +
        pi * price[j + 1, t + 2] * exp(-rate[j + 1, t + 2] * dt) +
        (1 - pi) * price[j + 2, t + 2] * exp(-rate[j + 2, t + 2] * dt)
      
      price[j + 1, t + 1] <- max(conversion_now, continuation)
      
      q[j + 1, t + 1] <- ifelse(
        conversion_now >= continuation,
        1,
        pi * q[j + 1, t + 2] + (1 - pi) * q[j + 2, t + 2]
      )
      
      rate[j + 1, t + 1] <- r + (1 - q[j + 1, t + 1]) * k
    }
  }
  
  price[1, 1]
}

# =========================
# Convergence error
# =========================
models  <- c("CRR", "Tian", "JR", "Haahtela")
n_grid  <- seq(5, 2000, by = 5)
n_bench <- 2000

conv_df <- do.call(rbind, lapply(models, function(model) {
  benchmark <- price_cb(model, n_bench)
  
  data.frame(
    model = model,
    n = n_grid,
    error = sapply(n_grid, function(n) {
      abs(price_cb(model, n) - benchmark)
    })
  )
}))

# =========================
# Plot
# =========================
plot(
  NULL,
  xlim = range(conv_df$n),
  ylim = range(conv_df$error),
  xlab = "Number of Steps",
  ylab = "|Price(n) - Price(2000)|",
  main = "Convergence Error of Convertible Bond Price",
  bty = "n"
)

cols <- c("black", "red", "blue", "darkgreen")

for (i in seq_along(models)) {
  tmp <- conv_df[conv_df$model == models[i], ]
  lines(tmp$n, tmp$error, col = cols[i], lwd = 2)
}

legend(
  "topright",
  legend = models,
  col = cols,
  lty = 1,
  lwd = 2,
  bty = "n"
)

grid()