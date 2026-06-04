rm(list = ls())

# =========================
# Input
# =========================
S0    <- 75
T     <- 5
r     <- 0.07
k     <- 0.03
m     <- 1
cp    <- 6
sigma <- 0.20
n     <- 5
dt    <- T / n

# =========================
# Tian parameters
# =========================
v  <- exp(sigma^2 * dt)
u  <- 0.5 * exp(r * dt) * v * 
  (v + 1 + sqrt(v^2 + 2 * v - 3))
d  <- 0.5 * exp(r * dt) * v * 
  (v + 1 - sqrt(v^2 + 2 * v - 3))
Pi <- (exp(r * dt) - d) / (u - d)

# =========================
# Stock price tree
# =========================
stock_tree <- matrix(NA, nrow = n + 1, ncol = n + 1)

for (t in 0:n) {
  for (j in 0:t) {
    stock_tree[j + 1, t + 1] <- S0 * u^(t - j) * d^j
  }
}
# =========================
# Conversion probability tree
# =========================
face_plus_coupon <- 100 + cp
q_tree <- matrix(NA, nrow = n + 1, ncol = n + 1)

for (j in 0:n) {
  ST <- stock_tree[j + 1, n + 1]
  q_tree[j + 1, n + 1] <- ifelse(m * ST > face_plus_coupon, 1, 0)
}

for (t in (n - 1):0) {
  for (j in 0:t) {
    q_up   <- q_tree[j + 1, t + 2]
    q_down <- q_tree[j + 2, t + 2]
    
    q_tree[j + 1, t + 1] <- Pi * q_up + (1 - Pi) * q_down
  }
}

# =========================
# Discount rate tree
# =========================
rate_tree <- r + (1 - q_tree) * k

# =========================
# Convertible bond price tree
# =========================
bond_tree <- matrix(NA, nrow = n + 1, ncol = n + 1)

for (j in 0:n) {
  ST <- stock_tree[j + 1, n + 1]
  bond_tree[j + 1, n + 1] <- max(m * ST, face_plus_coupon)
}

for (t in (n - 1):0) {
  for (j in 0:t) {
    S_now <- stock_tree[j + 1, t + 1]
    
    P_up   <- bond_tree[j + 1, t + 2]
    P_down <- bond_tree[j + 2, t + 2]
    
    r_up   <- rate_tree[j + 1, t + 2]
    r_down <- rate_tree[j + 2, t + 2]
    
    continuation_value <- cp +
      Pi * P_up * exp(-r_up * dt) +
      (1 - Pi) * P_down * exp(-r_down * dt)
    
    conversion_value <- m * S_now
    
    bond_tree[j + 1, t + 1] <- max(conversion_value, continuation_value)
  }
}

# =========================
# Plot function
# =========================
plot_tree <- function(tree, title = "Tree", digits = 2) {
  n <- ncol(tree) - 1
  
  plot(
    NA,
    xlim = c(-0.2, n + 0.3),
    ylim = c(-n - 0.5, n + 0.5),
    xlab = "Time step",
    ylab = "Node level",
    main = title,
    xaxt = "n",
    yaxt = "n",
    bty = "n"
  )
  
  axis(1, at = 0:n)
  axis(2, at = seq(-n, n, by = 2), las = 1)
  
  for (t in 0:n) {
    for (j in 0:t) {
      value <- tree[j + 1, t + 1]
      
      if (!is.na(value)) {
        x <- t
        y <- t - 2 * j
        
        if (t < n) {
          segments(x, y, x + 1, y + 1)
          segments(x, y, x + 1, y - 1)
        }
        
        points(x, y, pch = 1, cex = 0.8)
        text(x, y, labels = round(value, digits), pos = 3, cex = 0.7)
      }
    }
  }
}

# =========================
# Plot 4 trees in one frame
# =========================
layout(matrix(1:4, nrow = 2, byrow = TRUE))

plot_tree(stock_tree, "Stock Price Tree", digits = 2)
plot_tree(q_tree, "Conversion Probability Tree", digits = 2)
plot_tree(rate_tree, "Discount Rate Tree", digits = 4)
plot_tree(bond_tree, "Convertible Bond Price Tree", digits = 2)