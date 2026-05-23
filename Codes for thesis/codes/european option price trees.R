#Input
rm(list = ls())

vol <- 0.284345044546826
S0  <- 23750
K   <- 30000
T   <- 1/2
N   <- 4
r   <- 0.07
dt  <- T / N

#Build stock-tree
build_stock_tree <- function(S, u, d, N) {
  tree <- matrix(0, nrow = N + 1, ncol = N + 1)

  for (i in 0:N) {
    for (j in 0:i) {
      tree[i + 1, j + 1] <- S * u^j * d^(i - j)
    }
  }

  tree
}

#Generate put option tree
price_put_tree <- function(stock_tree, q, r, dt, K) {
  n <- nrow(stock_tree) - 1
  option_tree <- matrix(0, nrow = n + 1, ncol = n + 1)

  option_tree[n + 1, ] <- pmax(K - stock_tree[n + 1, ], 0)

  for (i in n:1) {
    for (j in 1:i) {
      option_tree[i, j] <-
        ((1 - q) * option_tree[i + 1, j] +
           q * option_tree[i + 1, j + 1]) / exp(r * dt)
    }
  }

  option_tree
}

#Plot function
plot_tree <- function(tree_mat, main_title, digits = 2) {
  n <- nrow(tree_mat) - 1

  plot(
    NA,
    xlim = c(-0.2, n + 0.3),
    ylim = c(-n - 0.5, n + 0.5),
    xlab = "period",
    ylab = "node level",
    main = main_title,
    xaxt = "n",
    yaxt = "n",
    bty = "n"
  )

  axis(1, at = 0:n)
  axis(2, at = seq(-n, n, by = 2), las = 1)

  for (t in 0:n) {
    for (j in 0:t) {
      x <- t
      y <- 2 * j - t
      val <- round(tree_mat[t + 1, j + 1], digits)

      if (t < n) {
        segments(x, y, x + 1, y + 1)
        segments(x, y, x + 1, y - 1)
      }

      points(x, y, pch = 1, cex = 0.8)
      text(x, y, labels = val, pos = 3, cex = 0.7)
    }
  }
}

#CRR model
u_crr <- exp(vol * sqrt(dt))
d_crr <- exp(-vol * sqrt(dt))
q_crr <- (exp(r * dt) - d_crr) / (u_crr - d_crr)

crr_stock <- build_stock_tree(S0, u_crr, d_crr, N)
crr_tree  <- price_put_tree(crr_stock, q_crr, r, dt, K)

#Tian model
v_tian <- exp(vol^2 * dt)

u_tian <- 0.5 * exp(r * dt) * v_tian *
  (v_tian + 1 + sqrt(v_tian^2 + 2 * v_tian - 3))

d_tian <- 0.5 * exp(r * dt) * v_tian *
  (v_tian + 1 - sqrt(v_tian^2 + 2 * v_tian - 3))

q_tian <- (exp(r * dt) - d_tian) / (u_tian - d_tian)

tian_stock <- build_stock_tree(S0, u_tian, d_tian, N)
tian_tree  <- price_put_tree(tian_stock, q_tian, r, dt, K)

#Jarrow-Rudd model
u_jr <- exp((r - 0.5 * vol^2) * dt + vol * sqrt(dt))
d_jr <- exp((r - 0.5 * vol^2) * dt - vol * sqrt(dt))
q_jr <- 0.5

jr_stock <- build_stock_tree(S0, u_jr, d_jr, N)
jr_tree  <- price_put_tree(jr_stock, q_jr, r, dt, K)

#Haahtela model
u_haahtela <- exp(r * dt) * (1 + sqrt(exp(vol^2 * dt) - 1))
d_haahtela <- exp(r * dt) * (1 - sqrt(exp(vol^2 * dt) - 1))
q_haahtela <- (exp(r * dt) - d_haahtela) / (u_haahtela - d_haahtela)

haahtela_stock <- build_stock_tree(S0, u_haahtela, d_haahtela, N)
haahtela_tree  <- price_put_tree(haahtela_stock, q_haahtela, r, dt, K)

#Plot four trees in one frame
layout(matrix(1:4, nrow = 2, byrow = TRUE))

plot_tree(crr_tree, "CRR Option Price Tree")
plot_tree(tian_tree, "Tian Option Price Tree")
plot_tree(jr_tree, "Jarrow-Rudd Option Price Tree")
plot_tree(haahtela_tree, "Haahtela Option Price Tree")