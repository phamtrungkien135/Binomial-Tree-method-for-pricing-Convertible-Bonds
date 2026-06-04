#Draw a chart for ACB' stock price over 6 years
symbols <- c("ACB.VN")
end_date   <- as.Date("2026-03-18")
start_date <- end_date - years(6)
prices <- tq_get(
  symbols,
  get  = "stock.prices",
  from = start_date,
  to   = end_date
)
glimpse(prices)
library(ggplot2)
library(dplyr)
prices %>%
  ggplot(aes(x = date, y = close)) +
  geom_line(color = "black", size = 0.4) +
  labs(title = "ACB stock price",
       x = NULL,
       y = NULL) +
  theme_minimal(base_family = "mono") +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.1),
    plot.background = element_rect(fill = "#eae3d5", color = NA),
    panel.background = element_rect(fill = "#eae3d5", color = NA),
    panel.grid.major.y = element_line(linetype = "dotted", color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_blank()
  )
#Finding volatility
price<-diff(log(prices$adjusted))  ## compute the log return
hist.vol<-sd(price,na.rm=T)*sqrt(252)
(vol <- hist.vol)