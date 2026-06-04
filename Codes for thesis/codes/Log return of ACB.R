#Draw a chart for ACB's log-return
prices <- prices %>%
  mutate(log_return = log(adjusted / lag(adjusted)))
prices %>%
  ggplot(aes(x = date, y = log_return)) +
  geom_line(color = "black", size = 0.3) +
  labs(title = "ACB log-returns",
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