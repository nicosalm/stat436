library(ggcorrplot)
library(ggplot2)
data(mtcars)

corr_matrix <- cor(mtcars)
print(round(corr_matrix, 2))

p_matrix <- cor_pmat(mtcars)

plot_enhanced <- ggcorrplot(
  corr_matrix,
  hc.order = TRUE,          # hierarchical clustering to group related variables
  type = "lower",           # show only lower triangle to avoid redundancy
  p.mat = p_matrix,         # p-value matrix for significance testing
  sig.level = 0.05,         # significance threshold
  insig = "blank",          # blank out non-significant correlations
  lab = TRUE,               # show correlation coefficients
  lab_size = 3.5,           # larger text for better readability
  method = "square",        # use squares for clean, tile-based visualization
  colors = c("#3B9AB2", "white", "#E46726"),
  outline.color = "white",
  tl.cex = 11,
  tl.col = "black",
  title = "Automobile Performance Correlations",
  ggtheme = theme_minimal(base_size = 12)
) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right",
    legend.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank()
  ) +
  labs(caption = "Only statistically significant correlations (p < 0.05) are shown.")

print(plot_enhanced)

ggsave("mtcars_correlations.png", plot_enhanced, width = 10, height = 8, dpi = 300)

efficiency_vars <- c("mpg", "wt", "disp", "hp", "cyl")

plot_focused <- ggcorrplot(
  cor(mtcars[, efficiency_vars]),
  hc.order = FALSE,
  lab = TRUE,
  lab_size = 4,
  method = "square",
  colors = c("#3B9AB2", "white", "#E46726"),
  title = "Fuel Efficiency Relationships",
  ggtheme = theme_minimal(base_size = 12)
) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank()
  )

print(plot_focused)

ggsave("mtcars_efficiency_correlations.png", plot_focused, width = 8, height = 6, dpi = 300)
