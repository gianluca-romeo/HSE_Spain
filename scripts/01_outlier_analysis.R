#==============================================================================
#OUTLIERS
#==============================================================================

#create a dummy: 1 if the ratio is in top 2%
data_out <- dataset %>%
  mutate(
    dens_pop = pop_2024 / totsurf_2024,
    outlier = ifelse(ratio_2024 > quantile(ratio_2024, 0.98, na.rm = TRUE), 1, 0),
    outlier_f = factor(outlier, levels = c("0", "1"), labels = c("Non outlier", "Outlier"))
  )

#plot population density and ratio. Outliers in red
out2024 <- ggplot(data_out, aes(x = dens_pop, y = ratio_2024, color = outlier_f)) +
  geom_point(alpha = 0.6) +
  labs(
    x = "Population density",
    y = "Proxy of the HSE",
    color = "Category"
  ) +
  scale_color_manual(values = c("Non outlier" = "grey", "Outlier" = "red")) +
  theme_minimal()

ggsave(
  filename = "outputs/figures/out2024.png",
  plot = out2024,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

#clean the workspace
rm(data_out)