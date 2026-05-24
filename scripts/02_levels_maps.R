#==============================================================================
#HSE in Spain in 2024 
#==============================================================================

#------------------------------------------------------------------------------
#by municipalities
#------------------------------------------------------------------------------

#plot the map of the ratio for 2024
map2024 <- ggplot(dataset) +
  geom_sf(aes(fill = ratio_2024), color = NA) +
  scale_fill_gradient(
    low = "darkviolet",
    high = "lightyellow",
    na.value = "lightgrey" 
  ) +
  theme_minimal() +
  labs(
    title = "Elasticity proxy for each municipality in Spain (2024)",
    fill = "Ratio"
  )

ggsave(
  filename = "outputs/figures/map2024.png",
  plot = map2024,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

#outliers - top 2%
dataset_out <- dataset %>%
  mutate(across(ratio_2024, ~ {
    p <- quantile(.x, probs = c( 0.98), na.rm = TRUE)
    ifelse(.x < p[1], NA, .x)
  }))

#plot the map of the outliers for 2024
map2024_out <- ggplot(dataset_out) +
  geom_sf(aes(fill = ratio_2024), color = NA) +
  scale_fill_gradient(
    low = "#ff5b5b", #from the lower
    high = "#a60000", #to the higher
    na.value = "lightgrey"
  ) +
  theme_minimal() +
  labs(
    title = "Outliers in 2024 - top 2%",
    fill = "Ratio"
  )

ggsave(
  filename = "outputs/figures/map2024_out.png",
  plot = map2024_out,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

#remove outliers
dataset_no_out <- remove_outliers(data = dataset, 
                                  variable = ratio_2024)

#plot the map of the ratio for 2024 without the outliers
map2024_no_out <- ggplot(dataset_no_out) +
  geom_sf(aes(fill = ratio_2024), color = NA) +
  scale_fill_gradient(
    low = "darkviolet",
    high = "lightyellow",
    na.value = "lightgrey"
  ) +
  theme_minimal() +
  labs(
    title = "Elasticity proxy for each municipality in Spain (2024) - no outliers",
    fill = "Ratio"
  )

ggsave(
  filename = "outputs/figures/map2024_no_out.png",
  plot = map2024_no_out,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

#clean the workspace
rm(dataset_out, dataset_no_out)


#------------------------------------------------------------------------------
#by CCAA
#------------------------------------------------------------------------------

#weighted mean of the ratio in 2024 by comunidad autonoma
data_ccaa <- dataset %>%
  st_drop_geometry() %>%
  group_by(COD_CCAA) %>%
  summarise(
    W_RATIO = weighted.mean(ratio_2024, totsurf_2024, na.rm = TRUE)
  )

#geographical borders of comunidad autonoma
ccaa <- esp_get_ccaa_siane()

#transform the variables to character type for merging
ccaa$codauto <- as.character(ccaa$codauto)
data_ccaa$COD_CCAA <- as.character(data_ccaa$COD_CCAA)

#new dataset with data by CCAA and geographical borders
map_ccaa <- ccaa %>%
  left_join(data_ccaa, by = c("codauto" = "COD_CCAA"))

#plot the map of the weighted ratio for 2024
map2024_ccaa <- ggplot(map_ccaa) +
  geom_sf(aes(fill = W_RATIO), color = NA) +
  scale_fill_gradient(
    low = "darkviolet",
    high = "lightyellow",
    na.value = "lightgrey"
  ) +
  theme_minimal() +
  labs(
    title = "Elasticity proxy for each CCAA in Spain (2024)",
    fill = "Weighted ratio"
  )

ggsave(
  filename = "outputs/figures/map2024_ccaa.png",
  plot = map2024_ccaa,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

#clean the workspace
rm(ccaa, data_ccaa, map_ccaa)