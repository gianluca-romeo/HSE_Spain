#==============================================================================
#VARIATION OF VARIABLES OVER TIME
#==============================================================================

#==============================================================================
#HSE variation in Spain
#==============================================================================
#------------------------------------------------------------------------------
#by municipalities
#------------------------------------------------------------------------------
#variation by 4-years periods (1995 - 1999 ... 2019-2023)
target_years <- seq(2023, 1995, by = -4)

variations_ratio <- compute_variations(data = dataset,
                                       id_var = COD_MUNI,
                                       prefix = "ratio_",
                                       years = target_years,
                                       log_change = FALSE)

#a new dataset without the outliers, top 2% up and down
no_out <- remove_outliers(variations_ratio, 
                          variation, 
                          lower_q = 0.02, 
                          upper_q = 0.98, 
                          group_var = period)

#plot the maps by period
variat_ratio <- ggplot(no_out) +
  geom_sf(aes(fill = variation), color = NA) +
  facet_wrap(~ period) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "lightyellow",
    high = "darkgreen",
    midpoint = 0,
    na.value = "lightgrey"
  ) +
  theme_minimal() +
  labs(
    title = "Elasticity proxy variations over the time for each municipality in Spain",
    fill = "Variation in p.p."
  ) +
  theme(
    strip.text = element_text(size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  filename = "outputs/figures/variat_ratio.png",
  plot = variat_ratio,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

#clean the workspace
rm(variations_ratio, no_out)

#------------------------------------------------------------------------------
#by CCAA
#------------------------------------------------------------------------------
#variation by 4-years periods (1995 - 1999 ... 2019-2023)

#weighted mean of the ratio in the comunidad autonoma by periods
data_ccaa <- dataset %>%
  group_by(COD_CCAA) %>%
  summarise(
    W_RATIO_2023 = weighted.mean(ratio_2023, totsurf_2023, na.rm = TRUE),
    W_RATIO_2019 = weighted.mean(ratio_2019, totsurf_2019, na.rm = TRUE),
    W_RATIO_2015 = weighted.mean(ratio_2015, totsurf_2015, na.rm = TRUE),
    W_RATIO_2011 = weighted.mean(ratio_2011, totsurf_2011, na.rm = TRUE),
    W_RATIO_2007 = weighted.mean(ratio_2007, totsurf_2007, na.rm = TRUE),
    W_RATIO_2003 = weighted.mean(ratio_2003, totsurf_2003, na.rm = TRUE),
    W_RATIO_1999 = weighted.mean(ratio_1999, totsurf_1999, na.rm = TRUE),
    W_RATIO_1995 = weighted.mean(ratio_1995, totsurf_1995, na.rm = TRUE)
  )

variations_ratio_CCAA <- compute_variations(data = data_ccaa, 
                                            id_var = COD_CCAA, 
                                            prefix = "W_RATIO_", 
                                            years = target_years, 
                                            log_change = FALSE)

#plot the maps by period
variat_ratio_ccaa <- ggplot(variations_ratio_CCAA) +
  geom_sf(aes(fill = variation), color = NA) +
  facet_wrap(~ period) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "lightyellow",
    high = "darkgreen",
    midpoint = 0,
    na.value = "lightgrey"
  ) +
  theme_minimal() +
  labs(
    title = "Elasticity proxy variations over the time for each CCAA in Spain",
    fill = "Variation in p.p."
  ) +
  theme(
    strip.text = element_text(size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  filename = "outputs/figures/variat_ratio_ccaa.png",
  plot = variat_ratio_ccaa,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

rm(data_ccaa, variations_ratio_CCAA)

#==============================================================================
#Built surface over the time in Spain
#==============================================================================
#------------------------------------------------------------------------------
#by municipalities
#------------------------------------------------------------------------------

variations_built = compute_variations(data = dataset, 
                                      id_var = COD_MUNI, 
                                      prefix = "built_", 
                                      years = target_years, 
                                      log_change = TRUE)

#a new dataset without the outliers, top 2% up and down
no_out <- remove_outliers(data = variations_built, 
                          variable = variation, 
                          lower_q = 0.02, 
                          upper_q = 0.98, 
                          group_var = period)

variat_built <- ggplot(no_out) +
  geom_sf(aes(fill = variation), color = NA) +
  facet_wrap(~ period) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "lightyellow",
    high = "darkgreen",
    midpoint = 0,
    na.value = "lightgrey"
  ) +
  theme_minimal() +
  labs(
    title = "Built surface variations over the time for each municipality in Spain",
    fill = "Yearly variation (%)"
  ) +
  theme(
    strip.text = element_text(size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  filename = "outputs/figures/variat_built.png",
  plot = variat_built,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

#clean the workspace
rm(variations_built, no_out)

#------------------------------------------------------------------------------
#by CCAA
#------------------------------------------------------------------------------
#variation by 4-years periods (1995 - 1999 ... 2019-2023)

#built surface in the comunidad autonoma by periods
data_ccaa <- dataset %>%
  group_by(COD_CCAA) %>%
  summarise(
    BUILT_2023 = sum(built_2023, na.rm = TRUE),
    BUILT_2019 = sum(built_2019, na.rm = TRUE),
    BUILT_2015 = sum(built_2015, na.rm = TRUE),
    BUILT_2011 = sum(built_2011, na.rm = TRUE),
    BUILT_2007 = sum(built_2007, na.rm = TRUE),
    BUILT_2003 = sum(built_2003, na.rm = TRUE),
    BUILT_1999 = sum(built_1999, na.rm = TRUE),
    BUILT_1995 = sum(built_1995, na.rm = TRUE)
  )

variations_built_CCAA <- compute_variations(data = data_ccaa, 
                                            id_var = COD_CCAA,
                                            prefix = "BUILT_",
                                            years = target_years, 
                                            log_change = TRUE)

#plot the maps by period
variat_built_ccaa <- ggplot(variations_built_CCAA) +
  geom_sf(aes(fill = variation), color = NA) +
  facet_wrap(~ period) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "lightyellow",
    high = "darkgreen",
    midpoint = 0,
    na.value = "lightgrey"
  ) +
  theme_minimal() +
  labs(
    title = "Variations over the time of the built surface at CCAA level in Spain",
    fill = "Yearly variation (%)"
  ) +
  theme(
    strip.text = element_text(size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  filename = "outputs/figures/variat_built_ccaa.png",
  plot = variat_built_ccaa,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

rm(data_ccaa, variations_built_CCAA)



#==============================================================================
#Total urban surface over the time in Spain
#==============================================================================
#------------------------------------------------------------------------------
#by municipalities
#------------------------------------------------------------------------------

variations_totsurf = compute_variations(data = dataset, 
                                        id_var = COD_MUNI, 
                                        prefix = "totsurf_", 
                                        years = target_years, 
                                        log_change = TRUE)

#a new dataset without the outliers, top 2% up and down
no_out <- remove_outliers(data = variations_totsurf, 
                          variable = variation, 
                          lower_q = 0.02, 
                          upper_q = 0.98, 
                          group_var = period)

variat_totsurf <- ggplot(no_out) +
  geom_sf(aes(fill = variation), color = NA) +
  facet_wrap(~ period) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "lightyellow",
    high = "darkgreen",
    midpoint = 0,
    na.value = "lightgrey"
  ) +
  theme_minimal() +
  labs(
    title = "Total urban surface variations over the time for each municipality in Spain",
    fill = "Yearly variation (%)"
  ) +
  theme(
    strip.text = element_text(size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  filename = "outputs/figures/variat_totsurf.png",
  plot = variat_totsurf,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

rm(variations_totsurf, no_out)

#------------------------------------------------------------------------------
#by CCAA
#------------------------------------------------------------------------------
#variation by 4-years periods (1995 - 1999 ... 2019-2023)

#total urban surface in the comunidad autonoma by periods
data_ccaa <- dataset %>%
  group_by(COD_CCAA) %>%
  summarise(
    TOTSURF_2023 = sum(totsurf_2023, na.rm = TRUE),
    TOTSURF_2019 = sum(totsurf_2019, na.rm = TRUE),
    TOTSURF_2015 = sum(totsurf_2015, na.rm = TRUE),
    TOTSURF_2011 = sum(totsurf_2011, na.rm = TRUE),
    TOTSURF_2007 = sum(totsurf_2007, na.rm = TRUE),
    TOTSURF_2003 = sum(totsurf_2003, na.rm = TRUE),
    TOTSURF_1999 = sum(totsurf_1999, na.rm = TRUE),
    TOTSURF_1995 = sum(totsurf_1995, na.rm = TRUE)
  )

variations_totsurf_CCAA <- compute_variations(data = data_ccaa, 
                                              id_var = COD_CCAA,
                                              prefix = "TOTSURF_",
                                              years = target_years, 
                                              log_change = TRUE)

#plot the maps by period
variat_totsurf_ccaa <- ggplot(variations_totsurf_CCAA) +
  geom_sf(aes(fill = variation), color = NA) +
  facet_wrap(~ period) +
  scale_fill_gradient2(
    low = "darkred",
    mid = "lightyellow",
    high = "darkgreen",
    midpoint = 0,
    na.value = "lightgrey"
  ) +
  theme_minimal() +
  labs(
    title = "Variations over the time of the total urban surface at CCAA level in Spain",
    fill = "Yearly variation (%)"
  ) +
  theme(
    strip.text = element_text(size = 12),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  filename = "outputs/figures/variat_totsurf_ccaa.png",
  plot = variat_totsurf_ccaa,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

rm(data_ccaa, variations_totsurf_CCAA)
