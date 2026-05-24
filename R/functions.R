#function to remove outliers
remove_outliers <- function(data,
                            variable,
                            lower_q = NULL,
                            upper_q = 0.98,
                            group_var = NULL) {
  
  variable <- rlang::ensym(variable)
  group_var <- rlang::enquo(group_var)
  
  if (!rlang::quo_is_null(group_var)) {
    data <- data %>% group_by(!!group_var)
  }
  
  data %>%
    mutate(
      lower = if (!is.null(lower_q))
        quantile(!!variable, lower_q, na.rm = TRUE)
      else
        -Inf,
      
      upper = quantile(!!variable, upper_q, na.rm = TRUE),
      
      !!variable := ifelse(
        !!variable < lower | !!variable > upper,
        NA,
        !!variable
      )
    ) %>%
    ungroup() %>%
    select(-lower, -upper)
}



#function to compute variations of variables over time
compute_variations <- function(data, 
                               id_var, 
                               prefix, 
                               years, 
                               log_change = FALSE) {
  
  id_var <- rlang::ensym(id_var)
  
  data %>%
    select(!!id_var, starts_with(prefix)) %>%
    pivot_longer(
      cols = starts_with(prefix),
      names_to = "year",
      names_prefix = prefix,
      values_to = "value"
    ) %>%
    mutate(year = as.integer(year)) %>%
    filter(year %in% years) %>%
    arrange(!!id_var, year) %>%
    group_by(!!id_var) %>%
    arrange(desc(year)) %>%
    mutate(
      value_lag4 = lead(value),
      year_lag4 = lead(year)
    ) %>%
    filter(!is.na(year_lag4)) %>%
    mutate(
      variation = if (log_change) {
        ifelse(value_lag4 == 0, NA, ((1 / 4) * (log(value) - log(value_lag4))) * 100)
      } else {
        (value - value_lag4) * 100
      },
      period = paste0(year_lag4, "-", year)
    ) %>%
    select(!!id_var, period, variation)
}