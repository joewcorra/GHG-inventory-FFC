# QAQC for Uploaded Data (Pre-Calculation)


validator <- function(data) {
  
  data %>%
    verify(nrow(.) > 0) %>%
    assert(not_na, everything())

}


gasoline_distribution %>% 
  validator() %>%
  assert(within_bounds(0, 1), gasoline_percent)

diesel_distribution %>% 
  validator() %>%
  assert(within_bounds(0, 1), diesel_percent)

adjustments %>% 
  validator() %>%
  assert(within_bounds(-1000, 50000), national_value)
