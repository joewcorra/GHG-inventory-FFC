# QAQC

# assert, verify

# all.equal, all.identical

# Validation Function
# Arguments: dataframe + any number of numeric columns
# Checks for negative values, then NA values

validation <- function(data, ...) {
  
  print(...)
  # Create a vector of column names
  columns <- list(...) %>% unlist()
  
  # Check for negative values
  data %>%
    assert(within_bounds(0, Inf), all_of(columns))
  
  # Check for NA values
  data %>%
    assert(not_na, all_of(columns))
  
  # assert can also use in_set and is_uniq
  # May also use 'insist' to find values within n SDs (within_n_sds)
  # Can also look across rows ('insist_rows', maha_dist')
  # Note that maha_dist can be used for character columns too
  # See https://cran.r-project.org/web/packages/assertr/vignettes/assertr.html
}

# Example
# validation(carbon, "ibf_adjusted_value", "carbon_factor")


