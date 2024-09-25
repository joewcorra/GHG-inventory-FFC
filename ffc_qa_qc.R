# QAQC

# QAQC w/ assertr for FFC (generate a report?)
# Everything: 
#   Make sure numbers are class numeric
# Blank values
# Superfluous spaces
# Adherence to naming conventions
# And look for changes in past data
# 
# Group by source:
#   z-scores (detect methodological changes) and variance?
#   Outliers



print("Create functions for QA/QC checks.")
# assert, verify

# all.equal, all.identical

# Validation Function
# Arguments: dataframe + any number of numeric columns
# Checks for negative values, then NA values


validation_seds_raw <- function(data) {

  get_errors <- attr_getter("assertr_errors")
  
  
  # Data Grouped by MSN and State (i.e., years combined)
  errors <- data %>%
    group_by(msn, state) %>%
    group_map(~ chain_start(.x) %>%
          # Evaluate the entire dataset using verify()
          # Verify than the dataset isn't empty
          verify(nrow(.) == 32) %>%
          
          # Evaluate every column element independently using assert()
          # SEDS data from RIA should have no NA values anywhere
          assert(not_na, everything()) %>%
          
          # Evaluate every column element as a whole using insist()
          
          # insist_rows(maha_dist, within_n_mads(2), everything()) %>%
          
          chain_end(success_fun = success_logical, error_fun = error_append),
          
          .keep = TRUE) 
  
  errors <- errors %>% 
    keep(\(x) is.list(x)) %>%
    set_names(map_chr(., ~paste0(.x$state[1], "_", .x$msn[1]))) %>%
    imap(~ get_errors(.x) %>%
          pluck(1, 1) %>% 
          as_tibble() %>%
           mutate(dataset = .y, .before = 1)) 
  
  return(errors)
  
}

errors <- validation_seds_raw(seds) %>% list_rbind()


# Check for year-over-year changes with t-tests
# Data Grouped by MSN and Year (i.e., states combined)



stats_tests <- function(data) {
  
  paired <- data %>%
    mutate (test_group = case_when(
      year == latest_year ~ "newest", 
      year %in% (latest_year - 1:5) ~ "prior",
      .default = "not_used")) %>%
    group_by(state, test_group) %>%
    summarize(value = mean(value, na.rm = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = test_group) 
  
  # T-test the means
  t <- t.test(paired$prior, paired$newest, paired = TRUE)
  
  # F-test the variances
  f <- var.test(paired$prior, paired$newest)
  
  stats <- tibble(msn = data$msn[1], 
                  t_stat = t$statistic, t_p_value = t$p.value, 
                  f_stat = f$statistic, f_p_value = f$p.value)
  
  return(stats)
  
}

paired <- seds %>%
  group_by(msn) %>%
  group_map(~ stats_tests(.x), .keep = TRUE) %>%
  list_rbind() %>%
  mutate(significance = case_when(
    t_p_value < 0.05 & f_p_value < 0.05 ~ "both",
    t_p_value < 0.05 ~ "t-stat only",
    f_p_value < 0.05 ~ "f-stat only",
      .default = "none"))


"natural gas consumed by the transportation sector"  
"aviation gasoline blending components consumed by the industrial sector" 
"unfinished oils consumed by the industrial sector"   