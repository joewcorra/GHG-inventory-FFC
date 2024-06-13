# QAQC

print("Create functions for QA/QC checks.")
# assert, verify

# all.equal, all.identical

# Validation Function
# Arguments: dataframe + any number of numeric columns
# Checks for negative values, then NA values

validation_1 <- function(data) {
  
  data %>%
  verify(nrow(.) > 0) %>%
    assert(not_na, state:msn, value) %>%
    assert(within_bounds(0, 5000), value)
  

}


validation_2 <- function(data) {
  
  chain_start(data) %>%
    verify(nrow(.) > 0) %>%
    assert(not_na, state:msn, value) %>%
    assert(within_bounds(0, Inf), value) %>%
    assert(within_bounds(0, 10000), adjusted_value) %>%
    chain_end()

  
}


