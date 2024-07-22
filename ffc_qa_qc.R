# QAQC

print("Create functions for QA/QC checks.")
# assert, verify

# all.equal, all.identical

# Validation Function
# Arguments: dataframe + any number of numeric columns
# Checks for negative values, then NA values


validation_seds_raw <- function(data) {
  
  data %>%
  verify(nrow(.) > 0) %>%
    # SEDS data from RIA should have no NA values anywhere
    assert(not_na, state:unit) %>%
    # Should there be a range for values? Some are negative...
    # Nothing exceeds 5000 bBtu(as of 2023)
    assert(within_bounds(-1000, 5000), value)
  

}


validation_fha_raw <- function(data) {
  
  data %>%
    verify(nrow(.) > 0) %>%
    # SEDS data from RIA should have no NA values anywhere
    assert(not_na, year:state) %>%
    # All percentages must be between 0 and 1
    assert(within_bounds(0, 1), ends_with("percent"))
  
  
}


