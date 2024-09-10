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



# WHY AM I GETTING AN ERROR?
validation_seds_raw <- function(data) {
  
  # NOTE Must use as.numeric before chain_start or else it throws errors
  
    chain_start(data) %>%
    # Evaluate the entire dataset using verify()
    # Verify than the dataset isn't empty
    # verify(nrow(.) > 0) %>%
    
    # Evaluate every column element independently using assert()
    # SEDS data from RIA should have no NA values anywhere
    # assert(not_na, year:sector_description) %>%
    
    # Evaluate every column element as a whole using insist()

    insist(within_n_sds(4), v) %>%

    # Evaluate every row independently using assert_rows()
    # Nothing exceeds 5000 bBtu(as of 2023)
    # assert_rows(within_bounds(-1000, 5000), value) %>%

    # Evaluate every row relative to other rows using insist_rows()
    # Nothing exceeds 5000 bBtu(as of 2023)
    # insist_rows(maha_dist, within_n_mads(10), everything()) %>%
    
    chain_end() 
  
}
validation_seds_raw(data = mydata)
validation_seds_raw(us_consumption %>% select(msn, value) %>% slice(1:5))

mydata <- us_consumption %>% select(msn, v = value) %>% slice(1:10) %>% mutate(v = as.numeric(v))
insist(mydata, within_n_sds(3), v)
  
validation_fha_raw <- function(data) {
  
  data %>%
    verify(nrow(.) > 0) %>%
    # SEDS data from EIA should have no NA values anywhere
    assert(not_na, year:state) %>%
    # All percentages must be between 0 and 1
    assert(within_bounds(0, 1), ends_with("percent"))
  
  
}


