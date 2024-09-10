# Access DOE data via DOE API

# This script contains two options for retrieving SEDS data from the EIA API:
# 1) retrieve entire dataset for all states + DC, 1990-present, inclusive; 
# 2) retrieve most recent year of data and append to the existing SEDS csv.

print("Retreieving SEDS data via EIA's API.")

# Objects Created--------------------------------------------------------

# Persistent objects created in the global environment:

# api_seds (dataframe): desired data pulled from the 'api_results' list.


# API Key----------------------------------------------------------------

# API key generated 11/22/23 
key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"

# API key check
eia_api_url <- "https://api.eia.gov/v2/seds/data"
response <- GET(eia_api_url, query = list(api_key = key))
# If status = 200, then it's working
print(response)

# Function for Options 1 & 2---------------------------------------------

# Function to Query EIA API
get_results <- function(state, year, offset) {
  
  # offset_by is the offset (i.e., row to start with) for pagination
  # For now, to avoid exceeding the 5000-row data limit, we will pull
  # only one year and state per query. This requires n=51*years API queries. 
  results <- paste0("https://api.eia.gov/v2/seds/data/?frequency=annual",
                    "&data[0]=value", 
                    "&facets[stateId][]=", state, # state input
                    "&start=", year - 1, # start = previous year for some reason
                    "&end=", year,
                    "&sort[0][column]=period&sort[0][direction]=desc&offset=",
                    offset, "&length=5000", # offset (usually 0)
                    "&api_key=", key) %>% # our API key is required
    GET() %>% # retrieve page from url
    content("raw") %>% # extract content as a raw vector
    rawToChar() %>% # convert to character data
    fromJSON() # convert from JSON to R object
  
  # The data limit from EIA's API is 5000 rows per query. 
  # Here, we check the results to see if we exceeded that. 
  # Extract warnings (if they exist)
  limits <- pluck(results, "response", "warnings", "warning")
  limits <- ifelse(is_empty(limits), "nothing", limits)
  print(limits)
  
  # Check if data limit (5000 rows) was reached, ignoring empty values
  limit_reached <<- case_when(
    limits == "nothing" ~ FALSE,
    str_detect(limits, "incomplete return") ~ TRUE,
    .default = FALSE)
  print(limit_reached)
  
  return(results)
  
}



# Option 1: Retrieve All SEDS Data-------------------------------------------

# Apply API data query function across all states and years.
# Using tic and toc() will indicate the time elapsed. Expected: about 18 min.
tic()
api_results <- expand_grid(state = states_and_dc, 
                           year = 1990:2022, 
                           offset = 0) %>%
  pmap(function(state, year, offset) get_results(state, year, offset))
toc()

 api_seds <- api_results %>%
        map(\(.x) pluck(.x, "response", "data")) %>%
  list_rbind()

 # Write data to csv file
write_csv(api_seds, "data/api_seds.csv")

# The script read_seds_data.R performs further transformation of this data.

# Option 2: Retrieve New Year of Data Only---------------------------------

api_results_new <- expand_grid(state = states_and_dc, 
                               year = 2022, 
                               offset = 0) %>%
  pmap(function(state, year, offset) get_results(state, year, offset))

api_seds_new <- api_results_new %>%
  map(\(.x) pluck(.x, "response", "data")) %>%
  list_rbind() %>% mutate(period = as.numeric(period), 
                          value = as.numeric(value))

# Load existing data
api_seds <- read_csv("data/api_seds.csv") 

# Check if new data is different from existing data
new_stuff <- api_seds_new %>% 
  filter(seriesId %in% msn_lookup) %>%
  # Remove any superfluous spaces from the character data
  mutate(across(where(is.character), ~ str_trim(.))) %>%
  # Filter: only retain new data that's not in the existing dataset
  anti_join(api_seds %>% 
              filter (period == "2022",
                      seriesId %in% msn_lookup))

# View the results
new_stuff

# If it looks OK, append new data to existing dataset
api_seds <- read_csv("data/api_seds.csv") %>%
  rbind(api_seds_new)

# Save the updated file 
write_csv(api_seds, "data/api_seds.csv")


# Cleanup-------------------------------------------------------------------

# Remove unneeded objects 

rm(c(key, get_results, api_results, limited_reached))

# Notes --------------------------------------------------------------------

# For total CO2 emissions:
# co2-emissions-aggregates
# Fuel types: CO, NG, PE, TO (total)
# All states are postal code; all years are 4-digit
# Sectors: CC (commercial), EC (electric power), IC (industrial), 
#   RC (residential), TC (transportation), TT (total)

# CO2 emissions w/ carbon coefficients are more complex:
# co2-emissions-and-carbon-coefficients
# All states are postal code; all years are 4-digit
# Data type: carbon-coefficient, emissions
# Fuel type and series are numeric; see long list 
