# Access DOE data via DOE API

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# key: character vector; API key generated from DoE or data.gov
# arguments: dataframe; user arguments that are passed to 'get_results'.
# get_results: function; queries the Dept of Energy API.
# limit_reached: boolean; indicates if API data limit has been reached.
# results: list; full results from the 'get_results' API query.
# data: dataframe; desired data pulled from the 'results' list.


# API Key----------------------------------------------------------------

key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"# API key generated 11/22/23 


# Retrieve DoE Data------------------------------------------------------

# user inputs should create a dataframe of arguments for API query function
# The following is an example:
arguments <- tibble(offset_by = 0, start_year = 2000, end_year = 2021, 
                    state = "TX", sector = "EC", fuel = "CO")

# Get Total CO2 Emissions By Sector and Fuel
# Consider adding more arguments to function call (e.g., year, state, etc.)
get_results <- function(arguments) {
  
  # offset_by is the offset (i.e., row to start with) for pagination

  results <- paste0("https://api.eia.gov/v2/co2-emissions/co2-emissions-aggregates/data/?frequency=annual&data[0]=value&facets[sectorId][]=",
                    arguments$sector, "&facets[stateId][]=",
                    arguments$state, "&facets[fuelId][]=",
                    arguments$fuel, "&start=",
                    arguments$start_year, "&end=",
                    arguments$end_year, "&sort[0][column]=period&sort[0]",
                    "[direction]=desc&offset=",
                    arguments$offset_by, "&length=5000", "&api_key=", key) %>%
  GET() %>% # retrieve page from url
  content("raw") %>% # extract content as a raw vector
  rawToChar() %>% # convert to character data
  fromJSON() # convert from JSON to R object

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

results <- get_results(arguments) 


data <- pluck(results, "response", "data") %>% # pluck the data frame
  # Check if data limit was reached; if so, get the rest and bind.
  {if (limit_reached == TRUE) bind_rows(., get_results(5001) %>% # start at 5001
                                      pluck("response", "data")) else .} %>%
  clean_names() # standardize column names
# NOTE: # This works, but need to consider what happens if rows > 10,000



# https://api.eia.gov/v2/co2-emissions/co2-emissions-aggregates/data/?frequency=annual&data[0]=value&facets[sectorId][]=TC&facets[stateId][]=KY&facets[fuelId][]=CO&start=2012&end=2013&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000




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
