# Calculate  National-Level CO2 Emissions
# Master Script




# Load required packages
source("ffc_libraries.R")

# # data retrieval scripts here (API, etc.)?
# source("eia_api.R")
# 
# # QAQC functions to be used in subsequent scripts
# source("qa_qc.R")

# EIA SEDS code MSN descriptors, US state postal codes
source("msn_descriptions.R")

# API Key----------------------------------------------------------------

# API key generated 11/22/23 
key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"

results <- paste0("https://api.eia.gov/v2/total-energy/data/?frequency", 
                  "=annual&data[0]=value&start=2005&end=2005&sort[0][column]", 
                  "=period&sort[0][direction]", 
                  "=desc&offset=0&length=5000&api_key=", key) %>% # our API key 
  GET() %>% # retrieve page from url
  content("raw") %>% # extract content as a raw vector
  rawToChar() %>% # convert to character data
    fromJSON() # convert from JSON to R object

api_national <- pluck(results, "response", "data") %>%
  mutate(sector_code = str_sub(msn, 3, 4)) %>%
  filter(unit == "Trillion Btu", 
         sector_code %in% c("AC", "IC", "RC", "CC", "EI"))

