# Calculate State-Level CO2 Emissions
# Step 1: Total Fuel Consumption by Fuel Type and Sector


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:



# API Key----------------------------------------------------------------

# API key generated 11/22/23 
key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"

# Set Year---------------------------------------------------------------

# Change to match most recent available year (current year minus two)
latest_year <- year(Sys.Date()) -2

# Read EIA Consumption Data----------------------------------------------

eia_api_consumption <- paste0(
  "https://api.eia.gov/v2/total-energy/data/?frequency", 
  "=annual&data[0]=value&start=1990&end=2022&sort[0][column]", 
  "=period&sort[0][direction]", 
  "=desc&offset=0&length=5000&api_key=", key) %>% # our API key 
  GET() %>% # retrieve page from url
  content("raw") %>% # extract content as a raw vector
  rawToChar() %>% # convert to character data
  fromJSON() # convert from JSON to R object

eia_national <- pluck(eia_api_consumption, "response", "data") %>%
  mutate(msn = str_sub(msn, 1, 5)) %>%
  filter(str_detect(unit, "Btu"), 
         str_sub(msn, 3,4) %in% c("AC", "IC", "RC", "CC", "EI")) %>%
  select(-unit, -seriesDescription)

us_consumption <- eia_national %>%
  left_join(msn_names$msn, by = "msn") %>%
  filter(msn %in% msn_names$msn_lookup) %>%
  # Remove "(consumption)" from electric power sector description
  mutate(sector_description = if_else(
    str_detect(sector_description, "electric power"), 
      "electric power sector", sector_description), 
    # Make btu value numeric and remove non-numeric data (generates warning)
   value = parse_number(value)) %>%
  rename(year = period) 

# Apply labels to variables 
us_consumption <- apply_variable_labels(us_consumption)


# Read EIA Heat Content Data----------------------------------------------

# Heat content may vary and is used for some adjustments
eia_api_heat <- paste0(
  "https://api.eia.gov/v2/total-energy/data/?frequency=annual&data[0]", 
  "=value&facets[msn][]=DMTCKUS&facets[msn][]=MGTCKUS&start=1990&end=", 
  latest_year, 
  "&sort[0][column]=msn&sort[0][direction]=asc&offset=0&length=5000&api_key=",
  key) %>%
  GET() %>% # retrieve page from url
  content("raw") %>% # extract content as a raw vector
  rawToChar() %>% # convert to character data
  fromJSON() # convert from JSON to R object

# Units in Millions of Btu / Barrel
heat_content <- pluck(eia_api_heat, "response", "data") %>%
  select(year = period, msn, 
         eia_description = seriesDescription, heat_content = value) %>%
  # Make heat content value numeric
  mutate(heat_content = as.numeric(heat_content))


# Read EIA Vessel Bunkering Diesel Data----------------------------------

eia_api_vessel_bunker <- paste0(
  "https://api.eia.gov/v2/petroleum/cons/821usea/data/?frequency=annual",
  "&data[0]=value&facets[duoarea][]=NUS&facets[process][]=VAB&start=1990&end=",
  latest_year, 
  "&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000",
  "&api_key=", key) %>%
  GET() %>% # retrieve page from url
  content("raw") %>% # extract content as a raw vector
  rawToChar() %>% # convert to character data
  fromJSON() # convert from JSON to R object

# Units in Millions of Gallons
vessel_bunker_dist_fuel <- pluck(eia_api_vessel_bunker, "response", "data") %>%
  select(year = period, eia_description = 'series-description', value) %>%
  # Make fuel consumption value numeric
  mutate(value = as.numeric(value))

# Apply labels to variables
vessel_bunker_dist_fuel <- apply_variable_labels(vessel_bunker_dist_fuel)

# Read EIA Ethanol (Transportation) Data----------------------------------

eia_api_ethanol <- paste0(
  "https://api.eia.gov/v2/total-energy/data/?frequency", 
  "=annual&data[0]=value&start=1990&end=2022&sort[0][column]", 
  "=period&sort[0][direction]", 
  "https://api.eia.gov/v2/total-energy/data/?frequency", 
  "=annual&data[0]=value&facets[msn][]=EMACBUS&start=1990&end=2023&sort[0]",
  "[column]=period&sort[0][direction]=desc&offset=0&length=5000&api_key=", key) %>%
  GET() %>% # retrieve page from url
  content("raw") %>% # extract content as a raw vector
  rawToChar() %>% # convert to character data
  fromJSON() # convert from JSON to R object

ethanol_tra <- pluck(eia_api_ethanol, "response", "data") %>%
  mutate(msn = str_sub(msn, 1, 5), value = as.numeric(value)) %>%
  select(-unit, eia_description = seriesDescription, year = period, ethanol = value) 

# Apply labels to variables
ethanol_tra <- apply_variable_labels(ethanol_tra)

# Cleanup-----------------------------------------------------------------

national_ffc_data <- lst(us_consumption, vessel_bunker_dist_fuel,
                         heat_content, ethanol_tra)

rm(list = c("us_consumption", "vessel_bunker_dist_fuel", 
            "ethanol_tra", "eia_api_ethanol", 
            "eia_api_vessel_bunker", "heat_content", "eia_api_heat", 
            "key", "eia_national", "eia_api_consumption"))
