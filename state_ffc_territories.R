# Calculate Emissions for US Territories

# The procedure for retrieving and collating the FFC data for US territories
# differs from the procedure for states. 


# API Key----------------------------------------------------------------

# API key generated 11/22/23 
key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"


# Retrieve Territories FF Consumption from EIA---------------------------


# American Samoa, Guam, Puerto Rico, US Virgin Islands, 
# US Pacific islands, Wake Island

api_territories <- paste0(
  "https://api.eia.gov/v2/international/data/?frequency=",
  "annual&data[0]=value", 
  "&facets[countryRegionId][]=ASM&facets[countryRegionId][]=",
  "GUM&facets[countryRegionId][]=PRI&facets[countryRegionId]",
  "[]=USIQ&facets[countryRegionId][]=VIR&facets",
  "[countryRegionId][]=WAK&facets[activityId][]=2&facets",
  "[unit][]=BCF&facets[unit][]=TBPD&facets[unit][]=TST&facets",
  "[productId][]=26&facets[productId][]=5&facets[productId]",
  "[]=54&facets[productId][]=62&facets[productId]",
  "[]=63&facets[productId][]=64&facets[productId]",
  "[]=65&facets[productId][]=66&facets[productId]",
  "[]=67&facets[productId][]=68&facets[productId]",
  "[]=7&start=1989&end=2022&sort[0][column]=",
  "period&sort[0][direction]=desc&offset=0&length=5000",
  "&api_key=", key) %>% # our API key is required
  GET() %>% # retrieve page from url
  content("raw") %>% # extract content as a raw vector
  rawToChar() %>% # convert to character data
  fromJSON() # convert from JSON to R object


ff_territories <- api_territories %>%
  map(\(.x) pluck(.x, "data")) %>%
  list_rbind() %>%
  select(state = countryRegionId, year = period, state_name = countryRegionName, 
         value, unit = unitName, source = productId, dataFlagDescription, 
         source_description = productName) %>%
  mutate(value = as.numeric(value), 
         source_description = str_to_lower(source_description), 
         source_description = case_when(
           source_description == "liquified petroleum gases" ~ "lpg",
           source_description == " " ~ "lubricants",
           source_description == " " ~ "other petroleum liquids" ))



# # Retrieve FF Heat Content from EIA----------------------------------

# Annually variable: have nat gas, mogas; 
# Constant: resid fuel, LPG, kerosene, jet fuel, petrol liquids, coal, 
  # lubricants, dist fuel (use high sulfur)

# Get dist fuel, mogas, and natural gas from EIA API
eia_api_heat <- paste0(
  "https://api.eia.gov/v2/total-energy/data/?frequency=annual&data[0]", 
  "=value&facets[msn][]=MGTCKUS&facets[msn][]=",
  "NGTCKUS&start=1990&end=", 
  # &facets[msn][]=DMTCKUS dist fuel currently excluded
  latest_year, 
  "&sort[0][column]=msn&sort[0][direction]=asc&offset=0&length=5000&api_key=",
  key) %>%
  GET() %>% # retrieve page from url
  content("raw") %>% # extract content as a raw vector
  rawToChar() %>% # convert to character data
  fromJSON() %>% # convert from JSON to R object
  pluck("response", "data") %>%
  select(year = period, # msn, 
         msn_description = seriesDescription, heat_content = value) %>%
  # Make heat content value numeric
  mutate(heat_content = as.numeric(heat_content), 
         source_description = case_when(
           str_detect(msn_description, "Motor") ~ "motor gasoline",
           str_detect(msn_description, "Natural Gas") ~ "natural gas")) %>%
  filter(!is.na(source_description)) %>%
  select(-msn_description)

# Get resid fuel, kerosene, jet fuel, lubricants, dist fuel (use high sulfur),
# other petrol liquids, and LPG (average of 9 HGLs)
heat_commodities <- c("Residual Fuel Oil", "Kerosene", 
                      "Jet Fuel, Kerosene Type", "Lubricants", 
                      "than 500 ppm sulfur", "Other Hydrocarbons",
                      "Ethane", "Propane", "Normal Butane", "Isobutane",
                      "Ethylene", "Propylene", "Butylene", 
                      "Isobutylene", "Pentanes Plus")

eia_table_a1 <- pdf_text(
  "https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_2.pdf") %>%
  str_remove_all("[()]")

heat_content_territories <- heat_commodities %>% 
  map(\(.x) str_match_all(
    eia_table_a1, pattern = paste0(.x, "\\s*(\\d+(?:\\.\\d+)?)")) %>% 
      pluck(1,2) %>% 
      tibble(source_description = .x, heat_content = .)) %>%
  list_rbind() %>%
  mutate(heat_content = as.numeric(heat_content), 
         source_description = str_replace_all(
    source_description,  "than 500 ppm sulfur", "Distillate Fuel Oil"), 
    source_description = str_remove_all(
      source_description,  ", Kerosene Type"),
    source_description = if_else(source_description %in% c("Ethane", "Propane", 
                                            "Normal Butane", "Isobutane",
                                            "Ethylene", "Propylene", 
                                            "Butylene", "Isobutylene", 
                                            "Pentanes Plus"), 
                  "lpg", source_description)) %>%
  group_by(source_description) %>%
  # Calculate mean heat content by source (affects LPGs only)
  summarize(heat_content = mean(heat_content)) %>%
  ungroup() %>%
  # Make sources lower case
  mutate(source_description = str_to_lower(source_description)) %>%
  # TEMPORARY: Add a row for coal until we know where it comes from
  add_row(source_description = "coal", heat_content = 22.3) %>%
  # Copy these constant values across all years
  expand_grid(year = 1990:2022) %>%
  # Make the year character to match EIA data
  mutate(year = as.character(year)) %>%
  # Merge with EIA heat content data (mogas and nat gas)
  bind_rows(eia_api_heat) %>%
  # Standardize source descriptions to match EIA territories data
  mutate(source_description = case_when(
    source_description == "natural gas" ~ "dry natural gas", 
    source_description == "other hydrocarbons" ~ "other petroleum liquids"))


# Calculcate TBtu---------------------------------------------------------

ffc_territories <- ff_territories %>% 
  left_join(heat_content_territories, 
            by = c("year", "source_description")) %>%
  # TBtu = consumption * days * 1000 * heat content / 1,000,000 
  mutate(tbtu = value * 365 * heat_content / 1000)



# Calculate Carbon Eq Emissions------------------------------------------

# need to figure out which carbon factors to use

carbon_territories <- ffc_territories %>% 
  left_join(carbon_factors, 
            by =c("source_description", "year")) %>%
  # MMT CO2  = btu * carbon factor/1000 * 44/12
  mutate(mmt_co2 = tbtu / 1000 * carbon_factor * carbon_ratio)

