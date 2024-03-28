# Calculate State-Level CO2 Emissions
# Step 1: Total Fuel Consumption by Fuel Type and Sector


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# adjustments: tibble; adjustment factors derived from national data (i think?)
# msn_lookup: vector; all MSNs used in the state summaries
# seds: tibble; all SEDS data


# EIA SEDS---------------------------------------------------------------

# EIA’s State Energy Data System (SEDS)
# Those data are broken out by fuel type and sector (residential, commercial,
# industrial, transportation, and electric power) and are available for the 
# years 1960–2021



# Vector of MSNs to look up in the state summaries:
# These MSNs are all in billions of BTUs; converted to millions below. 
msn_lookup <- c("CLRCB", "NGRCB", "SFRCB", "DFRCB", "KSRCB", "HLRCB", "PQRCB", 
          "CLCCB", "NGCCB", "SFCCB", "DFCCB", "KSCCB", "PQCCB", "HLCCB", 
          "MGCCB", "EMCCB", "RFCCB", "PCCCB", "CLKCB", "CLOCB", "CCNIB", 
          "NGICB", "SFINB", "ARICB", "DFICB", "KSICB", "HLICB", "PQICB", 
          "PYICB", "EQICB", "EYICB", "BQICB", "BYICB", "IQICB", "IYICB", 
          "LUICB", "MGICB", "EMICB", "RFICB", "ABICB", "COICB", "MBICB", 
          "MSICB", "FNICB", "FOICB", "PPICB", "PCICB", "SGICB", "SNICB", 
          "UOICB", "WXICB", "CLACB", "NGACB", "AVACB", "DFACB", "BDACB", 
          "JFACB", "HLACB", "PQACB", "LUACB", "MGACB", "EMACB", "RFACB", 
          "CLEIB", "NGEIB", "SFEIB", "DFEIB", "RFEIB", "PCEIB", "EMTCB", 
          "BDTCB")


# Read old SEDS data-------------------------------------------------------

# Read in SEDS data from use_all_btu_csv
seds_original <- read_csv("data/use_all_btu.csv") %>%
        clean_names() %>%
  # Change 'year' to a column
  pivot_longer(cols = starts_with("x") , names_to = "year") %>%
  # Remove unneeded columns
  select(-data_status) %>%
  # Get rid of leading 'x' in years
  mutate(year = str_remove(year, "x")) %>%
  # Retain data from 1990 onward
  filter(year > "1989") %>%   
  # Retain rows with MSN matching our msn_lookup data
  filter(msn %in% msn_lookup, 
         # Retain rows with states or DC
         state %in% states_and_dc) %>%
  left_join(msn, by = "msn") %>%
  # Convert to millions of BTUs, round to 2 places
  mutate(value = round(value / 1000, 2)) 

# Read API-derived SEDS data---------------------------------------------

# Alternatively, read SEDS data from EIA API file pulled with epa_api.R
seds <- read_csv("data/api_seds.csv") %>%
  clean_names() %>%
  select( state = state_id, year = period, msn = series_id, value, unit) %>%
  filter(unit == "Billion Btu") %>%
  filter(msn %in% msn_lookup) %>%
  mutate(unit = str_to_lower(unit), 
         year = as.character(year)) %>%
  left_join(msn %>% select(-unit), by = "msn")




# Read National Total Adjustment Data---------------------------------------

# Read in adjustment factors data, derived from national inventory
adjustments <- read_csv("us_compare.csv") %>%
  clean_names() %>%
  # Change 'year' to a column
  pivot_longer(cols = starts_with("x"), 
               names_to = "year", values_to = "national_value") %>%
  # Get rid of leading 'x' in years
  mutate(year = str_remove(year, "x"), 
         # Standardize sector descriptions   
         sector_description = str_c(sector_description, " sector"), 
         # Standardize source descriptions
         source_description = if_else(
           source_description == "hydrocarbon gas liquids", 
           "hgl", source_description)) 

# JOINING PROBLEMS: 'Adjustments' lumps all LPGs ("butylene", "propane", 
# "propylene", "isobutane", "normal butane") together as 'lpg' in the 
# source_description, while SEDS is specific. 
  # Solution 1: create a new column in both tibbles 
  # Solution 2: in the csv, create additional identical rows for all of the 
  # sources.
# Other adjustments source_description without corresponding match in 
# SEDS: 'other coal', 'other hydrocarbon gas liquids'
  # 'other hydrocarbon gas liquids' is correct, but no matches in SEDS
  # 'other coal' has no MSN match or SEDS match 
# other SEDS source_description without corresponding match in 
# adjustments: 'biodiesel', 'ethane', 'isobutylene', 'supplemental fuels', 
# 'ethylene', 'fuel ethanol, exluding denaturant'
  # Biodiesel is addressed in a later step
  # LPGs: butylene, propane, propylene, isobutane, normal butane
  # 3 ethanol fuels = ???
  # Isobutylene = ???
  # Supplemental gaseous fuels = ???


# Cleanup----------------------------------------------------------------

# Remove unneeded data objects
rm(msn_lookup)

