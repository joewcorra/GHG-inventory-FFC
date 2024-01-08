# Calculate State-Level CO2 Emissions
# Step 1: Total Fuel Consumption by Fuel Type and Sector
# State-Level Source: Based on EIA SEDS (adjusted to match national totals 
# as applicable)

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# adjustments: tibble; adjustment factors derived from national data (i think?)
# msn_lookup: vector; all MSNs used in the state summaries
# seds: tibble; all files in the 'data' subfolder
# seds_by_state: 


# EIA SEDS---------------------------------------------------------------

# EIA’s State Energy Data System (SEDS)
# Those data are broken out by fuel type and sector (residential, commercial,
# industrial, transportation, and electric power) and are available for the 
# years 1960–2021

# Read in SEDS data
seds <- list.files(path = "data", pattern = "\\.csv$", 
                   full.names = TRUE) %>%
  # Read all .csv files in the /data folder
  map(\(.x) read.csv(.x) %>% 
        clean_names()) %>%
  # Collapse all list elements into one data frame
  list_cbind() %>%
  # Change 'year' to a column
  pivot_longer(cols = starts_with("x") , names_to = "year") %>%
  # Remove unneeded columns
  select(-data_status) %>%
  # Get rid of leading 'x' in years
  mutate(year = str_remove(year, "x")) %>%
  # Retain data from 1990 onward
  filter(year > "1989")

# Might not keep this (?)
subsources <- c("residential", "commercial", "industrial", "transportation",
                "electricity generation", "us territories")

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


# Aggregate by State----------------------------------------------------

# For each state and year, create a data frame with the following columns:
# state, year, and values for all codes in 'msn_lookup'. Divide all 
# resulting values by 1000. 

seds_by_state <- seds %>%
  # Retain rows with matching MSNs
  filter(msn %in% msn_lookup) %>%
  left_join(msn, by = "msn") %>%
  # Convert to millions of BTUs, round to 2 places
  mutate(value = round(value / 1000, 2)) %>%
  # Split into list elements by state
  group_by(state) %>%
  group_split() 


# Adjust to Match National Totals----------------------------------------

# If SEDS data totals matched the national totals and there were no further
# adjustments needed (as per Steps 2–7), use the SEDS data

# Recursive: Requires data from Steps 2 and 5

# For fuels where the SEDS totals did not match the national totals (i.e., 
# coal, natural gas, and petroleum coke), adjust fuel use in each sector 
# to match the national totals. This calculation is based on the percentage of
# each fuel used in each state from the SEDS data. For the industrial
# sector, this adjustment was made after subtracting for uses in the
# IPPU sector (see Step 2 below).

# For other fuels where sector totals did not match up (e.g., gasoline 
# and diesel fuel), totals for each fuel type were generally taken from the 
# national Inventory (see Step 5), and the SEDS data or other proxy data 
# sources were used to determine state-level percentages of each fuel use.

# SEDS data -> adjust fuel use -> adjust industrial fuel use after Step 2 ->
# adjust other fuels after Step 5

# Read in adjustment factors data (derived from national inventory?)

adjustments <- read_csv("us_compare.csv") %>%
  clean_names() %>%
  # Change 'year' to a column
  pivot_longer(cols = starts_with("x"), 
               names_to = "year", values_to = "adjustment_factor") %>%
  # Get rid of leading 'x' in years
  mutate(year = str_remove(year, "x")) 

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
seds_not_in_adj <- setdiff(seds_by_state$source_description, adjustments$source_description)
adj_not_in_sed <- setdiff(adjustments$source_description, seds_by_state$source_description)
