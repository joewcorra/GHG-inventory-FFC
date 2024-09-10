# Create Dataframes of MSNs and descriptions and state codes

print("Creating data frames of sectors, sources, and states.")

# Create Filtering Dataframe---------------------------------------------

# EIA provides descriptors for MSNs, but not sources and sectors.
# Here, we create dataframes of descriptors for sources & sectors of interest.

# Create 'sources' data frame
sources <- data.frame(
  source_code = c("AR", "AB", "AV", "B1", "BD", "BF", "BO", "BQ", "BT", 
                  "BX", "BY", "CC",  "CL", "CO", "DF", "DK", "EL", "EM", "ES", 
                  "EQ", "EY", "FN", "FO", "FS", "HL",  "HP", "IQ", "IY", 
                  "JF", "KS", "LU", "MB", "MG", "MS", "NG", "NN", "NU", "OH", 
                  "OJ", "OP", "P1", "P5", "PA", "PC", "PE", "PP", "PQ", 
                  "PY", "RF", "SF", "SG",  "SN", "SU", "TE", "TN", "UO",  
                  "WD", "WW", "WX"), 
  source_description = c("asphalt and road oil", 
                         "aviation gasoline blending components",
                         "aviation gasoline", "renewable diesel",
                         "biodiesel", "biofuels", "other biofuels", 
                         "normal butane", "battery storage", 
                         "total biofuels (excluding fuel ethanol)", 
                         "butylene", "coal coke", "coal", 
                         "crude oil", "distillate fuel oil", 
                         "distillate fuel oil", "electricity", 
                         "fuel ethanol, excluding denaturant", 
                         "electricity sales", "ethane", "ethylene",  
                         "petrochemical feedstocks, naphtha less than 401 degrees F", 
                         "petrochemical feedstocks, other oils equal to or greater than 401 degrees F", 
                         "petrochemical feedstocks, still gas", 
                         "hydrocarbon gas liquids", 
                         "hydroelectric pumped storage", 
                         "isobutane", "isobutylene", 
                         "jet fuel", "kerosene", "lubricants", 
                         "motor gasoline blending components", "motor gasoline", 
                         "miscellaneous petroleum products", 
                         "natural gas, including supplemental gaseous fuels", 
                         "natural gas, excluding supplemental gaseous fuels",
                         "nuclear electric power", 
                         "other hydrocarbon gas liquids", 
                         "other gases", "other petroleum products", 
                         "asphalt and road oil, aviation gasoline, kerosene, lubricants, petroleum coke, and other petroleum products", 
                         "other intermediate products (petroleum only)", 
                         "all petroleum products", "petroleum coke", 
                         "primary energy", "pentanes plus", 
                         "propane", "propylene", "residual fuel oil", 
                         "supplemental gaseous fuels", 
                         "still gas", "special naphtha", 
                         "product supplied", "total energy", 
                         "end-use energy consumption", "unfinished oils", 
                         "wood", "wood and waste", "waxes"))

# Create 'sectors' data frame
sectors <- data.frame( 
  sector_code = c("AC", "CC", "EG", "EI", "ET", "IC", "RC", "TC", "TX", "TP", 
                  "EX", "GB", "IM", "KC", "NI", "OC", "SU", "AS", "CS", 
                  "IS", "RS", "SC", "SS", "RF", "PR", "FD", "PF", "IN", 
                  "LC", "LP", "PZ", "CP", "IP", "AP", "RP", "MP", "AB"), 
  sector_description = c("transportation sector", "commercial sector", 
                         "electric power sector (generation)", 
                         "electric power sector (consumption)", 
                         "total cost of electricity generation (nuclear only)", 
                         "industrial sector", "residential sector", 
                         "total consumption of all energy-consuming sectors", 
                         "total end-use consumption", 
                         "per capita expenditures", "exports", 
                         "generating units net summer capacity total (all sectors)", 
                         "imports", "coke plants (coal only)",
                         "net imports", 
                         "industrial consumption, excluding coke plants", 
                         "product supplied", 
                         "transportation sector adjusted consumption", 
                         "commercial sector adjusted consumption", 
                         "industrial sector adjusted consumption", 
                         "residential sector adjusted consumption", 
                         "total adjusted consumption, all sectors", 
                         "total adjusted consumption, all end-use sectors", 
                         "refinery fuel", "primary energy production", 
                         "feedstocks", "process fuel", 
                         "industrial sector (supplemental gaseous fuels)", 
                         "losses and co-products (biofuels)", 
                         "lease and plant fuel", 
                         "pipeline and distribution use",
                         "commercial consumption per capita", 
                         "industrial consumption per capita", 
                         "transportation consumption per capita", 
                         "residential consumption per capita", 
                         "marketed production", 
                         "aviation gasoline blending components consumed by the industrial sector"))

# Scrape MSN Data--------------------------------------------------------

# Scrape MSN from EIA (includes MSN, MSN descriptor, and units)

# URL for the Excel file of MSN data from EIA
page_url <- "https://www.eia.gov/state/seds/CDF/Codes_and_Descriptions.xlsx"

# Download the Excel file to a temporary location
temp_file <- tempfile(fileext = ".xlsx")
GET(page_url, write_disk(temp_file, overwrite = TRUE))

# Read data from sheet 2, skipping the first 10 empty rows
msn_data <- read_excel(temp_file, sheet = 2, skip = 10) %>%
  rename(msn = MSN, msn_description = Description, unit = Unit) %>%
  mutate(msn_description = str_to_lower(msn_description), 
         unit = str_to_lower(unit), 
         # Create source code and sector code 
         source_code = str_sub(msn, 1, 2),
         sector_code = str_sub(msn, 3, 4)) %>%
  # Remove unneeded rows
  filter(unit == "billion btu")

# Clean up the temporary file
unlink(temp_file)

# Read in MSN data file and join with 'sources' and 'sectors'
msn <- msn_data %>%
  left_join(sources, by = "source_code") %>%
  left_join(sectors, by = "sector_code") %>%
  # For national calcs: Add nat gas MSNs not included in SEDS
  add_row(msn = "NNCCB", sector_description = "commercial sector", 
          source_description = "natural gas consumed by the commercial sector (excluding supplemental gaseous fuels)") %>%
  add_row(msn = "NNEIB", sector_description = "electric power sector (generation)", 
          source_description = "natural gas consumed by the electric power sector (excluding supplemental gaseous fuels)") %>%
  add_row(msn = "NNICB", sector_description = "industrial sector", 
          source_description = "natural gas consumed by the industrial sector (excluding supplemental gaseous fuels)") %>%
  add_row(msn = "NNRCB", sector_description = "residential sector", 
          source_description = "natural gas consumed by the residential sector (excluding supplemental gaseous fuels)") 


# State and Territory Names---------------------------------------------

# Create dataframes of states and territories (names & 2-letter codes)

# non_states <- c("X3", "X5", "US")
# non_state_names <- c("Federal Offshore, Gulf of Mexico", 
# "Federal Offshore, Pacific", "United States")

# US state codes (including DC) and names
state_name_key <- tibble(states_and_dc = c("AK", "AL", "AR", "AZ", "CA", "CO", 
                                           "CT", "DC", "DE", "FL", "GA", "HI", 
                                           "IA", "ID", "IL", "IN", "KS", "KY", 
                                           "LA", "MA", "MD", "ME", "MI", "MN", 
                                           "MO", "MS", "MT", "NC", "ND", "NE", 
                                           "NH", "NJ", "NM", "NV", "NY", "OH", 
                                           "OK", "OR", "PA", "RI", "SC", "SD", 
                                           "TN", "TX", "UT", "VA", "VT", "WA", 
                                           "WI", "WV", "WY"), 
                         state_names = c("Alaska", "Alabama", "Arkansas", 
                                          "Arizona", "California",
                                          "Colorado", "Connecticut", "Delaware", 
                                          "District of Columbia",
                                          "Florida", "Georgia", "Hawaii", 
                                          "Iowa", "Idaho", "Illinois",
                                          "Indiana", "Kansas", "Kentucky", 
                                          "Louisiana", "Massachusetts",
                                          "Maryland", "Maine", "Michigan", 
                                          "Minnesota", "Missouri",
                                          "Mississippi", "Montana", 
                                          "North Carolina", "North Dakota",
                                          "Nebraska", "New Hampshire", 
                                          "New Jersey", "New Mexico",
                                          "Nevada", "New York", "Ohio", 
                                          "Oklahoma", "Oregon",
                                          "Pennsylvania", "Rhode Island", 
                                          "South Carolina",
                                          "South Dakota", "Tennessee", "Texas", 
                                          "Utah", "Virginia", "Vermont", 
                                          "Washington", "Wisconsin", 
                                          "West Virginia", "Wyoming"))

# US territory names and codes
territory_name_key <- tibble(territories = c("ASM", "GUM", "PRI", 
                                             "USIQ", "VIR", "WAK"), 
                             territories_names = c("American Samoa", "Guam", 
                                                   "Puerto Rico", 
                                                   "US Pacific islands", 
                                                   "US Virgin Islands", 
                                                   "Wake Island"))

# MSN Lookup for State and National Emissions-------------------------------


# # Vector of MSNs to look up in the state summaries:
msn_lookup <- c("ABICB", "ARICB", "AVACB", "BDACB", "BDTCB", "BQICB", "BYICB", 
                "CCNIB", "CLICB", "CLKCB", "CLOCB", "CLRCB", "CLACB", 
                "CLCCB", "CLEIB", "COICB", "DFACB", "DFCCB", "DFEIB",
                "DFICB", "DKEIB", "DFRCB", "EMACB", "EMCCB", "EMICB", "EMTCB", 
                "EQICB", "EYICB", "FNICB", "FOICB", "HLACB", "HLCCB", "HLICB",
                "HLRCB", "IQICB", "IYICB", "JFACB", "KSICB", "KSCCB", "KSRCB",
                "LUACB", "LUICB", "MBICB", "MGACB", "MGCCB", 
                "MGICB", "MSICB", "NGACB", "NGCCB", "NGEIB", "NGRCB", "NGICB", 
                "NNCCB", "NNEIB", "NNICB", "NNRCB", "PCCCB", "PCEIB",
                "PCICB", "PQACB", "PQCCB", "PQICB", "PPICB", "PQRCB", "PYICB", 
                "RFACB", "RFCCB", "RFEIB", "RFICB", "SFEIB", "SFCCB", "SFRCB", 
                "SGICB", "SFINB", "SNICB", "UOICB", "WXICB")

# Cleanup-------------------------------------------------------------------


msn_names <- lst(msn, msn_lookup, state_name_key, territory_name_key)

# Remove unneeded objects from global environment
rm(list = "sources", "sectors", "msn", "msn_data", "msn_lookup", "page_url", 
   "temp_file",  "state_name_key", "territory_name_key")


