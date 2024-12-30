# Functions

# Data setup 
data_setup <- function() {
  # Create Dataframes of MSNs and descriptions and state codes
  
  # Create function to add labels (with 'labelled')
  
  # Function: add variable labels to dataframes and convert 'year' to factor
  apply_variable_labels <- function(data, ghgi_variables) {
    
    # If 'year' column exists, convert to factor
    if ("year" %in% colnames(data)) { 
      data <- data %>% 
        mutate(year = as_factor(year))
    }
    
    # Filter the variables dataframe and apply metadata as variable labels
    set_variable_labels(data, 
                        .labels = deframe(ghgi_variables %>% 
                                            filter(variable %in% 
                                                     colnames(data)) %>% 
                                            select(-data_type)) %>% 
                          as.list())
    
  }
  
  
  # Read GHGI harmonization data---------------------------------------------
  
  ghgi_values <- read_excel("data_harmonization.xlsx", 
                             sheet = "values") %>%
    map(\(.x) na.omit(.x) %>% 
          as.vector())
  
  
  ghgi_variables <- read_excel("data_harmonization.xlsx", 
                                sheet = "variables") 
  
  
  ghgi_invdb_values <- read_excel("data_harmonization.xlsx", 
                                   sheet = "invdb") 
  
  # Add variable labels
  ghgi_invdb_values <- apply_variable_labels(ghgi_invdb_values, ghgi_variables)
  
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
    rename(msn = MSN, eia_description = Description, unit = Unit) %>%
    mutate(eia_description = str_to_lower(eia_description), 
           unit = str_to_lower(unit), 
           # Create source code and sector code 
           source_code = str_sub(msn, 1, 2),
           sector_code = str_sub(msn, 3, 4)) 
  
  # Clean up the temporary file
  unlink(temp_file)
  
  # Read in MSN data file and join with 'sources' and 'sectors'
  msn <- msn_data %>%
    filter(unit == "billion btu") %>%
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
  
  # Add variable labels
  msn <- apply_variable_labels(msn, ghgi_variables)
  
  # State and Territory Names---------------------------------------------
  
  # Create dataframes of states and territories (names & 2-letter codes)
  
  state_name_key <- tibble(state = ghgi_values$state, 
                           state_name = ghgi_values$state_name) %>%
    # Identify territories
    mutate(territory = if_else(str_length(state) > 2, TRUE, FALSE))
  
  
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
  
  
  msn_names <- lst(msn, msn_lookup, state_name_key)
  
  universal_data <- lst(ghgi_values, 
                        ghgi_variables, 
                        ghgi_invdb_values,
                        msn_names, 
                        apply_variable_labels)
  
  return(universal_data)
  
}


# Read SEDS data from EIA's API

state_ffc_get_seds_data <- function(msn_names, 
                                    apply_variable_labels, 
                                    ghgi_variables) {
  # Alternatively, read SEDS data from EIA API file pulled with epa_api.R
  seds <- read_csv("data/api_seds.csv") %>%
    clean_names() %>%
    select( state = state_id, year = period, msn = series_id, 
            value, unit) %>%
    filter(unit == "Billion Btu") %>%
    filter(msn %in% msn_names$msn_lookup) %>%
    mutate(unit = str_to_lower(unit), 
           year = as.character(year)) %>%
    left_join(msn_names$msn %>% select(-unit), by = "msn") %>%
    # Remove any duplicates caused by appending new annual data
    distinct() %>%
    # Convert to millions of BTUs
    mutate(value = value / 1000) 
  
  # # Access SEDS data via EIA API
  # 
  # # This script contains two options for retrieving SEDS data from the EIA API:
  # # 1) retrieve entire dataset for all states + DC, 1990-present, inclusive; 
  # # 2) retrieve most recent year of data and append to the existing SEDS csv.
  # 
  # # API key generated 11/22/23 
  # key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"
  # 
  # # API key check
  # eia_api_url <- "https://api.eia.gov/v2/seds/data"
  # response <- GET(eia_api_url, query = list(api_key = key))
  # # If status = 200, then it's working
  # print(response)
  # 
  # # Function for Options 1 & 2---------------------------------------------
  # 
  # # Function to Query EIA API
  # get_results <- function(state, year, offset) {
  #   
  #   # offset_by is the offset (i.e., row to start with) for pagination
  #   # For now, to avoid exceeding the 5000-row data limit, we will pull
  #   # only one year and state per query. This requires n=51*years API queries. 
  #   results <- paste0("https://api.eia.gov/v2/seds/data/?frequency=annual",
  #                     "&data[0]=value", 
  #                     "&facets[stateId][]=", state, # state input
  #                     "&start=", year - 1, # start = previous year for some reason
  #                     "&end=", year,
  #                     "&sort[0][column]=period&sort[0][direction]=desc&offset=",
  #                     offset, "&length=5000", # offset (usually 0)
  #                     "&api_key=", key) %>% # our API key is required
  #     GET() %>% # retrieve page from url
  #     content("raw") %>% # extract content as a raw vector
  #     rawToChar() %>% # convert to character data
  #     fromJSON() # convert from JSON to R object
  #   
  #   # The data limit from EIA's API is 5000 rows per query. 
  #   # Here, we check the results to see if we exceeded that. 
  #   # Extract warnings (if they exist)
  #   limits <- pluck(results, "response", "warnings", "warning")
  #   limits <- ifelse(is_empty(limits), "nothing", limits)
  #   print(limits)
  #   
  #   # Check if data limit (5000 rows) was reached, ignoring empty values
  #   limit_reached <<- case_when(
  #     limits == "nothing" ~ FALSE,
  #     str_detect(limits, "incomplete return") ~ TRUE,
  #     .default = FALSE)
  #   print(limit_reached)
  #   
  #   return(results)
  #   
  # }
  # 
  # 
  # 
  # # Option 1: Retrieve All SEDS Data-------------------------------------------
  # 
  # # Apply API data query function across all states and years.
  # # Using tic and toc() will indicate the time elapsed. Expected: about 18 min.
  # tic()
  # api_results <- expand_grid(state = states_and_dc, 
  #                            year = 1990:2022, 
  #                            offset = 0) %>%
  #   pmap(function(state, year, offset) get_results(state, year, offset))
  # toc()
  # 
  # api_seds <- api_results %>%
  #   map(\(.x) pluck(.x, "response", "data")) %>%
  #   list_rbind()
  # 
  # # Write data to csv file
  # write_csv(api_seds, "data/api_seds.csv")
  # 
  # # The script read_seds_data.R performs further transformation of this data.
  # 
  # # Option 2: Retrieve New Year of Data Only---------------------------------
  # 
  # api_results_new <- expand_grid(state = states_and_dc, 
  #                                year = 2022, 
  #                                offset = 0) %>%
  #   pmap(function(state, year, offset) get_results(state, year, offset))
  # 
  # api_seds_new <- api_results_new %>%
  #   map(\(.x) pluck(.x, "response", "data")) %>%
  #   list_rbind() %>% mutate(period = as.numeric(period), 
  #                           value = as.numeric(value))
  # 
  # # Load existing data
  # api_seds <- read_csv("data/api_seds.csv") 
  # 
  # # Check if new data is different from existing data
  # new_stuff <- api_seds_new %>% 
  #   filter(seriesId %in% msn_lookup) %>%
  #   # Remove any superfluous spaces from the character data
  #   mutate(across(where(is.character), ~ str_trim(.))) %>%
  #   # Filter: only retain new data that's not in the existing dataset
  #   anti_join(api_seds %>% 
  #               filter (period == "2022",
  #                       seriesId %in% msn_lookup))
  # 
  # # View the results
  # new_stuff
  # 
  # # If it looks OK, append new data to existing dataset
  # api_seds <- read_csv("data/api_seds.csv") %>%
  #   rbind(api_seds_new)
  # 
  # # Save the updated file 
  # write_csv(api_seds, "data/api_seds.csv")
  
  return(seds)
  
}

# Datascraping
scrape_data <- function(msn_names) {
  
  # Set year to match most recent available year (current year minus two)
  latest_year <- year(Sys.Date()) -2
  
  # Scrape FWHA Fuel Use (State & National FFC) ----------------------------
  
  # Temporary file storage path
  local_excel_path <- tempfile(fileext = ".xlsx")
  # URL for gasoline data by state, 1949 to present year
  gasoline_url <- paste0(
    "https://www.fhwa.dot.gov/policyinformation/statistics/", 
    latest_year, "/xls/mf226.xlsx")
  # URL for special fuel (diesel) data by state, 1949 to present year
  special_fuel_url <- paste0(
    "https://www.fhwa.dot.gov/policyinformation/statistics/", 
    latest_year, "/xls/mf225.xlsx")
  
  # Retrieve gasoline Excel file data
  GET(gasoline_url, write_disk(local_excel_path, overwrite = TRUE))
  
  # Read from temp file 
  gasoline_distribution <- read_excel(local_excel_path) %>%
    clean_names() %>%
    # Remove unneeded rows
    filter(!is.na(state), 
           state != "Total") %>%
    # Make all value columns numeric
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", 
                 values_to = "gasoline_percent") %>%
    # Remove letters from year column 
    mutate(year = str_remove(year, "[a-z]"),
           # Get national total for each year by insta-grouping
           national_total = sum(gasoline_percent, na.rm = TRUE), .by = year) %>%
    # Retain only 1990 onward
    filter(year > 1989) %>%
    # Get gasoline percentage for each state 
    mutate(gasoline_percent = gasoline_percent / national_total, 
           # Fix the dumb abbreviation for District of Columbia
           state = if_else(str_detect(state, "Dist"), 
                           "District of Colombia", state)) %>%
    rename(state_name = state) %>%
    # Get state codes
    left_join(msn_names$state_name_key %>% 
                filter(territory == FALSE), 
              by = "state_name") %>%
    # No longer need national total or full state name
    select(-national_total, -state_name)

  # Retrieve diesel Excel file data
  GET(special_fuel_url, write_disk(local_excel_path, overwrite = TRUE)) 
  # Read from temp file 
  
  diesel_distribution <- read_excel(local_excel_path) %>%
    clean_names() %>%
    # Remove unneeded rows
    filter(!is.na(state), 
           state != "Total") %>%
    # Make all value columns numeric
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", 
                 values_to = "diesel_percent") %>%
    # Remove letters from year column 
    mutate(year = str_remove(year, "[a-z]"),
           # Get national total for each year by insta-grouping
           national_total = sum(diesel_percent, na.rm = TRUE), .by = year) %>%
    # Retain only 1990 onward
    filter(year > 1989) %>%
    # Get gasoline percentage for each state 
    mutate(diesel_percent = diesel_percent / national_total, 
           # Fix the dumb abbreviation for District of Columbia
           state = if_else(str_detect(state, "Dist"), "District of Colombia", state)) %>%
    rename(state_name = state) %>%
    # Get state codes
    left_join(msn_names$state_name_key %>% 
                filter(territory == FALSE), 
              by = "state_name") %>%
    # No longer need national total or full state name
    select(-national_total, -state_name)
  
  # Scrape FWHA Fuel Use National FFC
  # Retrieve gasoline Excel file data
  GET(gasoline_url, write_disk(local_excel_path, overwrite = TRUE))
  # Read from temp file 
  
  gasoline_use_national <- read_excel(local_excel_path) %>%
    clean_names() %>%
    # Remove unneeded rows
    filter(state == "Total") %>%
    # Make all value columns numeric
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", 
                 values_to = "gasoline_use_gal") %>%
    # Remove letters from year column 
    mutate(year = str_remove(year, "[a-z]")) %>%
    # Retain only 1990 onward
    filter(year > 1989) %>%
    select(-state)    
  
  # URL for table VM-1, diesel fuel by class
  
  # Temporary file storage path
  local_excel_path <- tempfile(fileext = ".xlsx")
  
  diesel_url <- paste0(
    "https://www.fhwa.dot.gov/policyinformation/statistics/", 
    latest_year, "/xls/vm1.xlsx") 
  # https://www.fhwa.dot.gov/policyinformation/statistics/1998/vm1.cfm
  GET(diesel_url, write_disk(local_excel_path, overwrite = TRUE))
  
  diesel_use_by_class <- read_excel(local_excel_path) %>%
    clean_names() %>%
    # Make all value columns numeric
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", 
                 values_to = "gasoline_use_gal") %>%
    # Retain only year and value
    select(year, gasoline_use_gal) %>%
    # Remove letters from year column 
    mutate(year = str_remove(year, "[a-z]"))
  # Retain only 1990 onward
  
  # # Scrape EPA Flight solid waste combustion data
  # 
  # # Temporary file storage path
  # local_excel_path <- tempfile(fileext = ".xls")
  # 
  # # URL for SWC data by year and facility
  # swc_url <- paste0(
  #   "https://ghgdata.epa.gov/ghgp/service/export?q=&tr=current&ds=E&ryr=2023&cyr=2023&lowE=-20000&highE=23000000&st=&fc=&mc=&rs=ALL&sc=0&is=11&et=&tl=&pn=undefined&ol=0&sl=0&bs=&g1=1&g2=1&g3=1&g4=1&g5=1&g6=0&g7=1&g8=1&g9=1&g10=1&g11=1&g12=1&s1=0&s2=1&s3=0&s4=0&s5=0&s6=0&s7=0&s8=0&s9=0&s10=0&s201=0&s202=0&s203=0&s204=1&s301=0&s302=0&s303=0&s304=0&s305=0&s306=0&s307=0&s401=0&s402=0&s403=0&s404=0&s405=0&s601=0&s602=0&s701=0&s702=0&s703=0&s704=0&s705=0&s706=0&s707=0&s708=0&s709=0&s710=0&s711=0&s801=0&s802=0&s803=0&s804=0&s805=0&s806=0&s807=0&s808=0&s809=0&s810=0&s901=0&s902=0&s903=0&s904=0&s905=0&s906=0&s907=0&s908=0&s909=0&s910=0&s911=0&sf=11001100&allReportingYears=yes&listExport=false")
  # 
  # # Retrieve SWC Excel file data
  # GET(swc_url, write_disk(local_excel_path, overwrite = TRUE))
  # 
  # # Read from temp file 
  # # The top six lines are blank in this worksheet, so we'll skip them 
  # swc <- read_excel(local_excel_path, skip = 6) %>%
  #   # Clean up column names/apply snake-case style. 
  #   clean_names() 
  # 
  # # Apply variable labels 
  # swc <- apply_variable_labels(swc, ghgi_variables)
  # 
  scraped_data <- lst(diesel_distribution,
                      diesel_use_by_class, 
                      #swc, 
                      gasoline_distribution,
                      gasoline_use_national)
  
  return(scraped_data)
  
}


# Retrieve national FFC data for adjustments

state_ffc_get_corrections_data <- function() {
  
  # Adjustment factors data, derived from national inventory------------
  adjustments <- read_csv("data/us_compare.csv") %>%
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
  
  # National corrections data------------------------------------------
  national_corrections <- read_excel("data/national_inventory_CO2_data.xlsx", 
                                     sheet = "Corrections", 
                                     skip = 0, range = "B5:AI60",
                                     col_names = FALSE) %>%
    clean_names() %>%
    rename(categories = x1) %>%
    filter(!is.na(x2)) %>%
    mutate(categories = case_when(
      categories == "(TBtu)" ~ "year", 
      .default = categories %>% 
        str_to_lower() %>% 
        str_replace_all(" ", "_") %>%
        str_remove_all("\\(|\\)|\\.|>"))) %>%
    distinct(categories, .keep_all = TRUE) %>%
    mutate(across(starts_with("x"), ~as.numeric(.))) %>% 
    pivot_longer(cols = -1) %>%
    pivot_wider(names_from = categories) %>%
    mutate(year = as.character(year)) %>%
    select(year, sng_correction = dakota_gas,
           nat_gas_ammonia_factor = ammonia_production,
           ippu = coking_coal, 
           cb_factor = residual_fuel, 
           is_gas_factor = natural_gas, 
           is_distillate_fuel_factor = distillate_fuel, 
           is_coal_factor = coal)

  # Consumption input data------------------------------------------
  # Consumption input is 'US compare' data with corrections factors applied. 
  # It applies only to industrial coal, nat gas, resid fuel, & dist fuel.
  
  consumption_input <- read_excel("data/national_inventory_CO2_data.xlsx", 
                                  sheet = "Consumption Input", 
                                  skip = 0, range = "C5:AK131") %>%
    clean_names() %>%
    rename(sector_description = t_btu, 
           source_description = x2) %>%
    mutate(sector_description = if_else(
      str_detect(sector_description, "ource"), 
      NA_character_, sector_description), 
      # Standardize source descriptions for joins in industrial_adjustments.R
      source_description = case_when(
        str_detect(source_description, "istillate") ~ "distillate fuel oil", 
        str_detect(source_description, "esidual") ~ "residual fuel oil",
        .default = source_description)) %>%
    fill(sector_description) %>%
    pivot_longer(cols = !c(sector_description, source_description), 
                 values_to = "consumption_value", names_to = "year") %>%
    filter(!is.na(source_description), 
           !str_detect(year, "percent")) %>%
    mutate(year = parse_number(year) %>% as.character(), 
           # NAs are okay in the next line; we won't be using those values
           consumption_value = as.numeric(consumption_value),
           source_description = str_to_lower(source_description), 
           sector_description = str_to_lower(sector_description) %>% 
             str_c(" sector")) %>%
    # Only used for ind: resid fuel, dist fuel, nat gas, & coal. Remove others
    filter(sector_description == "industrial sector", 
           source_description %in% c("residual fuel oil", "other coal", 
                                     "distillate fuel oil", "natural gas")) %>%
    # Change other coal = coal for consistent joins in industrial_adjustments.R
    mutate(source_description = 
             if_else(source_description == "other coal", "coal", 
                     source_description))
  
  print("This generates a warning about NA values.")
  print("Ignore this warning. These values are not used.")
  
  # IBF corrections data--------------------------------------------
  # Read in IBF data from FFC excel workbook. Only need one line:
  ibf_corrections <- read_excel("data/national_inventory_CO2_data.xlsx", 
                                sheet = "International Bunker Fuels", 
                                skip = 0, range = "C7:AJ10") %>%
    clean_names() %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", values_to = "ibf_value") %>%
    # Rename source column
    rename(source_description = gas_mode_and_fuel_type) %>%
    # Remove letters from 'year' column and standardize source descriptions
    mutate(year = str_remove(year, "[a-z]"), 
           source_description = case_when(
             str_detect(source_description, "viation") ~ "jet fuel", 
             str_detect(source_description, "istillate") ~ "distillate fuel oil",
             str_detect(source_description, "esidual") ~ "residual fuel oil"))

  # NEU data---------------------------------------------------------
  # Read in NEU data from FFC excel workbook
  neu_corrections <- read_excel("data/national_inventory_CO2_data.xlsx", 
                                sheet = "Non-Energy Use", 
                                skip = 0, range = "D5:AK24") %>%
    clean_names() %>%
    # Rename to match column names in SEDS
    rename(source_description = sector_fuel_type) %>%
    # Add sector description based on source_description text
    mutate(sector_description = case_when(
      source_description == "Transportation" ~ "transportation sector",
      source_description == "Industry" ~ "industrial sector",
      .default = NA_character_)) %>%
    # Fill sector_description empty values from previous entry
    fill(sector_description, .direction = "down") %>%
    # Move sector_description to the first column in order to pivot
    relocate(sector_description) %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -c(1, 2), names_to = "year", 
                 values_to = "neu_factor") %>%
    # Remove letters from year column 
    mutate(year = str_remove(year, "[a-z]"), 
           # Make source lowercase
           source_description = str_to_lower(source_description) %>% 
             # Remove asterisks and the word "industrial" from source
             str_remove_all("\\*|industrial") %>% 
             # Remove extra spaces from source
             str_squish()) 

  # I & S distributions data--------------------------------------------
  # Read in I & S data from FFC excel workbook
  is_distribution <- read_excel(
    "data/ippu_i&s_percent.xlsx", 
    sheet = 1, 
    skip = 0, range = "A2:AK54") %>%
    clean_names() %>%
    # Don't need the national value; we compute it below
    filter(state != "National") %>%
    # Keep only state codes and value by year
    select(state, starts_with("x")) %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", 
                 values_to = "is_percent") %>%
    # Remove letters from year column 
    mutate(year = str_remove(year, "[a-z]"),
           # Get national total for each year by insta-grouping
           national_total = sum(is_percent), .by = year) %>%
    # Get I & S percentage for each state 
    mutate(is_percent = is_percent / national_total) %>%
    # No longer need national total
    select(-national_total)
  
  # Read in ammonia data from FFC excel workbook
  ammonia_distribution <- read_excel(
    "data/ippu_ammonia_percent.xlsx", 
    sheet = 1, 
    skip = 0, range = "A2:AJ54") %>%
    clean_names() %>%
    # Don't need the national value; we compute it below
    filter(state != "National") %>%
    # Keep only state codes and value by year
    select(state, starts_with("x")) %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", 
                 values_to = "ammonia_percent") %>%
    # Remove letters from year column 
    mutate(year = str_remove(year, "[a-z]"),
           # Get national total for each year by insta-grouping
           national_total = sum(ammonia_percent), .by = year) %>%
    # Get ammonia percentage for each state 
    mutate(ammonia_percent = ammonia_percent / national_total) %>%
    # No longer need national total
    select(-national_total)
  
  # Read in petrochemical carbon black data from FFC excel workbook
  petrochemicals_distribution <- read_excel(
    "data/ippu_petrochemicals_percent.xlsx", 
    sheet = 1, 
    # Choose the 'carbon black' cell range 
    skip = 0, range = "A2:AK54") %>%
    clean_names() %>%
    # Don't need the national value; we compute it below
    filter(state != "National") %>%
    # Keep only state codes and value by year
    select(state, starts_with("x")) %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", 
                 values_to = "petrochemical_percent") %>%
    # Remove letters from year column 
    mutate(year = str_remove(year, "[a-z]"),
           # Get national total for each year by insta-grouping
           national_total = sum(petrochemical_percent), .by = year) %>%
    # Get petrochemical percentage for each state 
    mutate(petrochemical_percent = 
             petrochemical_percent / national_total) %>%
    # No longer need national total
    select(-national_total)
  
  # Read in petrochemical carbon black data from FFC excel workbook
  petrochemicals_cb_distribution <- read_excel(
    "data/ippu_petrochemicals_percent.xlsx", 
    sheet = 1, 
    # Choose the 'carbon black' cell range 
    skip = 0, range = "A58:AK110") %>%
    clean_names() %>%
    # Don't need the national value; we compute it below
    filter(state != "National") %>%
    # Keep only state codes and value by year
    select(state, starts_with("x")) %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", 
                 values_to = "petrochemical_cb_percent") %>%
    # Remove letters from year column 
    mutate(year = str_remove(year, "[a-z]"),
           # Get national total for each year by insta-grouping
           national_total = sum(petrochemical_cb_percent), .by = year) %>%
    # Get petrochemical percentage for each state 
    mutate(petrochemical_cb_percent = 
             petrochemical_cb_percent / national_total) %>%
    # No longer need national total
    select(-national_total)

  # Read in diesel fuel data from FOKS excel workbook
  foks_diesel_distribution <- read_excel(
    "data/FOKS Diesel Fuel Bunker 2020.xls", 
    sheet = 3, 
    skip = 0, range = "A2:AF53") %>%
    clean_names() %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", 
                 values_to = "diesel_percent") %>%
    # Remove letters from year column 
    mutate(year = str_remove(year, "[a-z]"),
           # Get national total for each year by insta-grouping
           national_total = sum(diesel_percent), .by = year) %>%
    # Rename state column
    rename(state = x1) %>%
    # Get FOKS diesel percentage for each state 
    mutate(foks_diesel_percent = diesel_percent / national_total) %>%
    # No longer need national total
    select(-national_total)
  
  # Append Extrapolated Data for 2021 Onward 
  
  # FOKS data unavailable after 2020. Extrapolate using 2020 data
  # 2021 extrapolated data
  foks_diesel_distribution <- foks_diesel_distribution %>%
    bind_rows(foks_diesel_distribution %>% filter(year == "2020") %>% 
                mutate(year = "2021")) %>%
    # 2022 extrapolated data
    bind_rows(foks_diesel_distribution %>% filter(year == "2020") %>% 
                mutate(year = "2022"))
  
  # Read in residual fuel data from FOKS excel workbook
  foks_residual_distribution <- read_excel(
    "data/FOKS Resid Fuel Bunker 2020.xls", 
    sheet = 3, 
    skip = 0, range = "A2:AF53") %>%
    clean_names() %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", 
                 values_to = "residual_percent") %>%
    # Remove letters from year column 
    mutate(year = str_remove(year, "[a-z]"),
           # Get national total for each year by insta-grouping
           national_total = sum(residual_percent), .by = year) %>%
    # Rename state column
    rename(state = x1) %>%
    # Get FOKS residual fuel percentage for each state 
    mutate(foks_residual_percent = residual_percent / national_total) %>%
    # No longer need national total
    select(-national_total)
  
  # Append Extrapolated Data for 2021 Onward 
  
  # FOKS data unavailable after 2020. Extrapolate using 2020 data
  # 2021 extrapolated data
  foks_residual_distribution <- foks_residual_distribution %>%
    bind_rows(foks_residual_distribution %>% filter(year == "2020") %>% 
                mutate(year = "2021")) %>%
    # 2022 extrapolated data
    bind_rows(foks_residual_distribution %>% filter(year == "2020") %>% 
                mutate(year = "2022"))
  
  # Aggregate------------------------------------------------------
  corrections <- lst(
    adjustments, 
    national_corrections, 
    consumption_input, 
    ibf_corrections, 
    neu_corrections, 
    is_distribution, 
    ammonia_distribution, 
    petrochemicals_distribution, 
    petrochemicals_cb_distribution, 
    foks_diesel_distribution, 
    foks_residual_distribution)
  
  return(corrections)
  
}


# Carbon Factors
get_carbon_factors <- function (apply_variable_labels, 
                                ghgi_variables) {
  
  # Ratio of the molecular weight of carbon dioxide to carbon
  carbon_ratio <- 44/12
  
  # Apply label to variable
  carbon_ratio <- carbon_ratio %>% 
    set_variable_labels(.labels = ghgi_variables %>% 
                          filter(variable == "carbon_ratio") %>% 
                          pull(metadata))
  
  # Read in variable carbon factors data from FFC excel workbook
  carbon_factors_variable <- read_excel("data/national_inventory_CO2_data.xlsx", 
                                        sheet = "Factors", 
                                        skip = 0, range = "I12:AP28") %>%
    clean_names() %>%
    rename(source_description = fuel_type) %>%
    # Make sources lowercase and standardize sources
    mutate(source_description = str_to_lower(source_description), 
           source_description = case_when(
             source_description == "lpg (propane)" ~ "lpg",
             .default = source_description))
  
  
  # Read in carbon factors data from FFC excel workbook
  carbon_factors <- read_excel("data/national_inventory_CO2_data.xlsx", 
                               sheet = "Factors", 
                               skip = 0, range = "B9:D58") %>%
    clean_names() %>%
    # Remove middle column, rename other columns
    select(source_description = coal, carbon_factor = x3) %>%
    # NA = not applicable, NC = not calculated. Remove all NA & NC
    filter(!is.na(carbon_factor), 
           carbon_factor != "NC", 
           # Remove territories for now
           !str_detect(source_description, "erritor")) %>%
    # Make sources lowercase and standardize sources
    mutate(source_description = str_to_lower(source_description), 
           source_description = case_when(
             source_description == "naphtha (<401 deg. f)" ~ "naphtha", 
             source_description == "other oil (>401 deg. f)" ~ "other oils",
             source_description == "lpg (propane)" ~ "lpg",
             source_description == "residual fuel" ~ "residual fuel oil",
             source_description == "jet fuel (kerosene)" ~ "jet fuel", 
             str_detect(source_description, "utility coal") ~ "electric power coal", 
             .default = source_description)) %>%
    # Join with annually variable carbon factor data
    left_join(carbon_factors_variable, by = "source_description") %>%
    # Copy non-variable factors across all years
    mutate(across(starts_with("x"), 
                  ~ifelse(carbon_factor == "variable", ., carbon_factor))) %>%
    # First factor column no longer needed
    select(-carbon_factor) %>%
    # Pivot longer 
    pivot_longer(cols = starts_with("x"), 
                 names_to = "year", values_to = "carbon_factor") %>%
    # Remove x and make values numeric
    mutate(year = str_remove(year, "x"),
           carbon_factor = as.numeric(carbon_factor))
  
  # Apply labels to variables
  carbon_factors <- apply_variable_labels(carbon_factors, ghgi_variables)
  
  carbon <- lst(carbon_factors, carbon_ratio)
  
  return(carbon)
  
}



# Calculate Emissions for US Territories
# The procedure for retrieving and collating the FFC data for US territories
# differs from the procedure for states. 
get_territories_data <- function(carbon, 
                                 apply_variable_labels, 
                                 ghgi_variables) {
  # API Key----------------------------------------------------------------
  
  # API key generated 11/22/23 
  key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"
  
  
  # Retrieve Territories FF Consumption from EIA---------------------------
  
  # Get latest data year 
  latest_year <- year(now()) -1
  
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
    "[productId][]=26&&facets[productId]",
    "[]=62&facets[productId]",
    "[]=63&facets[productId][]=64&facets[productId]",
    "[]=65&facets[productId][]=66&facets[productId]",
    "[]=67&facets[productId][]=68&facets[productId]",
    "[]=7&start=1989&end=", latest_year, 
    "&sort[0][column]=",
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
             source_description == "liquefied petroleum gases" ~ "lpg",
             source_description == "dry natural gas" ~ "natural gas", 
             source_description == "foo" ~ "lubricants",
             .default = source_description))
  
  
  
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
                        "than 500 ppm sulfur", "Miscellaneous Products",
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
      source_description == "miscellaneous products" ~ "other petroleum liquids", 
      .default = source_description))
  
  
  # Calculcate TBtu---------------------------------------------------------
  
  ffc_territories <- ff_territories %>% 
    left_join(heat_content_territories, 
              by = c("year", "source_description")) %>%
    # time = 365 days for coal and nat gas, 1 year for everything else
    mutate(time_units = case_when(
      source_description %in% c("coal", "natural gas") ~ 1,
      .default = 365),
      # TBtu = consumption * time * 1000 * heat content / 1,000,000 
      tbtu = value * time_units * heat_content / 1000) %>%
    select(-unit, -dataFlagDescription, -value, -source)
  
  
  
  # Calculate Carbon Eq Emissions------------------------------------------
  
  # need to figure out which carbon factors to use
  
  carbon_territories <- ffc_territories %>% 
    left_join(carbon$carbon_factors, 
              by =c("source_description", "year")) %>%
    # MMT CO2  = btu * carbon factor/1000 * 44/12
    mutate(mmt_co2 = tbtu / 1000 * carbon_factor * carbon$carbon_ratio)
  
  # Apply labels to variables
  carbon_territories <- apply_variable_labels(carbon_territories, 
                                              ghgi_variables)
  
  # For Vince's spreadsheet------------------------------------------------
  
  territories_csv_format <- ff_territories %>%
    mutate(value = round(value, 4)) %>%
    arrange(year) %>% 
    arrange(source_description, state) %>%
    select(-dataFlagDescription, -source, -state, -unit) %>%
    group_by(state_name, source_description, year) %>%
    pivot_wider(names_from = year, names_prefix = "y") 
  
  write_csv(territories_csv_format, "territories_csv_format.csv")
  
  return(carbon_territories)
  
}


# Perform all state data adjustments
state_ffc_adjust_data <- function(seds, 
                                  corrections, 
                                  scraped_data,
                                  apply_variable_labels, 
                                  ghgi_variables) {
  
  # Residential
  seds_res_adjusted <- lst(
    
    coal = seds %>%
      filter(msn == "CLRCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(corrections$adjustments,
                by = c("source_description", "year", "sector_description")) %>%
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Deal with zeroes in the sums to avoid NaNs
      mutate(states_sum_value = if_else(
        states_sum_value == 0, 1, states_sum_value), 
        # Multiply adjustment factor by states's value / the above sum
        adjusted_value = national_value * 
          (value / states_sum_value)), 
    
    natural_gas = seds %>% 
      # Separate list element required to find net natural gas
      filter(msn %in% c("NGRCB", "SFRCB")) %>%
      # Subtract supplemental gas from total natural gas
      mutate(value = abs(diff(value)), .by = c(state, year)) %>%
      # Group_size shows that each group has exactly two rows. Good!
      # Supplemental gas no longer needed (and value is now duplicative)
      filter(msn != "SFRCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(corrections$adjustments,  
                by = c("source_description", "year", "sector_description")) %>%
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = national_value * 
               (value / states_sum_value)) %>%
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      mutate(msn = "net natural gas", 
             source_description = "natural gas"),
    
    distillate_fuel = seds %>%
      filter(msn == "DFRCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(corrections$adjustments, 
                by = c("source_description", "year", "sector_description")) %>%
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = national_value * 
               (value / states_sum_value)), 
    
    # LPGs (propane and/or HGL)
    lpg = seds %>%
      filter(case_when(as.integer(year) < 2010 ~ msn == "HLRCB", 
                       as.integer(year) >= 2010 ~ msn == "PQRCB")) %>% 
      # Adjusted = original value
      mutate(msn = "combined lpg", 
             adjusted_value = value), 
    
    # All other sources go in the last list element
    other_residential = seds %>% 
      filter(msn %in% c("KSRCB")) %>%
      mutate(adjusted_value = value)) %>%
    
    # Collapse list into a single data frame
    list_rbind()
  
  # Commercial
  seds_com_adjusted <- lst(
    
    coal = seds %>%
      filter(msn == "CLCCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(corrections$adjustments, 
                by = c("source_description", "year", "sector_description")) %>%
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = national_value * 
               (value / states_sum_value)), 
    
    distillate_fuel = seds %>%
      filter(msn == "DFCCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(corrections$adjustments, 
                by = c("source_description", "year", "sector_description")) %>%
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = national_value * 
               (value / states_sum_value)), 
    
    natural_gas = seds %>% 
      # Separate list element required to find net natural gas
      filter(msn %in% c("NGCCB", "SFCCB")) %>%
      # Subtract supplemental gas from total natural gas
      mutate(value = abs(diff(value)), .by = c(state, year)) %>%
      # Group_size shows that each group has exactly two rows. Good!
      # Supplemental gas no longer needed (and value is now duplicative)
      filter(msn != "SFCCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(corrections$adjustments, 
                by = c("source_description", "year", "sector_description")) %>%
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = national_value * 
               (value / states_sum_value)) %>%
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      mutate(msn = "net natural gas", 
             source_description = "natural gas"),
    
    gasoline = seds %>% 
      # Separate list element required to find net gasoline
      filter(msn %in% c("MGCCB", "EMCCB")) %>%
      # Subtract ethanol from total gasoline
      mutate(value = abs(diff(value)), .by = c(state, year)) %>%
      # Group_size shows that each group has exactly two rows. Good!
      # Ethanol no longer needed (and value is now duplicative)
      filter(msn != "EMCCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(corrections$adjustments , 
                by = c("source_description", "year", "sector_description")) %>%
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = national_value * 
               (value / states_sum_value)) %>%
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      mutate(msn = "net gasoline", 
             source_description = "motor gasoline"),
    
    # LPGs (propane and/or HGL)
    lpg = seds %>%
      filter(case_when(as.integer(year) < 2010 ~ msn == "HLCCB", 
                       as.integer(year) >= 2010 ~ msn == "PQCCB")) %>% 
      # Adjusted = original value
      mutate(msn = "combined lpg", 
             adjusted_value = value), 
    
    # All other sources go in the last list element
    other_commercial = seds %>% 
      filter(msn %in% c("KSCCB", "PCCCB", "RFCCB")) %>%
      mutate(adjusted_value = value)) %>%
    
    # Collapse list into a single data frame
    list_rbind()
  
  # Industrial
  seds_ind_adjusted <- lst(
    
    # Coking Coal
    coking_coal = seds %>%
      filter(msn == "CLKCB") %>%
      # Join with national data corrections
      left_join(corrections$national_corrections, by = "year") %>%
      mutate(states_sum_value = sum(value), .by = c(msn, year),
             # ippu percent = ippu / sum_states_value as long as ippu is greater
             ippu_factor = if_else(ippu < states_sum_value,
                                   ippu / states_sum_value, 1),
             adjusted_value = value - (value * ippu_factor), 
             # Standardize sector descriptions across all industrial sources
             sector_description = "industrial sector"),
    
    # Other Coal
    other_coal = seds %>%
      filter(msn == "CLOCB") %>%
      # Standardize sector descriptions across all industrial sources
      mutate(sector_description = "industrial sector") %>%
      left_join(coking_coal %>%
                  select(sum_coking_coal = states_sum_value,
                         coking_coal_value = value, year, state),
                by = c("year", "state")) %>%
      # Join with national data corrections
      left_join(corrections$national_corrections, by = "year") %>%
      # Join with consumption input data
      left_join(corrections$consumption_input,
                by = c("year", "source_description", "sector_description")) %>%
      # Join with I & S distribution data
      left_join(corrections$is_distribution, by = c("state", "year")) %>% 
      mutate(coke_factor = if_else(ippu < sum_coking_coal, 0,
                                   ippu - sum_coking_coal),
             other_coal_coke_adj =
               coke_factor * (coking_coal_value / sum_coking_coal),
             # SNG correction for North Dakota only
             other_coal_sng_adj  = if_else(state == "ND", sng_correction, 0),
             # Multiply I & S factor by I & S state distribution percentages
             other_coal_is_adj = is_coal_factor * is_percent,
             adjusted_value_pre = value -
               (other_coal_coke_adj + 
                  other_coal_sng_adj + 
                  other_coal_is_adj)) %>%
      # Get sum of all states' adjusted values
      mutate(states_sum_value = sum(adjusted_value_pre), 
             .by = c(msn, year)) %>%
      mutate(adjusted_value =
               (adjusted_value_pre / states_sum_value) * consumption_value),
    
    # Natural Gas
    natural_gas = seds %>%
      # Separate list element required to find net natural gas
      filter(msn %in% c("NGICB", "SFINB")) %>%
      # Subtract supplemental gas from total natural gas
      mutate(value = abs(diff(value)), .by = c(state, year)) %>%
      # Group_size shows that each group has exactly two rows. Good!
      # Supplemental gas no longer needed (and value is now duplicative)
      filter(msn != "SFINB") %>%
      # Join with national data corrections
      left_join(corrections$national_corrections, by = "year") %>%
      # Change source and MSN to reflect that it's net natural gas
      mutate(source_description = "natural gas",
             msn = "net natural gas") %>%
      # Join with consumption input data
      left_join(corrections$consumption_input,
                by = c("year", "source_description", "sector_description")) %>%
      # Join with I & S distribution data
      left_join(corrections$is_distribution, by = c("state", "year")) %>% 
      # Join with ammonia distribution data
      left_join(corrections$ammonia_distribution, by = c("state", "year")) %>% 
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      #  Ammonia factor * ammonia distribution = ammonia adjusted value 
      mutate(natural_gas_ammonia_adj = nat_gas_ammonia_factor * 
               ammonia_percent, 
             #  I & S factor * I & S distribution = I & S adjusted value 
             natural_gas_is_adj = is_gas_factor * is_percent,
             adjusted_value_pre = value -
               (natural_gas_ammonia_adj + natural_gas_is_adj)) %>%
      # Get sum of all states' adjusted (preliminary) values
      mutate(states_sum_value = sum(adjusted_value_pre), 
             .by = c(msn, year)) %>%
      # adj value / sum of all states' values * consumption = adjusted value
      mutate(adjusted_value =
               (adjusted_value_pre / states_sum_value) * 
               consumption_value),
    
    # Residual Fuel
    residual_fuel = seds %>%
      filter(msn == "RFICB") %>%
      # Join with national data corrections 
      left_join(corrections$national_corrections, by = "year") %>%
      # Join with consumption input data
      left_join(corrections$consumption_input,
                by = c("year", "source_description", "sector_description")) %>%
      # Join with petrochemicals carbon black distribution data
      left_join(corrections$petrochemicals_cb_distribution, by = c("state", "year")) %>% 
      # adjust for cb factor = cb_factor * petrochem cb distribution 
      # adjusted value = value - cb adjusted value (minimum = 0)
      mutate(residual_fuel_cb_adj = if_else(
        value - (cb_factor * petrochemical_cb_percent) < 0, 0, 
        value - (cb_factor * petrochemical_cb_percent))) %>% 
      # Get sum of all states' cb adjusted values
      mutate(states_sum_value = sum(residual_fuel_cb_adj), 
             .by = c(msn, year)) %>% 
      # now adjust by consumption value
      mutate(adjusted_value = 
               consumption_value * (residual_fuel_cb_adj / states_sum_value)),
    
    # Distillate Fuel
    distillate_fuel = seds %>%
      filter(msn == "DFICB") %>%
      # Join with national data corrections 
      left_join(corrections$national_corrections, by = "year") %>%
      # Join with consumption input data
      left_join(corrections$consumption_input,
                by = c("year", "source_description", "sector_description")) %>%
      # Join with I & S distribution data
      left_join(corrections$is_distribution, by = c("state", "year")) %>% 
      # distillate_fuel_is_adj = is_distillate_fuel_factor * 
      # mysterious hard-coded % used in the other_coal_is_adj, above
      # distillate_fuel_is_adj (if negative, then 0)
      mutate(distillate_fuel_is_adj = if_else(
        value - (is_distillate_fuel_factor * is_percent) < 0, 0, 
        value - (is_distillate_fuel_factor *  is_percent))) %>%
      # Get sum of all states' cb adjusted values
      mutate(states_sum_value = sum(distillate_fuel_is_adj), 
             .by = c(msn, year)) %>% 
      # now adjust by consumption value
      mutate(adjusted_value = 
               consumption_value *
               (distillate_fuel_is_adj / states_sum_value)),
    
    # Gasoline
    gasoline = seds %>% 
      # All gasoline - ethanol = net gasoline
      filter(msn %in% c("MGICB", "EMICB")) %>%
      # Subtract ethanol from total gasoline
      mutate(value = abs(diff(value)), .by = c(state, year)) %>%
      # Group_size shows that each group has exactly two rows. Good!
      # Supplemental gas no longer needed (and value is now duplicative)
      filter(msn != "EMICB") %>%
      # Get sum of all states' net gasoline
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # get adjustment factor for motor gasoline
      left_join(corrections$adjustments,
                by = c("source_description", "year", "sector_description")) %>%
      # Rename adjustment factor for clarity
      rename(motor_gas_factor = national_value) %>%
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      mutate(msn = "net gasoline", 
             source_description = "motor gasoline",
             # adjusted net gasoline = motor gas factor * gasoline - sum
             adjusted_value = motor_gas_factor * (value / states_sum_value)),
    # motor_gasoline_factor = US Compare--Industrial--motor gasoline 
    
    # Petroleum Coke
    petroleum_coke = seds %>%
      filter(msn == "PCICB") %>%
      # Get sum of all states' petroleum_coke
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # get adjustment factor for petroleum coke
      left_join(corrections$adjustments,
                by = c("source_description", "year", "sector_description")) %>%
      # Rename adjustment factor for clarity 
      rename(petro_coke_factor = national_value) %>%
      # adjusted petro coke = petro coke factor * (petro coke / sum of states)
      mutate(adjusted_value = petro_coke_factor * (value / states_sum_value)),
    
    # LPG
    lpg = seds %>%
      filter(msn %in% c("HLICB", "PPICB")) %>%
      # Subtract pentanes plus from HGL
      mutate(value = abs(diff(value)), .by = c(state, year)) %>%
      # Group_size shows that each group has exactly two rows. Good!
      # Remove pentanes plus from this list element:
      filter(msn != "PPICB") %>%
      # Change source description to reflect new value
      mutate(source_description = "hgl") %>%
      # Join with adjustments to get adjustment factor
      left_join(corrections$adjustments,
                by = c("source_description", "year", "sector_description")) %>%
      # Rename adjustment factor for clarity
      rename(ind_lpg_factor = national_value) %>%
      # Get sum of all states' lpg
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Rename MSN & source and calculate adjusted value
      mutate(msn = "net lpg", 
             adjusted_value = ind_lpg_factor * (value / states_sum_value)),
    # ind_lpg_factor = US SEDS Total--LPG (state's HLICB - PPICB)
    
    # All other sources go in this list element
    other_industrial = seds %>% 
      filter(msn %in% c("ARICB", "KSICB", "LUICB", "ABICB", "COICB", 
                        "MBICB", "MSICB", "FNICB", "FOICB", "PPICB", 
                        "SGICB", "SNICB", "UOICB", "WXICB", "PQICB", 
                        "PYICB", "EQICB", "EYICB", "BQICB", "BYICB", 
                        "IQICB", "IYICB")) %>%
      mutate(adjusted_value = value)) %>%
    
    # Collapse list into a single data frame
    list_rbind()
  # BTW: distillate_fuel_is_adj is required for neu_adjustments
  
  # Transportation
  seds_tra_adjusted <- lst(
    
    distillate_fuel = scraped_data$diesel_distribution %>% 
      # Join with adjustments data
      left_join(corrections$adjustments %>% 
                  # Can only join by 'year', so a filter is required
                  filter(source_description == "distillate fuel oil", 
                         sector_description == "transportation sector"), 
                by = c("year")) %>%
      mutate(adjusted_value = national_value * diesel_percent, 
             msn = "DFACB"), 
    
    gasoline = scraped_data$gasoline_distribution %>%
      # Join with adjustments data
      left_join(corrections$adjustments %>% 
                  # Can only join by 'year', so a filter is required
                  filter(source_description == "motor gasoline", 
                         sector_description == "transportation sector"), 
                by = c("year")) %>%
      mutate(adjusted_value = national_value * gasoline_percent, 
             msn = "net gasoline"), 
    
    lubricants = seds %>%
      filter(msn == "LUACB") %>%
      # Adjusted = original value 
      mutate(adjusted_value = value), 
    
    jet_fuel = seds %>%
      filter(msn == "JFACB") %>% 
      # Adjusted = original value
      mutate(adjusted_value = value), 
    
    residual_fuel = seds %>%
      filter(msn == "RFACB") %>% 
      # Adjusted = original value
      mutate(adjusted_value = value), 
    
    coal = seds %>%
      filter(msn == "CLACB") %>% 
      # Adjusted = original value 
      mutate(adjusted_value = value), 
    
    # LPGs (propane and/or HGL)
    lpg = seds %>%
      filter(case_when(as.integer(year) < 2010 ~ msn == "HLACB", 
                       as.integer(year) >= 2010 ~ msn == "PQACB")) %>% 
      # Adjusted = original value
      mutate(msn = "combined lpg", 
             adjusted_value = value), 
    
    aviation_gasoline = seds %>%
      filter(msn == "AVACB") %>%
      # Adjusted = original value
      mutate(adjusted_value = value), 
    
    natural_gas = seds %>%
      filter(msn == "NGACB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(corrections$adjustments, 
                by = c("source_description", "year", "sector_description")) %>%
      # rename for clarity
      rename(natural_gas_factor = national_value) %>%
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states' value / the above sum
      mutate(adjusted_value = natural_gas_factor * 
               (value / states_sum_value), 
             source_description = "natural gas")) %>%
    
    # Collapse list into a single data frame
    list_rbind()
  
  # Electric Power
  seds_ele_adjusted <- lst(
    
    coal = seds %>%
      filter(msn == "CLEIB") %>%
      # Make sector names match to complete the next join
      mutate(sector_description = word(sector_description, 
                                       start = 1, end = 3)) %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(corrections$adjustments, 
                by = c("source_description", "year", "sector_description")) %>%
      # Rename for clarity
      rename(coal_factor = national_value) %>%
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = coal_factor * 
               (value / states_sum_value)), 
    
    natural_gas = seds %>% 
      # find net natural gas
      filter(msn %in% c("NGEIB", "SFEIB")) %>%
      # Subtract supplemental gas from total natural gas
      mutate(value = abs(diff(value)), .by = c(state, year)) %>%
      # Group_size shows that each group has exactly two rows. Good!
      # Supplemental gas no longer needed (and value is now duplicative)
      filter(msn != "SFEIB") %>%
      # Make sector names match to complete the next join
      mutate(sector_description = word(sector_description, 
                                       start = 1, end = 3)) %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(corrections$adjustments, 
                by = c("source_description", "year", "sector_description")) %>%
      # Rename for clarity
      rename(natural_gas_factor = national_value) %>%
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = natural_gas_factor * 
               (value / states_sum_value)) %>%
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      mutate(msn = "net natural gas", 
             source_description = "natural gas"),
    
    residual_fuel = seds %>%
      filter(msn == "RFEIB") %>%
      # Make sector names match to complete the next join
      mutate(sector_description = word(sector_description, 
                                       start = 1, end = 3)) %>%
      mutate(adjusted_value = value),
    
    petroleum_coke = seds %>%
      filter(msn == "PCEIB") %>%
      # Make sector names match to complete the next join
      mutate(sector_description = word(sector_description, 
                                       start = 1, end = 3)) %>%
      mutate(adjusted_value = value),
    
    distillate_fuel = seds %>%
      filter(msn == "DFEIB") %>%
      # Make sector names match to complete the next join
      mutate(sector_description = word(sector_description, 
                                       start = 1, end = 3)) %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(corrections$adjustments, 
                by = c("source_description", "year", "sector_description")) %>%
      # Rename for clarity
      rename(distillate_fuel_factor = national_value) %>%
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = distillate_fuel_factor * 
               (value / states_sum_value))) %>%
    
    # Collapse list into a single data frame
    list_rbind()
  
  # IBF
  seds_ibf_adjusted <- lst(
    
    distillate_fuel = corrections$foks_diesel_distribution %>%
      left_join(corrections$ibf_corrections %>% filter(
        source_description == "distillate fuel oil"), 
        by = "year") %>% 
      # Calculate adjusted value (factor * percent)
      mutate(ibf_adjusted_value = ibf_value * foks_diesel_percent,
             # Add the MSN & sector  for transportation distillate fuel
             msn = "DFACB", 
             sector_description = "transportation sector"),
    
    residual_fuel = corrections$foks_residual_distribution %>%
      left_join(corrections$ibf_corrections %>% filter(
        source_description == "residual fuel oil"), 
        by = "year") %>% 
      # Calculate adjusted value (factor * percent)
      mutate(ibf_adjusted_value = ibf_value * foks_residual_percent, 
             # Add the MSN & sector for transportation residual fuel
             msn = "RFACB", 
             sector_description = "transportation sector"),
    
    jet_fuel = seds %>%
      filter(msn == "JFACB") %>%
      left_join(corrections$ibf_corrections,
                by = c("year", "source_description")) %>% 
      # Get sum of all states' value 
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(ibf_adjusted_value = ibf_value * 
               (value / states_sum_value))) %>%
    
    # Collapse list into a single data frame
    list_rbind() %>%
    # Remove nonessential columns to simplify joins in state_breakouts.R
    select(state, year, sector_description, source_description, 
           msn, ibf_adjusted_value)
  
  # NEU
  seds_neu_adjusted <- lst(
    
    # Other coal
    # NEU Correction applies to Tennessee only (Eastman Gas Plant)
    other_coal = corrections$neu_corrections %>%
      filter(source_description == "other coal", 
             sector_description == "industrial sector") %>%
      mutate(neu_adjusted_value = neu_factor,
             # Add industrial other coal MSN
             msn = "CLOCB", 
             # Tennessee only
             state = "TN"),
    
    # Natural gas
    natural_gas = corrections$neu_corrections %>%
      # Natural gas has a very long source name in the NEU data
      filter(str_detect(source_description, "natural gas")) %>%
      left_join(corrections$petrochemicals_distribution, by = "year") %>%
      # NEU value = NEU factor * distribution (will be zero for most states)
      mutate(neu_adjusted_value = neu_factor * petrochemical_percent, 
             # Standardize MSN & source to match seds_ind_adjusted
             msn = "net natural gas", 
             source_description = "natural gas"), 
    
    # Distillate fuel
    distillate_fuel = seds_ind_adjusted %>% 
      filter(msn == "DFICB") %>%
      # Join with NEU corrections data
      left_join(corrections$neu_corrections,
                by = c("year", "source_description", "sector_description")) %>%
      mutate(neu_adjusted_value = neu_factor * 
               (distillate_fuel_is_adj / states_sum_value)) %>%
      # These columns no longer needed
      select(-distillate_fuel_is_adj, -adjusted_value),
    
    # LPG
    lpg = seds %>%
      filter(msn %in% c("HLICB", "PPICB")) %>%
      # Subtract pentanes plus from HGL
      mutate(value = abs(diff(value)), .by = c(state, year)) %>%
      # Group_size shows that each group has exactly two rows. Good!
      # Pentanes plus computed below, so remove from this element:
      filter(msn != "PPICB") %>%
      # Change source description to reflect new value
      mutate(source_description = "hgl") %>%
      # Join with neu corrections to get neu factor
      left_join(corrections$neu_corrections,
                by = c("year", "source_description", "sector_description")) %>%
      # Get sum of all states' lpg
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Rename MSN and calculate adjusted value
      mutate(msn = "net lpg", 
             neu_adjusted_value = neu_factor * (value / states_sum_value)),
    
    # Pentanes plus
    pentanes_plus = seds %>%
      filter(msn == "PPICB") %>%
      # Join with neu corrections to get neu factor
      left_join(corrections$neu_corrections,
                by = c("year", "source_description", "sector_description")) %>%
      # Get sum of all states' pentanes plus
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Calculate adjusted value
      mutate(neu_adjusted_value = neu_factor * (value / states_sum_value)),
    
    # Petroleum coke
    petroleum_coke = seds %>%
      filter(msn == "PCICB") %>%
      # Join with neu corrections to get neu factor
      left_join(corrections$neu_corrections,
                by = c("year", "source_description", "sector_description")) %>%
      # Get sum of all states' petroleum coke
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Calculate adjusted value
      mutate(neu_adjusted_value = neu_factor * (value / states_sum_value)),
    
    # Still gas
    still_gas = seds %>%
      filter(msn == "SGICB") %>%
      # Join with neu corrections to get neu factor
      left_join(corrections$neu_corrections,
                by = c("year", "source_description", "sector_description")) %>%
      # Get sum of all states' still gas
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Calculate adjusted value
      mutate(neu_adjusted_value = neu_factor * (value / states_sum_value))) %>%
    
    # Collapse list into a single data frame
    list_rbind() %>%
    # Remove nonessential columns to simplify joins in state_breakouts.R
    select(sector_description:year, neu_adjusted_value, state, msn)
  
  seds_adjusted <- lst(seds_res_adjusted, 
                       seds_com_adjusted, 
                       seds_ind_adjusted,
                       seds_tra_adjusted, 
                       seds_ele_adjusted, 
                       seds_ibf_adjusted, 
                       seds_neu_adjusted)
  
  # Aggregate all data and make IBF & NEU adjustments
  seds_all_adjusted <- list_rbind(seds_adjusted %>% 
                                    # Remove IBF and NEU data for now
                                    discard(names(.) %in% 
                                              c("seds_ibf_adjusted", 
                                                "seds_neu_adjusted"))) %>%
    select(state:adjusted_value, -eia_description) %>%
    # NOTE: maybe move this to carbon calculations, below
    # standardize source descriptions for join with carbon_factors
    mutate(source_description = case_when(
      str_detect(source_description, "naphtha less than") ~ "naphtha", 
      str_detect(source_description, "other oils") ~ "other oils", 
      str_detect(source_description, " and ") ~ 
        str_replace(source_description, " and ", " & "),
      str_detect(source_description, "aviation gasoline c") ~ "aviation gasoline",
      source_description == "aviation gasoline blending components" ~ 
        "avgas blend components",
      source_description == "motor gasoline blending components" ~ 
        "mogas blend components",
      str_detect(source_description, "hydrocarbon|propane") ~ "lpg",
      str_detect(source_description, "miscellaneous") ~ "misc. products",
      str_detect(source_description, "distillate ") ~ "distillate fuel oil",
      str_detect(source_description, "residual") ~ "residual fuel oil",
      .default = source_description)) %>%
    # Add sector to each coal source--required for carbon factors & NEU
    mutate(source_description = case_when(
      source_description == "coal" & sector_code == "CC" ~ 
        "commercial coal", 
      source_description == "coal" & sector_code == "EI" ~ 
        "electric power coal",
      source_description == "coal" & sector_code == "OC" ~ 
        "industrial other coal",
      source_description == "coal" & sector_code == "KC" ~ 
        "industrial coking coal",
      source_description == "coal" & sector_code == "AC" ~ 
        "transportation coal",
      source_description == "coal" & sector_code == "RC" ~ 
        "residential coal",
      .default = source_description)) %>%
    
    # Subtract NEU and IBF adjustments
    # Join with NEU adjusted data
    left_join(seds_adjusted$seds_neu_adjusted, 
              by = c("sector_description", "source_description", "year", 
                     "state", "msn")) %>%
    # Join with IBF adjusted data
    left_join(seds_adjusted$seds_ibf_adjusted, 
              by = c("sector_description", "source_description", "year", 
                     "state", "msn")) %>%
    # Get adjusted value - NEU and IBD values = final adjusted tBtu 
    mutate(neu_ibf_adjusted_value = if_else(
      # Subtract IBF only if IBF applies (i.e., isn't NA)
      !is.na(ibf_adjusted_value), 
      adjusted_value - ibf_adjusted_value, 
      adjusted_value), 
      # Some MSNs are 100% NEU. For these, NEU value = 100% of adjusted value
      neu_adjusted_value = if_else(msn %in% c("ARICB", "LUICB", "FNICB", "CLKCB",
                                              "FOICB", "SNICB", "WXICB", 
                                              "MSICB", "LUACB"), 
                                   adjusted_value, neu_adjusted_value),  
      neu_ibf_adjusted_value = if_else(
        # Subtract NEU only if NEU applies (i.e., isn't NA)
        !is.na(neu_adjusted_value), 
        neu_ibf_adjusted_value - neu_adjusted_value, 
        neu_ibf_adjusted_value)) %>% 
    
    # Finally, ZERO OUT all pentanes plus and unfinished oils
    mutate(neu_ibf_adjusted_value = case_when(
      source_description == "pentanes plus" ~ 0, 
      source_description == "unfinished oils" ~ 0, 
      .default = neu_ibf_adjusted_value))
  
  # Apply labels to variables
  seds_all_adjusted <- apply_variable_labels(seds_all_adjusted, ghgi_variables)
  
  seds_all_plus_ind <- lst(seds_all_adjusted, seds_ind_adjusted)
  
  return(seds_all_plus_ind)
  
}


# Calculate Carbon Emissions
state_ffc_calculate_emissions <- function(seds_all_adjusted, 
                                          carbon, 
                                          apply_variable_labels, 
                                          ghgi_variables)  {
  
  carbon_emissions <- seds_all_adjusted %>% 
    left_join(carbon$carbon_factors, 
              by =c("source_description", "year")) %>%
    # MMT CO2  = btu * carbon factor/1000 * 44/12
    mutate(mmt_co2 = neu_ibf_adjusted_value * 
             (carbon_factor/1000) * carbon$carbon_ratio, 
           # Restore original coal source descriptions
           source_description = case_when(
             str_detect(source_description, "coking coal" ) ~ "coking coal", 
             str_detect(source_description, "(?<!coking )coal" ) ~ "coal", 
             .default = source_description))
  
  # Apply labels to variables
  carbon_emissions <- apply_variable_labels(carbon_emissions, ghgi_variables)
  
  return(carbon_emissions)
  
}


# Create state FFC ggplot figures
state_ffc_ggplot_figures <- function(seds_all_adjusted, 
                                     seds_ind_adjusted,
                                     corrections, 
                                     carbon_emissions) {
  
  # Read National Emissions Data (for Figures)------------------------------
  
  national_emissions <- read_excel("data/national_inventory_CO2_data.xlsx", 
                                   sheet = "InvDB", 
                                   skip = 0, range = "C16:BA32") %>%
    clean_names() %>%
    select(sector_description = category, source_description = fuel1, ghg, 
           starts_with("x")) %>%
    pivot_longer(starts_with("x"), names_to = "year", values_to = "value") %>%
    mutate(year = str_sub(year, 2, 5), 
           source_description = str_to_lower(source_description), 
           ghg = str_to_lower(ghg), 
           value = parse_number(value), 
           sector_description = str_to_lower(sector_description) %>% 
             str_c(" sector"), 
           sector_description = if_else(
             str_detect(sector_description, "electric"), 
             "electric power sector", sector_description))

  # Color Palettes-----------------------------------------------------------
  
  # Hex	Gas	Sector	Economic Sectors
  #4F81BD	Carbon Dioxide	Energy	Residential
  #C0504D	Methane	Agriculture	Agriculture
  #4198AF	Nitrous Oxide	IPPU	Industry
  #9BBB59	HFCs, PFCs, SF6, NF3	LULUF Emissions	Transportation
  #D7925D	 	Waste	Commercial
  #7F63A1	Net CO2 Flux from LULUCF	LULUCF Removals	Electric Power Industry
  #49525E	Net Emissions	Net Emissions	 
  
  # Style Guide Palette
  ghg_palette <- c("#4F81BD", "#C0504D", "#4198AF", "#9BBB59",
                   "#D7925D", "#7F63A1", "#49525E")
  
  myPalette <- colorRampPalette(c("thistle1","slateblue3"), space = "Lab")
  
  # Create Data Object (SEDS + National)--------------------------------------
  
  # Energy Use, State Totals vs. National: 
  
  state_vs_national_btu <-
    lst(
      states = seds_all_adjusted %>% 
        select(sector_description, source_description, year, value) %>% 
        mutate(value = value / 1000, 
               dataname = "state_total", 
               # sector_description = case_when(
               #   str_detect(sector_description, 
               #              "electric") ~ "electric power sector", 
               #   str_detect(sector_description, 
               #              "industrial consumption") ~ "industrial sector", 
               # .default = sector_description), 
               # source_description = if_else(
               source_description = if_else(source_description %in% c(
                 "hgl", "propane", "propylene",
                 "ethane","ethylene",
                 "normal butane", "butylene",
                 "isobutane", "isobutylene"), "lpg",
                 source_description)),
      
      national = corrections$adjustments %>% 
        rename(value = national_value) %>% 
        mutate(dataname = "national_total", 
               value = value / 1000, 
               source_description = case_when(
                 source_description == "other coal" ~  "coal", 
                 source_description == "hgl" ~  "lpg",
                 .default = source_description)))
  
  energy_use <- state_vs_national_btu %>%
    map(\(.x) filter(.x, str_detect(source_description, "coal|natural gas")) %>%
          # Get unadjusted SEDS totals to plot against national totals
          group_by(dataname, sector_description, source_description, year) %>%
          summarize(total_btu = sum(value, na.rm = TRUE)) %>%
          ungroup()) %>%
    list_rbind()
  
  # CO2 Emissions, State Totals vs. National: 
  
  # Energy Use, State Totals vs. National: 

  state_vs_national_co2 <-
    lst(
      states = carbon_emissions %>%
        select(sector_description, year, value = mmt_co2) %>%
        mutate(dataname = "state_total",
               ghg = "co2"),

      national = national_emissions %>%
        select(-source_description) %>%
        mutate(dataname = "national_total"))

  carbon_comparison <- state_vs_national_co2 %>%
    map(\(.x)
        # Get unadjusted SEDS totals to plot against national totals
        group_by(.x, dataname, sector_description, year) %>%
          summarize(total_co2 = sum(value, na.rm = TRUE)) %>%
          ungroup()) %>%

    list_rbind()


  # Plots---------------------------------------------------------------------

  # We should decide at the outset if we want to plot the raw values,
  # transformed values, the differences, or the proportions so we can be
  # consistent across figures and avoid confusion.

  # Differences in Coal/NG State Totals vs National Totals-------------------
  state_ffc_figures <- lst(

    fig_2_2 = energy_use %>%
      # Ignore coking coal and gasoline
      filter(!str_detect(source_description, "cok|gasoline"),
             # res, com, ind, and ele only
             sector_description != "transportation sector") %>%
      # Remove everything after the comma in 'natural gas'
      # NOTE: Should we be subtracting supplemental fuels for this figure?
      mutate(source_description = case_when(
        str_detect(source_description, "coal") ~ "coal",
        str_detect(source_description, "natural gas") ~ "natural gas")) %>%
      # Every observation needs both state and national totals
      pivot_wider(names_from = dataname, values_from = total_btu) %>%
      ggplot(aes(x = as.numeric(year), y = state_total - national_total)) +
      geom_line(aes(color = sector_description),
                linewidth = 2.8) +
      # geom_abline(slope = 0, intercept = 1) +
      theme_classic() +
      scale_color_manual(values = ghg_palette) +
      scale_x_continuous(n.breaks = 20) +
      theme(axis.text.x = element_text(size = 18, angle = 270, vjust = 0.08),
            axis.text.y = element_text(size = 18),
            axis.title.y = element_text(size = 20),
            strip.background = element_blank(),
            strip.text.x = element_text(size = 38),
            plot.title = element_text(family = "Calibri"),
            text = element_text(family = "Calibri"),
            legend.text = element_text(size = 24),
            legend.title = element_blank()) +
      labs(x = "", y = "Difference: SEDS - national (TBtu) ") +
      facet_wrap(~ source_description),



    ## Differences in Petroleum Coke State Totals vs National Totals----------

    fig_2_3 = state_vs_national_btu %>%
      map(\(.x)
          group_by(.x, dataname, sector_description, source_description, year) %>%
            summarize(total_btu = sum(value, na.rm = TRUE)) %>%
            ungroup()) %>%
      list_rbind() %>%
      filter(str_detect(source_description, "petroleum coke"),
             sector_description == "industrial sector") %>%
      # Every observation needs both state and national totals
      pivot_wider(names_from = dataname, values_from = total_btu) %>%
      ggplot(aes(x = as.numeric(year), y = state_total - national_total)) +
      # Plot state total as a proportion of national total;
      geom_line(aes(color = sector_description),
                linewidth = 3) +
      # geom_abline(slope = 0, intercept = 1) +
      theme_classic() +
      scale_color_manual(values = ghg_palette) +
      scale_x_continuous(n.breaks = 20) +
      theme(axis.text.x = element_text(size = 18, angle = 270, vjust = 0.08),
            axis.text.y = element_text(size = 18),
            axis.title.y = element_text(size = 24),
            # axis.title.y =
            plot.title = element_text(family = "Calibri"),
            text = element_text(family = "Calibri"),
            legend.position = "none") +
      labs(x = "", y = "Difference: SEDS - national (TBtu) "),

    ## Sectoral Differences in Select Fuels-----------------------------------

    fig_2_4a = state_vs_national_btu %>%
      list_rbind() %>%
      mutate(sector_description = word(sector_description)) %>%
      filter(year == "2021",
             sector_description != "electric",
             str_detect(source_description,
                        "kerosene|residual|lubricants")) %>%
      group_by(dataname, sector_description, source_description, year) %>%
      summarize(total_btu = sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      # Every observation needs both state and national totals
      pivot_wider(names_from = dataname, values_from = total_btu) %>%
      ggplot(aes(x = sector_description, y = state_total - national_total)) +
      geom_col(linewidth = 0.5, aes(fill = sector_description)) +
      theme_classic() +
      scale_fill_manual(values = ghg_palette) +
      geom_abline(slope = 0, intercept = 0) +
      theme(axis.text.x = element_text(size = 12, angle = 270, vjust = 0.08),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 18),
            strip.text.x = element_text(size = 18),
            plot.title = element_text(family = "Calibri"),
            text = element_text(family = "Calibri"),
            legend.position = "none",
            strip.background = element_blank()) +
      labs(x = "", y = "Difference: SEDS - national (TBtu) ") +
      # Option 2: facet_wrap to avoid overlapping lines
      facet_grid(~ source_description, scales = "free"),


    fig_2_4b = state_vs_national_btu %>%
      list_rbind() %>%
      mutate(sector_description = word(sector_description)) %>%
      filter(year == "2021",
             sector_description != "electric",
             str_detect(source_description,
                        "lpg")) %>%
      group_by(dataname, sector_description, source_description, year) %>%
      summarize(total_btu = sum(value, na.rm = TRUE)) %>%
      ungroup() %>%
      # Every observation needs both state and national totals
      pivot_wider(names_from = dataname, values_from = total_btu) %>%
      ggplot(aes(x = sector_description, y = state_total - national_total)) +
      geom_col(linewidth = 0.5, aes(fill = sector_description)) +
      theme_classic() +
      scale_fill_manual(values = ghg_palette) +
      geom_abline(slope = 0, intercept = 0) +
      theme(axis.text.x = element_text(size = 12, angle = 270, vjust = 0.08),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 14),
            strip.text.x = element_text(size = 14),
            plot.title = element_text(family = "Calibri"),
            text = element_text(family = "Calibri"),
            legend.position = "none",
            strip.background = element_blank()) +
      labs(x = "", y = ""),

    ## IPPU Adjustments Made to Industrial Sector Energy Use--------------------


    fig_2_5 = seds_ind_adjusted %>%
      mutate(ippu_adjustments = case_when(
        msn == "CLKCB" ~ value * ippu_factor,
        msn == "CLOCB" ~ other_coal_coke_adj + other_coal_is_adj,
        msn == "net natural gas" ~ natural_gas_ammonia_adj + natural_gas_is_adj,
        msn == "RFICB" ~ cb_factor * petrochemical_cb_percent,
        msn == "DFICB" ~ is_distillate_fuel_factor * is_percent,
        .default = 0)) %>%
      mutate(ippu_adjustments = if_else(
        ippu_adjustments < 0, 0, ippu_adjustments)) %>%
      group_by(year) %>%
      summarize(total_ippu_adjustments = sum(
        ippu_adjustments, na.rm = TRUE),
        percent_of_unadjusted = total_ippu_adjustments / sum(
          value, na.rm = TRUE)) %>%
      ungroup() %>%
      ggplot(aes(x = year, y = total_ippu_adjustments)) +
      geom_col(aes(fill = percent_of_unadjusted * 100)) +
      theme_classic() +
      scale_fill_gradientn(colours = myPalette(100)) +
      theme(axis.text.x = element_text(size = 12, angle = 270, vjust = 0.08),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 22),
            plot.title = element_text(family = "Calibri"),
            text = element_text(family = "Calibri"),
            legend.position = "bottom",
            legend.text = ,
            strip.background = element_blank()) +
      labs(x = "", y = "tBtu", fill = "% of unadj. ind. sector total"),

    # Figures 2-6 and 2-7 are infographics built from tables

    # Comparison of Transportation Sector Fuel Use----------------------------


    fig_2_8 = ggplot(state_vs_national_btu %>%
                        list_rbind() %>%
                        filter(sector_description == "transportation sector",
                               str_detect(source_description,
                                          "distillate|motor")) %>%
                        group_by(dataname, sector_description, source_description, year) %>%
                        summarize(total_btu = sum(value, na.rm = TRUE)) %>%
                        ungroup(),
                      aes(x = as.numeric(year), y = total_btu)) +
      geom_line(aes(color = dataname), linewidth = 1) +
      geom_point(aes(color = dataname), size = 1.9) +
      theme_classic() +
      scale_color_manual(values = ghg_palette) +
      scale_x_continuous(n.breaks = 20) +
      theme(axis.text.x = element_text(size = 10, angle = 270, vjust = 0.08),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 22),
            plot.title = element_text(family = "Calibri"),
            text = element_text(family = "Calibri"),
            legend.position = "bottom",
            legend.title = element_blank(),
            strip.text.x = element_text(size = 20),
            strip.background = element_blank()) +
      labs(x = "", y = "tBtu") +
      facet_grid(~ source_description),


    # Fig 2-9 requires the full suite of Transport sector data


    ## Adjustments made to Industrial Sector for NEUs---------------------------

    fig_2_10 = seds_all_adjusted %>%
      filter(sector_description == "industrial sector") %>%
      group_by(year) %>%
      summarize(total_neu_adjustments = sum(
        neu_adjusted_value, na.rm = TRUE),
        percent_of_unadjusted = total_neu_adjustments / sum(
          value, na.rm = TRUE)) %>%
      ungroup() %>%
      ggplot(aes(x = year, y = total_neu_adjustments)) +
      geom_col(aes(fill = percent_of_unadjusted * 100)) +
      theme_classic() +
      scale_fill_gradientn(colours = myPalette(100)) +
      theme(axis.text.x = element_text(size = 8, angle = 270, vjust = 0.08),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 22),
            plot.title = element_text(family = "Calibri"),
            text = element_text(family = "Calibri"),
            legend.position = "bottom",
            strip.background = element_blank()) +
      labs(x = "", y = "tBtu", fill = "% of unadj. ind. sector total"),


    ## Adjustments Made to Transportation Sector for IBFs------------------------

    fig_2_11 = seds_all_adjusted %>%
      filter(sector_description == "transportation sector") %>%
      group_by(year) %>%
      summarize(total_ibf_adjustments = sum(
        ibf_adjusted_value, na.rm = TRUE),
        percent_of_unadjusted = total_ibf_adjustments / sum(
          value, na.rm = TRUE)) %>%
      ungroup() %>%
      ggplot(aes(x = year, y = total_ibf_adjustments)) +
      geom_col(aes(fill = percent_of_unadjusted * 100)) +
      theme_classic() +
      scale_fill_gradientn(colours = myPalette(100)) +
      theme(axis.text.x = element_text(size = 8, angle = 270, vjust = 0.08),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(size = 22),
            plot.title = element_text(family = "Calibri"),
            text = element_text(family = "Calibri"),
            legend.position = "bottom",
            strip.background = element_blank()) +
      labs(x = "", y = "tBtu", fill = "% of unadj. trans. sector total"),


    ## Differences in State-Level Total and National Total FFC CO2 Emissions------


    fig_2_12a = carbon_comparison %>%
      group_by(year, sector_description, dataname) %>%
      summarize(value = sum(total_co2, na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = dataname, values_from = value) %>%
      ggplot(aes(x = as.numeric(year), y = state_total - national_total)) +
      geom_col(aes(fill = sector_description), position = "dodge", width = 2) +
      # geom_abline(slope = 0, intercept = 1) +
      theme_classic() +
      scale_color_manual(values = ghg_palette) +
      theme(axis.text.x = element_text(size = 14, angle = 270, vjust = 0.08),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(family = "Calibri"),
            text = element_text(family = "Calibri"),
            legend.text = element_text(size = 14),
            legend.title = element_blank(),
            legend.position = "bottom") +
      labs(x = "", y = "Difference: SEDS - national (MMT CO2) "),

    fig_2_12b = carbon_comparison %>%
      group_by(year, dataname) %>%
      summarize(value = sum(total_co2, na.rm = TRUE)) %>%
      ungroup() %>%
      pivot_wider(names_from = dataname, values_from = value) %>%
      ggplot(aes(x = as.numeric(year), y = state_total - national_total)) +
      geom_col(fill = "darkgreen", color = "green", width = 0.9) +
      # geom_abline(slope = 0, intercept = 1) +
      theme_classic() +
      scale_color_manual(values = ghg_palette) +
      theme(axis.text.x = element_text(size = 14, angle = 270, vjust = 0.08),
            axis.text.y = element_text(size = 14),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(family = "Calibri"),
            text = element_text(family = "Calibri"),
            legend.position = "none") +
      labs(x = "", y = "Difference: SEDS - national (MMT CO2) ")


    ## Differences in State-Level and National Total NEU CO2 Emissions--------

    # Not sure what data I'm looking at in this figure--ask Vince

    # fig_2_15 <- seds_all_adjusted %>%
    #   filter(!is.na(neu_adjusted_value)) %>%
    #   group_by(year) %>%
    #   summarize(total_neu_adjustments = sum(
    #     neu_adjusted_value, na.rm = TRUE),
    #     percent_of_unadjusted = total_ibf_adjustments / sum(
    #       value, na.rm = TRUE)) %>%
    #   ungroup() %>%
    #   ggplot(aes(x = year, y = total_ibf_adjustments)) +
    #   geom_col(aes(fill = percent_of_unadjusted * 100)) +
    #   theme_classic() +
    #   scale_fill_gradientn(colours = myPalette(100)) +
    #   theme(axis.text.x = element_text(size = 8, angle = 270, vjust = 0.08),
    #         axis.text.y = element_text(size = 12),
    #         axis.title.y = element_text(size = 22),
    #         legend.position = "bottom",
    #         strip.background = element_blank()) +
    #   labs(x = "", y = "tBtu", fill = "% of unadj. trans. sector total")

  )

  return(state_ffc_figures)

}


# create state 'gt' tables for report
state_ffc_gt_tables <- function() {
  state_ffc_tables <-lst(
    
    table_2_1 <- read_excel("data/state_report_tables.xlsx", sheet = 1) %>%
      gt() %>%
      tab_header(
        title = "Table 2-1. Overview of Approaches for Estimating State-Level Energy Sector GHG Emissions") %>%
      opt_row_striping() %>%
      sub_missing(missing_text = " ") %>%
      text_replace(pattern = "CH4", replacement = ("CH<sub>4</sub>")) %>%
      text_replace(pattern = "CO2", replacement = ("CO<sub>2</sub>")) %>%
      text_replace(pattern = "N2O", replacement = ("N<sub>2</sub>O")) %>%
      tab_options(
        heading.border.bottom.style = "solid", 
        heading.border.bottom.color = "black", 
        heading.title.font.weight = "bold", 
        heading.align = "center", 
        column_labels.background.color = "steelblue",
        column_labels.border.bottom.style = "solid", 
        column_labels.border.bottom.color = "black", 
        column_labels.border.bottom.width = 3,
        table.align = "center", 
        row.striping.include_stub = TRUE,
        row.striping.include_table_body = TRUE,
        row.striping.background_color = "lightsteelblue1") %>%
      tab_style(style = cell_text(align = "left"), 
                locations = list(cells_body())) %>%
      tab_style(style = cell_text(align = "center"), 
                locations = list(cells_column_labels())) %>%
      tab_style(style = cell_text(font = "Calibri"), 
                locations = cells_title()) %>%
      opt_footnote_marks(marks = "letters") %>%
      tab_footnote(footnote = "Emissions are not likely occurring in U.S. territories; due to a lack of available data and the nature of this category, territories not listed are not estimated.", 
                   locations = list(cells_column_labels(columns = 4), 
                                    cells_body(columns = 4, rows = c(3, 4, 6)))),
    
    table_2_2 <- read_excel("data/state_report_tables.xlsx", sheet = 2) %>%
      # Add grouping columns for the gt table
      mutate(group = c(rep("Determine Activity Data", times = 7), 
                       rep("Calculate CO2 Emissions", times = 3))) %>%
      gt(groupname_col = "group") %>%
      tab_header(
        title = "Table 2-2.  Comparison of Approaches/Data Sources Used to Determine FFC Emissions") %>%
      opt_row_striping() %>%
      # rows_add(`National-Level Estimates` = "Determine Activity Data", .before = 1) %>%
      sub_missing(missing_text = " ") %>%
      text_replace(pattern = "CH4", replacement = ("CH<sub>4</sub>")) %>%
      text_replace(pattern = "CO2", replacement = ("CO<sub>2</sub>")) %>%
      text_replace(pattern = "N2O", replacement = ("N<sub>2</sub>O")) %>%
      tab_options(
        heading.border.bottom.style = "solid", 
        heading.border.bottom.color = "black", 
        heading.title.font.weight = "bold", 
        heading.align = "center", 
        column_labels.background.color = "steelblue",
        column_labels.border.bottom.style = "solid", 
        column_labels.border.bottom.color = "black", 
        column_labels.border.bottom.width = 3,
        row_group.background.color = "lightsteelblue3",
        table.align = "center", 
        row.striping.include_stub = TRUE,
        row.striping.include_table_body = TRUE,
        row.striping.background_color = "lightsteelblue1") %>%
      tab_style(style = cell_text(align = "left"), 
                locations = list(cells_body())) %>%
      tab_style(style = cell_text(font = "Calibri"), 
                locations = cells_title()) %>%
      tab_style(style = cell_text(align = "center"), 
                locations = list(cells_column_labels(), cells_row_groups())),
    
    table_2_3 <- read_excel("data/state_report_tables.xlsx", sheet = 3) %>%
      gt(groupname_col = "Source/Category", row_group_as_column = TRUE) %>%
      tab_header(
        title = md("Table 2-3. Default Data Sources for Mobile Source Non-CO<sub>2</sub> Emissions")) %>%
      opt_row_striping() %>%
      # rows_add(`National-Level Estimates` = "Determine Activity Data", .before = 1) %>%
      sub_missing(missing_text = " ") %>%
      text_replace(pattern = "CH4", replacement = ("CH<sub>4</sub>")) %>%
      text_replace(pattern = "CO2", replacement = ("CO<sub>2</sub>")) %>%
      text_replace(pattern = "N2O", replacement = ("N<sub>2</sub>O")) %>%
      tab_stubhead(label = "Source/Category") %>%
      tab_options(
        heading.border.bottom.style = "solid", 
        heading.border.bottom.color = "black", 
        heading.title.font.weight = "bold", 
        heading.align = "center", 
        column_labels.background.color = "steelblue",
        column_labels.border.bottom.style = "solid", 
        column_labels.border.bottom.color = "black", 
        column_labels.border.bottom.width = 3,
        table.align = "center", 
        row.striping.include_stub = TRUE,
        row.striping.include_table_body = TRUE,
        row.striping.background_color = "lightsteelblue1") %>%
      tab_style(style = cell_text(align = "left"), 
                locations = list(cells_body())) %>%
      tab_style(style = cell_text(align = "center"), 
                locations = list(cells_column_labels(), cells_row_groups())) %>%
      tab_style(style = cell_text(font = "Calibri"), 
                locations = cells_title()) %>%
      tab_style(style = cell_fill(color = "lightsteelblue1"), 
                locations = list(cells_row_groups())),
    
    table_2_4 <- read_excel("data/state_report_tables.xlsx", sheet = 4) %>%
      gt() %>%
      tab_header(
        title = md("Table 2-4: Summary of Approaches to Disaggregate Waste Incineration Emissions Across Time Series")) %>%
      opt_row_striping() %>%
      # rows_add(`National-Level Estimates` = "Determine Activity Data", .before = 1) %>%
      sub_missing(missing_text = " ") %>%
      text_transform(locations = cells_body(column = `Summary of Data Used`), 
                     fn = function(x) {
                       paste("• ", x) }) %>%
      tab_options(
        heading.border.bottom.style = "solid", 
        heading.border.bottom.color = "black", 
        heading.title.font.weight = "bold", 
        heading.align = "center", 
        column_labels.background.color = "steelblue",
        column_labels.border.bottom.style = "solid", 
        column_labels.border.bottom.color = "black", 
        column_labels.border.bottom.width = 3,
        table.align = "center", 
        row.striping.include_table_body = TRUE,
        row.striping.background_color = "lightsteelblue1") %>%
      tab_style(style = cell_text(align = "left"), 
                locations = list(cells_body())) %>%
      tab_style(style = cell_text(font = "Calibri"), 
                locations = cells_title()) %>%
      tab_style(style = cell_text(align = "center"), 
                locations = list(cells_column_labels())),
    
    table_2_4 <- read_excel("data/state_report_tables.xlsx", sheet = 4) %>%
      gt() %>%
      tab_header(
        title = md("Table 2-4: Summary of Approaches to Disaggregate Waste Incineration Emissions Across Time Series")) %>%
      opt_row_striping() %>%
      # rows_add(`National-Level Estimates` = "Determine Activity Data", .before = 1) %>%
      sub_missing(missing_text = " ") %>%
      text_transform(locations = cells_body(column = `Summary of Data Used`), 
                     fn = function(x) {
                       paste("• ", x) }) %>%
      tab_options(
        heading.border.bottom.style = "solid", 
        heading.border.bottom.color = "black", 
        heading.title.font.weight = "bold", 
        heading.align = "center", 
        column_labels.background.color = "steelblue",
        column_labels.border.bottom.style = "solid", 
        column_labels.border.bottom.color = "black", 
        column_labels.border.bottom.width = 3,
        table.align = "center", 
        row.striping.include_table_body = TRUE,
        row.striping.background_color = "lightsteelblue1") %>%
      tab_style(style = cell_text(align = "left"), 
                locations = list(cells_body())) %>%
      tab_style(style = cell_text(font = "Calibri"), 
                locations = cells_title()) %>%
      tab_style(style = cell_text(align = "center"), 
                locations = list(cells_column_labels()))
    
    # table 2-5 must be populated with data; 
    # where is this data from? Ask Vince
  )
  
  return(state_ffc_tables)
  
}

