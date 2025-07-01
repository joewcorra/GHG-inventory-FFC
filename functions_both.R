
# DATA SETUP------------------------------------
data_setup <- function(harmonized_data) {
  # Create Dataframes of MSNs and descriptions and state codes
  # Also create general-use functions
  
  # Create function standardize FFC data-----------------------
  
  standardize_ffc <- function (data, msn_names) {
    
    standardized_data <- data %>%
      
      # Make selected columns lowercase (if they exist)
      mutate(across(matches(c("sector_description", "source_description", 
                              "eia_description", "unit")), 
                    ~str_to_lower(.))) 
    
    if ("sector_description" %in% colnames(standardized_data)) {
      # Make 'year' a factor
      standardized_data <- standardized_data %>% 
        mutate(year = as_factor(year),
               
               # Standardize sector descriptions; add the word "sector"
               sector_description = case_when(
                 str_detect(sector_description, "electric") ~  "electric power sector", 
                 msn == "CLOCB" ~  "industrial sector", # industrial coking coal
                 msn == "CLKCB" ~  "industrial sector", # other industrial coal
                 msn == "SFINB" ~  "industrial sector", # supp. gaseous fuels
                 .default = sector_description))
    }
    
    if ("source_description" %in% colnames(standardized_data)) {
      standardized_data <- standardized_data %>% 
        mutate(
          source_description = str_replace(source_description, " and ", " & "),
          source_description = case_when(
            source_description %in% 
              unique(msn_names$msn$source_description) ~ source_description,
            str_detect(source_description, "distillate") ~ "distillate fuel oil", 
            str_detect(source_description, "residual") ~ "residual fuel oil", 
            str_detect(source_description, "jet fuel") ~ "jet fuel", 
            str_detect(source_description, "av(?=blend)") ~ "aviation gasoline",
            str_detect(source_description, "(?=.*av)(?=.*blend)") ~ "aviation gasoline blending components", 
            str_detect(source_description, "utility coal") ~ "coal",
            str_detect(source_description, "other coal") ~ "coal",
            str_detect(source_description, "other oils") ~ "other oils",
            str_detect(source_description, "<401 deg|naphtha less") ~ "petrochemical feedstocks, naphtha less than 401 degrees F", 
            str_detect(source_description, "misc") ~ "miscellaneous petroleum products",
            str_detect(source_description, "(?=.*mo)(?=.*blend)") ~ "motor gasoline blending components", 
            str_detect(source_description, "lpg (propane)") ~ "lpg",
            str_detect(source_description, "liquefied petroleum gas") ~ "lpg",
            str_detect(source_description, "hgl|hydrocarbon gas liquids") ~ "lpg",
            .default = source_description)) }
    
    
    return(standardized_data) 
    
  }
  
  # Create function to add labels (with 'labelled')-------------------
  
  # Function: add variable labels to dataframes and convert 'year' to factor
  apply_variable_labels <- function(data, ghgi_variables) {
    # If 'year' column exists, convert to factor
    if ("year" %in% colnames(data)) {
      data <- data %>%
        mutate(year = as_factor(year))
    }
    
    labels <- as.list(deframe(ghgi_variables %>%
                                select(variable, metadata) %>%
                                filter(variable %in%
                                         colnames(data))))
    # Filter the variables dataframe and apply metadata as variable labels
    var_label(data) <- labels
    
    return(data)
    
  }
  
  
  # Read GHGI harmonization data-------------------------------------------
  
  ghgi_values <- harmonized_data$values %>%
    map(\(.x) na.omit(.x) %>%
          as.vector())
  
  ghgi_variables <- harmonized_data$variables
  
  ghgi_invdb_values <- harmonized_data$invdb
  
  
  # Create Filtering Dataframe-------------------------------------------
  
  # EIA provides a list of descriptors for MSNs, but not sources and sectors.
  # Official names of sources and sectors are found in the SEDS Documentation
  # Guide: https://www.eia.gov/state/seds/sep_prices/notes/pr_guide.pdf
  # We must create tibbles of descriptors for sources & sectors of interest.
  
  # Tried to read directly from EIA, but not all sources are included in PDF!
  # d <- pdftools::pdf_text("https://www.eia.gov/state/seds/sep_prices/notes/pr_guide.pdf") 
  # 
  # e <- d %>%
  #   str_split("\\s{2,}") %>% 
  #   map(\(x) str_replace(x, "\\\n.*", "")) %>% 
  #   map(as_tibble) %>% 
  #   list_rbind() %>% 
  #   filter(str_detect(value, "=")) %>%
  #   separate_wider_delim(cols = value, 
  #                        names = c("code", "description"), 
  #                        delim = "=") 
  
  # Create 'sources' tibble
  sources <- tibble(
    source_code = c(
      "AR", "AB", "AV", "B1", "BD", "BF", "BO", "BQ", "BT",
      "BX", "BY", "CC", "CL", "CO", "DF", "DK", "EL", "EM", "ES",
      "EQ", "EY", "FN", "FO", "FS", "GE", "HL", "HP", "IQ", "IY",
      "JF", "KS", "LU", "MB", "MG", "MS", "NG", "NN", "NU", "OH",
      "OJ", "OP", "P1", "P5", "PA", "PC", "PE", "PP", "PQ",
      "PY", "RF", "SF", "SG", "SN", "SU", "TE", "TN", "UO",
      "WD", "WW", "WX"
    ),
    source_description = c(
      "asphalt and road oil",
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
      "petrochemical feedstocks, still gas", "geothermal energy", 
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
      "wood", "wood and waste", "waxes"
    )
  )
  
  # Create 'sectors' data frame
  sectors <- tibble(
    sector_code = c(
      "AC", "CC", "EG", "EI", "ET", "IC", "RC", "TC", "TX", "TP",
      "EX", "GB", "IM", "KC", "NI", "OC", "SU", "AS", "CS",
      "IS", "RS", "SC", "SS", "RF", "PR", "FD", "PF", "IN",
      "LC", "LP", "PZ", "CP", "IP", "AP", "RP", "MP", "AB"
    ),
    sector_description = c(
      "transportation sector", "commercial sector",
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
      "aviation gasoline blending components consumed by the industrial sector"
    )
  )
  
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
    mutate(
      eia_description = str_to_lower(eia_description),
      unit = str_to_lower(unit),
      # Create source code and sector code
      source_code = str_sub(msn, 1, 2),
      sector_code = str_sub(msn, 3, 4)
    )
  
  # Clean up the temporary file
  unlink(temp_file)
  
  # Read in MSN data file and join with 'sources' and 'sectors'
  msn <- msn_data %>%
    filter(unit == "billion btu") %>%
    left_join(sources, by = "source_code") %>%
    left_join(sectors, by = "sector_code") %>%
    # For national calcs: Add nat gas MSNs not included in SEDS
    add_row(
      msn = "NNCCB", sector_description = "commercial sector",
      source_code = "NN", sector_code = "CC",
      source_description = "natural gas consumed by the commercial sector (excluding supplemental gaseous fuels)"
    ) %>%
    add_row(
      msn = "NNEIB", sector_description = "electric power sector (generation)",
      source_code = "NN", sector_code = "EI", 
      source_description = "natural gas consumed by the electric power sector (excluding supplemental gaseous fuels)"
    ) %>%
    add_row(
      msn = "NNICB", sector_description = "industrial sector",
      source_code = "NN", sector_code = "IC",
      source_description = "natural gas consumed by the industrial sector (excluding supplemental gaseous fuels)"
    ) %>%
    add_row(
      msn = "NNRCB", sector_description = "residential sector",
      source_code = "NN", sector_code = "RC",
      source_description = "natural gas consumed by the residential sector (excluding supplemental gaseous fuels)"
    )
  
  
  # State and Territory Names---------------------------------------------
  
  # Create dataframes of states and territories (names & 2-letter codes)
  
  state_name_key <- tibble(
    state = str_squish(ghgi_values$state),
    state_name = str_squish(ghgi_values$state_name)
  ) %>%
    # Identify territories
    mutate(territory = if_else(state %in% c(
      "AS", "GU", "PR",
      "VI", "USIQ", "WAK"
    ),
    TRUE, FALSE
    ))
  
  
  # MSN Lookup for State and National Emissions-------------------------------
  
  
  # # Vector of MSNs to look up in the state summaries:
  msn_lookup <- c(
    "ABICB", "ARICB", "AVACB", "BDACB", "BDTCB", "BQICB", "BYICB",
    "CCNIB", "CLICB", "CLKCB", "CLOCB", "CLRCB", "CLACB",
    "CLCCB", "CLEIB", "COICB", "DFACB", "DFCCB", "DFEIB",
    "DFICB", "DKEIB", "DFRCB", "EMACB", "EMCCB", "EMICB", "EMTCB",
    "EQICB", "EYICB", "FNICB", "FOICB", "GETCB", 
    "HLACB", "HLCCB", "HLICB",
    "HLRCB", "IQICB", "IYICB", "JFACB", "KSICB", "KSCCB", 
    "KSRCB","LUACB", "LUICB", "MBICB", "MGACB", "MGCCB",
    "MGICB", "MSICB", "NGACB", "NGCCB", "NGEIB", "NGRCB", "NGICB",
    "NNCCB", "NNEIB", "NNICB", "NNRCB", "PCCCB", "PCEIB",
    "PCICB", "PQACB", "PQCCB", "PQICB", "PPICB", "PQRCB", "PYICB",
    "RFACB", "RFCCB", "RFEIB", "RFICB", "SFEIB", "SFCCB", "SFRCB",
    "SGICB", "SFINB", "SNICB", "UOICB", "WXICB"
  )
  
  
  msn_names <- lst(msn, msn_lookup, state_name_key)
  
  general_data <- lst(
    ghgi_values,
    ghgi_values,
    ghgi_variables,
    ghgi_invdb_values,
    msn_names,
    apply_variable_labels, 
    standardize_ffc
  )
  
  return(general_data)
}

# CARBON FACTORS-----------------------------------
get_carbon_factors <- function(general_data,
                               carbon_factors, 
                               neu_storage) {
  
  # Ratio of the molecular weight of carbon dioxide to carbon
  carbon_ratio <- 44/12
  
  # Apply label to variable
  carbon_ratio <- carbon_ratio %>%
    set_variable_labels(.labels = general_data$ghgi_variables %>%
                          filter(variable == "carbon_ratio") %>%
                          pull(metadata))
  
  # Read in variable carbon factors data from FFC excel workbook
  carbon_factors_variable <- carbon_factors$factors_variable %>%
    clean_names() %>%
    rename(source_description = fuel_type) %>%
    mutate(source_description = str_to_lower(source_description))
  
  # Read in carbon factors data from FFC excel workbook
  carbon_factors <- carbon_factors$factors_fixed %>%
    clean_names() %>%
    select(source_description = fuel_type, carbon_coefficient) %>%
    mutate(source_description = str_to_lower(source_description)) %>%
    filter(
      # NA = not applicable, NC = not calculated. Remove all NA & NC
      !is.na(carbon_coefficient),
      carbon_coefficient != "NC"
    ) %>%
    # Join with annually variable carbon factor data
    left_join(carbon_factors_variable, by = "source_description") %>%
    # Copy non-variable factors across all years
    mutate(across(
      starts_with("x"),
      ~ ifelse(carbon_coefficient == "variable", ., carbon_coefficient)
    )) %>%
    # First factor column no longer needed
    select(-carbon_coefficient) %>%
    # Pivot longer
    pivot_longer(
      cols = starts_with("x"),
      names_to = "year", values_to = "carbon_factor"
    ) %>%
    # Remove x and make values numeric
    mutate(
      year = str_remove(year, "x"),
      carbon_factor = as.numeric(carbon_factor)
    ) 
  
  d <-general_data$standardize_ffc(carbon_factors, general_data$msn_names)
  
  neu_storage <- neu_storage %>%
    clean_names() %>%
    rename(sector_description = sector, 
           source_description = source) %>%
    pivot_longer(cols = starts_with("x"), 
                 names_to = "year", 
                 values_to = "storage_factor") %>%
    mutate(sector_description = str_to_lower(sector_description), 
           source_description = str_to_lower(source_description), 
           year = str_remove_all(year, "x") %>% as_factor())
  
  carbon_coefficients <- lst(carbon_factors, 
                             carbon_ratio, 
                             neu_storage)
  
  return(carbon_coefficients)
  
}
# DATASCRAPING (NATIONAL AND STATE)-----------------------------------------

scrape_data <- function(general_data) {
  # Set year to match most recent available year (current year minus two)
  latest_year <- year(Sys.Date()) - 2
  
  # Scrape FWHA Fuel Use (State & National FFC) ----------------------------
  
  # Temporary file storage path
  local_excel_path <- tempfile(fileext = ".xlsx")
  # URL for gasoline data by state, 1949 to present year
  gasoline_url <- paste0(
    "https://www.fhwa.dot.gov/policyinformation/statistics/",
    latest_year, "/xls/mf226.xlsx"
  )
  # URL for special fuel (diesel) data by state, 1949 to present year
  special_fuel_url <- paste0(
    "https://www.fhwa.dot.gov/policyinformation/statistics/",
    latest_year, "/xls/mf225.xlsx"
  )
  
  # Retrieve gasoline Excel file data
  GET(gasoline_url, write_disk(local_excel_path, overwrite = TRUE))
  
  # Read from temp file
  gasoline_distribution <- read_excel(local_excel_path) %>%
    clean_names() %>%
    # Remove unneeded rows
    filter(
      !is.na(state),
      state != "Total"
    ) %>%
    # Make all value columns numeric
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    # Make data long; i.e., one row per year
    pivot_longer(
      cols = -1, names_to = "year",
      values_to = "gasoline_percent"
    ) %>%
    # Remove letters from year column
    mutate(
      state = str_squish(state), 
      year = str_remove(year, "[a-z]"),
      # Get national total for each year by insta-grouping
      national_total = sum(gasoline_percent, na.rm = TRUE), .by = year
    ) %>%
    # Retain only 1990 onward
    filter(year > 1989) %>%
    # Get gasoline percentage for each state
    mutate(
      gasoline_percent = gasoline_percent / national_total,
      # Fix the dumb abbreviation for District of Columbia
      state = if_else(str_detect(state, "Dist"),
                      "District of Columbia", state
      )
    ) %>%
    rename(state_name = state) %>%
    # Get state codes
    left_join(
      general_data$msn_names$state_name_key %>%
        filter(territory == FALSE),
      by = "state_name"
    ) %>%
    # No longer need national total or full state name
    select(-national_total, -state_name)
  
  # Retrieve diesel Excel file data
  GET(special_fuel_url, write_disk(local_excel_path, overwrite = TRUE))
  # Read from temp file
  
  
  diesel_distribution <- read_excel(local_excel_path) %>%
    clean_names() %>%
    # Remove unneeded rows
    filter(
      !is.na(state),
      state != "Total"
    ) %>%
    # Make all value columns numeric
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    # Make data long; i.e., one row per year
    pivot_longer(
      cols = -1, names_to = "year",
      values_to = "diesel_percent"
    ) %>%
    # Remove letters from year column
    mutate(
      state = str_squish(state), 
      year = str_remove(year, "[a-z]"),
      # Get national total for each year by insta-grouping
      national_total = sum(diesel_percent, na.rm = TRUE), .by = year
    ) %>%
    # Retain only 1990 onward
    filter(year > 1989) %>%
    # Get gasoline percentage for each state
    mutate(
      diesel_percent = diesel_percent / national_total,
      # Fix the dumb abbreviation for District of Columbia
      state = if_else(str_detect(state, "Dist"), "District of Columbia", state)
    ) %>%
    rename(state_name = state) %>%
    # Get state codes
    left_join(
      general_data$msn_names$state_name_key %>%
        filter(territory == FALSE),
      by = "state_name"
    ) %>%
    # No longer need national total or full state name
    select(-national_total, -state_name) %>%
    # NOTE: diesel dist value for OR 2018 missing; interpolated instead
    mutate(diesel_percent = if_else(state == "OR" & year == "2018", 
                                    0.0139, diesel_percent))
  
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
    pivot_longer(
      cols = -1, names_to = "year",
      values_to = "gasoline_use_gal"
    ) %>%
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
    latest_year, "/xls/vm1.xlsx"
  )
  # https://www.fhwa.dot.gov/policyinformation/statistics/1998/vm1.cfm
  GET(diesel_url, write_disk(local_excel_path, overwrite = TRUE))
  
  diesel_use_by_class <- read_excel(local_excel_path) %>%
    clean_names() %>%
    # Make all value columns numeric
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    # Make data long; i.e., one row per year
    pivot_longer(
      cols = -1, names_to = "year",
      values_to = "gasoline_use_gal"
    ) %>%
    # Retain only year and value
    select(year, gasoline_use_gal) %>%
    # Remove letters from year column
    mutate(year = str_remove(year, "[a-z]"))
  # Retain only 1990 onward
  
  scraped_data <- lst(
    diesel_distribution,
    diesel_use_by_class,
    gasoline_distribution,
    gasoline_use_national
  )
  
  return(scraped_data)
}


