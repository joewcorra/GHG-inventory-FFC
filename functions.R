# Functions

# I. DATA SETUP------------------------------------
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

# II. CARBON FACTORS-----------------------------------
get_carbon_factors <- function(general_data,
                               carbon_factors) {
  
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
      carbon_coefficient != "NC",
      # Retain only fixed-value coefficients
      # !str_detect(carbon_coefficient, "variable"), 
      # Remove territories for now
      !str_detect(source_description, "erritor")
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


  carbon_coefficients <- lst(carbon_factors, carbon_ratio)

  return(carbon_coefficients)
  
}
# III. NATIONAL CONSUMPTION DATA-------------------------
national_ffc_read_eia_data <- function(general_data) {
  
  # API key generated 11/22/23
  key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"

  # Change to match most recent available year (current year minus two)
  latest_year <- year(Sys.Date()) - 2

  
  # Read EIA Consumption Data----------------------------------------------
  
  ## EIA Consumption Data--------------------------------------------------
  
  # Function to read data from EIA API, 1 year at a time
  get_national_results <- function(year) {
    # For now, to avoid exceeding the 5000-row data limit, we will pull
    # only one year and state per query. This requires n=51*years API queries.
    results <- paste0(
      "https://api.eia.gov/v2/total-energy/data/?frequency",
      "=annual&data[0]=value&start=", year, 
      "&end=", year, "&sort[0][column]=period&sort[0][direction]",
      "=desc&offset=0&length=5000&api_key=", key) %>% # our API key is required
      GET() %>% # retrieve page from url
      content("raw") %>% # extract content as a raw vector
      rawToChar() %>% # convert to character data
      fromJSON() # convert from JSON to R object
  }
  
  ### Read National Data-----------------------------------------------
  
  tic()
  api_results <- expand_grid(
    year = 1990:latest_year) %>%
    pmap(function(year) get_national_results(year))
  toc()

  ### Collate National Data-------------------------------------------
  
  # Extract national consumption data from API results
  us_consumption_all <- api_results %>%
    map(\(.x) pluck(.x, "response", "data")) %>%
    list_rbind() %>%
    clean_names() %>%
    select(year = period, msn, value, unit) %>%
    mutate(unit = str_to_lower(unit),
           year = as.character(year), 
           value = parse_number(value), 
           msn = str_sub(msn, 1, 5)) 
  
  # Collate national data (not including industrial coal)
  us_consumption <- us_consumption_all %>%
    filter(unit == "trillion btu", 
           msn %in% general_data$msn_names$msn_lookup, 
           str_sub(msn, 3, 4) %in% c("AC", "RC", "IC", "CC", "EI")) %>%
    left_join(general_data$msn_names$msn %>%
                select(-unit), by = "msn") %>%
    # Remove any duplicates caused by appending new annual data
    distinct() %>%
    general_data$standardize_ffc(general_data$msn_names)
  
  # Collate industrial coal data, calculate and append
  us_ind_coal <- us_consumption_all %>%
    filter(str_sub(msn, 3, 4) %in% c("KC", "OC")) %>%
    mutate(source_description = if_else(str_sub(msn, 3, 4) == "KC", 
           "coking coal", "other coal")) %>%
    group_by(year, source_description) %>%
    summarize(value = prod(value)) %>%
    ungroup() %>%
    mutate(sector_description = "industrial sector")
  
  # Append ind coal data to us_consumption
  us_consumption <- us_consumption %>%
    rows_append(us_ind_coal) %>%
    # Remove total ind coal (replaced by coking and other coal)
    filter(msn != "CLICB")
  
  ## HGL Component Data-----------------------------------------
  
  # # Also includes pentanes plus 
  # eia_api_lpg <- paste0(
  #   "https://api.eia.gov/v2/petroleum/cons/psup/data/?frequency=annual&", 
  #   "data[0]=value&",
  #   # "facets[series][]=MPPUPUS1&", # pentanes
  #   # "facets[series][]=MUOUPUS1&",  # unfinished oils
  #   "facets[series][]=MBIUPUS1&", # isobutane-isobutylene
  #   "facets[series][]=MBNUPUS1&", # butane-butylene
  #   "facets[series][]=METUPUS1&", # ethane-ethylene
  #   "facets[series][]=MPRUPUS1&", # propane-propylene
  #   "start=1990&end=", latest_year,
  #   "&sort[0][column]=period&sort[0][direction]=desc&", 
  #   "offset=0&length=5000&api_key=", key
  # ) %>%
  #   GET() %>% # retrieve page from url
  #   content("raw") %>% # extract content as a raw vector
  #   rawToChar() %>% # convert to character data
  #   fromJSON() # convert from JSON to R object
  # 
  # lpg_components <- pluck(eia_api_lpg, "response", "data") %>%
  #   select(
  #     year = period, 
  #     eia_description = 'series-description', value, unit = units
  #   ) %>%
  #   # Make value numeric
  #   mutate(value = as.numeric(value))

  # Doing this requires unpublished EIA propane data, as well as
  # heat content by lpg and disaggregating combined lpgs
  # For now we will read processed data from csv
  lpg_components <- read.csv("data/lpg_national.csv") %>%
    clean_names() %>%
    pivot_longer(cols = !lpg, names_to = "year") %>%
    group_by(year, lpg) %>%
    summarize(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(year = parse_number(year) %>% as_factor(), 
           msn = "combined lpg", 
           unit = "trillion btu",
           eia_description = NA_character_,
           source_code = NA_character_,
           sector_code = "IC", 
           source_description = "hydrocarbon gas liquids",
           sector_description = "industrial sector") %>%
    select(-lpg)
  
  # Append ind coal data to us_consumption
  us_consumption <- us_consumption %>%
    rows_append(lpg_components) %>%
    # Remove ind HGL (replaced by combined lpg)
    filter(msn != "HLICB")
  
  ## Heat Content Data----------------------------------------------

  # Heat content may vary and is used for some adjustments
  eia_api_heat <- paste0(
    "https://api.eia.gov/v2/total-energy/data/?frequency=annual&data[0]",
    "=value&facets[msn][]=DMTCKUS&facets[msn][]=MGTCKUS&facets[msn][]=HLTCKUS&",
    "start=1990&end=", latest_year,
    "&sort[0][column]=msn&sort[0][direction]=asc&offset=0&length=5000&api_key=",
    key
  ) %>%
    GET() %>% # retrieve page from url
    content("raw") %>% # extract content as a raw vector
    rawToChar() %>% # convert to character data
    fromJSON() # convert from JSON to R object

  # Units in Millions of Btu / Barrel
  heat_content <- pluck(eia_api_heat, "response", "data") %>%
    select(
      year = period, msn,
      eia_description = seriesDescription, heat_content = value
    ) %>%
    # Make heat content value numeric
    mutate(heat_content = as.numeric(heat_content))


  ## Vessel Bunkering Diesel Data----------------------------------

  eia_api_vessel_bunker <- paste0(
    "https://api.eia.gov/v2/petroleum/cons/821usea/data/?frequency=annual",
    "&data[0]=value&facets[duoarea][]=NUS&facets[process][]=VAB&start=1990&end=",
    latest_year,
    "&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000",
    "&api_key=", key
  ) %>%
    GET() %>% # retrieve page from url
    content("raw") %>% # extract content as a raw vector
    rawToChar() %>% # convert to character data
    fromJSON() # convert from JSON to R object

  # Units in Millions of Gallons
  vessel_bunker_dist_fuel <- pluck(eia_api_vessel_bunker, "response", "data") %>%
    select(year = period, eia_description = "series-description", value) %>%
    # Make fuel consumption value numeric
    mutate(value = as.numeric(value))

  ## Ethanol (Transportation) Data----------------------------------

  eia_api_ethanol <- paste0(
    "https://api.eia.gov/v2/total-energy/data/?frequency",
    "=annual&data[0]=value&start=1990&end=2022&sort[0][column]",
    "=period&sort[0][direction]",
    "https://api.eia.gov/v2/total-energy/data/?frequency",
    "=annual&data[0]=value&facets[msn][]=EMACBUS&start=1990&end=2023&sort[0]",
    "[column]=period&sort[0][direction]=desc&offset=0&length=5000&api_key=", key
  ) %>%
    GET() %>% # retrieve page from url
    content("raw") %>% # extract content as a raw vector
    rawToChar() %>% # convert to character data
    fromJSON() # convert from JSON to R object

  ethanol_tra <- pluck(eia_api_ethanol, "response", "data") %>%
    mutate(msn = str_sub(msn, 1, 5), value = as.numeric(value)) %>%
    select(-unit, eia_description = seriesDescription, year = period, ethanol = value)

  national_ffc_data <- lst(
    us_consumption, vessel_bunker_dist_fuel,
    heat_content, ethanol_tra
  )

  return(national_ffc_data)
}


# IV. NATIONAL MOBILE DATA---------------------------------------------
get_mobile_adjustments_data <- function(moves3,
                                        national_ffc_data,
                                        scraped_data) {
  # Applies to Commercial, Industrial, Transportation
  
  # Motor Gasoline------------------------------------------------------------
  ## MOVES Data---------------------------------------------------------

  moves <- moves3$vmt %>%
    clean_names() %>%
    pivot_longer(
      cols = starts_with("x"),
      values_to = "vmt_percent", names_to = "year"
    ) %>%
    left_join(
      moves3$fuel %>%
        clean_names() %>%
        pivot_longer(
          cols = starts_with("x"),
          values_to = "fuel_use_percent", names_to = "year"
        ),
      by = c("vehicle_type", "year")
    ) %>%
    mutate(
      year = str_sub(year, 2, 5),
      fuel_type = case_when(
        vehicle_type %in% c(
          "MC", "LDGV", "LDGT",
          "HDGV", "HDGB"
        ) ~ "gasoline",
        vehicle_type %in% c(
          "LDDV", "LDDT",
          "HDDT", "HDDB"
        ) ~ "diesel"
      )
    )



  ## EIA Mogas------------------------------------------------------------

  us_consumption_mogas <- national_ffc_data$us_consumption %>%
    filter(msn %in% c("MGCCB", "MGACB", "MGICB")) %>%
    mutate(mogas_ethanol_adjusted = value / 0.001)

  us_consumption_diesel <- national_ffc_data$us_consumption %>%
    filter(msn %in% c("DFACB", "DFCCB", "DFICB", "DFRCB", "DKEIB"))

  ## Total On-Road Mogas-----------------------------------------------------

  # Gasoline joules per gallon. Fixed value
  mogas_energy <- 43488 * 2839
  mogas_annual_totals <- 1
  mogas_nonroad_total <- 1
  # THIS WORKS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  mogas <- moves %>%
    filter(fuel_type == "gasoline") %>%
    left_join(scraped_data$gasoline_use_national, by = "year") %>%
    left_join(
      national_ffc_data$heat_content %>%
        filter(msn == "MGTCKUS") %>%
        select(year, heat_content),
      by = "year"
    ) %>%
    left_join(national_ffc_data$ethanol_tra, by = "year") %>%
    mutate(
      mogas_nonroad_total = mogas_nonroad_total,
      gas_use = fuel_use_percent * (
        gasoline_use_gal * 1000 - mogas_nonroad_total),
      tbtu = (gas_use / 42 * heat_content) / 10^9
    ) %>%
    mutate(tbtu_sum = sum(tbtu), .by = year) %>%
    mutate(
      ethanol_adjustment_factor = 1 - (ethanol / 1000 / tbtu_sum),
      tbtu_adjusted = tbtu * ethanol_adjustment_factor
    )

  # Incomplete as of 10/3/24
  diesel <- moves %>%
    filter(fuel_type == "diesel") %>%
    # This might not be the right diesel data..........
    left_join(scraped_data$diesel_use_by_class, by = "year") %>%
    left_join(
      national_ffc_data$heat_content %>%
        filter(msn == "DMTCKUS") %>%
        select(year, heat_content),
      by = "year"
    )

  ## Total Nonroad Mogas--------------------------------------------------

  # Total non-road motor gasoline use
  total_nonroad_mogas <- us_consumption_mogas %>%
    group_by(year) %>%
    # Get total EIA mogas by year (tra + com + ind)
    summarize(mogas_ethanol_adjusted = sum(mogas_ethanol_adjusted)) %>%
    ungroup() %>%
    # Get MOVES3 on-road mogas totals
    left_join(mogas_annual_totals %>%
      select(year, onroad_mogas_excl_ethanol_v2), by = "year") %>%
    # Annual non-road mogas = total mogas - on-road mogas
    mutate(nonroad_mogas = mogas_ethanol_adjusted -
      onroad_mogas_excl_ethanol_v2)

  ## Rec Boat Mogas-------------------------------------------------------

  # Recreational boat motor gasoline total is the smaller of 1) rec boat gas
  # calculated by the bottom-up method, or 2) total non-road motor gasoline

  # Placeholder values
  # These data come from [Nonroad] workbook. Awaiting data access.
  nonroad_2_stroke <- 1
  nonroad_4_stroke <- 2

  # First compute rec boat mogas by the bottom-up method
  rec_boat_mogas_bottom_up <- heat_content %>%
    # Motor gasoline only
    filter(str_detect(eia_description, "asoline")) %>%
    select(year, heat_content) %>%
    # Will need a left_join here once we get the nonroad engine data
    mutate(rec_boat_mogas_bottom_up = heat_content * ((nonroad_2_stroke +
      nonroad_4_stroke) / 42) / 10^9)
  # Are these parentheses correct?

  # Rec boat motor gas is the lower of two values. Start with the non-road data
  rec_boat_mogas <- total_nonroad_mogas %>%
    # Join with the bottom-up data
    left_join(rec_boat_mogas_bottom_up, by = "year") %>%
    # Select whichever value is lower: non-road or bottom up
    mutate(rec_boat_mogas = min(rec_boat_mogas_bottom_up, nonroad_mogas,
      na.rm = TRUE
    )) %>%
    select(year, rec_boat_mogas)

  ## Perform Motor Gasoline Adjustments-----------------------------------

  national_mogas <- us_consumption_mogas %>%
    # Join EIA consumption data with the MOVES3 annual results
    left_join(
      mogas_annual_totals %>%
        # Only a few columns are needed now
        select(
          year, onroad_mogas_excl_ethanol_v1,
          onroad_mogas_excl_ethanol_v2,
          onroad_mogas_incl_ethanol
        ),
      by = "year"
    ) %>%
    # Create 'meta sector' to differentiate transport from non-transport
    mutate(meta_sector = case_when(
      sector_description == "commercial sector" ~ "non-trans",
      sector_description == "industrial sector" ~ "non-trans",
      sector_description == "transportation sector" ~ "trans"
    )) %>%
    # Non-trans mogas total = total non-trans mogas - on-road - rec boats
    mutate(remaining_mogas = case_when(
      meta_sector == "non-trans" ~ sum(mogas_ethanol_adjusted) -
        (onroad_mogas_excl_ethanol_v2 + rec_boat_mogas),
      .default = mogas_ethanol_adjusted
    ), .by = year) %>%
    mutate(
      mogas_adjusted = case_when(
        # Com or ind = remaining mogas value * EIA mogas / sum of ind + com mogas
        meta_sector == "non-trans" ~
          remaining_mogas * mogas_ethanol_adjusted / sum(mogas_ethanol_adjusted),
        # Transportation = on-road total + rec boat total
        meta_sector == "trans" ~
          onroad_mogas_excl_ethanol_v1 + rec_boat_mogas
      ),
      .by = c(year, meta_sector)
    )


  # Distillate (Diesel) Fuel Adjustments--------------------------
  ## Vessel Fuel Data-----------------------------------------------

  # Retrieved from EIA

  vessel_bunker_dist_fuel
  # Data needed for vessel fuel and rail
  dist_fuel_vessel <- vessel_bunker_dist_fuel %>%
    # Dist fuel only
    filter(str_detect(eia_description, "istillate")) %>%
    # Convert units to million gallons
    mutate(value = value * 1000)

  ## Rail Fuel Data------------------------------------------------------

  # From weird rail sources...awaiting data access.

  dist_fuel_rail <- sum(
    dist_fuel_rail_i, dist_fuel_rail_ii_iii,
    dist_fuel_commuter, dist_fuel_amtrak
  )

  ## Biodiesel-------------------------------------------------------------

  # Pull from EIA consumption data
  biodiesel <- national_ffc_data$us_consumption %>%
    # Biodiesel only
    filter(msn == "BDACB") %>%
    # Rename value as 'biodiesel' since it must be subtracted later
    select(year, biodiesel = value) %>%
    # Convert to millions of gallons. Convert NAs to zero
    mutate(biodiesel = if_else(is.na(biodiesel), 0, biodiesel * 42 * 1000))


  ## FHWA Dist Fuel by Vehicle Class---------------------------------------

  # FWHA Source: FHWA Annual Highway Statistics, Table VM-1.
  # https://www.fhwa.dot.gov/policyinformation/statistics.cfm


  # dist_fuel_by_class
  # NEED TO READ THIS TERRIBLE DATA FROM TERRIBLE FHWA SITE

  ## EIA Deisel Fuel---------------------------------------------------------

  # For each: com, ind, res, and tra

  us_consumption_dist_fuel <- national_ffc_data$us_consumption %>%
    filter(msn %in% c("DFRCB", "DFICB", "DFCCB", "DFACB")) %>%
    mutate(meta_sector = case_when(
      sector_description == "commercial sector" ~ "non-trans",
      sector_description == "industrial sector" ~ "non-trans",
      sector_description == "residential sector" ~ "non-trans",
      sector_description == "transportation sector" ~ "trans"
    ))

  ## Perform Diesel Adjustments-----------------------------------

  # Total dist fuel = cars, rails, and vessels minus biodiesel
  dist_fuel_excl_biodiesel <- dist_fuel_vessel %>%
    select(year, value) %>%
    # bind_rows(dist_fuel_by_class) %>%
    # bind_rows(dist_fuel_rail) %>%
    bind_rows(biodiesel) %>%
    group_by(year) %>%
    summarize(total_dist_fuel = sum(
      value,
      na.rm = TRUE
    ) - sum(
      biodiesel,
      na.rm = TRUE
    )) %>%
    ungroup() %>%
    # Join with heat content data for distillate fuel
    left_join(heat_content %>%
      # Distillate fuel only
      filter(str_detect(eia_description, "istillate")), by = "year") %>%
    # Convert to barrels and multiply by heat content to get mmbtu
    mutate(
      total_dist_fuel = (total_dist_fuel / 42) * heat_content,
      bottom_up_trans = total_dist_fuel / 1000
    )

  national_diesel <- us_consumption_dist_fuel %>%
    left_join(dist_fuel_excl_biodiesel %>%
      select(year, bottom_up_trans), by = "year") %>%
    mutate(
      bottom_up_nontrans = sum(value, na.rm = TRUE) - bottom_up_trans,
      .by = c(year, meta_sector)
    ) %>%
    mutate(
      bottom_up_total = sum(value, na.rm = TRUE) - bottom_up_trans,
      .by = c(year)
    ) %>%
    mutate(
      adjusted_value = ((value * 10^6) / (sum(
        value,
        na.rm = TRUE
      ) * bottom_up_total)) / 10^3,
      .by = c(year)
    )

  # Aggregate------------------------------------------------

  mobile_adjustments <- lst(national_mogas, national_diesel)

  
  return(mobile_adjustments)
}


# V. NATIONAL IBF DATA---------------------------------------------
get_ibf_adjustments_data <- function(national_ffc_data,
                                     scraped_data) {
  # Fuel Densities----------------------------------------------

  # These are fixed (I think)

  fuels <- tibble(
    fuel = c(
      "jet fuel", "JP8", "JP5", "JP4", "JAA", "JA1", "JAB",
      "distillate fuel", "commerce marine",
      "military marine", "residual fuel",
      "aviation gasoline", "intermediate fuel oil (IFO)"
    ),
    fuel_density = c(
      3.002, 3.04, 3.08, 2.90, 3.08, 3.04, NA, NA,
      3.1916, 3.18, 3.575, 2.72, 3.81
    )
  )

  # Military Jet Fuel---------------------------------------------

  # jet_fuel_military = sum of all jet fuels: JAB, JAA, JA1, JP4, JP5, JP8
  # For each, account for both Navy and Air Force (what about the Army?)

  # each fuel = millions of gallons * fuel_density * 1000000
  # jet_fuel_military = sum(all of these)

  # each fuel's gallons data = black box

  # Civilian Jet Fuel----------------------------------------------

  # data from Int'l Commercial Aviation (AEDT ) (TBTU)
  # "Personal Communciation" data ie black box


  # Marine Residual Fuel-------------------------------------------

  # residual_fuel_marine <- residual_fuel_all * heat_content   / 1000
  #
  # residual_fuel_all <- (residual_vessels_american + residual_vessels_foreign) *  42  /  1000

  # diesel_vessels_american & diesel_vessels_foreign are black box data

  # They're using a fixed value for heat content instead of the annually variable
  # Heat Content data. Need to look into that

  # Marine Distillate Fuel-----------------------------------------

  # TEMPORARY!!!!!!!!!!!!!!------------------------------------
  tra_temp <- read_xl_data("data/misc_tra_data_temporary.xlsx")[[1]] %>%
    filter(str_detect(source,"marine|jet")) %>%
    pivot_longer(cols = !source, names_to = "year", values_to = "ibf_value")
  
  ibf_jet_fuel_adj <- tra_temp %>% 
    filter(str_detect(source, "jet")) %>%
    group_by(year) %>%
    summarize(ibf_value = sum(ibf_value, na.rm = TRUE)) %>%
    ungroup()
  
  ibf_marine_residual_fuel_adj <- tra_temp %>% 
    filter(source == "resid_marine_com") %>%
    select(-source)
  
  ibf_marine_dist_fuel_adj <- tra_temp %>% 
    filter(source %in% 
             c("diesel_marine_mil", "diesel_marine_com")) %>%
    group_by(year) %>%
    summarize(ibf_value = sum(ibf_value, na.rm = TRUE)) %>%
    ungroup()
    

  # IBF Adjustments---------------------------------------------


  # jet_fuel_consumption <- jet_fuel_civilian + jet_fuel_military

  # Requires heat_content from jet fuel which is CONSTANT at 5.670
  # Correction: no, it doesn't need this
  # jet_fuel_heat_content <- 5.670

  # Aviation jet fuel (tbtu) = intl comm aviation (hard coded number in Transport workbook) +
  # military aircraft (hidden worksheet)

  # Marine residual fuel = ??? (hidden worksheet)

  # Marine distillate fuel = ??? (hidden worksheet)

  # Aggregate---------------------------------------------

  ibf_adjustments <- lst(
    ibf_jet_fuel_adj, ibf_marine_residual_fuel_adj,
    ibf_marine_dist_fuel_adj)

  return(ibf_adjustments)
}

# VI. NATIONAL DATA, MISC.----------------------------------------
get_misc_adjustments_data <- function(misc_corrections) {
  
   # National adjustments data-------------------------------------
  misc_adjustments <- misc_corrections$corrections %>%
    clean_names() %>%
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    pivot_longer(cols = starts_with("x"), 
                 names_to = "year", 
                 values_to = "value") %>%
    mutate(year = parse_number(year) %>% as_factor()) %>%
    pivot_wider(names_from = misc_adjustment, values_from = value)

  # Aggregate---------------------------------------------

  return(misc_adjustments)
}


# VII. NATIONAL DATA ADJUSTMENTS----------------------------
national_ffc_adjust_data <- function(
    # national_ffc_data,
                                     # general_data, 
                                     # mobile_adjustments,
                                     # ibf_adjustments,
                                     # misc_adjustments
                                     ) {
  # Collate National consumption data-------------------------------------
  
  # # Apply standardization function to national data
  # national_ffc_data$us_consumption <- 
  #   general_data$standardize_ffc(national_ffc_data$us_consumption, 
  #                                general_data$msn_names)
  # 
  # ## Residential, Commercial, & Electric Power----------------------------
  # 
  # # No adjustments EXCEPT dist fuel and mogas (see those scripts).
  # 
  # us_res_com_ele <- lst(
  #   res = national_ffc_data$us_consumption %>%
  #     filter(msn %in% c("CLRCB", "NNRCB", "DFRCB", "HLRCB", "KSRCB")),
  #   com = national_ffc_data$us_consumption %>%
  #     filter(msn %in% c(
  #       "CLCCB", "NNCCB", "DFCCB", "EMCCB", "HLCCB",
  #       "KSCCB", "MGCCB", "PCCCB", "RFCCB"
  #     )),
  #   ele = national_ffc_data$us_consumption %>% # NNEIB
  #     filter(msn %in% c("CLEIB", "NNEIB", "DKEIB", "PCEIB", "RFEIB")),
  # ) %>%
  #   # ISSUES 3/28/24
  #   # Electric power needs: distillate fuel
  #   # Need to adjust for distillate fuel oil (com & res, but not electric?)
  #   # need to adjust motor gas (com)
  #   # commercial has ethanol in the dataset but not in the spreadsheet
  # 
  #   # Collapse list into a single data frame
  #   list_rbind() %>%
  #   # create 'adjusted_value' column to match ind and tra data
  #   mutate(adjusted_value = value)
  # 
  # ## Industrial------------------------------------------------
  # 
  # # coking coal
  # 
  # us_ind <- lst(
  # 
  #   # Asphalt & Road Oil (NEU adjustment: 100%)
  #   asphalt = national_ffc_data$us_consumption %>%
  #     filter(msn == "ARICB") %>%
  #     # NEU adjustment is 100% of total
  #     mutate(adjusted_value = value - value),
  # 
  #   # Coking Coal (IPPU adjustment)
  #   coking_coal = national_ffc_data$us_consumption %>%
  #     # What is the MSN for coking coal?
  #     filter(msn == "CLKCB") %>%
  #     # Subtract IPPU adjustment
  #     left_join(misc_adjustments, by = "year") %>%
  #     mutate(
  #       adjusted_value = value - coking_coal_adj, 
  #       # Adjusted value no lower than zero
  #       adjusted_value = if_else(adjusted_value < 0, 0, adjusted_value)
  #     ),
  # 
  #   # Other Coal (NEU adjustment: Eastman Gas coal gasification;
  #   # synthetic natural gas adjustment, coking coal adjustment,
  #   # i & s adjustment
  #   other_coal = national_ffc_data$us_consumption %>%
  #     filter(msn == "CLCCB") %>%
  #     # Subtract adjustments
  #     left_join(misc_adjustments, by = "year") %>%
  #     # Subtract adjustments
  #     mutate(adjusted_value = value - 
  #              eastman_adj - 
  #              dakota_adj - 
  #              coking_coal_adj - 
  #              is_coal_adj 
  #     ),
  # 
  #   # Natural Gas (NEU adjustment: special; blast furnace adjustment,
  #   # coke oven adjustment, biogas adjustment,
  #   # ammonia adjustment, and i & s adjustment)
  #   # Supplemental gas already excluded
  #   natural_gas = national_ffc_data$us_consumption %>%
  #     filter(msn == "NNICB") %>%
  #     left_join(misc_adjustments, by = "year") %>%
  #     # Subtract adjustments
  #     mutate(adjusted_value = value -
  #              blast_furnace_adj - 
  #              coke_oven_adj - 
  #              biogas_adj - 
  #              ammonia_adj - 
  #              is_natgas_adj
  #     ),
  # 
  #   # Residual Fuel (carbon black adjustment)
  #   residual_fuel = national_ffc_data$us_consumption %>%
  #     filter(msn == "RFICB") %>%
  #     # Subtract carbon black adjustment
  #     left_join(misc_adjustments, by = "year") %>%
  #     mutate(
  #       adjusted_value = value - cb_residual_adj, 
  #       # Adjusted value no lower than zero
  #       adjusted_value = if_else(adjusted_value < 0, 0, adjusted_value)
  #     ),
  # 
  #   # Distillate Fuel (i&s adjustment, mogas/df adjustment)
  #   distillate_fuel = national_ffc_data$us_consumption %>%
  #     filter(msn == "DFICB") %>%
  #     # Subtract iron & steel adjustment
  #     left_join(misc_adjustments, by = "year") %>%
  #     mutate(adjusted_value = value - is_diesel_adj), 
  # 
  #   # Motor gasoline (mogas/df adjustment)
  #   motor_gasoline = national_ffc_data$us_consumption %>%
  #     filter(msn %in% c("MGICB", "EMICB")) %>%
  #     mutate(adjusted_value = value),
  # 
  #   # Kerosene (no adjustment)
  #   kerosene = national_ffc_data$us_consumption %>%
  #     filter(msn == "KSICB") %>%
  #     mutate(adjusted_value = value),
  # 
  #   # Petroleum Coke (NEU adjustment: special)
  #   petroleum_coke = national_ffc_data$us_consumption %>%
  #     filter(msn == "PCICB") %>%
  #     mutate(adjusted_value = value),
  # 
  #   # LPG (AKA Propane) (no adjustment)
  #   lpg = national_ffc_data$us_consumption %>%
  #     filter(msn == "combined lpg") %>%
  #     mutate(adjusted_value = value),
  # 
  #   # PQICB     PYICB (NEU adjustment: special)
  #   # Propane and Propylene: Included w/ HLICB ?
  # 
  #   # Lubricants (NEU adjustment: 100%)
  #   lubricants = national_ffc_data$us_consumption %>%
  #     filter(msn == "LUICB") %>%
  #     mutate(adjusted_value = value),
  # 
  #   # Misc Products (NEU adjustment: 100%)
  #   misc_products = national_ffc_data$us_consumption %>%
  #     filter(msn == "MSICB") %>%
  #     # NEU adjustment is 100% of total
  #     mutate(adjusted_value = value - value),
  # 
  #   # Naphtha (<401 deg. F) (NEU adjustment: 100%)
  #   naphtha = national_ffc_data$us_consumption %>%
  #     filter(msn == "FNICB") %>%
  #     # NEU adjustment is 100% of total
  #     mutate(adjusted_value = value - value),
  # 
  #   # Other Oil (>401 deg. F) (NEU adjustment: 100%)
  #   other_oil = national_ffc_data$us_consumption %>%
  #     filter(msn == "FOICB") %>%
  #     # NEU adjustment is 100% of total
  #     mutate(adjusted_value = value - value),
  # 
  #   # Pentanes Plus (NEU adjustment: special)
  #   pentanes_plus = national_ffc_data$us_consumption %>%
  #     filter(msn == "PPICB") %>%
  #     mutate(adjusted_value = value),
  # 
  #   # Still Gas (NEU adjustment: special)
  #   still_gas = national_ffc_data$us_consumption %>%
  #     filter(msn == "SGICB") %>%
  #     mutate(adjusted_value = value),
  # 
  #   # Special Naphtha (NEU adjustment: 100%)
  #   special_naphtha = national_ffc_data$us_consumption %>%
  #     filter(msn == "SNICB") %>%
  #     # NEU adjustment is 100% of total
  #     mutate(adjusted_value = value - value),
  # 
  #   # Waxes (NEU adjustment: 100%)
  #   waxes = national_ffc_data$us_consumption %>%
  #     filter(msn == "WXICB") %>%
  #     # NEU adjustment is 100% of total
  #     mutate(adjusted_value = value - value),
  # 
  #   # Unfinished Oils (no adjustment)
  #   unfinished_oils = national_ffc_data$us_consumption %>%
  #     filter(msn == "UOICB") %>%
  #     mutate(adjusted_value = value) 
  # ) %>%
  #   # Collapse list into a single data frame
  #   list_rbind() %>%
  #   # Convert coal to "industrial sector"
  #   mutate(sector_description = "industrial sector")
  # 
  # ## Transportation--------------------------------------------------
  # 
  # us_tra <- lst(
  # 
  #   # Coal
  #   coal = national_ffc_data$us_consumption %>%
  #     filter(msn == "CLACB"),
  #   
  #   # Natural Gas
  #   natural_gas = national_ffc_data$us_consumption %>%
  #     filter(msn == "NGACB"),
  #   
  #   # Lubricants (NEU adjustment)
  #   lubricants = national_ffc_data$us_consumption %>%
  #     filter(msn == "LUACB"),
  # 
  #   # Aviation Gasoline
  #   aviation_gasoline = national_ffc_data$us_consumption %>%
  #     filter(msn == "AVACB"),
  # 
  #   # Distillate Fuel (IBF adjustment, mogas/df adjustment)
  #   distillate_fuel = national_ffc_data$us_consumption %>%
  #     filter(msn == "DFACB") %>%
  #     left_join(ibf_adjustments$ibf_marine_dist_fuel_adj),
  # 
  #   # Jet Fuel (IBF adjustment)
  #   jet_fuel = national_ffc_data$us_consumption %>%
  #     filter(msn == "JFACB") %>%
  #     left_join(ibf_adjustments$ibf_jet_fuel_adj),
  # 
  #   # LPG (Propane) AKA HGL
  #   lpg = national_ffc_data$us_consumption %>%
  #     filter(msn == "HLACB"),
  # 
  #   # Motor Gasoline (mogas/df adjustment)
  #   aviation_gasoline = national_ffc_data$us_consumption %>%
  #     filter(msn == "MGACB"),
  # 
  #   # Residual Fuel (IBF adjustment)
  #   residual_fuel = national_ffc_data$us_consumption %>%
  #     filter(msn == "RFACB") %>%
  #     left_join(ibf_adjustments$ibf_marine_residual_fuel_adj) 
  # ) %>%
  #   # Collapse list into a single data frame
  #   list_rbind() %>%
  #   # Calculate adjusted value across all elements
  #   mutate(ibf_value = replace_na(ibf_value, 0), 
  #     adjusted_value = value - ibf_value)
  #   # mutate(adjusted_value = value - value), # NEU is 100% of lubricants
  # 
  # # Aggregate------------------------------------------------------
  # 
  # national_ffc_adjusted <- lst(us_res_com_ele, us_ind, us_tra) %>%
  #   bind_rows()
  
  # TEMPORARY. FOR 2023 INV (5/8/2025) ONLY
  national_ffc_adjusted <- read_csv("data/temporary_national_inv_data.csv") %>%
    pivot_longer(cols = !c(sector, source), names_to = "year") %>%
    rename(source_description = source, 
           sector_description = sector, 
           adjusted_value = value) 
  
}

# VIII. NATIONAL CO2 EMISSIONS------------------------------------
national_ffc_calculate_emissions <- function(national_ffc_adjusted,
                                             carbon_coefficients,
                                             general_data) {
  
  carbon_emissions_national <- national_ffc_adjusted %>%
    left_join(carbon_coefficients$carbon_factors,
      by = c("source_description", "year")
    ) %>%
    # MMT CO2  = btu * carbon factor/1000 * 44/12
    mutate(mmt_co2 = adjusted_value *
      (carbon_factor / 1000) * carbon_coefficients$carbon_ratio)

  # Apply labels to variables
  carbon_emissions_national <- general_data$apply_variable_labels(carbon_emissions_national, general_data$ghgi_variables)

  return(carbon_emissions_national)
}


# IX. STATE CONSUMPTION DATA-------------------------------------------

state_ffc_get_seds_data <- function(general_data) {
  # read SEDS data from EIA API file pulled with epa_api.R
  seds <- read_csv("data/api_seds.csv") %>%
    clean_names() %>%
    select(
      state, year, msn,
      value, unit
    ) %>%
    # filter(unit == "Billion Btu") %>%
    filter(msn %in% general_data$msn_names$msn_lookup) %>%
    left_join(general_data$msn_names$msn %>%
      select(-unit), by = "msn") %>%
    # Remove any duplicates caused by appending new annual data
    distinct() %>%
    # # Convert to millions of BTUs
    # mutate(value = value / 1000) %>%
    # Standarize names and combine LPGs
    general_data$standardize_ffc(general_data$msn_names)

  # Access SEDS data via EIA API
  # This script contains two options for retrieving SEDS data from the EIA API:
  # 1) retrieve entire dataset for all states + DC, 1990-present, inclusive;
  # 2) retrieve most recent year of data and append to the existing SEDS csv.

  # # API key generated 11/22/23
  key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"

  # Get most recent year
  latest_year <- year(Sys.Date()) - 2
  # Function for Options 1 & 2---------------------------------------------

  # Function to Query EIA API
  get_state_results <- function(state, year, offset) {
    # For now, to avoid exceeding the 5000-row data limit, we will pull
    # only one year and state per query. This requires n=51*years API queries.
    results <- paste0(
      "https://api.eia.gov/v2/seds/data/?frequency=annual",
      "&data[0]=value",
      "&facets[stateId][]=", state, # state input
      "&start=", year - 1, # start = previous year
      "&end=", year,
      "&sort[0][column]=period&sort[0][direction]=desc&offset=",
      offset, "&length=5000", # offset (usually 0)
      "&api_key=", key
    ) %>% # our API key is required
      GET() %>% # retrieve page from url
      content("raw") %>% # extract content as a raw vector
      rawToChar() %>% # convert to character data
      fromJSON() # convert from JSON to R object

    # The data limit from EIA's API is 5000 rows per query.
    # Here, we check the results to see if we exceeded that.
    # Extract warnings (if they exist)
    # limits <- pluck(results, "response", "warnings", "warning")
    # limits <- ifelse(!is_empty(limits),
    #                  paste0("data limit reached: ", limits), limits)
    # print(limits)

    # Check if data limit (5000 rows) was reached, ignoring empty values
    # limit_reached <- case_when(
    #   limits == "nothing" ~ FALSE,
    #   str_detect(limits, "incomplete return") ~ TRUE,
    #   .default = FALSE)
    # print(limit_reached)

    return(results)
  }

  ## Retrieve All SEDS Data-------------------------------------------

  # Apply API data query function across all states and years.
  # Using tic and toc() will indicate the time elapsed. Expected: about 18 min.
  tic()
  api_results <- expand_grid(
    state = general_data$ghgi_values$state[1:51],
    year = 1990:latest_year,
    offset = 0) %>%
    pmap(function(state, year, offset) get_state_results(state, year, offset))
  toc()

  seds <- api_results %>%
    map(\(.x) pluck(.x, "response", "data")) %>%
    list_rbind() %>%
      clean_names() %>%
      select( state = state_id, year = period, msn = series_id,
              value, unit) %>%
      filter(unit == "Billion Btu") %>%
      filter(msn %in% general_data$msn_names$msn_lookup) %>%
      mutate(unit = str_to_lower(unit),
             year = as.character(year)) %>%
      left_join(general_data$msn_names$msn %>%
                  select(-unit), by = "msn") %>%
      # Remove any duplicates caused by appending new annual data
      distinct() %>%
      # Convert to millions of BTUs
      mutate(value = as.numeric(value) / 1000)

  # Write data to csv file
  write_csv(seds, "data/api_seds.csv")


  return(seds)
}

# X. DATASCRAPING (NATIONAL AND STATE)-----------------------------------------

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


# XI. STATE ADJUSTMENTS DATA (COMBINED)-----------------------------
state_ffc_get_adjustments_data <- function(national_ffc_adjusted,
                                           international_bunker_fuels,
                                           misc_adjustments,
                                           non_energy_use,
                                           ippu_distributions,
                                           foks_diesel,
                                           foks_residual) {
  # NOTE------------------------------
  # This function currently exists in a hybrid form.
  # It reads necessary national data from csv/Excel, but
  # eventually it will get it directly from this pipeline.
  # Here is a framework for the pipeline code:

  # foks_diesel_distribution & foks_residual_distribution may be read from
  #   Excel sheet (see below) since they are no longer being updated

  
  
  
  # Adjustment factors data from national inventory------------
  
  # NOTE: MOGAS AND DIESEL VALUES ARE INCORRECT AT THIS TIME (2/27/2025)
  # if necessary we can pull calculated diesel/mogas values from national data 
  national_inv_adjustments <- national_ffc_adjusted %>% 
    select(year, 
           sector_description, 
           source_description, 
           national_value = adjusted_value)
  

  # IBF adjustments data--------------------------------------------

  # Read in IBF data from FFC excel workbook. Only need one line:
  ibf_adjustments <- international_bunker_fuels$ibf %>%
    clean_names() %>%
    # Make data long; i.e., one row per year
    pivot_longer(cols = -1, names_to = "year", values_to = "ibf_value") %>%
    # Rename source column
    rename(source_description = gas_mode_and_fuel_type) %>%
    # Remove letters from 'year' column and standardize source descriptions
    mutate(
      year = str_remove(year, "[a-z]"),
      source_description = str_to_lower(source_description), 
      source_description = case_when(
        # In the IBF worksheet, jet fuel is listed as "aviation jet fuel"
        str_detect(source_description, "jet") ~ "jet fuel",
        str_detect(source_description, "distillate") ~ "distillate fuel oil",
        str_detect(source_description, "residual") ~ "residual fuel oil"
      )
    )

  # NEU data---------------------------------------------------------
  # Read in NEU data from FFC excel workbook
  neu_adjustments <- non_energy_use$neu %>%
    clean_names() %>%
    # Rename to match column names in SEDS
    rename(source_description = sector_fuel_type) %>%
    # Add sector description based on source_description text
    mutate(sector_description = case_when(
      source_description == "Transportation" ~ "transportation sector",
      source_description == "Industry" ~ "industrial sector",
      .default = NA_character_
    )) %>%
    # Fill sector_description empty values from previous entry
    fill(sector_description, .direction = "down") %>%
    # Move sector_description to the first column in order to pivot
    relocate(sector_description) %>%
    # Make data long; i.e., one row per year
    pivot_longer(
      cols = -c(1, 2), names_to = "year",
      values_to = "neu_factor"
    ) %>%
    # Remove letters from year column
    mutate(
      year = str_remove(year, "[a-z]"),
      # Make source lowercase
      source_description = str_to_lower(source_description) %>%
        # Remove asterisks and the word "industrial" from source
        str_remove_all("\\*|industrial") %>%
        # Remove extra spaces from source
        str_squish(), 
      source_description = if_else(source_description == "hgl", 
                                   "hydrocarbon gas liquids", source_description))

  # I & S distributions data--------------------------------------------

  # Read in I & S data from FFC excel workbook
  is_distribution <- ippu_distributions$iron_and_steel %>%
    clean_names() %>%
    # Don't need the national value; we compute it below
    filter(state != "National") %>%
    # Keep only state codes and value by year
    select(state, starts_with("x")) %>%
    # Make data long; i.e., one row per year
    pivot_longer(
      cols = -1, names_to = "year",
      values_to = "is_percent"
    ) %>%
    # Remove letters from year column
    mutate(
      year = str_remove(year, "[a-z]"),
      # Get national total for each year by insta-grouping
      national_total = sum(is_percent), .by = year
    ) %>%
    # Get I & S percentage for each state
    mutate(is_percent = is_percent / national_total) %>%
    # No longer need national total
    select(-national_total)

  # Read in ammonia data from FFC excel workbook
  ammonia_distribution <- ippu_distributions$ammonia %>%
    clean_names() %>%
    # Don't need the national value; we compute it below
    filter(state != "National") %>%
    # Keep only state codes and value by year
    select(state, starts_with("x")) %>%
    # Make data long; i.e., one row per year
    pivot_longer(
      cols = -1, names_to = "year",
      values_to = "ammonia_percent"
    ) %>%
    # Remove letters from year column
    mutate(
      year = str_remove(year, "[a-z]"),
      # Get national total for each year by insta-grouping
      national_total = sum(ammonia_percent), .by = year
    ) %>%
    # Get ammonia percentage for each state
    mutate(ammonia_percent = ammonia_percent / national_total) %>%
    # No longer need national total
    select(-national_total)

  # Read in petrochemical carbon black data from FFC excel workbook
  petrochemicals_distribution <- ippu_distributions$petrochemical %>%
    clean_names() %>%
    # Don't need the national value; we compute it below
    filter(state != "National") %>%
    # Keep only state codes and value by year
    select(state, starts_with("x")) %>%
    # Make data long; i.e., one row per year
    pivot_longer(
      cols = -1, names_to = "year",
      values_to = "petrochemical_percent"
    ) %>%
    # Remove letters from year column
    mutate(
      year = str_remove(year, "[a-z]"),
      # Get national total for each year by insta-grouping
      national_total = sum(petrochemical_percent), .by = year
    ) %>%
    # Get petrochemical percentage for each state
    mutate(
      petrochemical_percent =
        petrochemical_percent / national_total
    ) %>%
    # No longer need national total
    select(-national_total)

  # Read in petrochemical carbon black data from FFC excel workbook
  petrochemicals_cb_distribution <- ippu_distributions$carbon_black %>%
    clean_names() %>%
    # Don't need the national value; we compute it below
    filter(state != "National") %>%
    # Keep only state codes and value by year
    select(state, starts_with("x")) %>%
    # Make data long; i.e., one row per year
    pivot_longer(
      cols = -1, names_to = "year",
      values_to = "petrochemical_cb_percent"
    ) %>%
    # Remove letters from year column
    mutate(
      year = str_remove(year, "[a-z]"),
      # Get national total for each year by insta-grouping
      national_total = sum(petrochemical_cb_percent), .by = year
    ) %>%
    # Get petrochemical percentage for each state
    mutate(
      petrochemical_cb_percent =
        petrochemical_cb_percent / national_total
    ) %>%
    # No longer need national total
    select(-national_total)

  # Read in diesel fuel data from FOKS excel workbook
  foks_diesel_distribution <- foks_diesel %>%
    clean_names() %>%
    # Make data long; i.e., one row per year
    pivot_longer(
      cols = -1, names_to = "year",
      values_to = "diesel_percent"
    ) %>%
    # Remove letters from year column
    mutate(
      year = str_remove(year, "[a-z]") %>% 
        as_factor())

  # Append Extrapolated Data for 2021 Onward

  # FOKS data unavailable after 2020. Extrapolate using 2020 data
  # 2021 extrapolated data
  foks_diesel_distribution <- foks_diesel_distribution %>%
    bind_rows(foks_diesel_distribution %>% filter(year == "2020") %>%
                mutate(year = "2021")) %>%
    # 2022 extrapolated data
    bind_rows(foks_diesel_distribution %>% filter(year == "2020") %>%
                mutate(year = "2022")) %>%
    # 2022 extrapolated data
    bind_rows(foks_diesel_distribution %>% filter(year == "2020") %>%
                mutate(year = "2023"))

  # Read in residual fuel data from FOKS excel workbook
  foks_residual_distribution <- foks_residual %>%
    clean_names() %>%
    # Make data long; i.e., one row per year
    pivot_longer(
      cols = -1, names_to = "year",
      values_to = "residual_percent"
    ) %>%
    # Remove letters from year column
    mutate(
      year = str_remove(year, "[a-z]") %>% 
        as_factor())

  # Append Extrapolated Data for 2021 Onward

  # FOKS data unavailable after 2020. Extrapolate using 2020 data
  # 2021 extrapolated data
  foks_residual_distribution <- foks_residual_distribution %>%
    bind_rows(foks_residual_distribution %>% filter(year == "2020") %>%
      mutate(year = "2021")) %>%
    # 2022 extrapolated data
    bind_rows(foks_residual_distribution %>% filter(year == "2020") %>%
      mutate(year = "2022")) %>%
    bind_rows(foks_residual_distribution %>% filter(year == "2020") %>%
                mutate(year = "2023")) 
  
  # TEMPORARY--this will come from national data
  feedstock_export_adjustments <- read.csv("data/feedstock_export_adjustments.csv") %>%
    pivot_longer(cols = !source_description, 
                 names_to = "year", 
                 values_to = "feedstock_adjustment") %>%
    mutate(source_description = str_to_lower(source_description), 
           sector_description = "industrial sector",
           year = parse_number(year) %>% as_factor())

  # Aggregate------------------------------------------------------

  state_adjustments <- lst(
    misc_adjustments,
    national_inv_adjustments,
    ibf_adjustments,
    neu_adjustments,
    is_distribution,
    ammonia_distribution,
    petrochemicals_distribution,
    petrochemicals_cb_distribution,
    foks_diesel_distribution,
    foks_residual_distribution,
    feedstock_export_adjustments
  )

  return(state_adjustments)
}


# XII. TERRITORIES CONSUMPTION DATA---------------------------------------
# The procedure for retrieving and collating the FFC data for US territories
# differs from the procedure for states.
get_territories_data <- function(carbon_coefficients,
                                 general_data) {
  # API Key----------------------------------------------------------------

  # API key generated 11/22/23
  key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"


  # Retrieve Territories FF Consumption from EIA---------------------------

  # Get latest data year
  latest_year <- year(now()) - 1

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
    "&api_key=", key
  ) %>% # our API key is required
    GET() %>% # retrieve page from url
    content("raw") %>% # extract content as a raw vector
    rawToChar() %>% # convert to character data
    fromJSON() # convert from JSON to R object


  ff_territories <- api_territories %>%
    map(\(.x) pluck(.x, "data")) %>%
    list_rbind() %>%
    select(
      state = countryRegionId, year = period, state_name = countryRegionName,
      value, unit = unitName, source = productId, dataFlagDescription,
      source_description = productName
    ) %>%
    mutate(
      value = as.numeric(value),
      source_description = str_to_lower(source_description),
      source_description = case_when(
        source_description == "liquefied petroleum gases" ~ "lpg",
        source_description == "dry natural gas" ~ "natural gas",
        # We need lubricants data, but it's not listed in EIA
        # source_description == "????" ~ "lubricants",
        .default = source_description
      )
    )



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
    key
  ) %>%
    GET() %>% # retrieve page from url
    content("raw") %>% # extract content as a raw vector
    rawToChar() %>% # convert to character data
    fromJSON() %>% # convert from JSON to R object
    pluck("response", "data") %>%
    select(
      year = period, # msn,
      msn_description = seriesDescription, heat_content = value
    ) %>%
    # Make heat content value numeric
    mutate(
      heat_content = as.numeric(heat_content),
      source_description = case_when(
        str_detect(msn_description, "Motor") ~ "motor gasoline",
        str_detect(msn_description, "Natural Gas") ~ "natural gas"
      )
    ) %>%
    filter(!is.na(source_description)) %>%
    select(-msn_description)

  # Get resid fuel, kerosene, jet fuel, lubricants, dist fuel (use high sulfur),
  # other petrol liquids, and LPG (average of 9 HGLs)
  heat_commodities <- c(
    "Residual Fuel Oil", "Kerosene",
    "Jet Fuel, Kerosene Type", "Lubricants",
    "than 500 ppm sulfur", "Miscellaneous Products",
    "Ethane", "Propane", "Normal Butane", "Isobutane",
    "Ethylene", "Propylene", "Butylene",
    "Isobutylene", "Pentanes Plus"
  )

  eia_table_a1 <- pdf_text(
    "https://www.eia.gov/totalenergy/data/monthly/pdf/sec12_2.pdf"
  ) %>%
    str_remove_all("[()]")

  heat_content_territories <- heat_commodities %>%
    map(\(.x) str_match_all(
      eia_table_a1,
      pattern = paste0(.x, "\\s*(\\d+(?:\\.\\d+)?)")
    ) %>%
      pluck(1, 2) %>%
      tibble(source_description = .x, heat_content = .)) %>%
    list_rbind() %>%
    mutate(
      heat_content = as.numeric(heat_content),
      source_description = str_replace_all(
        source_description, "than 500 ppm sulfur", "Distillate Fuel Oil"
      ),
      source_description = str_remove_all(
        source_description, ", Kerosene Type"
      ),
      source_description = if_else(
        source_description %in% c(
          "Ethane", "Propane",
          "Normal Butane", "Isobutane",
          "Ethylene", "Propylene",
          "Butylene", "Isobutylene",
          "Pentanes Plus"
        ),
        "lpg", source_description
      )
    ) %>%
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
      .default = source_description
    ))


  # Calculcate TBtu---------------------------------------------------------

  ffc_territories <- ff_territories %>%
    left_join(heat_content_territories,
      by = c("year", "source_description")
    ) %>%
    # time = 365 days for coal and nat gas, 1 year for everything else
    mutate(
      time_units = case_when(
        source_description %in% c("coal", "natural gas") ~ 1,
        .default = 365
      ),
      # TBtu = consumption * time * 1000 * heat content / 1,000,000
      tbtu = value * time_units * heat_content / 1000
    ) %>%
    select(-unit, -dataFlagDescription, -value, -source)



  # Calculate Carbon Eq Emissions------------------------------------------

  # need to figure out which carbon factors to use

  carbon_emissions_territories <- ffc_territories %>%
    left_join(carbon_coefficients$carbon_factors,
      by = c("source_description", "year")
    ) %>%
   # Fill in missing carbon factors for coal and other petroleum
    mutate(carbon_factor = case_when(
      source_description == "other petroleum liquids" ~ 20.0,
      source_description == "coal" ~ 25.1,
      .default = carbon_factor
    ), 
           # MMT CO2  = btu * carbon factor/1000 * 44/12
      mmt_co2 = tbtu / 1000 * carbon_factor * carbon_coefficients$carbon_ratio)
  
  # Apply labels to variables
  carbon_emissions_territories <- general_data$apply_variable_labels(
    carbon_emissions_territories,
    general_data$ghgi_variables
  )

  # For Vince's spreadsheet------------------------------------------------

  territories_csv_format <- ff_territories %>%
    mutate(value = round(value, 4)) %>%
    arrange(year) %>%
    arrange(source_description, state) %>%
    select(-dataFlagDescription, -source, -state, -unit) %>%
    group_by(state_name, source_description, year) %>%
    pivot_wider(names_from = year, names_prefix = "y")

  write_csv(territories_csv_format, "territories_csv_format.csv")

  return(carbon_emissions_territories)
}


# XIII. STATE DATA ADJUSTMENTS---------------------------
state_ffc_adjust_data <- function(seds,
                                  state_adjustments,
                                  scraped_data,
                                  general_data) {
  
  
  seds <- general_data$standardize_ffc(seds, general_data$msn_names) %>%
    filter(year != "1989")
  
  ## Residential------------------------------------------------

  seds_res_adjusted <- lst(
    coal = seds %>%
      filter(msn == "CLRCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
      # Get sum of all states' value
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Deal with zeroes in the sums to avoid NaNs
      mutate(
        states_sum_value = if_else(
          states_sum_value == 0, 1, states_sum_value
        ),
        # Multiply adjustment factor by states's value / the above sum
        adjusted_value = national_value *
          (value / states_sum_value)
      ),
    
    natural_gas = seds %>%
      # Separate list element required to find net natural gas
      filter(msn %in% c("NGRCB", "SFRCB")) %>%
      # Subtract supplemental gas from total natural gas
      mutate(value = abs(diff(value)), .by = c(state, year)) %>%
      # Group_size shows that each group has exactly two rows. Good!
      # Supplemental gas no longer needed (and value is now duplicative)
      filter(msn != "SFRCB") %>%
      # Join with the adjustment factor data (from national inventory)
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      mutate(
        msn = "net natural gas",
        source_description = "natural gas"
      ) %>%
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
      # Get sum of all states' value
      mutate(states_sum_value = sum(value), .by = c(year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = national_value *
        (value / states_sum_value)),
    
    distillate_fuel = seds %>%
      filter(msn == "DFRCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
      # Get sum of all states' value
      mutate(states_sum_value = sum(value), .by = year) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = national_value *
        (value / states_sum_value)),

    # LPGs (propane and/or HGL)
    lpg = seds %>%
      filter(case_when(
        as.integer(year) < 2010 ~ msn == "HLRCB",
        as.integer(year) >= 2010 ~ msn == "PQRCB"
      )) %>%
      # Adjusted = original value
      mutate(
        msn = "combined lpg",
        # convert units
        adjusted_value = value 
      ),

    # All other sources go in the last list element
    other_residential = seds %>%
      filter(msn %in% c("KSRCB")) %>%
      # convert units
      mutate(adjusted_value = value)
  ) %>%
    # Collapse list into a single data frame
    list_rbind()

  ## Commercial---------------------------------------------------

  seds_com_adjusted <- lst(
    coal = seds %>%
      filter(msn == "CLCCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
      # Get sum of all states' value
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = national_value *
        (value / states_sum_value)),
    distillate_fuel = seds %>%
      filter(msn == "DFCCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
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
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      mutate(
        msn = "net natural gas",
        source_description = "natural gas"
      ) %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
      # Get sum of all states' value
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = national_value *
        (value / states_sum_value)),
    
    gasoline = seds %>%
      # Separate list element required to find net gasoline
      filter(msn %in% c("MGCCB", "EMCCB")) %>%
      # Subtract ethanol from total gasoline
      mutate(value = abs(diff(value)), .by = c(state, year)) %>%
      # Group_size shows that each group has exactly two rows. Good!
      # Ethanol no longer needed (and value is now duplicative)
      filter(msn != "EMCCB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
      # Get sum of all states' value
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = national_value *
        (value / states_sum_value)) %>%
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      mutate(
        msn = "net gasoline",
        source_description = "motor gasoline"
      ),

    # LPGs (propane and/or HGL)
    lpg = seds %>%
      filter(case_when(
        as.integer(year) < 2010 ~ msn == "HLCCB",
        as.integer(year) >= 2010 ~ msn == "PQCCB"
      )) %>%
      # Adjusted = original value
      mutate(
        msn = "combined lpg",
        # convert units
        adjusted_value = value
      ),

    # All other sources go in the last list element
    other_commercial = seds %>%
      filter(msn %in% c("KSCCB", "PCCCB", "RFCCB")) %>%
      # convert units
      mutate(adjusted_value = value)
  ) %>%
    # Collapse list into a single data frame
    list_rbind()

  ## Industrial---------------------------------------------------

  seds_ind_adjusted <- lst(

    # Coking Coal
    coking_coal = seds %>%
      filter(msn == "CLKCB") %>%
      # Join with national data adjustments
      left_join(state_adjustments$misc_adjustments, by = "year") %>%
      mutate(
        states_sum_value = sum(value), .by = c(msn, year),
        # adjusted value = value * adjustment or zero, whichever is larger
        adjusted_value = value * pmax(0, (1 - coking_coal_adj / states_sum_value))
      ),

    # Other Coal
    other_coal = seds %>%
      filter(msn == "CLOCB") %>%
      left_join(
        coking_coal %>%
          select(
            sum_coking_coal = states_sum_value,
            coking_coal_value = value, 
            year, state
          ),
        by = c("year", "state")
      ) %>%
      # Join with national data adjustments
      left_join(state_adjustments$misc_adjustments, by = "year") %>%
      # Join with national data
      left_join(state_adjustments$national_inv_adjustments,
        by = c("year", "source_description", "sector_description")
      ) %>%
      # Join with I & S distribution data
      left_join(state_adjustments$is_distribution, by = c("state", "year")) %>%
      mutate(
        coke_factor = if_else(coking_coal_adj < sum_coking_coal, 0,
                              coking_coal_adj - sum_coking_coal
        ),
        other_coal_coke_adj =
          coke_factor * (coking_coal_value / sum_coking_coal),
        # SNG adjustment for North Dakota only
        sng_dakota_adj = if_else(state == "ND", dakota_adj, 0),
        # Multiply I & S factor by I & S state distribution percentages
        other_coal_is_adj = is_coal_adj * is_percent,
        adjusted_value_pre = value -
          other_coal_coke_adj -
          sng_dakota_adj -
            other_coal_is_adj) %>%
      # Get sum of all states' adjusted values
      mutate(
        states_sum_value = sum(adjusted_value_pre),
        .by = c(msn, year)
      ) %>%
      mutate(
        adjusted_value =
          (adjusted_value_pre / states_sum_value) * national_value
      ),

    # Natural Gas
    natural_gas = seds %>%
      # Separate list element required to find net natural gas
      filter(msn %in% c("NGICB", "SFINB")) %>%
      # Subtract supplemental gas from total natural gas
      mutate(value = abs(diff(value)), .by = c(state, year)) %>%
      # Group_size shows that each group has exactly two rows. Good!
      # Supplemental gas no longer needed (and value is now duplicative)
      filter(msn != "SFINB") %>%
      # Join with national data adjustments
      left_join(state_adjustments$misc_adjustments, by = "year") %>%
      # Change source and MSN to reflect that it's net natural gas
      mutate(
        source_description = "natural gas",
        msn = "net natural gas"
      ) %>%
      # Join with national data
      left_join(state_adjustments$national_inv_adjustments,
        by = c("year", "source_description", "sector_description")
      ) %>%
      # Join with I & S distribution data
      left_join(state_adjustments$is_distribution,
        by = c("state", "year")
      ) %>%
      # Join with ammonia distribution data
      left_join(state_adjustments$ammonia_distribution,
        by = c("state", "year")
      ) %>%
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      #  Ammonia factor * ammonia distribution = ammonia adjusted value
      mutate(
        natural_gas_ammonia_adj = ammonia_adj *
          ammonia_percent,
        #  I & S factor * I & S distribution = I & S adjusted value
        natural_gas_is_adj = is_natgas_adj * is_percent,
        adjusted_value_pre = value -
          (natural_gas_ammonia_adj + natural_gas_is_adj)
      ) %>%
      # Get sum of all states' adjusted (preliminary) values
      mutate(
        states_sum_value = sum(adjusted_value_pre),
        .by = c(msn, year)
      ) %>%
      # adj value / sum of all states' values * consumption = adjusted value
      mutate(
        adjusted_value =
          (adjusted_value_pre / states_sum_value) *
            national_value
      ),

    # Residual Fuel
    residual_fuel = seds %>%
      filter(msn == "RFICB") %>%
      # Join with national data adjustments
      left_join(state_adjustments$misc_adjustments, by = "year") %>%
      # Join with national data
      left_join(state_adjustments$national_inv_adjustments,
        by = c("year", "source_description", "sector_description")
      ) %>%
      # Join with petrochemicals carbon black distribution data
      left_join(state_adjustments$petrochemicals_cb_distribution,
        by = c("state", "year")
      ) %>%
      # adjust for cb factor = cb_factor * petrochem cb distribution
      # adjusted value = value - cb adjusted value (minimum = 0)
      mutate(residual_fuel_cb_adj = if_else(
        value - (cb_residual_adj * petrochemical_cb_percent) < 0, 0,
        value - (cb_residual_adj * petrochemical_cb_percent)
      )) %>%
      # Get sum of all states' cb adjusted values
      mutate(
        states_sum_value = sum(residual_fuel_cb_adj),
        .by = c(msn, year)
      ) %>%
      # now adjust by consumption value
      mutate(
        adjusted_value =
          national_value * (residual_fuel_cb_adj / states_sum_value)
      ),

    # Distillate Fuel
    distillate_fuel = seds %>%
      filter(msn == "DFICB") %>%
      # Join with national data adjustments
      left_join(state_adjustments$misc_adjustments, by = "year") %>%
      # Join with national data
      left_join(state_adjustments$national_inv_adjustments,
        by = c("year", "source_description", "sector_description")
      ) %>%
      # Join with I & S distribution data
      left_join(state_adjustments$is_distribution, by = c("state", "year")) %>%
      # distillate_fuel_is_adj (if negative, then 0)
      mutate(distillate_fuel_is_adj = if_else(
        value - (is_diesel_adj * is_percent) < 0, 0,
        value - (is_diesel_adj * is_percent)
      )) %>%
      # Get sum of all states' cb adjusted values
      mutate(
        states_sum_value = sum(distillate_fuel_is_adj),
        .by = c(msn, year)
      ) %>%
      # now adjust by consumption value
      mutate(
        adjusted_value =
          national_value *
            (distillate_fuel_is_adj / states_sum_value)
      ),

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
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
      # Rename adjustment factor for clarity
      rename(motor_gas_factor = national_value) %>%
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      mutate(
        msn = "net gasoline",
        source_description = "motor gasoline",
        # adjusted net gasoline = motor gas factor * gasoline - sum
        adjusted_value = motor_gas_factor * (value / states_sum_value)
      ),

    # Petroleum Coke
    petroleum_coke = seds %>%
      filter(msn == "PCICB") %>%
      # Get sum of all states' petroleum_coke
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # get adjustment factor for petroleum coke
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
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
      mutate(source_description = "hydrocarbon gas liquids") %>%
      # Join with adjustments to get adjustment factor
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
      # Rename adjustment factor for clarity
      rename(ind_lpg_factor = national_value) %>%
      # Get sum of all states' lpg
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Rename MSN & source and calculate adjusted value
      mutate(
        msn = "combined lpg",
        adjusted_value = ind_lpg_factor * (value / states_sum_value)
      ),
    # ind_lpg_factor = US SEDS Total--LPG (state's HLICB - PPICB)

    # All other sources go in this list element
    other_industrial = seds %>%
      filter(msn %in% c(
        "ARICB", "KSICB", "LUICB", "ABICB", "COICB",
        "MBICB", "MSICB", "FNICB", "FOICB", "PPICB",
        "SGICB", "SNICB", "UOICB", "WXICB", "PQICB",
        "PYICB", "EQICB", "EYICB", "BQICB", "BYICB",
        "IQICB", "IYICB"
      )) %>%
      # convert units
      mutate(adjusted_value = value)
  ) %>%
    # Collapse list into a single data frame
    list_rbind()
  # BTW: distillate_fuel_is_adj is required for neu_adjustments

  ## Transportation-----------------------------------------

  seds_tra_adjusted <- lst(
    distillate_fuel = scraped_data$diesel_distribution %>%
      # Join with adjustments data
      left_join(
        state_adjustments$national_inv_adjustments %>%
          # Can only join by 'year', so a filter is required
          filter(
            source_description == "distillate fuel oil",
            sector_description == "transportation sector"
          ),
        by = c("year")
      ) %>%
      mutate(
        adjusted_value = national_value * diesel_percent,
        msn = "DFACB"
      ),
    
    gasoline = scraped_data$gasoline_distribution %>%
      # Join with adjustments data
      left_join(
        state_adjustments$national_inv_adjustments %>%
          # Can only join by 'year', so a filter is required
          filter(
            source_description == "motor gasoline",
            sector_description == "transportation sector"
          ),
        by = c("year")
      ) %>%
      mutate(
        adjusted_value = national_value * gasoline_percent,
        msn = "net gasoline"
      ),
    
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
      filter(case_when(
        as.integer(year) < 2010 ~ msn == "HLACB",
        as.integer(year) >= 2010 ~ msn == "PQACB"
      )) %>%
      # Adjusted = original value
      mutate(
        msn = "combined lpg",
        # convert units
        adjusted_value = value
      ),
    
    aviation_gasoline = seds %>%
      filter(msn == "AVACB") %>%
      # Adjusted = original value
      # Convert units
      mutate(adjusted_value = value),
    
    natural_gas = seds %>%
      filter(msn == "NGACB") %>%
      # Join with the adjustment factor data (from national inventory)
      mutate(source_description = "natural gas") %>%
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
      # rename for clarity
      rename(natural_gas_factor = national_value) %>%
      # Get sum of all states' value
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states' value / the above sum
      mutate(
        adjusted_value = natural_gas_factor *
          (value / states_sum_value))
  ) %>%
    # Collapse list into a single data frame
    list_rbind()

  ## Electric Power---------------------------------------------------

  seds_ele_adjusted <- lst(
    coal = seds %>%
      filter(msn == "CLEIB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
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
      # Change MSN identifier. old MSN distinction no longer needed(?)
      # However, MSN can be reconstituted from other _code fields if needed.
      mutate(
        msn = "net natural gas",
        source_description = "natural gas"
      ) %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
      # Rename for clarity
      rename(natural_gas_factor = national_value) %>%
      # Get sum of all states' value
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = natural_gas_factor *
        (value / states_sum_value)),
    
    geothermal = seds %>%
      filter(msn == "GETCB") %>%
      mutate(sector_description = "electric power sector") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(state_adjustments$national_inv_adjustments,
                by = c("source_description", "year", "sector_description")
      ) %>%
      # Rename for clarity
      rename(geothermal_factor = national_value) %>%
      # Get sum of all states' value
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = geothermal_factor *
               (value / states_sum_value)),
    
    
    residual_fuel = seds %>%
      filter(msn == "RFEIB") %>%
      # Convert units
      mutate(adjusted_value = value),
    petroleum_coke = seds %>%
      filter(msn == "PCEIB") %>%
      mutate(adjusted_value = value),
    distillate_fuel = seds %>%
      filter(msn == "DFEIB") %>%
      # Join with the adjustment factor data (from national inventory)
      left_join(state_adjustments$national_inv_adjustments,
        by = c("source_description", "year", "sector_description")
      ) %>%
      # Rename for clarity
      rename(distillate_fuel_factor = national_value) %>%
      # Get sum of all states' value
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(adjusted_value = distillate_fuel_factor *
        (value / states_sum_value))
  ) %>%
    # Collapse list into a single data frame
    list_rbind()

  ## IBF--------------------------------------------------------------

  seds_ibf_adjusted <- lst(
    distillate_fuel = state_adjustments$foks_diesel_distribution %>%
      left_join(
        state_adjustments$ibf_adjustments %>% filter(
          source_description == "distillate fuel oil"
        ),
        by = "year"
      ) %>%
      # Calculate adjusted value (factor * percent)
      mutate(
        ibf_adjusted_value = ibf_value * diesel_percent,
        # Add the MSN & sector for transportation distillate fuel
        msn = "DFACB",
        sector_description = "transportation sector"
      ),
    
    residual_fuel = state_adjustments$foks_residual_distribution %>%
      left_join(
        state_adjustments$ibf_adjustments %>% filter(
          source_description == "residual fuel oil"
        ),
        by = "year"
      ) %>%
      # Calculate adjusted value (factor * percent)
      mutate(
        ibf_adjusted_value = ibf_value * residual_percent,
        # Add the MSN & sector for transportation residual fuel
        msn = "RFACB",
        sector_description = "transportation sector"
      ),
    
    jet_fuel = seds %>%
      filter(msn == "JFACB") %>%
      left_join(state_adjustments$ibf_adjustments,
        by = c("year", "source_description")
      ) %>%
      # Get sum of all states' value
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Multiply adjustment factor by states's value / the above sum
      mutate(ibf_adjusted_value = ibf_value *
        (value / states_sum_value))
  ) %>%
    # Collapse list into a single data frame
    list_rbind() %>%
    # Remove nonessential columns to simplify joins in state_breakouts.R
    select(
      state, year, sector_description, source_description,
      msn, ibf_adjusted_value
    )

  ## NEU-------------------------------------------------------------

  seds_neu_adjusted <- lst(

    # Other coal
    # NEU adjustment applies to Tennessee only (Eastman Gas Plant)
    other_coal = state_adjustments$neu_adjustments %>%
      filter(
        source_description == "other coal",
        sector_description == "industrial sector"
      ) %>%
      mutate(
        neu_adjusted_value = neu_factor,
        # Add industrial other coal MSN
        msn = "CLOCB",
        # Tennessee only
        state = "TN"
      ),

    # Natural gas
    natural_gas = state_adjustments$neu_adjustments %>%
      # Natural gas has a very long source name in the NEU data
      filter(str_detect(source_description, "natural gas")) %>%
      left_join(state_adjustments$petrochemicals_distribution, by = "year") %>%
      # NEU value = NEU factor * distribution (will be zero for most states)
      mutate(
        neu_adjusted_value = neu_factor * petrochemical_percent,
        # Standardize MSN & source to match seds_ind_adjusted
        msn = "net natural gas",
        source_description = "natural gas"
      ),

    # Distillate fuel
    distillate_fuel = seds_ind_adjusted %>%
      filter(msn == "DFICB") %>%
      # Join with NEU adjustments data
      left_join(state_adjustments$neu_adjustments,
        by = c("year", "source_description", "sector_description")
      ) %>%
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
      mutate(source_description = "hydrocarbon gas liquids") %>%
      # Join with neu adjustments to get neu factor
      left_join(state_adjustments$feedstock_export_adjustments,
        by = c("year", "source_description", "sector_description")
      ) %>%
      # Get sum of all states' lpg
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Rename MSN and calculate adjusted value
      mutate(
        msn = "combined lpg",
        neu_adjusted_value = feedstock_adjustment * (value / states_sum_value)
      ),

    # Pentanes plus
    pentanes_plus = seds %>%
      filter(msn == "PPICB") %>%
      # Join with neu adjustments to get neu factor
      left_join(state_adjustments$neu_adjustments,
        by = c("year", "source_description", "sector_description")
      ) %>%
      # Get sum of all states' pentanes plus
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Calculate adjusted value
      mutate(neu_adjusted_value = neu_factor * (value / states_sum_value)),

    # Petroleum coke
    petroleum_coke = seds %>%
      filter(msn == "PCICB") %>%
      # Join with neu adjustments to get neu factor
      left_join(state_adjustments$neu_adjustments,
        by = c("year", "source_description", "sector_description")
      ) %>%
      # Pet Coke threshold to adjust petroleum coke
      left_join(state_adjustments$misc_adjustments %>%
                  select(year, starts_with("pet_")) %>%
                  mutate(pet_coke_adj = pet_coke_aluminum_adj  + 
                           pet_coke_ferroalloys_adj + 
                           pet_coke_titanium_adj + 
                           pet_coke_ammonia_adj + 
                           pet_coke_silicon_carbide_adj) %>%
                  select(year, pet_coke_adj), 
                by = "year") %>%
      # Get sum of all states' petroleum coke
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Calculate adjusted value
      mutate(neu_adjusted_value = neu_factor * (value / states_sum_value), 
             neu_adjusted_value = pmax(0, neu_adjusted_value + (-pet_coke_adj * (value / states_sum_value)))
             ),

    # Still gas
    still_gas = seds %>%
      filter(msn == "SGICB") %>%
      # Join with neu adjustments to get neu factor
      left_join(state_adjustments$neu_adjustments,
        by = c("year", "source_description", "sector_description")
      ) %>%
      # Get sum of all states' still gas
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Calculate adjusted value
      mutate(neu_adjusted_value = neu_factor * (value / states_sum_value))
  ) %>%
    # Collapse list into a single data frame
    list_rbind() %>%
    # Remove nonessential columns to simplify joins in state_breakouts.R
    select(sector_description:year, neu_adjusted_value, state, msn)

  ## Aggregate------------------------------------------------

  seds_adjusted <- lst(
    seds_res_adjusted,
    seds_com_adjusted,
    seds_ind_adjusted,
    seds_tra_adjusted,
    seds_ele_adjusted,
    seds_ibf_adjusted,
    seds_neu_adjusted
  )

  # Aggregate all data and make IBF & NEU adjustments
  seds_all_adjusted <- list_rbind(seds_adjusted %>%
    # Remove IBF and NEU data for now
    discard(names(.) %in%
      c(
        "seds_ibf_adjusted",
        "seds_neu_adjusted"
      ))) %>%
    select(state:adjusted_value, -eia_description) %>%
    ## Subtract NEU and IBF------------------------------
    # Join with NEU adjusted data
    left_join(seds_adjusted$seds_neu_adjusted,
      by = c(
        "sector_description", "source_description", "year",
        "state", "msn"
      )
    ) %>%
    # Join with IBF adjusted data
    left_join(seds_adjusted$seds_ibf_adjusted,
      by = c(
        "sector_description", "source_description", "year",
        "state", "msn"
      )
    ) %>%
    # Get adjusted value - NEU and IBD values = final adjusted tBtu
    mutate(
      neu_ibf_adjusted_value = if_else(
        # Subtract IBF only if IBF applies (i.e., isn't NA)
        !is.na(ibf_adjusted_value),
        adjusted_value - ibf_adjusted_value,
        adjusted_value
      ),
      # Some MSNs are 100% NEU. For these, NEU value = 100% of adjusted value
      neu_adjusted_value = if_else(
        msn %in% c(
          "ARICB", "LUICB", "FNICB", "CLKCB",
          "FOICB", "SNICB", "WXICB",
          "MSICB", "LUACB"
        ),
        adjusted_value, neu_adjusted_value
      ),
      neu_ibf_adjusted_value = if_else(
        # Subtract NEU only if NEU applies (i.e., isn't NA)
        !is.na(neu_adjusted_value),
        neu_ibf_adjusted_value - neu_adjusted_value,
        neu_ibf_adjusted_value
      )
    ) 
    

  # Apply labels to variables
  seds_all_adjusted <- general_data$apply_variable_labels(
    seds_all_adjusted,
    general_data$ghgi_variables
  )

  state_ffc_adjusted <- lst(seds_all_adjusted, 
                            seds_ind_adjusted, 
                            seds_neu_adjusted)

  return(state_ffc_adjusted)
}


# XIV. STATE CO2 EMISSIONS-----------------------------------------
state_ffc_calculate_emissions <- function(state_ffc_adjusted,
                                          carbon_coefficients,
                                          general_data, 
                                          state_adjustments) {
  

  carbon_emissions_state_ffc <- state_ffc_adjusted$seds_all_adjusted %>%
    # change source descriptions to match those in carbon_factors
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
      str_detect(source_description, "hydrocarbon") & sector_description != "industrial sector" ~ "lpg",
      str_detect(source_description, "hydrocarbon") & sector_description == "industrial sector" ~ "hgl (energy use)",
      str_detect(source_description, "miscellaneous") ~ "misc. products",
      str_detect(source_description, "distillate ") ~ "distillate fuel oil",
      str_detect(source_description, "residual") ~ "residual fuel oil",
      .default = source_description
    )) %>%
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
      .default = source_description
    )) %>%
    left_join(state_adjustments$feedstock_export_adjustments %>%
                filter(source_description %in% c("other oils", 
                                                 "naphtha", 
                                                 "special naphtha")),
              by = c("source_description", "sector_description", "year")) %>%
    mutate(feedstock_adjustment = replace_na(feedstock_adjustment, 0), 
           neu_ibf_adjusted_value = if_else(source_description %in% 
                                              c("other oils", 
                                                "naphtha", 
                                                "special naphtha"), 
                                            pmax(0, 
                                                 neu_ibf_adjusted_value + 
                                                   (feedstock_adjustment * 
                                                      (neu_ibf_adjusted_value / 
                                                         states_sum_value))), 
           neu_ibf_adjusted_value)) %>%
    # ZERO OUT all pentanes plus and unfinished oils
    mutate(neu_ibf_adjusted_value = case_when(
      source_description == "pentanes plus" ~ 0,
      source_description == "unfinished oils" ~ 0,
      .default = neu_ibf_adjusted_value
    )) %>%
    left_join(carbon_coefficients$carbon_factors,
      by = c("source_description", "year")
    ) %>%
    # MMT CO2  = btu * carbon factor/1000 * 44/12
    mutate(
      mmt_co2 = neu_ibf_adjusted_value *
        (carbon_factor / 1000) * carbon_coefficients$carbon_ratio,
      mmt_co2_neu = adjusted_value *
        (carbon_factor / 1000) * carbon_coefficients$carbon_ratio,
      # Restore original coal source descriptions
      source_description = case_when(
        str_detect(source_description, "coking coal") ~ "coking coal",
        str_detect(source_description, "(?<!coking )coal") ~ "coal",
        .default = source_description
      )
    )
  
 
  # NEU Industrial + Trans lubricants
  carbon_emissions_state_neu <- state_ffc_adjusted$seds_neu_adjusted %>%
    mutate(source_description = if_else(
      source_description == "other coal",  
      "industrial other coal", 
      source_description)) %>%
    left_join(state_adjustments$feedstock_export_adjustments %>%
                select(-sector_description),
              by = c("source_description", "year")) %>%
    left_join(state_adjustments$petrochemicals_distribution,
              by = c("state", "year")) %>%
    mutate(feedstock_adjustment = replace_na(feedstock_adjustment, 0), 
            neu_adjusted_value = pmax(0, neu_adjusted_value + (feedstock_adjustment * petrochemical_percent))) %>%
    left_join(carbon_coefficients$carbon_factors %>%
                filter (source_description != "still gas", 
                        source_description != "lpg") %>%
                mutate(source_description = case_when(
                  str_detect(source_description, "hgl") ~ "hydrocarbon gas liquids",
                  source_description == "still gas (non-energy)" ~ "still gas", 
                  .default = source_description
                )),
              by = c("source_description", "year")
    ) %>%
    # MMT CO2  = btu * carbon factor/1000 * 44/12
    mutate(
      mmt_co2_neu = neu_adjusted_value *
        (carbon_factor / 1000) * carbon_coefficients$carbon_ratio,
      # Restore original coal source descriptions
      source_description = if_else(
        source_description == "industrial other coal", 
        "other coal", 
        source_description), 
      # If divide by zero occurs, make it zero (pentanes plus only)
      mmt_co2_neu = if_else(is.nan(mmt_co2_neu), 0, mmt_co2_neu), 
      # zero out pentanes plus 
      mmt_co2_neu = if_else(source_description == "pentanes plus", 0, mmt_co2_neu)
    )
           
    
    
  # Apply labels to variables
  carbon_emissions_state_ffc <- general_data$apply_variable_labels(
    carbon_emissions_state_ffc,
    general_data$ghgi_variables
  )


  carbon_emissions_state <- lst(carbon_emissions_state_ffc, 
                                carbon_emissions_state_neu)
  
  return(carbon_emissions_state)
}


# XV. NATIONAL FIGURES--------------------------------------
national_ffc_ggplot_figures <- function(national_ffc_adjusted,
                                        carbon_emissions_national) {
  
  national_ffc_figures <- NULL
  
  return(national_ffc_figures)
}

# XVI. STATE FIGURES--------------------------------------
state_ffc_ggplot_figures <- function(seds_all_adjusted,
                                     seds_ind_adjusted,
                                     state_adjustments,
                                     carbon_emissions_state) {
  # Color Palettes-----------------------------------------------------------

  # Hex	Gas	Sector	Economic Sectors
  # 4F81BD	Carbon Dioxide	Energy	Residential
  # C0504D	Methane	Agriculture	Agriculture
  # 4198AF	Nitrous Oxide	IPPU	Industry
  # 9BBB59	HFCs, PFCs, SF6, NF3	LULUF Emissions	Transportation
  # D7925D	 	Waste	Commercial
  # 7F63A1	Net CO2 Flux from LULUCF	LULUCF Removals	Electric Power Industry
  # 49525E	Net Emissions	Net Emissions

  # Style Guide Palette
  ghg_palette <- c(
    "#4F81BD", "#C0504D", "#4198AF", "#9BBB59",
    "#D7925D", "#7F63A1", "#49525E"
  )

  myPalette <- colorRampPalette(c("thistle1", "slateblue3"), space = "Lab")

  # Read National Emissions Data (for Figures)------------------------------

  national_emissions <- read_excel("data/national_inventory_CO2_data.xlsx",
    sheet = "InvDB",
    skip = 0, range = "C16:BA32"
  ) %>%
    clean_names() %>%
    select(
      sector_description = category, source_description = fuel1, ghg,
      starts_with("x")
    ) %>%
    pivot_longer(starts_with("x"), names_to = "year", values_to = "value") %>%
    mutate(
      year = str_sub(year, 2, 5),
      source_description = str_to_lower(source_description),
      ghg = str_to_lower(ghg),
      value = parse_number(value),
      sector_description = str_to_lower(sector_description) %>%
        str_c(" sector"),
      sector_description = if_else(
        str_detect(sector_description, "electric"),
        "electric power sector", sector_description
      )
    )

  # Create Data Object (SEDS + National)--------------------------------------

  # Energy Use, State Totals vs. National:

  state_vs_national_btu <-
    lst(
      states = seds_all_adjusted %>%
        select(sector_description, source_description, year, value) %>%
        mutate(
          value = value / 1000,
          dataname = "state_total",
          # sector_description = case_when(
          #   str_detect(sector_description,
          #              "electric") ~ "electric power sector",
          #   str_detect(sector_description,
          #              "industrial consumption") ~ "industrial sector",
          # .default = sector_description),
          # source_description = if_else(
          source_description = if_else(source_description %in% c(
            "hgl", "propane", "propylene", "hydrocarbon gas liquids",
            "ethane", "ethylene",
            "normal butane", "butylene",
            "isobutane", "isobutylene"
          ), "lpg",
          source_description
          )
        ),
      national = state_adjustments$national_inv_adjustments %>%
        rename(value = national_value) %>%
        mutate(
          dataname = "national_total",
          value = value / 1000,
          source_description = case_when(
            source_description == "other coal" ~ "coal",
            source_description %in% c("hgl", "hydrocarbon gas liquids") ~ "lpg",
            .default = source_description
          )
        )
    )

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
      states = carbon_emissions_state$carbon_emissions_state_ffc %>%
        select(sector_description, year, value = mmt_co2) %>%
        mutate(
          dataname = "state_total",
          ghg = "co2"
        ),
      national = national_emissions %>%
        select(-source_description) %>%
        mutate(dataname = "national_total")
    )

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

  ## Fig 2-2: Differences in Coal/NG State Totals vs National Totals-----------
  state_ffc_figures <- lst(
    fig_2_2 = energy_use %>%
      # Ignore coking coal and gasoline
      filter(
        !str_detect(source_description, "cok|gasoline"),
        # res, com, ind, and ele only
        sector_description != "transportation sector"
      ) %>%
      # Remove everything after the comma in 'natural gas'
      # NOTE: Should we be subtracting supplemental fuels for this figure?
      mutate(source_description = case_when(
        str_detect(source_description, "coal") ~ "coal",
        str_detect(source_description, "natural gas") ~ "natural gas"
      )) %>%
      # Every observation needs both state and national totals
      pivot_wider(names_from = dataname, values_from = total_btu) %>%
      ggplot(aes(x = as.numeric(year), y = state_total - national_total)) +
      geom_line(aes(color = sector_description),
        linewidth = 2.8
      ) +
      # geom_abline(slope = 0, intercept = 1) +
      theme_classic() +
      scale_color_manual(values = ghg_palette) +
      scale_x_continuous(n.breaks = 20) +
      theme(
        axis.text.x = element_text(size = 18, angle = 270, vjust = 0.08),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 20),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 38),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.text = element_text(size = 24),
        legend.title = element_blank()
      ) +
      labs(x = "", y = "Difference: SEDS - national (TBtu) ") +
      facet_wrap(~source_description),



    ## Fig 2-3: Differences in Petroleum Coke State Totals vs Nat. Totals----

    fig_2_3 = state_vs_national_btu %>%
      map(\(.x)
      group_by(.x, dataname, sector_description, source_description, year) %>%
        summarize(total_btu = sum(value, na.rm = TRUE)) %>%
        ungroup()) %>%
      list_rbind() %>%
      filter(
        str_detect(source_description, "petroleum coke"),
        sector_description == "industrial sector"
      ) %>%
      # Every observation needs both state and national totals
      pivot_wider(names_from = dataname, values_from = total_btu) %>%
      ggplot(aes(x = as.numeric(year), y = state_total - national_total)) +
      # Plot state total as a proportion of national total;
      geom_line(aes(color = sector_description),
        linewidth = 3
      ) +
      # geom_abline(slope = 0, intercept = 1) +
      theme_classic() +
      scale_color_manual(values = ghg_palette) +
      scale_x_continuous(n.breaks = 20) +
      theme(
        axis.text.x = element_text(size = 18, angle = 270, vjust = 0.08),
        axis.text.y = element_text(size = 18),
        axis.title.y = element_text(size = 24),
        # axis.title.y =
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "none"
      ) +
      labs(x = "", y = "Difference: SEDS - national (TBtu) "),

    ## Fig 2-4: Sectoral Differences in Select Fuels-------------------

    fig_2_4a = state_vs_national_btu %>%
      list_rbind() %>%
      mutate(sector_description = word(sector_description)) %>%
      filter(
        year == "2021",
        sector_description != "electric",
        str_detect(
          source_description,
          "kerosene|residual|lubricants"
        )
      ) %>%
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
      theme(
        axis.text.x = element_text(size = 12, angle = 270, vjust = 0.08),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 18),
        strip.text.x = element_text(size = 18),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "none",
        strip.background = element_blank()
      ) +
      labs(x = "", y = "Difference: SEDS - national (TBtu) ") +
      # Option 2: facet_wrap to avoid overlapping lines
      facet_grid(~source_description, scales = "free"),
    fig_2_4b = state_vs_national_btu %>%
      list_rbind() %>%
      mutate(sector_description = word(sector_description)) %>%
      filter(
        year == "2021",
        sector_description != "electric",
        str_detect(
          source_description,
          "lpg"
        )
      ) %>%
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
      theme(
        axis.text.x = element_text(size = 12, angle = 270, vjust = 0.08),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "none",
        strip.background = element_blank()
      ) +
      labs(x = "", y = ""),

    ## Fig 2-5: IPPU Adjustments Made to Industrial Sector Energy Use-------

    fig_2_5 = seds_ind_adjusted %>%
      mutate(ippu_adjustments = case_when(
        msn == "CLKCB" ~ value * pmax(0, (1 - coking_coal_adj / states_sum_value)),
        msn == "CLOCB" ~ other_coal_coke_adj + other_coal_is_adj,
        msn == "net natural gas" ~ natural_gas_ammonia_adj + natural_gas_is_adj,
        msn == "RFICB" ~ cb_residual_adj * petrochemical_cb_percent,
        msn == "DFICB" ~ is_diesel_adj * is_percent,
        .default = 0
      )) %>%
      mutate(ippu_adjustments = if_else(
        ippu_adjustments < 0, 0, ippu_adjustments
      )) %>%
      group_by(year) %>%
      summarize(
        total_ippu_adjustments = sum(
          ippu_adjustments,
          na.rm = TRUE
        ),
        percent_of_unadjusted = total_ippu_adjustments / sum(
          value,
          na.rm = TRUE
        )
      ) %>%
      ungroup() %>%
      ggplot(aes(x = year, y = total_ippu_adjustments)) +
      geom_col(aes(fill = percent_of_unadjusted * 100)) +
      theme_classic() +
      scale_fill_gradientn(colours = myPalette(100)) +
      theme(
        axis.text.x = element_text(size = 12, angle = 270, vjust = 0.08),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "bottom",
        legend.text = ,
        strip.background = element_blank()
      ) +
      labs(x = "", y = "tBtu", fill = "% of unadj. ind. sector total"),

    ## Figs 2-6 and 2-7 are infographics built from tables----------------

    ## Fig 2-8: Comparison of Transportation Sector Fuel Use--------------

    fig_2_8 = ggplot(
      state_vs_national_btu %>%
        list_rbind() %>%
        filter(
          sector_description == "transportation sector",
          str_detect(
            source_description,
            "distillate|motor"
          )
        ) %>%
        group_by(
          dataname, sector_description,
          source_description, year
        ) %>%
        summarize(total_btu = sum(value, na.rm = TRUE)) %>%
        ungroup(),
      aes(x = as.numeric(year), y = total_btu)
    ) +
      geom_line(aes(color = dataname), linewidth = 1) +
      geom_point(aes(color = dataname), size = 1.9) +
      theme_classic() +
      scale_color_manual(values = ghg_palette) +
      scale_x_continuous(n.breaks = 20) +
      theme(
        axis.text.x = element_text(size = 10, angle = 270, vjust = 0.08),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.text.x = element_text(size = 20),
        strip.background = element_blank()
      ) +
      labs(x = "", y = "tBtu") +
      facet_grid(~source_description),


    ## Fig 2-9 requires the full suite of Transport sector data------------


    ## Fig 2-10: Adjustments made to Industrial Sector for NEUs------------

    fig_2_10 = seds_all_adjusted %>%
      filter(sector_description == "industrial sector") %>%
      group_by(year) %>%
      summarize(
        total_neu_adjustments = sum(
          neu_adjusted_value,
          na.rm = TRUE
        ),
        percent_of_unadjusted = total_neu_adjustments / sum(
          value,
          na.rm = TRUE
        )
      ) %>%
      ungroup() %>%
      ggplot(aes(x = year, y = total_neu_adjustments)) +
      geom_col(aes(fill = percent_of_unadjusted * 100)) +
      theme_classic() +
      scale_fill_gradientn(colours = myPalette(100)) +
      theme(
        axis.text.x = element_text(size = 8, angle = 270, vjust = 0.08),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "bottom",
        strip.background = element_blank()
      ) +
      labs(x = "", y = "tBtu", fill = "% of unadj. ind. sector total"),


    ## Fig 2-11: Adjustments Made to Transportation Sector for IBFs-------

    fig_2_11 = seds_all_adjusted %>%
      filter(sector_description == "transportation sector") %>%
      group_by(year) %>%
      summarize(
        total_ibf_adjustments = sum(
          ibf_adjusted_value,
          na.rm = TRUE
        ),
        percent_of_unadjusted = total_ibf_adjustments / sum(
          value,
          na.rm = TRUE
        )
      ) %>%
      ungroup() %>%
      ggplot(aes(x = year, y = total_ibf_adjustments)) +
      geom_col(aes(fill = percent_of_unadjusted * 100)) +
      theme_classic() +
      scale_fill_gradientn(colours = myPalette(100)) +
      theme(
        axis.text.x = element_text(size = 8, angle = 270, vjust = 0.08),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "bottom",
        strip.background = element_blank()
      ) +
      labs(x = "", y = "tBtu", fill = "% of unadj. trans. sector total"),


    ## Fig 2-12: Differences in State Total and Nat. Total FFC CO2-----

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
      theme(
        axis.text.x = element_text(size = 14, angle = 270, vjust = 0.08),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom"
      ) +
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
      theme(
        axis.text.x = element_text(size = 14, angle = 270, vjust = 0.08),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "none"
      ) +
      labs(x = "", y = "Difference: SEDS - national (MMT CO2) ")


    ## Fig 2-13: Differences in State and Nat.Total NEU CO2--------

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


# XVII. NATIONAL TABLES----------------------------------------------
national_ffc_gt_tables <- function(national_ffc_adjusted,
                                   carbon_emissions_national) {
  national_ffc_tables <- lst()
  gt(mtcars,
     rowname_col = "manufacturer") %>%
    tab_header(title = md(
      "Table 6-3: CO<sub>2</sub>, CH<sub>4</sub>, and N<sub>2</sub>O Emissions from Energy (MMT CO<sub>2</sub> Eq.)")) %>% 
    tab_stub_indent(
      rows = everything(),
      indent = 3) %>%
    # rows_add(Activity = "Reservoirs", .before = 1) %>%
    tab_stubhead(label = "Source") %>%
    opt_vertical_padding(2) %>%
    cols_align(align = "left") %>%
    fmt_number(decimals = 1) %>%
    sub_missing(missing_text = " ") %>%
    tab_options(
      heading.border.bottom.style = "solid", 
      heading.border.bottom.color = "black", 
      column_labels.border.bottom.style = "solid", 
      column_labels.border.bottom.color = "black", 
      column_labels.border.bottom.width = 3, 
      stub.text_transform = "capitalize",
      stub.border.style = "none",
      grand_summary_row.border.style = "solid",
      grand_summary_row.border.color = "black",
      grand_summary_row.border.width = 1) %>%
    grand_summary_rows(
      columns = where(is.numeric), 
      fmt = ~ fmt_number(., decimals = 1), 
      fns = list(Total ~ sum(., na.rm = TRUE))) %>%
    cols_add('TEST1' = '', .after = 'mpg') %>% 
    cols_add('TEST2' = '', .after = 'cyl') %>% 
    cols_label('TEST1' = md('  '),
               'TEST2' = md('  ')) %>%
    tab_style(style = cell_text(weight = "bold", color = "black"), 
              locations = list(cells_grand_summary(), cells_stubhead(), 
                               cells_title(), cells_stub_grand_summary(), 
                               cells_row_groups(),
                               cells_stub(1), 
                               cells_column_labels())) %>%
    tab_style (style = cell_fill(color = "gray", alpha = 0.5), 
               locations = list(cells_body(c(2, 4)), 
                                cells_grand_summary(c(2, 4)), 
                                cells_column_labels(c(2,4)))) %>%
    tab_style(style = cell_text(align = "center"),
              location = list(cells_body(), cells_column_labels(), 
                              cells_grand_summary())) %>%
    tab_footnote(footnote = "Note: Totals may not sum due to independent rounding.") 
  
  return(national_ffc_tables)
}


# XVIII. STATE TABLES----------------------------------------------
state_ffc_gt_tables <- function(seds_all_adjusted,
                                carbon_emissions_state) {
  state_ffc_tables <- lst(

    # Table 2-1---------------------------------------------------
    table_2_1 <- read_excel("data/state_report_tables.xlsx", sheet = 1) %>%
      gt() %>%
      tab_header(
        title = "Table 2-1. Overview of Approaches for Estimating State-Level Energy Sector GHG Emissions"
      ) %>%
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
        row.striping.background_color = "lightsteelblue1"
      ) %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_body())
      ) %>%
      tab_style(
        style = cell_text(align = "center"),
        locations = list(cells_column_labels())
      ) %>%
      tab_style(
        style = cell_text(font = "Calibri"),
        locations = cells_title()
      ) %>%
      opt_footnote_marks(marks = "letters") %>%
      tab_footnote(
        footnote = "Emissions are not likely occurring in U.S. territories; due to a lack of available data and the nature of this category, territories not listed are not estimated.",
        locations = list(
          cells_column_labels(columns = 4),
          cells_body(columns = 4, rows = c(3, 4, 6))
        )
      ),

    # Table 2-2---------------------------------------------------
    table_2_2 <- read_excel("data/state_report_tables.xlsx", sheet = 2) %>%
      # Add grouping columns for the gt table
      mutate(group = c(
        rep("Determine Activity Data", times = 7),
        rep("Calculate CO2 Emissions", times = 3)
      )) %>%
      gt(groupname_col = "group") %>%
      tab_header(
        title = "Table 2-2.  Comparison of Approaches/Data Sources Used to Determine FFC Emissions"
      ) %>%
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
        row.striping.background_color = "lightsteelblue1"
      ) %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_body())
      ) %>%
      tab_style(
        style = cell_text(font = "Calibri"),
        locations = cells_title()
      ) %>%
      tab_style(
        style = cell_text(align = "center"),
        locations = list(cells_column_labels(), cells_row_groups())
      ),

    # Table 2-3---------------------------------------------------
    table_2_3 <- read_excel("data/state_report_tables.xlsx", sheet = 3) %>%
      gt(groupname_col = "Source/Category", row_group_as_column = TRUE) %>%
      tab_header(
        title = md("Table 2-3. Default Data Sources for Mobile Source Non-CO<sub>2</sub> Emissions")
      ) %>%
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
        row.striping.background_color = "lightsteelblue1"
      ) %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_body())
      ) %>%
      tab_style(
        style = cell_text(align = "center"),
        locations = list(cells_column_labels(), cells_row_groups())
      ) %>%
      tab_style(
        style = cell_text(font = "Calibri"),
        locations = cells_title()
      ) %>%
      tab_style(
        style = cell_fill(color = "lightsteelblue1"),
        locations = list(cells_row_groups())
      ),

    # Table 2-4---------------------------------------------------
    table_2_4 <- read_excel("data/state_report_tables.xlsx", sheet = 4) %>%
      gt() %>%
      tab_header(
        title = md("Table 2-4: Summary of Approaches to Disaggregate Waste Incineration Emissions Across Time Series")
      ) %>%
      opt_row_striping() %>%
      # rows_add(`National-Level Estimates` = "Determine Activity Data", .before = 1) %>%
      sub_missing(missing_text = " ") %>%
      text_transform(
        locations = cells_body(column = `Summary of Data Used`),
        fn = function(x) {
          paste("• ", x)
        }
      ) %>%
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
        row.striping.background_color = "lightsteelblue1"
      ) %>%
      tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_body())
      ) %>%
      tab_style(
        style = cell_text(font = "Calibri"),
        locations = cells_title()
      ) %>%
      tab_style(
        style = cell_text(align = "center"),
        locations = list(cells_column_labels())
      ),

    # Table 2-5---------------------------------------------------
    # table 2-5 must be populated with data;
    # where is this data from? Ask Vince
  )

  return(state_ffc_tables)
}



# XIX. INVDB--------------------------------------------------
write_to_invdb <- function(
    # carbon_emissions_national, 
  carbon_emissions_territories, 
  carbon_emissions_state) {
  
  
  territories_invdb <- carbon_emissions_territories %>%  
    mutate(`Data Type` = "GHG", 
           Sector = "Energy",
           Subsector = "US Territories", 
           Category = "Fossil Fuel Combustion", 
           GeoRef = str_to_upper(state), 
           GHG = "CO2",
           Fuel1 = case_when(
             source_description == "coal" ~ "Coal", 
             source_description == "natural gas" ~ "Natural Gas", 
             .default = "Petroleum")) %>%
    group_by(`Data Type`, Sector, Subsector,  Category, 
             Fuel1, GeoRef, GHG, Year = year) %>%
    summarize(mmt_co2 = sum(mmt_co2, na.rm = TRUE)) %>%
    arrange(desc(Year)) %>%
    pivot_wider(names_from = Year, values_from = mmt_co2)
  
  neu_territories_invdb <- carbon_emissions_territories %>%  
    # Create or modify fields to conform to InvDB
    mutate(`Data Type` = "GHG", 
           Sector = "Energy",
           Subsector = "US Territories",
           Category = "Non-Energy Uses of Fossil Fuels",
           GeoRef = str_to_upper(state), 
           GHG = "CO2",
           Fuel1 = "") %>%
    filter(source_description %in% c("other petroleum liquids", 
                                     "lubricants")) %>%
    group_by(`Data Type`, Sector, Subsector,  Category, 
             Fuel1, GeoRef, GHG, Year = year) %>%
    summarize(mmt_co2 = sum(mmt_co2, na.rm = TRUE)) %>%
    arrange(desc(Year)) %>%
    pivot_wider(names_from = Year, values_from = mmt_co2)
 
  # NEU: naptha, s naphtha, other oil, misc, asphalt, waxes, lubrs, coking coal
 neu_invdb_1 <-
   carbon_emissions_state$carbon_emissions_state_ffc %>%
    # Create or modify fields to conform to InvDB
    mutate(`Data Type` = "GHG", 
           Sector = "Energy",
           Subsector = str_remove(sector_description, " sector") %>% 
             str_to_title(),
           Category = "Non-Energy Uses of Fossil Fuels",
           GeoRef = str_to_upper(state), 
           GHG = "CO2",
           Fuel1 = "", 
           mmt_co2 = mmt_co2_neu) %>%
   filter(Subsector %in% c("Industrial", "Transportation")) %>%
   filter(msn %in% c("LUACB", "CLKCB", 
                     "ARICB",  "LUICB",  
                     "FOICB", "FNICB", "SNICB", 
                     "WXICB", "MSICB")) 
    
 # NEU: other coal, pentanes, nat gas, pet coke, diesel, still gas, lpg
 neu_invdb_2 <-
   carbon_emissions_state$carbon_emissions_state_neu %>%
   # Create or modify fields to conform to InvDB
   mutate(`Data Type` = "GHG", 
          Sector = "Energy",
          Subsector = str_remove(sector_description, " sector") %>% 
            str_to_title(),
          Category = "Non-Energy Uses of Fossil Fuels",
          GeoRef = str_to_upper(state), 
          GHG = "CO2",
          Fuel1 = "", 
          mmt_co2 = mmt_co2_neu) %>%
   filter(Subsector == "Industrial") 
 
 neu_invdb <- bind_rows(neu_invdb_1, 
                        neu_invdb_2) %>%
   group_by(`Data Type`, Sector, Subsector,  Category, 
            Fuel1, GeoRef, GHG, Year = year) %>%
   summarize(mmt_co2 = sum(mmt_co2, na.rm = TRUE)) %>% 
   ungroup() %>%
   arrange(desc(Year)) %>%
   pivot_wider(names_from = Year, values_from = mmt_co2)
 
  ffc_invdb <-
    carbon_emissions_state$carbon_emissions_state_ffc %>%
    # Create or modify fields to conform to InvDB
    mutate(Subsector = str_remove(sector_description, " sector") %>% 
             str_to_title(),
           GeoRef = str_to_upper(state), 
           Fuel1 = case_when(
             source_description %in% c("coal", "coking coal") ~ "Coal", 
             source_description == "geothermal energy" ~ "Geothermal",
             source_description== "natural gas" ~ "Natural Gas", 
             .default = "Petroleum")) %>%    # Select InvDB fields
    select(Subsector, Fuel1, GeoRef, Year = year, mmt_co2) %>%
    # Sum mmt CO2 for each Subsource/Fuel/State/Year
    group_by(Subsector, Fuel1, GeoRef, Year) %>%
    summarize(value = sum(mmt_co2, na.rm = TRUE)) %>%
    filter(Year != "1989") %>%
    # Pivot data wide so the years are columns
    pivot_wider(names_from = Year, values_from = value) %>%
    ungroup() %>%
    mutate('Data Type' = "GHG",
           Sector = "Energy", 
           Category = "Fossil Fuel Combustion", 
           GHG = "CO2") %>%
    select('Data Type', Sector, Subsector, Category, 
           Fuel1, GeoRef, GHG, everything()) %>%
    bind_rows(territories_invdb) %>%
    bind_rows(neu_territories_invdb) %>%
    bind_rows(neu_invdb) %>%
    # make Territory codes conform to InvDB
    mutate(GeoRef = case_when(
      GeoRef == "GUM" ~ "GU",  
      GeoRef == "PRI" ~ "PR",
      GeoRef == "ASM" ~ "AS",
      GeoRef == "VIR" ~ "VI",
      GeoRef == "WAK" ~ "UM",
      GeoRef == "USIQ" ~ "MP",
      .default = GeoRef
    )) 

  
  # Load blank Excel workbook
  wb <- loadWorkbook("invDB/InvDB_ffc_template.xlsx")
  
  # Write data to each set of columns on the worksheet
  writeData(wb, select(ffc_invdb, 1:4), sheet = 1, 
            startCol = 1, startRow = 2, colNames = FALSE) 
  
  writeData(wb, select(ffc_invdb, Fuel1), sheet = 1, 
            startCol = 11, startRow = 2, colNames = FALSE) 
  
  writeData(wb, select(ffc_invdb, GeoRef), sheet = 1, 
            startCol = 13, startRow = 2, colNames = FALSE) 
  
  writeData(wb, select(ffc_invdb, GHG), sheet = 1, 
            startCol = 20, startRow = 2, colNames = FALSE) 
  
  writeData(wb, select(ffc_invdb, 8:last_col()), sheet = 1, 
            startCol = 22, startRow = 2, colNames = FALSE) 
  
  # Save InvDB workbook
  saveWorkbook(wb, "invDB/InvDB_ffc_new.xlsx", overwrite = TRUE)
  
  # Save as csv
  write_csv(ffc_invdb, "invDB/ffc.csv")
  
  # Save as JSON
  write_json(ffc_invdb, "invDB/ffc.json")
  
}