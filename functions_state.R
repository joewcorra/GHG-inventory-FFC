# STATE CONSUMPTION DATA-------------------------------------------
#'
#' @description This function retrieves state-by-state fossil fuel consumption data from the U.S. Energy Information Administration (EIA)'s State Energy Data System (SEDS).
#' @details
#' **Retrieval:** 
#' - Downloads annual fossil fuel consumption data from EIA API for all U.S. states.
#' - Constructs a URL for the API request, retrieves the data, and processes the JSON response into an R object.
#' - Defines a nested function, `get_state_results()`, to query the EIA API for annual energy data by state and year.
#' - Downloads either the entire time series and save to CSV, or loads an existing CSV and downloads only the most recent year of the time series.
#' - Measures the time taken for this operation.
#' - Uses the `general_data` object to identify the correct variable names and sector mappings.
#' **Transform:** 
#' - Cleans names, selects relevant columns, and filters the data to include only rows with units in "Billion Btu".
#' - Applies naming harmonization so sectors, fuels, and years align with GHGI data dictionary.
#' - Removes any duplicate rows that might have been added when new annual data was appended.
#' - Filters the dataset to include only fossil fuel energy sources relevant to GHGI reporting.
#' - Aggregates hydrocarbon gas liquids (HGLs) data.
#' **Collate/Output:** 
#' - `seds`: tibble; used by 
#' @param general_data List created by `data_setup()` (keys: ghgi_values, variables, etc.)
#' @return Tibble with columns: 
#' @seealso [state_ffc_adjust_data()], [national_ffc_calculate_emissions()]
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

# TERRITORIES CONSUMPTION DATA-----------------------------------------
#'
#' @description This function retrieves and processes fossil fuel consumption and heat content data for U.S. territories from the U.S. Energy Information Administration (EIA) API, using a process similar to `state_ffc_get_seds_data()`.
#' @details
#' **Retrieval:** 
#' - Downloads annual fossil fuel consumption data from EIA API for all U.S. territories.
#' - Constructs a URL for the API request, retrieves the data, and processes the JSON response into an R object.
#' - Downloads PDF of fossil fuel heat content by year from EIA df document extracts data from document. 
#' - Uses the `general_data` object to identify the correct variable names and sector mappings.
#' **Transform:** 
#' - Cleans names, selects relevant columns, and filters the data to include only rows with units in "Billion Btu".
#' - Applies naming harmonization so sectors, fuels, and years align with GHGI data dictionary.
#' - Filters the dataset to include only fossil fuel energy sources relevant to GHGI reporting.
#' - Manually creates time series for Puerto Rico lubricants data (not available from EIA).
#' - Aggregates hydrocarbon gas liquids (HGLs) from EIA data. 
#' **Collate/Output:** 
#' `territories`: a list containing the following:
#' `ff_territories`: a tibble
#' `heat_content_territories`: a tibble
#' @param general_data List created by `data_setup()` (keys: ghgi_values, variables, etc.)
#' @return 
#' @seealso [state_ffc_get_seds_data()], [territories_ffc_adjust_data()]
#' @examples
#' # minimal usage example
#' # state_ffc_get_seds_data(general_data)
get_territories_data <- function(general_data) {
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
    "&api_key=", key) %>% # our API key is required
    GET() %>% # retrieve page from url
    content("raw") %>% # extract content as a raw vector
    rawToChar() %>% # convert to character data
    fromJSON() # convert from JSON to R object
  
  # Manually create lubricants data for PR (not in EIA for some reason)
  pr_lubricants <- tibble(state = "PR", 
                          value = c(0.296, 0.247, 0.568, 0.760273973, 0.83, 
                                     0.62, 0.644262295, 1.096, 0.548, 0.603, 
                                     1.038251366, 0.823065753, 
                                     0.960564384, 2.154621918, 2.256830601, 
                                     2.035616438, 2.764383562, 2.649862286, 
                                     1.17, 0.44, 0.44, 0.44,  0.44, 0.44, 0.44, 
                                     0.44, 0.44, 0.44, 0.44, 0.44, 0.44, 0.44, 
                                     0.44, 0.44),
                          year = 1990:2023, 
                          state_name = "Puerto Rico",
                          source_description = "lubricants", 
                          unit = "", 
                          source = "", 
                          dataFlagDescription = "")
  

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
      state = case_when(
        state == "ASM" ~ "AS", 
        state == "VIR" ~ "VI", 
        state == "PRI" ~ "PR", 
        state == "GUM" ~ "GU", 
        .default = state
      ), 
      
      source_description = str_to_lower(source_description),
      source_description = case_when(
        source_description == "liquefied petroleum gases" ~ "lpg",
        source_description == "dry natural gas" ~ "natural gas",
        # We need lubricants data, but it's not listed in EIA
        # source_description == "????" ~ "lubricants",
        .default = source_description
      )
    ) %>% 
    rbind(pr_lubricants)
  

  
  # Retrieve FF Heat Content from EIA----------------------------------
  
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
          "Butylene", "Isobutylene"
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
    expand_grid(year = 1990:latest_year-1) %>%
    # Make the year character to match EIA data
    mutate(year = as.character(year)) %>%
    # Merge with EIA heat content data (mogas and nat gas)
    bind_rows(eia_api_heat) %>%
    # Standardize source descriptions to match EIA territories data
    mutate(source_description = case_when(
      source_description == "miscellaneous products" ~ "other petroleum liquids",
      .default = source_description
    ))
  
  territories <- lst(ff_territories, 
                      heat_content_territories)
}
# STATE ADJUSTMENTS DATA (COMBINED)-----------------------------
#'
#' @description This function collates and transforms the datasets that are required to perform adjustments/corrections on the SEDS fossil fuel consumption data. Data include international bunker fuels, non-energy use, industrial distributions, and specific fuel types. 
#' @details
#' **Retrieval:** 
#'  - Collates locally-stored CSv and Excel datasets retrieved in the `targets` pipeline.
#'  - in-work: Pulls required data from national results in `national_ffc_adjusted`.
#'  - in-work: Accesses `pins` board to retrieve CSv and Excel datasets.
#' **Transform:** 
#'  - Extracts required data from input datasets via selecting, pivoting, filtering, and mutating data as needed.
#'  - National inventory adjustments: Selects year, sector_description, source_description, and adjusted_value.
#'  - International bunker fuels (IBF) adjustments: Pivots data to long format and standardizes values.
#'  - Non-energy use (NEU) adjustments: Obtains values of national NEU from IPPU data, then calculates percent NEU distribution by state. 
#'  - Iron & steel (I&S) distributions: Obtains I&S values from national IPPU data, then calculates percent I&S distribution by state. 
#'  - Ammonia distributions: Obtains ammonia values from national IPPU data, then calculates percent ammonia distribution by state. 
#'  - Petrochemical and carbon black distributions: Obtains petrochemical values from national IPPU data, then calculates percent petrochemical and percent carbon black by state.
#'  - Fuel oil and kerosene (FOKS) distributions: Processes diesel and residual fuel distribution data from EIA and pivots it into a long format. Extrapolates data for years beyond 2020, as FOKS data is no longer being compiled by EIA.
#'  - Feedstock export adjustments: Reads data from export feedstocks (currently reading from Excel; later versions will read directly from functions_national.R output).
#' **Collate/Output:** 
#'   `state_adjustments`: a list containing the following: 
#'   `misc_adjustments`: a tibble
#'   `national_inv_adjustments`: a tibble
#'   `ibf_adjustments`: a tibble
#'   `neu_adjustments`: a tibble
#'   `is_distribution`: a tibble
#'   `ammonia_distribution`: a tibble
#'   `petrochemicals_distribution`: a tibble
#'   `petrochemicals_cb_distribution`: a tibble
#'   `foks_diesel_distribution`: a tibble
#'   `foks_residual_distribution`: a tibble
#'   `feedstock_export_adjustments`: a tibble
#'
#' @param national_ffc_adjusted tibble created by `national_ffc_adjust_data()`
#' @param international_bunker_fuels tibble loaded in `_targets.R` pipeline
#' @param misc_adjustments tibble loaded in `_targets.R` pipeline
#' @param non_energy_use tibble loaded in `_targets.R` pipeline
#' @param ippu_distributions list of 4 tibbles loaded in `_targets.R` pipeline
#' @param foks_diesel tibble loaded in `_targets.R` pipeline
#' @param foks_residual tibble loaded in `_targets.R` pipeline
#' @return 
#' @seealso [state_ffc_adjust_data()], [national_ffc_calculate_emissions()]
#' @examples
#' # minimal usage example
#' # state_ffc_get_seds_data(general_data)
state_ffc_get_adjustments_data <- function(national_ffc_adjusted,
                                           international_bunker_fuels,
                                           misc_adjustments,
                                           non_energy_use,
                                           ippu_distributions,
                                           foks_diesel,
                                           foks_residual) {

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
        # Remove extra spaces from source
        str_squish())
  
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
  feedstock_export_adjustments <- 
    read.csv("data/feedstock_export_adjustments.csv") %>%
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

# TERRITORIES ADJUSTMENTS AND CO2 EMISSIONS-------------------------------
#'
#' @description This function processes fossil fuel consumption data for U.S. territories and calculates carbon emissions based on that consumption.
#' @details
#' **Retrieval:** 
#' - No new data retrieved in this function. 
#' **Transform:** 
#' - Joins the two elements of the `territories` list, `ff_territories` and `heat_content_territories`, to get heat content information based on the year fossil fuel energy consumption source.
#' - Sets a constant storage factor of 0.1 for non-energy use (NEU); this is unique to territories.
#' - Interpolates missing carbon factors if necessary.
#' - Calculates carbon emissions in million metric tons of CO2 by multiplying consumption by the appropriate carbon factor.
#' - Adjusts emissions for other petroleum liquids and lubricants by applying the NEU storage factor.
#' - Applies metadata labels, derived from the data dictionary in `general_data`, to the column names.
#' **Collate/Output:** 
#' `carbon_emissions_territories`: a tibble
#'
#' @param carbon_coefficients a list created by `get_carbon_factors()`
#' @param territories a list created by `get_territories_data`
#' @param general_data a list created by `data_setup()`
#' @return 
#' @seealso [state_ffc_adjust_data()], [national_ffc_calculate_emissions()]
#' @examples
#' # minimal usage example
#' # state_ffc_get_seds_data(general_data)
territories_ffc_adjust_data <- function(carbon_coefficients, 
                                        territories, 
                                        general_data) {
  
  # Calculcate Consumption-------------------------------------------------
  
  ffc_territories <- territories$ff_territories %>%
    left_join(territories$heat_content_territories,
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
    # NEU storage factor is constant for all territories and years
    mutate(storage_factor = 0.1) %>%
    # Fill in missing carbon factors for coal and other petroleum
    # These values are also constant across territories and year
    mutate(carbon_factor = case_when(
      source_description == "other petroleum liquids" ~ 20.0,
      source_description == "coal" ~ 25.1,
      .default = carbon_factor
    ), 
    # MMT CO2  = btu * carbon factor/1000 * 44/12
    mmt_co2 = tbtu / 1000 * carbon_factor *  carbon_coefficients$carbon_ratio, 
    mmt_co2 = if_else(source_description %in% 
                        c("other petroleum liquids", "lubricants"), 
                      mmt_co2 * (1 - storage_factor), 
                      mmt_co2))
  
  # Apply labels to variables
  carbon_emissions_territories <- general_data$apply_variable_labels(
    carbon_emissions_territories,
    general_data$ghgi_variables
  )
  
  return(carbon_emissions_territories)
  
}

# STATE FFC ADJUSTMENTS---------------------------
#'
#' @description This function processes and adjusts fossil fuel consumption data for various sectors across U.S. states, incorporating adjustments for non-energy use (NEU) and international bunker fuels (IBF).
#' @details
#' **Retrieval:** 
#' - No new data retrieved in this function. 
#' **Transform:** 
#' - Standardizes the seds data using functions from functions_both.R.
#' - Sets the consumption values for pentanes plus and unfinished oils to zero to avoid double counting.
#' - Performs adjustments by sector, collecting the results of each sectors into a list element.
#' - Adjusts residential sector consumption:
#' - Processes residential sector data for various energy sources (coal, natural gas, distillate fuel, LPG, and others).
#' - For coal and natural gas, calculates adjusted values using national inventory adjustment factors and sums of state values.
#' - Subtracts supplemental natural gas to get net natural gas.
#' - For LPGs, uses the original values directly as adjusted values.
#' - Adjusts commercial sector consumption:
#' - Similar to the residential sector, processes commercial sector data for coal, distillate fuel, natural gas, gasoline, LPG, and other sources.
#' - Calculates adjusted values using national inventory adjustment factors.
#' - Subtracts supplemental natural gas to get net natural gas.
#' - Subtracts ethanol from total motor gasoline to get net gasoline.
#' - Adjusts industrial sector consumption:
#' - Processes industrial sector data for various sources, including coking coal, other coal, natural gas, residual fuel, distillate fuel, gasoline, petroleum coke, LPG, and other sources.
#' - Calculates adjusted values using national inventory adjustment factors.
#' - Subtracts supplemental natural gas to get net natural gas.
#' - Subtracts ethanol from total motor gasoline to get net gasoline.
#' - Applies IPPU adjustment factors to coking coal, other industrial coal, natural gas, diesel fuel, and residual fuel.
#' - Adjusts transportation sector consumption:
#' - Processes transportation sector data for distillate fuel, gasoline, lubricants, jet fuel, residual fuel, coal, LPG, aviation gasoline, and natural gas.
#' - Calculates motor gasoline and diesel consumption using distribution data scraped from FWHA website.
#' - Calculates consumption of other fuels using SEDS data.
#' - Calculates adjusted values using national inventory adjustment factors and distribution data.
#' - Subtracts supplemental natural gas to get net natural gas.
#' - Adjusts electric power sector consumption:
#' - Processes electric power sector data for coal, natural gas, geothermal, residual fuel, petroleum coke, and distillate fuel.
#' - Uses adjustment factors from national data to calculate adjusted values.
#' - Adjusts IBF consumption:
#' - Calculates IBF diesel and residual fuel consumption using FOKS distribution data. 
#' - Calculates jet fuel using SEDS data.
#' - Calculates IBF adjusted values using national data and selects relevant columns for further processing.
#' - Adjusts NEU consumption:
#' - Processes NEU data for various sources, including other coal, coking coal, natural gas, distillate fuel, LPG, petroleum coke, still gas, and other NEU sources.
#' - Calculates NEU adjusted values using NEU adjustment factors and IPPU distribution data.
#' # Aggregates all adjusted data:
#' - Aggregates all adjusted data from residential, commercial, industrial, transportation, and electric power sectors.
#' - Subtracts adjusted NEU and IBF values from the applicable adjusted industrial and transportation values to get the final adjusted energy consumption for those sectors.
#' **Collate/Output:** 
#'   - `state_ffc_adjusted`, list with three elements:
#'   - `seds_all_adjusted`, tibble used by 
#'   - `seds_ind_adjusted`, tibble used by 
#'   - `seds_neu_adjusted`, tibble used by 
#'
#' @param general_data List created by `data_setup()` (keys: ghgi_values, variables, etc.)
#' @param seds Tibble created by `state_ffc_get_seds_data()`
#' @param state_adjustments List created by `state_ffc_get_adjustments_data()`
#' @return 
#' @seealso [state_ffc_adjust_data()], [national_ffc_calculate_emissions()]
#' @examples
#' # minimal usage example
#' # state_ffc_get_seds_data(general_data)
state_ffc_adjust_data <- function(seds,
                                  state_adjustments,
                                  scraped_data,
                                  general_data) {
  
  
  seds <- general_data$standardize_ffc(seds, general_data$msn_names) %>%
    filter(year != "1989") %>%
    # ZERO OUT all pentanes plus and unfinished oils
    mutate(value = case_when(
      source_description == "pentanes plus" ~ 0,
      source_description == "unfinished oils" ~ 0,
      .default = value
    ),
    # Make source description for coking coal
    source_description = case_when(msn == "CLKCB" ~ "coking coal",
                                   str_detect(source_description, "naphtha less") ~ "naphtha", 
                                   .default = source_description))
  
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
      # Get sum by state for each year
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
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
        source_description = "coal",
        # Tennessee only
        state = "TN"
      ),
    
    # Coking coal
    coking_coal = seds_ind_adjusted %>%
      filter(msn == "CLKCB") %>%
      mutate(
        neu_adjusted_value = adjusted_value), 
    
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
      filter(msn == "HLICB") %>%
      # Change source description to reflect new value
      mutate(source_description = "hydrocarbon gas liquids") %>%
      # Join with neu adjustments
      left_join(state_adjustments$neu_adjustments,
                by = c("year", "source_description", "sector_description")
      ) %>%
      
      # Get sum of all states' lpg
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      # Rename MSN and calculate adjusted value
      mutate(
        msn = "combined lpg",
        neu_adjusted_value = neu_factor * (value / states_sum_value)
      ),
    
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
      mutate(neu_adjusted_value = neu_factor * (value / states_sum_value)) %>%
      mutate(
        petcoke_adjusted_value = pmax(0, neu_adjusted_value + (-pet_coke_adj * (value / states_sum_value)))),
    
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
      mutate(neu_adjusted_value = neu_factor * (value / states_sum_value)), 
    
    
    # Calculate state sum value for all of these NEU sources 
    other_neu = seds %>%
      filter(msn %in% c(
        "ARICB", "LUICB", "FNICB", 
        "FOICB", "SNICB", "WXICB",
        "MSICB", "LUACB")) %>%
      mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
      mutate(neu_adjusted_value = value)
    
  ) %>%
    # Collapse list into a single data frame
    list_rbind() %>%
    # Remove nonessential columns to simplify joins in state_breakouts.R
    select(1:year, neu_adjusted_value, state, msn, petcoke_adjusted_value)
  
  ## Aggregate------------------------------------------------
  
  seds_adjusted <- lst(
    seds_res_adjusted,
    seds_com_adjusted,
    seds_ind_adjusted,
    seds_tra_adjusted,
    seds_ele_adjusted
  )
  
  # Aggregate all data and make IBF & NEU adjustments
  seds_all_adjusted <- list_rbind(seds_adjusted) %>%
    select(state:adjusted_value, -eia_description) %>%
    ## Subtract NEU and IBF------------------------------
  # Join with NEU adjusted data
  left_join(seds_neu_adjusted, 
            by = c(
              "sector_description", "source_description", "year",
              "state", "msn"
            )
  ) %>%
    # Join with IBF adjusted data
    left_join(seds_ibf_adjusted,
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
      neu_ibf_adjusted_value = if_else(
        # Subtract NEU only if NEU applies (i.e., isn't NA)
        !is.na(neu_adjusted_value),
        neu_ibf_adjusted_value - neu_adjusted_value,
        neu_ibf_adjusted_value)
    ) 
  
  
  # Apply labels to variables
  # seds_all_adjusted <- general_data$apply_variable_labels(
  #   seds_all_adjusted,
  #   general_data$ghgi_variables
  # )
  
  state_ffc_adjusted <- lst(seds_all_adjusted, 
                            seds_ind_adjusted, 
                            seds_neu_adjusted)
  
  return(state_ffc_adjusted)
}

# STATE NEU CO2 EMISSIONS--------------------------------------
#'
#' @description This function calculates carbon emissions for non-energy use (NEU) of various fossil fuel sources across U.S. states. It incorporates adjustments for feedstock exports and applies carbon coefficients to derive emissions.
#' @details
#' **Retrieval:** 
#' - No new data retrieved in this function.  
#' **Transform:** 
#' - Begins with the adjusted SEDS consumption data.
#' - Joins with several datasets: feedstock export adjustments, NEU adjustments, and petrochemical distribution data.
#' -Replaces any missing values in feedstock exports with zero.
#' - Calculates the sum of NEU adjusted values for all states and renames petrochemical distribution percentages as feedstock distribution for clarity.
#' - Processes NEU data for different fossil fuel sources, such as coal, coking coal, natural gas, petroleum coke, hydrocarbon gas liquids (HGL), distillate fuel oil, waxes, still gas, miscellaneous petroleum products, naphtha, special naphtha, other oils, asphalt & road oil, and lubricants.
#' - For each source, calculates adjusted NEU values by incorporating feedstock adjustments and distribution percentages.Some sources use specific adjustment formulas, such as HGLs and other oils, which include additional factors.
#' - Calculates carbon emissions:
#' - Combines all NEU adjusted values into a single data frame.
#' - Joins with carbon factors ensuring the correct factors are used for NEU sources.
#' - Calculates carbon emissions in MMT of CO2 by multiplying consumption by the appropriate carbon factor.
#' - Adjusts the final carbon emissions by accounting for storage factors, which reduce emissions based on the proportion of carbon stored rather than emitted.
#' **Collate/Output:** 
#' `carbon_emissions_state_neu`, a tibble used by
#'
#' @param general_data List created by `data_setup()` (keys: ghgi_values, variables, etc.)
#' @param state_ffc_adjusted List created by `state_ffc_adjust_data()`
#' @param carbon_coefficients List created by `get_carbon_factors()`
#' @param state_adjustments List created by `state_ffc_get_adjustments_data()`
#' @return Tibble with columns: state, year, sector_description, source_description, value …
#' @seealso [state_ffc_adjust_data()], [national_ffc_calculate_emissions()]
#' @examples
#' # minimal usage example
#' # state_ffc_get_seds_data(general_data)
state_neu_calculate_emissions <- function(state_ffc_adjusted,
                                          carbon_coefficients,
                                          general_data, 
                                          state_adjustments) {
  
  # NEU: other coal, nat gas, pet coke, diesel, still gas, lpg
  
  neu <- state_ffc_adjusted$seds_neu_adjusted %>%
    # add: industrial other coal is a special case
    # rbind(state_ffc_adjusted$seds_ind_adjusted %>%
    #         filter(msn == "CLOCB") %>%
    #         mutate(neu_adjusted_value = adjusted_value, 
    #                petcoke_adjusted_value = NA) %>%
    #         select(source_description, sector_description, 
    #                year, neu_adjusted_value, state, 
    #                msn, petcoke_adjusted_value)) %>%
    # join with feedstock exports
  left_join(state_adjustments$feedstock_export_adjustments %>%
              select(-sector_description),
            by = c("source_description", "year")) %>%
    # join with NEU adjustments
    left_join(state_adjustments$neu_adjustments,
              by = c("source_description", 
                     "sector_description", "year")) %>%
    # join with IPPU distribution data
    left_join(state_adjustments$petrochemicals_distribution,
              by = c("state", "year")) %>%
    # Replace NAs in feedstock exports data
    mutate(feedstock_adjustment = replace_na(feedstock_adjustment, 0)) %>%
    # Calculate sum of all states' values
    mutate(states_sum_value = sum(neu_adjusted_value), 
           .by = c(msn, year)) %>%
    # rename for clarity: feedstock exports use same dist.% as petrochemicals
    rename(feedstock_distribution = petrochemical_percent)
  
  
  carbon_emissions_state_neu <- lst(
    
    other_coal = neu %>%
      filter(source_description == "coal"),
    
    coking_coal = neu %>%
      filter(source_description == "coking coal"),
    
    natural_gas = neu %>%
      filter(source_description == "natural gas") %>%
      mutate(neu_adjusted_value = pmax(0, 
                                       neu_adjusted_value + 
                                         (feedstock_adjustment * 
                                            feedstock_distribution))),
      
    petroleum_coke = neu %>%
      filter(source_description == "petroleum coke",
             sector_description == "industrial sector") %>%
      mutate(neu_adjusted_value = petcoke_adjusted_value),
    
    hgl = neu %>%
      filter(source_description == "hydrocarbon gas liquids") %>%
      # HGLs use a different feedstock dist % than everything else
      mutate(feedstock_distribution = neu_adjusted_value / states_sum_value, 
             neu_adjusted_value = pmax(0, 
                                       (neu_factor * 
                                          feedstock_distribution) +
                                         (feedstock_adjustment * 
                                            feedstock_distribution))),
    
    distillate_fuel_oil = neu %>%
      filter(source_description == "distillate fuel oil"),
    
    waxes = neu %>%
      filter(source_description == "waxes") %>%
      mutate(neu_adjusted_value = pmax(0, 
                                       neu_adjusted_value + 
                                         (feedstock_adjustment * 
                                            feedstock_distribution))),
    
    still_gas = neu %>%
      filter(source_description == "still gas"),
    
    miscellaneous = neu %>%
      filter(source_description == "miscellaneous petroleum products") %>%
               mutate(neu_adjusted_value = pmax(0, 
                                                neu_adjusted_value + 
                                                  (feedstock_adjustment * 
                                                     feedstock_distribution))),
    
    naphtha = neu %>%
      filter(source_description == "naphtha") %>%
      mutate(neu_adjusted_value = pmax(0, 
                                       neu_adjusted_value + 
                                         (feedstock_adjustment * 
                                            neu_adjusted_value/states_sum_value))),
    
    special_naphtha = neu %>%
      filter(source_description == "special naphtha") %>%
      mutate(neu_adjusted_value = pmax(0, 
                                       neu_adjusted_value + 
                                         (feedstock_adjustment * 
                                            neu_adjusted_value/states_sum_value))),
    
    other_oils = neu %>%
      filter(source_description == "other oils") %>%
      left_join(state_adjustments$misc_adjustments %>% 
                  select(cb_other_oil_adj, year), 
                by = "year") %>% 
      mutate(neu_adjusted_value = pmax(0, 
                                       neu_adjusted_value + 
                                         (
                                           (feedstock_adjustment - 
                                             cb_other_oil_adj) * 
                                             neu_adjusted_value/states_sum_value))),
    
    asphalt_road_oil = neu %>%
      filter(source_description == "asphalt & road oil") %>%
      mutate(neu_adjusted_value = pmax(0, 
                                       neu_adjusted_value + 
                                         (feedstock_adjustment * 
                                            feedstock_distribution))),
    
    lubricants_ind = neu %>%
      filter(source_description == "lubricants", 
             sector_description == "industrial sector") %>%
      mutate(neu_adjusted_value = pmax(0, 
                                       neu_adjusted_value + 
                                         (feedstock_adjustment * 
                                            feedstock_distribution))),
    
    lubricants_tra = neu %>%
      filter(source_description == "lubricants", 
             sector_description == "transportation sector") %>%
      mutate(neu_adjusted_value = pmax(0, 
                                       neu_adjusted_value + 
                                         (feedstock_adjustment * 
                                            feedstock_distribution))),  
    
  ) %>% 
    
    list_rbind()  %>%
    
    # Calculate mmt of CO2 emissions
    left_join(carbon_coefficients$carbon_factors %>%
                mutate(source_description = case_when(
                  # get correct still gas factor for NEU
                  source_description == "still gas" ~ "still gas (energy)", 
                  source_description == "still gas (non-energy)" ~ "still gas",
                  # get correct hgl factor for NEU
                  source_description == "hgl (non-energy use)" ~ "hydrocarbon gas liquids",
                  # only industrial other coal factor is required for NEU
                  source_description == "industrial other coal" ~ "coal",
                  .default = source_description)),
              by = c("source_description", "year")) %>%
    # MMT CO2  = btu * carbon factor/1000 * 44/12
    mutate(
      mmt_co2_neu = neu_adjusted_value *
        (carbon_factor / 1000) * carbon_coefficients$carbon_ratio)
    
  
  # carbon_emissions_state_neu <- state_ffc_adjusted$seds_neu_adjusted %>%
  #   mutate(neu_adjusted_value = if_else(
  #     source_description == "petroleum coke" & 
  #       sector_description == "industrial sector", 
  #     petcoke_adjusted_value, 
  #     neu_adjusted_value
  #   )) %>%
  #   mutate(source_description = if_else(
  #     source_description == "other coal",  
  #     "industrial other coal", 
  #     source_description)) %>%
  #   left_join(state_adjustments$feedstock_export_adjustments %>%
  #               select(-sector_description),
  #             by = c("source_description", "year")) %>%
  #   left_join(state_adjustments$neu_adjustments,
  #             by = c("source_description", 
  #                    "sector_description", "year")) %>%
  #   left_join(state_adjustments$petrochemicals_distribution,
  #             by = c("state", "year")) %>%
  #   
  #   mutate(feedstock_adjustment = replace_na(feedstock_adjustment, 0)) %>%
  #   mutate(states_sum_value = sum(neu_adjusted_value), 
  #          .by = c(msn, year)) %>%
  #   mutate(
  #     feedstock_distribution = if_else(
  #       source_description == "hydrocarbon gas liquids", 
  #       neu_adjusted_value / states_sum_value, 
  #       petrochemical_percent
  #     )) %>%
  #   mutate(neu_adjusted_value = if_else(
  #     source_description == "hydrocarbon gas liquids", 
  #     pmax(0, (neu_factor * feedstock_distribution) +
  #            (feedstock_adjustment * feedstock_distribution)),
  #     pmax(0, neu_adjusted_value + (feedstock_adjustment * feedstock_distribution)))) %>%
  #   left_join(carbon_coefficients$carbon_factors %>%
  #               filter (source_description != "still gas", 
  #                       source_description != "lpg") %>%
  #               mutate(source_description = case_when(
  #                 source_description == "hgl (non-energy use)" ~ "hydrocarbon gas liquids",
  #                 source_description == "still gas (non-energy)" ~ "still gas", 
  #                 .default = source_description
  #               )),
  #             by = c("source_description", "year")
  #   ) %>%
  #   # MMT CO2  = btu * carbon factor/1000 * 44/12
  #   mutate(
  #     mmt_co2_neu = neu_adjusted_value *
  #       (carbon_factor / 1000) * carbon_coefficients$carbon_ratio,
  #     # Restore original coal source descriptions
  #     source_description = if_else(
  #       source_description == "industrial other coal", 
  #       "other coal", 
  #       source_description), 
  #     # If divide by zero occurs, make it zero (pentanes plus only)
  #     mmt_co2_neu = if_else(is.nan(mmt_co2_neu), 0, mmt_co2_neu)
  #   )
  # 

  
  # Apply NEU storage factors where applicable
  carbon_emissions_state_neu <- carbon_emissions_state_neu %>%
    left_join(carbon_coefficients$neu_storage,
              by = c("source_description",
                     "sector_description",
                     "year")) %>%
    mutate(mmt_co2_neu = mmt_co2_neu * (1 - storage_factor))

return(carbon_emissions_state_neu)

}

# STATE FFC CO2 EMISSIONS-----------------------------------------
#'
#' @description This function calculates carbon emissions for fossil fuel consumption across different sectors in U.S. states. It incorporates adjustments for feedstock exports and applies carbon coefficients to derive emissions.
#' @details
#' **Retrieval:** 
#' - No new data retrieved in this function.  
#' **Transform:** 
#' - Begins with the adjusted SEDS consumption data.
#' - Modifies source descriptions to align with those in the carbon factors dataset by handling specific naming conventions and sector codes.
#' - Joins with feedstock export adjustments for specific sources.
#' - Joins with miscellaneous adjustments to include IPPU adjustment factors.
#' - Updates NEU sources needing feedstock adjustments, ensuring non-negative values.
#' - Calculates carbon emissions:
#' - Joins with carbon factors data to get the carbon coefficients for each source.
#' - Calculates carbon emissions in MMT of CO2 by multiplying consumption by the appropriate carbon factor.
#' - Calculates specific emissions for NEU sources based on adjusted values and carbon factors, handling specific sources differently.
#' - Adjusts the final NEU emissions by accounting for storage factors, which reduce emissions based on the proportion of carbon stored rather than emitted.
#' **Collate/Output:** 
#' `carbon_emissions_state_neu`, a tibble used by
#'
#' @param general_data List created by `data_setup()` (keys: ghgi_values, variables, etc.)
#' @param state_ffc_adjusted List created by `state_ffc_adjust_data()`
#' @param carbon_coefficients List created by `get_carbon_factors()`
#' @param state_adjustments List created by `state_ffc_get_adjustments_data()`
#' @return Tibble with columns: state, year, sector_description, source_description, value …
#' @seealso [state_ffc_adjust_data()], [national_ffc_calculate_emissions()]
#' @examples
#' # minimal usage example
#' # state_ffc_get_seds_data(general_data)
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
      str_detect(source_description, "distillate ") ~ "distillate fuel oil",
      str_detect(source_description, "residual") ~ "residual fuel oil",
      .default = source_description
    )) %>%
    mutate(source_description = case_when(
      source_description == "coal" & sector_code == "CC" ~
        "commercial coal",
      source_description == "coal" & sector_code == "EI" ~
        "electric power coal",
      source_description == "coal" & sector_code == "OC" ~
        "industrial other coal",
      source_description == "coking coal" & sector_code == "KC" ~
        "coking coal",
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
              by = c("source_description", 
                     "sector_description", 
                     "year")) %>%
    left_join(state_adjustments$misc_adjustments %>% 
                select(year, cb_other_oil_adj), 
              by = "year") %>%
    mutate(feedstock_adjustment = replace_na(feedstock_adjustment, 0), 
           other_oils_adjustment = feedstock_adjustment + (-cb_other_oil_adj), 
           neu_ibf_adjusted_value = case_when(
             source_description == "other oils" ~
               pmax(0, adjusted_value + (other_oils_adjustment * (adjusted_value / states_sum_value))),
             source_description %in% 
               c("naphtha", 
                 "special naphtha") ~ 
               pmax(0, 
                    neu_ibf_adjusted_value + 
                      (feedstock_adjustment * 
                         (neu_ibf_adjusted_value / 
                            states_sum_value))), 
             .default = neu_ibf_adjusted_value)) %>%
    left_join(carbon_coefficients$carbon_factors,
              by = c("source_description", "year")
    ) %>%
    # MMT CO2  = btu * carbon factor/1000 * 44/12
    mutate(
      mmt_co2 = neu_ibf_adjusted_value *
        (carbon_factor / 1000) * carbon_coefficients$carbon_ratio,
      mmt_co2_neu = case_when(
        source_description %in% c("misc. products", 
                                  "asphalt & road oil", 
                                  "waxes", 
                                  "lubricants", 
                                  "coking coal") ~ adjusted_value  *
          (carbon_factor / 1000) * carbon_coefficients$carbon_ratio, 
        source_description %in% c("naphtha", 
                                  "special naphtha", 
                                  "other oils") ~ neu_ibf_adjusted_value  *
          (carbon_factor / 1000) * carbon_coefficients$carbon_ratio, 
        .default = mmt_co2
      ),
      # Restore original coal source descriptions
      source_description = case_when(
        str_detect(source_description, "(?<!coking )coal") ~ "coal",
        .default = source_description
      )
    )
  
  # Apply NEU storage factors where applicable
  carbon_emissions_state_ffc <- carbon_emissions_state_ffc %>%
    left_join(carbon_coefficients$neu_storage,
              by = c("source_description",
                     "sector_description",
                     "year")) %>%
    mutate(mmt_co2_neu = mmt_co2_neu * (1 - storage_factor))
  
  return(carbon_emissions_state_ffc)
  
}

# STATE FIGURES--------------------------------------
#' @description Creates figures of emissions and consumption data for the Inventory,including State-level Methodology, Chapter 2: Energy.
#' @details
#' **Retrieval:** 
#' - No new data retrieved in this function. 
#' **Transform:** 
#'  - Creates color palettes used in ggplot figures. 
#'  - In-work: Reshapes national FFC data 
#'  - Creates several tibbles from input data, selecting and reshaping where necessary to obtain appropriate data formats for plotting. 
#'  - Creates 9 figures (including 2 sub-components) with figure numbers corresponding to those in State-level Methodology, Chapter 2: Energy.
#' **Collate/Output:** 
#' `state_ffc_figures`, a list used by 
#'
#' @param seds_all_adjusted Tibble; list element of `state_ffc_adjusted`, created by `state_ffc_adjust_data()`
#' @param seds_ind_adjusted Tibble; list element of `state_ffc_adjusted`, created by `state_ffc_adjust_data()`
#' @param state_adjustments List created by `state_ffc_get_adjustments_data()`
#' @param carbon_emissions_state_ffc Tibble created by `state_ffc_calculate_emissions()`
#' @return Tibble with columns: state, year, sector_description, source_description, value …
#' @seealso [state_ffc_adjust_data()], [national_ffc_calculate_emissions()]
#' @examples
#' # minimal usage example
#' # state_ffc_get_seds_data(general_data)
state_ffc_ggplot_figures <- function(seds_all_adjusted,
                                     seds_ind_adjusted,
                                     state_adjustments,
                                     carbon_emissions_state_ffc) {
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
  
  # Read National Emissions Data (for State Figures)---------------------------
  
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
      states = arbon_emissions_state_ffc %>%
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


# STATE TABLES----------------------------------------------
#' @description Creates tables of emissions and consumption data for the Inventory,including State-level Methodology, Chapter 2: Energy.
#' @details
#' **Retrieval:** 
#' - No new data retrieved in this function. 
#' **Transform:** 
#'  - Creates several gt tables from input data, selecting and reshaping where necessary to obtain appropriate data formats. 
#'  - Creates 4 tables with numbers corresponding to those in State-level Methodology, Chapter 2: Energy.
#' **Collate/Output:** 
#' `state_ffc_tables`, a list used by 
#'
#' @param seds_all_adjusted List created by `state_ffc_adjust_data()`
#' @param carbon_emissions_state_ffc Tibble created by `state_ffc_calculate_emissions()`
#' @return `state_ffc_tables`, a list used by 
#' @seealso [state_ffc_adjust_data()], [national_ffc_calculate_emissions()]
#' @examples
#' # minimal usage example
#' # state_ffc_get_seds_data(general_data)
state_ffc_gt_tables <- function(seds_all_adjusted,
                                carbon_emissions_state_ffc) {
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



# INVDB--------------------------------------------------
#' @description This function processes carbon emissions data for U.S. territories and states, organizing it for output to the inventory database (InvDB) in Excel, CSV, and JSON formats.
#' @details
#' **Retrieval:** 
#' - No new data retrieved in this function. 
#' **Transform:** 
#' #' - Processes `carbon_emissions_territories` for FFC sources to create `ffc_territories_invdb`.
#' - Filters out certain sources and aggregates emissions data by grouping, summing, and pivoting wide by year.
#' - Processes `carbon_emissions_state_neu` for NEU sources to create `neu_territories_invdb`.
#' - Filters out certain sources and aggregates emissions data by grouping, summing, and pivoting wide by year.
#' #' - Processes `carbon_emissions_state_ffc` for NEU sources to create `ffc_territories_invdb`.
#' - Filters out certain sources and aggregates emissions data by grouping, summing, and pivoting wide by year.
#' - Adds fields to conform to InvDB formatting requirements.
#' - Joins all data into a single tibble, `ffc_invdb`.
#' **Collate/Output:** 
#' - Writes `ffc_invdb` to InvDB Excel template
#' - Writes `ffc_invdb` to JSON
#' - Writes `ffc_invdb` to CSV
#' @param carbon_emissions_territories Tibble created by `territories_ffc_adjust_data()`
#' @param carbon_emissions_state_ffc Tibble created by `state_neu_calculate_emissions()`
#' @param carbon_emissions_state_neu Tibble created by `state_ffc_calculate_emissions()`
#' @return NA (side effects only)
#' @seealso [state_ffc_adjust_data()], [national_ffc_calculate_emissions()]
#' @examples
#' # minimal usage example
#' # state_ffc_get_seds_data(general_data)
write_to_invdb <- function(
    # carbon_emissions_national, 
  carbon_emissions_territories, 
  carbon_emissions_state_ffc, 
  carbon_emissions_state_neu) {
  
  
  territories_invdb <- carbon_emissions_territories %>%  
    mutate(`Data Type` = "GHG", 
           Sector = "Energy",
           Category = "US Territories", 
           Subsector = "Fossil Fuel Combustion", 
           GeoRef = str_to_upper(state), 
           GHG = "CO2",
           Fuel1 = case_when(
             source_description == "coal" ~ "Coal", 
             source_description == "coking coal" ~ "Coal", 
             source_description == "natural gas" ~ "Natural Gas", 
             .default = "Petroleum")) %>%
    filter(!source_description %in% c("other petroleum liquids", 
                                      "lubricants")) %>%
    group_by(`Data Type`, Sector, Subsector,  Category, 
             Fuel1, GeoRef, GHG, Year = year) %>%
    summarize(mmt_co2 = sum(mmt_co2, na.rm = TRUE)) %>%
    arrange(desc(Year)) %>%
    pivot_wider(names_from = Year, values_from = mmt_co2)
  
  neu_territories_invdb <- carbon_emissions_territories %>% 
    # Create or modify fields to conform to InvDB
    mutate(`Data Type` = "GHG", 
           Sector = "Energy",
           Category = "US Territories", 
           Subsector = "Non-Energy Uses of Fossil Fuels",
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
  
  # NEU: naptha, s naphtha, other oil, misc, asphalt, waxes, lubr, coking coal,
  # other coal, nat gas, pet coke, diesel, still gas, lpg
  neu_invdb <-
    carbon_emissions_state_neu %>%
    # Create or modify fields to conform to InvDB
    mutate(`Data Type` = "GHG", 
           Sector = "Energy",
           Category = str_remove(sector_description, " sector") %>% 
             str_to_title(),
           Subsector = "Non-Energy Uses of Fossil Fuels",
           GeoRef = str_to_upper(state), 
           GHG = "CO2",
           Fuel1 = "", 
           mmt_co2 = mmt_co2_neu) %>%
    group_by(`Data Type`, Sector, Subsector,  Category, 
             Fuel1, GeoRef, GHG, Year = year) %>%
    summarize(mmt_co2 = sum(mmt_co2, na.rm = TRUE)) %>% 
    ungroup() %>%
    arrange(desc(Year)) %>%
    pivot_wider(names_from = Year, values_from = mmt_co2)
  
  ffc_invdb <-
    carbon_emissions_state_ffc %>%
    filter(source_description %in% c("natural gas", 
                                     "hgl (energy use)", "lpg", 
                                     "coking coal", "coal", 
                                     "kerosene", "motor gasoline", 
                                     "avgas blend components", "crude oil", 
                                     "mogas blend components", "petroleum coke", 
                                     "still gas", "geothermal energy", 
                                     "aviation gasoline", "jet fuel",
                                     "residual fuel oil", 
                                     "distillate fuel oil")) %>%
    # Create or modify fields to conform to InvDB
    mutate(Category = str_remove(sector_description, " sector") %>% 
             str_to_title(),
           GeoRef = str_to_upper(state), 
           Fuel1 = case_when(
             source_description %in% c("coal", "coking coal") ~ "Coal", 
             source_description == "geothermal energy" ~ "Geothermal",
             source_description == "natural gas" ~ "Natural Gas", 
             .default = "Petroleum")) %>%    # Select InvDB fields
    select(Category, Fuel1, GeoRef, Year = year, mmt_co2) %>%
    # Sum mmt CO2 for each Subsource/Fuel/State/Year
    group_by(Category, Fuel1, GeoRef, Year) %>%
    summarize(value = sum(mmt_co2, na.rm = TRUE)) %>%
    filter(Year != "1989") %>%
    # Pivot data wide so the years are columns
    pivot_wider(names_from = Year, values_from = value) %>%
    ungroup() %>%
    mutate('Data Type' = "GHG",
           Sector = "Energy", 
           Subsector = "Fossil Fuel Combustion", 
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


# ACTIVITY DATA--------------------------------------------------
#' One line summary in plain English
#'
#' @description What the function does in business terms (data in, data out).
#' @details
#' **Retrieval:** where data comes from (APIs/files)  
#' **Transform:** key steps (filters, joins, adjustments)  
#' **Collate/Output:** objects returned and how they’re used downstream
#'
#' @param general_data List created by `data_setup()` (keys: ghgi_values, variables, etc.)
#' @return Tibble with columns: state, year, sector_description, source_description, value …
#' @seealso [state_ffc_adjust_data()], [national_ffc_calculate_emissions()]
#' @examples
#' # minimal usage example
#' # state_ffc_get_seds_data(general_data)
write_ffc_activity <- function(
    # carbon_emissions_national, 
  carbon_emissions_territories, 
  carbon_emissions_state_ffc, 
  carbon_emissions_state_neu) {
  
  
  activity_ffc <- carbon_emissions_state_ffc %>%
    select(state, year, sector_description, source_description, 
           tbtu = neu_ibf_adjusted_value, carbon_factor)
  
  activity_ffc_territories <- carbon_emissions_territories %>%
    select(state, year, source_description, 
           tbtu, carbon_factor, storage_factor) %>%
    filter(!source_description %in% c("other petroleum liquids", 
                                      "lubricants"))
  
  activity_neu_territories <- carbon_emissions_territories %>%
    select(state, year, source_description, 
           tbtu, carbon_factor, storage_factor) %>%
    filter(source_description %in% c("other petroleum liquids", 
                                     "lubricants")) 
  
  # NEU: naptha, s naphtha, other oil, misc, asphalt, waxes, lubr, coking coal
  activity_neu_1 <- 
    carbon_emissions_state_ffc %>%
    filter(sector_description %in% c("industrial sector", 
                                     "transportation sector"), 
           msn %in% c("LUACB", "CLKCB", 
                      "ARICB",  "LUICB",  
                      "FOICB", "FNICB", "SNICB", 
                      "WXICB", "MSICB")) %>%
    select(state, year, sector_description, source_description, 
           tbtu = neu_ibf_adjusted_value, carbon_factor, storage_factor)
  
  # NEU: other coal, nat gas, pet coke, diesel, still gas, lpg
  activity_neu_2 <-
    carbon_emissions_state_neu %>%
    filter(msn %in% c("CLOCB", "net natural gas", "PCICB", 
                      "DFICB", "SGICB", "combined lpg"), 
           sector_description == "industrial sector") %>%
    select(state, year, sector_description, source_description, 
           tbtu = neu_adjusted_value, carbon_factor, storage_factor)
  
  activity_ffc <- bind_rows(
    activity_ffc,
    activity_neu_1, 
    activity_neu_2,
    activity_neu_territories, 
    activity_ffc_territories
  )
  
  # Save as csv
  write_csv(activity_ffc, "invDB/ffc_activity.csv")
}