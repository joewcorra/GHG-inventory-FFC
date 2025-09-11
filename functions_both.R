
# MSN LOOKUP------------------------------------

lookup_msn <- function() {
  
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
  
  
  return(msn_lookup)
}

# CARBON FACTORS-----------------------------------
#' Prepare carbon factors, storage fractions, and the CO2/C ratio
get_carbon_factors <- function(carbon_factors_variable,
                               carbon_factors_fixed,
                               neu_storage) {
  
  # Ratio of the molecular weight of carbon dioxide to carbon
  carbon_ratio <- 44/12
  
  # Read in variable carbon factors data from FFC excel workbook
  carbon_factors_variable <- carbon_factors_variable %>%
    tanagerharmonize::pre_clean() %>%
    rename(source_description = fuel_type) 
  
  # Read in carbon factors data from FFC excel workbook
  carbon_factors <- carbon_factors_fixed %>%
    tanagerharmonize::pre_clean() %>%
    select(source_description = fuel_type, carbon_coefficient) %>%
    filter(
      # NA = not applicable, NC = not calculated. Remove all NA & NC
      !is.na(carbon_coefficient),
      carbon_coefficient != "nc"
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
    ) %>%
    # Remove rows w/ NA values (these are mostly header rows)
    filter(!is.na(carbon_factor))
  
  neu_storage <- neu_storage %>%
    tanagerharmonize::pre_clean() %>%
    rename(sector_description = sector,
           source_description = source) %>%
    pivot_longer(cols = starts_with("x"),
                 names_to = "year",
                 values_to = "storage_factor")
  
  carbon_coefficients <- lst(carbon_factors,
                             carbon_ratio,
                             neu_storage)
  
  return(carbon_coefficients)
  
}
# DATASCRAPING (NATIONAL AND STATE)-----------------------------------------

scrape_fhwa_data <- function(data_dictionary_values) {
  
  # Set year to match most recent available year (current year minus two)
  latest_year <- year(Sys.Date()) - 2
  
  states <- data_dictionary_values %>% 
    filter(value_variable == "state") %>%
    select(state = value, state_name = value_description)

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
    left_join(states,
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
  # NOTE: NOt sure if this is the right data; see issues in Github
  # Temporary file storage path
  # local_excel_path <- tempfile(fileext = ".xlsx")
  #
  # diesel_url <- paste0(
  #   "https://www.fhwa.dot.gov/policyinformation/statistics/",
  #   latest_year, "/xls/vm1.xlsx"
  # )
  # # https://www.fhwa.dot.gov/policyinformation/statistics/1998/vm1.cfm
  # GET(diesel_url, write_disk(local_excel_path, overwrite = TRUE))
  #
  # diesel_use_by_class <- read_excel(local_excel_path, skip = 5) %>%
  #   clean_names() %>%
  #   # Make all value columns numeric
  #   mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
  #   # Make data long; i.e., one row per year
  #   pivot_longer(
  #     cols = -1, names_to = "year",
  #     values_to = "gasoline_use_gal"
  #   ) %>%
  #   # Retain only year and value
  #   select(year, gasoline_use_gal) %>%
  #   # Remove letters from year column
  #   mutate(year = str_remove(year, "[a-z]"))
  # # Retain only 1990 onward
  
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
      states, 
      by = "state_name"
    ) %>%
    # No longer need national total or full state name
    select(-national_total, -state_name) %>%
    # NOTE: diesel dist value for OR 2018 missing; interpolated instead
    mutate(diesel_percent = if_else(state == "OR" & year == "2018",
                                    0.0139, diesel_percent))
  
  diesel_use_national <- read_excel(local_excel_path) %>%
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
      values_to = "diesel_use_gal"
    ) %>%
    # Retain only year and value
    select(year, diesel_use_gal) %>%
    # Remove letters from year column
    mutate(year = str_remove(year, "[a-z]")) %>%
    # Retain only 1990 onward
    filter(year >= 1990) %>%
    group_by(year) %>%
    summarize(diesel_use_gal = sum(diesel_use_gal, na.rm = TRUE)) %>%
    ungroup()
  
  fhwa_data <- lst(
    diesel_distribution,
    # diesel_use_by_class,
    diesel_use_national,
    gasoline_distribution,
    gasoline_use_national
  )
  
  return(fhwa_data)
  
}
  

# STANDARDIZE DATA-------------------------------------------

standardize_ffc <- function (data, msn_eia) {

  standardized_data <- data %>%
    tanagerharmonize::pre_clean()

  if ("sector_description" %in% colnames(standardized_data)) {
    # Make 'year' a factor
    standardized_data <- standardized_data %>%
      
             # Standardize sector descriptions; add the word "sector"
             mutate(sector_description = case_when(
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
          source_description %in% unique(msn_eia$source_description) ~ source_description,
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

