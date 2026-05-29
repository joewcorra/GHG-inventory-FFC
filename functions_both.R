
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
    syrinx::pre_clean() %>%
    rename(source_description = fuel_type)

  carbon_factors <- carbon_factors_fixed %>%
    syrinx::pre_clean() %>%
    select(source_description = fuel_type, carbon_coefficient) %>%
    filter(
      !is.na(carbon_coefficient),
      carbon_coefficient != "nc"
    ) %>%
    left_join(carbon_factors_variable, by = "source_description") %>%
    mutate(across(
      starts_with("x"),
      ~ ifelse(carbon_coefficient == "variable", ., carbon_coefficient)
    )) %>%
    select(-carbon_coefficient) %>%
    mutate(
      year = str_remove(year, "x"),
      carbon_factor = as.numeric(value)
    ) %>%
    select(-value) %>%
    filter(!is.na(carbon_factor))
  
  neu_storage <- neu_storage %>%
    syrinx::pre_clean() %>%
    rename(sector_description = sector,
           source_description = source,
           storage_factor = value)

  carbon_coefficients <- lst(carbon_factors,
                             carbon_ratio,
                             neu_storage)
  
  return(carbon_coefficients)
  
}
# DATASCRAPING (NATIONAL AND STATE)-----------------------------------------

scrape_fhwa_data <- function(data_dictionary_values) {

  latest_year <- year(Sys.Date()) - 2

  states <- data_dictionary_values %>%
    filter(value_variable == "state") %>%
    select(state = value, state_name = value_description)

  local_excel_path <- tempfile(fileext = ".xlsx")

  gasoline_url <- paste0(
    "https://www.fhwa.dot.gov/policyinformation/statistics/",
    latest_year, "/xls/mf226.xlsx"
  )
  special_fuel_url <- paste0(
    "https://www.fhwa.dot.gov/policyinformation/statistics/",
    latest_year, "/xls/mf225.xlsx"
  )
  
  GET(gasoline_url, write_disk(local_excel_path, overwrite = TRUE))

  gasoline_distribution <- read_excel(local_excel_path) %>%
    clean_names() %>%
    filter(!is.na(state), state != "Total") %>%
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    pivot_longer(cols = -1, names_to = "year", values_to = "gasoline_percent") %>%
    mutate(
      state = str_squish(state),
      year = str_remove(year, "[a-z]"),
      national_total = sum(gasoline_percent, na.rm = TRUE), .by = year
    ) %>%
    filter(year > 1989) %>%
    mutate(
      gasoline_percent = gasoline_percent / national_total,
      # "Dist. of Col." abbreviation used in FHWA data
      state = if_else(str_detect(state, "Dist"), "District of Columbia", state)
    ) %>%
    rename(state_name = state) %>%
    left_join(states, by = "state_name") %>%
    select(-national_total, -state_name)
  

  gasoline_use_national <- read_excel(local_excel_path) %>%
    clean_names() %>%
    filter(state == "Total") %>%
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    pivot_longer(cols = -1, names_to = "year", values_to = "gasoline_use_gal") %>%
    mutate(year = str_remove(year, "[a-z]")) %>%
    filter(year > 1989) %>%
    select(-state)

  GET(special_fuel_url, write_disk(local_excel_path, overwrite = TRUE))

  diesel_distribution <- read_excel(local_excel_path) %>%
    clean_names() %>%
    filter(!is.na(state), state != "Total") %>%
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    pivot_longer(cols = -1, names_to = "year", values_to = "diesel_percent") %>%
    mutate(
      state = str_squish(state),
      year = str_remove(year, "[a-z]"),
      national_total = sum(diesel_percent, na.rm = TRUE), .by = year
    ) %>%
    filter(year > 1989) %>%
    mutate(
      diesel_percent = diesel_percent / national_total,
      # "Dist. of Col." abbreviation used in FHWA data
      state = if_else(str_detect(state, "Dist"), "District of Columbia", state)
    ) %>%
    rename(state_name = state) %>%
    left_join(states, by = "state_name") %>%
    select(-national_total, -state_name) %>%
    # OR 2018 missing from source; interpolated
    mutate(diesel_percent = if_else(state == "OR" & year == "2018",
                                    0.0139, diesel_percent))
  
  diesel_use_national <- read_excel(local_excel_path) %>%
    clean_names() %>%
    filter(!is.na(state), state != "Total") %>%
    mutate(across(starts_with("x"), ~ as.numeric(.))) %>%
    pivot_longer(cols = -1, names_to = "year", values_to = "diesel_use_gal") %>%
    select(year, diesel_use_gal) %>%
    mutate(year = str_remove(year, "[a-z]")) %>%
    filter(year >= 1990) %>%
    group_by(year) %>%
    summarize(diesel_use_gal = sum(diesel_use_gal, na.rm = TRUE)) %>%
    ungroup()

  fhwa_data <- lst(
    diesel_distribution,
    diesel_use_national,
    gasoline_distribution,
    gasoline_use_national
  )
  
  return(fhwa_data)
  
}
  

# STANDARDIZE DATA-------------------------------------------

standardize_ffc <- function (data, msn_eia) {

  standardized_data <- data %>%
    syrinx::pre_clean()

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

