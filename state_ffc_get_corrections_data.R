# Retrieve national FFC data for adjustments

state_ffc_get_corrections_data <- function() {
  
  # Read in adjustment factors data, derived from national inventory
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
  
  # Apply labels to variables
  adjustments <- apply_variable_labels(adjustments)
  
  
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
  
  # Apply labels to variables
  national_corrections <- apply_variable_labels(national_corrections)
  
  
  # Consumption input is the 'US compare' data with corrections factors applied. 
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
  
  # Apply labels to variables
  consumption_input <- apply_variable_labels(consumption_input)
  
  print("This generates a warning about NA values.")
  print("Ignore this warning. These values are not used.")
  
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
  
  # Apply labels to variables
  ibf_corrections <- apply_variable_labels(ibf_corrections) 
  
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
  
  # Apply labels to variables
  neu_corrections <- apply_variable_labels(neu_corrections)
  
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
  
  # Apply labels to variables
  is_distribution <- apply_variable_labels(is_distribution)
  ammonia_distribution <- apply_variable_labels(ammonia_distribution)
  petrochemicals_distribution <- apply_variable_labels(petrochemicals_distribution)
  petrochemicals_cb_distribution <- apply_variable_labels(petrochemicals_cb_distribution)
  
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
  
  # Apply labels to variables
  foks_diesel_distribution <- apply_variable_labels(foks_diesel_distribution)
  
  
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
  
  # Apply labels to variables
  foks_residual_distribution <- apply_variable_labels(foks_residual_distribution)
  
  # Aggregate
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