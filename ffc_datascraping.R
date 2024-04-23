# Datascraping


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Set Year---------------------------------------------------------------

# Change to match most recent available year (current year minus two)
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
  mutate(gasoline_percent = gasoline_percent / national_total) %>%
  # Get state codes
  left_join(state_name_key, by = c("state" = "state_names")) %>%
  # No longer need national total or full state name
  select(-national_total, -state) %>%
  rename (state = states_and_dc)
  

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
  mutate(diesel_percent = diesel_percent / national_total) %>%
  # Get state codes
  left_join(state_name_key, by = c("state" = "state_names")) %>%
  # No longer need national total or full state name
  select(-national_total, -state) %>%
  rename (state = states_and_dc)

# Scrape FWHA Fuel Use National FFC---------------------------------------


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

# ------------------------------------------------------------------------

# URL for table VM-1, diesel fuel by class
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
  # Remove letters from year column 
  mutate(year = str_remove(year, "[a-z]"))
  # Retain only 1990 onward