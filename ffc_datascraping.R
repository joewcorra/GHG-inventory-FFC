# Datascraping

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Set Year---------------------------------------------------------------

# Change to match most recent available year (current year minus two)
latest_year <- year(Sys.Date()) -2

# Scrape FWHA Fuel Use (State & National FFC) ----------------------------

print("Datascraping: DOT FWHA data.")

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

# Add variable labels
gasoline_distribution <- apply_variable_labels(gasoline_distribution) 

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

# Apply metadata labels to variables 
diesel_distribution <- apply_variable_labels(diesel_distribution)

# Validate Data-----------------------------------------------------------

# Apply FHA validation functions to diesel and gasoline distribution data

# validation_fha_raw(gasoline_distribution)
# 
# validation_fha_raw(diesel_distribution)

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

# Apply metadata labels to variables 
gasoline_use_national <- apply_variable_labels(gasoline_use_national)

# Scrape EPA Flight solid waste combustion data---------------------------

library(tidyverse) 
library(janitor) # assorted functions for cleaning up data frames
library(readxl) # Read Excel files
library(httr) # working with HTML
library(rvest) # working with HTML

# Temporary file storage path
local_excel_path <- tempfile(fileext = ".xls")

# URL for SWC data by year and facility
swc_url <- paste0(
  "https://ghgdata.epa.gov/ghgp/service/export?q=&tr=current&ds=E&ryr=2023&cyr=2023&lowE=-20000&highE=23000000&st=&fc=&mc=&rs=ALL&sc=0&is=11&et=&tl=&pn=undefined&ol=0&sl=0&bs=&g1=1&g2=1&g3=1&g4=1&g5=1&g6=0&g7=1&g8=1&g9=1&g10=1&g11=1&g12=1&s1=0&s2=1&s3=0&s4=0&s5=0&s6=0&s7=0&s8=0&s9=0&s10=0&s201=0&s202=0&s203=0&s204=1&s301=0&s302=0&s303=0&s304=0&s305=0&s306=0&s307=0&s401=0&s402=0&s403=0&s404=0&s405=0&s601=0&s602=0&s701=0&s702=0&s703=0&s704=0&s705=0&s706=0&s707=0&s708=0&s709=0&s710=0&s711=0&s801=0&s802=0&s803=0&s804=0&s805=0&s806=0&s807=0&s808=0&s809=0&s810=0&s901=0&s902=0&s903=0&s904=0&s905=0&s906=0&s907=0&s908=0&s909=0&s910=0&s911=0&sf=11001100&allReportingYears=yes&listExport=false")

# Retrieve SWC Excel file data
GET(swc_url, write_disk(local_excel_path, overwrite = TRUE))

# Read from temp file 
# The top six lines are blank in this worksheet, so we'll skip them 
swc <- read_excel(local_excel_path, skip = 6) %>%
  # Clean up column names/apply snake-case style. 
  clean_names() 

# Apply variable labels 
swc <- apply_variable_labels(swc)

# ------------------------------------------------------------------------

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

# Apply variable labels 
diesel_use_by_class <- apply_variable_labels(diesel_use_by_class)

# Cleanup-------------------------------------------------------------------

fhwa_scraped <- lst(
  diesel_distribution,
  diesel_use_by_class, 
  gasoline_distribution,
  gasoline_use_national)

# Remove unneeded objects from global environment
rm(list = c("diesel_distribution", "diesel_use_by_class", 
     "gasoline_distribution",  "gasoline_use_national", "local_excel_path", 
     "special_fuel_url", "diesel_url", "gasoline_url", "swc_url"))

