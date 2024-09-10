# FOKS DATA

# Fuel Oil and Kerosene Sales for IBF ADjustmentes

# Note that FOKS data is no longer available as of 2020. Consider removing(?)

print("Retrieving FOKS data.")

# Objects Created--------------------------------------------------------


# List of objects created in the global environment:



# Read Excel Data--------------------------------------------------------

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
  mutate(diesel_percent = diesel_percent / national_total) %>%
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
  mutate(residual_percent = residual_percent / national_total) %>%
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



# Cleanup-----------------------------------------------------------------

# Append the new tibble to the existing 'corrections' list
corrections <- append(corrections, lst(foks_diesel_distribution, 
                                       foks_residual_distribution))


rm(list = c("foks_diesel_distribution","foks_residual_distribution"))
