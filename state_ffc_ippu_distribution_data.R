# IPPU DISTRIBUTION DATA

# Distribution of Emissions from Iron and Steel, Ammonia, & Petrochemicals


# Objects Created--------------------------------------------------------


# List of objects created in the global environment:
# is_distribution: tibble; iron and steel % dist by state + national
# ammonia_distribution: tibble; ammonia % dist by state + national
# petrochemicals_distribution: tibble; petrochemical % dist by state + national

# All used by industrial_adjustments.R

print("Retrieving IPPU data for industrial adjustments.")

# Read Excel Data--------------------------------------------------------

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


# Cleanup-----------------------------------------------------------------

# Append the new tibble to the existing 'corrections' list
corrections <- append(corrections, lst(is_distribution, 
                                       ammonia_distribution, 
                                       petrochemicals_distribution, 
                                       petrochemicals_cb_distribution))


rm(list = c("is_distribution", "ammonia_distribution",
            "petrochemicals_distribution", "petrochemicals_cb_distribution"))

