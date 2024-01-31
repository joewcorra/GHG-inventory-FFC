# IPPU DISTRIBUTION DATA

# Distribution of Emissions from Iron and Steel, Ammonia, & Petrochemicals


# Objects Created--------------------------------------------------------


# List of objects created in the global environment:
# is_distribution: tibble; iron and steel % dist by state + national
# ammonia_distribution: tibble; ammonia % dist by state + national
# petrochemicals_distribution: tibble; petrochemical % dist by state + national

# All used by industrial_adjustments.R


# Read Excel Data--------------------------------------------------------

# Read in NEU data from FFC excel workbook
is_distribution <- read_excel("ippu_i&s_percent_2021.xlsx", 
                              sheet = 1, 
                              skip = 0, range = "A2:AJ54") %>%
  clean_names() %>%
# Don't need the national value; we compute it below
  filter(state != "National") %>%
  # Keep only state codes and value by year
  select(state, starts_with("x")) %>%
  # Make data long; i.e., one row per year
  pivot_longer(cols = -1, names_to = "year", values_to = "is_percent") %>%
  # Remove letters from year column 
  mutate(year = str_remove(year, "[a-z]"),
         # Get national total for each year by insta-grouping
         national_total = sum(is_percent), .by = year) %>%
  # Get I & S percentage for each state 
  mutate(is_percent = is_percent / national_total) %>%
  # No longer need national total
  select(-national_total)


ammonia_distribution <- read_excel("ippu_ammonia_2021.xlsx", 
                              sheet = 1, 
                              skip = 0, range = "A2:AJ54") 


petrochemicals_distribution <- read_excel("ippu_petrochemicals_2021.xlsx", 
                              sheet = 1, 
                              skip = 0, range = "A2:AJ54") 
