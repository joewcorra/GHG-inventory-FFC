# Calculate Motor Gasoline Adjustments
# Applies to Commercial, Industrial, Transportation

# Read MOVE Data---------------------------------------------------------

moves <- read_excel("data/moves3.xlsx", sheet = 1) %>%
  clean_names() %>%
  pivot_longer(cols = starts_with("x"), 
               values_to = "vmt_percent", names_to = "year") %>%
  left_join(read_excel("data/moves3.xlsx", sheet = 2) %>% 
              clean_names() %>%
              pivot_longer(cols = starts_with("x"), 
                           values_to = "fuel_use_percent", names_to = "year"), 
            by = c("vehicle_type", "year")) %>%
  mutate(year = str_sub(year, 2, 5), 
         fuel_type = case_when(
           vehicle_type %in% c("MC", "LDGV", "LDGT", 
                               "HDGV", "HDGB") ~ "gasoline",
           vehicle_type %in% c("LDDV", "LDDT", 
                               "HDDT", "HDDB") ~ "diesel"))
  


# EIA Mogas------------------------------------------------------------

# For each: mogas_com, mogas_ind, mogas_tra

# ethanol correction for 1990-92 only
# ethanol_correction <- biomass hidden sheet data  
# Placeholder value for now
ethanol_correction <- 0

us_consumption_mogas <- national_ffc_data$us_consumption %>% 
  filter(msn %in% c("MGCCB", "MGACB", "MGICB")) %>%
  mutate(mogas_ethanol_corrected = (value + ethanol_correction) / 0.001) 


# Total On-Road Mogas-----------------------------------------------------


# Total ethanol varies annually; obtained from Biomass workbook, ethanol sheet
total_ethanol <- tra_ethanol_value * 0.001

# Gasoline joules per gallon. Fixed value
mogas_energy <- 43488 * 2839


# Calculate total annual on-road gasoline consumption (including ethanol)
mogas_annual_totals <- moves3_ratio %>%
  mutate(onroad_mogas_incl_ethanol = sum(
    onroad_mogas_by_vehicle) / 10^9, .by = year) %>%
  # Get non-ethanol total by subtracting the ethanol 
  mutate(onroad_mogas_excl_ethanol_v1 = 
           onroad_mogas_incl_ethanol - total_ethanol / 10^3) %>%
  # non-ethanol total version 2
  mutate(onroad_mogas_excl_ethanol_v2 = 
           (onroad_mogas_excl_ethanol_v1 / onroad_mogas_incl_ethanol) * 
           (onroad_mogas_by_vehicle / 10^9)) %>%
  group_by(year) %>%
  summarize(onroad_mogas_excl_ethanol_v1 = mean(onroad_mogas_excl_ethanol_v1), 
            onroad_mogas_excl_ethanol_v2 = sum(onroad_mogas_excl_ethanol_v2), 
            onroad_mogas_incl_ethanol = mean(onroad_mogas_incl_ethanol))

# total_onroad_mogas_excl_ethanol is calculated twice, by two different
# methods(?). The two results are identical (when v2 is summed)


# Calculate total on-road motor gasoline consumption in order to get the 
# total NON-road consumption 

# Placeholder values until we obtain data
fhwa_gas_consumed <- 3
nonroad_lawn_garden <- 1
nonroad_recreation <- 1
tra_ethanol_value <- 1

# Total Nonroad Mogas--------------------------------------------------

# Total non-road motor gasoline use
total_nonroad_mogas <- us_consumption_mogas %>% 
  group_by(year) %>%
  # Get total EIA mogas by year (tra + com + ind)
  summarize(mogas_ethanol_corrected = sum(mogas_ethanol_corrected)) %>%
  ungroup() %>%
  # Get MOVES3 on-road mogas totals
  left_join(mogas_annual_totals %>% 
              select(year, onroad_mogas_excl_ethanol_v2), by = "year") %>%
  # Annual non-road mogas = total mogas - on-road mogas
  mutate(nonroad_mogas = mogas_ethanol_corrected - onroad_mogas_excl_ethanol_v2)

# Rec Boat Mogas-------------------------------------------------------

# Recreational boat motor gasoline total is the smaller of 1) rec boat gas 
# calculated by the bottom-up method, or 2) total non-road motor gasoline

# Placeholder values
# These data come from [Nonroad] workbook. Awaiting data access.
nonroad_2_stroke <- 1
nonroad_4_stroke <- 2

# First compute rec boat mogas by the bottom-up method
rec_boat_mogas_bottom_up <- heat_content %>% 
  # Motor gasoline only 
  filter(str_detect(msn_description, "asoline")) %>%
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
                              na.rm = TRUE)) %>%
  select(year, rec_boat_mogas)

# Motor Gasoline Adjustments---------------------------------------------

mogas <- us_consumption_mogas %>% 
  # Join EIA consumption data with the MOVES3 annual results
  left_join(mogas_annual_totals %>% 
              # Only a few columns are needed now
              select(year, onroad_mogas_excl_ethanol_v1, 
                     onroad_mogas_excl_ethanol_v2, 
                     onroad_mogas_incl_ethanol), 
            by = "year") %>%
  # Create 'meta sector' to differentiate transport from non-transport
  mutate(meta_sector = case_when(
    sector_description == "commercial sector" ~ "non-trans", 
    sector_description == "industrial sector" ~ "non-trans",
    sector_description == "transportation sector" ~ "trans")) %>%
  # Non-trans mogas total = total non-trans mogas - on-road - rec boats
  mutate(remaining_mogas = case_when(
    meta_sector == "non-trans" ~ sum(mogas_ethanol_corrected) - 
      (onroad_mogas_excl_ethanol_v2 + rec_boat_mogas),
    .default = mogas_ethanol_corrected), .by = year) %>%
  
  mutate(mogas_adjusted = case_when(
    # Com or ind = remaining mogas value * EIA mogas / sum of ind + com mogas 
    meta_sector == "non-trans" ~ 
      remaining_mogas * mogas_ethanol_corrected / sum(mogas_ethanol_corrected),
    # Transportation = on-road total + rec boat total
    meta_sector == "trans" ~ 
      onroad_mogas_excl_ethanol_v1 + rec_boat_mogas), .by = c(year, meta_sector)) 





