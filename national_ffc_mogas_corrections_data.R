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


us_consumption_mogas <- national_ffc_data$us_consumption %>% 
  filter(msn %in% c("MGCCB", "MGACB", "MGICB")) %>%
  mutate(mogas_ethanol_corrected = value / 0.001)

us_consumption_diesel <- national_ffc_data$us_consumption %>% 
  filter(msn %in% c("DFACB", "DFCCB", "DFICB", "DFRCB", "DKEIB")) 

# Total On-Road Mogas-----------------------------------------------------


# Gasoline joules per gallon. Fixed value
mogas_energy <- 43488 * 2839
# Nonroad : For now I'm using 1990 values as placeholders
nonroad_lawn_garden <-  2280381389 
nonroad_recreational <-  725907978 


# THIS WORKS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

mogas <- moves %>%
  filter(fuel_type == "gasoline") %>%
  left_join(fhwa_scraped$gasoline_use_national, by = "year") %>%
  left_join(national_ffc_data$heat_content %>% 
              filter(msn == "MGTCKUS") %>% 
              select(year, heat_content), 
            by = "year") %>%
  left_join(national_ffc_data$ethanol_tra, by = "year") %>%
  mutate(nonroad_lawn_garden = nonroad_lawn_garden, 
         nonroad_recreational = nonroad_recreational, 
         gas_use = fuel_use_percent * (gasoline_use_gal * 1000 - nonroad_lawn_garden - nonroad_recreational), 
         tbtu = (gas_use / 42 * heat_content) / 10^9) %>%
  mutate(tbtu_sum = sum(tbtu), .by = year) %>%
  mutate(ethanol_adjustment_factor = 1 - (ethanol / 1000 / tbtu_sum), 
         tbtu_adjusted = tbtu * ethanol_adjustment_factor)

# Incomplete as of 10/3/24
diesel <- moves %>%
  filter(fuel_type == "diesel") %>%
  # This might not be the right diesel data..........
  left_join(fhwa_scraped$diesel_use_by_class, by = "year") %>%
  left_join(national_ffc_data$heat_content %>% 
              filter(msn == "DMTCKUS") %>% 
              select(year, heat_content), 
            by = "year") 

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
      onroad_mogas_excl_ethanol_v1 + rec_boat_mogas), 
    .by = c(year, meta_sector)) 





