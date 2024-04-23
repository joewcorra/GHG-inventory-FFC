# Calculate Motor Gasoline Adjustments
# Applies to Commercial, Industrial, Transportation


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# EIA Mogas------------------------------------------------------------

# For each: mogas_com, mogas_ind, mogas_tra

# ethanol correction for 1990-92 only
# ethanol_correction <- biomass hidden sheet data  
# Placeholder value for now
ethanol_correction <- 0

us_consumption_mogas <- us_consumption %>% 
  filter(msn %in% c("MGCCB", "MGACB", "MGICB")) %>%
  mutate(mogas_ethanol_corrected = (value + ethanol_correction) / 0.001) 


# Total On-Road Mogas-----------------------------------------------------

# Calculate total on-road motor gasoline consumption in order to get the 
# total NON-road consumption 

# Placeholder values until we obtain data
fhwa_gas_consumed <- 3
nonroad_lawn_garden <- 1
nonroad_recreation <- 1
tra_ethanol_value <- 1
# Total ethanol varies annually; obtained from Biomass workbook, ethanol sheet
total_ethanol <- tra_ethanol_value * 0.001

# Gasoline joules per gallon. Fixed value
mogas_energy <- 43488 * 2839

# Moves3 ratio is vehicle-specific
# 'pivots' data comes from emis_g_or_ener_j table--need to ask Sarah
# pivots data varies by year and vehicle class
# Here's the table structure w/ fake data for one year only
moves3_ratio <- tibble(year = "1992", 
                       vehicle_class = c("motorcycle", "automobile", 
                                         "light_truck", "light_truck", 
                                         "other_truck", "other_truck", 
                                         "other_truck",  "other_truck", 
                                         "other_truck", "other_truck",
                                         "bus"), 
                       vehicle_sub_class = c("motorcycle", "automobile", 
                                             "light_truck_1", "light_truck_2",
                                             "other_truck_1", "other_truck_2", 
                                             "other_truck_3", "other_truck_4", 
                                             "other_truck_5", "other_truck_6",
                                             "bus"), 
                       # Fake number for now
                       gallons_used = c(500, 18900, 5000, 4900, 1400, 1300, 
                                        1200, 1100, 1000, 1100, 800)) %>%
  group_by(vehicle_class, year) %>%
  summarize(gallons_used = sum(gallons_used)) %>%
  ungroup() %>%
  left_join(heat_content %>% filter(msn == "MGTCKUS"), by = "year") %>%
  mutate(moves3_ratio = (gallons_used / mogas_energy / 1000) / 
           sum(gallons_used), 
         gas_consumption_gal_by_vehicle = moves3_ratio * 
           (fhwa_gas_consumed - nonroad_lawn_garden - nonroad_recreation), 
         onroad_mogas_by_vehicle = heat_content * 
           (gas_consumption_gal_by_vehicle / 42))

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



# Motor Gasoline Adjustments---------------------------------------------

# commercial, as an example
mogas <- us_consumption_mogas %>% 
  left_join(mogas_annual_totals %>% 
              select(year, onroad_mogas_excl_ethanol_v1, 
                     onroad_mogas_excl_ethanol_v2, 
                     onroad_mogas_incl_ethanol), 
            by = "year") %>%
  mutate()
  
# START HERE 4/24/2024
    # C76
    (sum(mogas_tra, mogas_com, mogas_ind) - 
                     total_onroad_mogas_excl_ethanol_v2 - 
                     boat_mogas_adj) / 
  # C77 or C78
  sum(mogas_com, mogas_ind)  * 10^3


# Rec Boat Mogas-------------------------------------------------------

boat_mogas_adj <- min(total_nonroad_mogas, boat_mogas_bottom_up)
  
boat_mogas_bottom_up <- (heat_content * ((nonroad_2_stroke + nonroad_4_stroke) / 42)) / 10^9

# Total Nonroad Mogas--------------------------------------------------

total_nonroad_mogas <- sum(
  mogas_tra, mogas_com, mogas_ind) - 
  total_onroad_mogas_excl_ethanol_v2
  



