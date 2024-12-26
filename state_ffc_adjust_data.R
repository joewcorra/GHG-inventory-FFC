# Functionalized script for 'targets'
# Perform all state data adjustments

  
state_ffc_adjust_data <- function(seds, corrections) {
  
# Residential
seds_res_adjusted <- lst(
  
  coal = seds %>%
    filter(msn == "CLRCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments,
              by = c("source_description", "year", "sector_description")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Deal with zeroes in the sums to avoid NaNs
    mutate(states_sum_value = if_else(
      states_sum_value == 0, 1, states_sum_value), 
      # Multiply adjustment factor by states's value / the above sum
      adjusted_value = national_value * 
        (value / states_sum_value)), 
  
  natural_gas = seds %>% 
    # Separate list element required to find net natural gas
    filter(msn %in% c("NGRCB", "SFRCB")) %>%
    # Subtract supplemental gas from total natural gas
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Supplemental gas no longer needed (and value is now duplicative)
    filter(msn != "SFRCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments,  
              by = c("source_description", "year", "sector_description")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = national_value * 
             (value / states_sum_value)) %>%
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net natural gas", 
           source_description = "natural gas"),
  
  distillate_fuel = seds %>%
    filter(msn == "DFRCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = national_value * 
             (value / states_sum_value)), 
  
  # LPGs (propane and/or HGL)
  lpg = seds %>%
    filter(case_when(as.integer(year) < 2010 ~ msn == "HLRCB", 
                     as.integer(year) >= 2010 ~ msn == "PQRCB")) %>% 
    # Adjusted = original value
    mutate(msn = "combined lpg", 
           adjusted_value = value), 
  
  # All other sources go in the last list element
  other_residential = seds %>% 
    filter(msn %in% c("KSRCB")) %>%
    mutate(adjusted_value = value)) %>%
  
  # Collapse list into a single data frame
  list_rbind()

# Commercial
seds_com_adjusted <- lst(
  
  coal = seds %>%
    filter(msn == "CLCCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = national_value * 
             (value / states_sum_value)), 
  
  distillate_fuel = seds %>%
    filter(msn == "DFCCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = national_value * 
             (value / states_sum_value)), 
  
  natural_gas = seds %>% 
    # Separate list element required to find net natural gas
    filter(msn %in% c("NGCCB", "SFCCB")) %>%
    # Subtract supplemental gas from total natural gas
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Supplemental gas no longer needed (and value is now duplicative)
    filter(msn != "SFCCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = national_value * 
             (value / states_sum_value)) %>%
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net natural gas", 
           source_description = "natural gas"),
  
  gasoline = seds %>% 
    # Separate list element required to find net gasoline
    filter(msn %in% c("MGCCB", "EMCCB")) %>%
    # Subtract ethanol from total gasoline
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Ethanol no longer needed (and value is now duplicative)
    filter(msn != "EMCCB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments , 
              by = c("source_description", "year", "sector_description")) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = national_value * 
             (value / states_sum_value)) %>%
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net gasoline", 
           source_description = "motor gasoline"),
  
  # LPGs (propane and/or HGL)
  lpg = seds %>%
    filter(case_when(as.integer(year) < 2010 ~ msn == "HLCCB", 
                     as.integer(year) >= 2010 ~ msn == "PQCCB")) %>% 
    # Adjusted = original value
    mutate(msn = "combined lpg", 
           adjusted_value = value), 
  
  # All other sources go in the last list element
  other_commercial = seds %>% 
    filter(msn %in% c("KSCCB", "PCCCB", "RFCCB")) %>%
    mutate(adjusted_value = value)) %>%
  
  # Collapse list into a single data frame
  list_rbind()

# Industrial
seds_ind_adjusted <- lst(
  
  # Coking Coal
  coking_coal = seds %>%
    filter(msn == "CLKCB") %>%
    # Join with national data corrections
    left_join(corrections$national_corrections, by = "year") %>%
    mutate(states_sum_value = sum(value), .by = c(msn, year),
           # ippu percent = ippu / sum_states_value as long as ippu is greater
           ippu_factor = if_else(ippu < states_sum_value,
                                 ippu / states_sum_value, 1),
           adjusted_value = value - (value * ippu_factor), 
           # Standardize sector descriptions across all industrial sources
           sector_description = "industrial sector"),
  
  # Other Coal
  other_coal = seds %>%
    filter(msn == "CLOCB") %>%
    # Standardize sector descriptions across all industrial sources
    mutate(sector_description = "industrial sector") %>%
    left_join(coking_coal %>%
                select(sum_coking_coal = states_sum_value,
                       coking_coal_value = value, year, state),
              by = c("year", "state")) %>%
    # Join with national data corrections
    left_join(corrections$national_corrections, by = "year") %>%
    # Join with consumption input data
    left_join(corrections$consumption_input,
              by = c("year", "source_description", "sector_description")) %>%
    # Join with I & S distribution data
    left_join(corrections$is_distribution, by = c("state", "year")) %>% 
    mutate(coke_factor = if_else(ippu < sum_coking_coal, 0,
                                 ippu - sum_coking_coal),
           other_coal_coke_adj =
             coke_factor * (coking_coal_value / sum_coking_coal),
           # SNG correction for North Dakota only
           other_coal_sng_adj  = if_else(state == "ND", sng_correction, 0),
           # Multiply I & S factor by I & S state distribution percentages
           other_coal_is_adj = is_coal_factor * is_percent,
           adjusted_value_pre = value -
             (other_coal_coke_adj + 
                other_coal_sng_adj + 
                other_coal_is_adj)) %>%
    # Get sum of all states' adjusted values
    mutate(states_sum_value = sum(adjusted_value_pre), 
           .by = c(msn, year)) %>%
    mutate(adjusted_value =
             (adjusted_value_pre / states_sum_value) * consumption_value),
  
  # Natural Gas
  natural_gas = seds %>%
    # Separate list element required to find net natural gas
    filter(msn %in% c("NGICB", "SFINB")) %>%
    # Subtract supplemental gas from total natural gas
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Supplemental gas no longer needed (and value is now duplicative)
    filter(msn != "SFINB") %>%
    # Join with national data corrections
    left_join(corrections$national_corrections, by = "year") %>%
    # Change source and MSN to reflect that it's net natural gas
    mutate(source_description = "natural gas",
           msn = "net natural gas") %>%
    # Join with consumption input data
    left_join(corrections$consumption_input,
              by = c("year", "source_description", "sector_description")) %>%
    # Join with I & S distribution data
    left_join(corrections$is_distribution, by = c("state", "year")) %>% 
    # Join with ammonia distribution data
    left_join(corrections$ammonia_distribution, by = c("state", "year")) %>% 
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    #  Ammonia factor * ammonia distribution = ammonia adjusted value 
    mutate(natural_gas_ammonia_adj = nat_gas_ammonia_factor * 
             ammonia_percent, 
           #  I & S factor * I & S distribution = I & S adjusted value 
           natural_gas_is_adj = is_gas_factor * is_percent,
           adjusted_value_pre = value -
             (natural_gas_ammonia_adj + natural_gas_is_adj)) %>%
    # Get sum of all states' adjusted (preliminary) values
    mutate(states_sum_value = sum(adjusted_value_pre), 
           .by = c(msn, year)) %>%
    # adj value / sum of all states' values * consumption = adjusted value
    mutate(adjusted_value =
             (adjusted_value_pre / states_sum_value) * 
             consumption_value),
  
  # Residual Fuel
  residual_fuel = seds %>%
    filter(msn == "RFICB") %>%
    # Join with national data corrections 
    left_join(corrections$national_corrections, by = "year") %>%
    # Join with consumption input data
    left_join(corrections$consumption_input,
              by = c("year", "source_description", "sector_description")) %>%
    # Join with petrochemicals carbon black distribution data
    left_join(corrections$petrochemicals_cb_distribution, by = c("state", "year")) %>% 
    # adjust for cb factor = cb_factor * petrochem cb distribution 
    # adjusted value = value - cb adjusted value (minimum = 0)
    mutate(residual_fuel_cb_adj = if_else(
      value - (cb_factor * petrochemical_cb_percent) < 0, 0, 
      value - (cb_factor * petrochemical_cb_percent))) %>% 
    # Get sum of all states' cb adjusted values
    mutate(states_sum_value = sum(residual_fuel_cb_adj), 
           .by = c(msn, year)) %>% 
    # now adjust by consumption value
    mutate(adjusted_value = 
             consumption_value * (residual_fuel_cb_adj / states_sum_value)),
  
  # Distillate Fuel
  distillate_fuel = seds %>%
    filter(msn == "DFICB") %>%
    # Join with national data corrections 
    left_join(corrections$national_corrections, by = "year") %>%
    # Join with consumption input data
    left_join(corrections$consumption_input,
              by = c("year", "source_description", "sector_description")) %>%
    # Join with I & S distribution data
    left_join(corrections$is_distribution, by = c("state", "year")) %>% 
    # distillate_fuel_is_adj = is_distillate_fuel_factor * 
    # mysterious hard-coded % used in the other_coal_is_adj, above
    # distillate_fuel_is_adj (if negative, then 0)
    mutate(distillate_fuel_is_adj = if_else(
      value - (is_distillate_fuel_factor * is_percent) < 0, 0, 
      value - (is_distillate_fuel_factor *  is_percent))) %>%
    # Get sum of all states' cb adjusted values
    mutate(states_sum_value = sum(distillate_fuel_is_adj), 
           .by = c(msn, year)) %>% 
    # now adjust by consumption value
    mutate(adjusted_value = 
             consumption_value *
             (distillate_fuel_is_adj / states_sum_value)),
  
  # Gasoline
  gasoline = seds %>% 
    # All gasoline - ethanol = net gasoline
    filter(msn %in% c("MGICB", "EMICB")) %>%
    # Subtract ethanol from total gasoline
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Supplemental gas no longer needed (and value is now duplicative)
    filter(msn != "EMICB") %>%
    # Get sum of all states' net gasoline
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # get adjustment factor for motor gasoline
    left_join(corrections$adjustments,
              by = c("source_description", "year", "sector_description")) %>%
    # Rename adjustment factor for clarity
    rename(motor_gas_factor = national_value) %>%
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net gasoline", 
           source_description = "motor gasoline",
           # adjusted net gasoline = motor gas factor * gasoline - sum
           adjusted_value = motor_gas_factor * (value / states_sum_value)),
  # motor_gasoline_factor = US Compare--Industrial--motor gasoline 
  
  # Petroleum Coke
  petroleum_coke = seds %>%
    filter(msn == "PCICB") %>%
    # Get sum of all states' petroleum_coke
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # get adjustment factor for petroleum coke
    left_join(corrections$adjustments,
              by = c("source_description", "year", "sector_description")) %>%
    # Rename adjustment factor for clarity 
    rename(petro_coke_factor = national_value) %>%
    # adjusted petro coke = petro coke factor * (petro coke / sum of states)
    mutate(adjusted_value = petro_coke_factor * (value / states_sum_value)),
  
  # LPG
  lpg = seds %>%
    filter(msn %in% c("HLICB", "PPICB")) %>%
    # Subtract pentanes plus from HGL
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Remove pentanes plus from this list element:
    filter(msn != "PPICB") %>%
    # Change source description to reflect new value
    mutate(source_description = "hgl") %>%
    # Join with adjustments to get adjustment factor
    left_join(corrections$adjustments,
              by = c("source_description", "year", "sector_description")) %>%
    # Rename adjustment factor for clarity
    rename(ind_lpg_factor = national_value) %>%
    # Get sum of all states' lpg
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Rename MSN & source and calculate adjusted value
    mutate(msn = "net lpg", 
           adjusted_value = ind_lpg_factor * (value / states_sum_value)),
  # ind_lpg_factor = US SEDS Total--LPG (state's HLICB - PPICB)
  
  # All other sources go in this list element
  other_industrial = seds %>% 
    filter(msn %in% c("ARICB", "KSICB", "LUICB", "ABICB", "COICB", 
                      "MBICB", "MSICB", "FNICB", "FOICB", "PPICB", 
                      "SGICB", "SNICB", "UOICB", "WXICB", "PQICB", 
                      "PYICB", "EQICB", "EYICB", "BQICB", "BYICB", 
                      "IQICB", "IYICB")) %>%
    mutate(adjusted_value = value)) %>%
  
  # Collapse list into a single data frame
  list_rbind()
# BTW: distillate_fuel_is_adj is required for neu_adjustments

# Transportation
seds_tra_adjusted <- lst(
  
  distillate_fuel = scraped_data$diesel_distribution %>% 
    # Join with adjustments data
    left_join(corrections$adjustments %>% 
                # Can only join by 'year', so a filter is required
                filter(source_description == "distillate fuel oil", 
                       sector_description == "transportation sector"), 
              by = c("year")) %>%
    mutate(adjusted_value = national_value * diesel_percent, 
           msn = "DFACB"), 
  
  gasoline = scraped_data$gasoline_distribution %>%
    # Join with adjustments data
    left_join(corrections$adjustments %>% 
                # Can only join by 'year', so a filter is required
                filter(source_description == "motor gasoline", 
                       sector_description == "transportation sector"), 
              by = c("year")) %>%
    mutate(adjusted_value = national_value * gasoline_percent, 
           msn = "net gasoline"), 
  
  lubricants = seds %>%
    filter(msn == "LUACB") %>%
    # Adjusted = original value 
    mutate(adjusted_value = value), 
  
  jet_fuel = seds %>%
    filter(msn == "JFACB") %>% 
    # Adjusted = original value
    mutate(adjusted_value = value), 
  
  residual_fuel = seds %>%
    filter(msn == "RFACB") %>% 
    # Adjusted = original value
    mutate(adjusted_value = value), 
  
  coal = seds %>%
    filter(msn == "CLACB") %>% 
    # Adjusted = original value 
    mutate(adjusted_value = value), 
  
  # LPGs (propane and/or HGL)
  lpg = seds %>%
    filter(case_when(as.integer(year) < 2010 ~ msn == "HLACB", 
                     as.integer(year) >= 2010 ~ msn == "PQACB")) %>% 
    # Adjusted = original value
    mutate(msn = "combined lpg", 
           adjusted_value = value), 
  
  aviation_gasoline = seds %>%
    filter(msn == "AVACB") %>%
    # Adjusted = original value
    mutate(adjusted_value = value), 
  
  natural_gas = seds %>%
    filter(msn == "NGACB") %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # rename for clarity
    rename(natural_gas_factor = national_value) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states' value / the above sum
    mutate(adjusted_value = natural_gas_factor * 
             (value / states_sum_value), 
           source_description = "natural gas")) %>%
  
  # Collapse list into a single data frame
  list_rbind()

# Electric Power
seds_ele_adjusted <- lst(
  
  coal = seds %>%
    filter(msn == "CLEIB") %>%
    # Make sector names match to complete the next join
    mutate(sector_description = word(sector_description, 
                                     start = 1, end = 3)) %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # Rename for clarity
    rename(coal_factor = national_value) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = coal_factor * 
             (value / states_sum_value)), 
  
  natural_gas = seds %>% 
    # find net natural gas
    filter(msn %in% c("NGEIB", "SFEIB")) %>%
    # Subtract supplemental gas from total natural gas
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Supplemental gas no longer needed (and value is now duplicative)
    filter(msn != "SFEIB") %>%
    # Make sector names match to complete the next join
    mutate(sector_description = word(sector_description, 
                                     start = 1, end = 3)) %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # Rename for clarity
    rename(natural_gas_factor = national_value) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = natural_gas_factor * 
             (value / states_sum_value)) %>%
    # Change MSN identifier. old MSN distinction no longer needed(?)
    # However, MSN can be reconstituted from other _code fields if needed.
    mutate(msn = "net natural gas", 
           source_description = "natural gas"),
  
  residual_fuel = seds %>%
    filter(msn == "RFEIB") %>%
    # Make sector names match to complete the next join
    mutate(sector_description = word(sector_description, 
                                     start = 1, end = 3)) %>%
    mutate(adjusted_value = value),
  
  petroleum_coke = seds %>%
    filter(msn == "PCEIB") %>%
    # Make sector names match to complete the next join
    mutate(sector_description = word(sector_description, 
                                     start = 1, end = 3)) %>%
    mutate(adjusted_value = value),
  
  distillate_fuel = seds %>%
    filter(msn == "DFEIB") %>%
    # Make sector names match to complete the next join
    mutate(sector_description = word(sector_description, 
                                     start = 1, end = 3)) %>%
    # Join with the adjustment factor data (from national inventory)
    left_join(corrections$adjustments, 
              by = c("source_description", "year", "sector_description")) %>%
    # Rename for clarity
    rename(distillate_fuel_factor = national_value) %>%
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(adjusted_value = distillate_fuel_factor * 
             (value / states_sum_value))) %>%
  
  # Collapse list into a single data frame
  list_rbind()

# IBF
seds_ibf_adjusted <- lst(
  
  distillate_fuel = corrections$foks_diesel_distribution %>%
    left_join(corrections$ibf_corrections %>% filter(
      source_description == "distillate fuel oil"), 
      by = "year") %>% 
    # Calculate adjusted value (factor * percent)
    mutate(ibf_adjusted_value = ibf_value * foks_diesel_percent,
           # Add the MSN & sector  for transportation distillate fuel
           msn = "DFACB", 
           sector_description = "transportation sector"),
  
  residual_fuel = corrections$foks_residual_distribution %>%
    left_join(corrections$ibf_corrections %>% filter(
      source_description == "residual fuel oil"), 
      by = "year") %>% 
    # Calculate adjusted value (factor * percent)
    mutate(ibf_adjusted_value = ibf_value * foks_residual_percent, 
           # Add the MSN & sector for transportation residual fuel
           msn = "RFACB", 
           sector_description = "transportation sector"),
  
  jet_fuel = seds %>%
    filter(msn == "JFACB") %>%
    left_join(corrections$ibf_corrections,
              by = c("year", "source_description")) %>% 
    # Get sum of all states' value 
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Multiply adjustment factor by states's value / the above sum
    mutate(ibf_adjusted_value = ibf_value * 
             (value / states_sum_value))) %>%
  
  # Collapse list into a single data frame
  list_rbind() %>%
  # Remove nonessential columns to simplify joins in state_breakouts.R
  select(state, year, sector_description, source_description, 
         msn, ibf_adjusted_value)

# NEU
seds_neu_adjusted <- lst(
  
  # Other coal
  # NEU Correction applies to Tennessee only (Eastman Gas Plant)
  other_coal = corrections$neu_corrections %>%
    filter(source_description == "other coal", 
           sector_description == "industrial sector") %>%
    mutate(neu_adjusted_value = neu_factor,
           # Add industrial other coal MSN
           msn = "CLOCB", 
           # Tennessee only
           state = "TN"),
  
  # Natural gas
  natural_gas = corrections$neu_corrections %>%
    # Natural gas has a very long source name in the NEU data
    filter(str_detect(source_description, "natural gas")) %>%
    left_join(corrections$petrochemicals_distribution, by = "year") %>%
    # NEU value = NEU factor * distribution (will be zero for most states)
    mutate(neu_adjusted_value = neu_factor * petrochemical_percent, 
           # Standardize MSN & source to match seds_ind_adjusted
           msn = "net natural gas", 
           source_description = "natural gas"), 
  
  # Distillate fuel
  distillate_fuel = seds_ind_adjusted %>% 
    filter(msn == "DFICB") %>%
    # Join with NEU corrections data
    left_join(corrections$neu_corrections,
              by = c("year", "source_description", "sector_description")) %>%
    mutate(neu_adjusted_value = neu_factor * 
             (distillate_fuel_is_adj / states_sum_value)) %>%
    # These columns no longer needed
    select(-distillate_fuel_is_adj, -adjusted_value),
  
  # LPG
  lpg = seds %>%
    filter(msn %in% c("HLICB", "PPICB")) %>%
    # Subtract pentanes plus from HGL
    mutate(value = abs(diff(value)), .by = c(state, year)) %>%
    # Group_size shows that each group has exactly two rows. Good!
    # Pentanes plus computed below, so remove from this element:
    filter(msn != "PPICB") %>%
    # Change source description to reflect new value
    mutate(source_description = "hgl") %>%
    # Join with neu corrections to get neu factor
    left_join(corrections$neu_corrections,
              by = c("year", "source_description", "sector_description")) %>%
    # Get sum of all states' lpg
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Rename MSN and calculate adjusted value
    mutate(msn = "net lpg", 
           neu_adjusted_value = neu_factor * (value / states_sum_value)),
  
  # Pentanes plus
  pentanes_plus = seds %>%
    filter(msn == "PPICB") %>%
    # Join with neu corrections to get neu factor
    left_join(corrections$neu_corrections,
              by = c("year", "source_description", "sector_description")) %>%
    # Get sum of all states' pentanes plus
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Calculate adjusted value
    mutate(neu_adjusted_value = neu_factor * (value / states_sum_value)),
  
  # Petroleum coke
  petroleum_coke = seds %>%
    filter(msn == "PCICB") %>%
    # Join with neu corrections to get neu factor
    left_join(corrections$neu_corrections,
              by = c("year", "source_description", "sector_description")) %>%
    # Get sum of all states' petroleum coke
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Calculate adjusted value
    mutate(neu_adjusted_value = neu_factor * (value / states_sum_value)),
  
  # Still gas
  still_gas = seds %>%
    filter(msn == "SGICB") %>%
    # Join with neu corrections to get neu factor
    left_join(corrections$neu_corrections,
              by = c("year", "source_description", "sector_description")) %>%
    # Get sum of all states' still gas
    mutate(states_sum_value = sum(value), .by = c(msn, year)) %>%
    # Calculate adjusted value
    mutate(neu_adjusted_value = neu_factor * (value / states_sum_value))) %>%
  
  # Collapse list into a single data frame
  list_rbind() %>%
  # Remove nonessential columns to simplify joins in state_breakouts.R
  select(sector_description:year, neu_adjusted_value, state, msn)

seds_adjusted <- lst(seds_res_adjusted, 
                     seds_com_adjusted, 
                     seds_ind_adjusted,
                     seds_tra_adjusted, 
                     seds_ele_adjusted, 
                     seds_ibf_adjusted, 
                     seds_neu_adjusted)

# Aggregate all data and make IBF & NEU adjustments
seds_all_adjusted <- list_rbind(seds_adjusted %>% 
                                  # Remove IBF and NEU data for now
                                  discard(names(.) %in% 
                                            c("seds_ibf_adjusted", 
                                              "seds_neu_adjusted"))) %>%
  select(state:adjusted_value, -eia_description) %>%
  # NOTE: maybe move this to carbon calculations, below
  # standardize source descriptions for join with carbon_factors
  mutate(source_description = case_when(
    str_detect(source_description, "naphtha less than") ~ "naphtha", 
    str_detect(source_description, "other oils") ~ "other oils", 
    str_detect(source_description, " and ") ~ 
      str_replace(source_description, " and ", " & "),
    str_detect(source_description, "aviation gasoline c") ~ "aviation gasoline",
    source_description == "aviation gasoline blending components" ~ 
      "avgas blend components",
    source_description == "motor gasoline blending components" ~ 
      "mogas blend components",
    str_detect(source_description, "hydrocarbon|propane") ~ "lpg",
    str_detect(source_description, "miscellaneous") ~ "misc. products",
    str_detect(source_description, "distillate ") ~ "distillate fuel oil",
    str_detect(source_description, "residual") ~ "residual fuel oil",
    .default = source_description)) %>%
  # Add sector to each coal source--required for carbon factors & NEU
  mutate(source_description = case_when(
    source_description == "coal" & sector_code == "CC" ~ 
      "commercial coal", 
    source_description == "coal" & sector_code == "EI" ~ 
      "electric power coal",
    source_description == "coal" & sector_code == "OC" ~ 
      "industrial other coal",
    source_description == "coal" & sector_code == "KC" ~ 
      "industrial coking coal",
    source_description == "coal" & sector_code == "AC" ~ 
      "transportation coal",
    source_description == "coal" & sector_code == "RC" ~ 
      "residential coal",
    .default = source_description)) %>%
  
  # Subtract NEU and IBF adjustments
  # Join with NEU adjusted data
  left_join(seds_adjusted$seds_neu_adjusted, 
            by = c("sector_description", "source_description", "year", 
                   "state", "msn")) %>%
  # Join with IBF adjusted data
  left_join(seds_adjusted$seds_ibf_adjusted, 
            by = c("sector_description", "source_description", "year", 
                   "state", "msn")) %>%
  # Get adjusted value - NEU and IBD values = final adjusted tBtu 
  mutate(neu_ibf_adjusted_value = if_else(
    # Subtract IBF only if IBF applies (i.e., isn't NA)
    !is.na(ibf_adjusted_value), 
    adjusted_value - ibf_adjusted_value, 
    adjusted_value), 
    # Some MSNs are 100% NEU. For these, NEU value = 100% of adjusted value
    neu_adjusted_value = if_else(msn %in% c("ARICB", "LUICB", "FNICB", "CLKCB",
                                            "FOICB", "SNICB", "WXICB", 
                                            "MSICB", "LUACB"), 
                                 adjusted_value, neu_adjusted_value),  
    neu_ibf_adjusted_value = if_else(
      # Subtract NEU only if NEU applies (i.e., isn't NA)
      !is.na(neu_adjusted_value), 
      neu_ibf_adjusted_value - neu_adjusted_value, 
      neu_ibf_adjusted_value)) %>% 
  
  # Finally, ZERO OUT all pentanes plus and unfinished oils
  mutate(neu_ibf_adjusted_value = case_when(
    source_description == "pentanes plus" ~ 0, 
    source_description == "unfinished oils" ~ 0, 
    .default = neu_ibf_adjusted_value))

# Apply labels to variables
seds_all_adjusted <- apply_variable_labels(seds_all_adjusted)

return(seds_all_adjusted)

}
