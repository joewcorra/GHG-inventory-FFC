# Calculate Motor Gasoline Adjustments
# Applies to Commercial, Industrial, Transportation


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Motor Gasoline Adjustments---------------------------------------------



# TRY TESTING THIS WITH THE DATA IN THE SPREADSHEETS
heat_content <- 5.253 
fhwa_road_only <- 109520794000 - (725907978 + 2280381389) # same for all 1990
# fhwa_road_only = fhwa gas - (nonroad_lawn + nonroad_rec)

# VMT by class = total all vehicles (fhwa data) * moves3ratio
# Moves3Ratio: fuel breakdown for vehicle class/vehicle total for gasoline
# MOVES3 ratios are vehicle specific; i.e., S1, S2, etc. 
vehicle_gallons <- c(405791, 74192609, 34334331, 5180766, 5180766) # from miles&gallons <- 'pivots'/'codes'/1000
# from codes: Joules/gal  gasoline <- 43488*2839, diesel <- =43717*3167
# Pivots: ?? miles * rate (both vary by year)
# Therefore, vehicle_gallons = pivots/codes/1000; 
# moves3_ratio = vehicle_gallons / sum(vehicle_gallons); 
# vehicle_gas_consumed = fhwa_road_only * moves3_ratio 
# total on-road consumption, MMBTU = heat_content * (sum(vehicle_gas_consumed) / 42)   
# on-road consumption, MMBTU, by vehicle class (no ethanol) =      --C38, mogas workbook--
  # ((heat_content * ((fhwa_road_only * moves3_ratio) / 42)) / 10^9) * ((total_on_road_consumption / 10^9 - ethanol_correction) / total_on_road_consumption / 10^9)     
ethanol_correction <- ethanol_adj / 10^3  # derived from biomass ethanol B14 (hidden!)

fuel_consumption_excl_ethanol <- fuel_consumption - ethanol_correction 
#------
# This represents distillate mogas|adjustment, C50:54
# Numbers do no match because VMT and transport workbooks do not match! prior to 1990
fuel_consumption_by_vehicle_class_excl_ethanol <- map(
  vehicle_gallons,  ~ 
    ((heat_content * (fhwa_adjusted * (.x/sum(vehicle_gallons)) / 42)) / 10^9) * 
    (fuel_consumption_excl_ethanol / fuel_consumption))

# This is C55, total fuel consumption by vehicle class excl ethanol
total_gas_by_v_excl_ethanol <- sum(fuel_consumption_by_vehicle_class_excl_ethanol %>% list_c)

# total_gas_by_v_excl_ethanol is exactly the same as fuel_consumption_excl_ethanol
# Why calculate both?

# EIA data (MGCCB, MGICB, MGACB  - EIA ethanol) + biomass ethanol (90-92 only)
mogas_eia_ind_adjusted <- (mogas_eia_ind - ethanol_eia_ind + ethanol_adj) / 1000
mogas_eia_com_adjusted <- (mogas_eia_com - ethanol_eia_com + ethanol_adj) / 1000
mogas_eia_tra_adjusted <- (mogas_eia_tra - ethanol_eia_tra + ethanol_adj) / 1000

