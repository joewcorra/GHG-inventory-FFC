
# National Consumption Data-------------------------

  # Function to read data from EIA API, 1 year at a time
  get_national_results <- function(msn_lookup, msn_eia, lpg_national) {
    
    # API ke, generate your own
    key <- " "
    
    # Change to match most recent available year (current year minus two)
    latest_year <- lubridate::year(Sys.Date()) - 2
    
    # For now, to avoid exceeding the 5000-row data limit, we will pull
    # only one year and state per query. This requires n=51*years API queries.
    get_api_data <- function(year) {
      
      results <- paste0(
      "https://api.eia.gov/v2/total-energy/data/?frequency",
      "=annual&data[0]=value&start=", year,
      "&end=", year, "&sort[0][column]=period&sort[0][direction]",
      "=desc&offset=0&length=5000&api_key=", key) %>% # our API key is required
      httr::GET() %>% # retrieve page from url
      httr::content("raw") %>% # extract content as a raw vector
      rawToChar() %>% # convert to character data
      jsonlite::fromJSON() # convert from JSON to R object
    }
    
  ## Read National Data-----------------------------------------------
  
  tictoc::tic()
  api_results <- expand_grid(
    year = 1990:latest_year) %>%
    purrr::pmap(function(year) get_api_data(year))
  tictoc::toc()
  
  ## Collate National Data-------------------------------------------
  
  # Extract national consumption data from API results
  us_consumption_all <- api_results %>%
    purrr::map(\(.x) purrr::pluck(.x, "response", "data")) %>%
    purrr::list_rbind() %>%
    janitor::clean_names() %>%
    select(year = period, msn, value, unit) %>%
    mutate(unit = str_to_lower(unit),
           year = as.character(year),
           value = readr::parse_number(value),
           msn = str_sub(msn, 1, 5))
  
  # Collate national data (not including industrial coal)
  us_consumption <- us_consumption_all %>%
    filter(unit == "trillion btu",
           msn %in% msn_lookup,
           str_sub(msn, 3, 4) %in% c("AC", "RC", "IC", "CC", "EI")) %>%
    left_join(msn_eia %>%
                select(-unit), by = "msn") %>%
    # Remove any duplicates caused by appending new annual data
    distinct()
  
  # Collate industrial coal data, calculate and append
  us_ind_coal <- us_consumption_all %>%
    filter(str_sub(msn, 3, 4) %in% c("KC", "OC")) %>%
    mutate(source_description = if_else(str_sub(msn, 3, 4) == "KC",
                                        "coking coal", "other coal")) %>%
    group_by(year, source_description) %>%
    summarize(value = prod(value)) %>%
    ungroup() %>%
    mutate(sector_description = "industrial sector")
  
  # Append ind coal data to us_consumption
  us_consumption <- us_consumption %>%
    dplyr::rows_append(us_ind_coal) %>%
    # Remove total ind coal (replaced by coking and other coal)
    filter(msn != "CLICB")
  
  ## HGL Component Data-----------------------------------------
  
  # Also includes pentanes plus
  eia_api_lpg <- paste0(
    "https://api.eia.gov/v2/petroleum/cons/psup/data/?frequency=annual&",
    "data[0]=value&",
    # "facets[series][]=MPPUPUS1&", # pentanes
    # "facets[series][]=MUOUPUS1&",  # unfinished oils
    "facets[series][]=MBIUPUS1&", # isobutane-isobutylene
    "facets[series][]=MBNUPUS1&", # butane-butylene
    "facets[series][]=METUPUS1&", # ethane-ethylene
    "facets[series][]=MPRUPUS1&", # propane-propylene
    "start=1990&end=", latest_year,
    "&sort[0][column]=period&sort[0][direction]=desc&",
    "offset=0&length=5000&api_key=", key
  ) %>%
    httr::GET() %>% # retrieve page from url
    httr::content("raw") %>% # extract content as a raw vector
    rawToChar() %>% # convert to character data
    jsonlite::fromJSON() # convert from JSON to R object
  
  lpg_components <- purrr::pluck(eia_api_lpg, "response", "data") %>%
    select(
      year = period,
      eia_description = 'series-description', value, unit = units
    ) %>%
    # Make value numeric
    mutate(value = as.numeric(value))
  
  # Doing this requires unpublished EIA propane data, as well as
  # heat content by lpg and disaggregating combined lpgs
  # For now we will read processed data from csv
  lpg_components <- lpg_national %>%
    janitor::clean_names() %>%
    # pivot_longer(cols = !lpg, names_to = "year") %>%
    group_by(year, lpg) %>%
    summarize(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(
      # year = readr::parse_number(year) %>%
      #        forcats::as_factor(),
           msn = "combined lpg",
           unit = "trillion btu",
           eia_description = NA_character_,
           source_code = NA_character_,
           sector_code = "IC",
           source_description = "hydrocarbon gas liquids",
           sector_description = "industrial sector") %>%
    select(-lpg)
  
  # Append ind coal data to us_consumption
  us_consumption <- us_consumption %>%
    dplyr::rows_append(lpg_components) %>%
    # Remove ind HGL (replaced by combined lpg)
    filter(msn != "HLICB")
  
  return(us_consumption)
  
}

# Heat Content Data----------------------------------------------
  
get_heat_content <- function() {
  
  # API key generated 11/22/23
  key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"
  
  # Change to match most recent available year (current year minus two)
  latest_year <- lubridate::year(Sys.Date()) - 2
  
  eia_api_heat <- paste0(
    "https://api.eia.gov/v2/total-energy/data/?frequency=annual&data[0]",
    "=value&facets[msn][]=DMTCKUS&facets[msn][]=MGTCKUS&facets[msn][]=HLTCKUS&",
    "start=1990&end=", latest_year,
    "&sort[0][column]=msn&sort[0][direction]=asc&offset=0&length=5000&api_key=",
    key
  ) %>%
    httr::GET() %>% # retrieve page from url
    httr::content("raw") %>% # extract content as a raw vector
    rawToChar() %>% # convert to character data
    jsonlite::fromJSON() # convert from JSON to R object
  
  # Units in Millions of Btu / Barrel
  heat_content <- purrr::pluck(eia_api_heat, "response", "data") %>%
    select(
      year = period, msn,
      eia_description = seriesDescription, heat_content = value
    ) %>%
    # Make heat content value numeric
    mutate(heat_content = as.numeric(heat_content))
  
  return(heat_content)
  
}

  # Vessel Bunkering Diesel Data----------------------------------
  get_vessel_bunker <- function() {
    
    # API key generated 11/22/23
    key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"
    
    # Change to match most recent available year (current year minus two)
    latest_year <- lubridate::year(Sys.Date()) - 2
    
  eia_api_vessel_bunker <- paste0(
    "https://api.eia.gov/v2/petroleum/cons/821usea/data/?frequency=annual",
    "&data[0]=value&facets[duoarea][]=NUS&facets[process][]=VAB&start=1989&end=",
    latest_year,
    "&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000",
    "&api_key=", key
  ) %>%
    httr::GET() %>% # retrieve page from url
    httr::content("raw") %>% # extract content as a raw vector
    rawToChar() %>% # convert to character data
    jsonlite::fromJSON() # convert from JSON to R object
  
  # Units in Millions of Gallons
  vessel_bunker_dist_fuel <- purrr::pluck(eia_api_vessel_bunker, "response", "data") %>%
    select(year = period, eia_description = "series-description", value) %>%
    # Make fuel consumption value numeric
    mutate(value = as.numeric(value))
  
  return(vessel_bunker_dist_fuel)
  
} 

  # Ethanol (Transportation) Data----------------------------------
 get_ethanol_tra <- function() {
   
   # API key generated 11/22/23
   key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"
   
  eia_api_ethanol <- paste0(
    "https://api.eia.gov/v2/total-energy/data/?frequency",
    "=annual&data[0]=value&start=1990&end=2022&sort[0][column]",
    "=period&sort[0][direction]",
    "https://api.eia.gov/v2/total-energy/data/?frequency",
    "=annual&data[0]=value&facets[msn][]=EMACBUS&start=1990&end=2023&sort[0]",
    "[column]=period&sort[0][direction]=desc&offset=0&length=5000&api_key=", key
  ) %>%
    httr::GET() %>% # retrieve page from url
    httr::content("raw") %>% # extract content as a raw vector
    rawToChar() %>% # convert to character data
    jsonlite::fromJSON() # convert from JSON to R object
  
  ethanol_tra <- purrr::pluck(eia_api_ethanol, "response", "data") %>%
    mutate(msn = str_sub(msn, 1, 5), value = as.numeric(value)) %>%
    select(-unit, eia_description = seriesDescription,
           year = period, ethanol = value)
  
  return(ethanol_tra)
}

# NATIONAL MOBILE DATA---------------------------------------------


get_mobile_adjustments_data <- function(moves3_vmt, 
                                        moves3_fuel,
                                        misc_tra_data,
                                        ethanol_tra,
                                        vessel_bunker_dist_fuel,
                                        eia_heat_content,
                                        us_consumption, 
                                        biodiesel,
                                        rail_diesel,
                                        nonroad_backcast,
                                        fhwa_data) {
  
  # Applies to Commercial, Industrial, Transportation
  us_consumption <- us_consumption %>% mutate(msn = str_to_upper(msn))
  # Motor Gasoline------------------------------------------------------------
 
   ## MOVES Data---------------------------------------------------------
  
  moves <- moves3_vmt %>%
    janitor::clean_names() %>%
    rename(vmt_percent = value) %>%
    left_join(
      moves3_fuel %>%
        rename(fuel_use_percent = value), 
      by = c("vehicle_type", "year")
    ) %>%
    mutate(
      fuel_type = case_when(
        vehicle_type %in% c(
          "mc", "ldgv", "ldgt",
          "hdgv", "hdgb"
        ) ~ "gasoline",
        vehicle_type %in% c(
          "lddv", "lddt",
          "hddt", "lddb", "hddb"
        ) ~ "diesel"
      )
    )
  
  ## EIA Mogas------------------------------------------------------------
  
  eia_consumption_mogas <- us_consumption %>%
    filter(msn %in% c("MGCCB", "MGACB", "MGICB")) %>%
    mutate(mogas_ethanol_adjusted = value / 0.001)
  
  eia_consumption_diesel <- us_consumption %>%
    filter(msn %in% c("DFACB", "DFCCB", "DFICB", "DFRCB", "DKEIB"))
  
  ## Total On-Road Mogas-----------------------------------------------------
  
  # Gasoline joules per gallon. Fixed value
  mogas_energy <- 43488 * 2839
  
  # Fuel density for kg -> gallons conversions. Fixed values
  diesel_density <- 0.85
  mogas_density <- 0.74
  
  nonroad <- misc_tra_data %>%
    filter(source == "mogas_nonroad_total")
  
  # Calculate total onroad gas use by year 
  onroad_gasoline <- fhwa_data$gasoline_use_national %>%
    left_join(nonroad_backcast %>% 
                mutate(
                  nonroad_mogas = backcast_nonroad_lg_mogas +
                    backcast_nonroad_rec_mogas), 
              by = "year") %>%
    # Subtract backcast nonroad use (1990-2014 only)
    mutate(adj_fhwa_mogas_gal = gasoline_use_gal * 1000 - nonroad_mogas) %>%
    right_join(moves %>% filter(fuel_type == "gasoline"), 
               by = "year") %>%
    # calculate gas use by vehicle class and convert to barrels
    mutate(onroad_mogas_use_gal = adj_fhwa_mogas_gal * fuel_use_percent) %>%
    # join with heat content data
    left_join(
      eia_heat_content %>%
        filter(msn == "MGTCKUS") %>%
        select(year, heat_content),
      by = "year") %>%
    # Calculate qbtu per barrel
    mutate(onroad_mogas_qbtu = (onroad_mogas_use_gal / 
                                  42 * heat_content) / 10^9) %>%
    # join w ethanol data
    left_join(ethanol_tra %>% 
                select(year, ethanol), by = "year") %>%
    # get subtotal before ethanol adjustment
    mutate(onroad_mogas_qbtu_all_vehicles = sum(onroad_mogas_qbtu), 
           .by = year) %>%
    # calculate  ethanol adjusted mogas total for all vehicles, method 1
    mutate(onroad_mogas_qbtu_all_vehicles_no_ethanol = 
             onroad_mogas_qbtu_all_vehicles - (ethanol / 1000)) %>%
    # calculate  ethanol adjusted mogas for each vehicle class
    mutate(onroad_mogas_qbtu_no_ethanol = 
             (onroad_mogas_qbtu_all_vehicles_no_ethanol /
             onroad_mogas_qbtu_all_vehicles)  * 
             onroad_mogas_qbtu) 

  total_transport_mogas <- onroad_gasoline %>%
    group_by(year) %>%
    # collapse into a single row for each year
    summarize(mogas_adjusted = max(onroad_mogas_qbtu_all_vehicles_no_ethanol)) %>%
  ungroup() %>%
    # join w/ EIA data totals for mogas
    left_join(eia_consumption_mogas %>%
                group_by(year) %>%
                summarize(mogas_eia_total = 
                            # sum all sectors and convert to qbtu
                            sum(mogas_ethanol_adjusted) / 10^6),
              by = "year") %>%
    # join with rec boats data to get bottom up method mogas totals
    left_join(misc_tra_data %>% 
                filter(source == "mogas_rec_boats") %>%
                # convert units to qbtu
                mutate(value = value / 1000) %>%
                select(mogas_rec_boats_bottom_up = value, 
                       year),
              by = "year") %>%
    # top down = EIA total - adjusted mogas
    mutate(mogas_rec_boats_top_down = mogas_eia_total - mogas_adjusted, 
           # Final number is the lower of bottom up and top down method
           mogas_rec_boats_adjusted = pmin(mogas_rec_boats_top_down, 
                                           mogas_rec_boats_bottom_up)) 
    
  national_mogas <- eia_consumption_mogas %>%
    # retain only necessary columns
    select(year:value, source_description:mogas_ethanol_adjusted) %>%
    left_join(total_transport_mogas, 
              by = "year") %>%
    # remove "transportation sector" rows
    filter(sector_description != "transportation sector") %>%
    # get total remaining mogas (EIA total - onroad and boats)
    mutate(remaining_total_mogas = mogas_eia_total - 
             (mogas_adjusted + mogas_rec_boats_adjusted)) %>%
    # Get industrial + commercial (non-transport) total by year
    mutate(remaining_mogas_nontrans = sum(mogas_ethanol_adjusted) / 10^6, 
           .by = year) %>%
    select(-value) %>%
    # make data wider for final calcs--1 row per year w/ all 3 sectors
    pivot_wider(names_from = sector_description, 
                values_from = mogas_ethanol_adjusted) %>%
    janitor::clean_names() %>%
    # Get industrial + commercial adjusted mogas totals
    mutate(mogas_ind = remaining_total_mogas * 
             industrial_sector / remaining_mogas_nontrans, 
           mogas_com = remaining_total_mogas * 
             commercial_sector / remaining_mogas_nontrans, 
           mogas_tra = (mogas_adjusted + mogas_rec_boats_adjusted) * 1000) %>%
    # group by year and retain only non-NA rows
    select(-commercial_sector, -industrial_sector) %>%
    group_by(year) %>%
    summarize(mogas_com = max(mogas_com, na.rm = TRUE), 
              mogas_ind = max(mogas_ind, na.rm = TRUE), 
              mogas_tra = max(mogas_tra, na.rm = TRUE)) %>%
    ungroup()
          
  
  
  # Distillate (Diesel) Fuel Adjustments--------------------------
  
  ## Vessel Fuel Data-----------------------------------------------

  # vEIA essel_bunker_dist_fuel
  # Data needed for vessel fuel and rail
  vessel_diesel <- vessel_bunker_dist_fuel %>%
    # Dist fuel only
    filter(str_detect(eia_description, "istillate")) %>%
    # Convert units
    mutate(value = value * 1000)
  
  ## Calculate Diesel Consumption------------------------------------
  
  # Start with MOVES model diesel fuel consumption
  adjusted_diesel <- moves %>%
    filter(fuel_type == "diesel") %>%
    # join with FHWA fuel consumption data
    left_join(fhwa_data$diesel_use_national, by = "year") %>%
    # calculate diesel comsumption by vehicle class
    mutate(diesel_use_by_class = fuel_use_percent * diesel_use_gal * 1000) %>%
    # Retain only necessaey columns for the following row bind
    select(year, class = vehicle_type, diesel_use_by_class) %>%
    # Joni with rail diesel consumption
    bind_rows(rail_diesel %>%
                # Sum all rail classes by year
                group_by(year) %>%
                summarize(diesel_use_by_class = sum(diesel_consumption)) %>%
                ungroup() %>%
                # create vehicle class for all rail types
                mutate(class = "locomotives")) %>%
    # Join with marine diesel consumption 
    bind_rows(vessel_diesel %>%
                select(year, diesel_use_by_class = value) %>%
                # create vehicle class for vessels
                mutate(class = "ships and boats")) %>%
    # Calculate subtotal and add to every row
    mutate(subtotal_diesel_use = sum(diesel_use_by_class), .by = year) %>%
  # Joni with biodiesel data
  left_join(biodiesel, by = "year") %>%
    # Adjust subtotal for biodiesel use 
    mutate(subtotal_diesel_use_adjusted = subtotal_diesel_use - 
             biodiesel, 
           # adjust each fuel class for biodiesel use and convert to barrels
           diesel_use_by_class_adjusted = ((diesel_use_by_class / 
                                             subtotal_diesel_use) * 
             subtotal_diesel_use_adjusted) / 42) %>%
    # join with heat content data
    left_join(
      eia_heat_content %>%
        filter(msn == "DMTCKUS") %>%
        select(year, heat_content),
      by = "year")%>%
    # calculate heat content and convert to qbtu
    mutate(diesel_consumption_tbtu = (diesel_use_by_class_adjusted * 
                                        heat_content) / 10^6)
   
  national_diesel <- adjusted_diesel %>%
  # Summarize by year
    group_by(year) %>%
    summarize(diesel_tra = sum(diesel_consumption_tbtu)) %>%
    ungroup()
    
  
  ## EIA Diesel Fuel---------------------------------------------------------
  
  # # For each: com, ind, res, and tra
  # 
  # us_consumption_dist_fuel <- us_consumption %>%
  #   filter(msn %in% c("DFRCB", "DFICB", "DFCCB", "DFACB")) %>%
  #   mutate(meta_sector = case_when(
  #     sector_description == "commercial sector" ~ "non-trans",
  #     sector_description == "industrial sector" ~ "non-trans",
  #     sector_description == "residential sector" ~ "non-trans",
  #     sector_description == "transportation sector" ~ "trans"
  #   ))
  
  
## Aggregate Data--------------------------
  
  mobile_adjustments <- lst(national_mogas, national_diesel)
  
  return(mobile_adjustments)
  
}


# NATIONAL DATA, MISC.----------------------------------------

get_ippu_adjustments_data <- function(ippu_corrections) {
  
  # National adjustments data-------------------------------------
  ippu_adjustments <- ippu_corrections %>%
    janitor::clean_names() %>%
    pivot_wider(names_from = misc_adjustment, values_from = value)
  
  # Aggregate---------------------------------------------
  
  return(ippu_adjustments)
}


# NATIONAL DATA ADJUSTMENTS----------------------------


national_ffc_adjust_data <- function(
    us_consumption,
    mobile_adjustments,
    international_bunker_fuels,
    ippu_adjustments
) {
  # Collate National consumption data-------------------------------------
  
  us_consumption <- us_consumption %>% 
    mutate(msn = str_to_upper(msn))
  
  international_bunker_fuels <- international_bunker_fuels %>% 
    rename(ibf_value = value)
  
  # ## Residential, Commercial, & Electric Power----------------------------
  #
  # # No adjustments EXCEPT dist fuel and mogas (see those scripts).
  #
  us_res_com_ele <- lst(
    res = us_consumption %>%
      filter(msn %in% c("CLRCB", "NNRCB", "DFRCB", "HLRCB", "KSRCB")),
    com = us_consumption %>%
      filter(msn %in% c(
        "CLCCB", "NNCCB", "DFCCB", "EMCCB", "HLCCB",
        "KSCCB", "MGCCB", "PCCCB", "RFCCB"
      )),
    ele = us_consumption %>% # NNEIB
      filter(msn %in% c("CLEIB", "NNEIB", "DKEIB", "PCEIB", "RFEIB")),
  ) %>%
    # ISSUES 3/28/24
    # Electric power needs: distillate fuel
    # Need to adjust for distillate fuel oil (com & res, but not electric?)
    # need to adjust motor gas (com)
    # commercial has ethanol in the dataset but not in the spreadsheet
    
    # Collapse list into a single data frame
    purrr::list_rbind() %>%
    # create 'adjusted_value' column to match ind and tra data
    mutate(adjusted_value = value)
  #
  ## Industrial------------------------------------------------
  
  # coking coal
  
  us_ind <- lst(
    
    # Asphalt & Road Oil (NEU adjustment: 100%)
    asphalt = us_consumption %>%
      filter(msn == "ARICB") %>%
      # NEU adjustment is 100% of total
      mutate(adjusted_value = value - value),
    
    # Coking Coal (IPPU adjustment)
    coking_coal = us_consumption %>%
      # What is the MSN for coking coal?
      filter(msn == "CLKCB") %>%
      # Subtract IPPU adjustment
      left_join(ippu_adjustments, by = "year") %>%
      mutate(
        adjusted_value = value - coking_coal_adj,
        # Adjusted value no lower than zero
        adjusted_value = if_else(adjusted_value < 0, 0, adjusted_value)
      ),
    
    # Other Coal (NEU adjustment: Eastman Gas coal gasification;
    # synthetic natural gas adjustment, coking coal adjustment,
    # i & s adjustment
    other_coal = us_consumption %>%
      filter(msn == "CLOCB") %>%
      # Subtract adjustments
      left_join(ippu_adjustments, by = "year") %>%
      # Subtract adjustments
      mutate(adjusted_value = value -
               eastman_adj -
               dakota_adj -
               coking_coal_adj -
               is_coal_adj
      ),
    
    # Natural Gas (NEU adjustment: special; blast furnace adjustment,
    # coke oven adjustment, biogas adjustment,
    # ammonia adjustment, and i & s adjustment)
    # Supplemental gas already excluded
    natural_gas = us_consumption %>%
      filter(msn == "NNICB") %>%
      left_join(ippu_adjustments, by = "year") %>%
      # Subtract adjustments
      mutate(adjusted_value = value -
               blast_furnace_adj -
               coke_oven_adj -
               biogas_adj -
               ammonia_adj -
               is_natgas_adj
      ),
    
    # Residual Fuel (carbon black adjustment)
    residual_fuel = us_consumption %>%
      filter(msn == "RFICB") %>%
      # Subtract carbon black adjustment
      left_join(ippu_adjustments, by = "year") %>%
      mutate(
        adjusted_value = value - cb_residual_adj,
        # Adjusted value no lower than zero
        adjusted_value = if_else(adjusted_value < 0, 0, adjusted_value)
      ),
    
    # Distillate Fuel (i&s adjustment, mogas/df adjustment)
    distillate_fuel = us_consumption %>%
      filter(msn == "DFICB") %>%
      # Subtract iron & steel adjustment
      left_join(ippu_adjustments, by = "year") %>%
      mutate(adjusted_value = value - is_diesel_adj),
    
    # Motor gasoline (mogas/df adjustment)
    motor_gasoline = us_consumption %>%
      filter(msn %in% c("MGICB", "EMICB")) %>%
      mutate(adjusted_value = value),
    
    # Kerosene (no adjustment)
    kerosene = us_consumption %>%
      filter(msn == "KSICB") %>%
      mutate(adjusted_value = value),
    
    # Petroleum Coke (NEU adjustment: special)
    petroleum_coke = us_consumption %>%
      filter(msn == "PCICB") %>%
      mutate(adjusted_value = value),
    
    # LPG (AKA Propane) (no adjustment)
    lpg = us_consumption %>%
      filter(msn == "combined lpg") %>%
      mutate(adjusted_value = value),
    
    # PQICB     PYICB (NEU adjustment: special)
    # Propane and Propylene: Included w/ HLICB ?
    
    # Lubricants (NEU adjustment: 100%)
    lubricants = us_consumption %>%
      filter(msn == "LUICB") %>%
      mutate(adjusted_value = value),
    
    # Misc Products (NEU adjustment: 100%)
    misc_products = us_consumption %>%
      filter(msn == "MSICB") %>%
      # NEU adjustment is 100% of total
      mutate(adjusted_value = value - value),
    
    # Naphtha (<401 deg. F) (NEU adjustment: 100%)
    naphtha = us_consumption %>%
      filter(msn == "FNICB") %>%
      # NEU adjustment is 100% of total
      mutate(adjusted_value = value - value),
    
    # Other Oil (>401 deg. F) (NEU adjustment: 100%)
    other_oil = us_consumption %>%
      filter(msn == "FOICB") %>%
      # NEU adjustment is 100% of total
      mutate(adjusted_value = value - value),
    
    # Pentanes Plus (NEU adjustment: special)
    pentanes_plus = us_consumption %>%
      filter(msn == "PPICB") %>%
      mutate(adjusted_value = value),
    
    # Still Gas (NEU adjustment: special)
    still_gas = us_consumption %>%
      filter(msn == "SGICB") %>%
      mutate(adjusted_value = value),
    
    # Special Naphtha (NEU adjustment: 100%)
    special_naphtha = us_consumption %>%
      filter(msn == "SNICB") %>%
      # NEU adjustment is 100% of total
      mutate(adjusted_value = value - value),
    
    # Waxes (NEU adjustment: 100%)
    waxes = us_consumption %>%
      filter(msn == "WXICB") %>%
      # NEU adjustment is 100% of total
      mutate(adjusted_value = value - value),
    
    # Unfinished Oils (no adjustment)
    unfinished_oils = us_consumption %>%
      filter(msn == "UOICB") %>%
      mutate(adjusted_value = value)
  ) %>%
    # Collapse list into a single data frame
    purrr::list_rbind() %>%
    # Convert coal to "industrial sector"
    mutate(sector_description = "industrial sector")
  
  ## Transportation--------------------------------------------------
  
  us_tra <- lst(
    
    # Coal
    coal = us_consumption %>%
      filter(msn == "CLACB"),
    
    # Natural Gas
    natural_gas = us_consumption %>%
      filter(msn == "NGACB"),
    
    # Lubricants (NEU adjustment)
    lubricants = us_consumption %>%
      filter(msn == "LUACB"),
    
    # Aviation Gasoline
    aviation_gasoline = us_consumption %>%
      filter(msn == "AVACB"),
    
    # Distillate Fuel (IBF adjustment, mogas/df adjustment)
    distillate_fuel = us_consumption %>%
      filter(msn == "DFACB") %>%
      left_join(international_bunker_fuels%>% 
                  filter(gas_mode_and_fuel_type == "marine distillate fuel oil"), 
                by = "year"),
    
    # Jet Fuel (IBF adjustment)
    jet_fuel = us_consumption %>%
      filter(msn == "JFACB") %>%
      left_join(international_bunker_fuels %>% 
                  filter(gas_mode_and_fuel_type == "marine residual fuel oil"), 
                by = "year"),
  
    # LPG (Propane) AKA HGL
    lpg = us_consumption %>%
      filter(msn == "HLACB"),
    
    # Motor Gasoline (mogas/df adjustment)
    aviation_gasoline = us_consumption %>%
      filter(msn == "MGACB"),
    
    # Residual Fuel (IBF adjustment)
    residual_fuel = us_consumption %>%
      filter(msn == "RFACB") %>%
      left_join(international_bunker_fuels %>% 
                  filter(gas_mode_and_fuel_type == "marine residual fuel oil"), 
                by = "year")
  ) %>%
    # Collapse list into a single data frame
    purrr::list_rbind() %>%
    # Calculate adjusted value across all elements
    mutate(ibf_value = replace_na(ibf_value, 0),
           adjusted_value = value - ibf_value)
  # mutate(adjusted_value = value - value), # NEU is 100% of lubricants
  
  # Aggregate------------------------------------------------------
  
  national_ffc_adjusted <- lst(us_res_com_ele, 
                               select(us_ind, year:adjusted_value), 
                               select(us_tra, 
                                      -gas_mode_and_fuel_type, 
                                      -ibf_value)) %>%
    dplyr::bind_rows()
  
}

# NATIONAL CO2 EMISSIONS------------------------------------


national_ffc_calculate_emissions <- function(national_ffc_adjusted,
                                             carbon_coefficients) {
  
  carbon_emissions_national <- national_ffc_adjusted %>%
    left_join(carbon_coefficients$carbon_factors,
              by = c("source_description", "year")
    ) %>%
    # MMT CO2  = btu * carbon factor/1000 * 44/12
    mutate(mmt_co2 = adjusted_value *
             (carbon_factor / 1000) * carbon_coefficients$carbon_ratio)
  
  return(carbon_emissions_national)
}

# NATIONAL FIGURES--------------------------------------


national_ffc_ggplot_figures <- function(national_ffc_adjusted,
                                        carbon_emissions_national) {
  
  national_ffc_figures <- NULL
  
  return(national_ffc_figures)
}

# NATIONAL TABLES----------------------------------------------

national_ffc_gt_tables <- function(national_ffc_adjusted,
                                   carbon_emissions_national) {
  national_ffc_tables <- lst()
  gt(national_ffc_adjusted,
     rowname_col = " ") %>%
    tab_header(title = md(
      "Table 6-3: CO<sub>2</sub>, CH<sub>4</sub>, and N<sub>2</sub>O Emissions from Energy (MMT CO<sub>2</sub> Eq.)")) %>%
    tab_stub_indent(
      rows = everything(),
      indent = 3) %>%
    # rows_add(Activity = "Reservoirs", .before = 1) %>%
    tab_stubhead(label = "Source") %>%
    opt_vertical_padding(2) %>%
    cols_align(align = "left") %>%
    fmt_number(decimals = 1) %>%
    sub_missing(missing_text = " ") %>%
    tab_options(
      heading.border.bottom.style = "solid",
      heading.border.bottom.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.width = 3,
      stub.text_transform = "capitalize",
      stub.border.style = "none",
      grand_summary_row.border.style = "solid",
      grand_summary_row.border.color = "black",
      grand_summary_row.border.width = 1) %>%
    grand_summary_rows(
      columns = where(is.numeric),
      fmt = ~ fmt_number(., decimals = 1),
      fns = list(Total ~ sum(., na.rm = TRUE))) %>%
    cols_add('TEST1' = '', .after = 'mpg') %>%
    cols_add('TEST2' = '', .after = 'cyl') %>%
    cols_label('TEST1' = md('  '),
               'TEST2' = md('  ')) %>%
    tab_style(style = cell_text(weight = "bold", color = "black"),
              locations = list(cells_grand_summary(), cells_stubhead(),
                               cells_title(), cells_stub_grand_summary(),
                               cells_row_groups(),
                               cells_stub(1),
                               cells_column_labels())) %>%
    tab_style (style = cell_fill(color = "gray", alpha = 0.5),
               locations = list(cells_body(c(2, 4)),
                                cells_grand_summary(c(2, 4)),
                                cells_column_labels(c(2,4)))) %>%
    tab_style(style = cell_text(align = "center"),
              location = list(cells_body(), cells_column_labels(),
                              cells_grand_summary())) %>%
    tab_footnote(footnote = "Note: Totals may not sum due to independent rounding.")
  
  return(national_ffc_tables)
}

