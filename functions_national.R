
# National Consumption Data-------------------------

  # Function to read data from EIA API, 1 year at a time
  get_national_results <- function(msn_lookup, msn_eia, lpg_national) {
    
    # API key generated 11/22/23
    key <- "IF71xvc7rkBDFvzekErsoZx99OC7cKNVvcKEUBDm"
    
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
    "&data[0]=value&facets[duoarea][]=NUS&facets[process][]=VAB&start=1990&end=",
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
                                        nonroad_backcast,
                                        fhwa_data) {
  # Applies to Commercial, Industrial, Transportation
  
  # Motor Gasoline------------------------------------------------------------
  ## MOVES Data---------------------------------------------------------
  # Simplified equation for total on road mogas consumption, mmbtu
  # g <- outer(r - (p +q), s,
  # function(ri, sj) { (h * ri * sj) / (42 * sum(s)) })
  # returns g as a matrix; take the sum(g)
  
  # Simplified equation for non road mogas consumption by class
  # v <- outer(s, e, function(a, b) {
  #   (h * (r - (p + q)) * (a / sum(s)) / (42 * 10^9)) * (1 - (b * 10^6 / sum(g)))
  # })
  # returns v as a matrix; take the sum(v)
  
  # Simplified equation for total non-road gas consumption
  # n <- i + c + t -
  
  
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
          "hddt", "lddb"
        ) ~ "diesel"
      )
    )
  
  ## EIA Mogas------------------------------------------------------------
  
  us_consumption_mogas <- us_consumption %>%
    filter(msn %in% c("MGCCB", "MGACB", "MGICB")) %>%
    mutate(mogas_ethanol_adjusted = value / 0.001)
  
  us_consumption_diesel <- us_consumption %>%
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
  
  
  # START HERE
  total_onroad_mogas <- onroad_gasoline %>%
    group_by(year) %>%
    summarize(mogas_adjusted = max(onroad_mogas_qbtu_all_vehicles_no_ethanol)) %>%
  ungroup()
  # START HERE
  
  
      left_join(misc_tra_data %>%
                filter(source == "mogas_rec_boats"),
              by = "year") %>%
    mutate(mogas_use = fuel_use_percent * (
      gasoline_use_gal * 1000 - value),
      tbtu = (mogas_use / 42 * heat_content) / 10^9
    ) %>%
    mutate(tbtu_sum = sum(tbtu), .by = year) %>%
    mutate(
      ethanol_adjustment_factor = 1 - (ethanol / 1000 / tbtu_sum),
      tbtu_adjusted = tbtu * ethanol_adjustment_factor
    )
  
  diesel <- moves %>%
    filter(fuel_type == "diesel") %>%
    left_join(fhwa_data$diesel_use_national, by = "year") %>%
    left_join(
      eia_heat_content %>%
        filter(msn == "DMTCKUS") %>%
        select(year, heat_content),
      by = "year"
    ) %>%
    left_join(misc_tra_data %>%
                filter(source %in% c("diesel_rec_boats", 
                                     "diesel_rail", 
                                     "diesel_marine_com", 
                                     "diesel_marine_mil")) %>%
                group_by(year) %>%
                summarize(diesel_nonroad_consumption_total = sum(value)) %>%
                ungroup(),
              by = "year") %>%
    mutate(diesel_use = fuel_use_percent * (
      diesel_use_gal * 1000 - diesel_nonroad_consumption_total),
      tbtu = (diesel_use / 42 * heat_content) / 10^9
    ) %>%
    mutate(tbtu_sum = sum(tbtu), .by = year)
  
  ## Total Nonroad Mogas--------------------------------------------------
  
  # Total non-road motor gasoline use
  rec_boat_mogas_top_down <- us_consumption_mogas %>%
    group_by(year) %>%
    # Get total EIA mogas by year (tra + com + ind)
    summarize(mogas_ethanol_adjusted = sum(mogas_ethanol_adjusted) * 100) %>%
    ungroup() %>%
    # Get MOVES3 on-road mogas totals
    left_join(mogas %>%
                group_by(year) %>%
                summarize(gasoline_use_gal = sum(gasoline_use_gal, na.rm= TRUE)) %>%
                ungroup(),
              by = "year") %>%
    # Annual non-road mogas = total mogas - on-road mogas
    mutate(nonroad_mogas = mogas_ethanol_adjusted -
             gasoline_use_gal) %>%
    # Join w/ heat content data for mogas
    left_join(eia_heat_content %>%
                # Motor gasoline only
                filter(str_detect(eia_description, "asoline")),
              by = "year") %>%
    mutate(rec_boat_mogas_top_down = heat_content *
             (nonroad_mogas / 42) / 10^9)
  
  
  ## Rec Boat Mogas-------------------------------------------------------
  
  # Recreational boat motor gasoline total is the smaller of 1) rec boat gas
  # calculated by the bottom-up method, or 2) total non-road motor gasoline
  
  # First compute rec boat mogas by the bottom-up method
  rec_boat_mogas_bottom_up <- eia_heat_content %>%
    # Motor gasoline only
    filter(str_detect(eia_description, "asoline")) %>%
    select(year, heat_content) %>%
    left_join(misc_tra_data %>% filter(source == "mogas_rec_boats")) %>%
    rename(rec_boat_mogas_bottom_up = value)
  
  # Rec boat motor gas is the lower of two values
  rec_boat_mogas <- rec_boat_mogas_top_down %>%
    # Join with the bottom-up data
    left_join(rec_boat_mogas_bottom_up, by = "year") %>%
    # Select whichever value is lower: non-road or bottom up
    mutate(rec_boat_mogas = pmin(rec_boat_mogas_bottom_up,
                                rec_boat_mogas_top_down)) %>%
    select(year, rec_boat_mogas)
  
  ## Perform Motor Gasoline Adjustments-----------------------------------
  
  national_mogas <- us_consumption_mogas %>%
    # Join EIA consumption data with the MOVES3 annual results
    left_join(
      mogas %>%
        # Only a few columns are needed now
        select(
          year, tbtu_adjusted,
          tbtu
        ) %>%
        mutate(sector_description = "transportation sector"),
      by = c("year", "sector_description")
    ) %>%
    # Create 'meta sector' to differentiate transport from non-transport
    mutate(meta_sector = case_when(
      sector_description == "commercial sector" ~ "non-trans",
      sector_description == "industrial sector" ~ "non-trans",
      sector_description == "transportation sector" ~ "trans"
    )) %>%
    # Non-trans mogas total = total non-trans mogas - on-road - rec boats
    mutate(remaining_mogas = case_when(
      meta_sector == "non-trans" ~ sum(tbtu_adjusted) -
        (rec_boat_mogas),
      .default = tbtu_adjusted
    ), .by = year) %>%
    mutate(
      mogas_adjusted = case_when(
        # Com or ind = remaining mogas value * EIA mogas / sum of ind + com mogas
        meta_sector == "non-trans" ~
          remaining_mogas * tbtu_adjusted / sum(tbtu_adjusted),
        # Transportation = on-road total + rec boat total
        meta_sector == "trans" ~
          tbtu_adjusted + rec_boat_mogas
      ),
      .by = c(year, meta_sector)
    )
  
  
  # Distillate (Diesel) Fuel Adjustments--------------------------
  ## Vessel Fuel Data-----------------------------------------------
  
  # Retrieved from EIA
  
  # vessel_bunker_dist_fuel
  # Data needed for vessel fuel and rail
  dist_fuel_vessel <- vessel_bunker_dist_fuel %>%
    # Dist fuel only
    filter(str_detect(eia_description, "istillate")) %>%
    # Convert units to million gallons
    mutate(value = value * 1000)
  
  ## Rail Fuel Data------------------------------------------------------
  
  
  
  ## Biodiesel-------------------------------------------------------------
  
  # Pull from EIA consumption data
  biodiesel <- us_consumption %>%
    # Biodiesel only
    filter(msn == "BDACB") %>%
    # Rename value as 'biodiesel' since it must be subtracted later
    select(year, biodiesel = value) %>%
    # Convert to millions of gallons. Convert NAs to zero
    mutate(biodiesel = if_else(is.na(biodiesel), 0, biodiesel * 42 * 1000))
  

  ## EIA Diesel Fuel---------------------------------------------------------
  
  # For each: com, ind, res, and tra
  
  us_consumption_dist_fuel <- us_consumption %>%
    filter(msn %in% c("DFRCB", "DFICB", "DFCCB", "DFACB")) %>%
    mutate(meta_sector = case_when(
      sector_description == "commercial sector" ~ "non-trans",
      sector_description == "industrial sector" ~ "non-trans",
      sector_description == "residential sector" ~ "non-trans",
      sector_description == "transportation sector" ~ "trans"
    ))
  
  ## Perform Diesel Adjustments-----------------------------------
  
  # Total dist fuel = cars, rails, and vessels minus biodiesel
  dist_fuel_excl_biodiesel <- dist_fuel_vessel %>%
    select(year, value) %>%
    dplyr::bind_rows(biodiesel) %>%
    group_by(year) %>%
    summarize(total_dist_fuel = sum(
      value,
      na.rm = TRUE
    ) - sum(
      biodiesel,
      na.rm = TRUE
    )) %>%
    ungroup() %>%
    # Join with heat content data for distillate fuel
    left_join(eia_heat_content %>%
                # Distillate fuel only
                filter(str_detect(eia_description, "istillate")), by = "year") %>%
    # Convert to barrels and multiply by heat content to get mmbtu
    mutate(
      total_dist_fuel = (total_dist_fuel / 42) * heat_content,
      bottom_up_trans = total_dist_fuel / 1000
    )
  
  national_diesel <- us_consumption_dist_fuel %>%
    left_join(dist_fuel_excl_biodiesel %>%
                select(year, bottom_up_trans), by = "year") %>%
    mutate(
      bottom_up_nontrans = sum(value, na.rm = TRUE) - bottom_up_trans,
      .by = c(year, meta_sector)
    ) %>%
    mutate(
      bottom_up_total = sum(value, na.rm = TRUE) - bottom_up_trans,
      .by = c(year)
    ) %>%
    mutate(
      adjusted_value = ((value * 10^6) / (sum(
        value,
        na.rm = TRUE
      ) * bottom_up_total)) / 10^3,
      .by = c(year)
    )
  
  # Aggregate------------------------------------------------
  
  mobile_adjustments <- lst(national_mogas, national_diesel)
  
  return(mobile_adjustments)
}

# NATIONAL IBF DATA---------------------------------------------
#' Prepare misc. national adjustments (NEU/IPPU and special factors)
#'
#' @description Converts a corrections sheet to a long/wide tidy frame of
#' year specific adjustment factors (e.g., Eastman, Dakota SNG, IPPU coal/gas).
#'
#' @importFrom dplyr mutate select filter across case_when if_else left_join distinct group_by ungroup summarize rename arrange
#' @importFrom stringr str_squish str_remove str_remove_all str_to_lower str_detect
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble lst
#'
#' @details
#' **Retrieval:**
#'
#' **Transform:**
#' - Cleans names; coerces numeric; pivots long to `year`/`value`;
#' parses year; pivots wide to individual factor columns.
#'
#' **Collate/Output:**
#' - Tibble where each column is an adjustment factor (e.g., `eastman_adj`,
#' `dakota_adj`, `coking_coal_adj`, `is_natgas_adj`, `cb_residual_adj`, etc.).
#'
#' @param ippu_corrections List with element `corrections` (data frame).
#'
#' @return Tibble of per year adjustment factors (one row per year).
#'
#' @seealso [national_ffc_adjust_data()]
#'
#' @examples
#' \dontrun{
#' misc_adj <- get_ippu_adjustments_data(ippu_corrections)
#' }

get_ibf_adjustments_data <- function(national_ffc_data,
                                     fhwa_data) {
  # Fuel Densities----------------------------------------------
  
  # These are fixed (I think)
  
  fuels <- tibble(
    fuel = c(
      "jet fuel", "JP8", "JP5", "JP4", "JAA", "JA1", "JAB",
      "distillate fuel", "commerce marine",
      "military marine", "residual fuel",
      "aviation gasoline", "intermediate fuel oil (IFO)"
    ),
    fuel_density = c(
      3.002, 3.04, 3.08, 2.90, 3.08, 3.04, NA, NA,
      3.1916, 3.18, 3.575, 2.72, 3.81
    )
  )
  
  # TEMPORARY!!!!!!!!!!!!!!------------------------------------
  tra_temp <- readxl::read_excel("data/misc_tra_data_temporary.xlsx", 
                                 sheet = 1) %>%
    filter(str_detect(source,"marine|jet")) %>%
    pivot_longer(cols = !source, names_to = "year", values_to = "ibf_value")
  
  ibf_jet_fuel_adj <- tra_temp %>%
    filter(str_detect(source, "jet")) %>%
    group_by(year) %>%
    summarize(ibf_value = sum(ibf_value, na.rm = TRUE)) %>%
    ungroup()
  
  ibf_marine_residual_fuel_adj <- tra_temp %>%
    filter(source == "resid_marine_com") %>%
    select(-source)
  
  ibf_marine_dist_fuel_adj <- tra_temp %>%
    filter(source %in%
             c("diesel_marine_mil", "diesel_marine_com")) %>%
    group_by(year) %>%
    summarize(ibf_value = sum(ibf_value, na.rm = TRUE)) %>%
    ungroup()
  
  
  # IBF Adjustments---------------------------------------------
  
  
  # jet_fuel_consumption <- jet_fuel_civilian + jet_fuel_military
  
  # jet_fuel_heat_content <- 5.670
  
  # Aviation jet fuel (tbtu) = intl comm aviation (hard coded number in Transport workbook) +
  # military aircraft (hidden worksheet)
  
  # Marine residual fuel = ??? (hidden worksheet)
  
  # Marine distillate fuel = ??? (hidden worksheet)
  
  # Aggregate---------------------------------------------
  
  ibf_adjustments <- lst(
    ibf_jet_fuel_adj, ibf_marine_residual_fuel_adj,
    ibf_marine_dist_fuel_adj)
  
  return(ibf_adjustments)
}

# NATIONAL DATA, MISC.----------------------------------------
#' Prepare misc. national adjustments (NEU/IPPU and special factors)
#'
#' @description Converts a corrections sheet to a long/wide tidy frame of
#' year specific adjustment factors (e.g., Eastman, Dakota SNG, IPPU coal/gas).
#'
#'@importFrom dplyr mutate select filter across case_when if_else left_join distinct rename
#' @importFrom tidyr pivot_longer
#'
#' @details
#' **Retrieval:**
#' - Consumes `ippu_corrections$corrections` (wide with `xYYYY` columns).
#'
#' **Transform:**
#' - Cleans names; coerces numeric; pivots long to `year`/`value`;
#' parses year; pivots wide to individual factor columns.
#'
#' **Collate/Output:**
#' - Tibble where each column is an adjustment factor (e.g., `eastman_adj`,
#' `dakota_adj`, `coking_coal_adj`, `is_natgas_adj`, `cb_residual_adj`, etc.).
#'
#' @param ippu_corrections List with element `corrections` (data frame).
#'
#' @return Tibble of per year adjustment factors (one row per year).
#'
#' @seealso [national_ffc_adjust_data()]

#' }

get_ippu_adjustments_data <- function(ippu_corrections) {
  
  # National adjustments data-------------------------------------
  ippu_adjustments <- ippu_corrections %>%
    janitor::clean_names() %>%
    pivot_wider(names_from = misc_adjustment, values_from = value)
  
  # Aggregate---------------------------------------------
  
  return(ippu_adjustments)
}


# NATIONAL DATA ADJUSTMENTS----------------------------
#' Compute national adjusted fossil fuel consumption by sector/source
#'
#' @description Applies NEU, IPPU, IBF, and mobile re allocations to national
#' EIA consumption; returns sector/source TBtu adjusted for reporting.
#'
#' @importFrom dplyr mutate select filter across case_when if_else left_join distinct group_by ungroup summarize rename arrange
#' @importFrom stringr str_squish str_remove str_remove_all str_to_lower str_detect
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble lst
#'
#' @details
#' **Retrieval:**
#' - Consumes standardized `us_consumption` plus
#' `mobile_adjustments` (mogas/diesel), `ibf_adjustments`, and
#' per year `ippu_adjustments`.
#'
#' **Transform:**
#' - Residential/Commercial/Electric: passes through (with note that separate
#' mogas/DF adjustments may apply elsewhere), creates `adjusted_value = value`.
#' - Industrial: subtracts special factors (Eastman, Dakota SNG, IPPU coking coal,
#' iron & steel for coal/diesel, blast furnace/coke oven/biogas/ammonia for gas);
#' sets NEU 100% categories (asphalt, misc products, naphtha, other oil,
#' special naphtha, waxes) to zero adjusted energy; carries through others.
#' - Transportation: subtracts IBF for residual/distillate/jet; carries through
#' other fuels; keeps “combined lpg” where applicable.
#'
#' **Collate/Output:**
#' - Single tibble with `sector_description`, `source_description`, `year`,
#' original `value`, and `adjusted_value` (TBtu).
#'
#' @param national_ffc_data List from [national_ffc_read_eia_data()].
#' @param mobile_adjustments List from [get_mobile_adjustments_data()] (currently
#' not directly merged here; interface reserved).
#' @param ibf_adjustments List from [get_ibf_adjustments_data()].
#' @param ippu_adjustments Tibble from [get_ippu_adjustments_data()].
#'
#' @return Tibble of adjusted national consumption by sector/source/year.
#'
#' @seealso [national_ffc_read_eia_data()], [get_mobile_adjustments_data()],
#' [get_ibf_adjustments_data()], [get_ippu_adjustments_data()],
#' [national_ffc_calculate_emissions()]
#'
#' @examples
#' \dontrun{
#' adj <- national_ffc_adjust_data(national, mobiles, ibf, misc_adj)
#' dplyr::count(adj, sector_description, source_description)
#' }

national_ffc_adjust_data <- function(
    us_consumption,
    mobile_adjustments,
    ibf_adjustments,
    ippu_adjustments
) {
  # Collate National consumption data-------------------------------------
  
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
      left_join(ibf_adjustments$ibf_marine_dist_fuel_adj),
    
    # Jet Fuel (IBF adjustment)
    jet_fuel = us_consumption %>%
      filter(msn == "JFACB") %>%
      left_join(ibf_adjustments$ibf_jet_fuel_adj),
    
    # LPG (Propane) AKA HGL
    lpg = us_consumption %>%
      filter(msn == "HLACB"),
    
    # Motor Gasoline (mogas/df adjustment)
    aviation_gasoline = us_consumption %>%
      filter(msn == "MGACB"),
    
    # Residual Fuel (IBF adjustment)
    residual_fuel = us_consumption %>%
      filter(msn == "RFACB") %>%
      left_join(ibf_adjustments$ibf_marine_residual_fuel_adj)
  ) %>%
    # Collapse list into a single data frame
    purrr::list_rbind() %>%
    # Calculate adjusted value across all elements
    mutate(ibf_value = replace_na(ibf_value, 0),
           adjusted_value = value - ibf_value)
  # mutate(adjusted_value = value - value), # NEU is 100% of lubricants
  
  # Aggregate------------------------------------------------------
  
  national_ffc_adjusted <- lst(us_res_com_ele, us_ind, us_tra) %>%
    dplyr::bind_rows()
  
  # # TEMPORARY. FOR 2023 INV (5/8/2025) ONLY
  # national_ffc_adjusted <- read_csv("data/temporary_national_inv_data.csv") %>%
  #   pivot_longer(cols = !c(sector, source), names_to = "year") %>%
  #   rename(source_description = source,
  #          sector_description = sector,
  #          adjusted_value = value)
  
}

# NATIONAL CO2 EMISSIONS------------------------------------
' Calculate national CO2 emissions from adjusted energy use
#'
#' @description Joins adjusted TBtu with carbon factors and computes
#' CO2 emissions (MMT) using 44/12 molecular ratio.
#'
#' @importFrom dplyr mutate select filter across case_when if_else left_join distinct group_by ungroup summarize rename arrange
#' @importFrom stringr str_squish str_remove str_remove_all str_to_lower str_detect
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble lst
#'
#' @details
#' **Retrieval:**
#' - Consumes `national_ffc_adjusted` from [national_ffc_adjust_data()].
#' - Consumes `carbon_coefficients$carbon_factors` and `carbon_ratio`.
#'
#' **Transform:**
#' - Computes `mmt_co2 = adjusted_value * (carbon_factor / 1000) * carbon_ratio`.
#'
#' **Collate/Output:**
#' - Tibble of emissions by sector/source/year with `mmt_co2`.
#'
#' @param national_ffc_adjusted Tibble from [national_ffc_adjust_data()].
#' @param carbon_coefficients List with `carbon_factors` (per fuel/year) and
#' scalar `carbon_ratio` (44/12).
#'
#' @return Tibble with CO2 emissions (MMT) per sector/source/year.
#'
#' @seealso [national_ffc_adjust_data()]
#'
#' @examples
#' \dontrun{
#' co2_nat <- national_ffc_calculate_emissions(adj, carbon_coeffs)
#' }

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
#' Build national ggplot2 figures for FFC
#'
#' @description Placeholder that returns `NULL` until figures are implemented.
#'
#'#' @importFrom dplyr mutate select filter across case_when if_else left_join distinct group_by ungroup summarize rename arrange
#' @importFrom stringr str_squish str_remove str_remove_all str_to_lower str_detect
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble lst
#' @import ggplot2

#' @details
#' **Retrieval/Transform:** Not yet implemented.
#'
#' **Collate/Output:**
#' - Returns `NULL`. Intended to assemble a list of ggplot objects based on
#' adjusted consumption and emissions.
#'
#' @param national_ffc_adjusted Tibble from [national_ffc_adjust_data()].
#' @param carbon_emissions_national Tibble from [national_ffc_calculate_emissions()].
#'
#' @return `NULL` (placeholder).
#'
#' @seealso [national_ffc_adjust_data()], [national_ffc_calculate_emissions()]
#'
#' @examples
#' NULL

national_ffc_ggplot_figures <- function(national_ffc_adjusted,
                                        carbon_emissions_national) {
  
  national_ffc_figures <- NULL
  
  return(national_ffc_figures)
}

# NATIONAL TABLES----------------------------------------------
#' Produce national GT tables for FFC
#'
#' @description Returns a list of formatted `gt` tables.
#'
#' @importFrom dplyr mutate select filter across case_when if_else left_join distinct group_by ungroup summarize rename arrange
#' @importFrom stringr str_squish str_remove str_remove_all str_to_lower str_detect
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble lst
#' @import gt
#'
#' @details
#' **Retrieval:**
#' - Consumes adjusted national energy and CO2 tables (not yet wired in demo).
#'
#' **Transform:**
#' - Demonstrates table header, styling, stub configuration, grand totals, and
#' footnotes that match report style.
#'
#' **Collate/Output:**
#' - Returns a named list `national_ffc_tables` of `gt` objects for report export.
#'
#' @param national_ffc_adjusted Tibble from [national_ffc_adjust_data()].
#' @param carbon_emissions_national Tibble from [national_ffc_calculate_emissions()].
#'
#' @return Named list of `gt` tables.
#'
#' @seealso [gt::gt()]
#'
#' @examples
#' \dontrun{
#' tabs <- national_ffc_gt_tables(adj, co2_nat)
#' }

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

