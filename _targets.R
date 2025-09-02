# Targets Pipeline Setup----------------------

# Load packages
library(pins)
library(targets)
library(visNetwork)
library(tarchetypes)

targets::tar_option_set(
  error = "null",
  garbage_collection = TRUE,
  memory = "transient",
  packages = c(
    "extrafont", "gt", "gtExtras", "httr", "janitor",
    "jsonlite", "knitr", "labelled", "openxlsx",
    "pdftools", "quarto", "reactable", "readxl", "roxygen2", "rvest",
    "showtext", "tictoc", "tidyverse",
    "tanagerharmonize"
  )
)


# Source custom functions for FFC data
source("functions_national.R")
source("functions_state.R")
source("functions_both.R")

# Define the pins board for data retrieval
board <- board_folder("data/pins", versioned = TRUE)

# Define the Pipeline-------------------------------------------

list(
  
  ## Data Retrieval------------------
  
  ### Both national and state------------------------------
  tar_target(
    carbon_factors_fixed,
    pin_read(board, "carbon_factors_fixed")
  ),
  
  tar_target(
    carbon_factors_variable,
    pin_read(board, "carbon_factors_variable")
  ),
  
  tar_target(
    neu_storage,
    pin_read(board, "neu_storage")
  ),
  
  tar_target(
    msn_eia,
    pin_read(board, "msn")
  ),
  
  tar_target(
    data_dictionary_values,
    read.csv(file = system.file("extdata",
                                       "data_dictionary_values.csv", 
                                       package = "tanagerharmonize"))
  ),
  
  tar_target(
    data_dictionary_variables,
    read.csv(file = system.file("extdata", 
                                       "data_dictionary_variables.csv", 
                                       package = "tanagerharmonize"))
  ),
  
  ### National--------------------------------------------
  tar_target(
    moves3_fuel,
    pin_read(board, "moves3_fuel")
  ),
  
  tar_target(
    moves3_vmt,
    pin_read(board, "moves3_vmt")
  ),
  
  # tar_target(
  #   nonroad_consumption,
  # ),
  
  tar_target(
    misc_corrections,
    pin_read(board, "misc_corrections")
  ),
  
  # LPG national data is temporary until we can retrieve
  tar_target(
    lpg_national,
    pin_read(board, "lpg_national")
  ),
  
  ### State-----------------------------------------------
  tar_target(
    international_bunker_fuels,
    pin_read(board, "international_bunker_fuels")
  ),
  
  tar_target(
    non_energy_use,
    pin_read(board, "non_energy_use")
  ),
  
  tar_target(
    ippu_dist_ammonia,
    pin_read(board, "ippu_dist_ammonia")
  ),
  
  tar_target(
    ippu_dist_petrochemical,
    pin_read(board, "ippu_dist_petrochemical")
  ),
  
  tar_target(
    ippu_dist_carbon_black,
    pin_read(board, "ippu_dist_carbon_black")
  ),
  
  tar_target(
    ippu_dist_iron_and_steel,
    pin_read(board, "ippu_dist_iron_and_steel")
  ),
  
  tar_target(
    foks_diesel,
    pin_read(board, "foks_diesel")
  ),
  
  tar_target(
    foks_residual,
    pin_read(board, "foks_residual")
  ),
  
  ## Data Transformation-------------------------------------------
  
  ### Both national and state---------------------------------
  tar_target(
    msn_lookup,
    lookup_msn()
  ),
  
  tar_target(
    fhwa_data,
    scrape_fhwa_data(data_dictionary_values), 
    # Scrape FWHA data only if it's a month old
    cue = tar_cue_age(name = fhwa_data, 
                      age = as.difftime(30, units = "days"))
  ),
  
  tar_target(
    carbon_coefficients,
    get_carbon_factors(
      carbon_factors_variable,
      carbon_factors_fixed,
      neu_storage)
  ), 
  
  ### National-----------------------------------------------
  tar_target(
    us_consumption,
    get_national_results(msn_lookup, msn_eia, lpg_national) |>
      standardize_ffc(msn_eia),
    # Pull data only if it's a month old
    cue = tar_cue_age(name = us_consumption, 
                      age = as.difftime(30, units = "days"))
  ),
  
  tar_target(
    eia_heat_content,
    get_heat_content(),
    # Pull data only if it's a month old
    cue = tar_cue_age(name = eia_heat_content,
                      age = as.difftime(30, units = "days"))
  ),
  
  tar_target(
    vessel_bunker_dist_fuel,
    get_vessel_bunker(),
    # Pull data only if it's a month old
    cue = tar_cue_age(name = vessel_bunker_dist_fuel, 
                      age = as.difftime(30, units = "days"))
  ),
  
  tar_target(
    ethanol_tra,
    get_ethanol_tra(),
    # Pull data only if it's a month old
    cue = tar_cue_age(name = ethanol_tra, 
                      age = as.difftime(30, units = "days"))
  ),

  # Mobile is in-work
  # tar_target(
  #   mobile_adjustments,
  #   get_mobile_adjustments_data(
  #     moves3_vmt,
  #     moves3_fuel,
  #     national_ffc_data,
  #     ethanol_tra,
  #     vessel_bunker_dist_fuel,
  #     eia_heat_content,
  #     us_consumption,
  #     fhwa_data
  #   )
  # ),

  # IBF is in-work
  tar_target(
    ibf_adjustments,
    get_ibf_adjustments_data(
      us_consumption,
      fhwa_data
    )
  ),

  tar_target(
    misc_adjustments,
    get_misc_adjustments_data(misc_corrections)
  ),

  tar_target(
    national_ffc_adjusted,
    national_ffc_adjust_data(
      us_consumption
      # general_data,
      # mobile_adjustments,
      # ibf_adjustments,
      # misc_adjustments
    )
  ),

  tar_target(
    carbon_emissions_national,
    national_ffc_calculate_emissions(
      national_ffc_adjusted,
      carbon_coefficients,
      general_data
    )
  ),

  ### State---------------------------------------------------
  tar_target(
    seds,
    state_ffc_get_seds_data(msn_lookup,
                            msn_eia, 
                            data_dictionary_values) |>
      standardize_ffc(msn_eia),
    # Pull data only if it's a month old
    cue = tar_cue_age(name = seds, 
                      age = as.difftime(30, units = "days"))
  ),

  tar_target(
    territories,
    get_territories_data() |>
      # Standardize ff_territories only 
      purrr::modify_at("ff_territories", ~standardize_ffc(., msn_eia)),
    # Pull data only if it's a month old
    cue = tar_cue_age(name = territories, 
                      age = as.difftime(30, units = "days"))
  ),

  tar_target(
    state_adjustments,
    state_ffc_get_adjustments_data(
      national_ffc_adjusted,
      international_bunker_fuels,
      misc_adjustments,
      non_energy_use,
      ippu_dist_ammonia,
      ippu_dist_petrochemical,
      ippu_dist_carbon_black,
      ippu_dist_iron_and_steel,
      foks_diesel,
      foks_residual
    )
  ),

  tar_target(
    carbon_emissions_territories,
    territories_ffc_adjust_data(
      carbon_coefficients,
      territories,
      general_data
    )
  ),

  tar_target(
    state_ffc_adjusted,
    state_ffc_adjust_data(
      seds,
      state_adjustments,
      fhwa_data
    )
  ),

  tar_target(
    carbon_emissions_state_neu,
    state_neu_calculate_emissions(
      state_ffc_adjusted,
      carbon_coefficients,
      general_data,
      state_adjustments
    )),

  tar_target(
    carbon_emissions_state_ffc,
    state_ffc_calculate_emissions(
      state_ffc_adjusted,
      carbon_coefficients,
      general_data,
      state_adjustments
    )),

  #   ## Figures, Tables, and Quarto Reports--------------------------------
  #
  #   ### National-------------------------------------------------
  #   # tar_target(
  #   #   national_ffc_figures,
  #   #   national_ffc_ggplot_figures(
  #   #     national_ffc_adjusted,
  #   #     carbon_emissions_national
  #   #   )
  #   # ),
  #   #
  #   # tar_target(saved_national_ffc_figures,
  #   #   {
  #   #     saveRDS(national_ffc_figures,
  #   #       file = "saved_national_ffc_figures.rds"
  #   #     )
  #   #     "saved_national_ffc_figures.rds"
  #   #   },
  #   #   format = "file"
  #   # ),
  #   #
  #   # tar_target(
  #   #   national_ffc_tables,
  #   #   national_ffc_gt_tables(national_ffc_adjusted,
  #   #                          carbon_emissions_national)
  #   # ),
  #   #
  #   # tar_target(saved_national_ffc_tables,
  #   #   {
  #   #     saveRDS(national_ffc_tables,
  #   #       file = "saved_national_ffc_tables.rds"
  #   #     )
  #   #     "saved_national_ffc_tables.rds"
  #   #   },
  #   # ),
  #   #
  #   # ### State--------------------------------------------
  #   # tar_target(
  #   #   state_ffc_figures,
  #   #   state_ffc_ggplot_figures(
  #   #     state_ffc_adjusted$seds_all_adjusted,
  #   #     state_ffc_adjusted$seds_ind_adjusted,
  #   #     state_adjustments,
  #   #     carbon_emissions_state_ffc
  #   #   )
  #   # ),
  #   #
  #   # tar_target(saved_state_ffc_figures,
  #   #   {
  #   #     saveRDS(state_ffc_figures, file = "saved_state_ffc_figures.rds")
  #   #     "saved_state_ffc_figures.rds"
  #   #   },
  #   #   format = "file"
  #   # ),
  #   #
  #   # tar_target(
  #   #   state_ffc_tables,
  #   #   state_ffc_gt_tables(state_ffc_adjusted$seds_all_adjusted,
  #   #                       carbon_emissions_state_ffc)
  #   # ),
  #   #
  #   # tar_target(saved_state_ffc_tables,
  #   #   {
  #   #     saveRDS(state_ffc_tables, file = "saved_state_ffc_tables.rds")
  #   #     "saved_state_ffc_tables.rds"
  #   #   },
  #   #   format = "file"
  #   # ),
  #
  #   ## InvDB Output-------------------------------

  tar_target(
    invdb,
    write_to_invdb(
      # carbon_emissions_national,
      carbon_emissions_territories,
      carbon_emissions_state_ffc,
      carbon_emissions_state_neu)
  )

  #   ## National--------------------------------------------
  #   tar_quarto(
  #     national_ffc_final_report,
  #     path = "national_ffc_final_report.qmd",
  #     extra_files = c("saved_national_ffc_tables.rds",
  #                     "saved_national_ffc_figures.rds")
  #   ),
  #
  # ## State----------------------------------------------
  #   tar_quarto(
  #     state_ffc_final_report,
  #     path = "state_ffc_final_report.qmd",
  #     extra_files = c("saved_state_ffc_tables.rds",
  #                     "saved_state_ffc_figures.rds")
  #   )
  # )

)
# Run the pipeline (only executes targets that require updating)
# targets::tar_make()


# Create visualization of targets network
# targets::tar_visnetwork()

# Load a cached target into global environment
# targets::tar_read()
# blahblah <- tar_read(seds_ind_adjusted) # use any target name

# Check for problems
# targets::tar_manifest()




