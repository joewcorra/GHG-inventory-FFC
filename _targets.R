# Targets Pipeline Setup----------------------

# Load packages
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
# assertr, shiny, waldo

# Function to read Excel files
read_xl_data <- function(path) {
  # Read names of all worksheets
  sheets <- excel_sheets(path)
  
  data <- sheets |>
    # Set names of worksheets as the list element names
    purrr::set_names() |>
    # Read each worksheet as a separate list element
    purrr::map(\(.x) readxl::read_excel(path, sheet = .x))
  
  return(data)
}

# Source custom functions for FFC data
source("functions_national.R")
source("functions_state.R")
source("functions_both.R")

# Define the Pipeline-------------------------------------------

list(
  
  ## Load Local/Network Files------------------------------------
  
  # Track data files (check for updates)
  
  ### Both national and state----------------------
  tar_file(
    carbon_factors_file,
    # Sheets: Factors
    "data/carbon_factors.xlsx"
  ),
  
  tar_file(
    neu_storage_file,
    # Sheets: Factors
    "data/neu_storage.csv"
  ),
  
  tar_file(
    msn_file,
    # Sheets: Factors
    "data/msn.csv"
  ),
  
  ### National------------------------------------
  tar_file(
    moves3_file,
    "data/moves3.xlsx"
  ),
  
  tar_file(
    nonroad_file,
    "data/nonroad_consumption.csv"
  ),
  
  tar_file(
    misc_corrections_file,
    "data/misc_corrections.xlsx"
  ),
  
  ### State-----------------------------------------
  tar_file(
    international_bunker_fuels_file,
    "data/international_bunker_fuels.xlsx"
  ),
  
  tar_file(
    non_energy_use_file,
    "data/non_energy_use.xlsx"
  ),
  
  tar_file(
    ippu_distributions_file,
    "data/ippu_distributions.xlsx"
  ),
  
  tar_file(
    foks_diesel_file,
    "data/FOKS_diesel_fuel_bunker_2020.xls"
  ),
  
  tar_file(
    foks_residual_file,
    "data/FOKS_resid_fuel_bunker_2020.xls"
  ),
  
  ## Load data files (if updates detected)-------------------------------
  
  ### Both national and state------------------------------
  tar_target(
    carbon_factors,
    read_xl_data(carbon_factors_file)
  ),
  
  tar_target(
    neu_storage,
    readr::read_csv(neu_storage_file)|>
      tanagerharmonize::pre_clean()
  ),
  
  tar_target(
    msn,
    readr::read_csv(msn_file) |>
      tanagerharmonize::pre_clean()
  ),
  
  tar_target(
    data_dictionary_values,
    readr::read_csv(file = system.file("extdata", 
                                       "data_dictionary_values.csv", 
                                       package = "tanagerharmonize"))
  ),
  
  tar_target(
    data_dictionary_variables,
    readr::read_csv(file = system.file("extdata", 
                                       "data_dictionary_variables.csv", 
                                       package = "tanagerharmonize"))
  ),
  
  ### National--------------------------------------------
  tar_target(
    moves3,
    read_xl_data(moves3_file)
  ),
  
  tar_target(
    nonroad_consumption,
    readr::read_csv(nonroad_file) |>
      tanagerharmonize::pre_clean()
  ),
  
  tar_target(
    misc_corrections,
    read_xl_data(misc_corrections_file)
  ),
  
  ### State-----------------------------------------------
  tar_target(
    international_bunker_fuels,
    read_xl_data(international_bunker_fuels_file)
  ),
  
  tar_target(
    non_energy_use,
    read_xl_data(non_energy_use_file)
  ),
  
  tar_target(
    ippu_distributions,
    read_xl_data(ippu_distributions_file)
  ),
  
  tar_target(
    foks_diesel,
    read_xl_data(foks_diesel_file) |>
      purrr::pluck(4)
  ),
  
  tar_target(
    foks_residual,
    read_xl_data(foks_residual_file) |>
      purrr::pluck(4)
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
    cue = tar_cue_age(age = as.difftime(30, units = "days"))
  ),
  
  tar_target(
    carbon_coefficients,
    get_carbon_factors(
      carbon_factors,
      neu_storage)
  ),
  
  ### National-----------------------------------------------
  tar_target(
    us_consumption,
    get_national_results() |>
      standardize_ffc(), 
    # Pull data only if it's a month old
    cue = tar_cue_age(age = as.difftime(30, units = "days"))
  ),
  
  tar_target(
    eia_heat_content,
    get_heat_content(), 
    # Pull data only if it's a month old
    cue = tar_cue_age(age = as.difftime(30, units = "days"))
  ),
  
  tar_target(
    vessel_bunker_dist_fuel,
    get_vessel_bunker(), 
    # Pull data only if it's a month old
    cue = tar_cue_age(age = as.difftime(30, units = "days"))
  ),
  
  tar_target(
    ethanol_tra,
    get_ethanol_tra(), 
    # Pull data only if it's a month old
    cue = tar_cue_age(age = as.difftime(30, units = "days"))
  ),

  # Mobile is in-work 
  # tar_target(
  #   mobile_adjustments,
  #   get_mobile_adjustments_data(
  #     moves3,
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
      national_ffc_data,
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
      # national_ffc_data,
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
                            msn) |>
      standardize_ffc(), 
    # Pull data only if it's a month old
    cue = tar_cue_age(age = as.difftime(30, units = "days"))
  ),
  
  tar_target(
    territories,
    get_territories_data() |>
      standardize_ffc(), 
    # Pull data only if it's a month old
    cue = tar_cue_age(age = as.difftime(30, units = "days"))
  ),
  
  tar_target(
    state_adjustments,
    state_ffc_get_adjustments_data(
      national_ffc_adjusted,
      international_bunker_fuels,
      misc_adjustments,
      non_energy_use,
      ippu_distributions,
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
      fhwa_data,
      general_data
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




