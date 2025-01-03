# Load packages 
library(targets)
library(visNetwork)
library(quarto)
library(tarchetypes)

tar_option_set(error = "null", 
               packages = c("extrafont", "gt", "gtExtras", "httr", "janitor", 
                            "jsonlite", "knitr", "labelled", "openxlsx", 
                            "pdftools", "reactable", "readxl", "rvest", 
                            "showtext", "tictoc", "tidyverse"))
# assertr, shiny, tictoc, waldo

# Source custon functions
source("functions.R")

# Define the pipeline

list(
  
  # Both national and state
  tar_target(
    universal_data, 
    # Contains ghgi_values, ghgi_variables, and ghgi_invdb_values
    data_setup()),

  # Both national and state
  tar_target(
    scraped_data,
    scrape_data(universal_data)), 
  
  # Both national and state
  tar_target(
    carbon, 
    get_carbon_factors(universal_data)),
  
  # National
  tar_target(
    national_ffc_data, 
    national_ffc_read_eia_data(universal_data)), 
  
  # National
  # Mobile is in-work ----where are mobile adjustments made in national data?
  tar_target(
    mobile_adjustments,
    get_mobile_adjustments_data(national_ffc_data,
                                scraped_data)),
  
  # National
  # IBF is in-work
  tar_target(
    ibf_adjustments,
    get_ibf_adjustments_data(national_ffc_data,
                                scraped_data)),
  
  # National
  tar_target(
    neu_and_ippu_adjustments, 
    get_neu_ippu_adjustments_data()),
  
  # National
  tar_target(
    national_ffc_adjusted,
    national_ffc_adjust_data(national_ffc_data,
                             mobile_adjustments,
                             neu_and_ippu_adjustments,
                             ibf_adjustments)),
  
  # National
  tar_target(
    carbon_emissions_national,
    national_ffc_calculate_emissions(national_ffc_adjusted,
                                     carbon,
                                     apply_variable_labels,
                                     ghgi_variables)),
  
  # State
  tar_target(
    seds,
    state_ffc_get_seds_data(universal_data)),
  
  # State
  tar_target(
    state_adjustments, 
    state_ffc_get_adjustments_data(national_ffc_adjusted, 
                                   ibf_adjustments, 
                                   neu_and_ippu_adjustments)),

  # State
  tar_target(
    carbon_territories, 
    get_territories_data(carbon, 
                         universal_data)), 
  
  # State
  tar_target(
    seds_all_plus_ind, 
    state_ffc_adjust_data(seds, 
                          state_adjustments, 
                          scraped_data,
                          universal_data)),
  
  # State
  tar_target(
    seds_all_adjusted, 
    seds_all_plus_ind$seds_all_adjusted), 
  
  # State
  tar_target(
    seds_ind_adjusted, 
    seds_all_plus_ind$seds_ind_adjusted), 

  # State
  tar_target(
    carbon_emissions_state,
    state_ffc_calculate_emissions(seds_all_adjusted,
                                  carbon,
                                  universal_data)),
  
  # Figures, Tables, and Quarto Reports
  
  # National 
  tar_target(
    national_ffc_figures,
    national_ffc_ggplot_figures(national_ffc_adjusted, 
                                carbon_emissions_national)),
  
  # National 
  tar_target(saved_national_ffc_figures, 
             {saveRDS(national_ffc_figures, 
                      file = "saved_national_ffc_figures.rds")
               "saved_national_ffc_figures.rds"}, format = "file"),
  
  # National 
  tar_target(national_ffc_tables,
             national_ffc_gt_tables()),
  
  # National 
  tar_target(saved_national_ffc_tables, 
             {saveRDS(national_ffc_tables, 
                      file = "saved_national_ffc_tables.rds")
               "saved_national_ffc_tables.rds"}, format = "file"),
  
  # State
  tar_target(state_ffc_figures,
             state_ffc_ggplot_figures(seds_all_adjusted,
                                      seds_ind_adjusted, 
                                      state_adjustments,
                                      carbon_emissions_state)),
  
  # State
  tar_target(saved_state_ffc_figures, 
             {saveRDS(state_ffc_figures, file = "saved_state_ffc_figures.rds")
               "saved_state_ffc_figures.rds"}, format = "file"),
  
  # State
  tar_target(state_ffc_tables,
             state_ffc_gt_tables()),
  
  # State
  tar_target(saved_state_ffc_tables, 
             {saveRDS(state_ffc_tables, file = "saved_state_ffc_tables.rds")
               "saved_state_ffc_tables.rds"}, format = "file"),
  
  # National
  tar_quarto(
    national_ffc_final_report,
    path = "national_ffc_final_report.qmd"),

  # State
  tar_quarto(
    state_ffc_final_report,
    path = "state_ffc_final_report.qmd")
  
)
  



# tar_make()
# tar_visnetwork()
# tar_read(seds_ind_adjusted) # use any target name
