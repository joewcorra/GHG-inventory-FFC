# Load packages 
library(targets)
library(visNetwork)

tar_option_set(packages = c("extrafont", "gt", "gtExtras", "httr", "janitor", 
                            "jsonlite", "knitr", "labelled", "openxlsx", 
                            "pdftools", "reactable", "readxl", "rvest", 
                            "showtext", "tidyverse"))
# assertr, shiny, tictoc, waldo

# Source custon functions
source("functions.R")

# Define the pipeline
list(
  tar_target(
    universal_data, 
    # Contains ghgi_values, ghgi_variables, and ghgi_invdb_values
    data_setup()),
  
  tar_target(
    ghgi_variables,
    universal_data$ghgi_variables),
  
  tar_target(
    apply_variable_labels,
    universal_data$apply_variable_labels),
  
  tar_target(
    msn_names,
    universal_data$msn_names), 
  
  tar_target(
    seds,
    state_ffc_get_seds_data(msn_names)),
  
  tar_target(
    scraped_data,
    scrape_data(msn_names)), 
  
  tar_target(
    corrections, 
    state_ffc_get_corrections_data()),
  
  tar_target(
    carbon, 
    get_carbon_factors(apply_variable_labels, 
                       ghgi_variables)),
  
  tar_target(
    carbon_territories, 
    get_territories_data(carbon, 
                         apply_variable_labels, 
                         ghgi_variables)), 
  
  tar_target(
    seds_all_plus_ind, 
    state_ffc_adjust_data(seds, 
                          corrections, 
                          scraped_data,
                          apply_variable_labels, 
                           ghgi_variables)),
  
  tar_target(
    seds_all_adjusted, 
    seds_all_plus_ind$seds_all_adjusted), 
  
  tar_target(
    seds_ind_adjusted, 
    seds_all_plus_ind$seds_ind_adjusted), 

  tar_target(
    carbon_emissions,
    state_ffc_calculate_emissions(seds_all_adjusted,
                                  carbon,
                                  apply_variable_labels,
                                   ghgi_variables)),
  
  tar_target(state_ffc_figures,
             state_ffc_ggplot_figures(seds_all_adjusted,
                                      seds_ind_adjusted, 
                                      corrections,
                                      carbon_emissions)),
  
  tar_target(state_ffc_tables,
             state_ffc_gt_tables()), 
  
  tar_target(
    state_ffc_report, 
    rmarkdown::render("state_ffc_final_report.Rmd", 
                      params = list(data = carbon_emissions, 
                                    figures = state_ffc_figures), 
                      output_file = "state_ffc_final_report.html"))
  )



# tar_make()
# tar_visnetwork()
# tar_read(seds_ind_adjusted) # use any target name
