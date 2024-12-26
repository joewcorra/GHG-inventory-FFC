# Load packages 
library(targets)

tar_option_set(packages = c("extrafont", "gt", "gtExtras", "httr", "janitor", 
                            "jsonlite", "knitr", "labelled", "openxlsx", 
                            "pdftools", "reactable", "readxl", "rvest", 
                            "showtext", "tidyverse"))
# assertr, shiny, tictoc, waldo

# Source custon functions
source("functions.R")

# Define the pipeline
list(
  tar_target
  
  
)