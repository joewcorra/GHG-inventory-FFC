# GHG-inventory-FFC
GHG-inventory-FFC is a collection of scripts designed to facilitate the compilation and analysis of emissions data from fossil fuel combustion (FFC). This project is part of the Inventory of U.S. Greenhouse Gas Emissions and Sinks and is aimed at retrieving, processing, and reporting emissions data at both national and state levels. The project integrates data retrieval, transformation, and reporting, with built-in automated QA/QC (quality assurance/quality control) capabilities.

Features

Data Retrieval: Retrieve emissions data from local files, web scraping, or APIs.

Data Processing: Collate and transform the data to compute emissions.

Emissions Calculation: Calculate emissions for both national and state-level datasets.

Reporting: Generate detailed reports and data outputs based on the compiled data.

Automated QAQC: Ensure data quality with built-in automated checks.


Installation

1. Clone the repository to your local machine:

git clone https://github.com/USEPA/GHG-inventory-FFC.git


2. Ensure that you have the required R packages installed. You can install any missing dependencies by running the following in your R console:

install.packages(c(
    "extrafont", "gt", "gtExtras", "httr", "janitor",
    "jsonlite", "knitr", "labelled", "openxlsx",
    "pdftools", "quarto", "reactable", "readxl", "roxygen2", "rvest",
    "showtext", "tictoc", "tidyverse", 
    "targets", "tarchetypes", "visNetwork"
  )



Usage

This repository contains two main components:

1. National FFC Emissions Calculation

To compute emissions data at the national level, use functions_both.R and functions_national.R. These scripts call on other source scripts and compile data from multiple sources.



2. State-Level FFC Emissions Calculation

To compute emissions data at the state level, use functions_both.R, functions_national.R, and functions_state.R. The latter script processes state-specific data.



Data Sources

Emissions data is retrieved from various sources, including local files, web scraping, and API access.

The specific sources for each dataset are defined within the respective source scripts.



Disclaimer: The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government. cluded in this repository (supported by GitHub, Zenodo, and many reference managers).

If your work makes substantial use of this code, or if you would like to discuss methodological details or potential collaboration, please feel free to contact me.

---
