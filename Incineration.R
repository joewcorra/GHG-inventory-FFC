library(tidyverse)
library(janitor)
library(readxl)
library(httr)
library(rvest)

local_excel_path <- tempfile(fileext = ".xls")

swc_url <- paste0('https://ghgdata.epa.gov/ghgp/service/export?q=&tr=current&ds=E&ryr=2011&cyr=2011&lowE=-20000&highE=23000000&st=&fc=&mc=&rs=ALL&sc=0&is=11&et=&tl=&pn=undefined&ol=0&sl=0&bs=&g1=1&g2=0&g3=0&g4=0&g5=0&g6=0&g7=0&g8=0&g9=0&g10=0&g11=0&g12=0&s1=0&s2=1&s3=0&s4=0&s5=0&s6=0&s7=0&s8=0&s9=0&s10=0&s201=0&s202=0&s203=0&s204=1&s301=0&s302=0&s303=0&s304=0&s305=0&s306=0&s307=0&s401=0&s402=0&s403=0&s404=0&s405=0&s601=0&s602=0&s701=0&s702=0&s703=0&s704=0&s705=0&s706=0&s707=0&s708=0&s709=0&s710=0&s711=0&s801=0&s802=0&s803=0&s804=0&s805=0&s806=0&s807=0&s808=0&s809=0&s810=0&s901=0&s902=0&s903=0&s904=0&s905=0&s906=0&s907=0&s908=0&s909=0&s910=0&s911=0&sf=11001100&allReportingYears=yes&listExport=false')

GET(swc_url, write_disk(local_excel_path, overwrite = TRUE))

data_years <- 2011:(year(Sys.Date()) - 2)

swc_data <- data_years %>%
  map(\(.x) read_excel(local_excel_path, sheet = paste0(.x), skip = 6) %>%
        clean_names()) %>%
  bind_rows()

subpart_c_unit_data <- read_excel("data/emissions_by_unit_and_fuel_type_c_d_aa.xlsx", sheet = 1, skip = 6) %>%
  clean_names()

subpart_c_fuel_data <- read_excel("data/emissions_by_unit_and_fuel_type_c_d_aa.xlsx", sheet = 2, guess_max = 10000, skip = 5) %>%
  clean_names()

fuel_pivot <- filter(subpart_c_fuel_data, specific_fuel_type == "Municipal Solid Waste")

msw_ch4_ef <- 0.032
msw_n2o_ef <- 0.0042
ch4_gwp <- 25
n2o_gwp <- 298

fuel_pivot <- mutate(fuel_pivot, ch4_mmbtu = fuel_methane_ch4_emissions_mt_co2e / ch4_gwp * 1000 / msw_ch4_ef)
fuel_pivot <- mutate(fuel_pivot, n2o_mmbtu = fuel_nitrous_oxide_n2o_emissions_mt_co2e / n2o_gwp * 1000 / msw_n2o_ef)

msw_hhv <- 9.95

fuel_pivot <- fuel_pivot %>% rowwise() %>% mutate(msw_short_tons = mean(c(ch4_mmbtu, n2o_mmbtu)) / msw_hhv)

state_summary <- fuel_pivot %>%
  group_by(state, reporting_year) %>%
  summarize(short_tons = sum(msw_short_tons) / 1000)

state_summary <- state_summary %>% pivot_wider(names_from = reporting_year, values_from = short_tons)

state_names <- data.frame("state" = c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"))

state_summary <- left_join(state_names, state_summary, by = "state")

write_csv(state_summary, "state_data.csv")

msw_co2_data <- left_join(fuel_pivot, subpart_c_unit_data, by = c("reporting_year", "facility_id", "unit_name", "state"))

msw_co2_data <- msw_co2_data %>%
  select(state, facility_id, unit_name, reporting_year,
         unit_co2_emissions_non_biogenic, unit_biogenic_co2_emissions_metric_tons) %>%
  arrange(state)

msw_co2_data_facility <- msw_co2_data %>%
  group_by(state, facility_id, reporting_year) %>%
  summarize(
    unit_co2_emissions_non_biogenic = sum(unit_co2_emissions_non_biogenic),
    unit_biogenic_co2_emissions_metric_tons = sum(unit_biogenic_co2_emissions_metric_tons)
  )

swc_all_years_data <- left_join(swc_data, msw_co2_data_facility,
                                by = c("reporting_year", "ghgrp_id" = "facility_id", "state"))

fuel_pivot_facility <- fuel_pivot %>%
  group_by(state, facility_id, reporting_year) %>%
  summarize(msw_short_tons = sum(msw_short_tons))

swc_all_years_data <- left_join(swc_all_years_data, fuel_pivot_facility,
                                by = c("reporting_year", "ghgrp_id" = "facility_id", "state"))

data_for_years <- swc_all_years_data %>%
  group_by(reporting_year) %>%
  summarize(
    flight_co2    = sum(ghg_quantity_metric_tons_co2e, na.rm = TRUE),
    calc_co2      = sum(unit_co2_emissions_non_biogenic, na.rm = TRUE),
    calc_bio_co2  = sum(unit_biogenic_co2_emissions_metric_tons, na.rm = TRUE),
    calc_short_tons = sum(msw_short_tons, na.rm = TRUE)
  )

calc_ton_data <- fuel_pivot_facility %>%
  group_by(reporting_year) %>%
  summarize(calc_total_tons = sum(msw_short_tons))

results <- left_join(data_for_years, calc_ton_data, by = "reporting_year")

results <- results %>%
  mutate(
    fossil_co2  = flight_co2 / calc_short_tons * calc_total_tons,
    biomass_co2 = calc_bio_co2 / calc_short_tons * calc_total_tons
  )
