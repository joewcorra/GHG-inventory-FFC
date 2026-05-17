
# Get list of all CSV files in the "data" folder
files <- list.files("data", pattern = "\\.csv$", full.names = TRUE)

# Read all CSVs into a list of data frames
data_list <- lapply(files, read_csv)

# (Optional) Name each list element after its file
names(data_list) <- basename(files %>% str_remove_all(".csv"))

# View the names of the loaded data frames
names(data_list)


data_list[2]
list2env(data_list, envir = .GlobalEnv)

reshape <- function(data) {
  
  data <- data %>% pre_clean() %>% 
    pivot_longer(cols = starts_with("x"), 
                 values_to = "value", names_to = "year") %>% 
    mutate(year = parse_number(year) %>% as_factor())
  return(data)
}

carbon_factors_fixed <- carbon_factors_fixed %>% pre_clean() 
carbon_factors_variable <- reshape(carbon_factors_variable)
feedstock_export_adjustments <- reshape(feedstock_export_adjustments)
foks_diesel <- reshape(foks_diesel)
foks_residual <- reshape(foks_residual)
international_bunker_fuels<- reshape(international_bunker_fuels)

ippu_dist_ammonia <- reshape(ippu_dist_ammonia)
ippu_dist_carbon_black <- reshape(ippu_dist_carbon_black)
ippu_dist_iron_and_steel<- reshape(ippu_dist_iron_and_steel)
ippu_dist_petrochemical<- reshape(ippu_dist_petrochemical)

lpg_national<- reshape(lpg_national)

misc_corrections<- reshape(misc_corrections)

moves3_fuel <- reshape(moves3_fuel)
moves3_fuel <- reshape(moves3_vmt)

msn

neu_storage <- reshape(neu_storage)

non_energy_use <- reshape(non_energy_use)
rm(temporary_national_inv_data)
rm(data_list)



# Identify all data frames/tibbles in the global environment
df_names <- ls(envir = .GlobalEnv) |>
  keep(~ is.data.frame(get(.x, envir = .GlobalEnv)))

# Write each one to your board using purrr::walk2
walk(df_names, \(nm) {
  pin_write(board, get(nm, envir = .GlobalEnv), name = nm)
})

