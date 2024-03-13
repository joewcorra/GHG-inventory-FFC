# INVDB


# Collate state CO2 equivalent data into format for export to InvDB
# Write data directly to InvDB Excel workbook


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:
# ffc_invdb: tibble; co2 equivalent values (from the 'carbon' tibble), grouped
  # by fuel type and formatted to match column names InvDB
# wb: workbook object (openxlsx); data to be saved as InvDB Excel workbook

# Format InvDB Data-------------------------------------------------------

ffc_invdb <-
  carbon %>%
  # Create or modify fields to conform to InvDB
  mutate(Sector = "Energy", 
         Source = "Fossil Fuel Combustion", 
         Subsource = str_remove(sector_description, " sector") %>% 
           str_to_title(),
         GHG = "CO2", 
         State = str_to_upper(state), 
         Fuel = case_when(
           source_description %in% c("coal", "coking coal") ~ "Coal", 
           source_description== "natural gas" ~ "Natural Gas", 
           .default = "Petroleum")) %>%
  # Select InvDB fields
  select(Sector, Source, Subsource, Fuel, State, GHG, Year = year, mmt_co2) %>%
  # Sum mmt CO2 for each Subsource/Fuel/State/Year
  group_by(Sector, Source, Subsource, Fuel, State, GHG, Year) %>%
  summarize(value = sum(mmt_co2, na.rm = TRUE)) %>%
  # Pivot data wide so the years are columns
  pivot_wider(names_from = Year, values_from = value) %>%
  ungroup()

# Write data to InvDB Excel Workbook---------------------------------------

# Load blank Excel workbook
wb <- loadWorkbook("InvDB/InvDB_ffc.xlsx")

# Write data to each set of columns on the worksheet
writeData(wb, select(ffc_invdb, Sector:Fuel), sheet = 1, 
          startCol = 1, startRow = 17, colNames = FALSE) 

writeData(wb, select(ffc_invdb, State), sheet = 1, 
          startCol = 7, startRow = 17, colNames = FALSE) 

writeData(wb, select(ffc_invdb, GHG:last_col()), sheet = 1, 
          startCol = 9, startRow = 17, colNames = FALSE) 

# Save InvDB workbook
saveWorkbook(wb, "InvDB/InvDB_ffc_new.xlsx", overwrite = TRUE)

# Save as csv
write_csv(ffc_invdb, "ffc.csv")

# Save as JSON
write_json(ffc_invdb, "ffc.json")

# # Convert to Python object
# py_run_string("import pandas as pd")
# py_run_string("ffc_invdb = pd.DataFrame(r.df)")
  
