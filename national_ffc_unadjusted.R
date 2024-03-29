# Calculate National-Level CO2 Emissions
# Unadjusted Sectors: Residential, Commercial, Electric Power


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Collate Consumption Data------------------------------------------------

# No adjustments EXCEPT dist fuel and mogas (see those scripts).

seds_unadjusted <- lst(
  
  res = us_consumption %>%
    filter(msn %in% c("CLRCB", "NNRCB", "DFRCB", "HLRCB", "KSRCB")), 
  
  com = us_consumption %>% 
    filter(msn %in% c("CLCCB", "NNCCB", "DFCCB", "EMCCB", "HLCCB", 
                      "KSCCB", "MGCCB", "PCCCB", "RFCCB")),
           
  ele = us_consumption %>% # NNEIB   
    filter(msn %in% c("CLEIB", "NNEIB", "DKEIB", "PCEIB", "RFEIB")),   
           
           
           ) %>%
  
  # ISSUES 3/28/24
  # Electric power needs: distillate fuel
  # Need to adjust for distillate fuel oil (com & res, but not electric?)
  # need to adjust motor gas (com)
  # commercial has ethanol in the dataset but not in the spreadsheet
  

# Collapse list into a single data frame
list_rbind()
