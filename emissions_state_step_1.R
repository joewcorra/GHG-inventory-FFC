# Calculate State-Level CO2 Emissions
# Step 1: Total Fuel Consumption by Fuel Type and Sector
# State-Level  Source: Based on EIA SEDS (adjusted to match national totals 
# as applicable)

# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

#


# EIA SEDS---------------------------------------------------------------

# EIA’s State Energy Data System (SEDS)
# Those data are broken out by fuel type and sector (residential, commercial,
# industrial, transportation, and electric power) and are available for the 
# years 1960–2021

# Read in data; this should already have been downloaded and saved



# Adjust to Match National Totals----------------------------------------

# If SEDS data totals matched the national totals and there were no further
# adjustments needed (as per Steps 2–7), use the SEDS data

# Recursive: Requires data from Steps 2 and 5

# For fuels where the SEDS totals did not match the national totals (i.e., 
# coal, natural gas, and petroleum coke), adjust fuel use in each sector 
# to match the national totals. This calculation is based on the percentage of
# each fuel used in each state from the SEDS data. For the industrial
# sector, this adjustment was made after subtracting for uses in the
# IPPU sector (see Step 2 below).

# For other fuels where sector totals did not match up (e.g., gasoline 
# and diesel fuel), totals for each fuel type were generally taken from the 
# national Inventory (see Step 5), and the SEDS data or other proxy data 
# sources were used to determine state-level percentages of each fuel use.

# SEDS data -> adjust fuel use -> adjust industrial fuel use after Step 2 ->
# adjust other fuels after Step 5 