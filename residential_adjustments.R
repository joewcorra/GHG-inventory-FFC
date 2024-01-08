# Calculate State-Level CO2 Emissions
# Residential Adjustments


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:

# Residential Adjustments-------------------------------------------------

# CLRCB, NGRCB, SFRCB, DFRCB "residential sector"
# coal, natural gas, distillate fuel

# NOTE 1/2/24: need to join adjustments with seds_by_state; not sure if it 
# makes more sense to do this before or after the group_split()

# Any adjusted values = adjustment from US Compare * 
# (state btu / total of all states' btu)

# natural gas = NGRCB - SFRCB (i.e., subtract the supplemental)

