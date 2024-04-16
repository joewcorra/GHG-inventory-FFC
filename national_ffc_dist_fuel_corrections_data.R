# Calculate Distillate Fuel Oil Adjustments
# Applies to Residential, Commercial, Industrial, Transportation, Elec Power


# Objects Created--------------------------------------------------------

# List of objects created in the global environment:


# Distillate Fuel Adjustments--------------------------------------------

# commercial distillate fuel 2010 = mo gas! summary v8 = EIA adjust! c21/ 10^3
# EIA adjust! c21  = estimated commercial consumption = C7 / C17 * C16
# C7 = commercial dist fuel data from EIA = EIA_Output QBtu'!G16 * 10^6
# C16 = total res com ind, Bottom up analysis = C12 - C15
# C17 = sales for res com ind, Bottom up analysis = C6 + C7 + C8
# C12 = total without ele power = C11 - C10
# C15 = transportation, Bottom up analysis = Trans!C41 / 1000
# C6, 7, 8 = com, ind, res dist fuel data from EIA (see line 14)
# C10 = ele power dist fuel data from EIA (see line 14)
# C11 = sum of res com ind tra ele dist fuel data from EIA (see line 14)
# mogas Trans sheet
# Trans!C41 = dist fuel consumption MMBTU, all classes = C31 * heat contents!C39
# C31 = dist fuel consumption barrels, all classes = C21 / 42
# [transport]heat contents!C39 = heat contents for dist fuel mmbtu/barrel
# C21 = dist fuel consumption gallons, all classes = C7 - C17
# C7 = dist fuel consumption gallons, all classes, inc biodiesel = 
  # sum of all classes incl trains & boats = sum C8 C9 C10 C11 C12 C13
# C17 = biodiesel = [Add_Var_22_FR.xls]Distillate  -Biodiesel'!AU7) * 42 * 1000
# [Add_Var_22_FR]DistillateBiodiesel!AU7) = EIA input???????????????????
# C8, 9, 10, 11, 13= [transport]Main calcs!C14, 15, 16, 17, 20
# C12 = [Mobile]EIA Vessel Bunkering'!D48*1000
# [transport]Main calcs!C14:17: dist fuel by class = [VMT]Outputs!E40:43
# [transport]Main calcs!C20: rail dist fuel = [Mobile]Non_HW_Input!F$13
# [Mobile]EIA Vessel Bunkering'!D48 = US dist fuel = 2064842,HARD CODED see below
# [Mobile]Non_HW_Input!F$13 = locomotive diesel = F102
# F102 = sum rail clas I, II, III, commuter, amtrak = sum E98:101
# E98:101 = HARD CODED see below
# [VMT]Outputs!E40:43 = FHWA MPG = HARD CODED see below



# EIA stuff
# EIA [Add_Var_22_FR]DistillateBiodiesel!AU7)
# commercial dist fuel data from EIA = EIA_Output QBtu'!G16 *
# Vessel bunkering US: Tables 13 "Adjusted Sales of Diesel by End Use 
  # in the United States" in EIA annual Fuel Oil and Kerosene Sales. Energy 
  # Information Administration, U.S. Department of Energy. Washington, D.C.
  # Available online at: <http://www.eia.gov/petroleum/fueloilkerosene/>.

# FWHA Source: FHWA Annual Highway Statistics, Table VM-1.  
    # https://www.fhwa.dot.gov/policyinformation/statistics.cfm

# Rail diesel:
  # Amtrak: Louise Huttinger: data pulled from DOE Transportation Energy Data 
    # Book Table A.16. http://cta.ornl.gov/data/index.shtml  
  # Commuter rail: Beth Moore: Table a.14 of TEDB. Starting in 2003, data pulled 
    # directly from APTA web site.
    # Louise Huttinger: Starting in 2007, data pulled from Public Transportation 
      # Fact Book - Appendix A - Table 57.  
      # APTA (2007 through 2017) Public Transportation Fact Book.
      # at <http://www.apta.com/resources/statistics/Pages/transitstats.aspx>.
  # Class II & III Rail: Beth Moore: 1990-2001 data from Doug Benson, 
    # Upper Great Plains Transportation Institute, Doug.Benson@ndsu.nodak.edu.
    # Louise Huttinger: Data for BY2006 and BY2013 is from David Whorton.  
        # Document Reference: Whorton, D. (2006 through 2014) Personal 
        # communication, Class II and III Rail Energy Consumption, 
        # American Short Line and Regional Railroad Association.
    #  Emily Peterson: Data for BY2013 and forward is estimated based on 
    # carload data reported from RailInc. Calculations in 'Class II and III 
    # Diesel Consumption Estimates.xlsx'.
  # Class I Rail: Louise Huttinger: From Railroad Facts.  Advanced estimates 
    # can be received from Clyde Crimmel at AAR.


# Rail sources: https://tedb.ornl.gov/wp-content/uploads/2022/03/TEDB_Ed_40.pdf
  # Table A.13: Class I rail
  # Table A.14: Commuter rail
  # Table A.16: Amtrak

