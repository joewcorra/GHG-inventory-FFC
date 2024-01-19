# Calculate State-Level CO2 Emissions
# Final State Summaries


# Final State Summaries break down by state, source, subsource, year, with subtotals for each source/subsource; fuel = "geothermal" is excluded (?)

# Final Full Dataset will look like this:
tibble(sector = "energy", source = "", subsource = "", fuel = "",
       state = "", ghg = "CO2", year = "2022", value = 0)



sources <- c("FFC", "NEU")
# NEU subsources: transportation, industrial, or us territories only.
# FFC subsources: residential, commercial, industrial, transportation, or
# electricity generation.
subsources <- c("residential", "commercial", "industrial", "transportation",
                "electricity generation", "us territories")
fuels <- c("coal", "natural gas", "petroleum", "geothermal")

years <- as_factor(1990:2024)
# MSNs: 5-character codes. First 2 characters = energy source; 
# 3rd 4th characters = energy activity; 5th character = type of data. 
# There are hundreds of MSNs.

# Data vectors or files to create: 1) sectors; 2) sources; 3) subsources;
# 4) fuels; 5) GHG's; 6) states and territory codes; 7) MSNs; 
# 8) factors, which will vary by source and year; 9) subsource adjustments; 
# 10) ???

# All SEDS data (35 years 1990-2023 inclusive, 54 states, all categories)
# = ~1.4 million rows of data
final_full <- crossing(year = years, state = states, fuel = fuels,
                       source = sources, subsource = subsources) %>%
  filter(!(source == "NEU" & subsource %in% c("residential", 
                                              "commercial", 
                                              "electricity generation"))) %>%
  filter(!(source == "FFC" & subsource == "us territories")) %>%
  filter(!(fuel == "geothermal" & subsource != "electricity generation")) %>%
  mutate(value = rnorm(row_number()))


final_states <- final_full %>% 
  filter(year == "2021") %>%
  group_by(state) %>% 
  group_split() %>% 
  map(\(.x) gt(.x, rowname_col = "year",
               groupname_col = c("source", "subsource")) %>%
        summary_rows(columns = value, fns = "sum") %>% 
        tab_header(
          title = md("**State Summaries**"),
          subtitle = md("by source and subsource")) %>%
        opt_stylize(style = 1,  color = "cyan",
                    add_row_striping = TRUE)) # This may take a while

final_states[10]