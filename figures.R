# FIGURES

# We may integrate this script into a Markdown report

# Create colorblind-friendly palette
okabe_ito_colors <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")



# Create Data Object (SEDS + National)--------------------------------------

# Energy Use, State Totals vs. National: 

state_vs_national <-
  lst(
    states = seds %>% 
      select(sector_description, source_description, year, value) %>% 
      mutate(dataname = "state_total", 
             sector_description = case_when(
               str_detect(sector_description, 
                          "electric") ~ "electric power sector", 
               str_detect(sector_description, 
                          "industrial consumption") ~ "industrial sector", 
               .default = sector_description), 
             source_description = if_else(
               source_description %in% c("hgl", "propane", "propylene",
                                         "ethane","ethylene",
                                         "normal butane", "butylene",
                                         "isobutane", "isobutylene"), "lpg",
               source_description)),
    
    national = adjustments %>% 
      rename(value = national_value) %>% 
      mutate(dataname = "national_total", 
             source_description = case_when(
               source_description == "other coal" ~  "coal", 
               source_description == "hgl" ~  "lpg",
               .default = source_description)))
  
energy_use <- state_vs_national %>%
  map(\(.x) filter(.x, str_detect(source_description, "coal|natural gas"), 
                   !str_detect(sector_description, "coke"),
                   !str_detect(source_description, "coke"),
                   !str_detect(source_description, "gasoline")) %>%
        # Get unadjusted SEDS totals to plot against national totals
        group_by(dataname, sector_description, source_description, year) %>%
        summarize(total_btu = sum(value, na.rm = TRUE)) %>%
        ungroup()) %>%
  list_rbind()

# Plots---------------------------------------------------------------------

# We should decide at the outset if we want to plot the raw values, 
# transformed values, the differences, or the proportions so we can be 
# consistent across figures and avoid confusion. 

## Differences in Coal State Totals vs National Totals-------------------

fig_2_2_coal <- ggplot(energy_use %>% 
                        filter(str_detect(source_description, "coal"), 
                    # res, com, ind, and ele only 
                    sector_description != "transportation sector") %>%
         # Every observation needs both state and national totals
         pivot_wider(names_from = dataname, values_from = total_btu),
       # Plot state total as a proportion of national total; 
       # I think this is useful since it shows that com and res are identical
       aes (x = as.numeric(year), y = state_total/national_total)) + 
  # geom_point(color = "darkgray") +
  geom_line(aes(color = sector_description), 
            linewidth = 1.2) + 
  geom_abline(slope = 0, intercept = 1) + 
  theme_classic() +
  scale_color_manual(values = okabe_ito_colors) +
  scale_x_continuous(n.breaks = 20) + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.08), 
        legend.position = "none") + 
  labs(x = "Year", y = "State total as a proportion of national total") +
  # Option 2: facet_wrap to avoid overlapping lines
  facet_wrap(~ sector_description)
  
## Differences in Natural Gas State Totals vs National Totals-------------

fig_2_2_ng <-ggplot(energy_use %>% 
                      filter(str_detect(source_description, "natural gas"), 
                             # res, com, ind, and ele only 
                             sector_description != "transportation sector") %>%
                      # Every observation needs both state and national totals
                    pivot_wider(names_from = dataname, values_from = total_btu),
                  # Plot state total as a proportion of national total; 
                  # may be useful since it shows that com and res are identical
                  aes (x = as.numeric(year), y = state_total/national_total)) + 
  # geom_point(color = "darkgray") +
  geom_line(aes(color = sector_description), 
            linewidth = 1.2) + 
  geom_abline(slope = 0, intercept = 1) + 
  theme_classic() +
  scale_color_manual(values = okabe_ito_colors) +
  scale_x_continuous(n.breaks = 20) + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.08), 
        legend.position = "none") + 
  labs(x = "Year", y = "State total as a proportion of national total") +
  # Option 2: facet_wrap to avoid overlapping lines
  facet_wrap(~ sector_description)


## Differences in Petroleum Coke State Totals vs National Totals----------

  # As above, but change the filtering. (could purrr::map() this)

## Sectoral Differences in Select Fuels-----------------------------------

sec_fuel_fig <- ggplot(state_vs_national %>% 
         list_rbind() %>% 
           mutate(sector_description = word(sector_description)) %>%
         filter(year == "2021", 
                sector_description != "electric", 
                str_detect(source_description, 
                           "kerosene|residual fuel|lubricants|lpg")) %>%
         group_by(dataname, sector_description, source_description, year) %>%
         summarize(total_btu = sum(value, na.rm = TRUE)) %>%
         ungroup() %>%
         # Every observation needs both state and national totals
         pivot_wider(names_from = dataname, values_from = total_btu), 
       aes(x = sector_description, y = state_total-national_total)) + 
  geom_col(linewidth = 0.5, aes(fill = sector_description)) + 
  theme_classic() +
  scale_fill_manual(values = okabe_ito_colors) +
  geom_abline(slope = 0, intercept = 0) + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.08), 
        legend.position = "none") + 
  labs(x = "Source", y = "State total - national total (TBtu) ") +
  # Option 2: facet_wrap to avoid overlapping lines
  facet_grid(~ source_description, scales = "free_x")


## Comparison of Transportation Sector Fuel Use----------------------------

# No jet fuel??

trans_fuels_fig <- ggplot(state_vs_national %>% 
         list_rbind() %>% 
         filter(sector_description == "transportation sector", 
                str_detect(source_description, 
                           "distillate|motor")) %>%
         group_by(dataname, sector_description, source_description, year) %>%
         summarize(total_btu = sum(value, na.rm = TRUE)) %>%
         ungroup(),
       aes(x = year, y = total_btu)) + 
  geom_line(color = "#999999", linewidth = 0.4) +
  geom_point(aes(color = dataname), size = 1.3) +
  geom_abline(slope = 0, intercept = 1) + 
  theme_classic() +
  scale_color_manual(values = okabe_ito_colors) +
  theme(axis.text.x = element_text(angle = 270, vjust = 0.08), 
        legend.position = "bottom") + 
  labs(x = "Year", y = "tBtu") + 
  facet_grid(~ source_description)
