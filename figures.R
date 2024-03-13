# FIGURES

# We may integrate this script into a Markdown report

# Create colorblind-friendly palette
okabe_ito_colors <- c("#E69F00", "#000000", "#56B4E9", "#009E73",
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

## Differences in Coal/NG State Totals vs National Totals-------------------

fig_2_2 <- energy_use %>% 
  filter(str_detect(source_description, "coal|natural gas"), 
         # res, com, ind, and ele only 
         sector_description != "transportation sector") %>%
  # Every observation needs both state and national totals
  pivot_wider(names_from = dataname, values_from = total_btu) %>%
  # Remove everything after the comma in 'natural gas'
  # NOTE: Should we be subtracting supplemental fuels for this figure?
  mutate(source_description = word(source_description, sep = ",")) %>%
  group_by(source_description) %>%
  group_split() %>%
  map(\(.x) ggplot(.x, aes(x = as.numeric(year), y = state_total/national_total)) +
# Plot state total as a proportion of national total; 
# I think this is useful since it shows that com and res are identical
  # geom_point(color = "darkgray") +
  geom_line(aes(color = sector_description), 
            linewidth = 1.2) + 
  geom_abline(slope = 0, intercept = 1) + 
  theme_classic() +
  scale_color_manual(values = okabe_ito_colors) +
  scale_x_continuous(n.breaks = 20) + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.08), 
        legend.position = "none", 
        strip.background = element_blank()) + 
  labs(x = "", y = "Ratio: SEDS to National Total") +
  facet_wrap(~ sector_description))
  


## Differences in Petroleum Coke State Totals vs National Totals----------

fig_2_3 <- state_vs_national %>% 
  map(\(.x) 
  group_by(.x, dataname, sector_description, source_description, year) %>%
  summarize(total_btu = sum(value, na.rm = TRUE)) %>%
  ungroup()) %>%
  list_rbind() %>%
  filter(str_detect(source_description, "petroleum coke"), 
                    sector_description == "industrial sector") %>%
  # Every observation needs both state and national totals
  pivot_wider(names_from = dataname, values_from = total_btu) %>%
  ggplot(aes(x = as.numeric(year), y = state_total/national_total)) +
        # Plot state total as a proportion of national total; 
        geom_line(aes(color = sector_description), 
                  linewidth = 1.2) + 
        geom_abline(slope = 0, intercept = 1) + 
        theme_classic() +
        scale_color_manual(values = okabe_ito_colors) +
        scale_x_continuous(n.breaks = 20) + 
        theme(axis.text.x = element_text(angle = 270, vjust = 0.08), 
              legend.position = "none",
              strip.background = element_blank()) + 
        labs(x = "", y = "Ratio: SEDS to National Total")

## Sectoral Differences in Select Fuels-----------------------------------

fig_2_4 <- ggplot(state_vs_national %>% 
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
        legend.position = "none", 
        strip.background = element_blank()) + 
  labs(x = "", y = "Difference: SEDS - national (TBtu) ") +
  # Option 2: facet_wrap to avoid overlapping lines
  facet_grid(~ source_description, scales = "free_x")


## Comparison of Transportation Sector Fuel Use----------------------------


fig_2_5 <- ggplot(state_vs_national %>% 
                    list_rbind() %>% 
                    filter(sector_description == "transportation sector", 
                           str_detect(source_description, 
                                      "distillate|motor")) %>%
                    group_by(dataname, sector_description, source_description, year) %>%
                    summarize(total_btu = sum(value, na.rm = TRUE)) %>%
                    ungroup(),
                  aes(x = as.numeric(year), y = total_btu)) + 
  geom_line(aes(color = dataname), linewidth = 0.5) +
  geom_point(aes(color = dataname), size = 1.3) +
  theme_classic() +
  scale_color_manual(values = okabe_ito_colors) +
  scale_x_continuous(n.breaks = 20) + 
  theme(axis.text.x = element_text(size = 8, angle = 270, vjust = 0.08), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        strip.background = element_blank()) + 
  labs(x = "", y = "tBtu") + 
  facet_grid(~ source_description)

## Comparison of Transportation Sector Fuel Use----------------------------


fig_2_8 <- ggplot(state_vs_national %>% 
         list_rbind() %>% 
         filter(sector_description == "transportation sector", 
                str_detect(source_description, 
                           "distillate|motor")) %>%
         group_by(dataname, sector_description, source_description, year) %>%
         summarize(total_btu = sum(value, na.rm = TRUE)) %>%
         ungroup(),
       aes(x = as.numeric(year), y = total_btu)) + 
  geom_line(aes(color = dataname), linewidth = 0.5) +
  geom_point(aes(color = dataname), size = 1.3) +
  theme_classic() +
  scale_color_manual(values = okabe_ito_colors) +
  scale_x_continuous(n.breaks = 20) + 
  theme(axis.text.x = element_text(size = 8, angle = 270, vjust = 0.08), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        strip.background = element_blank()) + 
  labs(x = "", y = "tBtu") + 
  facet_grid(~ source_description)
