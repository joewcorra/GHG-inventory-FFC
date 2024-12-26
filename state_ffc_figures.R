# Create state FFC ggplot figures

state_ffc_figures <- function(seds_all_adjusted, corrections) {
  
# Read National Emissions Data (for Figures)------------------------------

national_emissions <- read_excel("data/national_inventory_CO2_data.xlsx", 
                                 sheet = "InvDB", 
                                 skip = 0, range = "C16:BA32") %>%
  clean_names() %>%
  select(sector_description = category, source_description = fuel1, ghg, 
         starts_with("x")) %>%
  pivot_longer(starts_with("x"), names_to = "year", values_to = "value") %>%
  mutate(year = str_sub(year, 2, 5), 
         source_description = str_to_lower(source_description), 
         ghg = str_to_lower(ghg), 
         value = parse_number(value), 
         sector_description = str_to_lower(sector_description) %>% 
           str_c(" sector"), 
         sector_description = if_else(
           str_detect(sector_description, "electric"), 
           "electric power sector", sector_description))

# Color Palettes-----------------------------------------------------------

# Hex	Gas	Sector	Economic Sectors
#4F81BD	Carbon Dioxide	Energy	Residential
#C0504D	Methane	Agriculture	Agriculture
#4198AF	Nitrous Oxide	IPPU	Industry
#9BBB59	HFCs, PFCs, SF6, NF3	LULUF Emissions	Transportation
#D7925D	 	Waste	Commercial
#7F63A1	Net CO2 Flux from LULUCF	LULUCF Removals	Electric Power Industry
#49525E	Net Emissions	Net Emissions	 

# Style Guide Palette
ghg_palette <- c("#4F81BD", "#C0504D", "#4198AF", "#9BBB59",
                      "#D7925D", "#7F63A1", "#49525E")

myPalette <- colorRampPalette(c("thistle1","slateblue3"), space = "Lab")

# Create Data Object (SEDS + National)--------------------------------------

# Energy Use, State Totals vs. National: 

state_vs_national_btu <-
  lst(
    states = seds_all_adjusted %>% 
      select(sector_description, source_description, year, value) %>% 
      mutate(value = value / 1000, 
        dataname = "state_total", 
             # sector_description = case_when(
             #   str_detect(sector_description, 
             #              "electric") ~ "electric power sector", 
             #   str_detect(sector_description, 
             #              "industrial consumption") ~ "industrial sector", 
               # .default = sector_description), 
        # source_description = if_else(
        source_description = if_else(source_description %in% c(
          "hgl", "propane", "propylene",
          "ethane","ethylene",
          "normal butane", "butylene",
          "isobutane", "isobutylene"), "lpg",
          source_description)),
    
    national = corrections$adjustments %>% 
      rename(value = national_value) %>% 
      mutate(dataname = "national_total", 
             value = value / 1000, 
             source_description = case_when(
               source_description == "other coal" ~  "coal", 
               source_description == "hgl" ~  "lpg",
               .default = source_description)))
  
energy_use <- state_vs_national_btu %>%
  map(\(.x) filter(.x, str_detect(source_description, "coal|natural gas")) %>%
        # Get unadjusted SEDS totals to plot against national totals
        group_by(dataname, sector_description, source_description, year) %>%
        summarize(total_btu = sum(value, na.rm = TRUE)) %>%
        ungroup()) %>%
  list_rbind()

# CO2 Emissions, State Totals vs. National: 

# Energy Use, State Totals vs. National: 

state_vs_national_co2 <-
  lst(
    states = state_ffc_results$carbon_emissions %>% 
      select(sector_description, year, value = mmt_co2) %>% 
      mutate(dataname = "state_total", 
             ghg = "co2"),
    
    national = national_emissions %>% 
      select(-source_description) %>%
      mutate(dataname = "national_total"))

carbon_comparison <- state_vs_national_co2 %>%
  map(\(.x) 
        # Get unadjusted SEDS totals to plot against national totals
        group_by(.x, dataname, sector_description, year) %>%
        summarize(total_co2 = sum(value, na.rm = TRUE)) %>%
        ungroup()) %>%
  
  list_rbind()


# Plots---------------------------------------------------------------------

# We should decide at the outset if we want to plot the raw values, 
# transformed values, the differences, or the proportions so we can be 
# consistent across figures and avoid confusion. 

## Differences in Coal/NG State Totals vs National Totals-------------------
state_ffc_figures <- lst( 
  
fig_2_2 <- energy_use %>% 
  # Ignore coking coal and gasoline
  filter(!str_detect(source_description, "cok|gasoline"), 
         # res, com, ind, and ele only 
         sector_description != "transportation sector") %>%
  # Remove everything after the comma in 'natural gas'
  # NOTE: Should we be subtracting supplemental fuels for this figure?
  mutate(source_description = case_when(
    str_detect(source_description, "coal") ~ "coal",
    str_detect(source_description, "natural gas") ~ "natural gas")) %>%
  # Every observation needs both state and national totals
  pivot_wider(names_from = dataname, values_from = total_btu) %>%
  ggplot(aes(x = as.numeric(year), y = state_total - national_total)) +
  geom_line(aes(color = sector_description), 
            linewidth = 2.8) + 
  # geom_abline(slope = 0, intercept = 1) + 
  theme_classic() +
  scale_color_manual(values = ghg_palette) +
  scale_x_continuous(n.breaks = 20) + 
  theme(axis.text.x = element_text(size = 18, angle = 270, vjust = 0.08), 
        axis.text.y = element_text(size = 18), 
        axis.title.y = element_text(size = 20), 
        strip.background = element_blank(), 
        strip.text.x = element_text(size = 38),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.text = element_text(size = 24),
        legend.title = element_blank()) + 
  labs(x = "", y = "Difference: SEDS - national (TBtu) ") +
  facet_wrap(~ source_description),
  


## Differences in Petroleum Coke State Totals vs National Totals----------

fig_2_3 <- state_vs_national_btu %>% 
  map(\(.x) 
  group_by(.x, dataname, sector_description, source_description, year) %>%
  summarize(total_btu = sum(value, na.rm = TRUE)) %>%
  ungroup()) %>%
  list_rbind() %>%
  filter(str_detect(source_description, "petroleum coke"), 
                    sector_description == "industrial sector") %>%
  # Every observation needs both state and national totals
  pivot_wider(names_from = dataname, values_from = total_btu) %>%
  ggplot(aes(x = as.numeric(year), y = state_total - national_total)) +
        # Plot state total as a proportion of national total; 
        geom_line(aes(color = sector_description), 
                  linewidth = 3) + 
        # geom_abline(slope = 0, intercept = 1) + 
        theme_classic() +
        scale_color_manual(values = ghg_palette) +
        scale_x_continuous(n.breaks = 20) + 
        theme(axis.text.x = element_text(size = 18, angle = 270, vjust = 0.08),
              axis.text.y = element_text(size = 18),
              axis.title.y = element_text(size = 24),
              # axis.title.y = 
              plot.title = element_text(family = "Calibri"),
              text = element_text(family = "Calibri"),
              legend.position = "none") + 
        labs(x = "", y = "Difference: SEDS - national (TBtu) "),

## Sectoral Differences in Select Fuels-----------------------------------

fig_2_4a <- state_vs_national_btu %>% 
         list_rbind() %>% 
           mutate(sector_description = word(sector_description)) %>%
         filter(year == "2021", 
                sector_description != "electric", 
                str_detect(source_description, 
                           "kerosene|residual|lubricants")) %>%
         group_by(dataname, sector_description, source_description, year) %>%
         summarize(total_btu = sum(value, na.rm = TRUE)) %>%
         ungroup() %>%
         # Every observation needs both state and national totals
         pivot_wider(names_from = dataname, values_from = total_btu) %>%
  ggplot(aes(x = sector_description, y = state_total - national_total)) + 
  geom_col(linewidth = 0.5, aes(fill = sector_description)) + 
  theme_classic() +
  scale_fill_manual(values = ghg_palette) +
  geom_abline(slope = 0, intercept = 0) + 
  theme(axis.text.x = element_text(size = 12, angle = 270, vjust = 0.08), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 18),
        strip.text.x = element_text(size = 18),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "none", 
        strip.background = element_blank()) + 
  labs(x = "", y = "Difference: SEDS - national (TBtu) ") +
  # Option 2: facet_wrap to avoid overlapping lines
  facet_grid(~ source_description, scales = "free"),


fig_2_4b <- state_vs_national_btu %>% 
  list_rbind() %>% 
  mutate(sector_description = word(sector_description)) %>%
  filter(year == "2021", 
         sector_description != "electric", 
         str_detect(source_description, 
                    "lpg")) %>%
  group_by(dataname, sector_description, source_description, year) %>%
  summarize(total_btu = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  # Every observation needs both state and national totals
  pivot_wider(names_from = dataname, values_from = total_btu) %>%
  ggplot(aes(x = sector_description, y = state_total - national_total)) + 
  geom_col(linewidth = 0.5, aes(fill = sector_description)) + 
  theme_classic() +
  scale_fill_manual(values = ghg_palette) +
  geom_abline(slope = 0, intercept = 0) + 
  theme(axis.text.x = element_text(size = 12, angle = 270, vjust = 0.08), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 14),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "none", 
        strip.background = element_blank()) + 
  labs(x = "", y = ""),

## IPPU Adjustments Made to Industrial Sector Energy Use--------------------


fig_2_5 <- seds_adjusted$seds_ind_adjusted %>%
  mutate(ippu_adjustments = case_when(
    msn == "CLKCB" ~ value * ippu_factor,
    msn == "CLOCB" ~ other_coal_coke_adj + other_coal_is_adj,
    msn == "net natural gas" ~ natural_gas_ammonia_adj + natural_gas_is_adj,
    msn == "RFICB" ~ cb_factor * petrochemical_cb_percent,
    msn == "DFICB" ~ is_distillate_fuel_factor * is_percent,
    .default = 0)) %>%
  mutate(ippu_adjustments = if_else(
    ippu_adjustments < 0, 0, ippu_adjustments)) %>%
  group_by(year) %>%
  summarize(total_ippu_adjustments = sum(
    ippu_adjustments, na.rm = TRUE), 
    percent_of_unadjusted = total_ippu_adjustments / sum(
      value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = total_ippu_adjustments)) +
  geom_col(aes(fill = percent_of_unadjusted * 100)) +
  theme_classic() +
  scale_fill_gradientn(colours = myPalette(100)) +
  theme(axis.text.x = element_text(size = 12, angle = 270, vjust = 0.08), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "bottom", 
        legend.text = ,
        strip.background = element_blank()) + 
  labs(x = "", y = "tBtu", fill = "% of unadj. ind. sector total"),

# Figures 2-6 and 2-7 are infographics built from tables

## Comparison of Transportation Sector Fuel Use----------------------------


fig_2_8 <- ggplot(state_vs_national_btu %>% 
          list_rbind() %>% 
         filter(sector_description == "transportation sector", 
                str_detect(source_description, 
                           "distillate|motor")) %>%
         group_by(dataname, sector_description, source_description, year) %>%
         summarize(total_btu = sum(value, na.rm = TRUE)) %>%
         ungroup(),
       aes(x = as.numeric(year), y = total_btu)) + 
  geom_line(aes(color = dataname), linewidth = 1) +
  geom_point(aes(color = dataname), size = 1.9) +
  theme_classic() +
  scale_color_manual(values = ghg_palette) +
  scale_x_continuous(n.breaks = 20) + 
  theme(axis.text.x = element_text(size = 10, angle = 270, vjust = 0.08), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "bottom", 
        legend.title = element_blank(), 
        strip.text.x = element_text(size = 20),
        strip.background = element_blank()) + 
  labs(x = "", y = "tBtu") + 
  facet_grid(~ source_description),


# Fig 2-9 requires the full suite of Transport sector data


## Adjustments made to Industrial Sector for NEUs---------------------------

fig_2_10 <- state_ffc_results$seds_all_adjusted %>%
  filter(sector_description == "industrial sector") %>%
  group_by(year) %>%
  summarize(total_neu_adjustments = sum(
    neu_adjusted_value, na.rm = TRUE), 
    percent_of_unadjusted = total_neu_adjustments / sum(
      value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = total_neu_adjustments)) +
  geom_col(aes(fill = percent_of_unadjusted * 100)) +
  theme_classic() +
  scale_fill_gradientn(colours = myPalette(100)) +
  theme(axis.text.x = element_text(size = 8, angle = 270, vjust = 0.08), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "bottom", 
        strip.background = element_blank()) + 
  labs(x = "", y = "tBtu", fill = "% of unadj. ind. sector total"),


## Adjustments Made to Transportation Sector for IBFs------------------------

fig_2_11 <- state_ffc_results$seds_all_adjusted %>%
  filter(sector_description == "transportation sector") %>%
  group_by(year) %>%
  summarize(total_ibf_adjustments = sum(
   ibf_adjusted_value, na.rm = TRUE), 
    percent_of_unadjusted = total_ibf_adjustments / sum(
      value, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = total_ibf_adjustments)) +
  geom_col(aes(fill = percent_of_unadjusted * 100)) +
  theme_classic() +
  scale_fill_gradientn(colours = myPalette(100)) +
  theme(axis.text.x = element_text(size = 8, angle = 270, vjust = 0.08), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 22),
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "bottom", 
        strip.background = element_blank()) + 
  labs(x = "", y = "tBtu", fill = "% of unadj. trans. sector total"),
  
  
## Differences in State-Level Total and National Total FFC CO2 Emissions------


fig_2_12a <- carbon_comparison %>%
  group_by(year, sector_description, dataname) %>%
  summarize(value = sum(total_co2, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = dataname, values_from = value) %>%
  ggplot(aes(x = as.numeric(year), y = state_total - national_total)) +
  geom_col(aes(fill = sector_description), position = "dodge", width = 2) + 
  # geom_abline(slope = 0, intercept = 1) + 
  theme_classic() +
  scale_color_manual(values = ghg_palette) +
  theme(axis.text.x = element_text(size = 14, angle = 270, vjust = 0.08), 
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(size = 16), 
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.text = element_text(size = 14),
        legend.title = element_blank(), 
        legend.position = "bottom") + 
  labs(x = "", y = "Difference: SEDS - national (MMT CO2) "),

fig_2_12b <- carbon_comparison %>%
  group_by(year, dataname) %>%
  summarize(value = sum(total_co2, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = dataname, values_from = value) %>%
  ggplot(aes(x = as.numeric(year), y = state_total - national_total)) +
  geom_col(fill = "darkgreen", color = "green", width = 0.9) + 
  # geom_abline(slope = 0, intercept = 1) + 
  theme_classic() +
  scale_color_manual(values = ghg_palette) +
  theme(axis.text.x = element_text(size = 14, angle = 270, vjust = 0.08), 
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(size = 16), 
        plot.title = element_text(family = "Calibri"),
        text = element_text(family = "Calibri"),
        legend.position = "none") + 
  labs(x = "", y = "Difference: SEDS - national (MMT CO2) "),


## Differences in State-Level and National Total NEU CO2 Emissions--------

# Not sure what data I'm looking at in this figure--ask Vince

# fig_2_15 <- seds_all_adjusted %>%
#   filter(!is.na(neu_adjusted_value)) %>%
#   group_by(year) %>%
#   summarize(total_neu_adjustments = sum(
#     neu_adjusted_value, na.rm = TRUE), 
#     percent_of_unadjusted = total_ibf_adjustments / sum(
#       value, na.rm = TRUE)) %>%
#   ungroup() %>%
#   ggplot(aes(x = year, y = total_ibf_adjustments)) +
#   geom_col(aes(fill = percent_of_unadjusted * 100)) +
#   theme_classic() +
#   scale_fill_gradientn(colours = myPalette(100)) +
#   theme(axis.text.x = element_text(size = 8, angle = 270, vjust = 0.08), 
#         axis.text.y = element_text(size = 12),
#         axis.title.y = element_text(size = 22),
#         legend.position = "bottom", 
#         strip.background = element_blank()) + 
#   labs(x = "", y = "tBtu", fill = "% of unadj. trans. sector total")

)
 return(state_ffc_figures)

}
