# FIGURES

# We may integrate this script into a Markdown report







# Energy Use, State Totals vs. National: 

x <-
  lst(
    states = seds %>% 
      select(sector_description, source_description, year, value) %>% 
      mutate(dataname = "state total", 
             sector_description = if_else(
               str_detect(sector_description, "electric"), 
               "electric power sector", 
               sector_description)),
    
    national = adjustments %>% 
      rename(value = national_value) %>% 
      mutate(dataname = "national total")) %>%
  map(\(.x) filter(.x, str_detect(source_description, "coal|natural gas"), 
                   !str_detect(sector_description, "coke"), 
                   !str_detect(source_description, "gasoline")) %>%
        # Get unadjusted SEDS totals to plot against national totals
        group_by(dataname, sector_description, source_description, year) %>%
        summarize(total_btu = sum(value, na.rm = TRUE)) %>%
        ungroup()) %>%
  list_rbind()


ggplot(x %>% filter(str_detect(source_description, "coal") %>%
                      group_by), 
       aes (x = as.numeric(year), y = total_btu)) + 
  geom_point(aes(color = sector_description, shape = dataname), 
            linewidth = 1) + 
  theme_classic()
  
  
