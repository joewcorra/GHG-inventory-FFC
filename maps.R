# MAPS


# US state and non-state codes
states_and_dc <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
                   "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
                   "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
                   "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", 
                   "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", 
                   "WY")

state_names <- c("Alaska", "Alabama", "Arkansas", "Arizona", "California",
            "Colorado", "Connecticut", "Delaware", "District of Columbia",
            "Florida", "Georgia", "Hawaii", "Iowa", "Idaho", "Illinois",
            "Indiana", "Kansas", "Kentucky", "Louisiana", "Massachusetts",
            "Maryland", "Maine", "Michigan", "Minnesota", "Missouri",
            "Mississippi", "Montana", "North Carolina", "North Dakota",
            "Nebraska", "New Hampshire", "New Jersey", "New Mexico",
            "Nevada", "New York", "Ohio", "Oklahoma", "Oregon",
            "Pennsylvania", "Rhode Island", "South Carolina",
            "South Dakota", "Tennessee", "Texas", "Utah", "Virginia",
            "Vermont", "Washington", "Wisconsin", "West Virginia",
            "Wyoming")

states <- tibble(states_and_dc, state_names) %>% 
  mutate(state_names = str_to_lower(state_names))

usa <- map_data("state") 


mapdata <- carbon %>% 
  left_join(states, by = c("state" = "states_and_dc")) %>%
  group_by(state_names) %>%
  summarize(mmt_co2 = sum(mmt_co2, na.rm = TRUE))
  
  mymap <- left_join(usa, mapdata, by = c("region" = "state_names"))
  
  
  
  ggplot(mymap, aes(x = long, y = lat)) + 
  geom_polygon(aes(fill = mmt_co2, group = group), color = "black") + 
    scale_color_continuous()
  