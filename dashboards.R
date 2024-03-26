# Shiny Dashboard with data table (reactable) and plotting


# Prepare data for display
seds_db_formatted <- carbon %>%
  select(state, year, msn, sector_description, source_description, 
         raw_BTU = value, adjusted_BTU = adjusted_value, 
         NEU_IBF_adjusted_BTU = neu_ibf_adjusted_value, 
         carbon_factor, mmt_co2) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) 

# Mapping data
usa <- map_data("state") 

# Create tibble of state names; required for map data
states <- tibble(states_and_dc, state_names) %>% 
  mutate(state_names = str_to_lower(state_names))

mapdata <- carbon %>% 
  left_join(states, by = c("state" = "states_and_dc")) %>%
  group_by(state_names, year) %>%
  summarize(mmt_co2 = sum(mmt_co2, na.rm = TRUE))




# Create colorblind-friendly color palette for plots
cbPalette = c("#465177", "#E4C22B", "#965127", "#29483A", "#759C44", "#9FB6DA", 
              "#DF3383")



# UI
ui <- fluidPage( 
  titlePanel("Fossil Fuels GHG Emissions"), 
  sidebarLayout( 
    sidebarPanel(width = 2,  
      selectInput("year", "Choose year:",
                  choices = unique(seds_db_formatted$year), 
                  multiple = TRUE, selected = "1990"),
      selectInput("state", "Choose state:",
                  choices = unique(seds_db_formatted$state), 
                  multiple = TRUE, selected = "AK"),
      selectInput("sector", "Choose sector:",
                  choices = unique(seds_db_formatted$sector_description), 
                  multiple = TRUE, selected = "commercial sector"),
      selectInput("source", "Choose source:",
                  choices = unique(seds_db_formatted$source_description), 
                  multiple = TRUE, selected = "commercial coal"),
      sliderInput("emissions", "Select range (CO2 MMT eq.)", 
                  min = min(seds_db_formatted$mmt_co2, na.rm = TRUE), 
                  max = max(seds_db_formatted$mmt_co2, na.rm = TRUE), 
                  value = c(min(seds_db_formatted$mmt_co2, na.rm = TRUE), 
                            max(seds_db_formatted$mmt_co2, na.rm = TRUE))) 
    ), 
    mainPanel(tabsetPanel(
      tabPanel("Data Table",
               reactableOutput("table")),
      tabPanel("Scatter plot", 
               plotOutput("plot")), 
      tabPanel("Column plot", 
               plotOutput("plot2")), 
      tabPanel("Map", 
               plotOutput("map"))
    )
    ) 
  ) 
) 

# Shiny server logic 
server <- function(input, output) { 
  output$table <- renderReactable({ 
    seds_db_formatted %>% 
      rename(sector = sector_description, source = source_description) %>%
      filter(year %in% input$year, 
             state %in% input$state,
             sector %in% input$sector,
             source %in% input$source) %>%
      filter(mmt_co2 >= input$emissions[1], mmt_co2 <= input$emissions[2]) %>% 
      # select(Name, all_of(input$variable)) %>% 
      reactable() 
  })
  
  # Scatter Plots
  output$plot <- renderPlot({
    seds_db_formatted %>% 
      rename(sector = sector_description, source = source_description) %>%
      filter(year %in% input$year, 
             state %in% input$state,
             sector %in% input$sector,
             source %in% input$source) %>%
      filter(mmt_co2 >= input$emissions[1], mmt_co2 <= input$emissions[2]) %>%
      ggplot() +
      geom_text(aes(x = NEU_IBF_adjusted_BTU, y = mmt_co2,  
                    label = state, color = source, fontface = "bold")) +
      # geom_smooth(aes(x = NEU_IBF_adjusted_BTU, y = mmt_co2, color = source), 
      #           method = "lm") + 
      scale_color_manual(values = cbPalette) + 
      theme_classic() +
      theme(axis.title.x = element_text(size = 14), 
            axis.title.y = element_text(size = 14), 
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12)) + 
      ggtitle("Emissions by Year") + 
      facet_wrap(~ sector)
    
    
  })
  
  # Bar Plots
  output$plot2 <- renderPlot({
    seds_db_formatted %>% 
      rename(sector = sector_description, source = source_description) %>%
      filter(year %in% input$year, 
             state %in% input$state, 
             sector %in% input$sector,
             source %in% input$source) %>%
      ggplot(aes(x = year, y = mmt_co2, fill = source)) +
      geom_col() + 
      scale_fill_manual(values = cbPalette) + 
      theme_classic() +
      theme(axis.title.x = element_text(size = 14), 
            axis.title.y = element_text(size = 14), 
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12)) + 
      ggtitle("Emissions by Year") + 
      facet_wrap(~ state + sector)
    
    
  })
  
  # Map
  output$map <- renderPlot({
    usa %>% left_join(mapdata, by = c("region" = "state_names")) %>%
      filter(year %in% input$year) %>%
      ggplot(aes(x = long, y = lat)) + 
      geom_polygon(aes(fill = mmt_co2, group = region), color = "black") + 
      scale_color_continuous() + 
      facet_wrap(~ year)
    
    
  })
  
} 


# Run the application
shinyApp(ui, server)
