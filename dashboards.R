# Shiny Dashboard with data table (reactable) and plotting


# Prepare data for display
seds_db_formatted <- carbon %>%
  select(state, year, msn, sector_description, source_description, 
         raw_BTU = value, adjusted_BTU = adjusted_value, 
         NEU_IBF_adjusted_BTU = neu_ibf_adjusted_value, 
         carbon_factor, mmt_co2) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) 


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
               plotOutput("plot2"))
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
  
  output$plot <- renderPlot({
    seds_db_formatted %>% 
      rename(sector = sector_description, source = source_description) %>%
      filter(year %in% input$year, 
             state %in% input$state,
             sector %in% input$sector,
             source %in% input$source) %>%
      filter(mmt_co2 >= input$emissions[1], mmt_co2 <= input$emissions[2]) %>%
      ggplot(aes(x = year, y = mmt_co2)) +
      geom_text(aes(label = state, color = source, fontface = "bold")) +
      scale_color_manual(values = cbPalette) + 
      theme_classic() +
      theme(axis.title.x = element_text(size = 14), 
            axis.title.y = element_text(size = 14), 
            axis.text.x = element_text(size = 12), 
            axis.text.y = element_text(size = 12)) + 
      ggtitle("Emissions by Year") + 
      facet_wrap(~ sector)
    
    
  })
  
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
} 


# Run the application
shinyApp(ui, server)
