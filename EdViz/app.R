library(shiny)
library(tidyverse)
library(bslib)
library(bsicons)
library(thematic)
library(plotly)

source("setup.R")
source("helper.R")

# Set the default theme for plots
ggplot2::theme_set(ggplot2::theme_minimal())


#----- User Interface -----#
ui <- page_sidebar(
  
  # Set CSS theme
  theme = bs_theme(bootswatch = "darkly",
                   version = 5,
                   success = "#4D9086"),
  
  title = div(style = "font-size: 28px;", 'EdViz: Health Service Executive Emergency Department Visualization'),
  sidebar = sidebar(
    sidebar_content,
    HTML('<img src = "logo.png", width = "auto", height = "auto">')
    
  ),
  
  layout_columns(
    card(full_screen = TRUE,
         card_header("% of Attendees Admitted/Discharged within the selected KPI Time Period. (All Ages)"),
         plotlyOutput("line_All")),
    
    card(full_screen = TRUE,
         card_header("% of Attendees Admitted/Discharged within the selected KPI Time Period. (75 and Older)"),
         plotlyOutput("line_75")),
    
    value_box(title = "Avg. # of Patients per Month, who leave before treatment",
              textOutput('avg_dna'),
              showcase = bs_icon("person-walking"),
              theme = "success"),
    
    value_box(title = "Avg. # of Patients per Month, who leave before treatment.(National)",
              textOutput('avg_nat_dna'),
              showcase = bs_icon("globe"),
              theme = "success"),
    
    value_box(title = "Avg # of Patients per Month, waiting over 24 Hours in ED.(75 +)",
              textOutput('avg_24_75'),
              showcase = bs_icon("radioactive"),
              theme = "danger"),
    
    card(full_screen = TRUE,
         card_header("# of Attendees Who Leave before decision to admit/discharge."),
         plotlyOutput("barplot_dna")),
    
    card(full_screen = TRUE,
         card_header("# Waiting Longer than 24 Hours in ED (75+)"),
         plotlyOutput("barplot_24_75")),
    
    
    col_widths = c(6, 6, 4, 4, 4, 6, 6),
    row_heights = c(3, 1, 3)
  )
  
  
)

#----- Server -----#
server <- function(input, output) {
  
  
  selected_hospital <-
    reactive(if (is.null(input$hospitals)) hospitals else input$hospitals)
  
  selected_kpi <-
    reactive(if (is.null(input$kpis)) kpis else input$kpis)
  
  
  selected_data <-
    reactive({
      filter_data(selected_hospital(),
                  selected_kpi())
    })
  
#----- PLOTs & Cards -----#
  
  # Line Plot for All Patients
  output$line_All <- renderPlotly({
    
    plot_line_All(selected_data())
    
  })
  
  # Line Plot for 75+ Patients
  output$line_75 <- renderPlotly({
    
    plot_line_75(selected_data())
    
  })
  
  
  # DNA Column Chart
  output$barplot_dna <- renderPlotly({
    
    combo_data <-
      data %>% 
      filter(Hospital == selected_hospital())
    
    barplot_dna(combo_data)
    
  })
  
  
  
  # Line Plot for 75+ Patients
  output$barplot_24_75 <- renderPlotly({
    
    combo_data <-
      data %>% 
      filter(Hospital == selected_hospital())
    
    barplot_24_75(combo_data)
    
  })
  
  # Average of Monthly DNA's Card
  output$avg_dna <- renderText({
    
    combo_data <-
      data %>% 
      filter(Hospital == selected_hospital())
    
    monthly_average_dna(combo_data)
    
  })
  
  # National Average of DNA's Card
  output$avg_nat_dna <- renderText({
    
    combo_data <-
      data %>% 
      filter(Hospital == 'National')
    
    monthly_average_dna(combo_data)
    
  })
  
  
  # Average of Over 24 hours - 75+ Card
  output$avg_24_75 <- renderText({
    
    combo_data <-
      data %>% 
      filter(Hospital == selected_hospital())
    
    monthly_average_24_75(combo_data)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
