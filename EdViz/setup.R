# setup.R
library(purrr)

#----- Get Data & Create SideBar Layout -----#
data <- read_csv("Data/data.csv") %>% 
  mutate(Date = ymd(Date))

kpis <- c('% Within 6 Hours', '% Within 9 Hours', '% Within 24 Hours')

sidebar_content <-
  list(
    selectInput("hospitals",
                "Select Hospital",
                choices = unique(data$Hospital) %>% discard(~ .x == 'National'),
                selected = "",
                multiple  = FALSE),
    selectInput("kpis",
                "Select KPI",
                choices = kpis,
                selected = "",
                multiple  = FALSE),
    
    "This Web-App uses publicly available data published by the HSE, available at:", 
    tags$a("Link to HSE Performance Reports", href = "https://www.hse.ie/eng/services/publications/performancereports/") ,
    "Data Validation is always being performed by HSE hospitals, and as such, this data may change over time"
  )