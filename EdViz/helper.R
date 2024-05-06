# helpers.R

filter_data <- function(hospital, kpi) {
  
  data %>% 
    filter(Hospital %in% hospital,
           KPI %in% kpi)
}


monthly_average_24_75 <- function(data) {
  
  data %>% 
    filter(Metric == '75',
           KPI == '# > 24 Hours') %>% 
    summarise(Mean = mean(Value)) %>% 
    pull(Mean) %>% 
    round() %>% 
    format(big.mark = ",") 
}



monthly_average_dna <- function(data) {
  
  data %>% 
    filter(Metric == 'none',
           KPI == '# DNA') %>% 
    summarise(Mean = mean(Value)) %>% 
    pull(Mean) %>% 
    round() %>% 
    format(big.mark = ",") 
}



plot_line_All <- function(data) {
  
  data <- data %>% 
    filter(Metric == 'All') %>%
    arrange(Date)  
  
  plot_ly(data = data,
          x = ~Date, y = ~Value, 
          type = 'scatter', mode = 'lines+markers',
          marker = list(color = "#5F3DC4", size = 8, line = list(color = '#9BAAB3', width = 2)),
          line = list(shape = 'spline', smoothing = 1.3, color = "#5F3DC4", width = 3)) %>%
    layout(title = "",
           xaxis = list(title = "", type = "date", tickangle = 45, tickfont = list(size = 12, color = 'white'), tickformat = '%b-%y'),
           yaxis = list(title = "", tickfont = list(size = 12, color = 'white'), tickformat = ".1%"),
           paper_bgcolor = '#333333',  
           plot_bgcolor = '#333333',   
           font = list(size = 14),
           margin = list(b = 50)) %>%
    config(displayModeBar = FALSE)
}





plot_line_75 <- function(data){
  
  data <- data %>% 
    filter(Metric == '75') %>%
    arrange(Date)  
  
  plot_ly(data = data,
          x = ~Date, y = ~Value, 
          type = 'scatter', mode = 'lines+markers',
          marker = list(color = "#5F3DC4", size = 8, line = list(color = '#9BAAB3', width = 2)),
          line = list(shape = 'spline', smoothing = 1.3, color = "#5F3DC4", width = 3)) %>%
    layout(title = "",
           xaxis = list(title = "", type = "date", tickangle = 45, tickfont = list(size = 12, color = 'white'), tickformat = '%b-%y'),
           yaxis = list(title = "", tickfont = list(size = 12, color = 'white'), tickformat = ".1%"),
           paper_bgcolor = '#333333',  
           plot_bgcolor = '#333333',   
           font = list(size = 14),
           margin = list(b = 50)) %>%
    config(displayModeBar = FALSE)
  
  
}


barplot_dna <- function(data){
  
  data <- data %>% 
    filter(Metric == 'none', 
           KPI == '# DNA') %>%
    arrange(Date)  
  
  plot_ly(data = data,
          x = ~Date, y = ~Value,
          type = 'bar',
          marker = list(color = "#0C2950", line = list(color = '#9BAAB3', width = 2))) %>%
    layout(title = "",
           xaxis = list(title = "", type = "date", tickangle = 45, tickfont = list(size = 12), tickformat = "%b-%y"),
           yaxis = list(title = "", tickfont = list(size = 12)),
           paper_bgcolor = '#333333',  
           plot_bgcolor = '#333333',   
           font = list(size = 14, color = "white"), 
           margin = list(b = 50)) %>%
    config(displayModeBar = FALSE)
  
}



barplot_24_75 <- function(data){
  
  data <- data %>% 
    filter(Metric == '75', 
           KPI == '# > 24 Hours') %>%
    arrange(Date)  
  
  plot_ly(data = data,
          x = ~Date, y = ~Value, 
          type = 'bar',
          marker = list(color = "#0048A8", line = list(color = '#9BAAB3', width = 2))) %>%
    layout(title = "",
           xaxis = list(title = "", type = "date", tickangle = 45, tickfont = list(size = 12), tickformat = '%b-%y'),
           yaxis = list(title = "", tickfont = list(size = 12)),
           paper_bgcolor = '#333333',  
           plot_bgcolor = '#333333',   
           font = list(size = 14, color = "white"), 
           margin = list(b = 50)) %>%
    config(displayModeBar = FALSE)
}
