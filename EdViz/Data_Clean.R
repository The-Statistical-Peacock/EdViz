library(readxl)
library(tidyverse)
library(magrittr)

#----- ED All Pet Times -----#
Pet_All <- read_excel("EdViz/Data/EdViz-Data.xlsx", sheet = "PET (ALL)")


Pet_All %<>% pivot_longer(cols = -1,
                         values_to = 'Value')
  
Pet_All %<>% separate("name", into = c("Date", "KPI"), sep = " - ") %>% 
  mutate(Date = trimws(Date),
         KPI = trimws(KPI),
         Date = dmy(Date),
         Metric = 'All')


#----- ED 75+ Pet Times -----#
Pet_75 <- read_excel("EdViz/Data/EdViz-Data.xlsx", sheet = "PET (75+)")

Pet_75 %<>% pivot_longer(cols = -1,
                          values_to = 'Value')

Pet_75 %<>% separate("name", into = c("Date", "KPI"), sep = " - ") %>% 
  mutate(Date = trimws(Date),
         KPI = trimws(KPI),
         Date = dmy(Date),
         Metric = '75')


#----- ED DNA -----#
DNA <- read_excel("EdViz/Data/EdViz-Data.xlsx", sheet = "ED DNA")

DNA %<>% pivot_longer(cols = -1,
                          values_to = 'Value')

DNA %<>% separate("name", into = c("Date", "KPI"), sep = " - ") %>% 
  mutate(Date = trimws(Date),
         KPI = trimws(KPI),
         Date = dmy(Date),
         Metric = 'none') 

data <- rbind(Pet_All, Pet_75, DNA) %>% 
  rename(Hospital = `Reporting Level`) %>%
  filter(!grepl("group|children's", `Hospital`, ignore.case = TRUE)) %>% 
  drop_na()


write_csv(data, "EdVIz/Data/data.csv")

