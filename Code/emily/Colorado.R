getwd()
setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")
rm(list = ls())
install.packages("pacman")

library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)
col_05_data <- pdf_data("Data/raw/Colorado/2005-ccv-scorecard.pdf")
col05 <- col_05_data[[13]] %>% 
  filter(y>=max(if_else(grepl("Anderson", text), y, 0))) %>% 
  mutate(group = cumsum(na.fill(!lag(space), 0))) %>% 
  group_by(group) %>% 
  arrange(y, x) %>% 
  mutate(y = y+height/2) %>% 
  summarize(text = paste0(text, collapse = " "),
            x = first(x), 
            y = round(first(y)/10),
            width = first(width))%>%
  mutate(x = if_else(grepl("\\(", text), round(x/10), round((x+width/3)/10)))%>%
  filter(!grepl("Anderson", text)) %>%
  arrange(x) %>% 
  pivot_wider(id_cols = "text",
              values_from = "y", 
              names_from = "x")