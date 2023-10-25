getwd()
setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")
rm(list = ls())
install.packages("pacman")

library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)
az_07_text <- pdf_text("Data/raw/Arizona/azlcv_scorecard_2007.pdf")


az_scores <- az_07_text[15] %>% 
  str_replace_all(" \001", "\\|") %>%
  str_replace_all(" \030", "\\|") %>%  
  str_replace_all("\\\n\\|", "\\\n") %>% 
  str_replace_all("\\) ", "\\)|") %>% 
  str_replace_all("U N", "U|N") %>% 
  str_replace_all("U U", "U|U") %>%
  str_replace_all("N U", "N|U") %>%
  str_replace_all("U U", "U|U") %>%
  str_replace_all("U Y", "U|Y") %>%
  str_replace_all("Y U", "Y|U") %>%
  str_replace_all("Y Y", "Y|Y") %>%
  str_replace_all("Y N", "Y|N") %>%
  str_replace_all("N N", "N|N") %>%
  read_delim(delim = "|", col_names = F) 
### there are a lot of 001, 003 in the Arizona file, I'm having a hard time cleaning them
### and I don't know what they represent
  
