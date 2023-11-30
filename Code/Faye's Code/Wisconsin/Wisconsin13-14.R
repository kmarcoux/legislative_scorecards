#####set up
rm(list = ls())
library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)
library(dplyr)

####load data
setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")
wi_text <- pdf_text("Data/raw/Wisconsin/2013-2014ScorecardWeb.pdf")



wi_text[9]%>% 
  substr(str_locate(., 'DISTRICT'), str_locate(., 'JOHN')-1) %>%     ## Subset to the right rows
  str_replace_all(" {2,}", "\\|") %>%                                       ## Replace double spaces with pipes
  str_replace_all("\\\n\\|", "\\\n") %>% 
  read_delim(delim = "|")







