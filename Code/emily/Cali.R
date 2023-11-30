

getwd()
setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")
rm(list = ls())
install.packages("pacman")

library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)


ca_05_text <- pdf_text("Data/raw/California/scorecard_2005.pdf")
rowAlls <- function(x, rows = NULL, cols = NULL, value = TRUE, na.rm = FALSE, dim. = dim(x), ..., useNames = TRUE)
  if (is.numeric(x) && is.logical(value) && !is.na(value)) 
    if (is.na(useNames)) deprecatedUseNamesNA()
    has_nas <- TRUE
    if (isTRUE(value)) 
      counts <- .Call(C_rowCounts, x, dim., rows, cols, FALSE, 1L, na.rm, has_nas, useNames)
      res <- (counts == 0L)
score <- ca_05_text[26] %>% 
  substr(str_locate(., "Pro-Environmental Votes"), str_locate(., "24")-1) %>%
  str_replace_all(" {2,}", "\\|") %>%
  # str_replace_all("N N", "N|N") %>%
  # str_replace_all("\\) ", "\\)|") %>%
  # str_replace_all("\\\n\\|", "\\\n") %>%    
  # str_replace_all(" {2,}", "\\|") %>% 
  read_delim(delim = "|", col_names = F) 


ca_05_data <- pdf_data("Data/raw/California/scorecard_2005.pdf")

score26_data <- ca_05_data[[26]] %>% 
  filter(y>=max(if_else(grepl("Anti", text), y, 0))) %>% 
  mutate(group = cumsum(na.fill(!lag(space), 0))) %>% 
  group_by(group) %>% 
  arrange(y, x) %>% 
  mutate(y = y+height/2) %>% 
  summarize(text = paste0(text, collapse = " "),
            x = first(x), 
            y = round(first(y)/10),
            width = first(width)) %>% 
  mutate(x = if_else(grepl("\\(", text), round(x/10), round((x+width/2)/10))) %>%
  filter(!grepl("ommittee|ssembly", text)) %>% 
  arrange(x) %>% 
  pivot_wider(id_cols = "y",
              values_from = "text", 
              names_from = "x") %>% 
  filter(!is.na(`5`)) %>% 
  select(-`6`, -`9`)


score27_data <- ca_05_data[[27]] %>% 
  filter(y>=max(if_else(grepl("2005", text), y, 0))) %>% 
  mutate(group = cumsum(na.fill(!lag(space), 0))) %>% 
  group_by(group) %>% 
  arrange(y, x) %>% 
  mutate(y = y+height/2) %>% 
  summarize(text = paste0(text, collapse = " "),
            x = first(x), 
            y = round(first(y)/10),
            width = first(width)) %>% 
  mutate(x = if_else(grepl("\\(", text), round(x/10), round((x+width/2)/10))) %>%
  filter(!grepl("ommittee|ssembly", text)) %>% 
  arrange(x) %>% 
  pivot_wider(id_cols = "y",
              values_from = "text", 
              names_from = "x") %>%  
  filter(!is.na(`5`)) %>% 
  select(-`34`)


score26_data_long <- score26_data %>% 
  select(name = `5`, air_1 = `16`, air_2 = `19`, air_3 = `22`,
         coast_6 = `30`, coast_7 = `32`, 
         water_9 = `38`, water_10 = `40`, water_11 = `43`,
         energy_12 = `46`, energy_15 = `54`) %>% 
  pivot_longer(cols = matches("[1-9]"),
               names_to = "bill", 
               values_to = "vote") %>% 
  mutate(tag = sapply(str_split(bill, "\\_"), "[", 1),
         vote = case_when(vote=="8" ~ 0, 
                          vote=="4" ~ 1, 
                          T ~ NA_integer_)) %>% 
  group_by(name, tag) %>% 
  mutate(tag_score = mean(vote, na.rm = T))
