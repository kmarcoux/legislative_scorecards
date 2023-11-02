

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
  substr(str_locate(., "Aghazarian"), str_locate(., "know")-1) %>%
  str_replace_all(" {2,}", "\\|") %>%
  str_replace_all("N N", "N|N") %>%
  str_replace_all("\\) ", "\\)|") %>%
  str_replace_all("\\\n\\|", "\\\n") %>%    
  str_replace_all(" {2,}", "\\|") %>% 
  read_delim(delim = "|", col_names = F) 

