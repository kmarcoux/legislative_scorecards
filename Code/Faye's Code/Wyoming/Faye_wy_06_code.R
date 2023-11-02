###############################################################################
##Legislative Scorecard PDF Scrap  Code
###############################################################################

##Clean the working environment
rm(list = ls())

library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)

#change the working directory to box legislative scorecards file
setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")

################################################################################
## READ IN DATA AND MERGE
################################################################################

################### Wyoming 2006 ###################


wy_06_data <- pdf_data("Data/raw/Wyoming/WCV Scorecard06.pdf")
###########page12 Half house rep########
## Now select the right page and do some manipulation
wy_assembley_scores_6 <- wy_06_data[[12]] %>%
  mutate(y = round(y/15)) %>%                                 ## We round because other wise the rows arent grouped together correctly - they are too precise
  filter(y>=max(if_else(grepl("Alden", text), y, 0))) %>%     ## Filter to the start row that you want
  mutate(group = cumsum(na.fill(!lag(space), 0))) %>%         ## Find where the string is followed by a space, group those observations together so we get the text how it is supposed to show up
  dplyr::group_by(group) %>%
  dplyr::summarise(text = paste0(text, collapse = " "),              ## Now paste together the text that is followed by a space so the two (or more) words are in the same observation
            x = round(first(x)/35),                           ## Round you x's like you did with the y's (if the text is left-aligned choose first(x), if it is centered aligned, choose mean(x), if it is right-aligned, choose last (x))
            y = first(y)) %>%                                 ## Play around with what function of y to use
  arrange(x, y) %>%                                           ## Sort your data by x, y so that it shows up in a readable fashion when you pivot
  pivot_wider(id_cols = "y",                                  ## Pivot the data wider, the y values will tell you what should be in the same row together
              values_from = "text",                           ## The values of the table will come from the text column
              names_from = "x") %>%                           ## The columns of the table will come from the x column
  












