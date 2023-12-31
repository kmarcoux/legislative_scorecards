getwd()
setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")
rm(list = ls())
install.packages("pacman")

library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)




ny_06_text <- pdf_text("Data/raw/New York/votersguide_2006.pdf")
assembly_scores <- ny_06_text[14] %>%           ## Selected the page you want to scrape
  substr(.,                                ## I select only the text that I want to convert to a table
         str_locate(., "Peter Abbate"),    ## The names of the bills in this example are diagonal and are not read correctly by R
         str_locate(., "KEY")-1) %>%       ## So I will have to add these names manually at the end
  str_replace_all(" {2,}", "\\|") %>%      ## Replace double spaces with a pipe
  str_replace_all("\\\n\\|", "\\\n") %>%   ## This replaces the double spaces following a new line - replace those again "\n" means "new line"
  str_replace_all("\\) ", "\\)|") %>%      ## I noticed often that there are only single spaces between columns because the table is tight
  str_replace_all("U N", "U|N") %>%        ## These replaces help to fix these mistakes so I can get a table in a nice format
  str_replace_all("U U", "U|U") %>%        ## Notice that Adobe read checks as "U", Xs as "Y" and N as "N"
  str_replace_all("N U", "N|U") %>%
  str_replace_all("U U", "U|U") %>%
  str_replace_all("U Y", "U|Y") %>%
  str_replace_all("Y U", "Y|U") %>%
  str_replace_all("Y Y", "Y|Y") %>% 
  str_replace_all("Y N", "Y|N") %>%
  str_replace_all("N N", "N|N") %>%
  read_delim(delim = "|", col_names = F) %>%                                   ## read_delim will take our string and turn it into a table!
  set_names(c("name", "scores", "Budget", "Wetland Protection",                ## set_names helps you add variable names 
              "Bigger Better Bottle Bill", "Community Preservation Act",
              "EPF Enhancement", "Environmental Access to Justice Act",
              "Pesticide Phase-Out", "Burn Barrel Ban", 
              "Recyclables in Landfills", "Outdoor Lighting", 
              "Great Lakes Compact", "E-Waste Recycling Act", 
              "School Water Lead Testing", "Cancer Mapping", 
              "Alternative Fuels on the Thruway", "Used Oil Filter Recycling", 
              "Smart Growth Infrastructure Act", "Environmental Justice Report",
              "Urban Pesticide Board", "Mercury Emissions Reductions",
              "Auto Dismantler Regulations", "Farm Pesticide Collection",
              "Power Procurement")) %>% 
  mutate(                                                                   ## mutate() creates new variables
    state = "New York",
    senate = 0,
    party = substr(sapply(str_split(name, "\\("), "[", 2), 1, 1),           ## Manipulate variables so they are in the format we want
    district = str_remove(sapply(str_split(name, "\\-"), "[", 2), "\\)"),
    district = if_else(grepl("Carrozza", district), "26", district),
    score_2006 = sapply(str_split(scores, " "), "[", 1),
    score_2005 = sapply(str_split(scores, " "), "[", 2),
    name = sapply(str_split(name, "\\("), "[", 1)
  ) %>% 
  pivot_longer(cols = matches(" |Budget"),
               names_to = "bill",
               values_to = "vote") %>% 
  mutate(                                                               ## You can add all tags as one variable, just separate each with a comma
    tags = case_when(bill=="Budget" ~ "Other", 
                     bill=="Wetland Protection" ~ "Conservation", 
                     bill=="Bigger Better Bottle Bill" ~ "Waste",
                     bill=="Community Preservation" ~ "Conservation",
                     bill=="EPF Enhancement" ~ "Conservation, Climate",
                     # T ~ "Other"
                     ## ...
    ))

## Now you can make the legislation table very easily!
assembly_legislation <- assembly_scores %>%                            ## distinct() takes the unqiue observations of the selected variables
  distinct(state, senate, bill, tags)


# ################### Example using pdf_data ###################

ny_06_data <- pdf_data("Data/raw/New York/votersguide_2006.pdf")

senate_scores <- ny_06_data[[20]] %>%
  mutate(group = cumsum(na.fill(!lag(space), 0))) %>%
  group_by(group) %>%
  summarise(text = paste0(text, collapse = " "),
            x = last(x),
            y = first(y)) %>%
  ungroup %>%
  filter(
    y>=max(if_else(grepl("James Alesi", text), y, 0)),
    y<max(if_else(grepl("KEY", text), y, 0))
  ) %>%
  pivot_wider(id_cols = "y",
              values_from = "text",
              names_from = "x")




