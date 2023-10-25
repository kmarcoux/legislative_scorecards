################################################################################
## PURPOSE: 
##   - Example scorecard scraping
################################################################################
rm(list = ls())

library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)

setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")
################################################################################
## READ IN DATA AND MERGE
################################################################################

################### Example using pdf_text ###################

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
assembly_legislation <- assembly_scores %>%                            ## distinct() takes the unique observations of the selected variables
  distinct(state, senate, bill, tags)



### Example for Arizona 07 
az07_text <- pdf_text("Data/raw/Arizona/azlcv_scorecard_2007.pdf")

## Scrape the names and final scores from page 14
az_house_names_final_scores <- az07_text[14] %>%                                      ## Select page 14
  substr(str_locate(., "FINAL SCORE")+11, str_locate(., "www\\.azlcv\\.org")-1) %>%   ## Subset to right info - we want the right-most table only so start at that first row
  str_replace_all(" D ", "  D") %>%                                                   ## To get the right number of columns, make sure theres two spaces around the party variable in the left table
  str_replace_all(" {2,}", "\\|") %>%                                                 ## Replace double spaces with pipes
  str_replace_all("\\\n\\|", "\\\n") %>%                                              ## replace pipes after line breaks with just line breaks
  str_replace_all("(Committee Average Score\\: [0-9]{2,}\\.[0-9]\\%\\|)|(House Transportation)", ## The rows with table names in the left tables dont have the right number of columns - this fixes it
                  "\\1\\|\\|\\|") %>%                                                 ## The "\\1" means select the first thing that shows up in parentheses in the pattern portion of the str_replace function
  str_replace_all("\\\n([A-Z][a-z]{2,} [A-Z][a-z]{2,}\\|[0-9])",                      ## Here we want to make sure that the names in the right-hand table show up in the right columns
                  "\\\n\\|\\|\\|\\|\\1") %>%                                          ## We are matching the times that a name following the pattern (Xxxx Xxxx) followed by a number (and not a name followed by a D or an R like in the left tables) gets extra pipes 
  str_replace_all("\\\n([A-Z][a-z]{2,} [A-Z][a-z][A-Z][a-z]{2,}\\|[0-9])",            ## We do the same thing again for the names following the pattern (Xxxx XxXxx) like "Nancy McLain"
                  "\\\n\\|\\|\\|\\|\\1") %>%                                          ## We make sure that there are the right number of pipes to put theses observations in the right columns
  str_replace_all("(Party District\\|Score)",                                         ## The rows with table names in the left tables dont have the right number of columns - this fixes it
                  "\\1\\|") %>% 
  read_delim(delim = "|",
             col_names = F) %>% 
  select(X5, X6) %>% 
  set_names("name", "final_score") %>% 
  filter(!is.na(name))                                                                ## Here we get rid of the NAs to get the list of house members 


## Scrape the voting records from page 15
az_house_scores <- az07_text[15] %>% 
  substr(str_locate(., "\\\n1"), str_locate(., "Arizona")-1) %>% 
  str_replace_all("\\\001", " U ") %>%                                               ## Check marks were read in as "\001"
  str_replace_all("\\\030", " X ") %>%                                               ## X's were read in as "\030" 
  str_replace_all(" {2,}", "\\|") %>%
  str_replace_all("1\\\n\\|House", "1\\|House") %>%
  str_replace_all("\\\n\\|", "\\\n") %>%
  read_delim(delim = "|",
             col_names = F) %>% 
  select(-X15) %>% 
  filter(X1!="D")

## Now bind the member names to their voting records
az_house_final_table <- tibble(name = c("weight", "position")) %>% 
  bind_rows(az_house_names_final_scores) %>% 
  bind_cols(az_house_scores)


  

# ################### Example using pdf_data ###################
## Read the PDF in using pdf_data()
wy_05_data <- pdf_data("Data/raw/Wyoming/WCV Scorecard05 Online Version.pdf")

## Now select the right page and do some manipulation
wy_assembley_scores <- wy_05_data[[12]] %>%               
  mutate(y = round(y/10)) %>%                                 ## We round because other wise the rows arent grouped together correctly - they are too precise 
  filter(y>=max(if_else(grepl("Alden", text), y, 0))) %>%     ## Filter to the start row that you want
  mutate(group = cumsum(na.fill(!lag(space), 0))) %>%         ## Find where the string is followed by a space, group those observations together so we get the text how it is supposed to show up
  group_by(group) %>%                                         
  summarise(text = paste0(text, collapse = " "),              ## Now paste together the text that is followed by a space so the two (or more) words are in the same observation
            x = round(first(x)/10),                           ## Round you x's like you did with the y's (if the text is left-aligned choose first(x), if it is centered aligned, choose mean(x), if it is right-aligned, choose last (x))
            y = first(y)) %>%                                 ## Play around with what function of y to use
  arrange(x, y) %>%                                           ## Sort your data by x, y so that it shows up in a readable fashion when you pivot
  pivot_wider(id_cols = "y",                                  ## Pivot the data wider, the y values will tell you what should be in the same row together
              values_from = "text",                           ## The values of the table will come from the text column
              names_from = "x")                               ## The columns of the table will come from the x column
              

              