#####set up
rm(list = ls())
library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)
library(dplyr)

####load data
setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")
wi_text <- pdf_text("Data/raw/Wisconsin/WLCVScorecard2010_web.pdf")

################### helper function################### 
basic_text_scrape_fun <- function(page, start_text, end_text, end_diff){      ## We can define our own function to speed things up
  
  page %>% 
    substr(str_locate(., start_text), str_locate(., end_text)-end_diff) %>%     ## Subset to the right rows
    str_replace_all(" {2,}", "\\|") %>%                                       ## Replace double spaces with pipes
    str_replace_all("\\\n\\|", "\\\n") %>%                                    ## replace pipes after line breaks with just line breaks
    str_replace_all("District Score", "District \\| Score") %>%               ## This line helps with a small problem in the senate table
    str_replace_all("compromise AB", "compromise \\| AB") %>%               ## This line helps with a small problem in the senate table
    read_delim(delim = "|")
  
}
##counts total bill with specific tag
count_bills_with_tag <- function(data_frame, specific_tag) {
  # Split the tags column and check for the specific tag
  count <- sum(sapply(strsplit(data_frame$tags, ",\\s*"), function(x) specific_tag %in% x))
  return(count)
}

each_Lname_each_tag<-function(data_frame, name, vote_sign, tag) {
  person_data <- data_frame[data_frame$name == name,]
  total_bills_tag<-count_bills_with_tag(person_data, tag)
  person_vote <- person_data[person_data$vote == vote_sign,]
  ##people who did not vote (to avoid type list error in count_bills_with_tag function)
  if (nrow(person_vote)==0){
    return(0)
  }
  else {
    person_vote_bills_tag <-count_bills_with_tag(person_vote, tag)
    return (person_vote_bills_tag /total_bills_tag)
  }
}
##calculate the ratio and transfer to score
calculate_and_return_tag_ratios <- function(data_frame, vote_sign) {
  # Get unique tags
  unique_tags <- unique(unlist(strsplit(data_frame$tags, ",\\s*")))
  
  # Create an empty data frame to store the results
  result_df <- unique(data_frame[c(1,2,3,4)])
  
  #each tag score
  result_df[unique_tags] <- lapply(unique_tags, function(tag) {
    sapply(result_df$name, function(name) {
      each_Lname_each_tag(data_frame, name, vote_sign, tag) * 100
    })
  })
  
  return(result_df)
}

################### Scrape the names and final scores for Wisconsin Assembly###################
wi_assembly_scores <- basic_text_scrape_fun(wi_text[10],                    ## Use our function to scrape page 8
                                            "Representative", 
                                            "Legislator", 
                                            4) %>% 
  right_join(                                                                ## Page 9 has the rest of the columns for the representatives on page 8 - we want to bind on these columns using bind_cols() 
    basic_text_scrape_fun(wi_text[11],                                      ## Use our function to scrape page 9
                          "Representative", 
                          "Scorecard", 
                          14), by=c('Representative')) %>% 
  bind_rows(                                                                ## Pages 10 and 11 have the rows for the rest of the representatives - we want to bind on these rows using bind_rows() 
    right_join(                                                              ## Pages 10 and 11 have all the columns for the representatives on page 10 - we want to bind these columns together using bind_cols() 
      basic_text_scrape_fun(wi_text[12],                                  ## Use our function to scrape page 10
                            "Representative", 
                            "Legislator", 
                            4),
      basic_text_scrape_fun(wi_text[13],                                  ## Use our function to scrape page 11
                            "Representative", 
                            "Scorecard", 
                            14), by=c('Representative'))
  ) %>% 
  filter(!is.na(`AB 3`)) %>% ## Drop rows that have NAs
  janitor :: clean_names() %>%  
  rename(name = representative, ab_138_override=override, sb_200=ssb_200) %>%  
  mutate(senate=0, .before = 'name')

wi_assembly_scores<- wi_assembly_scores%>% 
  pivot_longer(cols = names(wi_assembly_scores)[-c(1,2,3,4)],
               names_to = "bill",
               values_to = "vote") %>%
  mutate(
    tags = case_when(bill=="ab_3"~"pollution, water, conservation",
                     bill=="ab_43"~"wildlife",
                     bill=="ab_138"~"other",
                     bill=="ab_138_override"~"other",
                     bill=="ab_139"~"waste",
                     bill=="ab_258"~"waste",
                     bill=="ab_281"~"pollution, water",
                     bill=="ab_372"~"waste, water",
                     bill=="ab_910"~"conservation, other",
                     bill=="sb_6"~"wildlife",
                     bill=="sb_107"~"human_health, waste",
                     bill=="sb_123"~"wildlife, conservation",
                     bill=="sb_185"~"energy",
                     bill=="sb_200"~"toxics, pollution, human_health, water",
                     bill=="sb_271"~"toxics, human_health",
                     bill=="sb_279"~"energy",
                     bill=="sb_616"~"energy, green_building, pollution",
                     bill=="sb_651"~"energy, manufacturing"
    )) 
wi_assembly_scores<-calculate_and_return_tag_ratios(wi_assembly_scores, '+')


###############scrape the names and final scores for Wisconsin senate###############

wi_senate_score <- basic_text_scrape_fun(wi_text[14], "Senator", "JOINT", 1) %>% 
  right_join(
    basic_text_scrape_fun(wi_text[15], "Senator", "KEY", 1), by=('Senator')
  ) %>% 
  filter(!is.na(`SB 185`)) %>% 
  janitor::clean_names() %>% 
  rename(name = senator) %>% 
  mutate(senate = 1, .before='name')

wi_senate_score<-wi_senate_score %>% 
  pivot_longer(cols = names(wi_senate_score)[-c(1,2,3,4)],
               names_to = "bill",
               values_to = "vote") %>%
  mutate(
    tags = case_when(bill=="ab_3"~"pollution, water, conservation",
                     bill=="ab_138"~"other",
                     bill=="sb_107"~"human_health, waste",
                     bill=="sb_167"~"wildlife, conservation",
                     bill=="sb_185"~"energy",
                     bill=="sb_271"~"toxics, human_health",
                     bill=="sb_273"~"energy",
                     bill=="sa_1_to_sb_273"~"energy, pollution, air, waste",
                     bill=="sb_279"~"energy",
                     bill=="sb_557"~"wetland, wildlife",
                     bill=="sb_616"~"energy, green_building, pollution",
                     bill=="sb_651"~"energy, manufacturing"
    )) 
wi_senate_score<-calculate_and_return_tag_ratios(wi_senate_score, '+') 

wi_10_score <- wi_assembly_scores %>% bind_rows(wi_senate_score)













