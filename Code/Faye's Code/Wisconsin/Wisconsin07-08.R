#####set up
rm(list = ls())
library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)
library(dplyr)

####load data
setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")
wi07_text <- pdf_text("Data/raw/Wisconsin/WLCV-Scorecard-2007-2008-WEB.pdf")

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
wi_assembly_scores <- basic_text_scrape_fun(wi07_text[10],                    ## Use our function to scrape page 8
                                            "Representative", 
                                            "Legislator", 
                                            4) %>% 
  bind_cols(                                                                 ## Page 9 has the rest of the columns for the representatives on page 8 - we want to bind on these columns using bind_cols() 
    basic_text_scrape_fun(wi07_text[11],                                      ## Use our function to scrape page 9
                          "AB 157", 
                          "Conservation", 
                          1)) %>% 
  bind_rows(                                                                ## Pages 10 and 11 have the rows for the rest of the representatives - we want to bind on these rows using bind_rows() 
    bind_cols(                                                              ## Pages 10 and 11 have all the columns for the representatives on page 10 - we want to bind these columns together using bind_cols() 
      basic_text_scrape_fun(wi07_text[12],                                  ## Use our function to scrape page 10
                            "Representative", 
                            "Legislator", 
                            4),
      basic_text_scrape_fun(wi07_text[13],                                  ## Use our function to scrape page 11
                            "AB 157", 
                            "Conservation", 
                            1))
  ) %>% 
  filter(!is.na(`AB 131`)) %>% ## Drop rows that have NAs
  janitor :: clean_names() %>%  
  rename(name = representative) %>%  
  mutate(senate=0, .before = 'name')

wi_assembly_scores<- wi_assembly_scores%>% 
  pivot_longer(cols = names(wi_assembly_scores)[-c(1,2,3,4)],
               names_to = "bill",
               values_to = "vote") %>%
  mutate(
    tags = case_when(bill=="act_1"~"democracy",
                     bill=="act_20"~"energy, land, waste, water",
                     bill=="act_227"~"conservation, water, lake",
                     bill=="ab_130"~"wildlife, conservation",
                     bill=="ab_131"~"wildlife",
                     bill=="ab_157"~"climate",
                     bill=="ab_163"~"wetland, conservation",
                     bill=="ab_346"~"energy, waste",
                     bill=="ab_504"~"other",
                     bill=="ab_718"~"land_use, pollution",
                     bill=="ab_804"~"human_health, pollution, land, air, water, conservation, other",
                     bill=="ab_805"~"land_use, conservation",
                     bill=="ajr_34"~"wildlife",
                     bill=="sb_523"~"water, lake, conservation"
    )) 
wi_assembly_scores<-calculate_and_return_tag_ratios(wi_assembly_scores, '+')


###############scrape the names and final scores for Wisconsin senate###############

wi_senate_score <- basic_text_scrape_fun(wi07_text[14], "Senator", "JOINT", 1) %>% 
  bind_cols(
    basic_text_scrape_fun(wi07_text[15], "AB 625", "JOINT", 1)
  ) %>% 
  filter(!is.na(`AB 131`)) %>% 
  janitor::clean_names() %>% 
  rename(name = senator) %>% 
  mutate(senate = 1, .before='name')

wi_senate_score<-wi_senate_score %>% 
  pivot_longer(cols = names(wi_senate_score)[-c(1,2,3,4)],
               names_to = "bill",
               values_to = "vote") %>%
  mutate(
    tags = case_when(bill=="act_1"~"democracy",
                     bill=="act_20"~"energy, land, waste, water",
                     bill=="act_227"~"conservation, water, lake",
                     bill=="ab_130"~"wildlife, conservation",
                     bill=="ab_131"~"wildlife",
                     bill=="ab_625"~"energy",
                     bill=="sb_15"~"other",
                     bill=="sb_49"~"other",
                     bill=="sb_171"~"democracy",
                     bill=="sb_346"~"pollution, toxics, water, human_health",
                     bill=="sb_397"~"waste",
                     bill=="sb_523"~"water, lake, conservation",
                     bill=="sb_553"~"land_use, wetland, conservation"
    )) 
wi_senate_score<-calculate_and_return_tag_ratios(wi_senate_score, '+') 

wi_07_score <- wi_assembly_scores %>% bind_rows(wi_senate_score)

  











