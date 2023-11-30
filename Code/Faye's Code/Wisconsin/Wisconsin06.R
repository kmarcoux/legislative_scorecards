###############################################################################
##Legislative Scorecard PDF Scrap  Code
###############################################################################

##Clean the working environment
rm(list = ls())

library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)
library(dplyr)

#change the working directory to box legislative scorecards file
setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")

################### Wisconsin 2006 ###################
wi06_text <- pdf_text("Data/raw/Wisconsin/WLCV-Scorecard-2006.pdf")

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
wi_assembly_scores <- basic_text_scrape_fun(wi06_text[8],                    ## Use our function to scrape page 8
                                            "Representative", 
                                            "Excused", 
                                            5) %>% 
  bind_cols(                                                                 ## Page 9 has the rest of the columns for the representatives on page 8 - we want to bind on these columns using bind_cols() 
    basic_text_scrape_fun(wi06_text[9],                                      ## Use our function to scrape page 9
                          "AB 441", 
                          "Conservation", 
                          1) %>% set_names(c( "AB 441", "AB 675", "AB 734",      
                                              "AB 778", "AB 850", "AA22 to AB 100, ASA1", "AA24 to AB 100, ASA1",
                                              "AA39 to AB 100, ASA1", "AA45 to AB 100, ASA1", "AJR 77"  
                          ))
  ) %>% 
  bind_rows(                                                                ## Pages 10 and 11 have the rows for the rest of the representatives - we want to bind on these rows using bind_rows() 
    bind_cols(                                                              ## Pages 10 and 11 have all the columns for the representatives on page 10 - we want to bind these columns together using bind_cols() 
      basic_text_scrape_fun(wi06_text[10],                                  ## Use our function to scrape page 10
                            "Representative", 
                            "Excused", 
                            5),
      basic_text_scrape_fun(wi06_text[11],                                  ## Use our function to scrape page 11
                            "AB 441", 
                            "Conservation", 
                            1) %>% set_names(c( "AB 441", "AB 675", "AB 734",      
                                           "AB 778", "AB 850", "AA22 to AB 100, ASA1", "AA24 to AB 100, ASA1",
                                           "AA39 to AB 100, ASA1", "AA45 to AB 100, ASA1", "AJR 77"  
                            ))
    )
  ) %>% 
  filter(!is.na(`AB 437`)) %>% ## Drop rows that have NAs
  janitor :: clean_names() %>%  
  rename(name = representative, sb_1 = special) %>%  
  mutate(senate=0, .before = 'name') %>% 
  pivot_longer(cols = names(wi_assembly_scores)[-c(1,2,3,4)],
               names_to = "bill",
               values_to = "vote") %>%
  mutate(
    tags = case_when(bill=="sb_1"~"democracy",
                     bill=="sb_459"~"energy",
                     bill=="sb_551"~"wetland, conservation",
                     bill=="ab_71"~"wildlife",
                     bill=="ab_277"~"air_quality, pollution, human_health",
                     bill=="ab_278"~"other",
                     bill=="ab_437"~"land_use, other",
                     bill=="ab_441"~"energy, human_health",
                     bill=="ab_675"~"land_use, other",
                     bill=="ab_734"~"water",
                     bill=="ab_778"~"human_health, pollution",
                     bill=="ab_850"~"wildlife",
                     bill=="aa22_to_ab_100_asa1"~"water",
                     bill=="aa24_to_ab_100_asa1"~"land_use",
                     bill=="aa39_to_ab_100_asa1"~"other",
                     bill=="aa45_to_ab_100_asa1"~"other",
                     bill=="ajr_77"~"other"
    )) %>% 
  calculate_and_return_tag_ratios(., '+')


###############scrape the names and final scores for Wisconsin senate###############

wi_senate_scores <- basic_text_scrape_fun(wi06_text[12], "Senator", "JOINT", 1) %>% 
  bind_cols(
    basic_text_scrape_fun(wi06_text[13], "AB 299", "JOINT", 1)
  ) %>% 
  filter(!is.na(`SB 459`)) %>% 
  janitor::clean_names() %>% 
  rename(name = senator, ab_850=w_compromise, ab100_sa6=sa6, ab100_sa11=sa11) %>% 
  mutate(senate = 1, .before='name')%>%
  pivot_longer(cols = names(wi_senate_scores)[-c(1,2,3,4)],
               names_to = "bill",
               values_to = "vote") %>%
  mutate(
    tags = case_when(bill=="sb_1"~"democracy",
                     bill=="sb_402"~"human_health, pollution",
                     bill=="sb_425"~"other",
                     bill=="sb_459"~"energy",
                     bill=="sb_467"~"water",
                     bill=="sb_551"~"wetland,conservation",
                     bill=="ab_26"~"wildlife, conservation",
                     bill=="ab_277"~"air_quality, pollution, human_health",
                     bill=="ab_299"~"land_use, water",
                     bill=="ab_437"~"land_use, other",
                     bill=="ab_441"~"energy, human_health",
                     bill=="ab_675"~"land_use, other",
                     bill=="ab_718"~"other",
                     bill=="ab_850"~"wildlife",
                     bill=="ab_1012"~"other",
                     bill=="ab100_sa6"~"other",
                     bill=="ab100_sa11"~"water, human_health, pollution, toxics, conservation, air",
                     bill=="sjr_63"~"conservation, natural_resource",
    )) %>% 
  calculate_and_return_tag_ratios(., '+')
####################Complete####################  
wi_06_score<-row_bind(wi_assembly_scores, wi_senate_scores)







