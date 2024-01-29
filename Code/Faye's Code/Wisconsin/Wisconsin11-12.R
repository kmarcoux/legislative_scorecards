#####set up
rm(list = ls())
library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)
library(dplyr)

####load data
setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")
wi_text <- pdf_text("Data/raw/Wisconsin/WLCV-Scorecard-2012-FNL-LowRes.pdf")

################### helper function################### 
basic_text_scrape_fun <- function(page, start_text, end_text, end_diff){      ## We can define our own function to speed things up
  
  page %>% 
    substr(str_locate(., start_text), str_locate(., end_text)-end_diff) %>%     ## Subset to the right rows
    str_replace_all(" {2,}", "\\|") %>%                                       ## Replace double spaces with pipes
    str_replace_all("\\\n\\|", "\\\n") %>%                                    ## replace pipes after line breaks with just line breaks
    str_replace_all("Score\\\nScience", "Score\\|Science") %>%
    str_replace_all("Representative\\\nWetlands", "Representative\\|Wetlands") %>%
    str_replace_all("Senator\\\nAir", "Senator\\|Air") %>%
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
  rename(ss_sb_1=Science, ss_sb_10=Wetlands, ss_ab_8=`Public Health`, 
         sb_75=Wildlife, sb_81=`Clean Energy`, sb_326=Water) %>% 
  bind_cols(                                                                ## Page 9 has the rest of the columns for the representatives on page 8 - we want to bind on these columns using bind_cols() 
    basic_text_scrape_fun(wi_text[11],                                      ## Use our function to scrape page 9
                          "Representative", 
                          "Note", 
                          1) %>% 
      rename(sb_368=Wetlands, sb_407=Toxics, sb_441=Wildlife, ab_165=Phosphorus,
             ab_177=Water, ab_303=`Land Use`, ab_395=Land, ab_426=Mining)) %>%
  bind_rows(                                                                ## Pages 10 and 11 have the rows for the rest of the representatives - we want to bind on these rows using bind_rows() 
    bind_cols(                                                              ## Pages 10 and 11 have all the columns for the representatives on page 10 - we want to bind these columns together using bind_cols() 
      basic_text_scrape_fun(wi_text[12],                                  ## Use our function to scrape page 10
                            "Representative", 
                            "Legislator", 
                            4) %>% 
        rename(ss_sb_1=Science, ss_sb_10=Wetlands, ss_ab_8=`Public Health`, 
               sb_75=Wildlife, sb_81=`Clean Energy`, sb_326=Water),
      basic_text_scrape_fun(wi_text[13],                                  ## Use our function to scrape page 11
                            "Representative", 
                            "Note", 
                            1)%>% 
        rename(sb_368=Wetlands, sb_407=Toxics, sb_441=Wildlife, ab_165=Phosphorus,
               ab_177=Water, ab_303=`Land Use`, ab_395=Land, ab_426=Mining))
  ) %>% 
  select(.,-`Representative...10`) %>%  ##Drop repetitive columns.
  janitor :: clean_names() %>%  
  rename(name = `representative_1`) %>%  
  mutate(senate=0, .before = 'name')

wi_assembly_scores<- wi_assembly_scores%>% 
  pivot_longer(cols = names(wi_assembly_scores)[-c(1,2,3,4)],
               names_to = "bill",
               values_to = "vote") %>%
  mutate(
    tags = case_when(bill=="ss_sb_1"~"other, human_health, natural_resource",
                     bill=="ss_sb_10"~"wetland, wildlife",
                     bill=="ss_ab_8"~"human_health, natural_resource, other",
                     bill=="sb_75"~"wildlife, conservation",
                     bill=="sb_81"~"energy",
                     bill=="sb_326"~"water",
                     bill=="sb_368"~"wetland, wildlife",
                     bill=="sb_407"~"pollution, toxics, water, human_health",
                     bill=="sb_441"~"wildlife",
                     bill=="ab_165"~"water, pollution",
                     bill=="ab_177"~"water",
                     bill=="ab_303"~"land_use, conservation",
                     bill=="ab_395"~"land, wildlife, natural_resource",
                     bill=="ab_426"~"mining, land, natural_resource, human_health"
    )) 
wi_assembly_scores<-calculate_and_return_tag_ratios(wi_assembly_scores, '+')


###############scrape the names and final scores for Wisconsin senate###############

wi_senate_score <- basic_text_scrape_fun(wi_text[14], "Senator", "Legislator", 3) %>% 
  set_names(c('name', 'district', 'score', 'ss_sb_1', 'ss_sb_10', 'ss_ab_8',
              "sb_81")) %>% 
  bind_cols(
    basic_text_scrape_fun(wi_text[15], "Senator", "Note", 1) %>% 
      set_names(c('name', 'sb_111', 'sb_138', 'sb_326', 'sb_368', 'sb_441',
                  "ab_426"))  
  ) %>% 
  select(., -`name...8`) %>% 
  rename(name = `name...1`) %>% 
  mutate(senate = 1, .before='name')

wi_senate_score<-wi_senate_score %>% 
  pivot_longer(cols = names(wi_senate_score)[-c(1,2,3,4)],
               names_to = "bill",
               values_to = "vote") %>%
  mutate(
    tags = case_when(bill=="ss_sb_1"~"other, human_health, natural_resource",
                     bill=="ss_sb_10"~"wetland, wildlife",
                     bill=="ss_ab_8"~"human_health, natural_resource, other",
                     bill=="sb_81"~"energy",
                     bill=="sb_111"~"air, pollution",
                     bill=="sb_138"~"air, pollution, agriculture",
                     bill=="sb_326"~"water",
                     bill=="sb_368"~"wetland, wildlife",
                     bill=="sb_441"~"wildlife",
                     bill=="ab_426"~"mining, land, natural_resource, human_health"
    )) 
wi_senate_score<-calculate_and_return_tag_ratios(wi_senate_score, '+') 

wi_10_score <- wi_assembly_scores %>% bind_rows(wi_senate_score)













