#####set up
rm(list = ls())
library(pacman)
p_load(tidyverse, pdftools, rlist, zoo)
library(dplyr)
library(janitor)

####load data
setwd("~/Library/CloudStorage/Box-Box/Legislative Scorecards")
wi_senate <- read.csv("Data/clean/Wisconsin 2015-2016 Senate.csv") %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  rename(`2016_score` = x2016, senate=sanator) %>% 
  filter(senate != "")

wi_house<-read.csv("Data/clean/Wisconsin 2015-2016 House.csv") %>% 
  clean_names() %>% 
  rename(`2016_score` = x2016)%>% 
  filter(senate != "")


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
  result_df <- unique(data_frame[c(1,2,3,4,5)])
  
  #each tag score
  result_df[unique_tags] <- lapply(unique_tags, function(tag) {
    sapply(result_df$name, function(name) {
      each_Lname_each_tag(data_frame, name, vote_sign, tag) * 100
    })
  })
  
  return(result_df)
}


wi_senate_score<-wi_senate %>% 
  pivot_longer(cols = names(wi_senate)[-c(1,2,3,13, 14)],
               names_to = "bill",
               values_to = "vote") %>%
  mutate(
    tags = case_when(bill=="ab_582"~"pollution, industry, human_health, other",
                     bill=="sb_239"~"groundwater",
                     bill=="sb_459"~"water, natural resource, wetland, wildlife",
                     bill=="ab_384"~"energy, nuclear energy",
                     bill=="ab_603"~"shoreland, water",
                     bill=="sb_434"~"forest, wildlife",
                     bill=="ab_25"~"air, pollution, human_health",
                     bill=="ab_515"~"recycle, waste, education, other",
                     bill=="ab_563"~"pollution, wildlife, land, other"
    ))  %>% 
  calculate_and_return_tag_ratios(., 1)


wi_house_score<-wi_house %>% 
  pivot_longer(cols = names(wi_house)[-c(1,2,3,13, 14)],
               names_to = "bill",
               values_to = "vote") %>%
  mutate(
    tags = case_when(bill=="ab_582"~"pollution, industry, human_health, other",
                     bill=="ab_874"~"groundwater",
                     bill=="sb_459"~"water, natural resource, wetland, wildlife",
                     bill=="ab_603"~"shoreland, water",
                     bill=="ab_640"~"water, agriculture, wildlife, aquaculture",
                     bill=="sb_434"~"forest, wildlife",
                     bill=="ab_25"~"air, pollution, human_health",
                     bill=="ab_515"~"recycle, waste, education, other",
                     bill=="ab_563"~"pollution, wildlife, land, other"
    )) %>% 
  calculate_and_return_tag_ratios(., 1)

wi_senate_score$district <- as.integer(wi_senate_score$district)
wi_senate_score$senate <- as.integer(wi_senate_score$senate)
wi_house_score$district <- as.integer(wi_house_score$district)
wi_house_score$senate <- as.integer(wi_house_score$senate)

wi_15_16_score <- wi_house_score %>% bind_rows(wi_senate_score)



