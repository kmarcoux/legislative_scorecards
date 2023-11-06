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

################################################################################
## READ IN DATA AND MERGE
################################################################################

################### Wyoming 2006 ###################


wy_06_data <- pdf_data("Data/raw/Wyoming/WCV Scorecard06.pdf")
###########page12,13 House rep########
## Now select the right page and do some manipulation
wy_assembley_12 <- wy_06_data[[12]] %>%
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
  subset(., select = -`y`) %>% 
  unite("Lname", `1`:`2`, sep= " ", na.rm = TRUE) %>% 
  set_names(c('Lname', 'score_06', 'score_05', 'score_04', 'score_03', 'score_02',
              "Subsidies for Local Predator Control",
              "Game and Fish Electronic Licensing",
              "Limit Wildlife Trust Board (Intro.)",
              "State Land Rules Enforcement",
              "Clean up Leaking Landfills")) %>% 
  mutate(
    state = "Wyoming",
    senate = 0,
    party = substr(sapply(str_split(Lname, "\\—"), "[", 2), 2, 2),           ## Manipulate variables so they are in the format we want
    district_name = sapply(str_split(Lname, "\\, "), "[", 2),   ##sapply() use to apply a function, here is '[' function to seperate into two and we can choose 1 or two
    district=sapply(str_split(Lname, " "), "[", 1),
    Lname = sapply(str_split(Lname, " \\—"), "[", 1),
    Lname = sapply(str_split(Lname, " "), "[", 2),
    .before = 'Lname')
           
             
wy_assembley_13 <- wy_06_data[[13]] %>%
  mutate(y = round(y/15)) %>%                                 ## We round because other wise the rows arent grouped together correctly - they are too precise
  filter(y>=max(if_else(grepl("Landon", text), y, 0))) %>%     ## Filter to the start row that you want
  mutate(group = cumsum(na.fill(!lag(space), 0))) %>%         ## Find where the string is followed by a space, group those observations together so we get the text how it is supposed to show up
  dplyr::group_by(group) %>%
  dplyr::summarise(text = paste0(text, collapse = " "),              ## Now paste together the text that is followed by a space so the two (or more) words are in the same observation
                   x = round(first(x)/30),                           ## Round you x's like you did with the y's (if the text is left-aligned choose first(x), if it is centered aligned, choose mean(x), if it is right-aligned, choose last (x))
                   y = first(y)) %>%                                 ## Play around with what function of y to use
  arrange(x, y) %>%                                           ## Sort your data by x, y so that it shows up in a readable fashion when you pivot
  pivot_wider(id_cols = "y",                                  ## Pivot the data wider, the y values will tell you what should be in the same row together
              values_from = "text",                           ## The values of the table will come from the text column
              names_from = "x") %>%                           ## The columns of the table will come from the x column
  subset(., select = -`y`) %>% 
  unite("Lname", `1`:`2`, sep= " ", na.rm = TRUE) %>% 
  set_names(c('Lname', 'score_06', 'score_05', 'score_04', 'score_03', 'score_02',
              "Subsidies for Local Predator Control",
              "Game and Fish Electronic Licensing",
              "Limit Wildlife Trust Board (Intro.)",
              "State Land Rules Enforcement",
              "Clean up Leaking Landfills")) %>% 
  mutate(
    state = "Wyoming",
    senate = 0,
    party = substr(sapply(str_split(Lname, "\\—"), "[", 2), 2, 2),           ## Manipulate variables so they are in the format we want
    district_name = sapply(str_split(Lname, "\\, "), "[", 2),   ##sapply() use to apply a function, here is '[' function to seperate into two and we can choose 1 or two
    district=sapply(str_split(Lname, " "), "[", 1),
    Lname = sapply(str_split(Lname, " \\—"), "[", 1),
    Lname = sapply(str_split(Lname, " "), "[", 2),
    .before = 'Lname')


wy_house_vote <- rbind(wy_assembley_12,wy_assembley_13) %>% 
  pivot_longer(cols = matches(" |Subsidies for Local Predator Control"),
               names_to = "bill",
               values_to = "vote") %>% 
  mutate(                                                               
    tags = case_when(bill=="Subsidies for Local Predator Control"~"Agriculture, Wildlife",
                     bill=="Game and Fish Electronic Licensing"~"Wildlife",	
                     bill=="Limit Wildlife Trust Board (Intro.)"~"Conservation, Wildlife",
                     bill=="State Land Rules Enforcement"~"Conservation, Wildlife",
                     bill=="Clean up Leaking Landfills"~"Waste"	
    ))




###########page14,15 Senator#############
wy_assembley_14 <- wy_06_data[[14]] %>%
  mutate(y = round(y/15)) %>%                                 ## We round because other wise the rows arent grouped together correctly - they are too precise
  filter(y>=max(if_else(grepl("Anderson", text), y, 0))) %>%     ## Filter to the start row that you want
  mutate(group = cumsum(na.fill(!lag(space), 0))) %>%         ## Find where the string is followed by a space, group those observations together so we get the text how it is supposed to show up
  dplyr::group_by(group) %>%
  dplyr::summarise(text = paste0(text, collapse = " "),              ## Now paste together the text that is followed by a space so the two (or more) words are in the same observation
                   x = round(first(x)/35),                           ## Round you x's like you did with the y's (if the text is left-aligned choose first(x), if it is centered aligned, choose mean(x), if it is right-aligned, choose last (x))
                   y = first(y)) %>%                                 ## Play around with what function of y to use
  arrange(x, y) %>%                                           ## Sort your data by x, y so that it shows up in a readable fashion when you pivot
  pivot_wider(id_cols = "y",                                  ## Pivot the data wider, the y values will tell you what should be in the same row together
              values_from = "text",                           ## The values of the table will come from the text column
              names_from = "x") %>%                           ## The columns of the table will come from the x column
  subset(., select = -`y`) %>% 
  unite("info", `1`:`2`, sep= " ", na.rm = TRUE) %>% 
  set_names(c('info', 'score_06', 'score_05', 'score_04', 'score_03', 'score_02',
              "Subsidies for Local Predator Control",
              "Revise Control Subsidies",
              "Game and Fish Electronic Licensing")) %>% 
  mutate(
    state = "Wyoming",
    senate = 1,
    party = substr(sapply(str_split(info, "\\—"), "[", 2), 2, 2),           ## Manipulate variables so they are in the format we want
    district_name = sapply(str_split(info, "\\,"), "[", 2),   ##sapply() use to apply a function, here is '[' function to seperate into two and we can choose 1 or two
    district=sapply(str_split(info, " "), "[", 1),
    Lname = sapply(str_split(info, " "), "[", 2),
    .before = 'info') %>% 
  subset(., select = -info)



wy_assembley_15 <- wy_06_data[[15]] %>%
  mutate(y = round(y/15)) %>%                                 ## We round because other wise the rows arent grouped together correctly - they are too precise
  filter(y>=max(if_else(grepl("Anderson", text), y, 0))) %>%     ## Filter to the start row that you want
  mutate(group = cumsum(na.fill(!lag(space), 0))) %>%         ## Find where the string is followed by a space, group those observations together so we get the text how it is supposed to show up
  dplyr::group_by(group) %>%
  dplyr::summarise(text = paste0(text, collapse = " "),              ## Now paste together the text that is followed by a space so the two (or more) words are in the same observation
                   x = round(first(x)/35),                           ## Round you x's like you did with the y's (if the text is left-aligned choose first(x), if it is centered aligned, choose mean(x), if it is right-aligned, choose last (x))
                   y = first(y)) %>%                                 ## Play around with what function of y to use
  arrange(x, y) %>%                                           ## Sort your data by x, y so that it shows up in a readable fashion when you pivot
  pivot_wider(id_cols = "y",                                  ## Pivot the data wider, the y values will tell you what should be in the same row together
              values_from = "text",                           ## The values of the table will come from the text column
              names_from = "x") %>%                           ## The columns of the table will come from the x column
  subset(., select = -`y`) %>% 
  unite("Lname", `13`:`14`, sep= " ", na.rm = TRUE) %>% 
  set_names(c("Wildlife Trust Fund Investment Increase",
              'Sensitive Species Conservation',
              'Wildlife Trust Fund Investment Decrease',
              'State Land Rules Enforcement',
              'Clean up Leaking Landfills',
              'Regulation of Coalbed Methane H2O (Intro.)',
              'Temporary In-stream Flow Rights (Intro.)',
              'Local-level Fisheries Protection',
              "Lname")) %>% 
  mutate(
    Lname = sapply(str_split(Lname, " \\—"), "[", 1)
  )

wy_senate_vote<-inner_join(wy_assembley_14,wy_assembley_15,by='Lname') %>% 
  pivot_longer(cols = matches(" |Subsidies for Local Predator Control"),
               names_to = "bill",
               values_to = "vote") %>% 
  mutate(                                                               
    tags = case_when(bill=="Subsidies for Local Predator Control"~"Agriculture, Wildlife",	
                     bill=="Revise Control Subsidies"~"Agriculture, Wildlife",	
                     bill=="Game and Fish Electronic Licensing"~"Wildlife",		
                     bill=="Wildlife Trust Fund Investment Increase"~"Conservation, Wildlife",	
                     bill=="Sensitive Species Conservation"~"Conservation, Wildlife",	
                     bill=="Wildlife Trust Fund Investment Decrease"~"Conservation, Wildlife",	
                     bill=="State Land Rules Enforcement"~"Agriculture, Conservation",	
                     bill=="Clean up Leaking Landfills"~"Waste",		
                     bill=="Regulation of Coalbed Methane H2O (Intro.)"~"Energy, Waste, Conservation",
                     bill=="Temporary In-stream Flow Rights (Intro.)"~"Conservation, Wildlife, Water",
                     bill=="Local-level Fisheries Protection"~"Agriculture"		
    ))














###########Helper Functions Calculate Score############
count_bills_with_tag <- function(data_frame, specific_tag) {
  count <- sum(sapply(data_frame$tags, 
                      function(tag_list) specific_tag %in% unlist(strsplit(tag_list, ",\\s*"))))
  return(count)
}


each_Lname_each_tag<-function(data_frame, name, vote_sign, tag) {
  person_data <- data_frame[data_frame$Lname == name,]
  total_bills_tag<-count_bills_with_tag(person_data, tag)
  person_vote <- person_data[person_data$vote == vote_sign,]
  if (nrow(person_vote)==0){
    return(0)
    }
  else {
    person_vote_bills_tag <-count_bills_with_tag(person_vote, tag)
    return (person_vote_bills_tag /total_bills_tag)
  }
}


calculate_tag_ratios <- function(data_frame, vote_sign) {
  unique_tags <- unique(unlist(strsplit(data_frame$tags, ",\\s*")))
  
  result_df <- data.frame(Lname = unique(data_frame$Lname))
  
  result_df[unique_tags] <- lapply(unique_tags, function(tag) {
    sapply(result_df$Lname, function(Lname) {
      each_Lname_each_tag(data_frame, Lname, vote_sign, tag) * 100
    })
  })
  
  return(result_df)
}

tag_score_house_senate <-function(data_frame, vote_sign) {
  house_rep_score <-calculate_tag_ratios(data_frame[data_frame$senate==0, ], vote_sign) %>% 
    mutate(senate=0)
  senator_score <-calculate_tag_ratios(data_frame[data_frame$senate==1, ], vote_sign) %>% 
    mutate(senate=1)
  return(bind_rows(house_rep_score, senator_score))
}




###########Analysis###########
wy_06_votes <- rbind(wy_house_vote,wy_senate_vote )
wy_06_legislation<-wy_06_votes %>% distinct(state, senate, bill, tags)
wy_06_tag_score<- tag_score_house_senate(wy_06_votes, vote_sign="+")
wy_06_info<-rbind(wy_assembley_12[,c("state", "senate", "party", "district_name","district", 
                                     "Lname", 'score_06', 'score_05', 'score_04', 'score_03', 'score_02')],
                  wy_assembley_13[,c("state", "senate", "party", "district_name","district", 
                                     "Lname", 'score_06', 'score_05', 'score_04', 'score_03', 'score_02')], 
                  wy_assembley_14[,c("state", "senate", "party", "district_name","district", 
                                     "Lname", 'score_06', 'score_05', 'score_04', 'score_03', 'score_02')])

wy_06_score<-wy_06_info %>% right_join(wy_06_tag_score, by=c('senate','Lname'))












