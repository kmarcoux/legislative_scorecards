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

################### Wyoming 2005 ###################

wy_text <- pdf_text("Data/raw/Wyoming/WCV Scorecard05 Online Version.pdf")
###########page12 Half house rep########
assembly_scores_12 <- wy_text[12] %>%           ## Selected the page you want to scrape
  substr(.,                                ## I select only the text that I want to convert to a table
         str_locate(., " 3               Alden — R, Wheatland"),    ###Substr is inclusive the bound##start location
         tail(unlist(gregexpr('x',.)), n=1)) %>%       ##end at find the last letter 'x'
  str_replace_all(" {2,26}", "\\|") %>%      ## Replace double space to 26 double space with a pipe--26 from unlist(gregexpr('\\+', assembly_scores))[1]-unlist(gregexpr('9', assembly_scores))[1]
  str_replace_all("\\\n\n", "\\\n") %>%   ## This replaces the double spaces following a new line - replace those again
  str_replace_all("(?<=\\d) ", "\\|") %>%      ##seperate district number with person's name
  read_delim(delim = "|", col_names = F, na = c("", "n/a")) %>% ##col_names=F -->don't make the first row as column name f-false
  set_names(c("district", "Lname", "Score_05", "Ave_Score_02to04","G&F Electronic Licensing","Boat AnglerAnchoring",
              "Environmental Health Study", "Fuel Efficient Vehicle Licensing", "Establish Roads on Fed’l Land",
               "Allow Land Board to Fire Director", "Use Fishery Pool to Water Crops")) %>% 
  mutate(
    state = "Wyoming",
    senate = 0,
    party = substr(sapply(str_split(Lname, "\\—"), "[", 2), 2, 2),           ## Manipulate variables so they are in the format we want
    district_name = sapply(str_split(Lname, "\\,"), "[", 2),   ##sapply() use to apply a function, here is '[' function to seperate into two and we can choose 1 or two
    Lname = sapply(str_split(Lname, "\\—"), "[", 1),.before = 'district'
    )

########page13 Half house rep#################
assembly_scores_13 <- wy_text[13] %>%           ## Selected the page you want to scrape
  substr(.,                                ## I select only the text that I want to convert to a table
         str_locate(., "Easements\n")+nchar('Easements\n'),    ###Substr is inclusive the bound##start location
         str_locate(., "D, Jackson")) %>%       ##end at find the last letter 'x'
  str_replace_all(" {2,26}", "\\|") %>%      ## Replace double space to 26 double space with a pipe--26 from unlist(gregexpr('\\+', assembly_scores))[1]-unlist(gregexpr('9', assembly_scores))[1]
  str_replace_all("\\\n\n", "\\\n") %>%   ## This replaces the double spaces following a new line - replace those again
  str_replace_all("(?<=\\d) ", "\\|") %>%      ##seperate district number with person's name
  read_delim(delim = "|", col_names = F, na = c("", "n/a")) %>% ##col_names=F -->don't make the first row as column name f-false
  select(-c(X1)) %>% 
  set_names(c("Fund State Land Sustainability","Study State Land Sustainability", 
              "Penalties for Water Misappropriation", "Wildlife Habitat Trust Fund",
              "Allow Instream Flow with Stored Water", "Split-Estate Surface Owner Rights", 
              "Limit Oil & Gas on G&F Property", "Penalties for Oil & Gas Violations", "Conservation Easements",'Lname'))%>% 
  mutate(
    Lname = sapply(str_split(Lname, "\\—"), "[", 1)
  )

assembly_vote_1<-full_join(assembly_scores_12,assembly_scores_13,by='Lname')

############page 14&15 2nd half house rep########
assembly_scores_14 <- wy_text[14] %>%           
  substr(.,                                
         str_locate(., "30 Landon — R, Sheridan"),    
         tail(unlist(gregexpr('\\+',.)), n=1)) %>%       
  str_replace_all(" {2,26}", "\\|") %>%
  str_replace_all("\\\n\n", "\\\n") %>%   
  str_replace_all("(?<=\\d) ", "\\|") %>%      
  read_delim(delim = "|", col_names = F, na = c("", "n/a")) %>% 
  set_names(c("district", "Lname","Score_05", "Ave_Score_02to04","G&F Electronic Licensing","Boat AnglerAnchoring",
              "Environmental Health Study", "Fuel Efficient Vehicle Licensing", "Establish Roads on Fed’l Land",
              "Allow Land Board to Fire Director", "Use Fishery Pool to Water Crops")) %>% 
    mutate(
    state = "Wyoming",
    senate = 0,
    party = substr(sapply(str_split(Lname, "\\—"), "[", 2), 2, 2),  
    district_name = sapply(str_split(Lname, "\\,"), "[", 2),  
    Lname = sapply(str_split(Lname, "\\—"), "[", 1), 
    .before = 'district'
  )

assembly_scores_15 <- wy_text[15] %>%          
  substr(.,                                
         str_locate(., "Easements\n")+nchar('Easements\n'),   
         str_locate(., "R, Cheyenne")) %>%      
  str_replace_all(" {2,26}", "\\|") %>%   
  str_replace_all("\\\n\n", "\\\n") %>%  
  str_replace_all("(?<=\\d) ", "\\|") %>%  
  read_delim(delim = "|", col_names = F, na = c("", "n/a")) %>% 
  select(-c(X1)) %>% 
  set_names(c("Fund State Land Sustainability","Study State Land Sustainability", 
              "Penalties for Water Misappropriation", "Wildlife Habitat Trust Fund",
              "Allow Instream Flow with Stored Water", "Split-Estate Surface Owner Rights", 
              "Limit Oil & Gas on G&F Property", "Penalties for Oil & Gas Violations", "Conservation Easements",'Lname'))%>% 
  mutate(
    Lname = sapply(str_split(Lname, "\\—"), "[", 1)
  )

assembly_vote_2<-full_join(assembly_scores_14,assembly_scores_15,by='Lname')

#############add wy_house_vote_05_2 to wy_house_vote_05##########
#############CHANGG TO PIVOT TABLE ADDED TAGS#############
wy_house_vote <- rbind(assembly_vote_1,assembly_vote_2) %>% 
  pivot_longer(cols = matches(" |G&F Electronic Licensing"),
               names_to = "bill",
               values_to = "vote") %>% 
  mutate(                                                               
    tags = case_when(bill=="G&F Electronic Licensing"~"Wildlife, Other",
                     bill=="Boat AnglerAnchoring"~"Wildlife, Other",
                     bill=="Environmental Health Study"~"Other",
                     bill=="Fuel Efficient Vehicle Licensing"~"Energy",	
                     bill=="Establish Roads on Fed’l Land"~"Conservation",	
                     bill=="Allow Land Board to Fire Director"~"Other",	
                     bill=="Use Fishery Pool to Water Crops"~"Agriculture, Conservation",
                     bill=="Fund State Land Sustainability"~"Conservation, Agriculture",
                     bill=="Study State Land Sustainability"~"Conservation,	Agriculture",
                     bill=="Penalties for Water Misappropriation"~"Conservation, Energy",
                     bill=="Wildlife Habitat Trust Fund"~"Conservation, Wildlife", 
                     bill=="Allow Instream Flow with Stored Water"~"Conservation, Water",
                     bill=="Split-Estate Surface Owner Rights"~"Energy",
                     bill=="Limit Oil & Gas on G&F Property"~"Wildlife, Energy",
                     bill=="Penalties for Oil & Gas Violations"~"Energy",	
                     bill=="Conservation Easements"~"Conservation"	
    ))

## Now you can make the legislation table very easily!
#wy_05_legislation <- wy_house_vote %>% 
 # distinct(state, senate, bill, tags)

#################Senate Vote Page 16&17###################

assembly_scores_16 <- wy_text[16] %>%           
  substr(.,                                
         str_locate(., "2                Anderson — R, Glenrock"),    
         tail(unlist(gregexpr('\\+',.)), n=1)) %>%     ####find last word str_locate_all(wy_text[16], '\\+')[[1]][***]  
  str_replace_all(" {2,26}", "\\|") %>%
  str_replace_all("\\\n\n", "\\\n") %>%   
  str_replace_all("(?<=\\d) ", "\\|") %>%      
  read_delim(delim = "|", col_names = F, na = c("", "n/a")) %>% 
  set_names(c("district", "Lname","Score_05", "Ave_Score_02to04",
              "G&F Electronic Licensing",
              "Environmental Health Study",
              "Establish Roads on Fed’l Land",
              "Allow Land Board to Fire Director",
              "Fund State Land Sustainability",
              "Study State Land Sustainability",
              "Penalties for Water Misappropriation")) %>% 
  mutate(
    state = "Wyoming",
    senate = 1,
    party = substr(sapply(str_split(Lname, "\\—"), "[", 2), 2, 2),  
    district_name = sapply(str_split(Lname, "\\,"), "[", 2),  
    Lname = sapply(str_split(Lname, "\\—"), "[", 1), 
    .before = 'district'
  )

assembly_scores_17 <- wy_text[17] %>%          
  substr(.,                                
         str_locate_all(., "\\+")[[1]][2],   
         str_locate(., "R, Gillette")) %>%      
  str_replace_all(" {2,26}", "\\|") %>% 
  str_replace_all("\\\n\n\\|", "\\\n") %>%
  str_replace_all('Casper\\\n\\|', "Casper\\\n") %>% 
  read_delim(delim = "|", col_names = F, na = c("", "n/a")) %>% 
  #select(-c(X1)) %>% 
  set_names(c("G&F Alternative Funding Package",
              "Wildlife Habitat Trust Fund",
              "No Property Purchase",
              "Full Funding of $30 M",
              "Allow Instream Flow with Stored Water",
              "Split-Estate Surface Owner Rights",
              "Limit Oil & Gas on G&F Property",
              "Penalties for Oil & Gas Violations",
             "Local Control for Light Pollution",
              "Conservation Easements",'Lname'))%>% 
  mutate(
    Lname = sapply(str_split(Lname, "\\—"), "[", 1)
  )

assembly_vote_3<-full_join(assembly_scores_16,assembly_scores_17,by='Lname')

wy_senate_vote <- assembly_vote_3 %>% 
  pivot_longer(cols = matches(" |G&F Electronic Licensing"),
               names_to = "bill",
               values_to = "vote") %>% 
  mutate(                                                               
    tags = case_when(bill=="G&F Electronic Licensing"~"Wildlife, Other",
                     bill=="Environmental Health Study"~"Other",
                     bill=="Establish Roads on Fed’l Land"~"Conservation",	
                     bill=="Allow Land Board to Fire Director"	~ "Other",	
                     bill=="Fund State Land Sustainability"	~ "Conservation, Agriculture",
                     bill=="Study State Land Sustainability"	~ "Conservation, Agriculture",
                     bill=="Penalties for Water Misappropriation"	~ "Conservation, Energy",
                     bill=="G&F Alternative Funding Package"	~ "Wildlife", 	
                     bill=="Wildlife Habitat Trust Fund"	~ "Conservation,	Wildlife",
                     bill=="No Property Purchase"	~ "Conservaion, Agriculture",
                     bill=="Full Funding of $30 M"	~ "Conservation",
                     bill=="Allow Instream Flow with Stored Water"	~ "Conservation, Water",
                     bill=="Split-Estate Surface Owner Rights"	~ "Energy",	
                     bill=="Limit Oil & Gas on G&F Property"	~ "Wildlife, Energy",
                     bill=="Penalties for Oil & Gas Violations"	~ "Energy",
                     bill=="Local Control for Light Pollution"~"Conservation",	
                     bill=="Conservation Easements"~"Conservation"	
    ))


############helper function################
#tag <-c("Agriculture", "Energy", "Climate", "Conservation", "Wildlife", "Waste","Water", "Other")
########counts total bill with specific tag
count_bills_with_tag <- function(data_frame, specific_tag) {
  # Split the tags column and check for the specific tag
  count <- sum(sapply(strsplit(data_frame$tags, ",\\s*"), function(x) specific_tag %in% x))
  return(count)
}
each_Lname_each_tag<-function(data_frame, name, vote_sign, tag) {
  person_data <- data_frame[data_frame$Lname == name,]
  total_bills_tag<-count_bills_with_tag(person_data, tag)
  person_vote <- person_data[person_data$vote == vote_sign,]
  person_vote_bills_tag <-count_bills_with_tag(person_vote, tag)
  return (person_vote_bills_tag /total_bills_tag)
}
#######calculate the ratio and transfer to score
calculate_tag_ratios <- function(data_frame, vote_sign) {
  # Get unique tags
  unique_tags <- unique(unlist(strsplit(data_frame$tags, ",\\s*")))
  
  # Create an empty data frame to store the results
  result_df <- data.frame(Lname = unique(data_frame$Lname))
  for (tag in unique_tags) {
    # Calculate the ratios for each person and the tag
    total_bills_tag<-count_bills_with_tag(data_frame, tag)
    tag_ratios <- sapply(result_df$Lname, function(Lname) {
      return (each_Lname_each_tag(data_frame,name=Lname, vote_sign, tag))
    })
    
    # Add a column for the current tag
    result_df[tag] <- tag_ratios*100 
  }
  
  return(result_df)
}

######do house and senate seperately and rbind together
tag_score_house_senate <-function(data_frame, vote_sign) {
  house_rep_score <-calculate_tag_ratios(data_frame[data_frame$senate==0, ], vote_sign)
  senator_score <-calculate_tag_ratios(data_frame[data_frame$senate==1, ], vote_sign)
  return(bind_rows(house_rep_score, senator_score))
}



#############################Analysis###############################
wy_05_votes <- rbind(wy_house_vote,wy_senate_vote )
wy_05_legislation<-wy_05_votes %>% distinct(state, senate, bill, tags)
# wy_05_score<-rbind(assembly_vote_1 %>% 
#                      select(state, senate, party, district_name, district, 
#                             Lname, Score_05, Ave_Score_02to04),
#                     assembly_vote_2 %>% 
#                      select(state, senate, party, district_name, district, 
#                             Lname, Score_05, Ave_Score_02to04),
#                     assembly_vote_3 %>% 
#                      select(state, senate, party, district_name,district, 
#                            Lname, Score_05, Ave_Score_02to04))
wy_05_tag_score<-wy_05_votes %>% tag_score_house_senate(., vote_sign="+")
  
  
wy_05_info<-rbind(assembly_vote_1[,c("state", "senate", "party", "district_name","district", 
                                     "Lname", "Score_05", "Ave_Score_02to04")],
                  assembly_vote_2[,c("state", "senate", "party", "district_name","district", 
                                     "Lname", "Score_05", "Ave_Score_02to04")], 
                  assembly_vote_3[,c("state", "senate", "party", "district_name","district", 
                                     "Lname", "Score_05", "Ave_Score_02to04")])

wy_05_score<-merge(wy_05_info,wy_05_tag_score, by='Lname')





