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
  set_names(c("district", "Lname", "2005_Score", "2002-2004_Ave_Score","G&F Electronic Licensing","Boat AnglerAnchoring",
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
  set_names(c("district", "Lname","2005_Score", "2002-2004_Ave_Score","G&F Electronic Licensing","Boat AnglerAnchoring",
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
#####CHANGG TO PIVOT TABLE ADDED TAGS
wy_house_vote <- rbind(assembly_vote_1,assembly_vote_2) %>% 
  pivot_longer(cols = matches(" |G&F Electronic Licensing"),
               names_to = "bill",
               values_to = "vote") %>% 
  mutate(                                                               
    tags = case_when(bill=="G&F Electronic Licensing"	~ "Wildlife, Other",
                     bill=="Boat AnglerAnchoring" ~	"Wildlife, Other",
                     bill=="Environmental Health Study"	~ "Other"	,
                     bill=="Fuel Efficient Vehicle Licensing"	~ "Energy",	
                     bill=="Establish Roads on Fed’l Land" ~ "Conservation",	
                     bill=="Allow Land Board to Fire Director" ~ "Other",	
                     bill=="Use Fishery Pool to Water Crops" ~ "Agriculture,	Conservation",
                     bill=="Fund State Land Sustainability"	~"Conservation, Agriculture",
                     bill=="Study State Land Sustainability" ~ "Conservation	Agriculture",
                     bill=="Penalties for Water Misappropriation"	~ "Conservation, Energy",
                     bill=="Wildlife Habitat Trust Fund" ~	"Conservation, Wildlife", 
                     bill=="Allow Instream Flow with Stored Water" ~ "Conservation, Water",
                     bill=="Split-Estate Surface Owner Rights" ~ "Energy",
                     bill=="Limit Oil & Gas on G&F Property"	~"Wildlife,	Energy",
                     bill=="Penalties for Oil & Gas Violations" ~	"Energy",	
                     bill=="Conservation Easements"	~ "Conservation"	
    ))

## Now you can make the legislation table very easily!
wy_05_legislation <- wy_house_vote %>% 
  distinct(state, senate, bill, tags)

#################Senate Vote Page 16&17###################

assembly_scores_16 <- wy_text[16] %>%           
  substr(.,                                
         str_locate(., "16 Aullman — R, Thayne"),    
         tail(unlist(gregexpr('\\+',.)), n=1)) %>%     ####find last word str_locate_all(wy_text[16], '\\+')[[1]][***]  
  str_replace_all(" {2,26}", "\\|") %>%
  str_replace_all("\\\n\n", "\\\n") %>%   
  str_replace_all("(?<=\\d) ", "\\|") %>%      
  read_delim(delim = "|", col_names = F, na = c("", "n/a")) %>% 
  set_names(c("district", "Lname","2005_Score", "2002-2004_Ave_Score","G&F Electronic Licensing","Boat AnglerAnchoring",
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

assembly_scores_17 <- wy_text[17] %>%          
  substr(.,                                
         str_locate_all(., "\\+")[2,][1,],   
         str_locate(., "R, Gillette")) %>%      
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

