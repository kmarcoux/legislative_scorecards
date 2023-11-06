########counts total bill with specific tag##############
count_bills_with_tag <- function(data_frame, specific_tag) {
  # Split the tags column and check for the specific tag
  count <- sum(sapply(strsplit(data_frame$tags, ",\\s*"), function(x) specific_tag %in% x))
  return(count)
}

each_Lname_each_tag<-function(data_frame, name, vote_sign, tag) {
  person_data <- data_frame[data_frame$Lname == name,]
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
#######calculate the ratio and transfer to score#######
calculate_and_return_tag_ratios <- function(data_frame, vote_sign) {
  # Get unique tags
  unique_tags <- unique(unlist(strsplit(data_frame$tags, ",\\s*")))
  
  # Create an empty data frame to store the results
  result_df <- data.frame(Lname = unique(data_frame$Lname))
  
  #each tag score
  result_df[unique_tags] <- lapply(unique_tags, function(tag) {
    sapply(result_df$Lname, function(Lname) {
      each_Lname_each_tag(data_frame, Lname, vote_sign, tag) * 100
    })
  })
  
  return(result_df)
}

#Round final score to 2 decimal points:
# calculate_tag_ratios <- function(data_frame, vote_sign) {
#   unique_tags <- unique(unlist(strsplit(data_frame$tags, ",\\s*")))
#   
#   result_df <- data.frame(Lname = unique(data_frame$Lname))
#   
#   result_df[unique_tags] <- lapply(unique_tags, function(tag) {
#     sapply(result_df$Lname, function(Lname) {
#       round(each_Lname_each_tag(data_frame, Lname, vote_sign, tag) * 100, 2)
#     })
#   })
#   
#   return(result_df)
# }

#seperate house/senate
tag_score_house_senate <-function(data_frame, vote_sign) {
  house_rep_score <-calculate_tag_ratios(data_frame[data_frame$senate==0, ], vote_sign)
  senator_score <-calculate_tag_ratios(data_frame[data_frame$senate==1, ], vote_sign)
  return(bind_rows(house_rep_score, senator_score))
}
#if there are same names in senate and house:
# tag_score_house_senate <-function(data_frame, vote_sign) {
#   house_rep_score <-calculate_tag_ratios(data_frame[data_frame$senate==0, ], vote_sign) %>% 
#     mutate(senate=0)
#   senator_score <-calculate_tag_ratios(data_frame[data_frame$senate==1, ], vote_sign) %>% 
#     mutate(senate=1)
#   return(bind_rows(house_rep_score, senator_score))
# }
# wy_06_score<-wy_06_info %>% right_join(wy_06_tag_score, by=c('senate','Lname'))

# Sample data frame refer to faye's wy_05_code
df <- wy_05_votes[1:31,]

# Usage of the function to create a new data frame with tag ratios
tag_ratios_df <- calculate_and_return_tag_ratios(df, vote_sign="+")
print(tag_ratios_df)




#####some other useful helper function#####
###count_bills_with_tags count the total number of bills with each tags (in legislation)
count_bills_with_tags <- function(data_frame) {
  tags <- unlist(strsplit(data_frame$tags, ",[ \\\t]"))
  tag_counts <- table(tags)
  tag_counts <- as.data.frame(tag_counts)
  colnames(tag_counts) <- c("Tag", "Count")
  tag_counts_dict <- as.list(setNames(tag_counts$Count, tag_counts$Tag))
  return(tag_counts_dict)
}














