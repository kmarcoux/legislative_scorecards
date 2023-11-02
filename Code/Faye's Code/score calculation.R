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
  person_vote_bills_tag <-count_bills_with_tag(person_vote, tag)
  return (person_vote_bills_tag /total_bills_tag)
}
#######calculate the ratio and transfer to score#######
calculate_and_return_tag_ratios <- function(data_frame, vote_sign) {
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














