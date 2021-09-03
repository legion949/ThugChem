


# Regular text - Adding "\n".
RegularText <- function(input_text = NULL, max_n = 6) {
  

  count_in <- str_count(input_text, "\n ")
  count_new_added <- max_n - count_in
  exit_text <- c()
  my_new_added <- c()
  
  for (k in 1:length(input_text)) my_new_added[k] <- paste0(rep("\n", count_new_added[k]), collapse = "")
  
  
  for (k in 1:length(input_text)) exit_text[k] <- paste0(input_text[k], my_new_added[k], collapse="")
  
  
  # Return
  return(exit_text)
}


