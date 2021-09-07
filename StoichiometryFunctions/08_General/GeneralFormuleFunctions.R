




# LaTeX 08: General
ChemicalFunction <- function(input_atomic_number1 = NULL, input_valence1 = NULL,
                             input_atomic_number2 = NULL, input_valence2 = NULL,
                             input_family = NULL,
                             input_internal_language = "en", input_external_language = NULL,
                             input_PeriodicTable = NULL){
  
  
  input_table <- GeneralLaTeX(input_atomic_number1 = input_atomic_number1,
                              input_valence1 = input_valence1,
                              input_atomic_number2 = input_atomic_number2,
                              input_valence2 = input_valence2,
                              input_family = input_family,
                              input_internal_language = input_internal_language,
                              input_external_language = input_external_language,
                              input_PeriodicTable = input_PeriodicTable)[[1]]
  
  
  my_names <- colnames(input_table)
  dt_coef3 <- my_names == "Coef3"
  orden_names <- c(1:length(my_names))
  my_col <- orden_names[dt_coef3] + 1
  my_row <- nrow(input_table)
  
  chemical_function_latex <- input_table[my_row, my_col]
  
  
  my_vector <- chemical_function_latex

  
  for (k in 1:length(my_vector)){     
    
    my_vector[k] <- gsub("[]]", "", my_vector[k])
    my_vector[k] <- gsub("[[]", "_", my_vector[k])
    my_vector[k] <- gsub("_1", "", my_vector[k])
    my_vector[k] <- gsub("[*]", "", my_vector[k])
    my_vector[k] <- gsub("1", "", my_vector[k])
    my_vector[k] <- gsub("[']", "", my_vector[k])
    
    my_vector[k] <- paste0("$$", my_vector[k], "$$", collapse ="")
    
  }  
  
  my_exit <- my_vector
  names(my_exit) <- c("Chemical Formule")
  
  
  # Return
  return(my_exit)
  
  
}


# 
# input_atomic_number1 <- 3 #Litio
# input_valence1 <- 1
# input_atomic_number2 <- 9 #Flúor
# input_valence2 <- 1
# input_family = "Oxosalt"
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# 
# 
# 
# input_general_resolution <- ChemicalFunction(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                          input_atomic_number2 = input_atomic_number2,
#                                          input_valence2 = input_valence2,
#                                          input_family = "Oxosalt",
#                                          input_internal_language = "en",
#                                          input_external_language = input_external_language,
#                                          input_PeriodicTable = input_PeriodicTable)
# 
# 
# input_general_resolution