

# LaTeX of Hydracid
HydracidLaTeX <-function(input_atomic_number1 = NULL, input_valence1 = NULL, 
                         input_internal_language = "en", input_external_language = NULL,
                         input_PeriodicTable = NULL) {
  
 
 
  latex_hydracid_resolution   <-         HydrideLaTeX(input_atomic_number1 = input_atomic_number1,
                                                         input_valence1 = input_valence1,
                                                         input_internal_language = "en",
                                                         input_external_language = input_external_language,
                                                         input_PeriodicTable = input_PeriodicTable)

  # Changes...
  my_option <- latex_hydracid_resolution

  for (k in 1:length(my_option)) {
  
      final_col <- ncol(my_option[[k]])
      selected_element <- my_option[[k]][,final_col]
      
      my_dt <- grepl("*", selected_element, fixed = TRUE)
      
      st1 <- strsplit(my_option[[k]][my_dt, final_col], "[*]") 
      st2 <- st1
      for (aver in 1:length(st2)) st2[[aver]] <- st2[[aver]][c(2,1)] 
      
      new_vector <- c()
      for (aver in 1:length(st2)) new_vector[aver] <- paste0(st2[[aver]][1], "*", st2[[aver]][2])
      
      my_option[[k]][my_dt, final_col] <- new_vector
    }
    
  
  # New exit
  latex_hydracid_resolution <- my_option
  

 
  
  
  # Part 3: Return
  {
    ###
    
    
    return( latex_hydracid_resolution)
    
    
    ###  
  } # End Part 3: Return
  ############################################
} # Fin HydracidLaTeX()




# 
# input_atomic_number1 <- 6
# input_valence1 <- 2
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_GeneralNomenclature01 <-  GeneralNomenclature01
# input_GeneralNomenclature02 <-  GeneralNomenclature02
# input_GeneralNomenclature03 <-  GeneralNomenclature03
# 
# 
# input_hydracid_resolution <- HydracidEquation(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                          input_internal_language = "en",
#                                          input_external_language = input_external_language,
#                                          input_PeriodicTable = input_PeriodicTable)
# 
# 
# input_hydracid_resolution
# 
# 
# 
# latex_hydracid_resolution   <-         HydracidLaTeX(input_atomic_number1 = input_atomic_number1,
#                                         input_valence1 = input_valence1,
#                                         input_internal_language = "en",
#                                         input_external_language = input_external_language,
#                                         input_PeriodicTable = input_PeriodicTable)
# 
# 
# 
# latex_hydracid_resolution