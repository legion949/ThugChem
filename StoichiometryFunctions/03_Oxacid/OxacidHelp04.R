

# Oxacid Helper
OxacidHelp04 <-function(input_atomic_number1 = NULL, input_valence1 = NULL, 
                      input_internal_language = "en",
                      input_external_language = NULL,
                      input_PeriodicTable = NULL,
                      input_family = NULL,
                      input_Helper = NULL) {
  
 
  # Internal Settings
  {
    ###
    
    # Important Detail!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # Internally are using "en" (English) as internal language and options.
    # The input_optional_language is for select some correct options form each language
    # in the output.
    input_internal_language <- "en"
    if (is.null(input_external_language)) input_external_language <- "en"
    
    ###  
  } # End Internal Settings
  ##########################################################################
  
  
  
  # Part 0: Necessary Participants
  {
    ###
    
    # We gather all the information we need to
    # carry out the exercise
    
    # Principal objects
    atomic_number1 <- as.numeric(as.character(input_atomic_number1))
    valence1 <-  as.numeric(as.character(input_valence1))
    internal_language <- as.character(input_internal_language)
    
    # Recruited values
    symbol1 <- as.character(input_PeriodicTable[[internal_language]][atomic_number1, 2])
    name1  <- as.character(input_PeriodicTable[[internal_language]][atomic_number1, 3])
    type1 <- as.character(input_PeriodicTable[[internal_language]][atomic_number1, 7])
    state1 <- as.character(input_PeriodicTable[[internal_language]][atomic_number1, 9])
    
    # Internal Control 
    internal_control <- InternalControl_Oxacid(input_atomic_number1 = input_atomic_number1,
                                              input_valence1 = input_valence1,
                                              input_internal_language = input_internal_language,
                                              input_external_language = input_external_language,
                                              input_PeriodicTable = input_PeriodicTable)
    
    
    input_oxacid_resolution <- OxacidEquation(input_atomic_number1 = input_atomic_number1,
                                             input_valence1 = input_valence1,
                                             input_internal_language = "en",
                                             input_external_language = input_external_language,
                                             input_PeriodicTable = input_PeriodicTable)
    
    ###  
  } # End Part 0: Necessary Participants
  ####################################################

  
  # Part 1: Isolate 
  {
  ###
    
    the_helper04 <- input_Helper[[input_family]][[input_external_language]]
    selected_columns <- c((ncol(the_helper04)-1), ncol(the_helper04))
    the_helper04 <- the_helper04[,selected_columns]
    
    # Options
    my_YES <- the_helper04[,1]
    my_NO <- the_helper04[,2]
    
    # General substitution
    my_YES <- gsub("_element1_", symbol1, my_YES)
    my_YES <- gsub("_valence1_", valence1, my_YES)
    
    my_NO <- gsub("_element1_", symbol1, my_NO)
    my_NO <- gsub("_valence1_", valence1, my_NO)
    
    # Default Values is NO
    new_help04 <- my_NO
    
  ###  
  } # End Part1
  ############################
  
  
  # Part 2: Selection
  {
  ###
    
    # Step 1: Default value
   
    # Step 2: Default value
    
    # Step 3: Is valence... Impar o par?
    {
      ###
      # Default value is Par.
      
      is_par <- (valence1/2) == (valence1%/%2)
    
      if (!is_par){ 
        
        new_help04[3] <- my_YES[3] 

      }
      
      remove(is_par)
      ###  
    } # End Step 3
    #########################################
    
    
    
    # Step 4: New subindex for Oxygen in Oxacid?
    {
      ###
      my_3 <- as.numeric(as.character(as.vector(input_oxacid_resolution[3, c("Sub7")])))
      my_4 <- as.numeric(as.character(as.vector(input_oxacid_resolution[4, c("Sub7")])))
      
      if (!identical(my_3, my_4)){ 

        new_help04[4] <- my_YES[4] 
        

      }
      
      # In "yes" or "no", both need a change
      new_help04[4] <- gsub("_sub7_", my_4, new_help04[4])
      
      
      remove(my_3, my_4)
      ###  
    } # End Step 4
    #######################################
    
    
    # Step 5: New coefficient 3?
    {
      ###
      my_4 <- as.numeric(as.character(as.vector(input_oxacid_resolution[4, c("Coef3")])))
      my_5 <- as.numeric(as.character(as.vector(input_oxacid_resolution[5, c("Coef3")])))
      
      if (!identical(my_4, my_5)){ 
        
        new_help04[5] <- my_YES[5] 
        
        
      }
      
      # In "yes" or "no", both need a change
      new_help04[5] <- gsub("_coef3_", my_5, new_help04[5])
      
      remove(my_4, my_5)
      ###  
    } # End Step 5
    #######################################
    
    # Step 6: Default value
    
    # Step 7: Default value
    
    # Step 8: Coefficients simplificated?
    {
      ###
      my_7 <- as.numeric(as.character(as.vector(input_oxacid_resolution[7, c("Coef1", "Coef2", "Coef3")])))
      my_8 <- as.numeric(as.character(as.vector(input_oxacid_resolution[8, c("Coef1", "Coef2", "Coef3")])))
      
      if (!identical(my_7, my_8)){ 
        library(numbers)
        mcd_7 <- mGCD(my_7)
        new_help04[8] <- my_YES[8] 
        new_help04[8] <- gsub("_mcd_coef_", mcd_7, new_help04[8])
        
        remove(mcd_7)
      }
      
      remove(my_7, my_8)
      ###  
    } # End Step 8
    #######################################
    
    
    
 
    
  ###  
  } # End Part2
  ##################################
  
  
  
  
      # Part 3: Return
      {
      ###
        return(new_help04)
        
      ###  
      } # End Part 3: Exit
      #######################################
      
  
                 

}  



# # 
# input_atomic_number1 <- 6
# input_valence1 <- 2
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_family = "Oxacid"
# input_Helper = PageHelperLevel
# 
# 
# 
# input_oxacid_help04 <- OxacidHelp04(input_atomic_number1 = input_atomic_number1,
#                                         input_valence1 = input_valence1,
#                                         input_internal_language = "en",
#                                         input_external_language = input_external_language,
#                                         input_PeriodicTable = input_PeriodicTable,
#                                         input_family = input_family,
#                                         input_Helper = input_Helper)
# 
# 
# input_oxacid_help04