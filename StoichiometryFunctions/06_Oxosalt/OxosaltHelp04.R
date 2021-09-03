

# Oxosalt Helper
OxosaltHelp04 <-function(input_atomic_number1 = NULL, 
                         input_valence1 = NULL, 
                         input_atomic_number2 = NULL, 
                         input_valence2 = NULL, 
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
    
    # Principal objects - First Element
    atomic_number1 <- as.numeric(as.character(input_atomic_number1))
    valence1 <-  as.numeric(as.character(input_valence1))
    
    
    # Recruited values - First Element
    symbol1 <- as.character(input_PeriodicTable[[input_internal_language]][atomic_number1, 2])
    name1  <- as.character(input_PeriodicTable[[input_internal_language]][atomic_number1, 3])
    type1 <- as.character(input_PeriodicTable[[input_internal_language]][atomic_number1, 7])
    state1 <- as.character(input_PeriodicTable[[input_internal_language]][atomic_number1, 9])
    if (type1 == "Metalloid") type1 <- "Non-metal"
    
    # Principal objects - Second Element
    atomic_number2 <- as.numeric(as.character(input_atomic_number2))
    valence2 <-  as.numeric(as.character(input_valence2))
    
    
    # Recruited values - Second Element
    symbol2 <- as.character(input_PeriodicTable[[input_internal_language]][atomic_number2, 2])
    name2  <- as.character(input_PeriodicTable[[input_internal_language]][atomic_number2, 3])
    type2 <- as.character(input_PeriodicTable[[input_internal_language]][atomic_number2, 7])
    state2 <- as.character(input_PeriodicTable[[input_internal_language]][atomic_number2, 9])
    if (type2 == "Metalloid") type2 <- "Non-metal"
    
    
    
    # Internal Control 
    internal_control <- InternalControl_Oxosalt(input_atomic_number1 = input_atomic_number1,
                                                input_valence1 = input_valence1,
                                                input_atomic_number2 = input_atomic_number2,
                                                input_valence2 = input_valence2,
                                                input_internal_language = input_internal_language,
                                                input_external_language = input_external_language,
                                                input_PeriodicTable = input_PeriodicTable)
    
    
    
    input_oxosalt_resolution <- OxosaltEquation(input_atomic_number1 = input_atomic_number1,
                                                input_valence1 = input_valence1,
                                                input_atomic_number2 = input_atomic_number2,
                                                input_valence2 = input_valence2,
                                                input_internal_language = input_internal_language,
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
    my_YES <- gsub("_element2_", symbol2, my_YES)
    my_YES <- gsub("_valence2_", valence2, my_YES)
    
    my_NO <- gsub("_element1_", symbol1, my_NO)
    my_NO <- gsub("_valence1_", valence1, my_NO)
    my_NO <- gsub("_element2_", symbol2, my_NO)
    my_NO <- gsub("_valence2_", valence2, my_NO)
    
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
    
    # Step 3: New subindex for Oxosalt?
    {
      ###
      my_2 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[2, c("Sub6", "Sub7")])))
      my_3 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[3, c("Sub6", "Sub7")])))
      
      if (!identical(my_2, my_3)){ 
        
        new_help04[3] <- my_YES[3] 

      }

      new_help04[3] <- gsub("_sub6_", my_3[1], new_help04[3])
      new_help04[3] <- gsub("_sub7_", my_3[2], new_help04[3])      
      remove(my_2, my_3)
      ###  
    } # End Step 3
    #######################################
    
    
    # Step 4: Subindex simplificated?
    {
      ###
      my_3 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[3, c("Sub6", "Sub7")])))
      my_4 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[4, c("Sub6", "Sub7")])))
      
      if (!identical(my_3, my_4)){ 
        library(numbers)
        mcd_3 <- mGCD(my_3)
        new_help04[4] <- my_YES[4] 
        new_help04[4] <- gsub("_mcd_index_", mcd_3, new_help04[4])
        
        remove(mcd_3)
      }
      
      remove(my_3, my_4)
      ###  
    } # End Step 4
    #######################################
    
    
    # Step 5: New coefficient 1?
    {
      ###
      my_4 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[4, c("Coef1")])))
      my_5 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[5, c("Coef1")])))
      
      if (!identical(my_4, my_5)){ 
        
        new_help04[5] <- my_YES[5] 
        new_help04[5] <- gsub("_coef1_", my_5, new_help04[5])
        
      }
      
      remove(my_4, my_5)
      ###  
    } # End Step 5
    #######################################
    
    
    # Step 6: New coefficient 2?
    {
      ###
      my_5 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[5, c("Coef2")])))
      my_6 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[6, c("Coef2")])))
      
      if (!identical(my_5, my_6)){ 
        
        new_help04[6] <- my_YES[6] 
        new_help04[6] <- gsub("_coef2_", my_6, new_help04[6])
        
      }
      
      remove(my_5, my_6)
      ###  
    } # End Step 6
    #######################################
    
    
    # Step 7: New coefficient 4?
    {
      ###
      my_6 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[6, c("Coef4")])))
      my_7 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[7, c("Coef4")])))
      
      if (!identical(my_6, my_7)){ 
        
        new_help04[7] <- my_YES[7] 
        new_help04[7] <- gsub("_coef4_", my_7, new_help04[7])
        
      }
      
      remove(my_6, my_7)
      ###  
    } # End Step 7
    #######################################

    
    # Step 8: Default value
    
  
    # Step 9: Coefficients simplificated?
    {
      ###
      my_8 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[8, c("Coef1", "Coef2", "Coef3", "Coef4")])))
      my_9 <- as.numeric(as.character(as.vector(input_oxosalt_resolution[9, c("Coef1", "Coef2", "Coef3", "Coef4")])))
      
      if (!identical(my_8, my_9)){ 
        library(numbers)
        mcd_8 <- mGCD(my_8)
        new_help04[9] <- my_YES[9] 
        new_help04[9] <- gsub("_mcd_coef_", mcd_8, new_help04[9])
        
        remove(mcd_8)
      }
      
      remove(my_8, my_9)
      ###  
    } # End Step 9
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


# 
# 
# input_atomic_number1 <- 26
# input_valence1 <- 2
# input_atomic_number2 <- 6
# input_valence2 <- 2
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_family = "Oxosalt"
# input_Helper = PageHelperLevel
# 
# 
# 
# input_oxosalt_help04 <- OxosaltHelp04(input_atomic_number1 = input_atomic_number1,
#                                         input_valence1 = input_valence1,
#                                         input_atomic_number2 = input_atomic_number2,
#                                         input_valence2 = input_valence2,
#                                         input_internal_language = "en",
#                                         input_external_language = input_external_language,
#                                         input_PeriodicTable = input_PeriodicTable,
#                                         input_family = input_family,
#                                         input_Helper = input_Helper)
# 
# 
# input_oxosalt_help04