

# Hydride Helper
HydrideHelp04 <-function(input_atomic_number1 = NULL, input_valence1 = NULL, 
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
    internal_control <- InternalControl_Hydride(input_atomic_number1 = input_atomic_number1,
                                              input_valence1 = input_valence1,
                                              input_internal_language = input_internal_language,
                                              input_external_language = input_external_language,
                                              input_PeriodicTable = input_PeriodicTable)
    
    
    input_hydride_resolution <- HydrideEquation(input_atomic_number1 = input_atomic_number1,
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
    
    # Step 3: Default value
    
    # Step 4: Default value
    
    # Step 5: New coefficient 2?
    {
      ###
      my_4 <- as.numeric(as.character(as.vector(input_hydride_resolution[4, c("Coef2")])))
      my_5 <- as.numeric(as.character(as.vector(input_hydride_resolution[5, c("Coef2")])))
      
      if (!identical(my_4, my_5)){ 
        
        new_help04[5] <- my_YES[5] 
        
        
      }
      
      # In "yes" or "no", both need a change
      new_help04[5] <- gsub("_coef2_", my_5, new_help04[5])
      
      remove(my_4, my_5)
      ###  
    } # End Step 5
    #######################################
    
    
    # Step 6: New coefficient 1?
    {
      ###
      my_5 <- as.numeric(as.character(as.vector(input_hydride_resolution[5, c("Coef1")])))
      my_6 <- as.numeric(as.character(as.vector(input_hydride_resolution[6, c("Coef1")])))
      
      if (!identical(my_5, my_6)){ 
        
        new_help04[6] <- my_YES[6] 
        
        
      }
      
      # In "yes" or "no", both need a change
      new_help04[6] <- gsub("_coef1_", my_6, new_help04[6])
      
      remove(my_5, my_6)
      ###  
    } # End Step 6
    #######################################
    
    

     
    # Step 7: Coefficients simplificated?
    {
      ###
      my_6 <- as.numeric(as.character(as.vector(input_hydride_resolution[6, c("Coef1", "Coef2", "Coef3")])))
      my_7 <- as.numeric(as.character(as.vector(input_hydride_resolution[7, c("Coef1", "Coef2", "Coef3")])))
      
      if (!identical(my_6, my_7)){ 
        library(numbers)
        mcd_6 <- mGCD(my_6)
        new_help04[7] <- my_YES[7] 
        new_help04[7] <- gsub("_mcd_coef_", mcd_6, new_help04[7])
        
        remove(mcd_6)
      }
      
      remove(my_6, my_7)
      ###  
    } # End Step 7
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
# input_atomic_number1 <- 6
# input_valence1 <- 2
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_family = "Hydride"
# input_Helper = PageHelperLevel
#
#
# 
# input_hydride_help04 <- HydrideHelp04(input_atomic_number1 = input_atomic_number1,
#                                         input_valence1 = input_valence1,
#                                         input_internal_language = "en",
#                                         input_external_language = input_external_language,
#                                         input_PeriodicTable = input_PeriodicTable,
#                                         input_family = input_family,
#                                         input_Helper = input_Helper)
# 
# 
# input_hydride_help04