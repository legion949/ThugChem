


# Resolution 08: General
GeneralEquation <- function(input_atomic_number1 = NULL, input_valence1 = NULL,
                            input_atomic_number2 = NULL, input_valence2 = NULL,
                            input_family = NULL,
                            input_internal_language = "en", input_external_language = NULL,
                            input_PeriodicTable = NULL) {
  
  
  
  # Internal Settings
  {
    ###
    
    # Important Detail!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # Internally are using "en" (English) as internal language and options.
    # The input_optional_language is for select some correct options form each language
    # in the output.
    input_internal_language <- "en"
    if (is.null(input_external_language)) input_external_language <- "en"
    
    
    # Chemestry Family
    my_family <- c("Oxide", "Hydroxide", "Oxacid",
                   "Hydride", "Hydracid", "Oxosalt",
                   "Salt")
    
    if(is.null(input_family)) cat("GeneralEquation() said: 'I need a input family!\n")
    
    ###  
  } # End Internal Settings
  ##########################################################################
  
  
  

  # Part 0: Necessary Participants
  {
    ###
    
    
    # Internal Language
    internal_language <- as.character(input_internal_language)
    
   
    
    
    ###  
  } # End Part 0: Necessary Participants
  ####################################################
    
    
    
      
      # Part 1: Resolution
      {
        ###
        
        
        # Emergency Exit
        {
          matrix_emergency_exit <- matrix("General Emergency Exit", 7, 11)
          colnames(matrix_emergency_exit) <- rep("Danger", ncol(matrix_emergency_exit))
        } 
        
        
      if ("Oxide" == input_family) {
        
        # Oxide!
        general_resolution <- OxideEquation(input_atomic_number1 = input_atomic_number1,
                                                input_valence1 = input_valence1,
                                                input_internal_language = "en",
                                                input_external_language = input_external_language,
                                                input_PeriodicTable = input_PeriodicTable)
      } else
        if ("Hydroxide" == input_family) {
          
          # Hydroxide!
           general_resolution <- HydroxideEquation(input_atomic_number1 = input_atomic_number1,
                                                           input_valence1 = input_valence1,
                                                           input_internal_language = "en",
                                                           input_external_language = input_external_language,
                                                           input_PeriodicTable = input_PeriodicTable)
        } else
          if ("Oxacid" == input_family) {
            
            # Oxacid
            general_resolution <- OxacidEquation(input_atomic_number1 = input_atomic_number1,
                            input_valence1 = input_valence1,
                            input_internal_language = "en",
                            input_external_language = input_external_language,
                            input_PeriodicTable = input_PeriodicTable)
             
          } else
            if ("Hydride" == input_family) {
              
              # Hydride
              general_resolution <- HydrideEquation(input_atomic_number1 = input_atomic_number1,
                                                       input_valence1 = input_valence1,
                                                       input_internal_language = "en",
                                                       input_external_language = input_external_language,
                                                       input_PeriodicTable = input_PeriodicTable)
            } else
              if ("Hydracid" == input_family) {
                
                # Hydradic
                general_resolution <- HydracidEquation(input_atomic_number1 = input_atomic_number1,
                                                         input_valence1 = input_valence1,
                                                         input_internal_language = "en",
                                                         input_external_language = input_external_language,
                                                         input_PeriodicTable = input_PeriodicTable)
              } else
                if ("Oxosalt" == input_family) {
                  
                  # Oxosalt
                  general_resolution <- OxosaltEquation(input_atomic_number1 = input_atomic_number1,
                                                             input_valence1 = input_valence1,
                                                             input_atomic_number2 = input_atomic_number2,
                                                             input_valence2 = input_valence2,
                                                             input_internal_language = "en",
                                                             input_external_language = input_external_language,
                                                             input_PeriodicTable = input_PeriodicTable)
                } else
                  if ("Salt" == input_family) {
                    
                    # Salt
                    general_resolution <- SaltEquation(input_atomic_number1 = input_atomic_number1,
                                                         input_valence1 = input_valence1,
                                                         input_atomic_number2 = input_atomic_number2,
                                                         input_valence2 = input_valence2,
                                                         input_internal_language = "en",
                                                         input_external_language = input_external_language,
                                                         input_PeriodicTable = input_PeriodicTable)
                  } else
                    if(sum(my_family == input_family) == 0) {
                      
                      # Emergency!
                      general_resolution <- matrix_emergency_exit
                    }
        ###
      } # End Part 1: Structure of the "matrix_solution"
      ############################################################
      
      
      
   
   
    
  
      # Part 3: Return
      {
        ###
        
        
        return(general_resolution)
        
        
        ###  
      } # End Part 5: Return
      ############################################
      
      
  
    
    
  
  
  
} # End Function***





# input_atomic_number1 <- 3 #Litio
# input_valence1 <- 1
# input_atomic_number2 <- 9 #Flúor
# input_valence2 <- 1
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_GeneralNomenclature01 <-  GeneralNomenclature01
# input_GeneralNomenclature02 <-  GeneralNomenclature02
# input_GeneralNomenclature03 <-  GeneralNomenclature03
# 
# 
# input_general_resolution <- GeneralEquation(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                          input_atomic_number2 = input_atomic_number2,
#                                          input_valence2 = input_valence2,
#                                          input_family = "Oxide",
#                                          input_internal_language = "en",
#                                          input_external_language = input_external_language,
#                                          input_PeriodicTable = input_PeriodicTable)
# 
# 
# input_general_resolution