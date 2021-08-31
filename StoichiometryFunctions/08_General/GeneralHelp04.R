


# LaTeX 08: General
GeneralHelp04 <- function(input_atomic_number1 = NULL, 
                          input_valence1 = NULL,
                          input_atomic_number2 = NULL, 
                          input_valence2 = NULL,
                          input_family = NULL,
                          input_internal_language = "en", 
                          input_external_language = NULL,
                          input_PeriodicTable = NULL,
                          input_Helper = NULL) {
  
  
  

  # Part 1: Resolution
  {
    ###
    
    
   
    
    if ("Oxide" == input_family) {
      
      # Oxide!
      general_help04 <- OxideHelp04(input_atomic_number1 = input_atomic_number1,
                                       input_valence1 = input_valence1,
                                       input_internal_language = "en",
                                       input_external_language = input_external_language,
                                       input_PeriodicTable = input_PeriodicTable,
                                       input_family = input_family,
                                       input_Helper = input_Helper)
      
      
    } else
      if ("Hydroxide" == input_family) {
        
        # Hydroxide!
        general_help04 <- HydroxideHelp04(input_atomic_number1 = input_atomic_number1,
                                          input_valence1 = input_valence1,
                                          input_internal_language = "en",
                                          input_external_language = input_external_language,
                                          input_PeriodicTable = input_PeriodicTable,
                                          input_family = input_family,
                                          input_Helper = input_Helper)
      } else
        if ("Oxacid" == input_family) {
          
          # Oxacid
          general_help04 <- OxacidHelp04(input_atomic_number1 = input_atomic_number1,
                                        input_valence1 = input_valence1,
                                        input_internal_language = "en",
                                        input_external_language = input_external_language,
                                        input_PeriodicTable = input_PeriodicTable,
                                        input_family = input_family,
                                        input_Helper = input_Helper)
          
        } else
          if ("Hydride" == input_family) {
            
            # Hydride
            general_help04 <- HydrideHelp04(input_atomic_number1 = input_atomic_number1,
                                           input_valence1 = input_valence1,
                                           input_internal_language = "en",
                                           input_external_language = input_external_language,
                                           input_PeriodicTable = input_PeriodicTable,
                                           input_family = input_family,
                                           input_Helper = input_Helper)
          } else
            if ("Hydracid" == input_family) {
              
              # Hydradic
              general_help04 <- HydracidHelp04(input_atomic_number1 = input_atomic_number1,
                                              input_valence1 = input_valence1,
                                              input_internal_language = "en",
                                              input_external_language = input_external_language,
                                              input_PeriodicTable = input_PeriodicTable,
                                              input_family = input_family,
                                              input_Helper = input_Helper)
            } else
              if ("Oxosalt" == input_family) {
                
                # Oxosalt
                general_help04 <- OxosaltHelp04(input_atomic_number1 = input_atomic_number1,
                                               input_valence1 = input_valence1,
                                               input_atomic_number2 = input_atomic_number2,
                                               input_valence2 = input_valence2,
                                               input_internal_language = "en",
                                               input_external_language = input_external_language,
                                               input_PeriodicTable = input_PeriodicTable,
                                               input_family = input_family,
                                               input_Helper = input_Helper)
              } else
                if ("Salt" == input_family) {
                  
                  # Salt
                  general_help04 <- SaltHelp04(input_atomic_number1 = input_atomic_number1,
                                              input_valence1 = input_valence1,
                                              input_atomic_number2 = input_atomic_number2,
                                              input_valence2 = input_valence2,
                                              input_internal_language = "en",
                                              input_external_language = input_external_language,
                                              input_PeriodicTable = input_PeriodicTable,
                                              input_family = input_family,
                                              input_Helper = input_Helper)
                } 
    ###
  } # End Part 1: Structure of the "matrix_solution"
  ############################################################
  
  
  
  
  
  
  
  # Part 3: Return
  {
    ###
    
    
    return(general_help04)
    
    
    ###  
  } # End Part 5: Return
  ############################################
  
  
  
  
  
  
  
  
} # End Function***



# 
# #
# input_atomic_number1 <- 3 #Litio
# input_valence1 <- 1
# input_atomic_number2 <- 9 #Flúor
# input_valence2 <- 1
# input_internal_language <- "en"
# input_family = "Oxide"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_Helper <- PageHelperLevel
# 
# 
# input_general_help04 <- GeneralHelp04(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                          input_atomic_number2 = input_atomic_number2,
#                                          input_valence2 = input_valence2,
#                                          input_family = input_family,
#                                          input_internal_language = "en",
#                                          input_external_language = input_external_language,
#                                          input_PeriodicTable = input_PeriodicTable,
#                                          input_Helper = input_Helper)
# 
#  input_general_help04