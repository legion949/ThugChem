


# Resolution 08: General
GeneralNomenclature <- function(input_atomic_number1 = NULL, input_valence1 = NULL,
                            input_atomic_number2 = NULL, input_valence2 = NULL,
                            input_family = NULL,
                            input_internal_language = NULL,
                            input_external_language = NULL,
                            input_PeriodicTable = NULL,
                            input_Nomenclature = NULL) {
  
  
  
  # Internal Settings
  {
    ###
    
   
    
    
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
    
    
  
    if ("Oxide" == input_family) {
      
      # Oxide!
      general_nomenclature <- OxideNomenclature(input_atomic_number1 = input_atomic_number1,
                                                input_valence1 = input_valence1,
                                                input_family = input_family,
                                                input_internal_language = input_internal_language,
                                                input_external_language = input_external_language,
                                                input_PeriodicTable = input_PeriodicTable,
                                                input_Nomenclature = input_Nomenclature)
      
    } else
      if ("Hydroxide" == input_family) {
        
        # Hydroxide!
        general_nomenclature <- HydroxideNomenclature(input_atomic_number1 = input_atomic_number1,
                                                      input_valence1 = input_valence1,
                                                      input_family = "Hydroxide",
                                                      input_internal_language = input_internal_language,
                                                      input_external_language = input_external_language,
                                                      input_PeriodicTable = input_PeriodicTable,
                                                      input_Nomenclature = input_Nomenclature)
      } else
        if ("Oxacid" == input_family) {
          
          # Oxacid
          general_nomenclature <- OxacidNomenclature(input_atomic_number1 = input_atomic_number1,
                                               input_valence1 = input_valence1,
                                               input_family = "Oxacid",
                                               input_internal_language = input_internal_language,
                                               input_external_language = input_external_language,
                                               input_PeriodicTable = input_PeriodicTable,
                                               input_Nomenclature = input_Nomenclature)
          
        } else
          if ("Hydride" == input_family) {
            
            # Hydride
            general_nomenclature <- HydrideNomenclature(input_atomic_number1 = input_atomic_number1,
                                                        input_valence1 = input_valence1,
                                                        input_internal_language = input_internal_language,
                                                        input_family = "Hydride",
                                                        input_external_language = input_external_language,
                                                        input_PeriodicTable = input_PeriodicTable,
                                                        input_Nomenclature = input_Nomenclature)
          } else
          if ("Hydracid" == input_family) {
            
            # Hydradic
            general_nomenclature <- HydracidNomenclature(input_atomic_number1 = input_atomic_number1,
                                                        input_valence1 = input_valence1,
                                                        input_internal_language = input_internal_language,
                                                        input_family = "Hydracid",
                                                        input_external_language = input_external_language,
                                                        input_PeriodicTable = input_PeriodicTable,
                                                        input_Nomenclature = input_Nomenclature)
          } else
         
            
              if ("Oxosalt" == input_family) {
                
                # Oxosalt
                general_nomenclature <- OxosaltNomenclature(input_atomic_number1 = input_atomic_number1,
                                                      input_valence1 = input_valence1,
                                                      input_atomic_number2 = input_atomic_number2,
                                                      input_valence2 = input_valence2,
                                                      input_family = "Oxosalt",
                                                      input_internal_language = input_internal_language,
                                                      input_external_language = input_external_language,
                                                      input_PeriodicTable = input_PeriodicTable,
                                                      input_Nomenclature = input_Nomenclature)
              } else
                if ("Salt" == input_family) {
                  
                  # Salt
                  general_nomenclature <- SaltNomenclature(input_atomic_number1 = input_atomic_number1,
                                                     input_valence1 = input_valence1,
                                                     input_atomic_number2 = input_atomic_number2,
                                                     input_valence2 = input_valence2,
                                                     input_family = "Salt",
                                                     input_internal_language = input_internal_language,
                                                     input_external_language = input_external_language,
                                                     input_PeriodicTable = input_PeriodicTable,
                                                     input_Nomenclature = input_Nomenclature)
                } 
    ###
  } # End Part 1: Structure of the "matrix_solution"
  ############################################################
  
  
  
  
  # Part 2: Changing for languages
  {
  ###
    
   
    
    
  ###  
  } # End Part 2
  #########################################
  
  
  # Part 3: Return
  {
    ###
    
    
    return(general_nomenclature)
    
    
    ###  
  } # End Part 3: Return
  ############################################
  
  
  
  
  
  
  
  
} # End Function***





# input_atomic_number1 <- 6 #Litio
# input_valence1 <- 2
# input_atomic_number2 <- 9 #Flúor
# input_valence2 <- 1
# input_family = "Oxide"
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_Nomenclature <- Nomenclature
# 
# 
# 
# input_general_nomenclature <- GeneralNomenclature(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                          input_atomic_number2 = input_atomic_number2,
#                                          input_valence2 = input_valence2,
#                                          input_family = "Oxide",
#                                          input_internal_language = "en",
#                                          input_external_language = input_external_language,
#                                          input_PeriodicTable = input_PeriodicTable,
#                                          input_Nomenclature = input_Nomenclature)
# 
# 
# input_general_nomenclature