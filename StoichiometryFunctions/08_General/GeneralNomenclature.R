


# Resolution 08: General
GeneralNomenclature <- function(input_atomic_number1 = NULL, input_valence1 = NULL,
                            input_atomic_number2 = NULL, input_valence2 = NULL,
                            input_family = NULL,
                            input_internal_language = "en", input_external_language = NULL,
                            input_PeriodicTable = NULL,
                            input_Nomenclature = NULL) {
  
  
  
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
    
    
  
    if ("Oxide" == input_family) {
      
      # Oxide!
      general_nomenclature <- OxideNomenclature(input_atomic_number1 = input_atomic_number1,
                                                input_valence1 = input_valence1,
                                                input_internal_language = "en",
                                                input_external_language = input_external_language,
                                                input_PeriodicTable = input_PeriodicTable,
                                                input_Nomenclature = input_Nomenclature)
      
    } else
      if ("Hydroxide" == input_family) {
        
        # Hydroxide!
        general_nomenclature <- HydroxideEquation(input_atomic_number1 = input_atomic_number1,
                                                input_valence1 = input_valence1,
                                                input_internal_language = "en",
                                                input_external_language = input_external_language,
                                                input_PeriodicTable = input_PeriodicTable)
      } else
        if ("Oxacid" == input_family) {
          
          # Oxacid
          general_nomenclature <- OxacidEquation(input_atomic_number1 = input_atomic_number1,
                                               input_valence1 = input_valence1,
                                               input_internal_language = "en",
                                               input_external_language = input_external_language,
                                               input_PeriodicTable = input_PeriodicTable)
          
        } else
          if ("Hydride" == input_family) {
            
            # Hydride
            general_nomenclature <- HydrideEquation(input_atomic_number1 = input_atomic_number1,
                                                  input_valence1 = input_valence1,
                                                  input_internal_language = "en",
                                                  input_external_language = input_external_language,
                                                  input_PeriodicTable = input_PeriodicTable)
          } else
            if ("Hydracid" == input_family) {
              
              # Hydradic
              general_nomenclature <- HydracidEquation(input_atomic_number1 = input_atomic_number1,
                                                     input_valence1 = input_valence1,
                                                     input_internal_language = "en",
                                                     input_external_language = input_external_language,
                                                     input_PeriodicTable = input_PeriodicTable)
            } else
              if ("Oxosalt" == input_family) {
                
                # Oxosalt
                general_nomenclature <- OxosaltEquation(input_atomic_number1 = input_atomic_number1,
                                                      input_valence1 = input_valence1,
                                                      input_atomic_number2 = input_atomic_number2,
                                                      input_valence2 = input_valence2,
                                                      input_internal_language = "en",
                                                      input_external_language = input_external_language,
                                                      input_PeriodicTable = input_PeriodicTable)
              } else
                if ("Salt" == input_family) {
                  
                  # Salt
                  general_nomenclature <- SaltEquation(input_atomic_number1 = input_atomic_number1,
                                                     input_valence1 = input_valence1,
                                                     input_atomic_number2 = input_atomic_number2,
                                                     input_valence2 = input_valence2,
                                                     input_internal_language = "en",
                                                     input_external_language = input_external_language,
                                                     input_PeriodicTable = input_PeriodicTable)
                } 
    ###
  } # End Part 1: Structure of the "matrix_solution"
  ############################################################
  
  
  
  
  
  
  
  # Part 3: Return
  {
    ###
    
    
    return(general_nomenclature)
    
    
    ###  
  } # End Part 5: Return
  ############################################
  
  
  
  
  
  
  
  
} # End Function***





input_atomic_number1 <- 3 #Litio
input_valence1 <- 1
input_atomic_number2 <- 9 #Flúor
input_valence2 <- 1
input_internal_language <- "en"
input_external_language <- "es"
input_PeriodicTable <- PeriodicTable
input_Nomenclature <- Nomenclature



input_general_nomenclature <- GeneralNomenclature(input_atomic_number1 = input_atomic_number1,
                                         input_valence1 = input_valence1,
                                         input_atomic_number2 = input_atomic_number2,
                                         input_valence2 = input_valence2,
                                         input_family = "Oxide",
                                         input_internal_language = "en",
                                         input_external_language = input_external_language,
                                         input_PeriodicTable = input_PeriodicTable,
                                         input_Nomenclature = input_Nomenclature)


input_general_nomenclature