


# Plot 08: General
GeneralPlot <- function(input_atomic_number1 = NULL, input_valence1 = NULL,
                            input_atomic_number2 = NULL, input_valence2 = NULL,
                            input_family = NULL,
                            input_roman = NULL,
                            input_internal_language = "en", input_external_language = NULL,
                            input_PeriodicTable = NULL,
                            input_step = NULL) {
  
  
  
  # Internal Settings
  {
    ###
    
    # Important Detail!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # Internally are using "en" (English) as internal language and options.
    # The input_optional_language is for select some correct options form each language
    # in the output.
    input_internal_language <- "en"
    if (is.null(input_external_language)) input_external_language <- "en"
    if (is.null(input_step)) input_step <- 1

    
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
  
  
  
  
  # Part 1: Plot Resolution
  {
    ###
    
  
    
    if ("Oxide" == input_family) {
      
        # Oxide!
        OxidePlot(input_atomic_number1 = input_atomic_number1,
                  input_valence1 = input_valence1,
                  input_internal_language = "en",
                  input_external_language = input_external_language,
                  input_roman = input_roman,
                  input_PeriodicTable = input_PeriodicTable,
                  input_step = input_step)
    } else
      if ("Hydroxide" == input_family) {
        
        # Hydroxide!
        HydroxidePlot(input_atomic_number1 = input_atomic_number1,
                      input_valence1 = input_valence1,
                      input_internal_language = "en",
                      input_external_language = input_external_language,
                      input_roman = input_roman,
                      input_PeriodicTable = input_PeriodicTable,
                      input_step = input_step)
      } else
        if ("Oxacid" == input_family) {
          
          # Oxacid
          OxacidPlot(input_atomic_number1 = input_atomic_number1,
                     input_valence1 = input_valence1,
                     input_internal_language = "en",
                     input_external_language = input_external_language,
                     input_roman = input_roman,
                     input_PeriodicTable = input_PeriodicTable,
                     input_step = input_step)
          
        } else
          if ("Hydride" == input_family) {
            
            # Hydride
            HydridePlot(input_atomic_number1 = input_atomic_number1,
                        input_valence1 = input_valence1,
                        input_internal_language = "en",
                        input_external_language = input_external_language,
                        input_roman = input_roman,
                        input_PeriodicTable = input_PeriodicTable,
                        input_step = input_step)
            
            
          } else
            if ("Hydracid" == input_family) {
              
              # Hydradic
              HydracidPlot(input_atomic_number1 = input_atomic_number1,
                           input_valence1 = input_valence1,
                           input_internal_language = "en",
                           input_external_language = input_external_language,
                           input_roman = input_roman,
                           input_PeriodicTable = input_PeriodicTable,
                           input_step = input_step)
            } else
              if ("Oxosalt" == input_family) {
                
                # Oxosalt
                OxosaltPlot(input_atomic_number1 = input_atomic_number1,
                           input_valence1 = input_valence1,
                           input_atomic_number2 = input_atomic_number2,
                           input_valence2 = input_valence2,
                           input_internal_language = "en",
                           input_external_language = input_external_language,
                           input_roman = input_roman,
                           input_PeriodicTable = input_PeriodicTable,
                           input_step = input_step)
              } else
                if ("Salt" == input_family) {
                  
                  # Salt
                  SaltPlot(input_atomic_number1 = input_atomic_number1,
                           input_valence1 = input_valence1,
                           input_atomic_number2 = input_atomic_number2,
                           input_valence2 = input_valence2,
                           input_internal_language = "en",
                           input_external_language = input_external_language,
                           input_roman = input_roman,
                           input_PeriodicTable = input_PeriodicTable,
                           input_step = input_step)
                } else
                  if(sum(my_family == input_family) == 0) {
                    
                    # Emergency!
                    plot(0,0, "GeneralPlot() Emergency!")
                  }
    ###
  } # End Plot Part 1
  ############################################################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} # End Function***




# 
# input_atomic_number1 <- 3 #Litio
# input_valence1 <- 1
# input_atomic_number2 <- 9 #Flúor
# input_valence2 <- 1
# input_internal_language <- "en"
# input_external_language <- "es"
# input_family <- "Oxide"
# input_step <- 8
# input_roman <- FALSE
# input_PeriodicTable <- PeriodicTable
# input_GeneralNomenclature01 <-  GeneralNomenclature01
# input_GeneralNomenclature02 <-  GeneralNomenclature02
# input_GeneralNomenclature03 <-  GeneralNomenclature03
# 
# 
#  GeneralPlot(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                          input_atomic_number2 = input_atomic_number2,
#                                          input_valence2 = input_valence2,
#                                          input_family = "Oxide",
#                                          input_internal_language = "en",
#                                          input_external_language = input_external_language,
#                                          input_roman = input_roman,
#                                          input_step = input_step,
#                                          input_PeriodicTable = input_PeriodicTable)
# 
# 
