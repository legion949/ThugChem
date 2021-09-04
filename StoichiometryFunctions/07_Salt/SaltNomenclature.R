



SaltNomenclature <- function(input_atomic_number1 = NULL, 
                                input_valence1 = NULL,
                                input_atomic_number2 = NULL, 
                                input_valence2 = NULL,
                                input_family = "Oxosalt",
                                input_internal_language = "en", 
                                input_external_language = NULL,
                                input_PeriodicTable = NULL,
                                input_Nomenclature = NULL) {
  
  
  
  # Internal Settings
  {
    ###
    # Internal language specifications by default and optional if null
    input_internal_language <- "en"
    if (is.null(input_external_language)) input_external_language <- input_internal_language
    # If is null the input_family
    #  if (is.null(input_family)) input_family <- "Oxide"
    ###
  } # End Internal Settings
  ##########################################################################
  
  
  # Part 0: Necessary Participants
  {
    ###
    # Element Values
    element_values1 <- Internal_ElementValues(input_atomic_number1 = input_atomic_number1,
                                              input_valence1 = input_valence1,
                                              input_internal_language = input_internal_language,
                                              input_external_language = input_external_language,
                                              input_PeriodicTable = input_PeriodicTable)
    
    element_values2 <- Internal_ElementValues(input_atomic_number1 = input_atomic_number2,
                                              input_valence1 = input_valence2,
                                              input_internal_language = input_internal_language,
                                              input_external_language = input_external_language,
                                              input_PeriodicTable = input_PeriodicTable)
    
    
    # Internal Control
    internal_control <- InternalControl_Oxosalt(input_atomic_number1 = input_atomic_number1,
                                                input_valence1 = input_valence1,
                                                input_atomic_number2 = input_atomic_number2,
                                                input_valence2 = input_valence2,
                                                input_internal_language = input_internal_language,
                                                input_external_language = input_external_language,
                                                input_PeriodicTable = input_PeriodicTable)
    # Salt Resolution
    input_salt_resolution <- SaltEquation(input_atomic_number1 = input_atomic_number1,
                                                input_valence1 = input_valence1,
                                                input_atomic_number2 = input_atomic_number2,
                                                input_valence2 = input_valence2,
                                                input_internal_language = "en",
                                                input_external_language = input_external_language,
                                                input_PeriodicTable = input_PeriodicTable)
    # LaTeX resolution
    input_salt_latex <- SaltLaTeX(input_atomic_number1 = input_atomic_number1,
                                        input_valence1 = input_valence1,
                                        input_atomic_number2 = input_atomic_number2,
                                        input_valence2 = input_valence2,
                                        input_internal_language = "en",
                                        input_external_language = input_external_language,
                                        input_PeriodicTable = input_PeriodicTable)
    
    input_latex <- input_salt_latex[[2]]
    
    # Chemestry Formule
    chemestry_formule_resolution <- input_salt_resolution[nrow(input_latex), c(13:16)]
    chemestry_formule_latex <- input_latex[nrow(input_latex), 8]
    
    amount1 <- as.numeric(as.character(chemestry_formule_resolution[1,2]))
    amount2 <- as.numeric(as.character(chemestry_formule_resolution[1,4]))
    
    ###
  } # Fin Part 0
  ##############################################
  
  
  # Part 1: Other Necessary Items
  {
    ###
    # Roman numerals
    romans <- c("I", "II", "III", "IV", "V", "VI", "VII", "")
    prefixes <- Nomenclature$Prefixes[[input_external_language]]
    medium_part_classic <- Nomenclature$General01[[input_external_language]]
    
    # This position is only for Salt
    my_pos <- 7
    
    ###
  } # Part 1
  ############################################
  
  
  
  # Part2: General Nomenclature
  {
    ###
    
    # Only nomenclature for Oxide in the selected language
    complete_nomenclature <- Nomenclature[[input_family]][[input_external_language]]
    
    # Some details...
    position <- ncol(complete_nomenclature)
    stock_nomenclature <- complete_nomenclature[,12]
    classic_nomenclature <- complete_nomenclature[,c(7:11)]
    iupac_nomenclature <- stock_nomenclature
    set_names <- input_Nomenclature$General02[[input_external_language]][,2]
    
    
    # Special Details
    language_pos <-        c(2,  3,  2)
    names(language_pos) <- c("en", "es", "fr")
    special_detail <- language_pos[names(language_pos) == input_external_language]
    
    
    ###  
  } # End Part2
  #####################################################
  
  
  
  # Part3: Isolate Nomenclature
  {
    ###
    
    
    pre01 <- prefixes[amount1, 2]
    pre02 <- prefixes[amount2, 2]
    my_family <- medium_part_classic[my_pos, special_detail]
    
    my_name1 <- element_values1$name
    my_name2 <- element_values2$name
    
    my_iupac_nomenclature1 <- iupac_nomenclature[input_atomic_number1]
    my_iupac_nomenclature2 <- iupac_nomenclature[input_atomic_number2]
    
    my_stock_nomenclature1 <- stock_nomenclature[input_atomic_number1]
    my_stock_nomenclature2 <- stock_nomenclature[input_atomic_number2]
    
    my_roman1 <- romans[element_values1$selected_valence]
    my_roman2 <- romans[element_values2$selected_valence]
    
    my_classic_nomenclature1 <- classic_nomenclature[input_atomic_number1, element_values1$order_selected_valence]
    my_classic_nomenclature2 <- classic_nomenclature[input_atomic_number2, element_values2$order_selected_valence]
    
    ###  
  } # End Part3
  #########################################
  
  
  
  # Part4: Nomenclature System
  {
    ###
    
    
    
    # Modifications for Language English
    if (input_external_language == "en") {
      
      # IUPAC
      iupac <- "Coming Soon!"
      
      
      
      # Stock 
      stock <- "Coming Soon!"
      
      
      # Classic
      classic <- "Coming Soon!"
      
      
    }
    
    
    
    # Modifications for Language Spanish
    if (input_external_language == "es") {
      
      # Prefix Correction for Spanish# Prefix Correction for Spanish
      if(pre01 == "Mono") pre01 <- ""
      
      # IUPAC
      iupac <- paste0(pre01, my_iupac_nomenclature2, " ", my_iupac_nomenclature1)
      
      
      # Stock 
      stock <- paste0(my_iupac_nomenclature2, " ", my_iupac_nomenclature1, " ", "(", my_roman1, ")")
      
      
      # Classic
      classic <- paste0(my_classic_nomenclature2, " ", my_classic_nomenclature1)
      
      
      

    }
    
    
    
    
    
    
    
    
    ###
  } # End Part4
  ################################################
  
  
  
  
  # Part5: My exit
  {
    ###
    # Save all
    nomenclature_system01 <- c(iupac, stock, classic, chemestry_formule_latex)
    nomenclature_system02 <- paste0(set_names, ": ", nomenclature_system01)
    nomenclature_system03 <- set_names
    my_exit <- as.data.frame(rbind(nomenclature_system01, nomenclature_system02,
                                   nomenclature_system03))
    
    colnames(my_exit) <- set_names
    
    
    ###  
  } # End: Part 5
  ###################################################
  
  
  # Part6: Return
  {
    ###
    
    return(my_exit)
    
    ###
  } # End Part6
  ######################################
  
  
  
  
  
  
} # End Function OxideNomenclature***


# # 
# input_atomic_number1 <- 26 #Litio
# input_valence1 <- 2
# input_atomic_number2 <- 7 #Carbono
# input_valence2 <- 2
# input_internal_language <- "en"
# input_external_language <- "es"
# input_family <- "Salt"
# input_PeriodicTable <- PeriodicTable
# input_Nomenclature <- Nomenclature
# 
# 
# 
#                                 SaltNomenclature(input_atomic_number1 = input_atomic_number1,
#                                                   input_valence1 = input_valence1,
#                                                   input_atomic_number2 = input_atomic_number2,
#                                                   input_valence2 = input_valence2,
#                                                   input_family = input_family,
#                                                   input_internal_language = input_internal_language,
#                                                   input_external_language = input_external_language,
#                                                   input_PeriodicTable = input_PeriodicTable,
#                                                   input_Nomenclature = input_Nomenclature)
# 
# 
# 
