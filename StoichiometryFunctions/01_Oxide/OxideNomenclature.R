



OxideNomenclature <- function(input_atomic_number1 = NULL, 
                              input_valence1 = NULL,
                              input_family = "Oxide",
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
    # Internal Control
    internal_control <- InternalControl_Oxide(input_atomic_number1 = input_atomic_number1,
                                              input_valence1 = input_valence1,
                                              input_internal_language = input_internal_language,
                                              input_external_language = input_external_language,
                                              input_PeriodicTable = input_PeriodicTable)
    # Oxide Resolution
    input_oxide_resolution <- OxideEquation(input_atomic_number1 = input_atomic_number1,
                                            input_valence1 = input_valence1,
                                            input_internal_language = "en",
                                            input_external_language = input_external_language,
                                            input_PeriodicTable = input_PeriodicTable)
    # LaTeX resolution
    input_oxide_latex <- OxideLaTeX(input_atomic_number1 = input_atomic_number1,
                                    input_valence1 = input_valence1,
                                    input_internal_language = "en",
                                    input_external_language = input_external_language,
                                    input_PeriodicTable = input_PeriodicTable)
    input_latex <- input_oxide_latex[[2]]
    # Chemestry Formule
    chemestry_formule_resolution <- input_oxide_resolution[8, c(9,10,11,12)]
    chemestry_formule_latex <- input_latex[8, 8]
    
    amount1 <- as.numeric(as.character(chemestry_formule_resolution[1,2]))
    amount2 <- as.numeric(as.character(chemestry_formule_resolution[1,4]))
    
    ###
  } # Fin Parte 0
  ##############################################
  
  
  # Part 1: Other Necessary Items
  {
    ###
    # Roman numerals
    romans <- c("I", "II", "III", "IV", "V", "VI", "VII", "")
    prefixes <- Nomenclature$Prefixes[[input_external_language]]
    medium_part_classic <- Nomenclature$General01[[input_external_language]]
    ###
  } # Part 1
  ############################################
  
  
  # Parte2: Nomenclature System
  {
    ###
    
    my_complet_nomenclature <- Nomenclature[[input_family]][[input_external_language]]
    
    position <- ncol(my_complet_nomenclature)
    
    my_iupac_nomenclature <- my_complet_nomenclature[,position]
    
    my_classic_nomenclature <- my_complet_nomenclature[,c((position-5): (position-1))]
    
    set_names <- input_Nomenclature$General02[[input_external_language]][,2]
    
    my_pos <- element_values1$order_selected_valence
    
    iupac <- paste0(my_iupac_nomenclature[input_atomic_number1],
                    " ", "(", romans[element_values1$selected_valence], ")")
    
    classic <- my_classic_nomenclature[input_atomic_number1, my_pos]
    
    numeral_stock <- paste0(prefixes[amount1, 2], medium_part_classic[my_pos, 3],
                            " ", prefixes[amount2, 2], element_values1$name)
    
    nomenclature_system01 <- c(iupac, classic, numeral_stock, chemestry_formule_latex)
    dim(nomenclature_system01) <- c(1, length(nomenclature_system01))
    nomenclature_system01 <- as.data.frame(nomenclature_system01)
    colnames(nomenclature_system01) <-  set_names
    
   # nomenclature_system02 <- paste0(names(nomenclature_system01), ": ",nomenclature_system01 )

    
    
    ###
  } # End Part2
  ################################################
  
  
  # Parte3: Return
  {
    ###
   # my_exit <- list(nomenclature_system01, nomenclature_system02)
    my_exit <- nomenclature_system01
    return(my_exit)
    ###
  } # Fin parte 3
  ######################################
  
  
  
  
} # End Function OxideNomenclature***



input_atomic_number1 <- 26 #Litio
input_valence1 <- 2
input_internal_language <- "en"
input_external_language <- "es"
input_family <- "Oxide"
input_PeriodicTable <- PeriodicTable
input_Nomenclature <- Nomenclature



                                OxideNomenclature(input_atomic_number1 = input_atomic_number1,
                                                  input_valence1 = input_valence1,
                                                  input_family = input_family,
                                                  input_internal_language = input_internal_language,
                                                  input_external_language = input_external_language,
                                                  input_PeriodicTable = input_PeriodicTable,
                                                  input_Nomenclature = input_Nomenclature)



