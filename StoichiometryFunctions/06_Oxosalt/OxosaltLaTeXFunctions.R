
# LaTeX of Oxosalt
OxosaltLaTeX <-function(input_atomic_number1 = NULL, input_valence1 = NULL,
                          input_atomic_number2 = NULL, input_valence2 = NULL,
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
    
    ###  
  } # End Internal Settings
  ##########################################################################
  
  
  
  # Part 0: Necessary Participants
  {
    ###
    
    
    # Internal Language
    internal_language <- as.character(input_internal_language)
    
    # Principal objects - First Element
    atomic_number1 <- as.numeric(as.character(input_atomic_number1))
    valence1 <-  as.numeric(as.character(input_valence1))
    
    
    # Recruited values - First Element
    symbol1 <- as.character(input_PeriodicTable[[internal_language]][atomic_number1, 2])
    name1  <- as.character(input_PeriodicTable[[internal_language]][atomic_number1, 3])
    type1 <- as.character(input_PeriodicTable[[internal_language]][atomic_number1, 7])
    state1 <- as.character(input_PeriodicTable[[internal_language]][atomic_number1, 9])
    if (type1 == "Metalloid") type1 <- "Non-metal"
    
    # Principal objects - Second Element
    atomic_number2 <- as.numeric(as.character(input_atomic_number2))
    valence2 <-  as.numeric(as.character(input_valence2))
    
    
    # Recruited values - Second Element
    symbol2 <- as.character(input_PeriodicTable[[internal_language]][atomic_number2, 2])
    name2  <- as.character(input_PeriodicTable[[internal_language]][atomic_number2, 3])
    type2 <- as.character(input_PeriodicTable[[internal_language]][atomic_number2, 7])
    state2 <- as.character(input_PeriodicTable[[internal_language]][atomic_number2, 9])
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
                                               input_internal_language = "en",
                                               input_external_language = input_external_language,
                                               input_PeriodicTable = input_PeriodicTable)
    ###  
  } # End Part 0: Necessary Participants
  ####################################################
  
  
  
  # Part 1: General structure of "matrix_latex"
  {
    ###
    
    
    
    # Name of the columns ...
    my_names <- c("Order", 
                  "Coef1", "E1",
                  "Sign1",
                  "Coef2", "E2",
                  "Coef3", "E3",
                  "Sign2",
                  "Coef4", "E4")
    
    matrix_latex <- matrix(NA, nrow(input_oxosalt_resolution), length(my_names))
    colnames(matrix_latex) <- my_names
    
    # My personal phantom
    my_phantom <- "phantom(1)"
    
    
    ###  
  }
  # End Part 1
  ##################################################
  
  
  # Part 2: Conversion to LaTeX 
  { 
    
    
    
    # We determine the number of steps that exist in the object ...
    total_step <- nrow(input_oxosalt_resolution)
    
    
    order_step <- c(1:nrow(matrix_latex))
    
    coef1 <- input_oxosalt_resolution[,2]
    armed_E1 <- paste0(input_oxosalt_resolution[,3], "[",
                          input_oxosalt_resolution[,4], "]","*",
                          input_oxosalt_resolution[,5], "[",
                          input_oxosalt_resolution[,6], "]") 
    
    sign1 <- rep("'+'", total_step)
    
    coef2 <- input_oxosalt_resolution[,7]
    armed_E2 <- paste0(input_oxosalt_resolution[, 8], "[",
                          input_oxosalt_resolution[, 9], "]","*",
                          input_oxosalt_resolution[,10], "[",
                          input_oxosalt_resolution[,11], "]","*",
                          input_oxosalt_resolution[,12], "[",
                          input_oxosalt_resolution[,13], "]") 
    
    coef3 <- input_oxosalt_resolution[,14]
    
    metralla <- strsplit(input_oxosalt_resolution[,17], "")
    
    my_non_metal <- strsplit(strsplit(input_oxosalt_resolution[2,17], "O")[[1]][1], "[(]")[[1]][2]
    
    for (k in 1:length(metralla)) {
      if (length(metralla[[k]]) > 0) { 
      
      my_pos <- length(metralla[[k]]) - 1
      my_sub <- metralla[[k]][my_pos]
      metralla[[k]] <- paste0("(", my_non_metal, "O", "[", my_sub, "]", ")")  
    } else metralla[[k]] <- ""
    } # End for k
    
    metralla <- unlist(metralla)
    metralla[1] <- ""
    
    armed_E3 <- paste0(input_oxosalt_resolution[,15], "[",
                              input_oxosalt_resolution[,16], "]","*",
                              metralla, "[",
                              input_oxosalt_resolution[,18], "]")  
    
    sign2 <- rep("'+'", total_step)
    
    coef4 <- input_oxosalt_resolution[,19]
    armed_E4 <- paste0(input_oxosalt_resolution[,20], "[",
                       input_oxosalt_resolution[,21], "]","*",
                       input_oxosalt_resolution[,22], "[",
                       input_oxosalt_resolution[,23], "]")  
    
    
    # Matrix Latex Mod
    matrix_latex[, 1] <- order_step
    matrix_latex[, 2] <- coef1
    matrix_latex[, 3] <- armed_E1
    matrix_latex[, 4] <- sign1 
    matrix_latex[, 5] <- coef2
    matrix_latex[, 6] <- armed_E2
    matrix_latex[, 7] <- coef3
    matrix_latex[, 8] <- armed_E3
    matrix_latex[, 9] <- sign2 
    matrix_latex[,10] <- coef4
    matrix_latex[,11] <- armed_E4
    
    
  } # End Part 2
  ###########################################################################################
  
  
  
  # Part 3: Final general modification
  {
    ###
    # Fist Version
    matrix_latex_v1 <- matrix_latex

    
    # Changing a litle for v1
    matrix_latex_v1 <- sub('\\[\\]\\*\\[\\]', "", matrix_latex_v1)
    matrix_latex_v1 <- sub('\\*\\[\\]', "", matrix_latex_v1)
    matrix_latex_v1 <- sub('\\[\\]\\[\\]', "", matrix_latex_v1)
    matrix_latex_v1 <- sub('\\[\\]', "", matrix_latex_v1)
    
    # Second Version
    matrix_latex_v2 <- matrix_latex_v1
    
    # My phamton for coef with value 1 phantom(1)
    matrix_latex_v2[,c(2,5,7,10)] <- gsub('^1$', my_phantom, matrix_latex_v2[,c(2,5,7,10)])
    
    # Non value 1 for subindex
    target <- "\\[1\\]"
    matrix_latex_v2[,c(3,6,8,11)] <- gsub(target, "", matrix_latex_v2[,c(3,6,8,11)])
    
    
    # More phamtons for empty values
    matrix_latex_v2[1,c(7,8,9,10,11)] <- paste0("phantom(", matrix_latex_v2[1,c(7,8,9,10,11)], ")")
    
    
    ###  
  } # End Part 3
  ###########################################################################
  
  
  
  # Part 4: Exit
  {
    ###
    exit <- list(matrix_latex_v1, matrix_latex_v2)
    names(exit) <- c("Original", "Mod")
    return(exit)
    
    ###  
  } # End Part 4: Exit
  #######################################
  
  
  
  
}   




# input_atomic_number1 <- 26 #Hierro
# input_valence1 <- 3
# input_atomic_number2 <- 17 #Cloro
# input_valence2 <- 5
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_GeneralNomenclature01 <-  GeneralNomenclature01
# input_GeneralNomenclature02 <-  GeneralNomenclature02
# input_GeneralNomenclature03 <-  GeneralNomenclature03
# 
# 
# input_oxosalt_resolution <- OxosaltEquation(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                           input_atomic_number2 = input_atomic_number2,
#                                          input_valence2 = input_valence2,
#                                          input_internal_language = "en",
#                                          input_external_language = input_external_language,
#                                          input_PeriodicTable = input_PeriodicTable)
# 
# input_oxosalt_resolution
# 
# 
# input_oxosalt_latex <- OxosaltLaTeX(input_atomic_number1 = input_atomic_number1,
#                                            input_valence1 = input_valence1,
#                                            input_atomic_number2 = input_atomic_number2,
#                                            input_valence2 = input_valence2,
#                                            input_internal_language = "en",
#                                            input_external_language = input_external_language,
#                                            input_PeriodicTable = input_PeriodicTable)
# 
# 
# input_oxosalt_latex
