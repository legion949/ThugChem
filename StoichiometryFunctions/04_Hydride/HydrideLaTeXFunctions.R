

# LaTeX of hydride
HydrideLaTeX <-function(input_atomic_number1 = NULL, input_valence1 = NULL, 
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
  
  
  
  # Part 1: General structure of "matrix_latex"
  {
    ###
    
    
    
    # Name of the columns ...
    my_names <- c("Order", 
                  "Coef1", "Element",
                  "Sign1",
                  "Coef2", "Hydrogen",
                  "Coef3", "Hydride")
    
    matrix_latex <- matrix(NA, nrow(input_hydride_resolution), length(my_names))
    colnames(matrix_latex) <- my_names
    
    # My personal phantom
    my_phantom <- "phantom(1)"
    
    ###  
  }
  # Fin Part 1
  ##################################################
  
  
  # Part 2: Conversion to LaTeX 
  { 
    
    
    
    # We determine the number of steps that exist in the object ...
    total_step <- nrow(input_hydride_resolution)
    
    
    order_step <- c(1:nrow(matrix_latex))
    
    coef1 <- input_hydride_resolution[,2]
    armed_element <- paste0(input_hydride_resolution[,3], "[",input_hydride_resolution[,4], "]") 
    
    sign1 <- rep("'+'", total_step)
    
    coef2 <- input_hydride_resolution[,5]
    armed_hydrogen <- paste0(input_hydride_resolution[,6], "[",input_hydride_resolution[,7], "]") 
    
    coef3 <- input_hydride_resolution[,8]
    armed_hydride <- paste0(input_hydride_resolution[,9], "[",input_hydride_resolution[,10], "]","*",
                          input_hydride_resolution[,11], "[",input_hydride_resolution[,12], "]") 
    
    # Matrix Latex Mod
    matrix_latex[,1] <- order_step
    matrix_latex[,2] <- coef1
    matrix_latex[,3] <- armed_element
    matrix_latex[,4] <- sign1 
    matrix_latex[,5] <- coef2
    matrix_latex[,6] <- armed_hydrogen
    matrix_latex[,7] <- coef3
    matrix_latex[,8] <- armed_hydride
    
    
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
    matrix_latex_v2[,c(2,5,7)] <- gsub('^1$', my_phantom, matrix_latex_v2[,c(2,5,7)])
    
    # Non value 1 for subindex
    target <- "\\[1\\]"
    matrix_latex_v2[,c(3,6,8)] <- gsub(target, "", matrix_latex_v2[,c(3,6,8)])
    
    
    # Empty values for more phamtons
    matrix_latex_v2[c(1,2), c(7,8)] <- paste0("phantom(", matrix_latex_v2[c(1,2),c(7,8)], ")")
    
    
    ###  
  } # End Part 3
  ###########################################################################
  
  
  
  # Part 3: Noble gas Modification
  {
    ###
    # Normal case and hydrogen case don't need changes.
    # But, for Noble gas we must make a new column.
    if (internal_control == 3) { 
      
      # Columns separation
      my_new <- matrix(unlist(strsplit(matrix_latex[,"Hydride"], "\\*")),nrow(matrix_latex) , 2, byrow = T)
      colnames(my_new) <- c("HydrogenProd", "NobleGas")
      
      # Signs added and Coef added
      Sign2 <- rep("'+'", nrow(matrix_latex))
      Coef3 <- rep(1, nrow(matrix_latex))
      matrix_latex <- cbind(matrix_latex[,c(1:7)], my_new[,1], Sign2, Coef3, my_new[,2])
      colnames(matrix_latex)[c(8,11)] <- colnames(my_new)
      
      
      # Fist Version 
      matrix_latex_v1 <- matrix_latex
      
      
      # Changing a litle for v1
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
      
      # Empty values for more phamtons
      matrix_latex_v2[c(1),c(7,8,9,11)] <- paste0("phantom(", matrix_latex_v2[c(1),c(7,8,9,11)], ")")
      
      
    }
  } # End Part 3
  ########################################################################
  
  
  
  
  
  # Part 5: Exit
  {
    ###
    exit <- list(matrix_latex_v1, matrix_latex_v2)
    names(exit) <- c("Original", "Mod")
    return(exit)
    
    ###  
  } # End Part 3: Exit
  #######################################
  
  
  
  
}  




# input_atomic_number1 <- 2
# input_valence1 <- 0
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_GeneralNomenclature01 <-  GeneralNomenclature01
# input_GeneralNomenclature02 <-  GeneralNomenclature02
# input_GeneralNomenclature03 <-  GeneralNomenclature03
# 
# 
# input_hydride_resolution <- hydrideEquation(input_atomic_number1 = input_atomic_number1,
#                                         input_valence1 = input_valence1,
#                                         input_internal_language = "en",
#                                         input_external_language = input_external_language,
#                                         input_PeriodicTable = input_PeriodicTable)
# 
# input_hydride_resolution
# 
# 
# 
# latex_hydride_resolution   <-         HydrideLaTeX(input_atomic_number1 = input_atomic_number1,
#                                         input_valence1 = input_valence1,
#                                         input_internal_language = "en",
#                                         input_external_language = input_external_language,
#                                         input_PeriodicTable = input_PeriodicTable)
# 
# 
# 
# latex_hydride_resolution