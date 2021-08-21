


Internal_ElementValues <- function(input_atomic_number1 = NULL,
                           input_valence1 = NULL,
                           input_internal_language = "en", 
                           input_external_language = NULL, 
                           input_PeriodicTable = NULL){
  
  
  # Internal Settings
  {
    ###
    # Internal language specifications by default and optional if null
    input_internal_language <- "en"
    if (is.null(input_external_language)) input_external_language <- input_internal_language

    
    ###
  } # End Internal Settings
  ##########################################################################
  
  
  # Details about my element
  {
  ###  
  
    # Principal objects
    atomic_number <- as.numeric(as.character(input_atomic_number1))
    selected_valence <-  as.numeric(as.character(input_valence1))
    internal_language <- as.character(input_internal_language)
    external_language <- as.character(input_external_language)
    
    # Recruited values
    symbol <- as.character(input_PeriodicTable[[external_language]][atomic_number, 2])
    name  <- as.character(input_PeriodicTable[[external_language]][atomic_number, 3])
    type <- as.character(input_PeriodicTable[[external_language]][atomic_number, 7])
    state <- as.character(input_PeriodicTable[[external_language]][atomic_number, 9])
    
    # More special items
    pack_valences <- input_PeriodicTable[[external_language]][atomic_number,10]
    all_valences_values <- as.numeric(strsplit(as.character(pack_valences), ";")[[1]])
    quantity_valences <- as.numeric(as.character(input_PeriodicTable[[external_language]][atomic_number,15]))
    order_valences <- c(1:quantity_valences)
    dt_valence <- all_valences_values == selected_valence
    order_selected_valence <- order_valences[dt_valence]
    
    
    my_armed <- as.data.frame(cbind(atomic_number, symbol, name, type, state, quantity_valences, pack_valences, selected_valence, order_selected_valence))
    my_names <- names(my_armed)
    my_exit <- list(atomic_number, symbol, name, type, state, quantity_valences, pack_valences, selected_valence, order_selected_valence)
    names(my_exit) <- my_names
    
  
    
  ###    
  }
  #############################
  
  # Return
  return(my_exit)
  
  
}



# input_atomic_number1 <- 26
# input_valence1 <- 2
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# 
# Internal_ElementValues(input_atomic_number1 = input_atomic_number1,
#                        input_valence1 = input_valence1,
#                        input_internal_language = input_internal_language,
#                        input_external_language = input_external_language,
#                        input_PeriodicTable = input_PeriodicTable)