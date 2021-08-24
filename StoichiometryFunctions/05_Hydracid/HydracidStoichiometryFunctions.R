


# Resolution 05: Hydracid
HydracidEquation <- function(input_atomic_number1 = NULL, input_valence1 = NULL, 
                        input_internal_language = "en", input_external_language = NULL,
                        input_PeriodicTable = NULL) {
  
  
  
                 
            # Hydracid is a Hydride on Water... No more.
            
  
  input_hydracid_resolution <- HydrideEquation(input_atomic_number1 = input_atomic_number1,
                                              input_valence1 = input_valence1,
                                              input_internal_language = "en",
                                              input_external_language = input_external_language,
                                              input_PeriodicTable = input_PeriodicTable)
  
  
  # New column order
  input_hydracid_resolution <- input_hydracid_resolution[,c(1:8, 11, 12, 9, 10)]
  
        
        # Part 3: Return
        {
        ###
          
          
          return(input_hydracid_resolution)
          
          
        ###  
        } # End Part 3: Return
        ############################################
        
   
      
  
   
  
  
} # End Function***


InternalControl_Hydracid <- function(input_atomic_number1 = NULL, 
                                   input_valence1 = NULL, 
                                   input_internal_language = "en",
                                   input_external_language = NULL,
                                   input_PeriodicTable = NULL) { 
  
  
  
  
  internal_control <- InternalControl_Oxacid(input_atomic_number1 = input_atomic_number1,
                                             input_valence1 = input_valence1,
                                             input_internal_language = input_internal_language,
                                             input_external_language = input_external_language,
                                             input_PeriodicTable = input_PeriodicTable)
  
  # Return
  return(internal_control)
  }

# input_atomic_number1 <- 6
# input_valence1 <- 2
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_GeneralNomenclature01 <-  GeneralNomenclature01
# input_GeneralNomenclature02 <-  GeneralNomenclature02
# input_GeneralNomenclature03 <-  GeneralNomenclature03
# 
# 
# input_hydracid_resolution <- HydracidEquation(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                          input_internal_language = "en",
#                                          input_external_language = input_external_language,
#                                          input_PeriodicTable = input_PeriodicTable)
# 
# 
# input_hydracid_resolution