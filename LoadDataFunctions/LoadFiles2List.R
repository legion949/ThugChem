

# The load for Periodic Table
LoadPeriodicTable <- function(){
  
  # The list
  PeriodicTable <- list()
  
  # The dir
  my_dir_periodic <- "data/PeriodicTables"
  
  # All files
  my_files_periodic <- list.files(my_dir_periodic, include.dirs = T)
  
  # Completed dir
  completed_files_periodic <- paste0(my_dir_periodic, "/", my_files_periodic)
  
  # The languages
  my_language <- unlist(strsplit(unlist(strsplit(my_files_periodic, ".csv")), "PeriodicTable_"))[c(F,T)]
  
  # The load!
  for (k in 1:length(completed_files_periodic)) {
    
    # PeriodicTable[[k]] <- read.csv(file = completed_files_periodic[k], sep=",", 
    #                                dec=".", header = T, encoding = "latin1")
    # 
    PeriodicTable[[k]] <- read.csv(file = completed_files_periodic[k], sep=",", 
                                   dec=".", header = T, fileEncoding="latin1")
  }
  
  # List names
  names(PeriodicTable) <- my_language  
  
  # Return
  return(PeriodicTable)
  
}



# The load for Nomenclature
LoadNomenclature <- function(){
  
  # The list
  Nomenclature <- list()
  
  # The dir
  my_dir <- "data/Nomenclature"
  my_dir2 <- list.files(my_dir, include.dirs = T)
  
  # Chem Family
  my_chem_family <- unlist(strsplit(my_dir2, "_"))[c(F,T)]

  # New list...
  for (k in 1:length(my_chem_family)) Nomenclature[[k]] <- list()
  names(Nomenclature) <- my_chem_family
  
  for (k in 1:length(my_dir2)){
    
  my_dir3 <- paste0(my_dir, "/", my_dir2[k])
  my_dir4 <- list.files(my_dir3, include.dirs = T)
  my_dir5 <- paste0(my_dir3, "/",my_dir4)
  
  if (length(my_dir4) > 0) {
    
  # The languages
  my_language <- unlist(strsplit(unlist(strsplit(my_dir4, ".csv")), "_"))[c(F,T)]
  
  
  # The load!
  for (h in 1:length(my_dir4)) {
    
    
    
    Nomenclature[[k]][[h]] <- read.csv(file = my_dir5[h], sep=",", 
                                   dec=".", header = T, fileEncoding="latin1")
  } # End for h
  
  # List names
  names(Nomenclature[[k]]) <- my_language 
  
  } # End ifff
  
  } # End for k
  
  
   
  
  # Return
  return(Nomenclature)
  
}



# The load for Inteligent Selection
LoadInteligentSelection <- function(){
  
  # The dir
  my_dir <- "data/ChemestryFamily/InteligentSelection.csv"
  
  
  InteligentSelection <- read.csv(file = my_dir, sep=",", dec=".", 
                              header = T, fileEncoding="latin1")
  
  
  
  # Return
  return(InteligentSelection)
  
}


# The load for Page Family Options
LoadPageFamilyOptions <- function(){
  
  # The dir
  my_dir <- "data/PageFamilyOptions/PageFamilyOptions.csv"
  
  
  PageFamilyOptions <- read.csv(file = my_dir, sep=",", dec=".", 
                                  header = T, fileEncoding="latin1", na.strings = "NA")
  
  
  
  # Return
  return(PageFamilyOptions)
  
}


# The new combination
all_app_language <- c("en", "es", "fr")
NewCombination <- function(all_app_language = NULL){
  
  combinated_options <- list()
  count_internal <- 0
  
  for (k in 1:length(all_app_language)) {
    
  if (sum(names(PeriodicTable) == all_app_language[k]) == 1) {
    
    # Counting...
    count_internal <- count_internal + 1 
    
  # The language
  the_language <- all_app_language[k]
  
  # Information for reactive() in server
  my_atomic_numbers <- PeriodicTable[[the_language]][,1]
  my_atomic_numbers_mod <- as.character(my_atomic_numbers)
  my_count <- str_count(my_atomic_numbers_mod)
  my_atomic_numbers_mod[my_count == 1] <- paste0("  ", my_atomic_numbers_mod[my_count == 1])
  my_atomic_numbers_mod[my_count == 2] <- paste0(" ", my_atomic_numbers_mod[my_count == 2])
  
  my_symbols <- PeriodicTable[[the_language]][,2]
  my_symbols_mod <-as.character(my_symbols)
  my_count2 <- str_count(my_symbols_mod)
  my_symbols_mod[my_count2 == 1] <- paste0(my_symbols_mod[my_count2 == 1], " ")
  my_symbols_mod[my_count2 == 2] <- paste0(my_symbols_mod[my_count2 == 2], "  ")
  
  my_state <- PeriodicTable[[the_language]][,"State"]
  my_state_mod <-as.character(my_state)
  
  my_names <- PeriodicTable[[the_language]][,3]
  my_types <- PeriodicTable[[the_language]][,7]
  
  my_valence <- strsplit(PeriodicTable[[the_language]][,10], ";")
  
  combinated <- my_atomic_numbers
  names(combinated) <- paste0(my_atomic_numbers_mod, " - ", my_symbols_mod,
                                      " - ", my_names, " - ", my_types, 
                                      " - ", my_state_mod) 
  
  
  combinated_options[[count_internal]] <- combinated
  }
  }
  
  
  names(combinated_options) <- all_app_language
  
  # Return
  return(combinated_options)
}


# The load for HelperLevel
LoadPageHelperLevel <- function(){
  

  # The list
  PageHelperLevel <- list()
  
  # The dir
  my_dir <- "data/PageHelperLevel"
  my_dir2 <- list.files(my_dir, include.dirs = T)
  
  # Chem Family
  my_chem_family <- unlist(strsplit(my_dir2, "_"))[c(F,T)]
  
  # New list...
  for (k in 1:length(my_chem_family)) PageHelperLevel[[k]] <- list()
  names(PageHelperLevel) <- my_chem_family
  
  for (k in 1:length(my_dir2)){
    
    my_dir3 <- paste0(my_dir, "/", my_dir2[k])
    my_dir4 <- list.files(my_dir3, include.dirs = T)
    my_dir5 <- paste0(my_dir3, "/",my_dir4)
    
    if (length(my_dir4) > 0) {
      
      # The languages
      my_language <- unlist(strsplit(unlist(strsplit(my_dir4, ".csv")), "_"))[c(F,T)]
      
      
      # The load!
      for (h in 1:length(my_dir4)) {
        
        
        
        PageHelperLevel[[k]][[h]] <- read.csv(file = my_dir5[h], sep=",", 
                                           dec=".", header = T, fileEncoding="latin1")
      } # End for h
      
      # List names
      names(PageHelperLevel[[k]]) <- my_language 
      
    } # End ifff
    
  } # End for k
  
  
  
  
  # Return
  return(PageHelperLevel)
  
  
}
