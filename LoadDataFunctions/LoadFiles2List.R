

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
    
    PeriodicTable[[k]] <- read.csv(file = completed_files_periodic[k], sep=",", 
                                   dec=".", header = T, encoding = "latin1")
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
                                   dec=".", header = T, encoding = "latin1")
  } # End for h
  
  # List names
  names(Nomenclature[[k]]) <- my_language 
  
  } # End ifff
  
  } # End for k
  
  
   
  
  # Return
  return(Nomenclature)
  
}



# The load for Chemestry Family
LoadChemestryFamily <- function(){
  
  # The dir
  my_dir <- "data/ChemestryFamily/ChemestryFamily.csv"

  
  ChemestryFamily <- read.csv(file = my_dir, sep=",", dec=".", 
                              header = T, encoding = "latin1")
  
    

  # Return
  return(ChemestryFamily)
  
}


