

# Libreries
library(shiny)
library(shinyjs) # Language detection
library(shinydashboard) # New Sthetic
library(tableHTML) # Color cell in tables
library(numbers) # Minimum Common Divisor
library(stringr) # str_count()
library(shiny.i18n)



# The function for load others functions!
LoadFunctions <- function(the_dir = NULL){
  
  all_chem_files <- list.files(the_dir, recursive = T)
  all_completed <- paste0(the_dir, "/", all_chem_files)
  for (k in 1:length(all_completed)) { 
    # cat(k, "\n")
    source(all_completed[k])
  }
  
}

# Load Stoichiometry Functions!
LoadFunctions(the_dir = "StoichiometryFunctions")


# Load Functions to Load Data!
LoadFunctions(the_dir = "LoadDataFunctions")

# Load More Functions!
LoadFunctions(the_dir = "MoreFunctions")


# Source prefiles
source("prefiles.R")




