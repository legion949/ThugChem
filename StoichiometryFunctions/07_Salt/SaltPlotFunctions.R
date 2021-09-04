

# Plot of Oxosal
SaltPlot <-function(input_atomic_number1 = input_atomic_number1,
                      input_valence1 = input_valence1,
                      input_atomic_number2 = input_atomic_number2,
                      input_valence2 = input_valence2,
                      input_internal_language = "en",
                      input_external_language = input_external_language,
                      input_roman = NULL,
                      input_PeriodicTable = input_PeriodicTable,
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
    if (is.null(input_roman)) input_roman <- TRUE
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
    internal_control <- InternalControl_Salt(input_atomic_number1 = input_atomic_number1,
                                                input_valence1 = input_valence1,
                                                input_atomic_number2 = input_atomic_number2,
                                                input_valence2 = input_valence2,
                                                input_internal_language = input_internal_language,
                                                input_external_language = input_external_language,
                                                input_PeriodicTable = input_PeriodicTable)
    
    
    input_salt_latex <- SaltLaTeX(input_atomic_number1 = input_atomic_number1,
                                        input_valence1 = input_valence1,
                                        input_atomic_number2 = input_atomic_number2,
                                        input_valence2 = input_valence2,
                                        input_internal_language = input_internal_language,
                                        input_external_language = input_external_language,
                                        input_PeriodicTable = input_PeriodicTable)
    
    
    input_latex <- input_salt_latex[[2]]
    
    # My armed step
    my_armed_step <- paste0("expression(",as.vector(input_latex[input_step,c(2:ncol(input_latex))]), ")")
    
    ###  
  } # End Part 0: Necessary Participants
  ####################################################

  
  
  # Part 1: New objects
  {
    ###
    # Romans
    romans <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", " ")
    
    # Roman Valences
    r1 <- romans[as.numeric(as.character(input_valence1))]
    r2 <- romans[as.numeric(as.character(input_valence2))]
    ###
  } # End Part 1
  #################################################################
  
  
  # Part 2: Standard Plot Settings
  {
  ### 
    
  # Print background
  # par(bg = 'orange')
  plot(c(0:30), c(0:30), axes=F, col="orange", xlab=" ", ylab=" ")
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "orange", border = "orange")
  
  ###
  }
  #################################
  
  
  # Part 3: Normal Case 
  {
  ###
    # Normal Case 
    if (internal_control == 1) {
      
      
      
      # Completed Chemical Equation
      {
      ###
      
  library(stringr)
  my_dt <- c(T, F)
  the_position <- c(28.5, 29) 
  my_number <- ""
  
  selected_value01 <- my_armed_step[9]
 # selected_value01 <-   "expression(phantom(3))"
  selected_value02 <- as.character(sub("\\)", "", sub("expression\\(", "", selected_value01)))
  selected_value03 <- sub("\\)", "", sub("phantom\\(", "", selected_value02))

  # logic value...
  if (str_count(selected_value03) > 0 ) {
  my_number <- as.numeric(as.character(selected_value03))
  if (str_count(my_number) > 1) my_dt <- c(F, T)
  }
       
    my_position <- the_position[my_dt]
    
  # GPS        1      2      3      4        5       6      7        8       9   10   11   12
  gps_y <- c(  7,     7,     7,     7,       7,     8.5,   8.5,      7,      7,   7,   7,   7) 
  gps_x <- c( -1,   0.5,     5,     7,     8.5,    12.5,    18,     19,   20.5,  24,  my_position,  28.5) 
  my_cex <- c( 7,     3,     7,     7,       3,       7,     3,      7,      7,   3) 
  
  

  
  # Coef 1
  text(gps_x[1], gps_y[1], eval(parse(text = my_armed_step[1])), cex = my_cex[1], pos = 4)
  
  # E1
  text(gps_x[2], gps_y[2], eval(parse(text = my_armed_step[2])), cex = my_cex[2], pos = 4)
  
  # Sign +
  text(gps_x[3], gps_y[3], eval(parse(text = my_armed_step[3])), cex = my_cex[3], pos = 4)
  
  # Coef 2
  text(gps_x[4], gps_y[4], eval(parse(text = my_armed_step[4])), cex = my_cex[4], pos = 4)
  
  # E2
  text(gps_x[5], gps_y[5], eval(parse(text = my_armed_step[5])), cex = my_cex[5], pos = 4)
  
  # Arrow
  arrows(gps_x[6], gps_y[6], gps_x[7], gps_y[7], lwd = 9, length = 0.70)
  
  # Coef3
  text(gps_x[8], gps_y[8], eval(parse(text = my_armed_step[6])), cex = my_cex[6], pos = 4)
  
  # E3
  text(gps_x[9], gps_y[9], eval(parse(text = my_armed_step[7])), cex = my_cex[7], pos = 4)

  # Sign +
  text(gps_x[10], gps_y[10], eval(parse(text = my_armed_step[8])), cex = my_cex[8], pos = 4)
  
  # Coef4
  text(gps_x[11], gps_y[11], eval(parse(text = my_armed_step[9])), cex = my_cex[9], pos = 2)
  
  # E4
  text(gps_x[12], gps_y[12], eval(parse(text = my_armed_step[10])), cex = my_cex[10], pos = 4)
  
  } # End Completed equation
      ##########################################

      
      # Valence in Roman Numbers
      if(input_roman) {
        ###
        
        # GPS       1    2     3      4       5       6      7        8       9
        gps_y_valence <- gps_y[c(2,5)] + 10
        gps_x_valence <- gps_x[c(2,5)] + c(0.3, 0.3)
        my_cex_valence <- c(3,3)
        
        
        # Valence in roman numbers
        text(gps_x_valence[1], gps_y_valence[1], r1, cex = my_cex_valence[1], pos = 4)
        text(gps_x_valence[2], gps_y_valence[2], r2, cex = my_cex_valence[2], pos = 4)
        
        ###  
      } # End Valence
      ###################################
      
      
    } # End if

  
  ###
  }
  ######################################################################
  


  
               

}  




# input_atomic_number1 <- 26 #Hierro
# input_valence1 <- 3
# input_atomic_number2 <- 17 #Cloro
# input_valence2 <- 7
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_GeneralNomenclature01 <-  GeneralNomenclature01
# input_GeneralNomenclature02 <-  GeneralNomenclature02
# input_GeneralNomenclature03 <-  GeneralNomenclature03
# input_step <- 1
# 
# 
#  SaltPlot(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                          input_atomic_number2 = input_atomic_number2,
#                                          input_valence2 = input_valence2,
#                                          input_internal_language = "en",
#                                          input_step = input_step,
#                                          input_external_language = input_external_language,
#                                          input_PeriodicTable = input_PeriodicTable)
# 
