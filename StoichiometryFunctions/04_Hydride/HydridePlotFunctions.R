

# Plot of Hydride
HydridePlot <-function(input_atomic_number1 = NULL, 
                       input_valence1 = NULL,
                       input_internal_language = "en", 
                       input_external_language = NULL,
                       input_roman = NULL,
                       input_PeriodicTable = NULL,
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
    internal_control <- InternalControl_Hydride(input_atomic_number1 = input_atomic_number1,
                                              input_valence1 = input_valence1,
                                              input_internal_language = input_internal_language,
                                              input_external_language = input_external_language,
                                              input_PeriodicTable = input_PeriodicTable)


    input_hydride_latex <- HydrideLaTeX(input_atomic_number1 = input_atomic_number1,
                                             input_valence1 = input_valence1,
                                             input_internal_language = "en",
                                             input_external_language = input_external_language,
                                             input_PeriodicTable = input_PeriodicTable)


    input_latex <- input_hydride_latex[[2]]

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
    r2 <- romans[1]
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


  # Part 3: Normal Case and Oxygen Case
  {
  ###
    # Normal Case and Oxygen Case
    if (internal_control == 1 | internal_control == 2) {
      
      
      # Completed Chemical Equation
      {
      ###
        
  # GPS       1    2     3      4       5       6      7        8       9
  gps_y <- c( 7,   7,    7,     7,      7,     8.5,   8.5,      7,      7)
  gps_x <- c( 0,   3,    8,    10,     13,      17,    20,   20.5,   23.5)
  my_cex <- c(15,  7,    7,    15,      7,      15,     7,      7,      7)


  # Coef 1
  text(gps_x[1], gps_y[1], eval(parse(text = my_armed_step[1])), cex = my_cex[1], pos = 4)

  # Metal
  text(gps_x[2], gps_y[2], eval(parse(text = my_armed_step[2])), cex = my_cex[2], pos = 4)

  # Sign +
  text(gps_x[3], gps_y[3], eval(parse(text = my_armed_step[3])), cex = my_cex[3], pos = 4)

  # Coef 2
  text(gps_x[4], gps_y[4], eval(parse(text = my_armed_step[4])), cex = my_cex[4], pos = 4)

  # Oxygen
  text(gps_x[5], gps_y[5], eval(parse(text = my_armed_step[5])), cex = my_cex[5], pos = 4)

  # Arrow
  arrows(gps_x[6], gps_y[6], gps_x[7], gps_y[7], lwd = 9, length = 0.70)

  # Coef3
  text(gps_x[8], gps_y[8], eval(parse(text = my_armed_step[6])), cex = my_cex[6], pos = 4)

  # Oxide
  text(gps_x[9], gps_y[9], eval(parse(text = my_armed_step[7])), cex = my_cex[7], pos = 4)

  } # End Completed Chemical Equation
      #################################################

      
      # Valence in Roman Numbers
      if(input_roman) {
        ###
        
        # GPS       1    2     3      4       5       6      7        8       9
        gps_y_valence <- gps_y[c(2,5)] + 10
        gps_x_valence <- gps_x[c(2,5)] + 0.5
        my_cex_valence <- c(3, 3)
        
        
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


  # Part 3: Non-Normal Case: Noble Gas
  {
    ###

      # Noble gas Case
      if (internal_control == 3) {                    ########
        # GPS       1    2     3     4        5       6      7        8       9   10   11  12
        gps_y <- c( 7,   7,    7,     7,      7,     7,   7,      7,      7,   7,    7,  7)
        gps_x <- c( 0,   -1,    5,    10,     9,      14,    18,   19,   19,  23 ,  26, 27)
        my_cex <- c(15,  7,     7,    15,      7,      15,     7,      7,     7,   7,   15,  7)
        #           C1    E1     +     C2      E2     C3      E3      +       C4   E4


        # Coef 1
        text(gps_x[1], gps_y[1], eval(parse(text = my_armed_step[1])), cex = my_cex[1], pos = 4)

        # Element Noble Gas - Reactive
        text(gps_x[2], gps_y[2], eval(parse(text = my_armed_step[2])), cex = my_cex[2], pos = 4)

        # Sign +
        text(gps_x[3], gps_y[3], eval(parse(text = my_armed_step[3])), cex = my_cex[3], pos = 4)

        # Coef 2
        text(gps_x[4], gps_y[4], eval(parse(text = my_armed_step[4])), cex = my_cex[4], pos = 4)

        # Oxygen
        text(gps_x[5], gps_y[5], eval(parse(text = my_armed_step[5])), cex = my_cex[5], pos = 4)

        # Arrow
        arrows(gps_x[6], gps_y[6], gps_x[7], gps_y[7], lwd = 9, length = 0.70)

        # Coef3
        text(gps_x[8], gps_y[8], eval(parse(text = my_armed_step[6])), cex = my_cex[6], pos = 4)

        # Element Noble Gas - Product
        text(gps_x[9], gps_y[9], eval(parse(text = my_armed_step[7])), cex = my_cex[7], pos = 4)

        # Sign +
        text(gps_x[10], gps_y[10], eval(parse(text = my_armed_step[8])), cex = my_cex[8], pos = 4)

        # Coef 4
        text(gps_x[11], gps_y[11], eval(parse(text = my_armed_step[9])), cex = my_cex[9], pos = 4)

        # Oxygen
        text(gps_x[12], gps_y[12], eval(parse(text = my_armed_step[10])), cex = my_cex[10], pos = 4)

      }



    ###
  }
  ######################################################################


 


}




# input_atomic_number1 <- 2
# input_valence1 <- 0
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_step <- 7
# input_GeneralNomenclature01 <-  GeneralNomenclature01
# input_GeneralNomenclature02 <-  GeneralNomenclature02
# input_GeneralNomenclature03 <-  GeneralNomenclature03
# 
# 
# HydridePlot(input_atomic_number1 = input_atomic_number1,
#                                         input_valence1 = input_valence1,
#                                         input_internal_language = "en",
#                                         input_external_language = input_external_language,
#                                         input_PeriodicTable = input_PeriodicTable,
#                                         input_step = input_step)
# 
