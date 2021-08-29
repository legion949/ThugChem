


# Resolution 06: Oxosalt
OxosaltEquation <- function(input_atomic_number1 = NULL, input_valence1 = NULL,
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




    ###
  } # End Part 0: Necessary Participants
  ####################################################


  # Part 1: Structure of the "matrix_solution"
  {
    ###

    # 1.0) Explanation about Part 1 (if you want to read)
    {
      ###
      # The "matrix_solution" has 8 rows y 21 columns.
      # Each row is a step on the stoichiometry resolution (21 steps for oxosalt = 21 rows).
      # Each column is a equation detail about each step.
      #
      # The "matrix_solution" has the next columns for oxides:
      #  1) Step Order

      # The Hydroxide
      #  2) coefficient of the hydroxide in reactants
      #  3) Abbreviation of the metal Element in the hydroxide in reagents
      #  4) Sub-index of the Metal Element in the hydroxide in reactants
      #  5) Abbreviation of the Hydroxyle  in the hydroxide in reagents
      #  6) Sub-index of the Hydroxyle  in the hydroxide in reactants

      # The Hydracid
      #  7) coefficient of the hydracid in reactants
      #  8) Abbreviation of the Hydrogen in the hydracid in reagents
      #  9) Sub-index of the Hydracid in the hydracid in reactants
      # 10) Abbreviation of the Non-metal element the hydracid in reagents
      # 11) Sub-index of the Non-metal element the hydracid in reagents

      # The Oxosalt
      # 12) coefficient of the oxosalt in products
      # 13) Abbreviation of the Metal Element in the oxosalt in products
      # 14) Sub-index of the Metal Element in the oxosalt in products
      # 15) Abbreviation of the Oxacid Group in the oxosalt in products
      # 16) Sub-index of the Oxacid Group in the oxosalt in products

      # The Water
      # 17) coefficient of the water in products
      # 18) Abbreviation of the Hydrogen in products
      # 19) Sub-index of the Hydrogen in products
      # 20) Abbreviation of the Oxygen in products
      # 21) Sub-index of the Oxygen in products

      ###
    } # End 1.0 Explanation
    ########################################################


    # 1.1) The creation for "matrix_solution"
    {
      ###

      # Total Steps for Oxosalt Resolution (TSFOR)
      TSFOR <- 9


      my_names <- c("Order",
                    "Coef1", "E01", "Sub1","E02", "Sub2",
                    "Coef2", "E03", "Sub3","E04", "Sub4", "E05", "Sub5",
                    "Coef3", "E06", "Sub6", "E07", "Sub7",
                    "Coef4", "E08", "Sub8", "E09", "Sub9")


      matrix_solution <- data.frame(matrix(NA, TSFOR, length(my_names)))
      colnames(matrix_solution) <- my_names
      ###
    } # End The creation for "matrix_solution"
    ########################################################


    ###
  } # End Part 1: Structure of the "matrix_solution"
  ############################################################



  # Part 2: Stoichiometry Resolution for Oxosalt
  {
    ###
    # Order is very important...
    # First time... we need a metal and a non-metal
    # and all is OK!
    if (internal_control == 1) {


      # 2.1) New Details
      {


        dt_internal_1 <- type1 == "Metal" && type2 == "Non-metal"
        dt_internal_2 <- type1 == "Non-metal" && type2 == "Metal"

        # If first is the metal and after that the non-metal
        if (dt_internal_1 == TRUE) {

          example1 <- HydroxideEquation(input_atomic_number1 = atomic_number1,
                                        input_valence1 = valence1,
                                        input_internal_language = "en",
                                        input_external_language = input_external_language,
                                        input_PeriodicTable = input_PeriodicTable)

          example2 <- OxacidEquation(input_atomic_number1 = atomic_number2,
                                     input_valence1 = valence2,
                                     input_internal_language = "en",
                                     input_external_language = input_external_language,
                                     input_PeriodicTable = input_PeriodicTable)

        }

        # If first is the non-metal and after that the metal
        if (dt_internal_2 == TRUE) {
          example1 <- HydroxideEquation(input_atomic_number1 = atomic_number2,
                                        input_valence1 = valence2,
                                        input_internal_language = "en",
                                        input_external_language = input_external_language,
                                        input_PeriodicTable = input_PeriodicTable)

          example2 <- OxacidEquation(input_atomic_number1 = atomic_number1,
                                     input_valence1 = valence1,
                                     input_internal_language = "en",
                                     input_external_language = input_external_language,
                                     input_PeriodicTable = input_PeriodicTable)
        }


        # hydroxide Chemical formule
        cf1_element1 <- as.character(example1[nrow(example1), 13])
        cf1_sub1 <- example1[nrow(example1), 14]
        cf1_element2 <- as.character(example1[nrow(example1), 15])
        cf1_sub2 <- example1[nrow(example1), 16]


        # Oxacid chemical formule
        cf2_element1 <- as.character(example2[nrow(example2), 13])
        cf2_sub1 <- example2[nrow(example2), 14]
        cf2_element2 <- as.character(example2[nrow(example2), 15])
        cf2_sub2 <- example2[nrow(example2), 16]
        cf2_element3 <- as.character(example2[nrow(example2), 17])
        cf2_sub3 <- example2[nrow(example2), 18]

        # Ion Oxacid chemical formule
        cf_ion_oxacid <- paste0("(", cf2_element2, cf2_element3, cf2_sub3,")")

      } # End NEw Details
      ########################################################################


      # 2.2) Explanation about Part 2 (if you want to read)
      {
        ###

        # In the case of Oxides ...
        # The Stoichiometry is carried out as follows:
        #
        # 1) Presentation for the Reactives
        # Hydroxide and Oxacid separated both with a "+" sign, both being reactive.
        # Subscripts added too.
        # ---- In computing, coefficients and subscripts equal to 1 ----
        # ---- and only in reagents. Nothing in products. ----
        # To the right of the Oxacid an arrow called "Reaction Arrow" is placed.
        # After the reaction arrow the products will be detailed.
        #
        #
        #
        # 2) Products presentation
        # Oxosal and water separated both with a "+" sign.
        # Oxosal without subindex.
        # Water with normal index.
        # Coefficients are 1 for each one.
        #
        #
        #
        # 3) Oxosal subindex presentation
        # - In the Oxosal, the Oxacid subindex is the subindex for the
        #   hydroxyle from reactives
        # - In the Oxosal, for Metal subindex is the subindex for the
        #   Hydrogen of the Oxacid in hydroxyle from reactives.
        #
        # 4) Oxosal subindex simplification
        # If is possible we must simplify subindex in the oxosal.
        # If that is possible, only simplify without modify coefficient for the oxosal.
        #
        # 5) Balance of the metal
        # The coefficient for Hydroxide in reactives will be our metal index in Oxosalt.
        #
        # 6) Balance of the non-metal
        # The coefficient for Oxacid in reactives will be our ion oxacid index in Oxosalt.
        #
        # 7) Balance of Water (1 of 2) - Hydrogen
        # This step 7 balances only hydrogen, and will modify the coefficient for the water.
        # To calculate the amount of hydrogen in reagents
        # multiply the coefficient for the hydroxide and the corresponding subindex for
        # the hydroxyle.
        # To calculate the amount of hydrogen in products
        # are multiplied by the water coefficient and the subindex
        # of hydrogen in the water.
        #
        #
        # 8) Balance of Oxygen
        # Balance is ready. The step before balanced all.
        #
        # 9) Coefficient Simplification
        # If possible, simplify all coefficients by the
        # same least common divisor (gcd).




        ###
      } # End: 2.0) Explanation about Part 2 (if you want to read)
      #########################################################


      # 2.3) Step by step
      {
        ###

        # We plant a "zero" seed with Stoichiometry steps.
        general_internal_step <- 0


        # Step 1 of 9 - Oxosalt
        {
          ###

          # 1) Presentation for the Reactives
          # Hydroxide and Oxacid separated both with a "+" sign, both being reactive.
          # Subscripts added too.
          # ---- In computing, coefficients and subscripts equal to 1 ----
          # ---- and only in reagents. Nothing in products. ----
          # To the right of the Oxacid an arrow called "Reaction Arrow" is placed.
          # After the reaction arrow the products will be detailed.
          ##################################################################

          # One more step...
          general_internal_step <- general_internal_step + 1

          # Creation of step 1 ...
          gate1 <- rep(NA, ncol(matrix_solution))
          names(gate1) <- colnames(matrix_solution)

          # Implementamos el gate1...
          gate1["Order"] <- general_internal_step

          # Hydroxide
          gate1["Coef1"] <- 1
          gate1["E01"]   <- cf1_element1
          gate1["Sub1"]  <- cf1_sub1
          gate1["E02"]   <- cf1_element2
          gate1["Sub2"]  <- cf1_sub2

          # Oxacid
          gate1["Coef2"] <- 1
          gate1["E03"]   <- cf2_element1
          gate1["Sub3"]  <- cf2_sub1
          gate1["E04"]   <- cf2_element2
          gate1["Sub4"]  <- cf2_sub2
          gate1["E05"]   <- cf2_element3
          gate1["Sub5"]  <- cf2_sub3

          # Oxosalt
          gate1["Coef3"] <- ""
          gate1["E06"] <- ""
          gate1["Sub6"] <- ""
          gate1["E07"] <- ""
          gate1["Sub7"] <- ""

          # Water
          gate1["Coef4"] <- ""
          gate1["E08"] <- ""
          gate1["Sub8"] <- ""
          gate1["E09"] <- ""
          gate1["Sub9"] <- ""


          # Load gate1 on matrix_solution
          matrix_solution[general_internal_step, ] <- gate1

          ###
        } # End Step 1 of 9 - Oxosalt
        ####################################################################


        # Step 2 of 9 - Oxosalt
        {
          ###

          # 2) Reactives Details
          # Oxosal and water separated both with a "+" sign.
          # Oxosal without subindex.
          # Water with normal index.
          # Coefficients are 1 for each one.
          #####################################################################


          # One more step...
          general_internal_step <- general_internal_step + 1


          # Creation of the gate2 (We take values before)
          gate2 <- gate1


          # Implementation of gate2 modifications...
          # Oxosalt
          gate2["Coef3"]  <- 1
          gate2["E06"]    <- cf1_element1
          gate2["Sub6"]   <- 1
          gate2["E07"]    <- cf_ion_oxacid
          gate2["Sub7"]   <- 1

          # Water
          gate2["Coef4"] <- 1
          gate2["E08"]   <- "H"
          gate2["Sub8"]  <- 2
          gate2["E09"]   <- "O"
          gate2["Sub9"]  <- "1"


          # Load the gate2
          matrix_solution[general_internal_step, ] <- gate2


          ###
        } # End Step 2 of 9 - Oxosalt
        ####################################################################


        # Step 3 of 9 - Oxosalt
        {
          ###

          # 3) Oxosal subindex presentation
          # - In the Oxosal, the Oxacid subindex is the subindex for the
          #   hydroxyle from reactives
          # - In the Oxosal, for Metal subindex is the subindex for the
          #   Hydrogen of the Oxacid in hydroxyle from reactives.
          #######################################################################


          # One more step...
          general_internal_step <- general_internal_step + 1


          # Creation of the gate2 (We take values before)
          gate3 <- gate2

          # Oxosalt
          gate3["Sub6"]   <- cf2_sub1
          gate3["Sub7"]   <- cf1_sub2


          # Load the gate3
          matrix_solution[general_internal_step, ] <- gate3


          ###
        } # End Step  3 of 9 - Oxosalt
        ####################################################################


        # Step  4 of 9 - Oxosalt
        {
          ###

          # 4) Oxosal subindex simplification
          # If is possible we must simplify subindex in the oxosal.
          # If that is possible, only simplify without modify coefficient for the oxosal.
          #############################################################################

          # One more step...
          general_internal_step <- general_internal_step + 1

          # Creation of the gate4
          gate4 <- gate3

          # Oxosalt - Subindex before step
          sub_ind6 <- as.numeric(as.character(gate3["Sub6"]))
          sub_ind7 <- as.numeric(as.character(gate3["Sub7"]))

          # We calculate the least common divisor.
          # I found a library that does it directly!
          library(numbers)
          mcd <- mGCD(c(sub_ind6, sub_ind7))

          # The new subscripts in the oxide ...
          mod_sub_ind6 <- sub_ind6/mcd
          mod_sub_ind7 <- sub_ind7/mcd


          # Implementation of gate4 modifications...
          gate4["Order"] <- general_internal_step
          gate4["Sub6"] <- mod_sub_ind6
          gate4["Sub7"] <- mod_sub_ind7


          # Load the gate gate4
          matrix_solution[general_internal_step, ] <- gate4

          # Remove objects...
          remove(sub_ind6, sub_ind7, mod_sub_ind6, mod_sub_ind7)

          ###
        } # End Step  4 of 9 - Oxosalt
        ####################################################################


        # Step 5 of 9 - Oxosalt
        {
          ###

          # 5) Balance of the metal
          # The coefficient for Hydroxide in reactives will be our metal subindex in Oxosalt.
          #####################################################################

          # One step more...
          general_internal_step <- general_internal_step + 1

          # Gate 5 creation
          gate5 <- gate4

          # Implementation of changes in Gate 5
          gate5["Order"] <- general_internal_step
          gate5["Coef1"] <- gate4["Sub6"]



          # Load of the gate 5
          matrix_solution[general_internal_step, ] <- gate5


          ###
        } # End Step 5 of 9 - Oxosalt
        ##################################################################################################################


        # Step 6 of 9 - Oxosalt
        {
          ###

          # 6) Balance of the non-metal
          # The coefficient for Oxacid in ractives will be our ion oxacid index in Oxosalt.
          ############################################################################



          # One step more...
          general_internal_step <- general_internal_step + 1

          # Gate 6 creation
          gate6 <- gate5

          # Implementation of changes in Gate 6
          gate6["Order"] <- general_internal_step
          gate6["Coef2"] <- gate5["Sub7"]


          # Load the gate6
          matrix_solution[general_internal_step, ] <- gate6


          ###
        } # End Step 6 of 9 - Oxosalt
        ##################################################################################################################



        # Step 7 of 9 - Oxosalt
        {
          ###

          # 7) Balance of Water (1 of 2) - Hydrogen
          # This step 7 balances only hydrogen, and will modify the coefficient for the water.
          # To calculate the amount of hydrogen in reagents
          # multiply the coefficient for the hydroxide and the corresponding subindex for
          # the hydroxyle.
          # To calculate the amount of hydrogen in products
          # are multiplied by the water coefficient and the subindex
          # of hydrogen in the water.
          ###########################################################################



          # One step more...
          general_internal_step <- general_internal_step + 1

          # Creation of the gate7...
          gate7 <- gate6

          # Implementation of general_internal_step
          gate7["Order"] <- general_internal_step

          # We retrieve subscripts and coefficient of gate6 for the Hydrogen
          coef1 <- as.numeric(as.character(gate6["Coef1"]))
          sub_ind2 <- as.numeric(as.character(gate6["Sub2"]))
          coef2 <- as.numeric(as.character(gate6["Coef2"]))
          sub_ind3 <- as.numeric(as.character(gate6["Sub3"]))
          coef4 <- as.numeric(as.character(gate6["Coef4"]))
          sub_ind8 <- as.numeric(as.character(gate6["Sub8"]))


          # We calculate the amount of the Hydrogen in reactants and in products
          counting_hydrogen_reactive <- coef1*sub_ind2 + coef2*sub_ind3
          counting_hydrogen_products <- coef4*sub_ind8

          mod_coef4 <- counting_hydrogen_reactive / counting_hydrogen_products



          # We implement the modifications of gate7 ...
          gate7["Coef4"] <- mod_coef4


          # Load the gate7
          matrix_solution[general_internal_step, ] <- gate7

          # We eliminate what is no longer needed from this internal step ...
          remove(coef1, sub_ind2, coef4, sub_ind8, mod_coef4)

          ###
        } # End Step 7 of 9 - Oxosalt
        ##################################################################################################################


        # Step 8 of 9 - Oxosalt
        {
          ###

          # 8) Balance of Oxygen
          # Balance is ready. The step before balanced all.
          ###########################################################################


          # One step more...
          general_internal_step <- general_internal_step + 1

          # Creation of the gate8...
          gate8 <- gate7

          # Implementation of general_internal_step
          gate8["Order"] <- general_internal_step


          # Load the gate8
          matrix_solution[general_internal_step, ] <- gate8


          ###
        } # End Step 8 of 9 - Oxosalt
        ##################################################################################################################



        # Step 9 of 9 - Oxosalt
        {
          ###

          # 9) Coefficient Simplification
          # If possible, simplify all coefficients by the
          # same least common divisor (gcd).
          ######################################################################


          # One step more...
          general_internal_step <- general_internal_step + 1

          # Creation of the gate9
          gate9 <- gate8

          # Implementation of de general_internal_step changes
          gate9["Order"] <- general_internal_step

          # We retrieve the three coefficients of gate8 ...
          coef1 <- as.numeric(as.character(gate8["Coef1"]))
          coef2 <- as.numeric(as.character(gate8["Coef2"]))
          coef3 <- as.numeric(as.character(gate8["Coef3"]))
          coef4 <- as.numeric(as.character(gate8["Coef4"]))


          # We calculate the least common divisor.
          # I found a library that does it directly!
          library(numbers)
          mcd <- mGCD(c(coef1, coef2, coef3, coef4))

          # We create the new subscripts in the oxide ...
          mod_coef1 <- coef1/mcd
          mod_coef2 <- coef2/mcd
          mod_coef3 <- coef3/mcd
          mod_coef4 <- coef4/mcd


          # We implement the modifications of gate8 ...
          gate9["Coef1"] <- mod_coef1
          gate9["Coef2"] <- mod_coef2
          gate9["Coef3"] <- mod_coef3
          gate9["Coef4"] <- mod_coef4


          # Load the gate9
          matrix_solution[general_internal_step, ] <- gate9

          # We eliminate what is no longer needed ...
          remove(coef1, coef2, coef3, coef4, mcd, mod_coef1, mod_coef2, mod_coef3, mod_coef4)

          ###
        } # End Step 9 of 9 - Oxosalt
        ##################################################################################################################


      } # End Step by step
      ###########################################################################


    }  # End all is OK!
    ##########################################################



  } # End Part 2: Stoichiometry Resolution for Oxosalt
  ############################################################


  # Parte3: Special cases
  {
    ###
    # All special case fall here!
    if (internal_control == 2) {

      matrix_solution <- data.frame(matrix("Oxosalt - Special Cases", nrow(matrix_solution), ncol(matrix_solution)))
    } #
    ###
  } # End Part 3: Special cases
  ##############################################################


  # Part 4: SandBox
  {
    ###
    # SandBox!
    if (internal_control == 3) {
      matrix_sandbox <- matrix("Oxide - SandBox", nrow(matrix_solution), ncol(matrix_solution))
      colnames(matrix_sandbox) <- colnames(matrix_solution)
      matrix_sandbox[,1] <- c(1:nrow(matrix_sandbox))
      matrix_solution <- matrix_sandbox
    } # End SandBox!
    ###
  } # End Part 4: SandBox
  ################################################




  # Part 5: Return
  {
    ###


    return(matrix_solution)


    ###
  } # End Part 5: Return
  ############################################









} # End Function***



InternalControl_Oxosalt <- function(input_atomic_number1 = NULL, input_valence1 = NULL,
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

    # We gather all the information we need to
    # carry out the exercise

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




    ###
  } # End Part 0: Necessary Participants
  ####################################################

  # Part 1: Detection
  {
    ###
    # Special detection
    dt1_out <- symbol1 == "H" | symbol2 == "H"
    dt2_out <- symbol1 == "O" | symbol2 == "O"
    dt3_out <- type1 == "Noble gas" | type2 == "Noble gas"
    dt4_out <- sum(dt1_out, dt2_out, dt3_out) == 0
    dt5_out <- type1 == "Metal" && type2 == "Non-metal"
    dt6_out <- type1 == "Non-metal" && type2 == "Metal"
    dt7_out <- type1 == "Metal" && type2 == "Metal"
    dt8_out <- type1 == "Non-metal" && type2 == "Non-metal"
    dt9_out <- dt4_out == T && dt7_out == F && dt8_out == F
    dt10_out <- !dt9_out


    # Initialization
    decision <- NA

    # Details
    if(dt9_out) decision <- 1 else       # Normal - case 1
      if(dt10_out) decision <- 2 else    decision <- 3     # SandBox!    # Non-Normal - case 3


    ###
  } # End Part 1: Detection
  #################################################################

  # Part 2: Return
  return(decision)


} # End InternalControl_Oxide()




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
# input_oxosal_resolution <- OxosaltEquation(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                           input_atomic_number2 = input_atomic_number2,
#                                          input_valence2 = input_valence2,
#                                          input_internal_language = "en",
#                                          input_external_language = input_external_language,
#                                          input_PeriodicTable = input_PeriodicTable)
# 
# 
# input_oxosal_resolution