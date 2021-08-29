


# Resolution 03: Oxacid
OxacidEquation <- function(input_atomic_number1 = NULL, input_valence1 = NULL,
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
    
    
    input_oxide_resolution <- OxideEquation(input_atomic_number1 = input_atomic_number1,
                                            input_valence1 = input_valence1,
                                            input_internal_language = "en",
                                            input_external_language = input_external_language,
                                            input_PeriodicTable = input_PeriodicTable)
    
    
    part_oxide <- input_oxide_resolution[nrow(input_oxide_resolution),c(9:12)]
    
    # Internal Control 
    internal_control <- InternalControl_Oxacid(input_atomic_number1 = input_atomic_number1,
                                                  input_valence1 = input_valence1,
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
      # The "matrix_solution" has 8 rowns y 12 columns.
      # Each row is a step on the stechyometry resolution (8 steps for oxides = 8 rowns).
      # Each column is a ecuation detail about each step.
      #
      # The "matrix_solution" has the next columns for oxides:
      #  1) Step Order
      #  2) coefficient of the oxide in reactants
      #  3) Abbreviation of the element from the oxide in reactants
      #  4) Sub-index of the element from the oxide in reactants
      #  5) Abbreviation of the Oxygen from the oxide in reactants
      #  6) Sub-index of the Oxygen from the oxide in reactants
      
      #  7) Coefficient of the water in reagents
      #  8) Abbreviation of the Hydrogen from the water in reactants
      #  9) Sub-index of the Hydrogen from the water in reactants
      # 10) Abbreviation of the Oxygen from the water in reactants
      # 11) Sub-index of the Oxygen from the water in reactants
      
      # 12) Coefficient of the hidracid
      # 13) Abbreviation of the Hydrogen from the hidroxide in products
      # 14) Sub-index of the Hydrogen from the hidroxide in products
      # 15) Abbreviation of the Element from the hidroxide in products
      # 16) Sub-index of the Element from the hidroxide in products
      # 17) Abbreviation of the Oxygen from the hidroxide in products
      # 18) Sub-index of the Oxygen from the hidroxide in products
      ###
    } # End 1.0 Explanation
    ########################################################
    
    
    # 1.1) The creation for "matrix_solution"
    {
      ###
      
      # Total Steps for Hidroxide Resolution (TSFHR)
      TSFHR <- 8
      
      my_names <- c("Order",
                    "Coef1", "ElementReactive", "Sub1", "OxygenReactive",  "Sub2",
                    "Coef2", "HydrogenWater",  "Sub3", "OxygenWater",  "Sub4",
                    "Coef3", "HydrogenProduct", "Sub5", "ElementProduct", "Sub6", "OxygenProduct", "Sub7")
      
      
      matrix_solution <- data.frame(matrix(NA, TSFHR, length(my_names)))
      colnames(matrix_solution) <- my_names
      ###
    } # End The creation for "matrix_solution"
    ########################################################
    
    
    ###
  } # End Part 1
  ##################################################
  
  # Part 2: Normal Stoichiometry Resolution for Oxacid (Case 1 of 5)
  {
    ###
    
    
    # 2.0) Explanation about Part 2 (if you want to read)
    {
      ###
      
      # In the case of Oxacid ...
      # The Stoichiometry is carried out as follows:
      #
      # 1) Presentation for the Reatives
      # The first element is placed the oxide and then the water separated both
      # with a "+" sign, both being reactive.
      # Oxide and water with subscripts.
      # ---- In computing, coefficients equal to 1 ----
      # ---- and only in reagents. Nothing in products. ----
      # To the right of the water an arrow called "Reaction Arrow" is placed.
      # After the reaction arrow the products will be detailed.
      #
      #
      #
      # 2) Presentation for the product: Oxacid
      # To the right of the arrow, the reaction product will be detailed
      # which in this case is an Oxacid. The structure of the Hydracid is the
      # Hydrogen, Element and Oxygen togethers (without space between them) indicating
      # that are part of the same molecule. In principle the sub-index for each one
      # is 1 in product.
      #
      #
      #
      # 3) Details for Hydracid composition - Sub-index for Hydrogen
      # If valence of the element is inpar, sub-index for Hydrogen is 1.
      # If valence of the element is par, sub-index for Hydrogen is 2.
      #
      # 4) Details for Hydracid composition - Sub-index for Oxygen
      # The sub-index for Oxygen must balance the sign loads in the molecule
      # of Hydracid. We must remenber, de valence of Oxygen es 2.
      #
      #
      # 5) Balance of the Hydrogen
      # We proceed to balance the  Hydrogen.
      # The result of this operation is a posible new coefficient for the
      # Hydracid in products.
      #
      # 6) Balance of the Element
      # The balance was ready in the before step. We must confirm that.
      #
      # 7) Balance of the Oxygen
      # The balance was ready in the before step 5. We must confirm that.
      #
      # 8) Coefficient Simplification
      # If possible, simplify all coefficients by the
      # same least common divisor (gcd).
      
      
      
      
      ###
    } # End: 2.0) Explanation about Part 2 (if you want to read)
    #########################################################
    
    
    
    
    # 2.1) Stoichiometric Algorithm for Oxacid
    if (internal_control == 1){ 
      
      # We plant a "zero" seed with stoichiometric steps.
      general_internal_step <- 0
      
      
      # Step 1 of 8 - Oxacid
      {
        ###
        
        # 1) Presentation for the Reatives
        # The first element is placed the oxide and then the water separated both
        # with a "+" sign, both being reactive.
        # Oxide and water with subscripts.
        # ---- In computing, coefficients equal to 1 ----
        # ---- and only in reagents. Nothing in products. ----
        # To the right of the water an arrow called "Reaction Arrow" is placed.
        # After the reaction arrow the products will be detailed.
        #########################################################################
        
        # One more step...
        general_internal_step <-general_internal_step + 1
        
        # Creation of step 1 ...
        gate1 <- rep(NA, ncol(matrix_solution))
        names(gate1) <- colnames(matrix_solution)
        
        # Implementation for the gate1...
        gate1["Order"] <- general_internal_step
        
        # The Oxide
        gate1["Coef1"] <- 1
        gate1["ElementReactive"] <- part_oxide[1]
        gate1["Sub1"] <- part_oxide[2]
        gate1["OxygenReactive"] <- part_oxide[3]
        gate1["Sub2"] <-part_oxide[4]
        
        # The Water
        gate1["Coef2"] <- 1
        gate1["HydrogenWater"] <- "H"
        gate1["Sub3"] <- "2"
        gate1["OxygenWater"] <- "O"
        gate1["Sub4"] <- 1
        
        # The Hydracid
        gate1["Coef3"] <- ""
        gate1["HydrogenProduct"] <- ""
        gate1["Sub5"] <- ""
        gate1["ElementProduct"] <- ""
        gate1["Sub6"] <- ""
        gate1["OxygenProduct"] <- ""
        gate1["Sub7"] <- ""
        
        
        
        
        # Load gate1 on matrix_solution
        matrix_solution[general_internal_step, ] <- gate1
        
        ###
      } # End Step 1 of 8 - Oxacid
      ####################################################################
      
      
      
      # Step 2 of 8 - Oxacid
      {
        ###
        
        # 2) Presentation for the product: Hydracid
        # To the right of the arrow, the reaction product will be detailed
        # which in this case is an Hydracid. The structure of the Hydracid is the
        # Hydrogen, Element and Oxygen togethers (without space between them) indicating
        # that are part of the same molecule. In principle the sub-index for each one
        # is 1 in product.
        ##############################################################################
        
        
        # One more step...
        general_internal_step <-general_internal_step + 1
        
        
        # Creation of the gate2 (We take values before)
        gate2 <- gate1
        
        # Implementation of gate2 modifications...
        # The Hydracid
        gate2["Coef3"] <- 1
        gate2["HydrogenProduct"] <- "H"
        gate2["Sub5"] <- 1
        gate2["ElementProduct"] <- symbol1
        gate2["Sub6"] <- 1
        gate2["OxygenProduct"] <- "O"
        gate2["Sub7"] <- 1
        
        
        
        # Load the gate2
        matrix_solution[general_internal_step, ] <- gate2
        
        
        
        ###
      } # End Step  2 of 8 - Oxacid
      ####################################################################
      
      
      # Step  3 of 8 - Oxacid
      {
        ###
        
        # 3) Details for Hydracid composition - Sub-index for Hydrogen
        # If valence of the element is inpar, sub-index for Hydrogen is 1.
        # If valence of the element is par, sub-index for Hydrogen is 2.
        #####################################################################################
        
        
        # One more step...
        general_internal_step <-general_internal_step + 1
        
        # Creation of the gate3 (We take values before)
        gate3 <- gate2
        
        dt_par <- valence1/2 == valence1%/%2
        if(dt_par) sub_ind5 <- 2 else sub_ind5 <- 1
        
        
        # Implementation of gate3 modifications...
        gate3["Order"] <- general_internal_step
        gate3["Sub5"] <- sub_ind5
        
        
        
        # Load the gate3
        matrix_solution[general_internal_step, ] <- gate3
        
        # Remove objects...
        remove(sub_ind5)
        
        ###
      } # End Step  3 of 8 - Oxacid
      ####################################################################
      
      
      # Step 4 of 8 - Oxacid
      {
        ###
        
        # 4) Details for Hydracid composition - Sub-index for Oxygen
        # The sub-index for Oxygen must balance the sign loads in the molecule
        # of Hydracid. We must remember, de valence of Oxygen es 2.
        ###################################################################
        
        # One step more...
        general_internal_step <- general_internal_step + 1
        
        # We retrieve the subscripts from step 3 ...
        sub_ind5 <- as.numeric(as.character(gate3["Sub5"]))
        sub_ind7 <- (sub_ind5 + valence1)/2
        
        # Gate 4 creation
        gate4 <- gate3
        
        # Implementation of changes in Gate 5
        gate4["Order"] <- general_internal_step
        gate4["Sub7"] <- sub_ind7
        
        
        # Load of the gate 5
        matrix_solution[general_internal_step, ] <- gate4
        
        # We eliminate what is no longer needed ...
        remove(sub_ind5, sub_ind7)
        
        ###
      } # End Step 4 of 8 - Oxacid
      ##################################################################################################################
      
      
      # Step 5 of 8 - Oxacid
      {
        ###
        
        # 5) Balance of the Hydrogen
        # We proceed to balance the  Hydrogen.
        # The result of this operation is a possible new coefficient for the
        # Hydracid in products.
        ###################################################################################
        
        
        # One step more...
        general_internal_step <- general_internal_step + 1
        
        # Creation of the gate5...
        gate5 <- gate4
        
        # Implkementation of changes in the general_internal_step
        gate5["Order"] <- general_internal_step
        
        # We retrieve subscripts and coefficient of gate4 for the Hydrogen...
        
        coef2 <- as.numeric(as.character(gate4["Coef2"]))
        sub_ind3 <- as.numeric(as.character(gate4["Sub3"]))
        
        coef3 <- as.numeric(as.character(gate4["Coef3"]))
        sub_ind5 <- as.numeric(as.character(gate4["Sub5"]))
        
        
        
        # We calculate the amount of the Hydrogen in reactants and in products
        amount_hydrogen_reactives <- coef2*sub_ind3
        amount_hydrogen_products <- coef3*sub_ind5
        
        # We calculate the new coefficient for the hydracid.
        # This will be the new coefficient for the Hydracid
        mod_coef3 <- amount_hydrogen_reactives / amount_hydrogen_products
        
        
        # If the quantities of the hydrogen are equal, the gate5 is equal to gate4
        # and that is already armed by default when the gate5 is created.
        # Now ... if the amounts of Hydrogen are different, you have to implement
        # the corresponding changes.
        
        # We implement the modifications of gate5 ...
        gate5["Coef3"] <- mod_coef3
        
        
        # Load gate5
        matrix_solution[general_internal_step, ] <- gate5
        
        # Remove objects...
        remove(coef2, sub_ind3, coef3, sub_ind5,
               amount_hydrogen_reactives, amount_hydrogen_products,
               mod_coef3)
        
        ###
      } # End Step 5 of 8 - Oxacid
      ##################################################################################################################
      
      
      # Step 6 of 8 - Oxacid
      {
        ###
        
        # 6) Balance of the Element
        # The balance was ready in the before step. We must confirm that.
        #####################################################################
        
        
        # One step more...
        general_internal_step <-general_internal_step + 1
        
        # Creation of the gate6
        gate6 <- gate5
        
        # Implementation of de general_internal_step changes
        gate6["Order"] <- general_internal_step
        
        
        
        
        # Load the gate8
        matrix_solution[general_internal_step, ] <- gate6
        
        
        ###
      } # End Step 6 of 8 - Oxacid
      ##################################################################################################################
      
      
      # Step 7 of 8 - Oxacid
      {
        ###
        
        # 7) Balance of the Oxygen
        # The balance was ready in the before step 5. We must confirm that.
        #####################################################################
        
        
        # One step more...
        general_internal_step <-general_internal_step + 1
        
        # Creation of the gate6
        gate7 <- gate6
        
        # Implementation of de general_internal_step changes
        gate6["Order"] <- general_internal_step
        
        
        
        
        # Load the gate7
        matrix_solution[general_internal_step, ] <- gate7
        
        
        ###
      } # End Step 7 of 8 - Oxacid
      ##################################################################################################################
      
      
      # Step 8 of 8 - Oxacid
      {
        ###
        
        # 8) Coefficient Simplification
        # If possible, simplify all coefficients by the
        # same least common divisor (gcd).
        ################################################################
        
        
        # One step more...
        general_internal_step <- general_internal_step + 1
        
        # Creation of the gate8
        gate8 <- gate7
        
        # Implementation of de general_internal_step changes
        gate8["Order"] <- general_internal_step
        
        # We retrieve the three coefficients of gate7 ...
        coef1 <- as.numeric(as.character(gate7["Coef1"]))
        coef2 <- as.numeric(as.character(gate7["Coef2"]))
        coef3 <- as.numeric(as.character(gate7["Coef3"]))
        
        
        # We calculate the least common divisor.
        # I found a library that does it directly!
        library(numbers)
        mcd <- mGCD(c(coef1, coef2, coef3))
        
        # We create the new subscripts in the oxide ...
        mod_coef1 <- coef1/mcd
        mod_coef2 <- coef2/mcd
        mod_coef3 <- coef3/mcd
        
        
        # If the least common divisor is equal to 1, do not do
        # no change, because it is like this by default.
        # But if the least common divisor is different from 1, you have to
        # implement changes ...
        
        # We implement the modifications of gate8 ...
        gate8["Coef1"] <- mod_coef1
        gate8["Coef2"] <- mod_coef2
        gate8["Coef3"] <- mod_coef3
        
        
        
        
        
        
        
        # Load the gate8
        matrix_solution[general_internal_step, ] <- gate8
        
        # We eliminate what is no longer needed ...
        remove(coef1, coef2, coef3, mcd, mod_coef1, mod_coef2, mod_coef3)
        
        ###
      } # End Step 8 of 8 - Oxacid
      ##################################################################################################################
      
      ###    
    } # End Stoichiometric Algorithm for Oxacid
    
    
  } # End Part 2: Normal Stoichiometry Resolution for Oxacid (Case 1 of 5)
  #########################################################
  
  # Part 3: Non-normal Stoichiometry Resolution for Oxacid (Case 2 of 5)
  {
    ###
    # 3.1) Noble gas selected
    if (internal_control == 2){
      
      matrix_noble_gas <- matrix("Oxacid - Noble gas Selected - No equation", nrow(matrix_solution), ncol(matrix_solution))    
      colnames(matrix_noble_gas) <- colnames(matrix_solution)
      matrix_noble_gas[,1] <- c(1:nrow(matrix_noble_gas))
      matrix_solution <- matrix_noble_gas
      
    }
    ###  
  } # End Part 3: Non-normal Stoichiometry Resolution for Oxacid (Case 2 of 5)
  #####################################################################################
  
  # Part 4: Non-normal Stoichiometry Resolution for Oxacid (Case 3 of 5)
  {
    ###
    # 4.1) Metal selected
    if (internal_control == 3){
      
      matrix_metal <- matrix("Oxacid - Metal Selected - No equation", nrow(matrix_solution), ncol(matrix_solution))    
      colnames(matrix_metal) <- colnames(matrix_solution)
      matrix_metal[,1] <- c(1:nrow(matrix_metal))
      matrix_solution <- matrix_metal
      
    }
    ###  
  } # End Part 4: Non-normal Stoichiometry Resolution for Oxacid (Case 3 of 5)
  #####################################################################################
  
  # Part 5: Non-normal Stoichiometry Resolution for Oxacid (Case 4 of 5)
  {
    ###
    # 4.1) Oxygen selected
    # if (internal_control == 4){
    #   
    #   matrix_oxygen <- matrix("Oxacid - Oxygen Selected - No equation", nrow(matrix_solution), ncol(matrix_solution))    
    #   colnames(matrix_oxygen) <- colnames(matrix_solution)
    #   matrix_oxygen[,1] <- c(1:nrow(matrix_oxygen))
    #   matrix_solution <- matrix_oxygen
    #   
    # }
    
    # When selected element is the Hydrogen
    if (internal_control == 4) {
      
      
      # We plant a "zero" seed with stoichiometric steps.
      general_internal_step <- 0
      
      
      
      # We plant a "zero" seed with stoichiometric steps.
      general_internal_step <- 0
      
      
      # Step 1 of 8 - Oxacid
      {
        ###
        
        # 1) Presentation 
        
        # Creation of step 1 ...
        gate1 <- rep(NA, ncol(matrix_solution))
        names(gate1) <- colnames(matrix_solution)
        
        # Implementation for the gate1...
        gate1["Order"] <- 1
        
        # The Oxide
        gate1["Coef1"] <- 1
        gate1["ElementReactive"] <- part_oxide[1]
        gate1["Sub1"] <- part_oxide[2]
        gate1["OxygenReactive"] <- part_oxide[3]
        gate1["Sub2"] <-part_oxide[4]
        
        # The Water
        gate1["Coef2"] <- 1
        gate1["HydrogenWater"] <- "H"
        gate1["Sub3"] <- "2"
        gate1["OxygenWater"] <- "O"
        gate1["Sub4"] <- 1
        
        # The Hydracid
        gate1["Coef3"] <- "2"
        gate1["HydrogenProduct"] <- "H"
        gate1["Sub5"] <- "2"
        gate1["ElementProduct"] <- "O"
        gate1["Sub6"] <- "1"
        gate1["OxygenProduct"] <- ""
        gate1["Sub7"] <- ""
        
        
        
        
        # Load gate1 on matrix_solution
        for (k in 1: nrow(matrix_solution)) matrix_solution[k, ] <- gate1
        matrix_solution[,1] <- c(1:nrow(matrix_solution))
        
        
        ###
      } # End Step 1 of 8 - Oxacid
      ####################################################################
      
      
   
      
      
      
     
      
      ###
    } # End: When selected element is the Oxygen
    ##################################################################################
    ###  
  } # End Part 5: Non-normal Stoichiometry Resolution for Oxacid (Case 4 of 5)
  #####################################################################################
  
  # Part 6: SandBox (Case 5 of 5)
  {
    ###
    # SandBox!
    if (internal_control == 5) {
      matrix_sandbox <- matrix("Oxacid - SandBox", nrow(matrix_solution), ncol(matrix_solution))    
      colnames(matrix_sandbox) <- colnames(matrix_solution)
      matrix_sandbox[,1] <- c(1:nrow(matrix_sandbox))
      matrix_solution <- matrix_sandbox
    } # End SandBox!
    ###  
  } # End Part 5: SandBox (Case 5 of 5)
  ##################################################################################
  
  
  
  # Part 7: Return
  {
    ###
    
    
    return(matrix_solution)
    
    
    ###  
  } # End Part 5: Return
  ############################################


} # End Function***


InternalControl_Oxacid <- function(input_atomic_number1 = NULL, 
                                      input_valence1 = NULL, 
                                      input_internal_language = "en",
                                      input_external_language = NULL,
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
    
    
    
    
    ###  
  } # End Part 0: Necessary Participants
  ####################################################
  
  # Part 1: Detection
  {
    ###
    
    # Special detection
    dt1_out <- (type1 == "Non-metal" | type1 == "Metalloid") && symbol1 != "H"
    dt2_out <- type1 == "Noble gas"
    dt3_out <- type1 == "Metal"
    dt4_out <- symbol1 == "H"
    dt5_out <- sum(dt1_out, dt2_out, dt3_out, dt4_out) == 0 # SandBox!
    
    # Initialization
    decision <- NA
    
    # Details
    if(dt1_out) decision <- 1 else     # Normal - case 1
      if(dt2_out) decision <- 2 else   # Non-Normal - case 2
        if(dt3_out) decision <- 3 else # Non-Normal - case 3
          if(dt4_out)  decision <- 4  else # Non-Normal -  Oxygen!  - Case 4
            if(dt5_out)  decision <- 5 # SandBox!
    ###  
  } # End Part 1: Detection
  #################################################################
  
  # Part 2: Return    
  return(decision)
  
  
} # End InternalControl_Hidroxide()



# input_atomic_number1 <- 7
# input_valence1 <- 5
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_GeneralNomenclature01 <-  GeneralNomenclature01
# input_GeneralNomenclature02 <-  GeneralNomenclature02
# input_GeneralNomenclature03 <-  GeneralNomenclature03
# 
# input_oxacid_resolution <- OxacidEquation(input_atomic_number1 = input_atomic_number1,
#                input_valence1 = input_valence1,
#                input_internal_language = "en",
#                input_external_language = input_external_language,
#                input_PeriodicTable = input_PeriodicTable)
# 
# 
# input_oxacid_resolution