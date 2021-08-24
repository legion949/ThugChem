


# Resolution 04: Hydride
HydrideEquation <- function(input_atomic_number1 = NULL, input_valence1 = NULL, 
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
    
    ###  
  } # End Part 0: Necessary Participants
  ####################################################
  
  
        # Part 1: Structure of the "matrix_solution"
        {
        ###
          
          # 1.0) Explanation about Part 1 (if you want to read)
          {
            ###
            # The "matrix_solution" has 7 rows y 12 columns.
            # Each row is a step on the stechyometry resolution (7 steps for Hydrides = 7 rows).
            # Each column is a equation detail about each step.
            #
            # The "matrix_solution" has the next columns for Hydrides:
            # 1) Step Order
            # 2) coefficient of the element in reactants
            # 3) Abbreviation of the element in reagents
            # 4) Sub-index of the element in reactants
            # 5) Coefficient of the Hydrogen in reagents
            # 6) Chemical symbol of Hydrogen in reagents
            # 7) Hydrogen sub-index in reagents
            # 8) Coefficient of the Hydride in products
            # 9) Chemical symbol of the element in products
            # 10) Sub-index of the element in the oxide in products
            # 11) Chemical symbol of the Hydrogen in products
            # 12) Sub-index of the Hydrogen in the oxide in products
            ###
          } # End 1.0 Explanation
          ########################################################
          
          
          # 1.1) The creation for "matrix_solution"
          {
          ###
            
            # Total Steps for Hydride Resolution (TSFHR)
            TSFHR <- 7
            
            my_names <- c("Order", 
                             "Coef1", "ElementReactive", "Sub1",    
                             "Coef2", "HydrogenReactive",  "Sub2",     
                             "Coef3", "HydrogenProduct", "Sub3", "ElementProduct", "Sub4")
            
            
            matrix_solution <- data.frame(matrix(NA, TSFHR, length(my_names)))
            colnames(matrix_solution) <- my_names
          ###  
          } # End The creation for "matrix_solution"
          ########################################################
          
          
          ###
        } # End Part 1: Structure of the "matrix_solution"
        ############################################################
        
        
        
        # Part 2: Stoichiometry Resolution for Hydrides
        {
        ###
          
          
          # 2.0) Explanation about Part 2 (if you want to read)
          {
            ###
            
            # In the case of Hydrides ...
            # The Stoichiometry is carried out as follows:
            #
            # 1) Presentation for the Reactives
            # The first element is placed and then the Hydrogen separated both
            # with a "+" sign, both being reactive.
            # No subscripts or coefficients added, yet.
            # ---- In computing, coefficients and subscripts equal to 1 ----
            # ---- and only in reagents. Nothing in products. ----
            # To the right of the Hydrogen an arrow called "Reaction Arrow" is placed.
            # After the reaction arrow the products will be detailed.
            #     
            #
            #  
            # 2) Reactives Details
            # The Hydrogen in the reactants has a subscript 2 because it is a gas.
            # If the element that will react with hydrogen is also a gas
            # and it is not a Noble gas, it will also have a two as a subscript.
            #     
            #
            #  
            # 3) Presentation for the product: Hydride
            # To the right of the arrow, the reaction product will be detailed
            # which in this case is an Hydride. The structure of the Hydride is the
            # hydrogen and the element together (without space between them) indicating
            # that are part of the same molecule. In principle the subindex
            # of hydrogen and element in products will be 1 in both cases.
            # 
            #
            # 4) Details on the Hydride: sub-indexes
            # The subscripts in the Hydride are changed.
            # In Hydride ... the sub-index of the element will be the valence of Hydrogen (is 1).
            # In Hydride ... the Hydrogen subscript will be the valence of the Element.
            #
            #
            #  
            # 5) Balance of the Hydrogen
            # We proceed to balance the equation for the Hydrogen.
            # - Subindex of the Hydrogen of Hydride in products will be the coefficient
            #   of the Hydrogen in reactives.
            # - Subindex of the Hydrogen in reactives will be the coefficient
            #   of the Hydride in ´´products.
            # 
            # 6) Balance of the Element
            # We proceed to balance the equation for the Element.
            # - Counting the element in reactives and product we must modify the coefficient
            #   of the element in reactives.
            #
            # 7) Coefficient Simplification
            # If possible, simplify all coefficients by the
            # same least common divisor (gcd).
          
            
            
            
            ###  
          } # End: 2.0) Explanation about Part 2 (if you want to read)
          #########################################################
          
          
          
          
          # 2.1) Stoichiometry Algorithm for hydride
          # If the element is not a Noble gas ... and it is not hydrogen
          if (internal_control == 1) {
          
          
          
            ###
            
            
            # We plant a "zero" seed with stoichiometric steps.
           general_internal_step <- 0
            
            
            # Step 1 of 7 - Hydride
            {
              ###
              
              # 1) Presentation for the Reactives
              # The first element is placed and then the Hydrogen separated both
              # with a "+" sign, both being reactive.
              # No subscripts or coefficients added, yet.
              # ---- In computing, coefficients and subscripts equal to 1 ----
              # ---- and only in reagents. Nothing in products. ----
              # To the right of the Hydrogen an arrow called "Reaction Arrow" is placed.
              # After the reaction arrow the products will be detailed.
              ##################################################################
              
              # One more step...
             general_internal_step <-general_internal_step + 1
              
              # Creation of step 1 ...
              gate1 <- rep(NA, ncol(matrix_solution))
              names(gate1) <- colnames(matrix_solution)
              
              # Implementation of the  gate1...
              gate1["Order"] <- general_internal_step
              gate1["Coef1"] <- 1
              gate1["ElementReactive"] <- symbol1
              gate1["Sub1"] <- 1
              gate1["Coef2"] <- 1
              gate1["HydrogenReactive"] <- "H"
              gate1["Sub2"] <- 1
              gate1["Coef3"] <- ""
              gate1["HydrogenProduct"] <- ""
              gate1["Sub3"] <- ""
              gate1["ElementProduct"] <- ""
              gate1["Sub4"] <- ""

              
              # Load gate1 on matrix_solution  
              matrix_solution[general_internal_step, ] <- gate1
              
              ###
            } # End Step 1 of 7 - Hydride
            ####################################################################
            
            
            # Step 2 of 7 - Hydride
            {
              ###
              
              # 2) Reactives Details
              # The Hydrogen in the reactants has a subscript 2 because it is a gas.
              # If the element that will react with hydrogen is also a gas
              # and it is not a Noble gas, it will also have a two as a subscript.
              #####################################################################
              
              
              # One more step...
             general_internal_step <-general_internal_step + 1
              

              # Subscript of element in reactants
              sub_ind1 <- 1   # Default value is 1
              # If is gas and not a Noble gas...  the value change to 2...
              if (state1 == "Gas" && type1 != "Noble gas") sub_ind1 <- 2
              
              # Subscript of Oxygen in reactants
              sub_ind2 <- 2   # Subindex is 2 becouse Hydrogen is a gas
              

              # Creation of the gate2 (We take values before)
              gate2 <- gate1
              
              
              # Implementation of gate2 modifications...
              gate2["Order"] <- general_internal_step
              gate2["Sub1"] <- sub_ind1
              gate2["Sub2"] <- sub_ind2
              
              
              # Load the gate2  
              matrix_solution[general_internal_step, ] <- gate2
              
              # We eliminate what is no longer needed ...
              remove(sub_ind1, sub_ind2)
              
              ###
            } # End Step 2 of 7 - Hydride
            ####################################################################
            
            
            # Step 3 of 7 - Hydride
            {
              ###
              
              # 3) Presentation for the product: Hydride
              # To the right of the arrow, the reaction product will be detailed
              # which in this case is an Hydride. The structure of the Hydride is the
              # hydrogen and the element together (without space between them) indicating
              # that are part of the same molecule. In principle the subindex
              # of hydrogen and element in products will be 1 in both cases.
              #######################################################################
              
              
              # One more step...
             general_internal_step <-general_internal_step + 1
              
             # Details about participants in gate3...
             hydrogen_prod <- "H"
             elemento_prod <- symbol1
             
             
             # Initial coefficiente for the hydride
             coef3 <- 1    # Default value is 1
             
             # Sub-index by default in the hydride
             sub_ind3 <- 1   # Default value is 1
             sub_ind4 <- 1   # Default value is 1
             
             # Creation of the gate3 (We take values before)
             gate3 <- gate2
              
             # Implementation of gate3 modifications...
              gate3["Order"] <- general_internal_step
              gate3["Coef3"] <- coef3
              gate3["HydrogenProduct"] <- hydrogen_prod
              gate3["Sub3"] <- sub_ind3
              gate3["ElementProduct"] <- elemento_prod
              gate3["Sub4"] <- sub_ind4
              
              
              # Load gate3  
              matrix_solution[general_internal_step, ] <- gate3
              
              # Remove objects...
              remove(coef3, elemento_prod, sub_ind3, hydrogen_prod, sub_ind4)
              
              ###
            } # End Step  3 of 7 - Hydride
            ####################################################################
            
            
            # Step  4 of 7 - Hydride
            {
              ###
              
              # 4) Details on the Hydride: sub-indexes
              # The subscripts in the Hydride are changed.
              # In Hydride ... the sub-index of the element will be the valence of Hydrogen (is 1).
              # In Hydride ... the Hydrogen subscript will be the valence of the Element.
              
              # One more step...
             general_internal_step <-general_internal_step + 1
              
             # Sub-indexes of the Element and Hydrogen in products ...
             sub_ind3 <- valence1    # Here is the valence of the element
             sub_ind4 <- 1   # Here is the hydrogen valence
             
             
              # Creation of the gate4
              gate4 <- gate3
              
              # Implementation of gate4 modifications...
              gate4["Order"] <- general_internal_step
              gate4["Sub3"] <- sub_ind3
              gate4["Sub4"] <- sub_ind4
              
              
              # Load gate4  
              matrix_solution[general_internal_step, ] <- gate4
              
              # Remove objects...
              remove(sub_ind3, sub_ind4)
              
              ###
            } # End Step  4 of 7 - Hydride
            ####################################################################
            
            
            # Step 5 of 7 - Hydride
            {
              ###
              
              # 5) Balance of the Hydrogen
              # We proceed to balance the equation for the Hydrogen.
              # - Subindex of the Hydrogen of Hydride in products will be the coefficient
              #   of the Hydrogen in reactives.
              # - Subindex of the Hydrogen in reactives will be the coefficient
              #   of the Hydride in ´´products.
              #####################################################################
              
              # One step more...
              general_internal_step <-general_internal_step + 1
              
              # Gate 5 creation
              gate5 <- gate4
              
              # We retrieve subscripts and coefficient of gate4 for the Hydrogen
              sub_ind2 <- as.numeric(as.character(gate4["Sub2"]))
              sub_ind3 <- as.numeric(as.character(gate4["Sub3"]))
              
              
              # Implementation of changes in Gate 5
              gate5["Order"] <- general_internal_step
              gate5["Coef2"] <- sub_ind3
              gate5["Coef3"] <- sub_ind2
              
              # Load of the gate 5  
              matrix_solution[general_internal_step, ] <- gate5
              
              # We eliminate what is no longer needed ...
              remove(sub_ind2, sub_ind3)
              
              ###
            } # End Step 5 of 7 - Hydride
            ##################################################################################################################
            
            
            # Step 6 of 7 - Hydride
            {
              ###
              
              # 6) Balance of the Element
              # We proceed to balance the equation for the Element.
              # - Counting the element in reactives and product we must modify the coefficient
              #   of the element in reactives.
              ############################################################################
              
              
              
              # One step more...
             general_internal_step <-general_internal_step + 1
              
             # Gate 6 creation
             gate6 <- gate5
              
             # Implementation of changes in Gate 6
             gate6["Order"] <- general_internal_step 
             
             # We recover subscripts and coefficient of gate5 for oxygen and so on
             # balance oxygen globally ...
              coef1 <- as.numeric(as.character(gate5["Coef1"]))
              sub_ind1 <- as.numeric(as.character(gate5["Sub1"]))
              coef3 <- as.numeric(as.character(gate5["Coef3"]))
              sub_ind4 <- as.numeric(as.character(gate5["Sub4"]))
              
              
              # We calculate the amount of oxygen, in reactants and in products
              counting_element_reactives <- coef1*sub_ind1
              counting_element_products <- coef3*sub_ind4
          
              mod_coef1 <-    counting_element_products / counting_element_reactives
              
                # We implement the gate6 modifications ...
                gate6["Coef1"] <- mod_coef1

                # We eliminate what is no longer needed from this internal step ...
                remove(coef1, coef3, sub_ind1, sub_ind4, counting_element_reactives,
                       counting_element_products, mod_coef1)
              
              
              # Load the gate6
              matrix_solution[general_internal_step, ] <- gate6
              
            ###
            } # End Step 6 of 7 - Hydride
            ##################################################################################################################
            
            
             
            # Step 7 of 7 - Hydride
            {
              ###
              
              # 7) Coefficient Simplification
              # If possible, simplify all coefficients by the
              # same least common divisor (gcd).
              #########################################################################
              
              # One step more...
             general_internal_step <-general_internal_step + 1
              
              # Creation of the gate7
              gate7 <- gate6
              
              # Implementation of de general_internal_step changes
              gate7["Order"] <- general_internal_step
              
              # We retrieve the three coefficients of gate6 ...
              coef1 <- as.numeric(as.character(gate6["Coef1"]))
              coef2 <- as.numeric(as.character(gate6["Coef2"]))
              coef3 <- as.numeric(as.character(gate6["Coef3"]))
              
              
              # We calculate the least common divisor.
              # I found a library that does it directly!
              library(numbers)
              mcd <- mGCD(c(coef1, coef2, coef3))
              
              # We create the new subscripts in the oxide ...
              mod_coef1 <- coef1/mcd
              mod_coef2 <- coef2/mcd
              mod_coef3 <- coef3/mcd
              
              
                # We implement the modifications of gate7 ...
                gate7["Coef1"] <- mod_coef1
                gate7["Coef2"] <- mod_coef2
                gate7["Coef3"] <- mod_coef3
                
                
              # Load the gate7  
              matrix_solution[general_internal_step, ] <- gate7
              
              # We eliminate what is no longer needed ...
              remove(coef1, coef2, coef3, mcd, mod_coef1, mod_coef2, mod_coef3)
              
              ###
            } # End Step 7 of 7 - Hydride
            ##################################################################################################################
            
          
            
             
                
                
              
              ###  
            } # End Part 2: Stoichiometry Resolution for Hydrides
        ############################################################
        
       
        
      
      
      
      
      ###    
    } # End Hydride Resolution
        ###################################################################
    
 
  # Part 3: Non-Normal Stoichiometry Resolution for Hydride (Case 2 of 3)
  {
    ###
    
    # When selected element is the Hydrogen
    if (internal_control == 2) {
      
      
      # We plant a "zero" seed with stoichiometric steps.
      general_internal_step <- 0
      

        
        # Creation of step 1 ...
        gate1 <- rep(NA, ncol(matrix_solution))
        names(gate1) <- colnames(matrix_solution)
        
        # Implementation of the  gate1...
        gate1["Order"] <- general_internal_step
        gate1["Coef1"] <- 1
        gate1["ElementReactive"] <- symbol1
        gate1["Sub1"] <- 2
        gate1["Coef2"] <- 1
        gate1["HydrogenReactive"] <- "H"
        gate1["Sub2"] <- 2
        gate1["Coef3"] <- "2"
        gate1["HydrogenProduct"] <- "H"
        gate1["Sub3"] <- "2"
        gate1["ElementProduct"] <- ""
        gate1["Sub4"] <- ""
        
        
        # Load gate1 on matrix_solution 
        for (k in 1:nrow(matrix_solution)) matrix_solution[k, ] <- gate1
        matrix_solution[,1] <- c(1:nrow(matrix_solution))
        
        ###
      } # End When selected element is the Hydrogen
      ####################################################################
      
      
     
     
      
      ###
    } # End Part 3: Non-Normal Stoichiometry Resolution for Oxides (Case 2 of 3)
  ####################################################################################   
  
  
  
  # Part 4: Non-Normal Stoichiometry Resolution for Hydride (Case 3 of 3)
  {
    ###
    
    # When selected element is the Noble Gas
    if (internal_control == 3) {
      
      
      # We plant a "zero" seed with stoichiometric steps.
      general_internal_step <- 0
      
      
      
      # Creation of step 1 ...
      general_internal_step <- general_internal_step + 1
      gate1 <- rep(NA, ncol(matrix_solution))
      names(gate1) <- colnames(matrix_solution)
      
      # Implementation of the  gate1...
      gate1["Order"] <- general_internal_step
      gate1["Coef1"] <- 1
      gate1["ElementReactive"] <- symbol1
      gate1["Sub1"] <- 1
      gate1["Coef2"] <- 1
      gate1["HydrogenReactive"] <- "H"
      gate1["Sub2"] <- 1
      gate1["Coef3"] <- 1
      gate1["HydrogenProduct"] <- "H"
      gate1["Sub3"] <- 2
      gate1["ElementProduct"] <- symbol1
      gate1["Sub4"] <- ""
      
      
      # Load gate1 on matrix_solution 
      for (k in 1:nrow(matrix_solution)) matrix_solution[k, ] <- gate1
      matrix_solution[,1] <- c(1:nrow(matrix_solution))
      
      ###
    } # End When selected element is the Hydrogen
    ####################################################################
    
    
    ###  
  } # End Part 4: Non-Normal Stoichiometry Resolution for Hydride (Case 3 of 3)
  ####################################################################################
  
  # Part 5: SandBox
  {
    ###
    # SandBox!
    if (internal_control == 4) {
      matrix_sandbox <- matrix("Hydride - SandBox", nrow(matrix_solution), ncol(matrix_solution))    
      colnames(matrix_sandbox) <- colnames(matrix_solution)
      matrix_sandbox[,1] <- c(1:nrow(matrix_sandbox))
      matrix_solution <- matrix_sandbox
    } # End SandBox!
    ###  
  } # End Part 5: SandBox
  ################################################
  
  
  
  
  # Part 6: Return
  {
    ###
    
    matrix_solution <- matrix_solution[,c(1:8, 11, 12, 9, 10)]
    return(matrix_solution)
    
    
    ###  
  } # End Part 5: Return
  ############################################
  
} # End Function***


InternalControl_Hydride <- function(input_atomic_number1 = NULL, 
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
    dt1_out <- type1 != "Noble gas" && symbol1 != "H"
    dt2_out <- symbol1 == "H"
    dt3_out <- type1 == "Noble gas"
    dt4_out <- sum(dt1_out, dt2_out, dt3_out) == 0 # SandBox!
    
    # Initialization
    decision <- NA
    
    # Details
    if(dt1_out) decision <- 1 else       # Normal - case 1
      if(dt2_out) decision <- 2 else     # Non-Normal - case 2
        if(dt3_out) decision <- 3 else   # Non-Normal - case 3
          if(dt4_out)  decision <- 4     # SandBox!    # Non-Normal - case 3
    
    
    ###  
  } # End Part 1: Detection
  #################################################################
  
  # Part 2: Return    
  return(decision)
  
  
} # End InternalControl_Hydride()


# input_atomic_number1 <- 3
# input_valence1 <- 1
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_GeneralNomenclature01 <-  GeneralNomenclature01
# input_GeneralNomenclature02 <-  GeneralNomenclature02
# input_GeneralNomenclature03 <-  GeneralNomenclature03
# 
# 
# input_hydride_resolution <- HydrideEquation(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                          input_internal_language = "en",
#                                          input_external_language = input_external_language,
#                                          input_PeriodicTable = input_PeriodicTable)
# 
# 
# input_hydride_resolution