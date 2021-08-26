


# Resolution 01: Oxide
OxideEquation <- function(input_atomic_number1 = NULL, input_valence1 = NULL, 
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
    internal_control <- InternalControl_Oxide(input_atomic_number1 = input_atomic_number1,
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
            # The "matrix_solution" has 8 rows y 12 columns.
            # Each row is a step on the stoichiometry resolution (8 steps for oxides = 8 rows).
            # Each column is a equation detail about each step.
            #
            # The "matrix_solution" has the next columns for oxides:
            # 1) Step Order
            # 2) coefficient of the element in reactants
            # 3) Abbreviation of the element in reagents
            # 4) Sub-index of the element in reactants
            # 5) Coefficient of the Oxygen in reagents
            # 6) Chemical symbol of Oxygen in reagents
            # 7) Oxygen sub-index in reagents
            # 8) Coefficient of the Oxide in products
            # 9) Chemical symbol of the element in products
            # 10) Sub-index of the element in the oxide in products
            # 11) Chemical symbol of the element in products
            # 12) Sub-index of the oxygen in the oxide in products
            ###
          } # End 1.0 Explanation
          ########################################################
          
          
          # 1.1) The creation for "matrix_solution"
          {
          ###
            
            # Total Steps for Oxide Resolution (TSFOR)
            TSFOR <- 8
            
            my_names <- c("Order", 
                             "Coef1", "ElementReactive", "Sub1",   
                             "Coef2", "OxygenReactive",  "Sub2",    
                             "Coef3", "ElementProduct", "Sub3", "OxygenProduct", "Sub4") 
            
            
            matrix_solution <- data.frame(matrix(NA, TSFOR, length(my_names)))
            colnames(matrix_solution) <- my_names
          ###  
          } # End The creation for "matrix_solution"
          ########################################################
          
          
          ###
        } # End Part 1: Structure of the "matrix_solution"
  ############################################################
        
        
        
   # Part 2: Normal Stoichiometry Resolution for Oxides (Case 1 of 3)
   {
          ###
            
            
            # 2.0) Explanation about Part 2 (if you want to read)
            {
              ###
              
              # In the case of Oxides ...
              # The Stoichiometry is carried out as follows:
              #
              # 1) Presentation for the Reactives
              # The first element is placed and then the oxygen separated both
              # with a "+" sign, both being reactive.
              # No subscripts or coefficients added, yet.
              # ---- In computing, coefficients and subscripts equal to 1 ----
              # ---- and only in reagents. Nothing in products. ----
              # To the right of the oxygen an arrow called "Reaction Arrow" is placed.
              # After the reaction arrow the products will be detailed.
              #     
              #
              #  
              # 2) Reactives Details
              # The oxygen in the reactants has a subscript 2 because it is a gas.
              # If the element that will react with oxygen is also a gas
              # and it is not a Noble gas, it will also have a two as a subscript.
              #     
              #
              #  
              # 3) Product presentation: Oxide
              # To the right of the arrow, the reaction product will be detailed
              # which in this case is an oxide. The structure of the oxide is the
              # element and oxygen together (without space between them) indicating
              # that are part of the same molecule. In principle the subindex
              # of element and oxygen in products will be 1 in both cases.
              # 
              #
              # 4) Details about the Oxide
              # The subindex in the Oxide are changed.
              # In Oxide ... the sub-index of the element will be the valence of oxygen.
              # In Oxide ... the oxygen subscript will be the valence of the element.
              #
              # 5) Oxide simplification
              # If possible, in the oxide, the subscripts are simplified by the
              # least common divisor (gcd). If it has been possible to simplify the
              # subscripts, the gcd is placed as the coefficient of oxide.
              #
              # 6) Oxygen balance
              # We proceed to balance the equation for the first time.
              # First the amount of oxygen is balanced and then the amount
              # of the element.
              # This step 6 balances only oxygen.
              # For this purpose, the amount of oxygen in reagents and in
              #     products.
              # To calculate the amount of oxygen in reagents
              # multiply the coefficient and the corresponding subscript together
              # to oxygen in reagents.
              # To calculate the amount of oxygen in products
              # are multiplied by the oxide coefficient and the subscript
              # of oxygen within the oxide, corresponding to the product.
              # If the amounts of oxygen in reactants and products are not equal
              # the subscript of oxygen in the oxide will be placed as a coefficient
              # of oxygen in reagents, and the oxygen subscript will be placed
              # in reagents as a coefficient of oxide in products. This procedure
              # ment balances the amount of oxygen in the equation.
              #
              #  
              # 7) Element balance
              # We proceed to balance the equation a second time.
              # We already have balanced Oxygen globally, now
              # we balance the element.
              # This step7 balances only the element.
              # For this purpose, the amount of the element is calculated in reagents and in
              #     products.
              # To calculate the amount of the element in reactants
              # multiply the coefficient and the corresponding subscript together
              # to the element in reagents.
              # To calculate the quantity of item in products
              # are multiplied by the oxide coefficient and the subscript
              # of the element within the oxide, corresponding to the product.
              # If the quantity of the element in reactants and products are not equal
              # only the coefficient of the element should be modified in reatives, and
              # This form will finally balance the entire equation.
              # This new element coefficient will be calculated as the quantity of the element
              # into products divided the amount of the element into reactants.
              # The result of this operation is the new coefficient for the
              # element in reagents, and will provide the general balance of the entire
              # chemical equation.
              # 
              # 8) Coefficient Simplification
              # If possible, simplify all coefficients by the
              # same least common divisor (gcd).
            
              
              
              
              ###  
            } # End: 2.0) Explanation about Part 2 (if you want to read)
            #########################################################
            
            
            
            
            # 2.1) Stoichiometry Algorithm for Oxides
            # If the element is not a Noble gas ... and it is not oxygen
            if (internal_control == 1) {
            
            
              
              
              # We plant a "zero" seed with Stoichiometry steps.
             general_internal_step <- 0
              
              
              # Step 1 of 8 - Oxide
              {
                ###
                
                # 1) Presentation for the Reactive
                # The first element is placed and then the oxygen separated both
                # with a "+" sign, both being reactive.
                # No subscripts or coefficients added, yet.
                # ---- In computing, coefficients and subscripts equal to 1 ----
                # ---- and only in reagents. Nothing in products. ----
                # To the right of the oxygen an arrow called "Reaction Arrow" is placed.
                # After the reaction arrow the products will be detailed.
                ##################################################################
                
                # One more step...
               general_internal_step <- general_internal_step + 1
                
                # Creation of step 1 ...
                gate1 <- rep(NA, ncol(matrix_solution))
                names(gate1) <- colnames(matrix_solution)
                
                # Implementamos el gate1...
                gate1["Order"] <- general_internal_step
                gate1["Coef1"] <- 1
                gate1["ElementReactive"] <- symbol1
                gate1["Sub1"] <- 1
                gate1["Coef2"] <- 1
                gate1["OxygenReactive"] <- "O"
                gate1["Sub2"] <- 1
                gate1["Coef3"] <- ""
                gate1["ElementProduct"] <- ""
                gate1["Sub3"] <- ""
                gate1["OxygenProduct"] <- ""
                gate1["Sub4"] <- ""
  
                
                # Load gate1 on matrix_solution  
                matrix_solution[general_internal_step, ] <- gate1
                
                ###
              } # End Step 1 of 8 - Oxide
              ####################################################################
              
              
              # Step 2 of 8 - Oxide
              {
                ###
                
                # 2) Reactive Details
                # The oxygen in the reactants has a subscript 2 because it is a gas.
                # If the element that will react with oxygen is also a gas
                # and it is not a Noble gas, it will also have a two as a subscript.
                #####################################################################
                
                
                # One more step...
               general_internal_step <- general_internal_step + 1
                
  
                # Subscript of element in reactants
                sub_ind1 <- 1   # Por defecto va un 1
                # Si es gaseoso y no es gas noble...  va un 2 en vez de un 1...
                if (state1 == "Gas" && type1 != "Noble gas") sub_ind1 <- 2
                
                # Subscript of Oxygen in reactants
                sub_ind2 <- 2   # Va un dos por ser el Oxigeno un gas... por defecto
                
  
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
              } # End Step 2 of 8 - Oxide
              ####################################################################
              
              
              # Step 3 of 8 - Oxide
              {
                ###
                
                # 3) Presentation for the product: Oxide
                # To the right of the arrow, the reaction product will be detailed
                # which in this case is an RUST. The structure of the OXIDE is the
                # element and oxygen together (without space between them) indicating
                # that are part of the same molecule. In principle the subindex
                # of element and oxygen in products will be 1 in both cases.
                #######################################################################
                
                
                # One more step...
               general_internal_step <- general_internal_step + 1
                
               # Details about participants in gate3...
               element_prod <- symbol1
               oxygen_prod <- "O"
               
               # Initial coefficient for the oxide
               coef3 <- 1    # Default value is 1
               
               # Subindex in the oxide for Element and the Oxygen
               sub_ind3 <- 1   # Default value is 1
               sub_ind4 <- 1   # Default value is 1
               
               # Creation of the gate3 (We take values before)
               gate3 <- gate2
                
               # Implementation of gate3 modifications...
                gate3["Order"] <- general_internal_step
                gate3["Coef3"] <- coef3
                gate3["ElementProduct"] <- element_prod
                gate3["Sub3"] <- sub_ind3
                gate3["OxygenProduct"] <- oxygen_prod
                gate3["Sub4"] <- sub_ind4
                
                
                # Load the gate3  
                matrix_solution[general_internal_step, ] <- gate3
                
                # Remove objects...
                remove(coef3, element_prod, sub_ind3, oxygen_prod, sub_ind4)
                
                ###
              } # End Step  3 of 8 - Oxide
              ####################################################################
              
              
              # Step  4 of 8 - Oxide
              {
                ###
                
                # 4) Details on the Oxide
                # The subscripts in the Oxide are changed.
                # In Oxide ... the sub-index of the element will be the valence of oxygen.
                # In Oxide ... the oxygen subscript will be the valence of the element.
                #############################################################################
                
                # One more step...
               general_internal_step <- general_internal_step + 1
                
               # Sub-indexes of the element and oxygen in products ...
               sub_ind3 <- 2   # Here is the oxygen valence
               sub_ind4 <- valence1    # Here is the valence of the element
               
                # Creation of the gate4
                gate4 <- gate3
                
                # Implementation of gate4 modifications...
                gate4["Order"] <- general_internal_step
                gate4["Sub3"] <- sub_ind3
                gate4["Sub4"] <- sub_ind4
                
                
                # Load the gate gate4  
                matrix_solution[general_internal_step, ] <- gate4
                
                # Remove objects...
                remove(sub_ind3, sub_ind4)
                
                ###
              } # End Step  4 of 8 - Oxide
              ####################################################################
              
              
              # Step 5 of 8 - Oxide
              {
                ###
                
                # 5) Simplification in the Oxide
                # If possible, in the oxide, the subscripts are simplified by the
                # least common divisor (gcd). If it has been possible to simplify the
                # subscripts, the gcd is placed as the coefficient of oxide.
                #####################################################################
                
                # One step more...
               general_internal_step <- general_internal_step + 1
                
               # We retrieve the subscripts from step 4 ...
               sub_ind3 <- as.numeric(as.character(gate4["Sub3"]))
               sub_ind4 <- as.numeric(as.character(gate4["Sub4"])) 
               
               # We calculate the least common divisor.
               # I found a library that does it directly!
               library(numbers)
               mcd <- mGCD(c(sub_ind3, sub_ind4))
               
               # The new subscripts in the oxide ...
               mod_sub_ind3 <- sub_ind3/mcd
               mod_sub_ind4 <- sub_ind4/mcd
               
               
                # Gate 5 creation
                gate5 <- gate4
                
                # Implementation of changes in Gate 5
                gate5["Order"] <- general_internal_step
                gate5["Sub3"] <- mod_sub_ind3
                gate5["Sub4"] <- mod_sub_ind4
                gate5["Coef3"] <- mcd
                
                # Load of the gate 5  
                matrix_solution[general_internal_step, ] <- gate5
                
                # We eliminate what is no longer needed ...
                remove(sub_ind3, sub_ind4, mod_sub_ind3, mod_sub_ind4, mcd)
                
                ###
              } # End Step 5 of 8 - Oxide
              ##################################################################################################################
              
              
              # Step 6 of 8 - Oxide
              {
                ###
                
                # 6) Balance of the Oxygen
                # We proceed to balance the equation for the first time.
                # First the amount of oxygen is balanced and then the amount
                # of the element.
                # This step 6 balances only oxygen.
                # For this purpose, the amount of oxygen in reagents and in
                #     products.
                # To calculate the amount of oxygen in reagents
                # multiply the coefficient and the corresponding subscript together
                # to oxygen in reagents.
                # To calculate the amount of oxygen in products
                # are multiplied by the oxide coefficient and the subscript
                # of oxygen within the oxide, corresponding to the product.
                # If the amounts of oxygen in reactants and products are not equal
                # the subscript of oxygen in the oxide will be placed as a coefficient
                # of oxygen in reagents, and the oxygen subscript will be placed
                # in reagents as a coefficient of oxide in products. This procedure
                # ment balances the amount of oxygen in the equation.
                ############################################################################
                
                
                
                # One step more...
               general_internal_step <- general_internal_step + 1
                
               # Gate 6 creation
               gate6 <- gate5
                
               # Implementation of changes in Gate 6
               gate6["Order"] <- general_internal_step 
               
               # We recover subscripts and coefficient of gate5 for oxygen and so on
               # balance oxygen globally ...
                coef2 <- as.numeric(as.character(gate5["Coef2"]))
                sub_ind2 <- as.numeric(as.character(gate5["Sub2"]))
                coef3 <- as.numeric(as.character(gate5["Coef3"]))
                sub_ind4 <- as.numeric(as.character(gate5["Sub4"]))
                
                
                # We calculate the amount of oxygen, in reactants and in products
                cantidad_oxigeno_reactivos <- coef2*sub_ind2
                cantidad_oxygen_productos <- coef3*sub_ind4
                
                # If the amounts of oxygen are equal, gate6 is equal to gate5
                # and that is already armed by default when the gate6 is created.
                # Now ... if the amounts of oxygen are different, you have to implement
                # the corresponding changes.
                if (cantidad_oxigeno_reactivos != cantidad_oxygen_productos){
                  
                  # We generate the changes for gate6 ...
                  mod_coef2 <- sub_ind4
                  mod_coef3 <- sub_ind2
                  
                  # We implement the gate6 modifications ...
                  gate6["Coef2"] <- mod_coef2
                  gate6["Coef3"] <- mod_coef3
                  
                  # We eliminate what is no longer needed from this internal step ...
                  remove(mod_coef2, mod_coef3)
                  
                } # Fin if
                
                
                # Load the gate6
                matrix_solution[general_internal_step, ] <- gate6
                
                # We eliminate what is no longer needed ...
                remove(coef2, sub_ind2, coef3, sub_ind4,
                       cantidad_oxigeno_reactivos, cantidad_oxygen_productos)
                
                ###
              } # End Step 6 of 8 - Oxide
              ##################################################################################################################
              
              
              
              # Step 7 of 8 - Oxide
              {
                ###
                
                # 7) Balance of the Element
                # We proceed to balance the equation a second time.
                # We already have balanced Oxygen globally, now
                # we balance the element.
                # This step7 balances only the element.
                # For this purpose, the amount of the element is calculated in reagents and in
                #     products.
                # To calculate the amount of the element in reactants
                # multiply the coefficient and the corresponding subscript together
                # to the element in reagents.
                # To calculate the quantity of item in products
                # are multiplied by the oxide coefficient and the subscript
                # of the element within the oxide, corresponding to the product.
                # If the quantity of the element in reactants and products are not equal
                # only the coefficient of the element should be modified in reatives, and
                # This form will finally balance the entire equation.
                # This new coefficient will be calculated as the quantity of the element
                # into products divided the amount of the element into reactants.
                # The result of this operation is the new coefficient for the
                # item in reagents, and will provide the general balance of the entire
                # chemical equation.
                ###########################################################################
                
                
                
                # One step more...
               general_internal_step <- general_internal_step + 1
                
                # Creation of the gate7...
                gate7 <- gate6
                
                # Implementacion de cambios degeneral_internal_step
                gate7["Order"] <- general_internal_step
                
                # We retrieve subscripts and coefficient of gate6 for the element
                # and thus balance oxygen globally ...
                coef1 <- as.numeric(as.character(gate6["Coef1"]))
                sub_ind1 <- as.numeric(as.character(gate6["Sub1"]))
                coef3 <- as.numeric(as.character(gate6["Coef3"]))
                sub_ind3 <- as.numeric(as.character(gate6["Sub3"]))
                
                
                # We calculate the amount of the element in reactants and in products
                cantidad_elemento_reactivos <- coef1*sub_ind1
                cantidad_element_productos <- coef3*sub_ind3
                
                
                
                
                # If the quantities of the element are equal, the gate7 is equal to gate6
                # and that is already armed by default when the gate7 is created.
                # Now ... if the amounts of oxygen are different, you have to implement
                # the corresponding changes.
                if (cantidad_elemento_reactivos != cantidad_element_productos){
                  
                  # We calculate the new coefficient in case it is necessary later ...
                  # This would be the new coefficient for the element in reactants        
                  mod_coef1 <- cantidad_element_productos / cantidad_elemento_reactivos  
                  
                  
                  # We implement the modifications of gate7 ...
                  gate7["Coef1"] <- mod_coef1
                  
                  # We eliminate what is no longer needed from this internal step ...
                  remove(mod_coef1)
                  
                } # Fin if
                
                
                
                
                # Load the gate7
                matrix_solution[general_internal_step, ] <- gate7
                
                # Remove objects...
                remove(coef1, sub_ind1, coef3, sub_ind3,
                       cantidad_elemento_reactivos, cantidad_element_productos)
                
                ###
              } # End Step 7 of 8 - Oxide
              ##################################################################################################################
              
              
              # Step 8 of 8 - Oxide
              {
                ###
                
                # 8) Coefficient Simplification
                # If possible, simplify all coefficients by the
                # same least common divisor (gcd).
                
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
                if (mcd != 1){
                  
                  # We implement the modifications of gate8 ...
                  gate8["Coef1"] <- mod_coef1
                  gate8["Coef2"] <- mod_coef2
                  gate8["Coef3"] <- mod_coef3
                  
                  # We eliminate what is no longer needed from this internal step ...
                  remove(mod_coef1, mod_coef2, mod_coef3)
                  
                } # Fin if
                
                
                
                # Load the gate8  
                matrix_solution[general_internal_step, ] <- gate8
                
                # We eliminate what is no longer needed ...
                remove(coef1, coef2, coef3, mcd)
                
                ###
              } # End Step 8 of 8 - Oxide
              ##################################################################################################################
              
            
              
              ###
            }  
                
                
                ###  
              } # End Part 2: Stoichiometry Resolution for Oxides (Case 1 of 3)
   ####################################################################################
       
     
    # Part 3: Oxygen - Non-normal Stoichiometry Resolution for Oxides (Case 2 of 3)
    {
      ###
      
        # When selected element is the Oxygen
        if (internal_control == 2) {
          
       
          # We plant a "zero" seed with stoichiometric steps.
          general_internal_step <- 0
            
            
            # Step 1 of 8 - Oxide - (Case 2 of 3)
            {
              ###
              
              # One more step...
              general_internal_step <- general_internal_step + 1
              
              # Creation of step 1 ...
              gate1 <- rep(NA, ncol(matrix_solution))
              names(gate1) <- colnames(matrix_solution)
              
              # Implementamos el gate1...
              gate1["Order"] <- general_internal_step
              gate1["Coef1"] <- 1
              gate1["ElementReactive"] <- symbol1
              gate1["Sub1"] <- 1
              gate1["Coef2"] <- 1
              gate1["OxygenReactive"] <- "O"
              gate1["Sub2"] <- 1
              gate1["Coef3"] <- ""
              gate1["ElementProduct"] <- ""
              gate1["Sub3"] <- ""
              gate1["OxygenProduct"] <- ""
              gate1["Sub4"] <- ""
              
              
              # Load gate1 on matrix_solution  
              matrix_solution[general_internal_step, ] <- gate1
              
              ###
            } # End Step 1 of 8 - Oxide - (Case 2 of 3)
            ####################################################################
            
            
            # Step 2 of 8 - Oxide - (Case 2 of 3)
            {
              ###
              
              # One more step...
              general_internal_step <- general_internal_step + 1
              
              
              # Subscript of element in reactants
              sub_ind1 <- 2   # Default value is 1
              sub_ind2 <- 2   # Default value is 1
              
              
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
            } # End Step 2 of 8 - Oxide - (Case 2 of 3)
            ####################################################################
            
            
            # Step 3 of 8 - Oxide - (Case 2 of 3)
            {
              ###
              
              # One more step...
              general_internal_step <- general_internal_step + 1
              
              # Details about participants in gate3...
              element_prod <- symbol1
              oxygen_prod <- "O"
              
              # Initial coefficient for Oxide
              coef3 <- 1    # Default value is 1
              
              # Default sub-index value
              sub_ind3 <- 1   # Por defecto va un 1
              
              # Creation of the gate3 (We take values before)
              gate3 <- gate2
              
              # Implementation of gate3 modifications...
              gate3["Order"] <- general_internal_step
              gate3["Coef3"] <- coef3
              gate3["ElementProduct"] <- element_prod
              gate3["Sub3"] <- sub_ind3
              
              
              # Load gate3  
              matrix_solution[general_internal_step, ] <- gate3
              
              # Remove some objects...
              remove(coef3, element_prod, sub_ind3, oxygen_prod)
              
              ###
            } # End Step  3 of 8 - Oxide - (Case 2 of 3)
            ####################################################################
            
            
            # Step  4 of 8 - Oxide - (Case 2 of 3)
            {
              ###
              
         
              # One more step...
              general_internal_step <- general_internal_step + 1
              
              # Sub-indexes for Oxygen in products...
              sub_ind3 <- 2   # Must be a value 2, because is a gas
              
              
              # Creation of the gate4
              gate4 <- gate3
              
              # Implementation of gate4 modifications...
              gate4["Order"] <- general_internal_step
              gate4["Sub3"] <- sub_ind3
              
              
              # Load gate4  
              matrix_solution[general_internal_step, ] <- gate4
              
              # Remove some objects...
              remove(sub_ind3)
              
              ###
            } # End Step  4 of 8 - Oxide - (Case 2 of 3)
            ####################################################################
            
            
            # Step 5 of 8 - Oxide - (Case 2 of 3)
            {
              ###
              # One step more...
              general_internal_step <- general_internal_step + 1
              
              # Gate 5 creation
              gate5 <- gate4
              
              # Implementation of changes in Gate 5
              gate5["Order"] <- general_internal_step
              
              
              # Load of the gate 5  
              matrix_solution[general_internal_step, ] <- gate5
              
              
              
              ###
            } # End Step 5 of 8 - Oxide - (Case 2 of 3)
            ##################################################################################################################
            
            
            # Step 6 of 8 - Oxide - (Case 2 of 3)
            {
              ###
              
              # One step more...
              general_internal_step <- general_internal_step + 1
              
              # Gate 6 creation
              gate6 <- gate5
              
              # Implementation of changes in Gate 6
              gate6["Order"] <- general_internal_step     
              
              # Load the gate6
              matrix_solution[general_internal_step, ] <- gate6
              
              
              
              ###
            } # End Step 6 of 8 - Oxide - (Case 2 of 3)
            ##################################################################################################################
            
            
            
            # Step 7 of 8 - Oxide - (Case 2 of 3)
            {
              ###
              
              # Un paso mas...
              general_internal_step <- general_internal_step + 1
              
              # Creation of the gate7...
              gate7 <- gate6
              
              # Implementacion de cambios degeneral_internal_step
              gate7["Order"] <- general_internal_step
              
              # We implement the modifications of gate7 ...
              gate7["Coef3"] <- 2
              
              # Load gate7
              matrix_solution[general_internal_step, ] <- gate7
              
            } # End Step 7 of 8 - Oxide - (Case 2 of 3)
            ##################################################################################################################
            
            
            
            
            
            # Step 8 of 8 - Oxide - (Case 2 of 3)
            {
              ###
              
              # One step more...
              general_internal_step <- general_internal_step + 1
              
              # Creation of the gate8
              gate8 <- gate7
              
              # Implementation of de general_internal_step changes
              gate8["Order"] <- general_internal_step
              
              
              
              # Load the gate8  
              matrix_solution[general_internal_step, ] <- gate8
              
              
              
              ###
            } # End Step 8 of 8 - Oxide - (Case 2 of 3)
            ##################################################################################################################
            
            
            ###
          } # End: When selected element is the Oxygen
          ##################################################################################
          
    } # End Part 3: Non-Normal Stoichiometry Resolution for Oxides (Case 2 of 3)
    ####################################################################################   
    
  
  
    # Part 4: Noble gas - Non-normal Stoichiometry Resolution for Oxides (Case 3 of 3)
    {
      ###
      
      # When selected element is a Noble gas
      if (internal_control == 3) {
     
            
              # We plant a "zero" seed with stoichiometric steps.
              general_internal_step <- 0
              
              
              # Step 1 of 8 - Oxide - (Case 3 of 3)
              {
                ###
                
                # 1) Presentation for the Reactives
                # The first element is placed and then the oxygen separated both
                # with a "+" sign, both being reactive.
                # No subscripts or coefficients added, yet.
                # ---- In computing, coefficients and subscripts equal to 1 ----
                # ---- and only in reagents. Nothing in products. ----
                # To the right of the oxygen an arrow called "Reaction Arrow" is placed.
                # After the reaction arrow the products will be detailed.
                ##################################################################
                
                # One more step...
                general_internal_step <- general_internal_step + 1
                
                # Creation of step 1 ...
                gate1 <- rep(NA, ncol(matrix_solution))
                names(gate1) <- colnames(matrix_solution)
                
                # Implementation of gate1...
                gate1["Order"] <- general_internal_step
                gate1["Coef1"] <- 1
                gate1["ElementReactive"] <- symbol1
                gate1["Sub1"] <- 1
                gate1["Coef2"] <- 1
                gate1["OxygenReactive"] <- "O"
                gate1["Sub2"] <- 1
                gate1["Coef3"] <- ""
                gate1["ElementProduct"] <- ""
                gate1["Sub3"] <- ""
                gate1["OxygenProduct"] <- ""
                gate1["Sub4"] <- ""
                
                
                # Load gate1 on matrix_solution  
                matrix_solution[general_internal_step, ] <- gate1
                
                ###
              } # End Step 1 of 8 - Oxide - (Case 3 of 3)
              ####################################################################
              
              
              # Step 2 of 8 - Oxide - (Case 3 of 3)
              {
                ###
                
                # 2) Reactives Details
                # The oxygen in the reactants has a subscript 2 because it is a gas.
                # If the element that will react with oxygen is also a gas
                # and it is not a Noble gas, it will also have a two as a subscript.
                #####################################################################
                
                
                # One more step...
                general_internal_step <- general_internal_step + 1
                
                
                # Subscript of element in reactants
                sub_ind1 <- 1   # Default value is 1
                sub_ind2 <- 2   # Default value is 1
                
                
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
              } # End Step 2 of 8 - Oxide - (Case 3 of 3)
              ####################################################################
              
              
              # Step 3 of 8 - Oxide - (Case 3 of 3)
              {
                ###
                
                # 3) Presentation for the product: Oxide
                # To the right of the arrow, the reaction product will be detailed
                # which in this case is an RUST. The structure of the OXIDE is the
                # element and oxygen together (without space between them) indicating
                # that are part of the same molecule. In principle the subindex
                # of element and oxygen in products will be 1 in both cases.
                #######################################################################
                
                
                # One more step...
                general_internal_step <- general_internal_step + 1
                
                # Details about participants in gate3...
                element_prod <- symbol1
                oxygen_prod <- "O"
                
                # Initial coefficient for Oxide
                coef3 <- 1    # Default value is 1
                
                # Default sub-index value
                sub_ind3 <- 1   # Default value is 1
                sub_ind4 <- 1   # Default value is 1
                
                # Creation of the gate3 (We take values before)
                gate3 <- gate2
                
                # Implementation of gate3 modifications...
                gate3["Order"] <- general_internal_step
                gate3["Coef3"] <- coef3
                gate3["ElementProduct"] <- element_prod
                gate3["OxygenProduct"] <- oxygen_prod
                gate3["Sub3"] <- sub_ind3
                gate3["Sub4"] <- sub_ind4
                
                
                # Load gate3  
                matrix_solution[general_internal_step, ] <- gate3
                
                # Remove some objects...
                remove(coef3, element_prod, sub_ind3, oxygen_prod, sub_ind4)
                
                ###
              } # End Step  3 of 8 - Oxide - (Case 3 of 3)
              ####################################################################
              
              
              # Step  4 of 8 - Oxide - (Case 3 of 3)
              {
                ###
                
                # 4) Details on the Oxide
                # The subscripts in the Oxide are changed.
                # In Oxide ... the sub-index of the element will be the valence of oxygen.
                # In Oxide ... the oxygen subscript will be the valence of the element.
                #############################################################################
                
                # One more step...
                general_internal_step <- general_internal_step + 1
                
                # Sub-indexes for Oxygen in products...
                sub_ind4 <- 2   # Must be a value 2, because is a gas
                
                
                # Creation of the gate4
                gate4 <- gate3
                
                # Implementation of gate4 modifications...
                gate4["Order"] <- general_internal_step
                gate4["Sub4"] <- sub_ind4
                
                
                # Load gate4  
                matrix_solution[general_internal_step, ] <- gate4
                
                # Remove some objects...
                remove(sub_ind4)
                
                ###
              } # End Step  4 of 8 - Oxide - (Case 3 of 3)
              ####################################################################
              
              
              # Step 5 of 8 - Oxide - (Case 3 of 3)
              {
                ###
                
                # 5) Simplification in the Oxide
                # If possible, in the oxide, the subscripts are simplified by the
                # least common divisor (gcd). If it has been possible to simplify the
                # subscripts, the gcd is placed as the coefficient of oxide.
                #####################################################################
                
                # One step more...
                general_internal_step <- general_internal_step + 1
                
                # Gate 5 creation
                gate5 <- gate4
                
                # Implementation of changes in Gate 5
                gate5["Order"] <- general_internal_step
                
                
                # Load of the gate 5  
                matrix_solution[general_internal_step, ] <- gate5
                
                
                
                ###
              } # End Step 5 of 8 - Oxide - (Case 3 of 3)
              ##################################################################################################################
              
              
              # Step 6 of 8 - Oxide - (Case 3 of 3)
              {
                ###
                
                # 6) Balance of the Oxygen
                # We proceed to balance the equation for the first time.
                # First the amount of oxygen is balanced and then the amount
                # of the element.
                # This step 6 balances only oxygen.
                # For this purpose, the amount of oxygen in reagents and in
                #     products.
                # To calculate the amount of oxygen in reagents
                # multiply the coefficient and the corresponding subscript together
                # to oxygen in reagents.
                # To calculate the amount of oxygen in products
                # are multiplied by the oxide coefficient and the subscript
                # of oxygen within the oxide, corresponding to the product.
                # If the amounts of oxygen in reactants and products are not equal
                # the subscript of oxygen in the oxide will be placed as a coefficient
                # of oxygen in reagents, and the oxygen subscript will be placed
                # in reagents as a coefficient of oxide in products. This procedure
                # ment balances the amount of oxygen in the equation.
                ############################################################################
                
                
                
                # One step more...
                general_internal_step <- general_internal_step + 1
                
                # Gate 6 creation
                gate6 <- gate5
                
                # Implementation of changes in Gate 6
                gate6["Order"] <- general_internal_step     
                
                # Load the gate6
                matrix_solution[general_internal_step, ] <- gate6
                
                
                
                ###
              } # End Step 6 of 8 - Oxide - (Case 3 of 3)
              ##################################################################################################################
              
              
              
              # Step 7 of 8 - Oxide - (Case 3 of 3)
              {
                ###
                
                # 7) Balance of the Element
                # We proceed to balance the equation a second time.
                # We already have balanced Oxygen globally, now
                # we balance the element.
                # This step7 balances only the element.
                # For this purpose, the amount of the element is calculated in reagents and in
                #     products.
                # To calculate the amount of the element in reactants
                # multiply the coefficient and the corresponding subscript together
                # to the element in reagents.
                # To calculate the quantity of item in products
                # are multiplied by the oxide coefficient and the subscript
                # of the element within the oxide, corresponding to the product.
                # If the quantity of the element in reactants and products are not equal
                # only the coefficient of the element should be modified in reatives, and
                # This form will finally balance the entire equation.
                # This new coefficient will be calculated as the quantity of the element
                # into products divided the amount of the element into reactants.
                # The result of this operation is the new coefficient for the
                # item in reagents, and will provide the general balance of the entire
                # chemical equation.
                ###########################################################################
                
                
                
                # Un paso mas...
                general_internal_step <- general_internal_step + 1
                
                # Creation of the gate7...
                gate7 <- gate6
                
                # Implementacion de cambios degeneral_internal_step
                gate7["Order"] <- general_internal_step
                
                # We implement the modifications of gate7 ...
                gate7["Coef3"] <- 1
                
                # Load gate7
                matrix_solution[general_internal_step, ] <- gate7
                
              } # End Step 7 of 8 - Oxide - (Case 3 of 3)
              ##################################################################################################################
              
              
              
              
              
              # Step 8 of 8 - Oxide - (Case 3 of 3)
              {
                ###
                
                # 8) Coefficient Simplification
                # If possible, simplify all coefficients by the
                # same least common divisor (gcd).
                
                # One step more...
                general_internal_step <- general_internal_step + 1
                
                # Creation of the gate8
                gate8 <- gate7
                
                # Implementation of de general_internal_step changes
                gate8["Order"] <- general_internal_step
                
                
                
                # Load the gate8  
                matrix_solution[general_internal_step, ] <- gate8
                
                
                
                ###
              } # End Step 8 of 8 - Oxide - (Case 3 of 3)
              ##################################################################################################################
              
              
            
            
          } # End When selected element is a Noble gas
      ################################################################################   
      
      
      ###  
    } # End Part 4: Non-Normal Stoichiometry Resolution for Oxides (Case 3 of 3)
    ############################################################
    
    # Part 5: SandBox
    {
    ###
      # SandBox!
      if (internal_control == 4) {
      matrix_sandbox <- matrix("Oxide - SandBox", nrow(matrix_solution), ncol(matrix_solution))    
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
          
          
          return(matrix_solution)
          
          
        ###  
        } # End Part 5: Return
    ############################################
        
        
        ### 
      } # End OxideEquation()
      #####################################################################################



InternalControl_Oxide <- function(input_atomic_number1 = NULL, 
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
    dt1_out <- type1 != "Noble gas" && symbol1 != "O"
    dt2_out <- symbol1 == "O"
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

  
  } # End InternalControl_Oxide()


# input_atomic_number1 <- 2
# input_valence1 <- 1
# input_internal_language <- "en"
# input_external_language <- "es"
# input_PeriodicTable <- PeriodicTable
# input_GeneralNomenclature01 <-  GeneralNomenclature01
# input_GeneralNomenclature02 <-  GeneralNomenclature02
# input_GeneralNomenclature03 <-  GeneralNomenclature03
# 
# 
# input_oxide_resolution <- OxideEquation(input_atomic_number1 = input_atomic_number1,
#                                          input_valence1 = input_valence1,
#                                          input_internal_language = "en",
#                                          input_external_language = input_external_language,
#                                          input_PeriodicTable = input_PeriodicTable)
# 
# 
# input_oxide_resolution