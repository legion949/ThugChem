



OxideNomenclature <- function(input_atomic_number1 = NULL,
                        input_valence1 = NULL,
                        input_internal_language = "en", 
                        input_external_language = NULL, 
                        input_PeriodicTable = NULL,
                        input_GeneralNomenclature01 = NULL,
                        input_GeneralNomenclature02 = NULL,
                        input_GeneralNomenclature03 = NULL) {
  
  
  
  
  # Internal language specifications by default and optional if null
  input_internal_language <- "en"
  if (is.null(input_external_language)) input_external_language <- input_internal_language
  
  
  
  
  
  # Parte 1: Informacion necesaria muy particular
  {
    ###
    
    
    # Initial values
    num_atom1 <- input_atomic_number1
    valence1 <- input_valence1
    
    
    # Specific of the exercise ...
    symbol1 <- as.character(input_oxide_resolution[1,3])
    dt1 <- input_PeriodicTable[[input_internal_language]]$"Symbol" == symbol1
    name1 <- input_PeriodicTable[[input_external_language]][dt1,3] # Name in original language
    type1 <- input_PeriodicTable[[input_internal_language]][dt1,"Type"]
    state1 <- input_PeriodicTable[[input_internal_language]][dt1,"State"]
    quantity_valences1 <- as.numeric(as.character(input_PeriodicTable[[input_internal_language]][dt1,15]))
    
    # Very special details...
    all_oxide_names <- strsplit(as.character(input_PeriodicTable[[input_external_language]][dt1,17]), ";")[[1]]
    all_valences_values <- as.numeric(strsplit(as.character(input_PeriodicTable[[input_internal_language]][dt1,10]), ";")[[1]])
    order_valences <- c(1:quantity_valences1)
    dt_valence1 <- all_valences_values == valence1
    my_order_valences1 <- order_valences[dt_valence1]
    

    
    ###  
  } # Fin Parte1
  ##############################################
  
  
  # Part 2: Other Necessary Items
  {
    ###
    
    # Roman numerals
    romans <- as.character(input_GeneralNomenclature01$Romans)
    
    # More details
    col_position <- grep(input_external_language, colnames(input_GeneralNomenclature01))
    
    
    # Detalles de cantidades
    amount_detail1 <- input_GeneralNomenclature01[,col_position[1]]
    amount_detail2 <- input_GeneralNomenclature01[,col_position[2]]
    
    ###  
  } # Part 2
  ############################################
  
  
  # Part3: Clasic Resolution
  {
    ###  
    # Generation of the input_example
    input_example <- Resol_01_Oxide(input_atomic_number1 = input_atomic_number1,
                                    input_valence1 = input_valence1,
                                    input_internal_language = "en",
                                    input_external_language = input_external_language,
                                    input_PeriodicTable = input_PeriodicTable)
    
    
    # # Litle change in "input_example".
    {
      ###
      # It happens that the element Fluor, which has the symbol "F" as it takes
      # as if it were a "FALSE". So, you have to see how I take it
      # and in this case assign a change so that it does not take it as a value
      # logical but as character type.
      # Of the "input_PeriodicTable" object, it is the third column that has the
      # chemical symbol of the element that participates in the oxide.
      # We will take that 3rd column and see if you have eaten it as "FALSE".
      
      # We make a technical stop ...
      # We take from the first row, column 3. There is the chemical symbol
      # If there is a "FALSE" noted ... we change it to "F" type character.
      
      # Technical stop...
      my_stop <- input_example[1,3]
      
      # If change is necessary ...
      if (!is.na(my_stop)) if (my_stop == "FALSE"){
        
        # It is going to fix on each element of the matrix to make the change ...
        for (v1 in 1:nrow(input_example)) for (v2 in 1:ncol(input_example)){
          
          # If necessary, implement the change ...
          if (!is.na(input_example[v1,v2])) if (input_example[v1,v2] == "FALSE") input_example[v1,v2] <- "F"
          
        } # Fin doble for...
      } # Fin if my_stop == "FALSE"
      
      
      
      
      
      ###  
    } # End Litle change
    
    
    
    ###  
  } # End Part3
  ##################################
  
  
  # Part 4: Resolution of the LaTeX
  {
    ###
    
    input_latex <- OxideLaTeX(input_oxide_resolution = input_example, 
               input_internal_language = input_internal_language, 
               input_external_language = input_external_language,  
               input_PeriodicTable = input_PeriodicTable)
    
    
   
    
    
    
    ###  
  } # End Part4
  ##########################################
  
  
  
  # Part 5: Creation of intermediate objects
  {
    ###
    # We take the last detail of the oxide itself.
    count_all_step <- nrow(input_example)
    coef3 <- as.numeric(as.character(input_example[count_all_step,8]))
    element3 <- as.character(input_example[count_all_step,9])
    sub3 <- as.numeric(as.character(input_example[count_all_step,10]))
    element4 <- as.character(input_example[count_all_step,11])
    sub4 <- as.numeric(as.character(input_example[count_all_step,12]))
    
    ###    
  } # End Part 5
  ################################################################
  
  
  # Part 6: Nomenclature
  {
    ###
    
    # General evaluation of the case ... 
    dt_case1 <- symbol1 != "O"  # Is not the oxygen...
    dt_case2 <- type1 != "Noble Gas" # Is not a noble gas...
    dt_case3 <- sum(dt_case1, dt_case2) == 2  # Is not the oxygen and is not a noble gas
    dt_case4 <- state1 == "Gas"   # Is a gas
    dt_case5 <- sum(dt_case2, dt_case4) == 2 # Is a gas and is not a noble gas...
    
    
    # Generation of 'nomenclature_text_oxide'
    {
      ###
      
      
      # To summarize, there are 3 different nomenclatures.
      # Two of them change their way of naming chemical compounds
      # according to the amount of valences that the chemical element that participates has.
      # These are "IUPAC" and "Classic".
      # The 3rd does not depend on the amount of valences, it is the "Numeral Stock".
      
      
      # General objects ...
      {
        ###
        
        r1 <- romans[valence1]
        r2 <- romans[2]
        
        fixed_name <- name1
        my_name <- name1
        
        ###    
      } # End General Objects...
      
      special_details <- input_GeneralNomenclature02[,input_external_language] # IUPAC, CLASSIC, Numeral Stock... 
      nomenclature_text_oxide <- rep(NA, length(special_details))
      names(nomenclature_text_oxide) <- special_details
      
      ###
    } # End Generation
    ###########################################################
    
    
    
    # Case 1) Cases for when the element is not oxygen and not a noble gas
    if(dt_case3){
      
      
      # The particle and others
      particle <- GeneralNomenclature03[1, input_external_language]
      special_particle <- strsplit(particle, " ")[[1]][1]
      prefix <- c(GeneralNomenclature03[5, input_external_language], "", "",
                  GeneralNomenclature03[c(6,7), input_external_language])

      
      # Form 1) IUPAC
      nomenclature_text_oxide[1] <- paste0(particle, fixed_name, " (", r1, ")")
      
      
      # Form 2) Classic
      {
        ###
        # Depends on the number of valences of the element
        # and the valence itself that uses of that total of valences
        
        # If it has a single valence ...
        if (quantity_valences1 == 1) {
          
          
          # In this chaos the name is very easy ...
          nomenclature_text_oxide[2] <- paste0(particle, fixed_name)
          
          
          
          
          
          
        }  # End if it has a single valence ...
        
        
        # If it has two valences ...
        if (quantity_valences1 == 2) {
         
          my_oxide_name <- all_oxide_names[dt_valence1]
          nomenclature_text_oxide[2] <- paste0(special_particle, my_oxide_name) # Oxide... 
          
          
          
        } # End If it has two valences ...
        
        
        # If it has three valences ...
        if (quantity_valences1 == 3) {
          
          # Prefix and pooled names
          nomenclature_options <- all_oxide_names[c(1,1,2)]
          
          # prefix y nombre seleccionado...
          my_prefix <- prefix[dt_valence1]
          my_oxide_name <- nomenclature_options[dt_valence1]
          
          # FInal name
          nomenclature_text_oxide[2] <- paste0(special_particle, " ", my_prefix, my_oxide_name)
          
          ###
        } # End If it has three valences ...
        #########################################################################
        
        # If it has four valences ...
        if (quantity_valences1 == 4) {
          
          # Prefix and pooled names
          nomenclature_options <- all_oxide_names[c(1,1,2,2)]
          
          # prefix y name selection...
          my_prefix <- prefix[dt_valence1]
          my_oxide_name <- nomenclature_options[dt_valence1]
          
          
          # FInal name
          nomenclature_text_oxide[2] <- paste0(special_particle, " ", my_prefix, my_oxide_name)
          
          ###
        } # End If it has four valences ...
        #########################################################################
        
        
        # Si tiene cinco valencias...
        if (quantity_valences1 == 5) {
          
          # prefixs y pool de nombres
          nomenclature_options <- strsplit(all_oxide_names, "; ")[[1]][c(1,1,2,2,2)]
          
          # prefix y nombre seleccionado...
          my_prefix <- prefix[dt_valence1]
          my_oxide_name <- nomenclature_options[dt_valence1]
          
          
          # FInal name
          nomenclature_text_oxide[2] <- paste0(special_particle, " ", my_prefix, my_oxide_name)
          
          ###
        } # Fin Si tiene cinco valencias...
        #########################################################################
        
        
      } # End Form 2) Classic
      ###################################    
      
      
      # Forma 3: Numeral Stock
      {
        ###
        
        # It does not depend on the number of valences. That is why the script is shorter.
        # We choose the correct quantity detail
        dc1 <- amount_detail1[sub4]
        dc2 <- amount_detail1[sub3]
        
        # We make a particular correction
        if (sub3 == 1) dc2 <- ""
        
        
        
        # Stock
        nomenclature_text_oxide[3] <- paste0(dc1, particle, " ", dc2, my_name)
        
        
        
        
        ###  
      } # Fin Forma 3: Numeral Stock
      ###########################################  
      
      
    } # Fin Caso 1)
    
    
    # Case 2) For when the element is "Oxygen" ...
    # It will only say "Oxygen" at the exit
    if(!dt_case1) {
      
      nomenclature_text_oxide[1] <- input_PeriodicTable[[input_external_language]][8,3]
      nomenclature_text_oxide[2] <- input_PeriodicTable[[input_external_language]][8,3] 
      nomenclature_text_oxide[3] <- input_PeriodicTable[[input_external_language]][8,3]
      
      
    } # Fin Caso 2)
    
    
    # Case 3) For when the element is a noble gas
    if(!dt_case2) {
      
      nomenclature_text_oxide[1] <- paste0(nombre1, " ", GeneralNomenclature03[8, input_external_language])
      nomenclature_text_oxide[2] <- paste0(name1, " ", GeneralNomenclature03[9, input_external_language])
      nomenclature_text_oxide[3] <- paste0(GeneralNomenclature03[10, input_external_language])
      
    } # Fin Caso 3)
    
    
    
    
    # Caso 4: Chem Formula
    {
      ###
      fq <-  paste0(input_example[nrow(input_example), c(9:12)], collapse="")
      fq_latex <- input_latex[nrow(input_latex), 7] 
      
      nomenclature_text_oxide[4] <- fq
      nomenclature_text_oxide[5] <- fq_latex
      ###  
    } # Forma 4
    #########################################################
    
    ###  
  } # Fin Parte6
  #######################################
  
  
  # Parte7: Salida
  {
    ###
    
    return(nomenclature_text_oxide)
    ###  
  } # Fin parte 7
  ######################################
  
} # Fun Function OxideNomenclature***




