
# options(encoding = 'UTF-8')
# options(encoding = 'latin1')

# options(encoding = 'latin1')
# Source initial UI settings
#opar <- par()
#par(mar = c(0, 0, 0, 0))

source("lib.R")



# Here we create our translator ...
suppressWarnings(translator <- Translator$new(translation_csvs_path = "data/TranslatePagei18n/"))

# 0001 - 1 de 3 - Deteccion de lenguaje por defecto....
jscode <- "var language =  window.navigator.userLanguage || window.navigator.language;
Shiny.onInputChange('mydata', language);
console.log(language);"


shinyServer(function(input, output ,session) {
  

# Update langeage por UI
  observeEvent(input$selected_language, {
    # This print is just for demonstration
    print(paste("Language change!", input$selected_language))
    # Here is where we update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })


  # Update langeage por SERVER
  i18n <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% translator$get_languages()) {
      translator$set_translation_language(selected)
    }
    translator
  })


  if (1 == 2) {
  # Language
  {
  ###

  # 0001 - 2 de 3 - Deteccion de lenguaje por defecto....
  runjs(jscode)
  output$your_lang <- renderPrint(input$mydata)
  #######################################################


  #
  default_language <- reactive({
    if (!is.null(input$mydata)) {

    partes <- strsplit(as.character(input$mydata), "-")

    unlist(partes)[1]
    }
    })





  output$nn_language <- renderUI({

    idiomas <- c("English", "Español", "Francais")
    codificacion <- c("en", "es", "fr")
    names(codificacion) <- idiomas

    dt <- codificacion == default_language()

    selectInput(inputId = "language_selector",
                label = "Language",
                choices = codificacion,
                selected = codificacion[dt]
                )

  })


  selected_language <- reactive({

    if(is.null(input$language_selector)) mi_lenguaje <- default_language() else mi_lenguaje <- input$language_selector

    mi_lenguaje <- as.character(mi_lenguaje)
    mi_lenguaje



  })

  ###
  } # End Language
  ######################################
  }




  # General Resolution
  {
  ###

    the_equation <- reactive({

      GeneralEquation(input_atomic_number1 = input$atomic_number1,
                      input_valence1 = input$valence1,
                      input_atomic_number2 = input$atomic_number2,
                      input_valence2 = input$valence2,
                      input_family = input$chemestry_family,
                      input_internal_language = "en",
                      input_external_language = "en",
                      input_PeriodicTable = PeriodicTable)
    })


    the_latex <-   reactive({

      input_general_resolution <- GeneralLaTeX(input_atomic_number1 = input$atomic_number1,
                                               input_valence1 = input$valence1,
                                               input_atomic_number2 = input$atomic_number2,
                                               input_valence2 = input$valence2,
                                               input_family = input$chemestry_family,
                                               input_internal_language = "en",
                                               input_external_language = "en",
                                               input_PeriodicTable = PeriodicTable)


      input_general_resolution[[2]]
    })


    the_jax <-   reactive({
      
      input_latex <- GeneralLaTeX(input_atomic_number1 = input$atomic_number1,
                                               input_valence1 = input$valence1,
                                               input_atomic_number2 = input$atomic_number2,
                                               input_valence2 = input$valence2,
                                               input_family = input$chemestry_family,
                                               input_internal_language = "en",
                                               input_external_language = "en",
                                               input_PeriodicTable = PeriodicTable)[[1]]
      
      
 
      
      GeneralJax(input_table = input_latex )
      
      
    })
    
    the_jax2 <-   reactive({
      
      input_latex2 <- GeneralLaTeX(input_atomic_number1 = input$atomic_number1,
                                  input_valence1 = input$valence1,
                                  input_atomic_number2 = input$atomic_number2,
                                  input_valence2 = input$valence2,
                                  input_family = input$chemestry_family,
                                  input_internal_language = "en",
                                  input_external_language = "en",
                                  input_PeriodicTable = PeriodicTable)[[2]]
      
      
      
      
      GeneralJax2(input_table = input_latex2 )
      
      
    })
    
    nomenclature <- reactive({

      GeneralNomenclature(input_atomic_number1 = as.numeric(as.character(input$atomic_number1)),
                          input_valence1 = as.numeric(as.character(input$valence1)),
                          input_atomic_number2 = as.numeric(as.character(input$atomic_number2)),
                          input_valence2 = as.numeric(as.character(input$valence2)),
                          input_family = input$chemestry_family,
                          input_internal_language = "en",
                          input_external_language = input$selected_language,
                          input_PeriodicTable = PeriodicTable,
                          input_Nomenclature = Nomenclature)





          })


    chemical_formule <-  reactive({
      
      input_formule <- ChemicalFunction(input_atomic_number1 = input$atomic_number1,
                                               input_valence1 = input$valence1,
                                               input_atomic_number2 = input$atomic_number2,
                                               input_valence2 = input$valence2,
                                               input_family = input$chemestry_family,
                                               input_internal_language = "en",
                                               input_external_language = "en",
                                               input_PeriodicTable = PeriodicTable)
      
      
      input_formule
    })

  ###
  } # End General Resolution
  ####################################################################


  # Labels and options (Reactive)
  {
  ###




  ###
  } # End Labels and options (Reactive)
  ##############################################################################



  # Language Update
  ###### Coming soon


  # Chemestry Family (Update)
  observe({

    # Internal Options (Default - En. For example: Oxide)
    my_family_chem <- PageFamilyOptions[,"chemestry_family"]

    # Visual User options (Default - En)
    user_options <- PageFamilyOptions[,"user_chem_fam"]


    # Translate Visual user options (For example, in Spanish: Oxido)
    names(my_family_chem) <- i18n()$t(user_options)




    # Control the value, min, max, and step.
    updateRadioButtons(session, "chemestry_family",
                       label = i18n()$t("Chemestry Family"),
                       choices = my_family_chem,
                       selected = my_family_chem[1])



  })


  # # Default step (Update)
  # observeEvent(input$selected_language,{
  #
  #     names(my_step_options) <- i18n()$t(names(my_step_options))
  #
  #     # Control the value, min, max, and step.
  #     updateRadioButtons(session, "default_step",
  #                        label = i18n()$t("Default step"),
  #                        choices = my_step_options,
  #                        selected = my_step_options[1])
  #   })






  # Steps - Reactive objects
  {
  ###

    # Inititating reactive values, these will `reset` for each session
    # These are just for counting purposes so we can step through the questions
    # Cantidad de pasos

    step_manager <- reactive({
      
      my_names <- names(PageHelperLevel)
      first_step <- rep(1, length(my_names))
      final_step <- rep(NA, length(my_names))
      for (k in 1:length(my_names)){
        
        final_step[k] <- nrow(PageHelperLevel[[my_names[k]]][["en"]])
      }
      
      equation_step <- final_step + 1
      nomenclature_step <- equation_step + 1
      
      my_table <- rbind(first_step, final_step, equation_step, nomenclature_step)
      colnames(my_table) <- my_names
      my_table
      
    })
    
    # Total steps of the stoichiometry
    total_stoichiometry_step <- reactive({
      # # # nrow(the_latex())
       # nrow(PageHelperLevel[[input$chemestry_family]][["en"]])
      step_manager()[2,input$chemestry_family]
    })

    # Vector for each step in stoichiometry
    vector_stoichiometry <- reactive({

      c(1:total_stoichiometry_step())

    })

    # Total step in page (stoichiometry + Final ecuation + nomenclature)
    vector_slider <- reactive({

    #  extra_step <- 2   # Final Equation and Nomenclature
     # total_steps <- total_stoichiometry_step() + extra_step
      total_steps <- step_manager()[4,input$chemestry_family]
      seq(1, total_steps, by = 1)


    })



    # Step number for final estoichiometry step
    final_step <- reactive({

      # result <- max(vector_slider()) - 2
      # result
      # nrow(PageHelperLevel[[input$chemestry_family]][["en"]])
      step_manager()[2,input$chemestry_family]
    })


    # Step number for final equation step
    equation_step <- reactive({

      #result <- max(vector_slider()) - 1
      result <- step_manager()[3,input$chemestry_family]
      result
    })

    # Step number for nomenclature
    nomenclature_step <- reactive({

      #max(vector_slider())
      step_manager()[4,input$chemestry_family]

    })


    # Default step selection
    default_step_selection <- reactive({


      c(1, equation_step(), nomenclature_step())

    })


    # Selected Step (Default is first step)
    selected_step <- reactiveVal(1)


  } # End Slider settings
  #########################################################



  # Element Selection - Reactive objects
  {
  ###



    # Reactive for order row for Info and details
    RowSelectedInfo <- reactive({



       #all_pos <- rep(NA, 3)
      # all_pos <- rep(1, 3)
      all_pos <- c(5,5,1)
      names(all_pos) <- c("01", "02", "Generic")

      dt_family <- PageFamilyOptions$chemestry_family ==  input$chemestry_family
      dt_hydroxide <- PageFamilyOptions$chemestry_family == "Hydroxide"
      dt_oxacid <- PageFamilyOptions$chemestry_family == "Oxacid"
      dt_hydracid <- PageFamilyOptions$chemestry_family == "Hydracid"
      order <- PageFamilyOptions$Order

      # Default values por 01 and generic
      all_pos[1] <- order[dt_family]
      all_pos[3] <- order[dt_family]

      # Especial conditions
      if (input$chemestry_family == "Oxosalt") {
        all_pos[1] <- order[dt_hydroxide]
        all_pos[2] <- order[dt_oxacid]
      } else if (input$chemestry_family == "Salt") {
        all_pos[1] <- order[dt_hydroxide]
        all_pos[2] <- order[dt_hydracid]
      }

      all_pos



    })

    InternalFamily <- reactive({

      PageFamilyOptions$chemestry_family[RowSelectedInfo()]

    })

    LabelForSelector <- reactive({

      # Initial values
      my_labels <- rep(NA, 2)

      # Defual values
      my_labels[1] <- PageFamilyOptions[RowSelectedInfo()[1], "selector_label_01"]
      my_labels[2] <- PageFamilyOptions[RowSelectedInfo()[2], "selector_label_01"]

      if (InternalFamily()[3] == "Oxosalt") {
        my_labels[1] <- PageFamilyOptions[RowSelectedInfo()[1], "selector_label_02"]
        my_labels[2] <- PageFamilyOptions[RowSelectedInfo()[2], "selector_label_02"]
      }

      if (InternalFamily()[3] == "Salt") {
        my_labels[1] <- PageFamilyOptions[RowSelectedInfo()[1], "selector_label_03"]
        my_labels[2] <- PageFamilyOptions[RowSelectedInfo()[2], "selector_label_03"]
      }

      # Exit
      my_labels
    })


    # Reacitve for Element Selection 1 and 2
    Selection01 <- reactive({

      my_family <- InternalFamily()[1]
      dt_selected <- as.logical(as.character(InteligentSelection[,my_family]))
      dt_selected

    })

    Selection02 <- reactive({

      my_family <- InternalFamily()[2]
      dt_selected <- as.logical(as.character(InteligentSelection[,my_family]))
      dt_selected

    })



    # Reacitve for SAN to EP 1 and 2
    # SAN: Start Atomic Number
    # EP: Element Position
    SAN2EP01 <- reactive({

      the_start_atomic_number <- PageFamilyOptions[RowSelectedInfo()[1], "start_atomic_number"]

      all_sybols <- PeriodicTable[["en"]][, "Symbol"]
      the_symbol <- all_sybols[the_start_atomic_number]

      selected_symbols <- all_sybols[Selection01()]
      dt_pos <- selected_symbols == the_symbol

      new_pos <- c(1:length(dt_pos))[dt_pos]
      new_pos


    })
    SAN2EP02 <- reactive({

      the_start_atomic_number <- PageFamilyOptions[RowSelectedInfo()[2], "start_atomic_number"]

         all_sybols <- PeriodicTable[["en"]][, "Symbol"]
         the_symbol <- all_sybols[the_start_atomic_number]

         selected_symbols <- all_sybols[Selection02()]
         dt_pos <- selected_symbols == the_symbol

         new_pos <- c(1:length(dt_pos))[dt_pos]
         new_pos

    })

  ###
  }
  #############################################################


  # Element Selection - Reactive objects (Update)
  observe({

      combinated_options01 <- Elements_Info[[input$selected_language]][Selection01()]


      updateSelectInput(session, "atomic_number1",
                    #     label = i18n()$t("Selection 01"),
                          label = i18n()$t(LabelForSelector()[1]),
                         choices = combinated_options01,
                         selected = combinated_options01[SAN2EP01()])



      if(input$chemestry_family == "Oxosalt" | input$chemestry_family == "Salt") {


        combinated_options02 <- Elements_Info[[input$selected_language]][Selection02()]


        updateSelectInput(session, "atomic_number2",
                          #label = i18n()$t("Selection 02"),
                          label = i18n()$t(LabelForSelector()[2]),
                          choices = combinated_options02,
                          selected = combinated_options02[SAN2EP02()])


      }


    })



  # Element Seleccion - Text
  {
  ###


  # Text01 for ES 01 and 02
  output$fc_ES01_text01 <- renderText({

    if (!is.null(RowSelectedInfo()[1])) {

      i18n()$t(PageFamilyOptions[RowSelectedInfo()[1], "text_01"])

    } else return (NULL)

    })

    output$fc_ES02_text01 <- renderText({

      if (!is.null(RowSelectedInfo()[2])) {

      i18n()$t(PageFamilyOptions[RowSelectedInfo()[2], "text_01"])
      } else return (NULL)
    })

    # Text02 for ES 01 and 02
    output$fc_ES01_text02 <- renderText({

      if (!is.null(RowSelectedInfo()[1])) {

      i18n()$t(PageFamilyOptions[RowSelectedInfo()[1], "text_02"])
    } else return (NULL)

    })
    output$fc_ES02_text02 <- renderText({
      if (!is.null(RowSelectedInfo()[2])) {

      i18n()$t(PageFamilyOptions[RowSelectedInfo()[2], "text_02"])
      } else return (NULL)
    })



    ###
  }
  ########################################################






  # Slider (Update)
  {
  ###

    # Slide Update for a new chemestryr family
    observeEvent(vector_slider(),{

      # Reset selected step
      selected_step(1)

     # Control the value, min, max, and step.
     updateSliderInput(session, "slider",
                       label = i18n()$t("Slider"),
                       value = selected_step(),
                       min = min(vector_slider()), max = max(vector_slider())
     )


    })





    # Slide Update for a new step
    observe({


      # Control the value, min, max, and step.
      updateSliderInput(session, "slider",
                        label = i18n()$t("Slider"),
                        value = selected_step(),
                        min = min(vector_slider()), max = max(vector_slider())
      )


    })






    # Si aprieta el boton "Inicio"
    observeEvent(input$atomic_number1,{
      selected_step(1)

    })


    # Si aprieta el boton "Inicio"
    observeEvent(input$atomic_number2,{
      selected_step(1)

    })

    # Si aprieta el boton "Inicio"
    observeEvent(input$b1, {
      selected_step(1)

    })


    # Si aprieta el boton "Inicio"
    observeEvent(input$valence1,{
      selected_step(1)

    })



    # Si aprieta el boton "Inicio"
    observeEvent(input$valence2,{
      selected_step(1)

    })


    # Si aprieta el bonton "Prev"
    observeEvent(input$b2, {
      if (selected_step() > 1) selected_step(selected_step() - 1)


    })

    # Si aprieta el bonton "Next"
    observeEvent(input$b3, {

      if (selected_step() < length(vector_slider()))  selected_step(selected_step() + 1)




    })

    # Si aprieta el boton "Resolucion"
    observeEvent(input$b4, {
      selected_step(max(vector_slider())-1)

    })

    # Si aprieta el boton "Nomenclatura"
    observeEvent(input$b5, {
      selected_step(max(vector_slider()))

    })

    # Si cambia el Slider
    observeEvent(input$slider, {
      selected_step(input$slider)

    })


  }
  ########################################################





  # Valences Upgrade
  {
  ###

     observe({


      # Control the value, min, max, and step.
      updateRadioButtons(session, "valence1",
                         label = i18n()$t("Valence 01"),
                         choices = my_valence[[as.numeric(input$atomic_number1)]],
                         selected = my_valence[[as.numeric(input$atomic_number1)]][1])

      updateRadioButtons(session, "valence2",
                         label = i18n()$t("Valence 02"),
                         choices = my_valence[[as.numeric(input$atomic_number2)]],
                         selected = my_valence[[as.numeric(input$atomic_number2)]][1])


    })


  ###
  }
  ########################################################




  # Helper
  {
  ###


    HelperTable <- reactive({

      # Part 01
      my_rows <- vector_stoichiometry()
      part01 <- paste0(my_rows, " ", i18n()$t("of"), " ", final_step())


      armed01 <- paste0(InternalFamily()[3] , "_help01")
      armed02 <- paste0(InternalFamily()[3] , "_help02")
      armed03_YES <- paste0(InternalFamily()[3] , "_help03_YES")
      armed03_NO <- paste0(InternalFamily()[3] , "_help03_NO")
      all_armed <- c(armed01, armed02, armed03_YES, armed03_NO)


      part02 <- PageHelperLevel[[input$chemestry_family]][[input$selected_language]][my_rows, all_armed]


      part03 <- cbind(part01, part02)


      part04 <- rep(i18n()$t("Final Equation"), ncol(part03))
      part04[1] <- equation_step()

      part05 <- rep(i18n()$t("Nomenclature"), ncol(part03))
      part05[1] <- nomenclature_step()

      part06 <- rbind(part03, part04, part05)
      part06
    })

    HelperLevel01 <- reactive({

    # Part 01
    my_rows <- vector_stoichiometry()
   # part01 <- paste0(my_rows, " ", i18n()$t("of"), " ", final_step())
    part01 <- HelperTable()[my_rows,1]
    part02 <- paste0("Step ", part01)


    #Exit
    exit <- list(part01, part02)
    exit

  })

    HelperLevel02 <- reactive({

    # Part 01
    my_rows <- vector_stoichiometry()

    part01 <- paste0(HelperLevel01()[[2]], " - ", HelperTable()[,2])

    part01



  })

    HelperLevel04 <- reactive({

     GeneralHelp04(input_atomic_number1 = input$atomic_number1,
                                          input_valence1 = input$valence1,
                                          input_atomic_number2 = input$atomic_number2,
                                          input_valence2 = input$valence2,
                                          input_family = input$chemestry_family,
                                          input_internal_language = "en",
                                          input_external_language = input$selected_language,
                                          input_PeriodicTable = PeriodicTable,
                                          input_Helper = PageHelperLevel)





  })


    HelperTable_Mod <- reactive({

    # HelperTable()
    my_rows <- vector_stoichiometry()
    my_colnames <- i18n()$t(c("Step", "Resumen", "General details", "Specific details"))

    # part01 <- HelperTable()
    part01 <- HelperTable()[,c(1:length(my_colnames))]
    colnames(part01) <- my_colnames


    new_help04 <- HelperLevel04()

    part01[my_rows, ncol(part01)] <- new_help04
    # if(input$help_level < 3) part01[,3] <- rep("Help Level 3", nrow(part01))
    # if(input$help_level < 4) part01[,4] <- rep("Help Level 4", nrow(part01))

      part01

  })


    HelperLevel01_Mod <- reactive({

      RegularText(input_text = HelperLevel01()[[2]], max_n = 6)

      })

    HelperLevel02_Mod <- reactive({

      RegularText(input_text = HelperLevel02(), max_n = 6)
      })

    HelperLevel03_Mod <- reactive({

      RegularText(input_text = HelperTable_Mod()[,3], max_n = 6)

      })

    HelperLevel04_Mod <- reactive({

      RegularText(input_text = HelperTable_Mod()[,4], max_n = 6)

    })


    HelperTable_ModV2 <- reactive({
      
      my_rows <- c(1:nrow(the_jax()), rep(nrow(the_jax()), 2)) 
      new_table <- cbind(HelperTable_Mod(), the_jax()[my_rows, 2])
      colnames(new_table)[ncol(new_table)] <- colnames(the_jax())[2]
      new_table
    })
    

  # HelperLevel03 <- reactive({
  #
  #   HelperTable_Mod()[3]
  #
  # })

  #
  # HelperLevel04 <- reactive({
  #
  #   HelperTable_Mod()[,3]
  #
  # })


  ###
  }
  #########################################






  # Table Helper Level
  output$fc_HL_Table <- renderTable({




    # if (input$help_level <= 4) HelperTable_Mod()[selected_step(), ]
    #   else HelperTable_Mod()
 the_last <- ncol(HelperTable_ModV2())
 
    if (input$help_level == 1) HelperTable_ModV2()[,c(1,  the_last)]
     else if (input$help_level == 2) HelperTable_ModV2()[,c(1,2,  the_last)]
        else if (input$help_level == 3) HelperTable_ModV2()[,c(1:3,  the_last)]
          else if (input$help_level == 4) HelperTable_ModV2()[,c(1:4,  the_last)]
   
  })





  output$resolution_plot <- renderPlot(width = 1600, height = 450,{


    if(!is.null(input$selected_language)) {

      if (selected_step() <= final_step() ) {
    GeneralPlot( input_atomic_number1 = input$atomic_number1,
                 input_valence1 = input$valence1,
                 input_atomic_number2 = input$atomic_number2,
                 input_valence2 = input$valence2,
                 input_family = input$chemestry_family,
                 input_roman = TRUE,
                 input_step = selected_step(),
                 input_internal_language = "en",
                 input_external_language = "en",
                 input_PeriodicTable = PeriodicTable)

        # Level 1 or 2
        if(input$help_level == 1) text(x = 1, y = 28,
                                       labels = HelperLevel01_Mod()[selected_step()],
                                       adj = c(0,1), cex = 4)


        else if(input$help_level >= 2)  text(x = 1, y = 28,
                                             labels = HelperLevel02_Mod()[selected_step()],
                                             adj = c(0,1), cex = 4)
      } else if (selected_step() == equation_step()) {

        GeneralPlot( input_atomic_number1 = input$atomic_number1,
                     input_valence1 = input$valence1,
                     input_atomic_number2 = input$atomic_number2,
                     input_valence2 = input$valence2,
                     input_family = input$chemestry_family,
                     input_roman = FALSE,
                     input_step = selected_step()-1,
                     input_internal_language = "en",
                     input_external_language = "en",
                     input_PeriodicTable = PeriodicTable)


       text(15, 25, i18n()$t("Final Equation"), cex = 7)

      } else if (selected_step() == nomenclature_step()) {
        plot(c(0:30), c(0:30), axes=F, col="orange", xlab=" ", ylab=" ")
        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "orange", border = "orange")

        text(-0.5, 25, i18n()$t("Nomenclature"), cex = 7, pos = 4)

        armed01 <- nomenclature()[2,1]
        armed02 <- nomenclature()[2,2]
        armed03 <- nomenclature()[2,3]
        armed04 <- nomenclature()[3,4]
        armed05 <- paste0("expression(", nomenclature()[1,4], ")")

        text(0, 17, armed01, cex = 2, pos = 4)
        text(0, 11, armed02, cex = 2, pos = 4)
        text(0,  5, armed03, cex = 2, pos = 4)
        text(22, 25, armed04, cex = 7)
        text(22, 10, eval(parse(text=armed05)), cex = 10)

      }

    } else return(NULL)

  })




  output$tabla_nomenclatura <- renderTable({

    aver <- nomenclature()[1,]
    aver[4] <-  chemical_formule()
    aver
  })


  ###############################################################################


  output$resolution_plot_V2 <- renderPlot(width = 1600, height = 450,{



    if (selected_step() <= final_step() ) {
      GeneralPlot( input_atomic_number1 = input$atomic_number1,
                   input_valence1 = input$valence1,
                   input_atomic_number2 = input$atomic_number2,
                   input_valence2 = input$valence2,
                   input_family = input$chemestry_family,
                   input_roman = TRUE,
                   input_step = selected_step(),
                   input_internal_language = "en",
                   input_external_language = "en",
                   input_PeriodicTable = PeriodicTable)


      text(0, 0, "Plot V2", cex = 4)




      # # # # Headers
      {
      ###

      cex_1 <- 2
      ycoord_1 <- 28

      # Helper Level 1 and 2
      if(input$help_level == 1)    text(x = -1, y = ycoord_1,
                                        labels = colnames(HelperTable_Mod())[1],
                                        adj = c(0,0), cex = cex_1)

      else if(input$help_level >= 2)    text(x = -1, y = ycoord_1,
                                             labels = colnames(HelperTable_Mod())[2],
                                             adj = c(0,0), cex = cex_1)

      # # Level 3
      if(input$help_level >= 3)    text(x = 5, y = ycoord_1,
                                        labels = colnames(HelperTable_Mod())[3],
                                        adj = c(0,0), cex = cex_1)

      # # Level 4
      if(input$help_level >= 4)    text(20, 28, colnames(HelperTable_Mod())[4],
                                        adj = c(0,0), cex = cex_1)

      ###
      } # End Header
      #######################################################################################



      # # # # Text
      {
      ###

      cex_2 <- 1
      ycoord_2 <- 27



      # Level 1 or 2
      if(input$help_level == 1) text(x = -1, y = ycoord_2,
                                     labels = HelperLevel01_Mod()[selected_step()],
                                     adj = c(0,1), cex = cex_2)


      else if(input$help_level >= 2)  text(x = -1, y = ycoord_2,
                                           labels = HelperLevel02_Mod()[selected_step()],
                                           adj = c(0,1), cex = cex_2)

      # # Level 3
      if(input$help_level >= 3)    text(x = 5, y = ycoord_2,
                                       labels = HelperLevel03_Mod()[selected_step()],
                                        adj = c(0,1), cex = cex_2)

      # # Level 4
      if(input$help_level >= 4)    text(x = 20, y = ycoord_2,
                                        labels =  HelperLevel04_Mod()[selected_step()],
                                        adj = c(0,1), cex = cex_2, col = "blue")


      ###
      } # End Text
      #######################################################################################


    } else if (selected_step() == equation_step()) {

      GeneralPlot( input_atomic_number1 = input$atomic_number1,
                   input_valence1 = input$valence1,
                   input_atomic_number2 = input$atomic_number2,
                   input_valence2 = input$valence2,
                   input_family = input$chemestry_family,
                   input_roman = FALSE,
                   input_step = selected_step()-1,
                   input_internal_language = "en",
                   input_external_language = "en",
                   input_PeriodicTable = PeriodicTable)


      text(15, 25, i18n()$t("Final Equation"), cex = 7)

    } else if (selected_step() == nomenclature_step()) {
      plot(c(0:30), c(0:30), axes=F, col="orange", xlab=" ", ylab=" ")
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "orange", border = "orange")

      text(-0.5, 25, i18n()$t("Nomenclature"), cex = 7, pos = 4)

      armed01 <- nomenclature()[2,1]
      armed02 <- nomenclature()[2,2]
      armed03 <- nomenclature()[2,3]
      armed04 <- nomenclature()[3,4]
      armed05 <- paste0("expression(", nomenclature()[1,4], ")")

      text(0, 17, armed01, cex = 2, pos = 4)
      text(0, 11, armed02, cex = 2, pos = 4)
      text(0,  5, armed03, cex = 2, pos = 4)
      text(22, 25, armed04, cex = 7)
      text(22, 10, eval(parse(text=armed05)), cex = 10)

    }


  })


 
  


  output$text_V1 <- renderText({

    my_text <- i18n()$t("Version 1 - Steichiometry on plot and Help Table")
    my_text
  })

  output$text_V2 <- renderText({

    my_text <- i18n()$t("Version 2 - Steichiometry and Help all in Plot")
    my_text
  })

  output$text_V3 <- renderText({

    my_text <- i18n()$t("Help Table")
    my_text
  })

  output$text_V4 <- renderText({

    my_text <- i18n()$t("Nomenclature Table")
    my_text
  })
  
  ########################################################
  
  output$ex1 <- renderTable({  
    HelperTable_ModV2()[selected_step(),]
    })
    
  output$ex2 <- renderUI({
    
    the_step <- selected_step()
    if (the_step > final_step()) the_step <- final_step()
    
    my_row <- the_jax()[the_step,2]
    

    fluidRow(

             h1(withMathJax(my_row))
  
    )
   
    
  })
  
 ############################################################
  
  output$exit01 <- renderUI({
    
    the_step <- selected_step()
    if (the_step > final_step()) the_step <- final_step()
    
    my_row <- the_jax()[the_step,2]
    

    fluidRow(
      i18n()$t("Chemical Equation"), br(),
      h1(withMathJax(my_row))
      
    )
    
    
  })
  
  output$exit02 <- renderUI({
    
    my_chemical_formule <- chemical_formule()
    
     
    fluidRow(
      i18n()$t("Chemical Formule"), br(),
      h1(withMathJax(my_chemical_formule))
      
    )
    
    
  })
  
  output$exit03 <- renderTable({
    
    aver <- nomenclature()[1,]
    
    aver[4] <-  chemical_formule()
    aver
 
    
    
  })
  
  
  
  output$resolution_plot_V3 <- renderPlot({
    
    
    the_step <- selected_step()
    if (the_step > final_step()) the_step <- final_step()
    
    my_row <- the_jax2()[the_step,2]
    my_main <- paste0("Step ", HelperTable_Mod()[selected_step(),1], " - ", HelperTable_Mod()[selected_step(),2])
    
  #  cat("my_main: ", my_main,"\n")
  #  if (selected_step() == nomenclature_step()) my_row <- paste0("expression(", gsub("[$$]", "", chemical_formule()), ")")
    
      
# opar <- par()
#  par(mar = c(0, 0, 0, 0))
    plot(-1, 0, ylim = c(0,2), xlim = c(0,10), col ="white",
         axes = F, xlab = "", ylab = "", main = my_main)
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "orange", border = "orange")
    
  #  par(opar)
    #text(5, 1, expression(A  *symbol('\256')*  B), cex = 6)
    

    #aver <- "expression(4*Li*'+'*phantom(1)*O[2]*symbol('\\256')*2*Li[2]*O)"
    text(0, 1, eval(parse(text=my_row)), cex = 3, pos = 4)
    
    
  })
  
  
  
  
  output$exit04 <- renderText({
    
    the_step <- selected_step()
    if (the_step > final_step()) the_step <- final_step()
    
    my_row <- the_jax2()[the_step,2]
    
    my_row
    
    
  })
  
  
  output$table_fullhelper <- renderTable({
    
    HelperTable_ModV2()
  })
  
 # HelperTable_ModV2
  
}) # End shinySever()