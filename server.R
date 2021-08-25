
# options(encoding = 'UTF-8')
# options(encoding = 'latin1')

# options(encoding = 'latin1')
# Source initial UI settings
source("lib.R")



# Here we create our translator ...
translator <- Translator$new(translation_csvs_path = "data/TranslatePagei18n/")

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
    
    my_equation <- reactive({
      
      GeneralEquation(input_atomic_number1 = input$atomic_number1,
                      input_valence1 = input$valence1,
                      input_atomic_number2 = input$atomic_number2,
                      input_valence2 = input$valence2,
                      input_family = input$chemestry_family,
                      input_internal_language = "en",
                      input_external_language = "en",
                      input_PeriodicTable = PeriodicTable)
    })
    
    
    my_latex <-   reactive({
      
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
    

  ###  
  } # End General Resolution
  ####################################################################
  
  
  # Chemestry Family
  {
    ###
    
    observe({
      
      # Internal Options
      my_family_chem <- ChemestryFamily[,"en"]
      
      # Visual User options (Default - En)
      user_options <- my_family_chem
      
      # Translate Visual user options
      names(my_family_chem) <- i18n()$t(user_options)
      
      # Control the value, min, max, and step.
      updateRadioButtons(session, "chemestry_family", 
                         label = i18n()$t("Chemestry Family"),
                         choices = my_family_chem,
                         selected = my_family_chem[1])
    })
      
    
    observe({
      # Information for reactive() in server
      my_atomic_numbers <- PeriodicTable[[input$selected_language]][,1]
      my_atomic_numbers_mod <- as.character(my_atomic_numbers)
      my_count <- str_count(my_atomic_numbers_mod)
      my_atomic_numbers_mod[my_count == 1] <- paste0("  ", my_atomic_numbers_mod[my_count == 1])
      my_atomic_numbers_mod[my_count == 2] <- paste0(" ", my_atomic_numbers_mod[my_count == 2])
      
      my_symbols <- PeriodicTable[[input$selected_language]][,2]
      my_symbols_mod <-as.character(my_symbols)
      my_count2 <- str_count(my_symbols_mod)
      my_symbols_mod[my_count2 == 1] <- paste0(my_symbols_mod[my_count2 == 1], " ")
      my_symbols_mod[my_count2 == 2] <- paste0(my_symbols_mod[my_count2 == 2], "  ")
      
      my_names <- PeriodicTable[[input$selected_language]][,3]
      my_valence <- strsplit(PeriodicTable[[input$selected_language]][,10], ";")
      combinated_options <- my_atomic_numbers
      names(combinated_options) <- paste0(my_atomic_numbers_mod, " - ",my_symbols_mod , " - ", my_names) 
      
      updateSelectInput(session, "atomic_number1",
                         label = i18n()$t("Selection 01"),
                         choices = combinated_options,
                         selected = combinated_options[26])
      
      
      if(input$chemestry_family == "Oxosalt" | input$chemestry_family == "Salt") {
        
        updateSelectInput(session, "atomic_number2",
                          label = i18n()$t("Selection 02"),
                          choices = combinated_options,
                          selected = combinated_options[7])
        
      }
      
      
    })
    
    
    ###  
  }
  ########################################################
  
  
  
  
  # Slider settings
  {
    
    # Inititating reactive values, these will `reset` for each session
    # These are just for counting purposes so we can step through the questions
    # Cantidad de pasos
    vector_slider <- reactive({
      
        extra_step <- 2
        total_steps <- nrow(my_latex()) + extra_step
        
        seq(1, total_steps, by = 1)
  
      
    })
    
    
    
    # Imagen elegida para mostrar "Paso a Paso Imagenes"
    selected_step <- reactiveVal(1)


    nomenclature_step <- reactive({ 
      
      max(vector_slider())
      
      
    })
    
    final_step <- reactive({ 
      
      result <- max(vector_slider()) - 2
      result
    })
    
    equation_step <- reactive({ 
      
      result <- max(vector_slider()) - 1
      result
    })

    
    
  } # End Slider settings
  #########################################################
  
  
  # Slider Upgrade
  {
  ###
    
    # Slide Update
    observe({
      
    #  selected_step(max(vector_slider() - 2))
                    
      # Control the value, min, max, and step.
      updateSliderInput(session, "slider", value = selected_step(),
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
  
  
 
  output$resolution_plot <- renderPlot(width = 1600, height = 400,{ 
    
  

      if (selected_step() <= final_step() ) { 
    GeneralPlot( input_atomic_number1 = input$atomic_number1,
                 input_valence1 = input$valence1,
                 input_atomic_number2 = input$atomic_number2,
                 input_valence2 = input$valence2,
                 input_family = input$chemestry_family,
                 input_step = selected_step(),
                 input_internal_language = "en",
                 input_external_language = "en",
                 input_PeriodicTable = PeriodicTable)
   
        text(15, 25, paste0("Step ", selected_step(), " of ", (equation_step()-1) ), cex = 4)
        
      } else if (selected_step() == equation_step()) {
        
        GeneralPlot( input_atomic_number1 = input$atomic_number1,
                     input_valence1 = input$valence1,
                     input_atomic_number2 = input$atomic_number2,
                     input_valence2 = input$valence2,
                     input_family = input$chemestry_family,
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
  
  
  output$tabla_nomenclatura <- renderTable({
    
    nomenclature()
    
    
  })
  
  
  

  
  
  

  

  
  
 
  

  
 
 
  

  

  
  

  
  
  
  
  
  
}



) # End shinySever()