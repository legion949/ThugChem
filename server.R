
options(encoding = 'UTF-8')

# options(encoding = 'latin1')
# Source initial UI settings
source("lib.R")



# Here we create our translator ...
translator <- Translator$new(translation_csvs_path = "data/TranslatePage/")

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
      
          })
    
  ###  
  } # End General Resolution
  ####################################################################
  
  
  
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
        text(15, 25, i18n()$t("Nomenclature"), cex = 7)
      }
    
    
  })
  
  
  
  
  
  
  output$tabla1 <- renderTable({
    
    my_equation()
    
    
  })
  
  
  
  
  ###################################################################################
  
  # Todos los textos
  
  # Titulo de la App 
  # idShiny01
  output$texto01_01 <- renderText({ 
    
    dt <- idShiny == "idS01_01"
    details[dt,input$language_selector]

    })  
  
  
  # Munu Lateral - Opcion: "Estequiometria"
  # idShiny02
  output$texto02_01 <- renderText({ 
    
    dt <- idShiny == "idS02_01"
    
    details[dt,input$language_selector]
    
  })  
  
  
  
 
  
  # Titulo de la App 
  # idShiny01
  output$texto06_01 <- renderText({ 
    
    dt <- idShiny == "idS06_01"
    details[dt,input$language_selector]
    
  })  
  
 
 
  
  
  
  output$texto07_01 <- renderText({ 
    
    dt <- idShiny == "idS07_01"
    details[dt,input$language_selector]
    
  })  
  

  
  

  

  
  
if (1 == 2){
  output$graf1 <- renderPlot({
    
    # Controlador 000...
    # Este controlador 000, despues hay que fletarlo...
    # Lo puse para poder poner un grafico que diga "Proximamente..." en las
    # partes que todavia no hice...
    if(!is.null(num_chemfam()))
      if(length(num_chemfam()) > 0) {
   
        
    # Controlador 000-01
        if (num_chemfam() == 1) {
          
           
          if(!is.null(selected_language()))
            if(!is.null(PasosResolucion()))
              if (dim(PasosLaTeX())[1] > 1) 
                if (dim(PasosLaTeX())[2] > 1) 
                  if(!is.null(selected_step()))
                    if(!is.null(vector_slider()))
                      if(!is.null(input$helper))
          if(selected_step() < length(vector_slider())) {
            
            
            input_numfam <- num_chemfam()
            input_ejemplo <- PasosResolucion()
            input_language_interno <- "es"
            input_language_optativo <-selected_language()
            input_paso <- selected_step()
            input_total <- nrow(input_ejemplo)
            input_subtitulos <- subtitulos
            input_tabla <- PeriodicTable
            
            mis_subtitulos <- SubtitulosGeneral(
                                  input_numfam = input_numfam,
                                  input_ejemplo = input_ejemplo, 
                                  input_language_interno = input_language_interno, 
                                  input_language_optativo = input_language_optativo, 
                                  input_subtitulos = input_subtitulos, 
                                  input_tabla = input_tabla,
                                  input_paso = input_paso,
                                  input_total = input_total)
          
            
            # Grafico
            GrafGeneral(input_numfam = num_chemfam(), 
                        input_latex = PasosLaTeX(),
                        input_paso = selected_step(),
                        input_language_interno = "es",
                        input_language_optativo = NULL,
                        input_tabla = PeriodicTable,
                        input_color_fondo = "orange",
                        input_color_general = "black",
                        input_color_especifico = "blue",
                        input_color_ecuacion = "black",
                        input_color_signo = "black",
                        input_color_paso = "blue")
          
            
            
          # Paso...
          if(input$helper >= 1) text(mis_subtitulos[[1]][1], mis_subtitulos[[1]][2], mis_subtitulos[[1]][3], pos=1, cex=1.3, adj = c(0.5, 0.5)) 
          
          # Frase general...
          if(input$helper >= 2)text(mis_subtitulos[[2]][1], mis_subtitulos[[2]][2], mis_subtitulos[[2]][3], pos=1, cex=1.3, adj = c(0.5, 0.5)) 
          
          # Frase especifica...
          if(input$helper >= 3)text(mis_subtitulos[[3]][1], mis_subtitulos[[3]][2], mis_subtitulos[[3]][3], pos=1, cex=1.3, col="blue", adj = c(0.5, 0.5)) 
          
          
       #   text(gps_x2[input_paso], gps_y2[input_paso], salida_sub2, pos=1, cex=1.3, adj = c(0.5, 0.5))
          
          # Frase especifica...
       #   text(gps_x3[input_paso], gps_y3[input_paso], salida_sub3, pos=1, cex=1.3, adj = c(0.5, 0.5), col="blue")
          
          } # Fin Parte5
            ###########################################################
          
          
          # Caso 2... Si le tiramos por la cabeza la nomenclatura...
          else if(selected_step() == length(vector_slider())){
            
            if (selected_language() == "es") if(!is.null(Nomenclatura())) {
            
              # Pintada de fondo
              plot(c(0:30), axes=F, col="white", xlab=" ", ylab=" ")
              rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "orange")
              
              # Detalles de nomenclatura
              nomenclatura_completa <- Nomenclatura()
              
              # Coordenadas
              x_nomen <- c( 4,  4,  4,  4, 12)
              y_nomen <- c(25, 19, 13,  7,  7)
              
              
              nombre_uipac <- paste0(names(nomenclatura_completa)[1],": ", nomenclatura_completa[1])
              nombre_clasico <- paste0(names(nomenclatura_completa)[2],": ", nomenclatura_completa[2])
              nombre_stock <- paste0(names(nomenclatura_completa)[3],": ", nomenclatura_completa[3])
              intro_fq <- "Fórmula Química: "
              fq <- nomenclatura_completa[4]
              fq_latex <- nomenclatura_completa[5]
              
              armado_fq  <-   parse(text=paste("text(x_nomen[5], y_nomen[5], 
                                            expression(",nomenclatura_completa[5],"),
                                            col='black', cex=2, pos = 4)", collapse=""))
              
              
              
#              frase4 <- paste0(names(nomenclatura_completa)[1],": ", nomenclatura_completa[1])
              text(x_nomen[1],y_nomen[1], nombre_uipac, cex=2, pos = 4)
              text(x_nomen[2],y_nomen[2], nombre_clasico, cex=2, pos = 4)
              text(x_nomen[3],y_nomen[3], nombre_stock, cex=2, pos = 4)
              text(x_nomen[4],y_nomen[4], intro_fq, cex=2, pos = 4)
              eval(armado_fq)
              
              
            }
              
              if (selected_language() != "es") {
            # Buscamos la opcion correcta...
            search.me <- "idS10_01"
            dt10 <- grep(search.me, idShiny)
            my_choices <- as.character(details[dt10,input$language_selector])
            
            # my_choices <- Nomenclatura()[1]
            
            # Creamos un grafico de aviso...
            
           
            
            
            
            plot(1,1, col="white", axes= F, xlab="", ylab="")
            text(1,1, my_choices)
            } 
          }
          
        } 
   
   
               
        if (num_chemfam() == 2) {
          
          
         
          
          # Buscamos la opcion correcta...
          search.me <- "idS09_01"
          dt10 <- grep(search.me, idShiny)
          my_choices <- as.character(details[dt10,input$language_selector])
          
          # Creamos un grafico de aviso...
          plot(1,1, col="white", axes= F, xlab="", ylab="")
          text(1,1, my_choices)
          
        }             
                   
         
        
                
                      
                    } # Fin Controlador 000
    
  })

  

 
  output$tabla4 <- render_tableHTML({
    
    if (!is.null(CantidadesPaso())) {
      
      # Leyenda en botones...
      search.me <- "idS12"
      dt12 <- grep(search.me, idShiny)
      my_choices12 <- details[dt12,input$language_selector]
      remove(search.me)
      
      .afirmativo <- as.character(as.vector(as.matrix(my_choices12[12])))
      .negativo <- as.character(as.vector(as.matrix(my_choices12[13])))
      
    # #f6f6f6
    data <- CantidadesPaso();
    # //Some operations on data
    data %>% 
      tableHTML(rownames = F) %>% 
      add_css_conditional_column(conditional = '==',
                                 value = .negativo,
                                 css = list(c('background-color'),
                                            c('red')),
                                 columns = 1:ncol(data)) %>% 
      add_css_conditional_column(conditional = '==',
                                 value = .afirmativo,
                                 css = list(c('background-color'),
                                            c('green')),
                                 columns = 1:ncol(data)) %>% 
      add_css_conditional_column(conditional = 'between',
                                 between = c(10, 20),
                                 css = list(c('background-color'),
                                            c('lightred')),
                                 columns = 1:ncol(data))
    
    
    }
    
    
  })
  
  
  
  output$tabla5 <- renderTable({
    
    if(!is.null(Nomenclatura())) {
      
      aver <- Nomenclatura()
  
  cat(is.null(aver))
  as.matrix(aver)
    
    }
    
    
  })
  
 
  
}
  
  
  
  
  
  
}



) # End shinySever()