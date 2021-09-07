
# options(encoding = 'UTF-8')
# options(encoding = 'latin1')

# Source initial UI settings
source("lib.R")

armado <- list()
armado[[1]] <- "latin1"
names(armado) <- "fileEncoding"

# File with translations
suppressWarnings(i18n <- Translator$new(translation_csvs_path = "data/TranslatePagei18n/"))

initial_language <- "es"
i18n$set_translation_language(initial_language) # here you select the default translation to display

shinyUI(
  fluidPage(
    withMathJax(),
    shiny.i18n::usei18n(i18n),
    tags$head(includeHTML("www/google-analytics-ThugChem.html")),
  useShinyjs(),
  
  # 0001 - 3 de 3 - Deteccion de lenguaje por defecto....
# # "This is your browser language",
# #  textOutput('your_lang'),
  ##############################################
  
  
  # load page layout
  dashboardPage(title = "ThugChem - EasyChem", 
    skin = "green",
    
    # Encabezado
   # dashboardHeader(title="Biodiversity in National Parks", titleWidth = 300),
   # dashboardHeader(title = details[1,2], titleWidth = 300),
   dashboardHeader(title = "ThugChem - EasyChem" , titleWidth = 300),
    # Lateral
    dashboardSidebar(width = 300,
                     sidebarMenu( 
                       
                      # Language Selector... 
                      #uiOutput("nn_language"),
                       selectInput('selected_language',
                                   i18n$t("Change language"),
                                   choices = i18n$get_languages(),
                                   selected = initial_language),
                                #   selected = i18n$get_key_translation()),
                       selectInput('plot_version',
                                   i18n$t("Plot version"),
                                   choices = c("Version 1", "Version 2", "Version 3"),
                                   selected = "Version 3"),
                       
                      # Menu
                     menuItem(i18n$t("Steichiometry"), tabName = "tab_internal01", icon = icon("home")),
                     menuItem(i18n$t("Instructions"), tabName = "tab_internal02", icon = icon("thumbtack")),
                     menuItem(i18n$t("Contact"), tabName = "tab_internal03", icon = icon("thumbtack"))
                       )
                     
    ), # end dashboardSidebar
  
    
    # Body
    dashboardBody(
      
      tabItems(
        
        # Tab01 1: Steichiometry
        tabItem(tabName = "tab_internal01",
                fluidRow(
                  
                  # Chemestry Family
                  column(2, 
                        radioButtons(inputId = "chemestry_family", 
                               label = i18n$t("Chemestry Family"),
                               choices = PageFamilyOptions[,"chemestry_family"],
                               selected = PageFamilyOptions[,"chemestry_family"][1])
                  ),
                  #
                  # column(2, radioButtons(inputId = "default_step", 
                  #                        label = i18n$t("Default step"),
                  #                        choices = my_step_options,
                  #                        selected = my_step_options[1])
                  #        ),
                  # Atomic Number and Valence 01
                  column(4, 
                        selectInput(inputId = "atomic_number1", 
                              label = i18n$t("Selection 01"),
                              choices = Elements_Info[["en"]],
                              selected = Elements_Info[["en"]][26]),
                        textOutput("fc_ES01_text01"),
                        br(),
                        
                        column(2, 
                        radioButtons(inputId = "valence1", 
                                    label = i18n$t("Valence 01"),
                                    choices = c(0:8),
                                    selected = 3)
                        ),
                        column(3,
                               br(),
                        textOutput("fc_ES01_text02")
                        )
                  ),
                  
                  # Atomic Number and Valence 02
                  column(4, 
                         conditionalPanel(
                           condition = "input.chemestry_family == 'Oxosalt' | input.chemestry_family == 'Salt'",
                         selectInput(inputId = "atomic_number2", 
                                     label = i18n$t("Selection 02"),
                                     choices = Elements_Info[["en"]],
                                     selected = Elements_Info[["en"]][7]),
                         textOutput("fc_ES02_text01"),
                         br(),
                         
                         column(2,
                                radioButtons(inputId = "valence2", 
                                      label = i18n$t("Valence 02"),
                                      choices = c(0:8),
                                      selected = 7)
                                ),
                         column(3,
                                br(),
                         textOutput("fc_ES02_text02")
                                )
                         )
                  ),
                  br()
                ),


       
        fluidRow(
          column(4,
                        actionButton(inputId = "b1", label = i18n$t("Start")),
                        actionButton(inputId = "b2", label = i18n$t("<<<")),
                        actionButton(inputId = "b3", label = i18n$t(">>>")),
                        actionButton(inputId = "b4", label = i18n$t("Final")),
                        actionButton(inputId = "b5", label = i18n$t("Nomenclature"))
                 ),
          column(4,    
                 sliderInput(inputId = "slider", label = i18n$t("Slider"), min=1, 
                             max=10, value=1, step=1, ticks = T)
                 ),
          column(4,
                 conditionalPanel(
                   condition = "input.plot_version == 'Version 1' | input.plot_version == 'Version 2'",
                 sliderInput(inputId = "help_level", label = i18n$t("Help Level"), 
                             min=0, max=5, value=0, step=1)
                 )
          )
                
              ),

        br(), br(),
        
        
        conditionalPanel(
          condition = "input.plot_version == 'Version 1'",
          h1(textOutput("text_V1")),
        plotOutput("resolution_plot"), br(), br(),
        ),
        conditionalPanel(
          condition = "input.plot_version == 'Version 2'",
          h1(textOutput("text_V2")),
          plotOutput("resolution_plot_V2"),
        ),
        conditionalPanel(
          condition = "input.plot_version == 'Version 3'",
         
          fluidRow(
            column(7,
                   h3(i18n$t("Step by step Resolution")),
                   plotOutput("resolution_plot_V3", width = "800px", height = "200px"), 
                  ),
            column(4,
                   h3(i18n$t("Nomenclature")),
                   h3(tableOutput("tabla_nomenclatura"))
                  ),
            ),
          br(),
          fluidRow(
            column(11,
            h3(i18n$t("Help Table")),
            h4(uiOutput('ex1'))
            ),
            column(1)
          ),
          br(),
          h3(uiOutput('exit01')), br(), 
          h3(uiOutput('exit02')),
         
   br(),
   fluidRow(
     column(11,
   h3(i18n$t("Full Help Table")),
   h4(tableOutput("table_fullhelper"))
     ), 
   column(1)
   )
   #       tableOutput("fc_HL_Table"),
    #      tableOutput('ex2'),
        ),

        br(), br(), br(),
      
        br(), br(), 
     
        br(),
     
  
  


        br(), br(), br()),
        tabItem(tabName = "tab_internal02", "Seleccione un elemento químico de la lista y una valencia.", br(), 
                                            "Se desarrollará paso a paso la estequimetría química.", br(),
                                            "Podrá visualizar la resolución con diferente grando de ayuda.", br(),
                                            "Las explicaciones que se dan detallan orientan al estudiente a
                                             entender cómo se está procediento en cada paso.", br(),
                                            "La resolución finaliza con el nombre correcto en las 3 nomenclaturas
                                             químicas más utilizadsa: UIPAC, Clásica y Numeral Stock.", br(),
                                            "La página se encuentra en proceso de traducción a diferentes idiomas."),
      tabItem(tabName = "tab_internal03", "Correo: d.eliaspanigo@gmailcom") 
        
      )
    ) # end dashboardBody
    
  )# end dashboardPage
  
 ) # End fluidpage()
) # End ShinyUI