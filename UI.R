shinyUI(
  
  navbarPage("Series de Tiempo",
             navbarMenu("Datos",
                        tabPanel("Cargar",
                                 fileInput("archivo", "Choose CSV File",
                                           multiple = TRUE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")),
                                 checkboxInput("header", "¿ Archivo con Cabecera ?", FALSE)
                                 #Descomentar para que mustre en ambos paneles la gráfica
                                 # ,
                                 # mainPanel(
                                 #   plotOutput("distPlot")
                                 # )
                        )

             ),
             tabPanel("Gráficos",
                      mainPanel(
                        plotOutput("distPlot2")
                      )
             ),
             navbarMenu("Modelos"

                        )
             ),
             navbarMenu("More",
                        tabPanel("Table",
                                 dataTableOutput("table")
                        ),
                        tabPanel("About",
                                 fluidRow(
                                   column(3,
                                          img(class="img-polaroid",
                                              src=paste0("http://upload.wikimedia.org/",
                                                         "wikipedia/commons/9/92/",
                                                         "1919_Ford_Model_T_Highboy_Coupe.jpg"))
                                   )
                                 )
                        )
             )
  #)
  # pageWithSidebar(
  #   
  #   #Titulo de la pagina
  #   headerPanel("Series de Tiempo - Madagascar"),
  #   #Menu del lado izquierdo de la pagina
  #   sidebarPanel(
  #   
  #     #Entrada para la frecuencia de la serie de tiempo
  #     sliderInput("freq", 
  #                 "Frecuencia de la Serie de Tiempo:", 
  #                 min = 1,
  #                 max = 12, 
  #                 value = 4),
  #     #Entrada para el numero de datos de la series aleratoria
  #     sliderInput("obs", 
  #                 "Observaciones de la serie", 
  #                 min = 1,
  #                 max = 1000, 
  #                 value = 500),
  #     tags$hr(),
  #     fileInput("archivo", "Choose CSV File",
  #               multiple = TRUE,
  #               accept = c("text/csv",
  #                          "text/comma-separated-values,text/plain",
  #                          ".csv")),
  #     checkboxInput("header", "¿ Archivo con Cabecera ?", FALSE),
  #     tags$hr()    
  # ),
  # 
  # # Show a plot of the generated distribution
  # mainPanel(
  #   plotOutput("distPlot")
  # )
))