library(shiny)
library(plotly)
library(shinythemes)
library(DataCombine)
# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("sandstone"),
        navbarPage("Series de tiempo",
             tabPanel("Carga de Datos",
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          # Input: Select a file ----
                          fileInput("file1", "Choose CSV File",
                                    multiple = TRUE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Checkbox if file has header ----
                          checkboxInput("header", "Header", TRUE),
                          
                          
                          selectInput("columns", "Select Column", choices = NULL), # no choices before uploading 
                          tags$hr(),
                          h4("Summary"),
                          verbatimTextOutput("summary"),
                          plotOutput("content")
                          
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          # Output: Data file ----
                          plotlyOutput("histograma"),
                          tableOutput("contents")
                          
                         
                        
                        )
                        
                      )
             ),
             tabPanel("Analisis Serie de Tiempo",
                      sidebarLayout(
                          # Sidebar panel for inputs ----
                          sidebarPanel(
                            radioButtons("tipoSerie", "Tipo de Serie:",
                                         c("Aditiva" = "additive",
                                           "Multiplicativa" = "multiplicative"))
                          ),
                          mainPanel(
                            # Output: Data file ---
                            plotlyOutput("descompuesta"),
                            plotlyOutput("acfGraph"),
                            tableOutput("lag")
                          )
                        )
                     
             ),
             tabPanel("Coming Soon",
                      mainPanel(
                        plotlyOutput("plot"),
                        verbatimTextOutput("event")
                      )
                      
                      
             )
 
  )
)
# Define server logic to read selected file ----
server <- function(input, output , session) {
  
  info2 <- reactive({
    inFile <- input$file1
    # Instead # if (is.null(inFile)) ... use "req"
    req(inFile)
    # Leyendo el csv 
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = ',',
                   quote = '"')
    #Nombre de las columnas
    vars <- names(df)
    updateSelectInput(session, "columns","Select Column", choices = vars)
    return(df)
  })
  
 
  
  #Tabla de Contenido
  output$contents <- renderTable({
    if (is.null(info2()))
      return(NULL)
    columna = head(info2() , n = 10L,addrownums = FALSE)
    return(columna)
  })
  
  #Summary
  output$summary <- renderPrint({
    req(input$columns)
    f <- subset(info2(), select = input$columns)
    summary(f)
  })
  
  #Histo Normal en sideBar
  output$content <- renderPlot({
    req(input$columns)
    f <- info2()
    x    <- unlist(f[,input$columns])
    # Dubujarlo en otro lado con los bins dinamico
    hist(as.numeric(x), breaks = 20, col = 'darkgray', border = 'white',freq = FALSE , main = input$columns ,xlab = input$columns )
    
    #layout(hist,title = "Titulo del Gráfico",
           #xaxis = list(title = input$columns),
           #yaxis = list(title = "Cantidad"))
  })
  
  #bigotes
  output$box <- renderPlot({
    req(input$columns)
    f <- info2()
    x  <- unlist(f[,input$columns])
    serie=ts(x,frequency=12,start=c(1990,1))
    boxplot(serie~cycle(serie))
  })
  
  #histograma plotly pestaña coming soon
  output$plot <- renderPlotly({
    
    f <- info2()
    y  <- unlist(f[,input$columns])
    x=seq(1:length(y))
    plot_ly(x = y,
            type = "histogram",
            histnorm = "probability")
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
  
  #histograma plotly en el main
  output$histograma <- renderPlotly({
    #f = DataFrame de leer el archivo
    req(input$columns)
    f <- info2()
    #Devuelve los valores de la columna seleccionada en el select
    y  <- unlist(f[,input$columns])
    #x <- seq(1:length(y))
    serie=ts(y,frequency=12,start=c(2000,1))
    x <- time(serie)
    descom<-decompose(serie,type="additive")
    descom2<-decompose(serie,type="mult")
    observada <- plot_ly(x = x, y = serie, type = 'scatter', mode = 'lines',
            line = list(color = '#000080'))
            # ,
            # xaxis='Tiempo',
            # yaxis=input$columns)
    layout(observada,title = "Serie de Tiempo Observada",
           xaxis = list(title = "Tiempo"),
           yaxis = list(title = input$columns))
  })

###################################################################################################
  #Pestaña de Analisis de Series de Tiempo
  
  output$descompuesta <- renderPlotly({
    #f = DataFrame de leer el archivo
    req(input$columns)
    f <- info2()
    #Devuelve los valores de la columna seleccionada en el select
    y  <- unlist(f[,input$columns])
    #x <- seq(1:length(y))
    serie=ts(y,frequency=12,start=c(1990,1))
    descom<-decompose(serie,type=input$tipoSerie)
    x <- time(serie)
    #p <- plot_ly(x = x, y = descom$x, type = 'scatter', mode = 'lines', line = list(color = '#000080'))

    observada <- plot_ly(x = x, y = descom$x, name=input$columns, type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
    estacional <- plot_ly(x = x, y = descom$seasonal, name="Estacional", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
    tendencia <- plot_ly(x = x, y = descom$trend, name="Tendencia", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
    residual <- plot_ly(x = x, y = descom$random, name="Residual", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
    
    myDf <- slide(f, input$columns, NewVar = "Lag", slideBy = -1)  # create lag1 variable
    myDf <- slide(f, input$columns, NewVar = "Lead", slideBy = 1)  # create lead1 variable
    
    lag1 <- plot_ly(x = x, y = myDf$Lag  , name="Lag", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
    
    p <- subplot(observada, estacional,tendencia,residual,lag1,nrows = 5)
    
    #p <- add_trace(observada, x = x, y = descom$x, name=input$columns, type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
    #p <- add_trace(p, x = x, y=descom$trend, name="Tendencia", type = 'scatter', mode = 'lines', line = list(color = '#000800'))
    #p <- add_trace(p, x = x, y=descom$seasonal, name="Estacional", type = 'scatter', mode = 'lines', line = list(color = '#008000'))
    #p <- add_trace(p, x = x, y=descom$random, name="Residual",  type = 'scatter', mode = 'lines', line = list(color = '#080000'))
    
    layout(p,title = "Serie de Tiempo Descompuesta")
  })
  
  output$acfGraph <- renderPlotly({
    #f = DataFrame de leer el archivo
    req(input$columns)
    f <- info2()
    y  <- unlist(f[,input$columns])
    x <- seq(1:length(y))
    serie=ts(y,frequency=12,start=c(1990,1))
    descom<-decompose(serie,type=input$tipoSerie)
    g <- acf(descom$x)
    p <- plot_ly(x = x, y = g, type = 'scatter', mode = 'lines', line = list(color = '#000080'))
    
  })
  
  #Tabla de Contenido
  output$lag <- renderTable({

    req(input$columns)
    f <- info2()
    myDf <- slide(f, input$columns, NewVar = "Lag", slideBy = -1)  # create lag1 variable
    columna = head(myDf$Lag, n = 10L,addrownums = FALSE)
    
    return(columna)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)