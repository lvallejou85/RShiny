library(shiny)
library(plotly)
library(shinythemes)
library(DataCombine)
library(forecast)
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
                            tags$hr(),
                            plotlyOutput("acfGraph")
                          )
                        )
                     
             ),
             navbarMenu("Distirbucion",
                        tabPanel("Modelos",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("input_model", "Modelo De Tendencia",
                                                 c("Lineal", "Cuadratica", "Cubica"
                                                 )
                                     ),
                                     numericInput("inNumber", "Numero de Predicciones:",
                                                  min = 10, max = 60, value = 24, step = 1)
                                   )
                                   ,
                                   mainPanel(
                                     # Output: Data file ----
                                     plotlyOutput("plote"),
                                     h4("Model Summary"),
                                     verbatimTextOutput("summarymodel")
                                     
                                     
                                     
                                   )
                                 )
                        ),
                        tabPanel("Sub-Component B",
                                 navlistPanel(
                                   "Header",
                                   tabPanel("First",
                                            h3("This is the first panel")
                                   ),
                                   tabPanel("Second",
                                            h3("This is the second panel")
                                   ),
                                   tabPanel("Third",
                                            h3("This is the third panel")
                                   )
                                 )
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
    serie<-ts(x,frequency=12,start=c(2000,1))
    boxplot(serie~cycle(serie))
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
    serie <- ts(y,frequency=12,start=c(2000,1))
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
    serie <- ts(y,frequency=12,start=c(2000,1))
    
    if (input$tipoSerie == "additive"){
      
      descom<-decompose(serie,type=input$tipoSerie)
      x <- time(serie)
      observada <- plot_ly(x = x, y = descom$x, name=input$columns, type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      estacional <- plot_ly(x = x, y = descom$seasonal, name="Estacional", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      tendencia <- plot_ly(x = x, y = descom$trend, name="Tendencia", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      residual <- plot_ly(x = x, y = descom$random, name="Residual", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      p <- subplot(observada, estacional,tendencia,residual,nrows = 4)
      layout(p,title = "Serie de Tiempo Descompuesta")
      
    } else {
      
      logserie <- log(serie) #Convierte la serie multiplicativa a serie aditiva
      descom<-decompose(serie,type=input$tipoSerie)
      x <- time(serie)
      
      observada <- plot_ly(x = x, y = descom$x, name=input$columns, type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      convAditiva <- plot_ly(x = x, y = logserie, name="Aditiva", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      estacional <- plot_ly(x = x, y = descom$seasonal, name="Estacional", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      tendencia <- plot_ly(x = x, y = descom$trend, name="Tendencia", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      residual <- plot_ly(x = x, y = descom$random, name="Residual", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      p <- subplot(observada,convAditiva, estacional,tendencia,residual,nrows = 5)
      layout(p,title = "Serie de Tiempo Descompuesta")
      
    }
    
  })
  
##########GRAFICA ACF y PACF 
  
  output$acfGraph <- renderPlotly({
    #f = DataFrame de leer el archivo
    req(input$columns)
    f <- info2()
    y  <- unlist(f[,input$columns])
    serie <- ts(y,frequency=12,start=c(2000,1))
    x <- time(serie)
    descom<-decompose(serie,type=input$tipoSerie)
    
    #ACF asignado a una variable
    a <- acf(descom$x)
    #Eje Y
    b <- unlist(a$acf)
    d <- b[1:length(b)]
    #Eje X
    c <- unlist(a$lag)
    e <- c[1:length(c)]
    #W -> Ancho de la barra
    w <- rep(0.01,length(e))
    
    #Ribbons
    ySup <- rep(0.2,length(e))
    yInf <- rep(-0.2,length(e))
    xLine <- rep(0.1,20)
    
    data <- data.frame(e, d, w)
    
    p <- plot_ly(data) %>%
      add_bars(
        x= ~e,
        y= ~d,
        width = ~w
      ) %>%
      add_ribbons(x= ~e,
                  ymin = yInf,
                  ymax = ySup,
                  line = list(color = 'rgba(7, 164, 181, 0.05)'),
                  fillcolor = 'rgba(7, 164, 181, 0.2)',
                  name = "Lag")  #%>%
    
    #layout(p,title = "Autocorrelación",xaxis = list(title = "LAG"),
           #yaxis = list(title = "ACF"))
    
    
    #INI PACF
    
    a <- pacf(descom$x)
    #Eje Y
    b <- unlist(a$acf)
    d <- b[1:length(b)]
    #Eje X
    c <- unlist(a$lag)
    e <- c[1:length(c)]
    #W -> Ancho de la barra
    w <- rep(0.01,length(e))
    
    #Ribbons
    ySup <- rep(0.2,length(e))
    yInf <- rep(-0.2,length(e))
    xLine <- rep(0.1,20)
    
    data <- data.frame(e, d, w)
    
    pp <- plot_ly(data) %>%
      add_bars(
        x= ~e,
        y= ~d,
        width = ~w
      ) %>%
      add_ribbons(x= ~e,
                  ymin = yInf,
                  ymax = ySup,
                  line = list(color = 'rgba(7, 164, 181, 0.05)'),
                  fillcolor = 'rgba(7, 164, 181, 0.2)',
                  name = "Lag")  %>%
      
      layout(p,title = "<---- Autocorrelación  |  Autocorrelación Parcial ---->",xaxis = list(title = "LAG"),
             yaxis = list(title = "PACF"))
    
    pT <- subplot(p,pp)
  })

########################### Pronostico########################
  
  output$plote <- renderPlotly({
    req(input$columns)
    f <- info2()
    #Devuelve los valores de la columna seleccionada en el select
    y  <- unlist(f[,input$columns])
    y <- ts(y,frequency=12,start=c(2000,1))
    x <- time(y)
    
    if (input$input_model == "Lineal") {
      
      modelo <- lm(y~x)
      
    } else if (input$input_model == "Cuadratica") {
      
      modelo <- lm(y~x+I(x^2))
      
    }  else if (input$input_model == "Cubica") {
      
      modelo <- lm(y~x+I(x^2)+I(x^3))
      
    }
    
    fore <- forecast(modelo$fitted.values, h = input$inNumber, level = 95)
    
    LS <- rep(0,input$inNumber)
    LI <- rep(0,input$inNumber)
    
    for (t in 1:input$inNumber){
      LS[t]= fore$mean[t]+ sqrt(t)
      LI[t]= fore$mean[t]- sqrt(t)
    }
    
    cast <- ts(fore$mean,frequency=12,start=c(max(time(y)),1))
    
    LS[1] <- modelo$fitted.values[length(modelo$fitted.values)]
    LI[1] <- modelo$fitted.values[length(modelo$fitted.values)]
    
    p <- plot_ly() %>%
      add_lines(x = x, y = y,
                color = I("black"), name = "real") %>%
      add_ribbons(x = time(cast), ymin = LI, ymax = LS,
                  color = I("gray80"), name = "90% Confianza") %>%
      add_lines(x = time(cast), y = cast, color = I("blue"), name = "Prediccion")%>%
      add_lines(x = x, y = modelo$fitted.values, color = I("steelblue"),name = "Pronostico" )
    
  })
  
  #Summary model
  output$summarymodel <- renderPrint({
    req(input$columns)
    f <- info2()
    #Devuelve los valores de la columna seleccionada en el select
    y  <- unlist(f[,input$columns])
    x <- seq(1:length(y))
    #x <- time(y)
    y <- ts(y,frequency=12,start=c(2000,1))
    if (input$input_model == "Lineal") {
      modelo=lm(y~x)
    } else if (input$input_model == "Cuadratica") {
      modelo=lm(y~x+I(x^2))
    }  else if (input$input_model == "Cubica") {
      modelo=lm(y~x+I(x^2)+I(x^3))
    }
    summary(modelo)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)