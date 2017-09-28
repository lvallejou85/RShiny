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
                        sidebarPanel(width = 2,
                          
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
                        mainPanel(width = 10,
                          
                          # Output: Data file ----
                          plotlyOutput("histograma"),
                          tableOutput("contents")
                          
                         
                        
                        )
                        
                      )
             ),
             navbarMenu("Serie de Tiempo",
                       tabPanel("Descomposición",
                                sidebarLayout(
                                    # Sidebar panel for inputs ----
                                    sidebarPanel(width = 2,
                                      radioButtons("tipoSerie", "Tipo de Serie:",
                                                   c("Aditiva" = "additive",
                                                     "Multiplicativa" = "multiplicative"))
                                    ),
                                    mainPanel(width = 10,
                                      # Output: Data file ---
                                      plotlyOutput("descompuesta"),
                                      tags$hr()
                                      #plotlyOutput("acfGraph")
                                    )
                                  )
                               
                       ),
                       tabPanel("ACF - PACF",
                                sidebarLayout(
                                  # Sidebar panel for inputs ----
                                  sidebarPanel(width = 2,
                                    checkboxGroupInput("checkGroup", 
                                                       h3("Gráficos"), 
                                                       choices = list("ACF" = 1, 
                                                                      "PACF" = 2, 
                                                                      "F(densidad)" = 3,
                                                                      "QQNorm" = 4),
                                                       selected = 1)
                                  ),
                                  mainPanel(width = 10,
                                    # Output: Data file ---
                                    plotlyOutput("acfGraph"),
                                    tags$hr(),
                                    plotlyOutput("pacfGraph"),
                                    tags$hr(),
                                    plotlyOutput("densidad")
                                    #,
                                    #plotlyOutput("QQNorm")
                                  )
                                )
                                
                       )
             ),
             navbarMenu("Modelos",
                        tabPanel("Modelos",
                                 sidebarLayout(
                                   sidebarPanel(width = 2,
                                     selectInput("input_model", "Modelo De Tendencia",
                                                 c("Lineal", "Cuadratica", "Cubica","Holt-winters"
                                                 )
                                     ),
                                     numericInput("inNumber", "Numero de Predicciones:",
                                                  min = 10, max = 60, value = 24, step = 1)
                                   )
                                   ,
                                   mainPanel(width = 10,
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
      observada <- plot_ly(x = x, y = descom$x, name="Observada", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      estacional <- plot_ly(x = x, y = descom$seasonal, name="Estacional", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      tendencia <- plot_ly(x = x, y = descom$trend, name="Tendencia", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      residual <- plot_ly(x = x, y = descom$random, name="Residual", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      p <- subplot(observada, estacional,tendencia,residual,nrows = 4)
      layout(p,title = "Serie de Tiempo Descompuesta")
      
    } else {
      
      logserie <- log(serie) #Convierte la serie multiplicativa a serie aditiva
      descom<-decompose(serie,type=input$tipoSerie)
      x <- time(serie)
      
      observada <- plot_ly(x = x, y = descom$x, name="Observada", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      convAditiva <- plot_ly(x = x, y = logserie, name="Aditiva", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      estacional <- plot_ly(x = x, y = descom$seasonal, name="Estacional", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      tendencia <- plot_ly(x = x, y = descom$trend, name="Tendencia", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      residual <- plot_ly(x = x, y = descom$random, name="Residual", type = 'scatter', mode = 'lines', line = list(color = '#0000080'))
      p <- subplot(observada,convAditiva, estacional,tendencia,residual,nrows = 5)
      layout(p,title = "Serie de Tiempo Descompuesta")
      
    }
    
  })
  
##########GRAFICA ACF y PACF Densidad y QQ Norm
  
  #GRAFICA DE ACF
  
  output$acfGraph <- renderPlotly({
    #f = DataFrame de leer el archivo
    req(input$columns)
    f <- info2()
    y  <- unlist(f[,input$columns])
    serie <- ts(y,frequency=12,start=c(2000,1))
    x <- time(serie)
    descom<-decompose(serie,type=input$tipoSerie)
    t <- 1
    
    if(length(input$checkGroup) != 0){
    #Si hay un checkbox seleccionado
      
          if ('1' %in% input$checkGroup){
            #Si el checkbox es el 1
            
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
                
                layout(p,title = "Autocorrelación",xaxis = list(title = "LAG"),
                       yaxis = list(title = "ACF"))
          }else{
              
            plotly_empty()
            
          }
          
    }else{
          
      plotly_empty()
      
    }
  })

  #GRAFICA DE PACF
  
  output$pacfGraph <- renderPlotly({
    #f = DataFrame de leer el archivo
    req(input$columns)
    f <- info2()
    y  <- unlist(f[,input$columns])
    serie <- ts(y,frequency=12,start=c(2000,1))
    x <- time(serie)
    descom<-decompose(serie,type=input$tipoSerie)
    t <- 1
    
    if(length(input$checkGroup) != 0){
      #Si hay un checkbox seleccionado
      
          if ('2' %in% input$checkGroup){
            #Si el checkbox 2 está seleccionado
            
            #ACF asignado a una variable
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
            
            layout(p,title = "Autocorrelación Parcial",xaxis = list(title = "LAG"),
                   yaxis = list(title = "pacf"))
          }else{
            
            plotly_empty()
            
          }
    }else{
        
      plotly_empty()
    }
    
  })

  #GRAFICA FUNCION DE DENSIDAD

  output$densidad <- renderPlotly({
    #f = DataFrame de leer el archivo
    req(input$columns)
    f <- info2()
    y  <- unlist(f[,input$columns])
    serie <- ts(y,frequency=12,start=c(2000,1))
    x <- time(serie)
    descom<-decompose(serie,type=input$tipoSerie)
    
    if(length(input$checkGroup) != 0){
      #Si hay un checkbox seleccionado
      
      if ('3' %in% input$checkGroup){
        #Si el checkbox 3 está seleccionado
        
        #DENSITY asignado a una variable, se genera de descom$residuals (Los residuales)
        a <- density(descom$x)
        #a <- density(descom$random)
        #Eje Y
        b <- unlist(a$y)
        d <- b[1:length(b)]
        #Eje X
        c <- unlist(a$x)
        e <- c[1:length(c)]
        
        data <- data.frame(e, d)

        plot_ly(data) %>%
          add_trace(x = ~e, y = ~d, type = 'scatter', mode = 'lines', name = 'Densidad', yaxis = 'y',
                    line = list(color = '#45171D')) %>%
        
        layout(p,title = "Densidad",xaxis = list(title = "X"),
               yaxis = list(title = "Y"))
        
      }else{
        
        plotly_empty()
        
      }
    }else{
      
      plotly_empty()
    }
    
  })
  
  ########################### Pronostico########################
  
  output$plote <- renderPlotly({
    graficahw <- function(y,fit,cast,LI,LS){
      plot_ly()%>%
        add_lines(x = time(fit), y = fit ,color = I("black"), name = "real")%>%
        add_lines(x = time(y), y = y, color = I("steelblue"),name = "Winters" )%>%
        add_lines(x = time(cast), y = cast, color = I("red"),name = "Pronostico" )%>%
        add_ribbons(x = time(cast), ymin = LI, ymax = LS,color = I("gray80"), name = "90% Confianza") 
    }
    
    grafica <- function(y,fit,cast,LI,LS){
      plot_ly() %>%
        add_lines(x = time(y), y = y,
                  color = I("black"), name = "real") %>%
        add_ribbons(x = time(cast), ymin = LI, ymax = LS,
                    color = I("gray80"), name = "90% Confianza") %>%
        add_lines(x = time(cast), y = cast, color = I("blue"), name = "Prediccion")%>%
        add_lines(x = time(y), y = fit, color = I("steelblue"),name = "Pronostico" )
    }
    
    req(input$columns)
    f <- info2()
    #Devuelve los valores de la columna seleccionada en el select
    y  <- unlist(f[,input$columns])
    y <- ts(y,frequency=12,start=c(2000,1))
    x <- time(y)
    
    if (input$input_model == "Lineal") {
      
      modelo <- lm(y~x)
      fore <- forecast(modelo$fitted.values, h = input$inNumber, level = 95)
      LS <- rep(0,input$inNumber)
      LI <- rep(0,input$inNumber)
      for (t in 1:input$inNumber){
        LS[t]= fore$mean[t]+ sqrt(t)
        LI[t]= fore$mean[t]- sqrt(t)
      }
      cast <- ts(fore$mean,frequency=12,start=c(max(time(y)),1))
      grafica(y,modelo$fitted.values,cast,LI,LS)
    } else if (input$input_model == "Cuadratica") {
      
      modelo <- lm(y~x+I(x^2))
      fore <- forecast(modelo$fitted.values, h = input$inNumber, level = 95)
      cast <- ts(fore$mean,frequency=12,start=c(max(time(y)),1))
      LS <- rep(0,input$inNumber)
      LI <- rep(0,input$inNumber)
      for (t in 1:input$inNumber){
        LS[t]= fore$mean[t]+ sqrt(t)
        LI[t]= fore$mean[t]- sqrt(t)
      }
      
      grafica(y,modelo$fitted.values,cast,LI,LS)
    }  else if (input$input_model == "Cubica") {
      
      modelo <- lm(y~x+I(x^2)+I(x^3))
      fore <- forecast(modelo$fitted.values, h = input$inNumber, level = 95)
      cast <- ts(fore$mean,frequency=12,start=c(max(time(y)),1))
      LS <- rep(0,input$inNumber)
      LI <- rep(0,input$inNumber)
      for (t in 1:input$inNumber){
        LS[t]= fore$mean[t]+ sqrt(t)
        LI[t]= fore$mean[t]- sqrt(t)
      }
      
      grafica(y,modelo$fitted.values,cast,LI,LS)
      
    }else if (input$input_model == "Holt-winters") {
      modelo <- HoltWinters(y)
      forecast <- predict(modelo, n.ahead = input$inNumber, prediction.interval = T, level = 0.95)
      graficahw(modelo$x,modelo$fitted[,1],forecast[,1],forecast[,3],forecast[,2])
    }
    
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
    }else if (input$input_model == "Holt-winters") {
      modelo <- HoltWinters(y)
    }
    summary(modelo)
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)