shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    #Espera la entrada del archivo
    req(input$archivo)    
    df <- read.csv(input$archivo$datapath,
                   header = input$header,
                   sep = ',',
                   quote = '"')
    
    #generate an rnorm distribution and plot it
    #dist <- cumsum(rnorm(input$obs))
    #x <- ts(dist, freq = input$freq, start = c(2010,01))
    #x <- ts(data = cumsum(rnorm(20)),start = c(1959, 2),frequency = 4)
    
    columna <- df[2]
    #SerieTiempo <- ts(columna, freq = input$freq, start = c(2010,01))
    plot(df,t='l')
    return(head(df))

  })
  
  output$distPlot2 <- renderPlot({
    
    req(input$archivo)    
    df <- read.csv(input$archivo$datapath,
                   header = input$header,
                   sep = ',',
                   quote = '"')
    
    columna <- df[2]
    plot(df,t='l')
    return(head(df))
    
  })
})
