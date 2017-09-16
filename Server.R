# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$distPlot <- renderPlot({
    
    # generate an rnorm distribution and plot it
    dist <- cumsum(rnorm(input$obs))
    x <- ts(dist, freq = input$freq, start = c(2010,01))
    #x <- ts(data = cumsum(rnorm(20)),start = c(1959, 2),frequency = 4) 
    plot(x,t='l')
    
#     req(input$file1)
#     
#     df <- read.csv(input$file1$datapath,
#                    header = input$header,
#                    sep = input$sep,
#                    quote = input$quote)
#     
#     if(input$disp == "head") {
#       return(head(df))
#     }
#     else {
#       return(df)
#     }

  })
})
