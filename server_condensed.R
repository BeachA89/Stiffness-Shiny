

# Update the server function to use the analyze_hop_data function
server <- function(input, output) {
  output$resultstableLeft1.7 <- renderTable({
    if (is.null(input$Left1.7))
      return(NULL)
    analyze_hop_data(input$Left1.7$datapath, input$Mass)
  })
  
  output$resultstableLeft2.0 <- renderTable({
    if (is.null(input$Left2.0))
      return(NULL)
    analyze_hop_data(input$Left2.0$datapath, input$Mass)
  })
  
  output$resultstableLeft2.3 <- renderTable({
    if (is.null(input$Left2.3))
      return(NULL)
    analyze_hop_data(input$Left2.3$datapath, input$Mass)
  })
  
  output$resultstableLeft2.6 <- renderTable({
    if (is.null(input$Left2.6))
      return(NULL)
    analyze_hop_data(input$Left2.6$datapath, input$Mass)
  })
  
  output$resultstableLeftSS <- renderTable({
    if (is.null(input$LeftSS))
      return(NULL)
    analyze_hop_data(input$LeftSS$datapath, input$Mass)
  })
  
  output$resultstableRight1.7 <- renderTable({
    if (is.null(input$Right1.7))
      return(NULL)
    analyze_hop_data(input$Right1.7$datapath, input$Mass)
  })
  
  output$resultstableRight2.3 <- renderTable({
    if (is.null(input$Right2.3))
      return(NULL)
    analyze_hop_data(input$Right2.3$datapath, input$Mass)
  })
  
  output$resultstableRight2.6 <- renderTable({
    if (is.null(input$Right2.6))
      return(NULL)
    analyze_hop_data(input$Right2.6$datapath, input$Mass)
  })
  
  output$resultstableRightSS <- renderTable({
    if (is.null(input$RightSS))
      return(NULL)
    analyze_hop_data(input$RightSS$datapath, input$Mass)
  })
  
  output$projectplotLeft1.7 <- renderPlot({
    if (is.null(input$Left1.7))
      return(NULL)
    results <- analyze_hop_data(input$Left1.7$datapath, input$Mass)
    boxplot(results[, 2:7], main = "Boxplot of Hopping Metrics for File 1")
  })
  
  output$projectplotLeft2.0 <- renderPlot({
    if (is.null(input$Left2.0))
      return(NULL)
    results <- analyze_hop_data(input$Left2.0$datapath, input$Mass)
    boxplot(results[, 2:7], main = "Boxplot of Hopping Metrics for File 2")
  })
  
  output$projectplotLeft1.7 <- renderPlot({
    if (is.null(input$file3))
      return(NULL)
    results <- analyze_hop_data(input$file3$datapath, input$mass3)
    boxplot(results[, 2:7], main = "Boxplot of Hopping Metrics for File 3")
  })
}
