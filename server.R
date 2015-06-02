function(input,output) {
  set.seed(123)
  histData = rnorm(100)
  
  output$plot1 = renderPlot({
    data = histData[seq_len(input$slider)]
    hist(data)
  })
  output$MenuItem = renderMenu({
    menuItem("Menu Item", icon = icon("calendar"))
    
  })
}
