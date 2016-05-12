
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(corrplot)

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

  bsp_lax_stats = read.csv("~/Documents/OneDrive/Bishops Varsity Stats - 5_3_16-CORMAT.csv")
  attach(bsp_lax_stats)

  M = cor(bsp_lax_stats[input$xstat], bsp_lax_stats[input$ystat])
  
  #corrplot::corrplot(M, tl.col = "dark blue", tl.srt = input$bins, diag = TRUE, tl.cex =.75,  title = input$bins)
  plot(M)
  


  
  
 
  })

})
