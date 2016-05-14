

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(corrplot)
library(scatterplot3d)
library(ggplot2)
library(gridExtra)


shinyServer(function(input, output) {
  bsp_lax_stats = read.csv("~/Documents/OneDrive/Bishops Varsity Stats - 5_3_16-CORMAT.csv")
  output$ui <- renderUI({
    if (input$chrt_type == "Correlation Matrix") {
      uiCorrMat(bsp_lax_stats)
    }else if (input$chrt_type == "Line Graph") {
      uiStatPlot(bsp_lax_stats)
    }else if (input$chrt_type == "3D Plot") {
      ui3dPlot(bsp_lax_stats)
    }else if (input$chrt_type == "Plot") {
      uiScatPlot(bsp_lax_stats) 
    }
  })
  
  output$distPlot <- renderPlot({
  #attach(bsp_lax_stats)
    
    if (input$chrt_type == "3D Plot") {
      render3dPlot(bsp_lax_stats, input) 
    }else if(input$chrt_type == "Plot") {
      renderScatPlot(bsp_lax_stats, input = input)
    }else if(input$chrt_type == "Correlation Matrix") {
      renderCorrMat(bsp_lax_stats, input) 
    }else if(input$chrt_type == "Line Graph") {
      renderStatPlot(stats = bsp_lax_stats, input = input) 
    }
    #end if
  }) #end renderPlot
  
  tmp_stat <- bsp_lax_stats
  rownames(tmp_stat) <- paste(letters[1:16])
  colnames(tmp_stat) <- paste(names(tmp_stat))
  #trans_stat <- t(tmp_stat)
  output$table <- renderDataTable(tmp_stat)
  
  cleanup(bsp_lax_stats)
}) #end shinyServer



#Utility Functions
#cleanup: Clean up session.  To be used at of shiney server block
cleanup <- function(stats){
  rm(stats)
}

#lm_eqn: Generate lm equation for print a plot
lm_eqn <- function(df, formula){
  m <- lm(formula, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq)); 
  print(eq)
}



###########################
#####Dynamic UI Functions
#uiSCatPlot: UI elements for scatter plot inputs
uiScatPlot <- function (bsp_lax_stats) {
  list(
    selectInput("xstat", "X axis", choices = names(bsp_lax_stats),selected = "PR"),
    selectInput("ystat", "Y axis", choices = names(bsp_lax_stats),selected = "Diff")
  )
}

#uiStatPlot: UI elements for stanard plot inputs
uiStatPlot <- function (stats) {
  list(
    selectInput("xstat", "Stat", choices = names(stats),selected = "PR")
  )
}

#ui3dPlot: UI elements for 3D scatter plot inputs
ui3dPlot <- function(stats) {
  list(
    sliderInput(
      "angle", "Angle", min = 0, max = 360, value = 30, animate = TRUE
    ),
    selectInput(
      "xstat", "X axis", choices = names(stats), selected = "Diff"
    ),
    selectInput(
      "ystat", "Y axis", choices = names(stats), selected = "Goals"
    ),
    selectInput(
      "zstat", "Y axis", choices = names(stats), selected = "PR"
    ),
    radioButtons(
      "subsets", "Sub category", choices = c("All", "Wins", "Losses", "Higher PR", "Lower PR")
    )
  )
}


#uiCorMat: 
uiCorrMat <- function (stats) {
  list(
    selectInput("options",
              label = "Included stats",
              choices = names(stats[,1:length(stats)]),selected = c("PR", "Diff", "Goals", "Win"), multiple = TRUE),
    radioButtons("order", "Ordering Method", choices = c( "original", "hclust", "FPC"), selected = "original") 
  )
    
}



#########################
####Plot Render Functions
#renderScatPlot: Render scatter plot
renderScatPlot <- function(stats, input) {
  plt = ggplot(data = stats, aes(x = get(input$xstat), y = get(input$ystat)), label = list("test", "test")) + geom_point(shape = factor(stats$Win), color = "blue", fill = "blue", size = 3.5)  + geom_smooth(method = lm) + scale_shape_discrete(c(17, 0, 22),solid = T)   
  plt
  
}


#renderStatPlot: Render line graph
renderStatPlot  <- function(input, output = NULL, stats = NULL){
  plot(stats[,input$xstat], ylab = input$xstat, type = "b")
  axis(1, at=1:16, labels = stats$Opponent )
}


#render3dPlot: Render 3D scatter plot
render3dPlot <- function (stats, input) {
  if (input$subsets != "All") {
    stats <- switch(
      input$subsets,
      "Wins" = subset(stats,Win > 0),
      "Losses" = subset(stats , Win < 0),
      "Higher PR" = subset(stats, PR.Ddif < 0),
      "Lower PR" = subset(stats, PR.Ddif > 0)
    )
  } #end if
  index = c(1,0,-1)
  #values = type.convert(c("Win","Tie","Loss"))
  shapes = c(19,17,17)
  shape_data <- shapes[match(stats$Win, index)]
  
  s3d <- scatterplot3d(
    x = stats[input$xstat], y = stats[input$ystat], z = stats[input$zstat],
    xlab = input$xstat, ylab = input$ystat, zlab =  input$zstat,
    pch = shape_data,
    type = "h", angle = input$angle, color = "steelblue", box = FALSE, cex.symbols = 1.5,
    highlight.3d = TRUE
  )
}


#renderCorrMat: Render correlation matrix
renderCorrMat <- function(stats, input) {
  #index <- match(input$options, names(stats))
  #index <- sort(c(index - 1,index))
  #M = cor(stats[,2:5])
  tmp <- stats[input$options]
  print(tmp)
  summary(tmp)
    M <- cor( tmp )
  corrplot(M, tl.col = "dark blue", tl.srt = 45, tl.cex = 1, order = input$order)
}

