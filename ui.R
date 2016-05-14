

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
shinyUI(fluidPage(
  # Application title
  titlePanel("Bishops 2016 Stats Analysis"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(sidebarPanel(
    wellPanel(
      shiny::selectInput(
      "chrt_type", "Type of chart", c("Plot","Line Graph", "3D Plot", "Correlation Matrix"), selected = "Plot"
      )
  ),       wellPanel(
    uiOutput("ui")
  )),
  
  # Show a plot of the generated distribution
  mainPanel(plotOutput("distPlot", height = 800))),
  dataTableOutput('table')
))
