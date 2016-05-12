
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Numbsliderer of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      shiny::selectInput("xstat", label = "Stat for X Axis", choices = c("Diff", "Win"), selected = "Diff" ),
      shiny::selectInput("ystat", label = "Stat for Y Axis", choices = c("Diff", "Win", "PR", "Assists"), selected = "Win")
    ),
    
 

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot", height = 800)
    )
  )
))
