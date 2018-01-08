#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Single Cell Quality Control"),
  
  # Sidebar with input file upload control
  # and a slider input for number of cells selected 
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId="inputFile",
                label="upload UMI counts table:",
                multiple=F),
      numericInput(inputId="estCells",
                   label="Estimated cell number:",
                   value=1,
                   min=1),
      conditionalPanel(condition="output.cellSlider",uiOutput("cellSlider"))
      #uiOutput("cellSlider")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("fracPlot")
    )
  )
))
