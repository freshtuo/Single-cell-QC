#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  useShinyjs(),# include shinyjs
  
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
      # fraction of accumulative UMI counts plot
      plotOutput("fracPlot"),
      hr(),
      # download the "fraction counts" plot
      downloadButton(outputId="downloadFracPlot", label="Download your plot"),
      hr(),
      # raw UMI counts plot
      plotOutput("rawPlot"),
      hr(),
      # download the "raw counts" plot
      downloadButton(outputId="downloadRawPlot", label="Download your plot")
    )
  )
))
