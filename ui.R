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

# default 'estCells'
initEstCells <- 1200

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
                   value=initEstCells,
                   min=1),
      conditionalPanel(condition="output.cellSlider",uiOutput("cellSlider")),
      #uiOutput("cellSlider")
      # summary table on #genes/UMIcounts per cell
      hr(),
      tableOutput(outputId="sumTable")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        style="height:480px;",
        # fraction of accumulative UMI counts plot
        column(6, plotOutput(outputId="fracPlot", width="100%")),
        # raw UMI counts plot
        column(6, plotOutput(outputId="rawPlot", width="100%"))
      ),
      fluidRow(
        style="height:60px;",
        # download "fraction counts" plot
        column(5, offset=1, downloadButton(outputId="downloadFracPlot", label="Download your plot")),
        # download "raw counts" plot
        column(5, offset=1, downloadButton(outputId="downloadRawPlot", label="Download your plot"))
      ),
      fluidRow(
        style="height:480px;",
        # gene violin plot
        column(6, plotOutput(outputId="geneViolinPlot", width="100%")),
        # UMI counts violin plot
        column(6, plotOutput(outputId="UMIViolinPlot", width="100%"))
      ),
      fluidRow(
        style="height:60px;",
        # download "gene violin" plot
        column(5, offset=1, downloadButton(outputId="downloadGeneViolinPlot", label="download your plot")),
        # download "UMI counts violin" plot
        column(5, offset=1, downloadButton(outputId="downloadUMIViolinPlot", label="download your plot"))
      )
    )
  )
))
