#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

options(shiny.maxRequestSize=30*1024^2) 

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # update cell number slider based on user input
  output$cellSlider <- renderUI({
    if (is.null(input$estCells))
      return()
    sliderInput(inputId="numCells",
                label="Number of cells:",
                min=1,
                max=input$estCells*3,# allow 3 times more cells than estimated
                value=input$estCells)
  })
  ##outputOptions(output, "cellSlider", suspendWhenHidden=F)

  # load UMI counts data if available
  getUMICounts <- reactive({
    inputFile <- input$inputFile
    if (is.null(inputFile))
      return(NULL)
    read.table(inputFile$datapath, header=T, check.names=F, sep=",")
  })

  # calculate fraction of accumulative UMI counts for plotting
  getAccumulativeUMIFrac <- reactive({
    # load UMI counts
    counts <- getUMICounts()
    if (is.null(counts))
      return(NULL)
    # number of estimated beads (20 X estimated cells)
    # this number is for ddSeq/Drop-seq only
    numBeads <- 20*input$estCells
    # max number of cells to plot
    maxCells <- 3*input$estCells
    # remove noise counts
    counts <- counts[1:numBeads,]
    # calculate accumulative counts
    # for "sid.cell.summary.csv" from BaseSpace
    accumCounts <- cumsum(counts$GeneUmiCount)
    sn <- c(1:length(accumCounts))
    # calculate fraction of accumulative counts
    fracCounts <- accumCounts/max(accumCounts)
    # prepare data.frame for plotting
    fracCountsPlot <- data.frame(sn, fracCounts, counts$CellId)
    fracCountsPlot <- fracCountsPlot[1:maxCells,]
    # return
    return(fracCountsPlot)
  })

  # draw fraction of accumulative UMI counts plot
  output$fracPlot <- renderPlot({
    # calculate accumulative UMI counts fraction
    fracCountsPlot <- getAccumulativeUMIFrac()
    if (is.null(fracCountsPlot))
      return(NULL)
    # max number of cells to plot
    maxCells <- 3*input$estCells
    # draw plot
    g <- ggplot(fracCountsPlot, aes(x=sn,y=fracCounts))
    g <- g + geom_line()
    g <- g + geom_vline(xintercept=input$numCells,colour="#990000", linetype="dashed")
    g <- g + coord_cartesian(xlim=c(0,maxCells), ylim=c(0,1))
    g <- g + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill="gray98"))
    g <- g + xlab("Cell sorted by UMI counts [descending]") + ylab("Accumulative fraction of UMI counts")
    # return
    g
  })
  
})
