#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(ggplot2)
library(scales)

# allow 30M for input UMI counts file
options(shiny.maxRequestSize=30*1024^2)

# capture efficiency: used to remove empty beads, this can speed up calculation
# while this value affects the saturation rate, it should not impact your judgement on the number of captured cells
# for ddSeq/Drop-seq, we assume a 5% capture rate
# maxBeads = estCells / capEffCut
capEffCut <- 0.05

# maximum number of cells to show:
# maxCells = maxRatioCut * estCells
maxRatioCut <- 3

# initiate 'estCells'
initEstCells <- 1200

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # update EstCells (avoid NULL value)
  getEstCells <- reactive({
    if (is.na(input$estCells)){
      return(initEstCells)# use default value if estCells is not available
    }
    else{
      initEstCells <- input$estCells# update 'initEstCells', for some reason not really working...
      return(input$estCells)# load estCells if available
    }
  })

  # update MaxCells (avoid NULL value)
  getMaxCells <- reactive({
    if (is.na(input$estCells))
      return(initEstCells*maxRatioCut)# use default value if estCells is not available
    else
      return(input$estCells*maxRatioCut)# allow 3 times more cells than estimated
  })

  # update cell number slider based on user input
  output$cellSlider <- renderUI({
    sliderInput(inputId="numCells",
                label="Number of cells:",
                min=1,
                max=getMaxCells(),
                value=getEstCells())
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
    # estimated number of cells to plot
    estCells <- getEstCells()
    # number of estimated beads (20 X estimated cells)
    # this number is for ddSeq/Drop-seq only
    numBeads <- estCells/capEffCut
    # max number of cells to plot
    maxCells <- getMaxCells()
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
  drawFracPlot <- reactive({
    # calculate accumulative UMI counts fraction
    fracCountsPlot <- getAccumulativeUMIFrac()
    if (is.null(fracCountsPlot))
      return(NULL)
    # max number of cells to plot
    maxCells <- getMaxCells()
    # draw plot
    g <- ggplot(fracCountsPlot, aes(x=sn,y=fracCounts))
    g <- g + geom_line(size=1)
    g <- g + geom_vline(xintercept=input$numCells,colour="#990000", linetype="dashed")
    g <- g + coord_cartesian(xlim=c(0,maxCells), ylim=c(0,1))
    g <- g + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill="gray98"))
    g <- g + xlab("Cells sorted by UMI counts [descending]") + ylab("Cumulative fraction of UMI counts")
    g <- g + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=18,face="bold"), title=element_text(size=16,face="bold"))
    # return
    g
  })

  # render fraction of accumulative UMI counts plot
  output$fracPlot <- renderPlot({
    drawFracPlot()
  })

  # disable download buttion if the input UMI counts file is not ready
  observe({
    toggleState("downloadFracPlot", !is.null(input$inputFile))
  })

  # download fraction of accumulative UMI counts plot
  output$downloadFracPlot <- downloadHandler(
    filename=function(){
      ####write.table(input$inputFile$name, file="C:\\Users\\taz2008\\Downloads\\file.txt")
      sampleId <- tools::file_path_sans_ext(basename(input$inputFile$name))
      return(paste(sampleId,"fraction","accumulative","UMI","counts","png",sep="."))
    },
    content=function(file){
      g <- drawFracPlot()
      if (is.null(g))
        return(NULL)
      ggsave(filename=file, plot=g, width=8, height=6, units="in", dpi=300)
    }
  )

  ####outputOptions(output, "downloadFracPlot", suspendWhenHidden=FALSE)
  
  # format raw UMI counts for plotting
  formatUMICounts <- reactive({
    # load UMI counts
    counts <- getUMICounts()
    if (is.null(counts))
      return(NULL)
    # max number of cells to plot
    maxCells <- getMaxCells()
    # prepare data.frame for plotting
    sn <- c(1:nrow(counts))
    countsPlot <- data.frame(sn, counts$GeneUmiCount, counts$CellId)
    colnames(countsPlot) <- c("sn", "UMICounts", "CellId")
    countsPlot <- countsPlot[1:maxCells,]
    # return
    return(countsPlot)
  })

  # draw raw UMI counts plot
  drawRawPlot <- reactive({
    # format raw UMI counts
    countsPlot <- formatUMICounts()
    if (is.null(countsPlot))
      return(NULL)
    # max number of cells to plot
    maxCells <- getMaxCells()
    # draw plot
    g <- ggplot(countsPlot, aes(x=sn, y=UMICounts))
    #g <- g + geom_point(shape=21, size=4, alpha=0.6, colour="blue", fill=NA)
    #g <- g + geom_line(colour="green") + scale_y_log10(labels=comma)
    g <- g + geom_line(size=1) + scale_y_log10(labels=comma) + scale_x_log10(labels=comma)
    g <- g + geom_vline(xintercept=input$numCells, colour="#990000", linetype="dashed")
    g <- g + coord_cartesian(xlim=c(1,maxCells))
    g <- g + theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.background=element_rect(fill="gray98"))
    g <- g + xlab("Cells sorted by UMI counts [descending]") + ylab("UMI counts")
    g <- g + theme(axis.text=element_text(size=16,face="bold"), axis.title=element_text(size=18,face="bold"), title=element_text(size=16,face="bold"))
    # return
    g
  })
  
  # render raw UMI counts plot
  output$rawPlot <- renderPlot({
    drawRawPlot()
  })

  # disable download buttion if the input UMI counts file is not ready
  observe({
    toggleState("downloadRawPlot", !is.null(input$inputFile))
  })

  # download raw UMI counts plot
  output$downloadRawPlot <- downloadHandler(
    filename=function(){
      sampleId <- tools::file_path_sans_ext(basename(input$inputFile$name))
      return(paste(sampleId,"raw","UMI","counts","png",sep="."))
    },
    content=function(file){
      g <- drawRawPlot()
      if (is.null(g))
        return(NULL)
      ggsave(filename=file, plot=g, width=8, height=6, units="in", dpi=300)
    }
  )
})
