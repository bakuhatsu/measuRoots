#################################
# 10/28/2016                    #
# Sven Nelson                   #
# function: rootLengths         #
#################################



# library(shiny)
# ##library(shinyFiles)
# ##library(shinythemes)
# library(miniUI)
# library(ggplot2)
# library(cowplot)
#'
#'  @export
#'
rootLengthsApp <- function(wwday0csv = NULL, wwday1csv = NULL, wwday2csv = NULL, wsday0csv = NULL, wsday1csv = NULL, wsday2csv = NULL, returnPlots = FALSE, pdfLabels = TRUE, highLow = 65) {

  ui <- miniUI::miniPage(
    #shinythemes::themeSelector(), # spacelab was good
    #theme = shinythemes::shinytheme("spacelab"),
    miniUI::gadgetTitleBar("Root Lengths Data Analysis"),
    miniUI::miniTabstripPanel(
      # miniUI::miniTabPanel("Prepare Files", icon = shiny::icon("copy"),
      #                      miniUI::miniContentPanel(
      #                      )
      # ),
      miniUI::miniTabPanel("Select Input", icon = shiny::icon("sliders"),
                           miniUI::miniContentPanel(
                             htmltools::h4("Choose CSV Files with root lengths:"),
                             shiny::fillRow(
                               # WW day 0
                               # WW day 1
                               # WW day 2
                               shiny::fillCol(
                                 # fileInput("fileWWday0", "WW day 0",
                                 #           accept = c(
                                 #             "text/csv",
                                 #             "text/comma-separated-values,text/plain",
                                 #             ".csv"), width = 250
                                 # ),
                                 shiny::uiOutput("wwday0"),
                                 shiny::uiOutput("wwday1"),
                                 shiny::uiOutput("wwday2")
                               ),
                               shiny::fillCol(
                                 # WS day 0
                                 # WS day 1
                                 # WW day 2
                                 shiny::uiOutput("wsday0"),
                                 shiny::uiOutput("wsday1"),
                                 shiny::uiOutput("wsday2")
                               )
                             )
                           )
      ),
      miniUI::miniTabPanel("Plot Results", icon = shiny::icon("bar-chart"),
                           # shiny::fillRow(
                           #   shiny::helpText("Return "),
                             # shiny::selectizeInput("returnVal", NULL,
                             #                    c("Well-watered plot" = "SummaryWWdata",
                             #                      "Water-stressed plot" = "SummaryWSdata",
                             #                      "Difference (WSvsWW) plot" = "SummaryWSvsWWdata"),
                             #                    width = "90%"),
                           #   shiny::helpText(" when Done is clicked."),
                           #   shiny::downloadButton('downloadPlots', 'Download Plots as Figure'), flex = c(1,1,1,NA)
                           # ),
                           shiny::downloadButton('downloadPlots', 'Download Plots as Figure'),
                           miniUI::miniContentPanel(
                             #plotOutput("comboPlot", width = "100%")
                             shiny::fillRow(
                               shiny::plotOutput("wellWatered", width = "85%"), #"80%"
                               shiny::plotOutput("waterStress", width = "85%") #"80%"
                             ),
                             shiny::plotOutput("difference", width = "93%", height = "60%")#, align = "center"
                           )
      ),
      miniUI::miniTabPanel("Summaries", icon = shiny::icon("table"),
                           miniUI::miniContentPanel(
                             shiny::fillCol(flex = c(1,13),
                               shiny::fillRow(
                                 shiny::selectInput("summary", "Choose summary:",
                                             c("Well-watered" = "SummaryWWdata",
                                               "Water-stressed" = "SummaryWSdata",
                                               "Difference (WSvsWW)" = "SummaryWSvsWWdata"),
                                             width = "45%"),
                                 shiny::downloadButton('downloadCSV', 'Download CSV'), flex = c(1,NA)
                               ),
                               shiny::dataTableOutput("summTable")
                             )
                           )
      )
    )
  )

  server <- function(input, output, session) {

    output$wwday0 <- shiny::renderUI({
      if (is.null(wwday0csv)) {
        shiny::fileInput("fileWWday0", "WW day 0",
                            accept = c(
                              "text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"), width = 250
                  )
      } else {
        htmltools::HTML(paste("<b>File loaded for WW day 0:</b><br/><pre><span class='inner-pre' style='font-size: 10px'>", wwday0csv, "</span></pre>"))
        #shiny::renderPrint({"hello"})
      }
    })

    output$wwday1 <- shiny::renderUI({
      if (is.null(wwday1csv)) {
        shiny::fileInput("fileWWday1", "WW day 1",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"), width = 250
        )
      } else {
        htmltools::HTML(paste("<b>File loaded for WW day 1:</b><br/><pre><span class='inner-pre' style='font-size: 10px'>", wwday1csv, "</span></pre>"))
        #shiny::renderPrint({"hello"})
      }
    })

    output$wwday2 <- shiny::renderUI({
      if (is.null(wwday2csv)) {
        shiny::fileInput("fileWWday2", "WW day 2",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"), width = 250
        )
      } else {
        htmltools::HTML(paste("<b>File loaded for WW day 2:</b><br/><pre><span class='inner-pre' style='font-size: 10px'>", wwday2csv, "</span></pre>"))
        #shiny::renderPrint({"hello"})
      }
    })

    output$wsday0 <- shiny::renderUI({
      if (is.null(wsday0csv)) {
        shiny::fileInput("fileWSday0", "WS day 0",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"), width = 250
        )
      } else {
        htmltools::HTML(paste("<b>File loaded for WS day 0:</b><br/><pre><span class='inner-pre' style='font-size: 10px'>", wsday0csv, "</span></pre>"))
        #shiny::renderPrint({"hello"})
      }
    })

    output$wsday1 <- shiny::renderUI({
      if (is.null(wsday1csv)) {
        shiny::fileInput("fileWSday1", "WS day 1",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"), width = 250
        )
      } else {
        htmltools::HTML(paste("<b>File loaded for WS day 1:</b><br/><pre><span class='inner-pre' style='font-size: 10px'>", wsday1csv, "</span></pre>"))
        #shiny::renderPrint({"hello"})
      }
    })

    output$wsday2 <- shiny::renderUI({
      if (is.null(wsday2csv)) {
        shiny::fileInput("fileWSday2", "WS day 2",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"), width = 250
        )
      } else {
        htmltools::HTML(paste("<b>File loaded for WS day 2:</b><br/><pre><span class='inner-pre' style='font-size: 10px'>", wsday2csv, "</span></pre>"))
        #shiny::renderPrint({"hello"})
      }
    })

    rootDFww <- shiny::reactive({
      if (is.null(wwday0csv)) {
        inWWday0 <- input$fileWWday0
      } else {
        inWWday0 <- list()
        inWWday0$datapath <- wwday0csv
      }

      if (is.null(wwday1csv)) {
        inWWday1 <- input$fileWWday1
      } else {
        inWWday1 <- list()
        inWWday1$datapath <- wwday1csv
      }

      if (is.null(wwday2csv)) {
        inWWday2 <- input$fileWWday2
      } else {
        inWWday2 <- list()
        inWWday2$datapath <- wwday2csv
      }

      if (is.null(inWWday0) | is.null(inWWday1) | is.null(inWWday2))
        return(NULL)

      # Compile the data into a single dataframe with only length information
      compileRootDF(inWWday0$datapath, inWWday1$datapath, inWWday2$datapath, printFilePaths = FALSE)
    })
    rootDFws <- shiny::reactive({
      if (is.null(wsday0csv)) {
        inWSday0 <- input$fileWSday0
      } else {
        inWSday0 <- list()
        inWSday0$datapath <- wsday0csv
      }

      if (is.null(wsday1csv)) {
        inWSday1 <- input$fileWSday1
      } else {
        inWSday1 <- list()
        inWSday1$datapath <- wsday1csv
      }

      if (is.null(wsday2csv)) {
        inWSday2 <- input$fileWSday2
      } else {
        inWSday2 <- list()
        inWSday2$datapath <- wsday2csv
      }

      if (is.null(inWSday0) | is.null(inWSday1) | is.null(inWSday2))
        return(NULL)

      #read.csv(inFile$datapath, header = TRUE)

      # Compile the data into a single dataframe with only length information
      compileRootDF(inWSday0$datapath, inWSday1$datapath, inWSday2$datapath, printFilePaths = FALSE)
    })

    summWSvsWW <- shiny::reactive({
      # if (is.null(rootDFww() | rootDFws())) # error
      #   return(NULL)

        # make summary
        summaryWSvsWW(rootDFww(), rootDFws(), pCuttoff = 0.05)
    })

    #### Preparing comboPlot for use in figure ####
    wwPlot <- shiny::reactive({
      if(is.null(rootDFww()))
        return(NULL)
      rootPlot(rootDF = rootDFww(), "Well Watered", max = highLow, by = 10)
      })
    wsPlot <- shiny::reactive({
      if(is.null(rootDFws()))
        return(NULL)
      rootPlot(rootDF = rootDFws(), "Water Stressed", max = highLow, by = 10)
      })
    diffPlot <- shiny::reactive({
      if(is.null(rootDFww()) | is.null(rootDFws))
        return(NULL)
      # # make summary
      # summWSvsWW <- summaryWSvsWW(rootDFww(), rootDFws(), pCuttoff = 0.05)
      # make plot
      plotDifferences(summWSvsWW())
      })
    comboPlot <- shiny::reactive({

      #plot_grid(wwPlot, wsPlot, diffPlot, labels = c("A", "B", "C"), ncol = 2)
      if(pdfLabels) {
        plotLabels <- c("A", "B", "C")
      } else {
        plotLabels <- c("", "", "")
      }
      labA <- c()
      labA$x <- 0
      labA$y <- 10.8

      labB <- c()
      labB$x <- 3.75
      labB$y <- 10.8

      labC <- c()
      labC$x <- 0
      labC$y <- 4

      cowplot::ggdraw(xlim = c(0,7.5), ylim = c(0,11)) + # 7 4,
        cowplot::draw_plot(wwPlot(), x = 0, y = 4, width = 3.75, height = 7) +
        cowplot::draw_plot(wsPlot(), x = 3.75, y = 4, width = 3.75, height = 7) +
        cowplot::draw_plot(diffPlot(), x = 0, y = 0, width = 7.5, height = 4) +
        cowplot::draw_plot_label(plotLabels, x = c(labA$x, labB$x, labC$x), y = c(labA$y, labB$y, labC$y), size = 18)
    })

    ### Plot the well-watered rootPlot
    output$wellWatered <- shiny::renderPlot({wwPlot()})
    ### Plot the water stress rootPlot
    output$waterStress <- shiny::renderPlot({wsPlot()})
    # Plot the differences of WSvsWW with significance
    output$difference <- shiny::renderPlot({diffPlot()})

    ##### ---------SUMMARIES--------- #####
    datasetInput <- shiny::reactive({

      if (input$summary == "SummaryWWdata") {
        if (is.null(rootDFww()))
          return(NULL)

        ## Return summaries:
        rootPlot(rootDF = rootDFww(), returnSummary = T)
      } else if (input$summary == "SummaryWSdata") {
        if (is.null(rootDFws()))
          return(NULL)

        ## Return summary:
        rootPlot(rootDF = rootDFws(), returnSummary = T)
      } else if (input$summary == "SummaryWSvsWWdata") {
        if (is.null(rootDFww()) | is.null(rootDFws()))
          return(NULL)

        ## Return summary:
        summaryWSvsWW(rootDFww(), rootDFws(), pCuttoff = 0.05)
      }
    })

    output$summTable <- shiny::renderDataTable({
      datasetInput()
    })

    ## Save a file locally as .csv
    output$downloadCSV <- shiny::downloadHandler(
      filename = function() {
        paste(gsub("-", "", Sys.Date()), " ", input$summary, '.csv', sep='')
      },
      content = function(filename) {
        utils::write.csv(datasetInput(), filename, row.names = FALSE)
      }
    )

    #### Save the plots locally as a pdf figure ####
    output$downloadPlots <- shiny::downloadHandler(
      filename = function() { paste(gsub("-", "", Sys.Date()), " Root Lengths Figure", ".pdf", sep="") },
      content = function(file) {
        #ggsave(file, plot = comboPlot(), device = "pdf", scale = 1.25)
        cowplot::ggsave(file, plot = comboPlot(), device = "pdf", width = 7.5*.9, height = 11*.85, units = "in")
      }
    )

    shiny::observeEvent(input$done, {
      if(returnPlots) {
        returnedPlots <- list()
        returnedPlots$WW <- wwPlot()
        returnedPlots$WS <- wsPlot()
        returnedPlots$Diff <- diffPlot()
        shiny::stopApp(returnedPlots)
      } else {
        shiny::stopApp(NULL)
      }
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Root Lengths", width = 600, height = 700)) # 8.5 x 11 --> 612 x 792
}

# rootLengthsApp(wwday0csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160318 Diva wt WW vs WS (3 plates each)/WW combined files/20160319 Diva wt WW day0 (combined).csv", wwday1csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160318 Diva wt WW vs WS (3 plates each)/WW combined files/20160320 Diva wt WW day1 (combined).csv",wwday2csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160318 Diva wt WW vs WS (3 plates each)/WW combined files/20160321 Diva wt WW day2 (combined).csv", wsday0csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160318 Diva wt WW vs WS (3 plates each)/WS combined files/20160319 Diva wt WS day0 (combined).csv", wsday1csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160318 Diva wt WW vs WS (3 plates each)/WS combined files/20160320 Diva wt WS day1 (combined).csv",wsday2csv = "~/CloudStation/000 Lab Notebook - Oliver Lab/Data collection/Root elongation WW vs WS (soil plates)/20160318 Diva wt WW vs WS (3 plates each)/WS combined files/20160321 Diva wt WS day2 (combined).csv",returnPlots = T) ## for testing
