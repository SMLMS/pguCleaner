library(shiny)
library(shinydashboard)

body <- shinydashboard::dashboardBody(shinydashboard::tabItems(
  shinydashboard::tabItem(
    tabName = "tab_import",
    shiny::fluidPage(
      width = 12,
      title = "Import",
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shinydashboard::box(
            width = 12,
            height = "100%",
            #style = "margin-top: 5px;",
            title = "Menue",
            status = "primary",
            solidHeader = TRUE,
            #collapsible = TRUE,
            shiny::fileInput(
              "fi.import",
              label = h5(" "),
              accept = c(".csv", ".h5", ".xls", ".xlsx"),
              width = "100%"
            ),
            shiny::verbatimTextOutput("txt.loadInfo"),
            shiny::numericInput(
              "ni.import",
              label = h5("Sheet Number"),
              value = 1
            ),
            shiny::numericInput(
              "ni.skipRows",
              label = h5("Skip Rows"),
              value = 0
            ),
            shiny::radioButtons(
              "rb.separator",
              label = h5("Separator"),
              choices = c(
                Comma = ",",
                Semicolon = ";",
                Whitespace = " ",
                Tab = "\t"
              ),
              selected = "\t"
            ),
            shiny::checkboxInput("cb.header",
                                 label = h5("Header"),
                                 value = TRUE),
            shiny::actionButton("ab.import",
                                label = h5("import"),
                                width = "100%")
          )
        ),
        shiny::column(
          width = 8,
          #height = "100%",
          shinydashboard::box(
            width = 12,
            height = "100%",
            #style = "margin-top: 5px;",
            title = "Info",
            status = "primary",
            solidHeader = TRUE,
            #collapsible = TRUE,
            DT::dataTableOutput("tbl.dataInfo", width = "100%", height = "50%")
          ),
          shinydashboard::box(
            width = 12,
            height = "100%",
            #style = "margin-top: 5px;",
            title = "Statistics",
            status = "primary",
            solidHeader = TRUE,
            #collapsible = TRUE,
            DT::dataTableOutput("tbl.rawDataStatistics", width = "100%", height = "50%")
          )
        )
      )
    )), 
  
  shinydashboard::tabItem(
    tabName = "tab_filter",
    shiny::fluidPage(
      width = 12,
      title = "Filter",
      shiny::fluidRow(
        height = "50%",
        shiny::column(
          width = 2,
          shinydashboard::box(
            width = 12,
            #height = "100%",
            title = "Menue",
            status = "primary",
            solidHeader = TRUE,
            shiny::actionButton(
              inputId = "ab.filterSet",
              label = "Filter",
              width = "100%"
            ),
            shiny::br(),
            shiny::br(),
            shiny::actionButton(
              inputId = "ab.filterInvSet",
              label = "Filter inverse",
              width = "100%"
            ),
            shiny::br(),
            shiny::br(),
            shiny::actionButton(
              inputId = "ab.filterReset",
              label = "Reset",
              width = "100%"
            )
          )
        ),
        shiny::column(
          width = 5,
          shinydashboard::box(
            width = 12,
            height = "100%",
            title = "statistics",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("tbl.filterStatistics", width = "100%", height = "100%")
          )
        ),
        shiny::column(
          width = 5,
          shinydashboard::box(
            width = 12,
            height = "100%",
            title = "Missings",
            status = "primary",
            solidHeader = TRUE,
            DT::dataTableOutput("tbl.filterMissings", width = "100%", height = "100%")
          )
        )
      ),
      shinydashboard::box(
        width = 12,
        height = "50%",
        title = "Filter",
        status = "primary",
        solidHeader = TRUE,
        DT::dataTableOutput("tbl.filterData", width = "100%", height = "100%")
      )
    )
  ),
  
  shinydashboard::tabItem(
    tabName = "tab_explore",
    fluidPage(
      width = 12,
      title = "Explore",
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shinydashboard::box(
            width = 12,
            #height = "100%",
            title = "Menue",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "si.exploreAbs",
              label = h5("Abscissae"),
              choices = list(),
              selected = 1,
              width = "100%"
            ),
            selectInput(
              "si.exploreOrd",
              label = h5("Ordinate"),
              choices = list(),
              selected = 1,
              width = "100%"
            )
          )
        ),
        shiny::column(
          width = 10,
          shinydashboard::tabBox(
            width = 12,
            title = "Explore",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabsetExplore",
            height = "100%",
            tabPanel("Graphics",
                     fluidPage(plotOutput(
                       "plt.exploreGraphics"
                     ))),
            tabPanel("Statistics",
                     DT::dataTableOutput("tbl.exploreStatistics"))
          )
        )
      )
    )),
  
  shinydashboard::tabItem(
    tabName = "tab_wizard",
    shiny::fluidPage(
      width = 12,
      height = "575px",
      title = "Model",
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shinydashboard::box(
            width = 12,
            height = "575px",
            title = "Menue",
            status = "primary",
            solidHeader = TRUE,
            shiny::checkboxInput(
              "cb.wizardLog",
              width = "100%",
              label = h5("Log"),
              value = TRUE
            ),
            shiny::checkboxInput(
              "cb.wizardRoot",
              width = "100%",
              label = h5("Root"),
              value = TRUE
            ),
            shiny::checkboxInput(
              "cb.wizardArcsine",
              width = "100%",
              label = h5("arcsine"),
              value = TRUE
            ),
            shiny::checkboxInput(
              "cb.wizardInverse",
              width = "100%",
              label = h5("inverse"),
              value = TRUE
            ),
            shiny::checkboxInput(
              "cb.wizardTLOP",
              width = "100%",
              label = h5("tuckeyLOP"),
              value = FALSE
            ),
            shiny::checkboxInput(
              "cb.wizardBoxCox",
              width = "100%",
              label = h5("boxCox"),
              value = FALSE
            ),
            shiny::checkboxInput(
              "cb.wizardMirror",
              width = "100%",
              label = h5("mirror"),
              value = FALSE
            ),
            shiny::hr(),
            shiny::actionButton(
              inputId = "ab.wizardOptimize",
              label = "optimize",
              width = "100%"
            ),
            shiny::actionButton(
              inputId = "ab.wizardReset",
              label = "reset",
              width = "100%"
            )
          )
        ),
        shiny::column(
          width = 10,
          shinydashboard::box(
            width = 12,
            height = "575px",
            title = "Optimized Models",
            status = "primary",
            solidHeader = TRUE,
            shinydashboard::tabBox(
              width = 12,
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabsetModel",
              height = "100%",
              tabPanel(
                title = "Optimal Transformations",
                shiny::fluidPage(
                  DT::dataTableOutput("tbl.optimizedTypes")
                )
              ),
              tabPanel(
                title = "Optimized Transformation Parameters",
                shiny::fluidPage(
                  DT::dataTableOutput("tbl.optimizedValues")
                )
              )
      )
    )
  )
  )
  )
  ),

  shinydashboard::tabItem(
    tabName = "tab_trafo",
    fluidPage(
      width = 12,
      height = "575px",
      title = "Transformation",
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shinydashboard::box(
            width = 12,
            height = "575px",
            title = "Menue",
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "si.transformationFeature",
              label = h5("Feature"),
              choices = list(),
              selected = 1,
              width = "100%"
            ),
            selectInput(
              "si.transformationType",
              label = h5("Transformation Type"),
              choices = list(),
              selected = 1,
              width = "100%"
            ),
            shiny::checkboxInput(
              "cb.mirrorLogic",
              label = h5("Mirror Data"),
              value = FALSE
            ),
            shiny::hr(),
            shiny::actionButton(
              inputId = "ab.trafoSet",
              label = "set",
              width = "100%"
            ),
            shiny::actionButton(
              inputId = "ab.trafoSetGlobal",
              label = "set globally",
              width = "100%"
            ),
            shiny::hr(),
            shiny::actionButton(
              inputId = "ab.trafoReset",
              label = "reset",
              width = "100%"
            )
          )
        ),
        shiny::column(
          width = 9,
          shinydashboard::box(
            width = 12,
            height = "100%",
            title = "Model Results",
            status = "primary",
            solidHeader = TRUE,
            tabBox(
              width = 12,
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabsetTransformation",
              height = "250px",
              tabPanel(
                "Feature",
                shiny::fluidPage(shiny::plotOutput("plt.featureTransformFit")),
                hr(),
                fluidPage(fluidRow(
                  column(6,
                         DT::dataTableOutput("tbl.featureModelParameter")),
                  column(6,
                         DT::dataTableOutput("tbl.featureModelQuality"))
                ))
              ),
              tabPanel(
                "Transformation Parameter",
                DT::dataTableOutput("tbl.transformationParameter")
              ),
              tabPanel("Model Parameter",
                       DT::dataTableOutput("tbl.modelParameter")),
              tabPanel("Model Quality",
                       DT::dataTableOutput("tbl.modelQuality")),
              tabPanel("Test Results",
                       DT::dataTableOutput("tbl.testResults"))
            )
        
  ))))),
  shinydashboard::tabItem(tabName = "tab_tidy",
                          fluidPage(
                            width = 12,
                            height = "575px",
                            shiny::fluidRow(
                              shiny::column(
                                width = 3,
                                shinydashboard::box(
                                  width = 12,
                                  height = "575px",
                                  title = "Menue",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  shiny::selectInput(
                                    "si.nanHandleMethod",
                                    label = h5("Cleaning Method"),
                                    choices = list(),
                                    selected = 1
                                  ),
                                  shiny::numericInput("ni.nanSeed", label = h5("Seed"),
                                                      value = 42),
                                  shiny::hr(),
                                  shiny::selectInput(
                                    "si.nanHandleFeature",
                                    label = h5("Feature"),
                                    choices = list(),
                                    selected = 1
                                  ),
                                  selectInput(
                                    "si.nanSummary",
                                    label = h5("Information Detail"),
                                    choices = list("Statistics", "Details"),
                                    selected = 1
                                  ),
                                  shiny::hr(),
                                  shiny::actionButton(
                                    inputId = "ab.fillMissings",
                                    label = "fill missings",
                                    width = "100%"
                                  ),
                                  shiny::actionButton(
                                    inputId = "ab.missingsReset",
                                    label = "reset",
                                    width = "100%"
                                  )
                                  )),
                              shiny::column(
                                width = 9,
                                shinydashboard::box(
                                  width = 12,
                                  height = "1050px",
                                  title = "Handle NANs",
                                  status = "primary",
                                  solidHeader = TRUE,
                            shinydashboard::tabBox(
                              width = 12,
                              # The id lets us use input$tabset1 on the server to find the current tab
                              id = "tabsetTidy",
                              height = "1000px",
                              shiny::tabPanel(
                                "NAN Summary",
                                shiny::fluidPage(
                                  shiny::plotOutput("plt.nanSummary"),
                                  shiny::hr(),
                                  DT::dataTableOutput("tbl.nanSummary")
                              )
                              ),
                              shiny::tabPanel(
                                "Results",
                                shiny::fluidPage(
                                  plotOutput("plt.nanCleaningSummary"),
                                  shiny::hr(),
                                  DT::dataTableOutput("tbl.nanCleaningSummary")
                                )
                              )
                            )))
                          )))
  
  
))
    

    # shinydashboard::tabItem(tabName = "tab_model",
    #                         fluidPage(
    #                           width =12,
    #                           height = 500,
    #                           tabBox(
    #                             width = 12,
    #                             title = "Model",
    #                             # The id lets us use input$tabset1 on the server to find the current tab
    #                             id = "tabsetModel", height = 500,
    #                             tabPanel("Transformation",
    #                                      fluidPage(
    #                                        fluidRow(
    #                                          column(2,
    #                                                 selectInput("si.transformationFeature", label = h5("Feature"),
    #                                                             choices = list(),
    #                                                             selected = 1),
    #                                                 selectInput("si.transformationType", label = h5("Transformation Type"),
    #                                                             choices = list(),
    #                                                             selected = 1),
    #                                                 shiny::checkboxInput("cb.mirrorLogic", label = h5("Mirror Data"),
    #                                                                      value = FALSE)),
    #                                          column(10,
    #                                                 plotOutput("plt.featureTransformFit"))
    #                                        )),
    #                                      hr(),
    #                                      fluidPage(
    #                                        fluidRow(
    #                                          column(6,
    #                                                 DT::dataTableOutput("tbl.featureTransformFit")),
    #                                          column(6,
    #                                                 DT::dataTableOutput("tbl.featureTransformTest"))))),
    #                             tabPanel("Transformation Parameter",
    #                                        DT::dataTableOutput("tbl.transformationParameter")
    #                                      ),
    #                             tabPanel("Model Parameter",
    #                                        DT::dataTableOutput("tbl.modelParameter")
    #                                      ),
    #                             tabPanel("Model Quality",
    #                                        DT::dataTableOutput("tbl.modelQuality")
    #                                      ),
    #                             tabPanel("Test Results",
    #                                        DT::dataTableOutput("tbl.testResults")
    #                                      )))),
    # shinydashboard::tabItem(tabName = "tab_tidy",
    #                         fluidPage(
    #                           width =12,
    #                           height = 500,
    #                           tabBox(
    #                             width = 12,
    #                             title = "NAN explore",
    #                             # The id lets us use input$tabset1 on the server to find the current tab
    #                             id = "tabsetTidy", height = 500,
    #                             tabPanel("Summary",
    #                                      fluidPage(
    #                                        fluidRow(
    #                                          column(2,
    #                                                 selectInput("si.nanSummary", label = h5("Information Detail"),
    #                                                             choices = list("Statistics", "Details"),
    #                                                             selected = 1)),
    #                                          column(10,
    #                                                 plotOutput("plt.nanSummary"))
    #                                        )),
    #                                      shiny::hr(),
    #                                      shiny::fluidPage(
    #                                        DT::dataTableOutput("tbl.nanSummary")
    #                                      )),
    #                             tabPanel("Tidy",
    #                                      shiny::fluidPage(
    #                                        shiny::fluidRow(
    #                                          shiny::column(2,
    #                                                        shiny::selectInput("si.nanHandleMethod", label = h5("Cleaning Method"),
    #                                                                    choices = list(),
    #                                                                    selected = 1),
    #                                                        shiny::numericInput("ni.nanSeed", label = h5("Seed"),
    #                                                                            value = 42),
    #                                                        shiny::selectInput("si.nanHandleFeature", label = h5("Feature"),
    #                                                                           choices = list(),
    #                                                                           selected = 1)),
    #                                          shiny::column(10,
    #                                                      plotOutput("plt.nanCleaningSummary"))
    #                                        )),
    #                                      shiny::hr(),
    #                                      shiny::fluidPage(
    #                                        DT::dataTableOutput("tbl.nanCleaningSummary")
    #                                      ))))),
    # shinydashboard::tabItem(tabName = "tab_revise",
    #                         shiny::fluidPage(
    #                           width =12,
    #                           height = 500,
    #                           shinydashboard::tabBox(
    #                             width = 12,
    #                             title = "Revise Outliers",
    #                             # The id lets us use input$tabset1 on the server to find the current tab
    #                             id = "tabsetRevise", height = 500,
    #                             shiny::tabPanel("Summary",
    #                                             shiny::fluidPage(
    #                                               shiny::fluidRow(
    #                                                 shiny::column(2,
    #                                                               shiny::selectInput("si.outlierSummaryPlt", label = h5("Plot Type"),
    #                                                                                  choices=list("Histogram", "Heatmap"),
    #                                                                                  selected = 1),
    #                                                               shiny::selectInput("si.outlierSummaryTbl", label = h5("Information Detail"),
    #                                                                           choices=list("Statistics", "Details"),
    #                                                                           selected = 1)),
    #                                                 shiny::column(10,
    #                                                               shiny::plotOutput("plt.outlierHist"))
    #                                               )),
    #                                             shiny::hr(),
    #                                             shiny::fluidPage(
    #                                               DT::dataTableOutput("tbl.outlierSummary")
    #                                             )),
    #                             shiny::tabPanel("Revise",
    #                                             shiny::fluidPage(
    #                                               shiny::fluidRow(
    #                                                 shiny::column(2,
    #                                                               shiny::selectInput("si.outlierHandleMethod", label = h5("Cleaning Method"),
    #                                                                                  choices = list(),
    #                                                                                  selected = 1),
    #                                                               shiny::numericInput("ni.outlierSeed", label = h5("Seed"),
    #                                                                                   value = 42),
    #                                                               shiny::selectInput("si.outlierHandleFeature", label = h5("Feature"),
    #                                                                                  choices = list(),
    #                                                                                  selected = 1)),
    #                                                 shiny::column(10,
    #                                                               plotOutput("plt.outlierCleaningSummary"))
    #                                               )),
    #                                             shiny::hr(),
    #                                             shiny::fluidPage(
    #                                               DT::dataTableOutput("tbl.outlierCleaningSummary")))
    #                         ))),
    # shinydashboard::tabItem(tabName = "tab_correlate",
    #                         shiny::fluidPage(
    #                           width = 12,
    #                           title = "Correlation and Regression",
    #                           id = "tabsetCorrelation",
    #                           shinydashboard::box(
    #                             width = 12,
    #                             title = "Linear Regression",
    #                             status = "primary",
    #                             solidHeader = TRUE,
    #                             collapsible = TRUE,
    #                             shiny::fluidPage(
    #                               shiny::fluidRow(
    #                                 shiny::column(2,
    #                                               shiny::selectInput("si.regressionAbs", label = h5("Abscissae"),
    #                                                                  choices = list(),
    #                                                                  selected = 1),
    #                                               shiny::selectInput("si.regressionOrd", label = h5("Ordinate"),
    #                                                                  choices = list(),
    #                                                                  selected = 1)),
    #                                 column(10,
    #                                        plotOutput("plt.regression"),
    #                                        DT::dataTableOutput("tbl.individualRegression"))))),
    #                           shiny::hr(),
    #                           shinydashboard::box(
    #                             width = 12,
    #                             title = "Statistics",
    #                             status = "primary",
    #                             solidHeader = TRUE,
    #                             collapsible = TRUE,
    #                             shiny::fluidPage(
    #                               shiny::fluidRow(
    #                                 shiny::column(2,
    #                                               shiny::selectInput("si.correlationStat", label = h5("Statistics"),
    #                                                                  choices = list("Intercept", "Slope", "p.regression", "Rho", "p.correlation"),
    #                                                                  selected = 1)),
    #                                 shiny::column(10,
    #                                               DT::dataTableOutput("tbl.correlationMatrix"))))))),
    # shinydashboard::tabItem(tabName = "tab_overview",
    #                         shiny::fluidPage(
    #                           width = 12,
    #                           shinydashboard::tabBox(
    #                             width = 12,
    #                             title = "Overview",
    #                             id = "tabsetOverview",
    #                             shiny::tabPanel("Data",
    #                                             shiny::fluidPage(
    #                                               shinydashboard::box(
    #                                                 width = 12,
    #                                                 title = "Input",
    #                                                 status = "primary",
    #                                                 solidHeader = TRUE,
    #                                                 collapsible = TRUE,
    #                                                 shiny::selectInput("si.overviewDataData", label = h5("Data Format"),
    #                                                                    choices = list("Raw", "Transformed", "Tidy"),
    #                                                                    selected = 1))),
    #                                             shiny::hr(),
    #                                             shiny::fluidPage(
    #                                               shinydashboard::box(
    #                                                 width = 12,
    #                                                 title = "Result",
    #                                                 status = "primary",
    #                                                 solidHeader = TRUE,
    #                                                 collapsible = TRUE,
    #                                                 DT::dataTableOutput("tbl.overviewData")))),
    #                             shiny::tabPanel("Frequency",
    #                                             shiny::fluidPage(
    #                                               shinydashboard::box(
    #                                                 width = 12,
    #                                                 title = "Input",
    #                                                 status = "primary",
    #                                                 solidHeader = TRUE,
    #                                                 collapsible = TRUE,
    #                                                 shiny::fluidRow(
    #                                                   shiny::column(6,
    #                                                                 shiny::selectInput("si.overviewFreqView", label = h5("Presentation Type"),
    #                                                                                    choices = list("Histogram", "Box Plot"),
    #                                                                                    selected = 1)),
    #                                                   shiny::column(6,
    #                                                                 shiny::selectInput("si.overviewFreqData", label = h5("Data Format"),
    #                                                                                    choices = list("Raw", "Transformed", "Tidy"),
    #                                                                                    selected = 1))))),
    #                                             shiny::hr(),
    #                                             shiny::fluidPage(
    #                                               shinydashboard::box(
    #                                                 width = 12,
    #                                                 title = "Result",
    #                                                 status = "primary",
    #                                                 solidHeader = TRUE,
    #                                                 collapsible = TRUE,
    #                                                 plotOutput("plt.overviewFreq", width="100%"),
    #                                                 style = "overflow-y:scroll; max-height: 600px"))),
    #                             shiny::tabPanel("Continuous",
    #                                             shiny::fluidPage(
    #                                               shinydashboard::box(
    #                                                 width = 12,
    #                                                 title = "Input",
    #                                                 status = "primary",
    #                                                 solidHeader = TRUE,
    #                                                 collapsible = TRUE,
    #                                                 shiny::fluidRow(
    #                                                   shiny::column(6,
    #                                                                 shiny::selectInput("si.overviewContView", labe = h5("Abscissae"),
    #                                                                                    choices = list(),
    #                                                                                    selected = 1)),
    #                                                   shiny::column(6,
    #                                                                 shiny::selectInput("si.overviewContData", labe = h5("Data Format"),
    #                                                                                    choices = list("Raw", "Transformed", "Tidy"),
    #                                                                                    selected = 1))))),
    #                                             shiny::hr(),
    #                                             shiny::fluidPage(
    #                                               shinydashboard::box(
    #                                                 width = 12,
    #                                                 title = "Result",
    #                                                 status = "primary",
    #                                                 solidHeader = TRUE,
    #                                                 collapsible = TRUE,
    #                                                 plotOutput("plt.overviewCont", width="100%"),
    #                                                 style = "overflow-y:scroll; max-height: 600px"))),
    #                             shiny::tabPanel("Statistics",
    #                                             shiny::fluidPage(
    #                                               shinydashboard::box(
    #                                                 width = 12,
    #                                                 title = "Input",
    #                                                 status = "primary",
    #                                                 solidHeader = TRUE,
    #                                                 collapsible = TRUE,
    #                                                 shiny::selectInput("si.overviewStatisticsData", label = h5("Data Format"),
    #                                                                    choices = list("Raw", "Transformed", "Tidy"),
    #                                                                    selected = 1))),
    #                                             shiny::hr(),
    #                                             shiny::fluidPage(
    #                                               shinydashboard::box(
    #                                                 width = 12,
    #                                                 title = "Result",
    #                                                 status = "primary",
    #                                                 solidHeader = TRUE,
    #                                                 collapsible = TRUE,
    #                                                 DT::dataTableOutput("tbl.overviewStatistics"))))
    #                           )
    #                         )),
    # shinydashboard::tabItem(tabName = "tab_export",
    #                         fluidPage(
    #                           width = 12,
    #                           shinydashboard::box(
    #                             width = 12,
    #                             title = "Download Results",
    #                             status = "primary",
    #                             solidHeader = TRUE,
    #                             collapsible = TRUE,
    #                               shiny::selectInput("si.exportType", label = h5("Data Set"),
    #                                                 choices = list(),
    #                                                 selected = 1),
    #                               shiny::selectInput("si.exportFormat", label = h5("Format"),
    #                                                  choices = list(),
    #                                                  selected = 1),
    # 
    #                               downloadButton('db.export', 'Download'))))
