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
                                    label = "Handle NANs",
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
                          ))),
  
  shinydashboard::tabItem(tabName = "tab_revise",
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
                                    "si.outHandleMethod",
                                    label = h5("Cleaning Method"),
                                    choices = list(),
                                    selected = 1
                                  ),
                                  shiny::numericInput("ni.outSeed", label = h5("Seed"),
                                                      value = 42),
                                  shiny::hr(),
                                  shiny::selectInput(
                                    "si.outHandleFeature",
                                    label = h5("Feature"),
                                    choices = list(),
                                    selected = 1
                                  ),
                                  selectInput(
                                    "si.outSummary",
                                    label = h5("Information Detail"),
                                    choices = list("Statistics", "Outliers", "Data"),
                                    selected = 1
                                  ),
                                  shiny::hr(),
                                  shiny::actionButton(
                                    inputId = "ab.detectOutliers",
                                    label = "detect outliers",
                                    width = "100%"
                                  ),
                                  shiny::actionButton(
                                    inputId = "ab.reviseOutliers",
                                    label = "revise outliers",
                                    width = "100%"
                                  ),
                                  shiny::actionButton(
                                    inputId = "ab.outliersReset",
                                    label = "reset",
                                    width = "100%"
                                  )
                                )),
                              shiny::column(
                                width = 9,
                                shinydashboard::box(
                                  width = 12,
                                  height = "1050px",
                                  title = "Revise Outliers",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  shinydashboard::tabBox(
                                    width = 12,
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabsetTidy",
                                    height = "1000px",
                                    shiny::tabPanel(
                                      "Outlier Summary",
                                      shiny::fluidPage(
                                        shiny::plotOutput("plt.outSummary"),
                                        shiny::hr(),
                                        DT::dataTableOutput("tbl.outSummary")
                                      )
                                    ),
                                    shiny::tabPanel(
                                      "Results",
                                      shiny::fluidPage(
                                        plotOutput("plt.outCleaningSummary"),
                                        shiny::hr(),
                                        DT::dataTableOutput("tbl.outCleaningSummary")
                                      )
                                    )
                                  )))
                            ))),
  shinydashboard::tabItem(tabName = "tab_regression",
                          shiny::fluidPage(
                            width = 12,
                            title = "Regression",
                            id = "tabsetCorrelation",
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
                                    "si.regressionAbs",
                                    width = "100%",
                                    label = h5("Abscissae"),
                                    choices = list(),
                                    selected = 1
                                  ),
                                  shiny::selectInput(
                                    "si.regressionOrd",
                                    width = "100%",
                                    label = h5("Ordinate"),
                                    choices = list(),
                                    selected = 1
                                  ),
                                  shiny::selectInput(
                                    "si.regressionStats",
                                    width = "100%",
                                    label = h5("Statistics"),
                                    choices = list("Intercept", "p.Intercept",
                                                   "Slope", "p.Slope",
                                                   "r", "p.Pearson",
                                                   "tau", "p.Kendall",
                                                   "rho", "p.Spearman"),
                                    selected = 1
                                  ),
                                  shiny::hr(),
                                  shiny::actionButton(
                                    inputId = "ab.regression",
                                    label = "calculate",
                                    width = "100%"
                                  ),
                                  shiny::actionButton(
                                    inputId = "ab.refreshRegression",
                                    label = "refresh",
                                    width = "100%"
                                  ),
                                  shiny::actionButton(
                                    inputId = "ab.resetRegression",
                                    label = "reset",
                                    width = "100%"
                                  )
                                )
                              ),
                              shiny::column(
                                width = 9,
                            shinydashboard::box(
                              width = 12,
                              title = "Linear Regression",
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              plotOutput("plt.regressionFeature"),
                              shiny::hr(),
                              shiny::fluidRow(
                                shiny::column(
                                  width = 6,
                                  shiny::fluidPage(
                                    DT::dataTableOutput("tbl.regressionFeature")
                                  )
                                ),
                                shiny::column(
                                  width = 6,
                                  shiny::fluidPage(
                                    DT::dataTableOutput("tbl.correlationFeature")
                                  )
                                )
                              )
                            ),
                            shinydashboard::box(
                              width = 12,
                              title = "Statistics",
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              DT::dataTableOutput("tbl.regressionMatrix"))
                              )
      )
    )
  ),
  shinydashboard::tabItem(tabName = "tab_export",
                          fluidPage(
                            width = 12,
                            shinydashboard::box(
                              width = 12,
                              title = "Download Results",
                              status = "primary",
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              shiny::selectInput("si.exportType", label = h5("Data Set"),
                                                 choices = list(),
                                                 selected = 1),
                              shiny::selectInput("si.suffix", label = h5("Format"),
                                                 choices = list(),
                                                 selected = 1),
                              
                              shiny::downloadButton('db.export', 'Download'))))
))
    


