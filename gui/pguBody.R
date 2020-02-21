library(shiny)
library(shinydashboard)

menueColumnWidth <- 3
dataColumnWidth <- 12 - menueColumnWidth
halfDataColumnWidth <- as.integer(dataColumnWidth/2)

body <- shinydashboard::dashboardBody(shinydashboard::tabItems(
  shinydashboard::tabItem(
    tabName = "tab_import",
    shiny::fluidPage(
      width = 12,
      title = "Import",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::fileInput(
            "fi.import",
            label = h5(" Select Excel file "),
            accept = c(".xls", ".xlsx"),
            width = "100%"
          ),
          shiny::hr(),
          shiny::actionButton("ab.import",
                              label = h5("import"),
                              width = "100%")
        ),
        shiny::column(
          
          width = dataColumnWidth,
          shinydashboard::tabBox(
            width = 12,
            title = "Data Types",
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabsetImport",
            height = "100%",
            shiny::tabPanel("raw data",
                            DT::dataTableOutput("tbl.rawDataInfo", width = "100%")
            ),
            shiny::tabPanel("loq",
                            DT::dataTableOutput("tbl.loqInfo", width = "100%")
            ),
            shiny::tabPanel("metadata",
                            DT::dataTableOutput("tbl.metadataInfo", width = "100%")
            )
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
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Filter"),
          shiny::br(),
          shiny::br(),
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
        ),
        shiny::column(
          width = dataColumnWidth,
          DT::dataTableOutput("tbl.filter", width = "100%"),
          shiny::fluidRow(
            shiny::column(
              width = 8,
              shiny::h3("Statistics"),
              DT::dataTableOutput("tbl.filterStatistics", width = "100%")
            ),
            shiny::column(
              width = 4,
              shiny::h3("Missings"),
              DT::dataTableOutput("tbl.filterMissings", width = "100%")
            )
          )
        )
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
          width = menueColumnWidth,
          shiny::h1("Explore"),
          shiny::br(),
          shiny::br(),
          selectInput(
            "si.exploreAbs",
            label = h5("Abscissa"),
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
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::h3("Feature Scatter Plot"),
          shiny::plotOutput("plt.exploreGraphic"),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h3("Abscissa Statistics"),
              shiny::plotOutput("plt.exploreAbscissaGraphic"),
              shiny::br(),
              DT::dataTableOutput("tbl.exploreAbscissaStatistics")
            ),
            shiny::column(
              width = 6,
              shiny::h3("Ordinate Statistics"),
              shiny::plotOutput("plt.exploreOrdinateGraphic"),
              shiny::br(),
              DT::dataTableOutput("tbl.exploreOrdinateStatistics")
            )
          )
        )
      )
    )
  ),
  
  shinydashboard::tabItem(
    tabName = "tab_detect_loq",
    shiny::fluidPage(
      width = 12,
      title = "Detect LOQ",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Detect LOQ Outlier"),
          shiny::br(),
          shiny::br(),
          shiny::selectInput(
            "si.loqNaHandling",
            width = '100%',
            label = h5("NA Handling"),
            choices = list(),
            selected = 1,
          ),
          shiny::selectInput(
            "si.loqDetectFeature",
            width = '100%',
            label = h5("Feature"),
            choices = list(),
            selected = 1
          ),
          shiny::br(),
          shiny::br(),
          shiny::actionButton(
            inputId = "ab.detectLoq",
            label = "analyze LOQ",
            width = "100%"
          ),
          shiny::br(),
          shiny::br(),
          shiny::actionButton(
            inputId = "ab.resetDetectLoq",
            label = "reset",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::h3("LOQ Distribution"),
          shiny::plotOutput("plt.loqDetectStatistics"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              h3("Feature Plot"),
              shiny::plotOutput("plt.loqDetectFeature")
            ),
            shiny::column(
              width = 6,
              h3("Feature Data"),
              DT::dataTableOutput("tbl.loqDetectFeature")
            )
          ),
          shiny::br(),
          shiny::br(),
          shiny::h3("LOQ Statistics"),
          DT::dataTableOutput("tbl.loqDetectStatistics"),
          shiny::br(),
          shiny::h3("LOQ Outlier"),
          DT::dataTableOutput("tbl.loqDetectOutlier"),
          shiny::br(),
          shiny::h3("LOQ Data"),
          DT::dataTableOutput("tbl.loqDetectData")
        )
      )
    )
  ),
  
  shinydashboard::tabItem(
    tabName = "tab_mutate_loq",
    shiny::fluidPage(
      width = 12,
      title = "Mutate LOQ",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Mutate LOQ Outlier"),
          shiny::br(),
          shiny::br(),
          shiny::selectInput(
            "si.lloqSubstitute",
            width = '100%',
            label = h5("LLOQ Substitute"),
            choices = list(),
            selected = 1,
          ),
          shiny::selectInput(
            "si.uloqSubstitute",
            width = '100%',
            label = h5("ULOQ Substitute"),
            choices = list(),
            selected = 1,
          ),
          shiny::selectInput(
            "si.loqMutateFeature",
            width = '100%',
            label = h5("Feature"),
            choices = list(),
            selected = 1
          ),
          shiny::br(),
          shiny::br(),
          shiny::actionButton(
            inputId = "ab.mutateLoq",
            label = "mutate LOQ",
            width = "100%"
          ),
          shiny::br(),
          shiny::br(),
          shiny::actionButton(
            inputId = "ab.resetMutateLoq",
            label = "reset",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::h3("LOQ Distribution"),
          shiny::plotOutput("plt.loqMutateStatistics"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              h3("Feature Plot"),
              shiny::plotOutput("plt.loqMutateFeature")
            ),
            shiny::column(
              width = 6,
              h3("Feature Data"),
              DT::dataTableOutput("tbl.loqMutateFeature")
            )
          ),
          shiny::br(),
          shiny::br(),
          shiny::h3("LOQ Statistics"),
          DT::dataTableOutput("tbl.loqMutateStatistics"),
          shiny::br(),
          shiny::h3("LOQ Outlier"),
          DT::dataTableOutput("tbl.loqMutateOutlier"),
          shiny::br(),
          shiny::h3("LOQ Data"),
          DT::dataTableOutput("tbl.loqMutateData")
        )
      )
    )
  ),
  
  shinydashboard::tabItem(
    tabName = "tab_detect_trafo",
    shiny::fluidPage(
      width = 12,
      title = "Parameter Wizard",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Detect Model Parameter"),
          shiny::br(),
          shiny::br(),
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
          shiny::br(),
          shiny::br(),
          shiny::actionButton(
            inputId = "ab.wizardOptimize",
            label = "optimize",
            width = "100%"
          ),
          shiny::br(),
          shiny::br(),
          shiny::actionButton(
            inputId = "ab.wizardReset",
            label = "reset",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::h3("Optimal Transformations"),
          DT::dataTableOutput("tbl.trafoDetectTypes"),
          shiny::br(),
          shiny::h3("Optimized Transformation Parameters"),
          DT::dataTableOutput("tbl.trafoDetectParameters")
        )
      )
    )
  ),
  
  shinydashboard::tabItem(
    tabName = "tab_mutate_trafo",
    fluidPage(
      width = 12,
      title = "Transformation",
      shiny::fluidRow(
        shiny::column(
          width = menueColumnWidth,
          shiny::h1("Transformation"),
          shiny::br(),
          shiny::br(),
          selectInput(
            "si.trafoMutateFeature",
            label = h5("Feature"),
            choices = list(),
            selected = 1,
            width = "100%"
          ),
          selectInput(
            "si.trafoMutateType",
            label = h5("Transformation Type"),
            choices = list(),
            selected = 1,
            width = "100%"
          ),
          shiny::checkboxInput(
            "cb.trafoMutateMirror",
            label = h5("Mirror Data"),
            value = FALSE
          ),
          shiny::hr(),
          shiny::actionButton(
            inputId = "ab.trafoMutateFeature",
            label = "set",
            width = "100%"
          ),
          shiny::actionButton(
            inputId = "ab.trafoMutateSetGlobal",
            label = "set globally",
            width = "100%"
          ),
          shiny::hr(),
          shiny::actionButton(
            inputId = "ab.trafoMutateReset",
            label = "reset",
            width = "100%"
          )
        ),
        shiny::column(
          width = dataColumnWidth,
          shiny::h3("Feature Transformation"),
          shiny::plotOutput("plt.trafoMutateFeature", height = "600"),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::h5("Model Parameter"),
              DT::dataTableOutput("tbl.trafoMutateFeatureParameter")
              ),
            shiny::column(
              width = 6,
              shiny::h5("Model Quality"),
              DT::dataTableOutput("tbl.trafoMutateFeatureQuality")
              )
          ),
          shiny::br(),
          shiny::br(),
          shiny::h3("Global Transformation"),
          shiny::h5("Transformation Parameter"),
          DT::dataTableOutput("tbl.trafoMutateGlobalParameter"),
          shiny::br(),
          shiny::br(),
          shiny::h5("Model Parameter"),
          DT::dataTableOutput("tbl.trafoMutateGlobalModel"),
          shiny::br(),
          shiny::br(),
          shiny::h5("Model Quality"),
          DT::dataTableOutput("tbl.trafoMutateGlobalQuality"),
          shiny::br(),
          shiny::br(),
          shiny::h5("Test Results"),
          DT::dataTableOutput("tbl.trafoMutateGlobalTests")
        )
      )
    )
  )
        

  
  
  # 
  # shinydashboard::tabItem(
  #   tabName = "tab_trafo",
  #   fluidPage(
  #     width = 12,
  #     height = "575px",
  #     title = "Transformation",
  #     shiny::fluidRow(
  #       shiny::column(
  #         width = 3,
  #         shinydashboard::box(
  #           width = 12,
  #           height = "575px",
  #           title = "Menue",
  #           status = "primary",
  #           solidHeader = TRUE,
  #           selectInput(
  #             "si.transformationFeature",
  #             label = h5("Feature"),
  #             choices = list(),
  #             selected = 1,
  #             width = "100%"
  #           ),
  #           selectInput(
  #             "si.transformationType",
  #             label = h5("Transformation Type"),
  #             choices = list(),
  #             selected = 1,
  #             width = "100%"
  #           ),
  #           shiny::checkboxInput(
  #             "cb.mirrorLogic",
  #             label = h5("Mirror Data"),
  #             value = FALSE
  #           ),
  #           shiny::hr(),
  #           shiny::actionButton(
  #             inputId = "ab.trafoSet",
  #             label = "set",
  #             width = "100%"
  #           ),
  #           shiny::actionButton(
  #             inputId = "ab.trafoSetGlobal",
  #             label = "set globally",
  #             width = "100%"
  #           ),
  #           shiny::hr(),
  #           shiny::actionButton(
  #             inputId = "ab.trafoReset",
  #             label = "reset",
  #             width = "100%"
  #           )
  #         )
  #       ),
  #       shiny::column(
  #         width = 9,
  #         shinydashboard::box(
  #           width = 12,
  #           height = "100%",
  #           title = "Model Results",
  #           status = "primary",
  #           solidHeader = TRUE,
  #           tabBox(
  #             width = 12,
  #             # The id lets us use input$tabset1 on the server to find the current tab
  #             id = "tabsetTransformation",
  #             height = "250px",
  #             tabPanel(
  #               "Feature",
  #               shiny::fluidPage(shiny::plotOutput("plt.featureTransformFit")),
  #               hr(),
  #               fluidPage(fluidRow(
  #                 column(6,
  #                        DT::dataTableOutput("tbl.featureModelParameter")),
  #                 column(6,
  #                        DT::dataTableOutput("tbl.featureModelQuality"))
  #               ))
  #             ),
  #             tabPanel(
  #               "Transformation Parameter",
  #               DT::dataTableOutput("tbl.transformationParameter")
  #             ),
  #             tabPanel("Model Parameter",
  #                      DT::dataTableOutput("tbl.modelParameter")),
  #             tabPanel("Model Quality",
  #                      DT::dataTableOutput("tbl.modelQuality")),
  #             tabPanel("Test Results",
  #                      DT::dataTableOutput("tbl.testResults"))
  #           )
  #       
  # ))))),
  # shinydashboard::tabItem(tabName = "tab_tidy",
  #                         fluidPage(
  #                           width = 12,
  #                           height = "575px",
  #                           shiny::fluidRow(
  #                             shiny::column(
  #                               width = 3,
  #                               shinydashboard::box(
  #                                 width = 12,
  #                                 height = "575px",
  #                                 title = "Menue",
  #                                 status = "primary",
  #                                 solidHeader = TRUE,
  #                                 shiny::selectInput(
  #                                   "si.nanHandleMethod",
  #                                   label = h5("Mutation Method"),
  #                                   choices = list(),
  #                                   selected = 1
  #                                 ),
  #                                 shiny::numericInput("ni.nanSeed", label = h5("Seed"),
  #                                                     value = 42),
  #                                 shiny::hr(),
  #                                 shiny::selectInput(
  #                                   "si.nanHandleFeature",
  #                                   label = h5("Feature"),
  #                                   choices = list(),
  #                                   selected = 1
  #                                 ),
  #                                 selectInput(
  #                                   "si.nanSummary",
  #                                   label = h5("Information Detail"),
  #                                   choices = list("Statistics", "Missings", "Data"),
  #                                   selected = 1
  #                                 ),
  #                                 shiny::hr(),
  #                                 shiny::actionButton(
  #                                   inputId = "ab.analyzeMissings",
  #                                   label = "analyze NA",
  #                                   width = "100%"
  #                                 ),
  #                                 shiny::actionButton(
  #                                   inputId = "ab.mutateMissings",
  #                                   label = "mutate NA",
  #                                   width = "100%"
  #                                 ),
  #                                 shiny::actionButton(
  #                                   inputId = "ab.resetMissings",
  #                                   label = "reset",
  #                                   width = "100%"
  #                                 )
  #                                 )),
  #                             shiny::column(
  #                               width = 9,
  #                               shinydashboard::box(
  #                                 width = 12,
  #                                 height = "1050px",
  #                                 title = "Handle NANs",
  #                                 status = "primary",
  #                                 solidHeader = TRUE,
  #                           shinydashboard::tabBox(
  #                             width = 12,
  #                             # The id lets us use input$tabset1 on the server to find the current tab
  #                             id = "tabsetTidy",
  #                             height = "1000px",
  #                             shiny::tabPanel(
  #                               "NAN Summary",
  #                               shiny::fluidPage(
  #                                 shiny::plotOutput("plt.nanSummary"),
  #                                 shiny::hr(),
  #                                 DT::dataTableOutput("tbl.nanSummary")
  #                             )
  #                             ),
  #                             shiny::tabPanel(
  #                               "Results",
  #                               shiny::fluidPage(
  #                                 plotOutput("plt.nanCleaningSummary"),
  #                                 shiny::hr(),
  #                                 DT::dataTableOutput("tbl.nanCleaningSummary")
  #                               )
  #                             )
  #                           )))
  #                         ))),
  # 
  # shinydashboard::tabItem(tabName = "tab_revise",
  #                         fluidPage(
  #                           width = 12,
  #                           height = "575px",
  #                           shiny::fluidRow(
  #                             shiny::column(
  #                               width = 3,
  #                               shinydashboard::box(
  #                                 width = 12,
  #                                 height = "575px",
  #                                 title = "Menue",
  #                                 status = "primary",
  #                                 solidHeader = TRUE,
  #                                 shiny::selectInput(
  #                                   "si.outHandleMethod",
  #                                   label = h5("Mutation Method"),
  #                                   choices = list(),
  #                                   selected = 1
  #                                 ),
  #                                 shiny::numericInput("ni.outSeed", label = h5("Seed"),
  #                                                     value = 42),
  #                                 shiny::hr(),
  #                                 shiny::selectInput(
  #                                   "si.outHandleFeature",
  #                                   label = h5("Feature"),
  #                                   choices = list(),
  #                                   selected = 1
  #                                 ),
  #                                 selectInput(
  #                                   "si.outSummary",
  #                                   label = h5("Information Detail"),
  #                                   choices = list("Statistics", "Outliers", "Data"),
  #                                   selected = 1
  #                                 ),
  #                                 shiny::hr(),
  #                                 shiny::actionButton(
  #                                   inputId = "ab.detectOutliers",
  #                                   label = "analyze outliers",
  #                                   width = "100%"
  #                                 ),
  #                                 shiny::actionButton(
  #                                   inputId = "ab.reviseOutliers",
  #                                   label = "mutate outliers",
  #                                   width = "100%"
  #                                 ),
  #                                 shiny::actionButton(
  #                                   inputId = "ab.outliersReset",
  #                                   label = "reset",
  #                                   width = "100%"
  #                                 )
  #                               )),
  #                             shiny::column(
  #                               width = 9,
  #                               shinydashboard::box(
  #                                 width = 12,
  #                                 height = "1050px",
  #                                 title = "Revise Outliers",
  #                                 status = "primary",
  #                                 solidHeader = TRUE,
  #                                 shinydashboard::tabBox(
  #                                   width = 12,
  #                                   # The id lets us use input$tabset1 on the server to find the current tab
  #                                   id = "tabsetTidy",
  #                                   height = "1000px",
  #                                   shiny::tabPanel(
  #                                     "Outlier Summary",
  #                                     shiny::fluidPage(
  #                                       shiny::plotOutput("plt.outSummary"),
  #                                       shiny::hr(),
  #                                       DT::dataTableOutput("tbl.outSummary")
  #                                     )
  #                                   ),
  #                                   shiny::tabPanel(
  #                                     "Results",
  #                                     shiny::fluidPage(
  #                                       plotOutput("plt.outCleaningSummary"),
  #                                       shiny::hr(),
  #                                       DT::dataTableOutput("tbl.outCleaningSummary")
  #                                     )
  #                                   )
  #                                 )))
  #                           ))),
  # shinydashboard::tabItem(tabName = "tab_correlation",
  #                         shiny::fluidPage(width = 12,
  #                                          id = "tabsetCorrelation",
  #                                          shiny::fluidRow(
  #                                            shiny::column(
  #                                              width = 3,
  #                                              height = "800px",
  #                                              shinydashboard::box(
  #                                                width = 12,
  #                                                height = "800px",
  #                                                title = "Menue",
  #                                                status = "primary",
  #                                                solidHeader = TRUE,
  #                                                shiny::fluidPage(width = 12,
  #                                                                 height = "400px",
  #                                                                 shiny::selectInput(
  #                                                                   inputId = "si.correlationMatrix",
  #                                                                   width = "100%",
  #                                                                   label = h5("Statistics"),
  #                                                                   choices = list("r", "p.Pearson",
  #                                                                                  "tau", "p.Kendall",
  #                                                                                  "rho", "p.Spearman"),
  #                                                                   selected = 1
  #                                                                 )
  #                                                ),
  #                                                shiny::hr(),
  #                                                shiny::fluidPage(width = 12,
  #                                                                 height = "400px",
  #                                                                 shiny::actionButton(
  #                                                                   inputId = "ab.correlation",
  #                                                                   label = "calculate",
  #                                                                   width = "100%")
  #                                                                 )
  #                                              )
  #                                            ),
  #                                            shiny::column(
  #                                              width = 9,
  #                                              height = "800px",
  #                                              shinydashboard::box(
  #                                                width = 12,
  #                                                height = "800px",
  #                                                title = "Correlation",
  #                                                status = "primary",
  #                                                solidHeader = TRUE,
  #                                                shiny::fluidPage(
  #                                                  width = 12,
  #                                                  height = "800px",
  #                                                  DT::dataTableOutput("tbl.correlationMatrix")
  #                                                )
  #                                              )
  #                                            )
  #                                          )
  #                                          )
  #                         ),
  # shinydashboard::tabItem(tabName = "tab_regression",
  #                         shiny::fluidPage(
  #                           width = 12,
  #                           title = "Regression",
  #                           id = "tabsetRegression",
  #                           shiny::fluidRow(
  #                             shiny::column(
  #                               width = 3,
  #                               shinydashboard::box(
  #                                 width = 12,
  #                                 height = "800px",
  #                                 title = "Menue",
  #                                 status = "primary",
  #                                 solidHeader = TRUE,
  #                                 shiny::fluidPage(
  #                                   width = 12,
  #                                   height = "400px",
  #                                   shiny::selectInput(
  #                                     "si.regressionAbs",
  #                                     width = "100%",
  #                                     label = h5("Abscissae"),
  #                                     choices = list(),
  #                                     selected = 1
  #                                   ),
  #                                   shiny::selectInput(
  #                                     "si.regressionOrd",
  #                                     width = "100%",
  #                                     label = h5("Ordinate"),
  #                                     choices = list(),
  #                                     selected = 1
  #                                   ),
  #                                   shiny::selectInput(
  #                                     "si.regressionMatrix",
  #                                     width = "100%",
  #                                     label = h5("Statistics"),
  #                                     choices = list("Intercept", "p.Intercept",
  #                                                    "Slope", "p.Slope"),
  #                                     selected = 1
  #                                   )
  #                                 ),
  #                                 shiny::hr(),
  #                                 shiny::fluidPage(
  #                                   width = 12,
  #                                   height = "400px",
  #                                   shiny::actionButton(
  #                                     inputId = "ab.regression",
  #                                     label = "calculate",
  #                                     width = "100%"
  #                                   )
  #                                 )
  #                               )
  #                             ),
  #                             shiny::column(
  #                               width = 9,
  #                               shinydashboard::box(
  #                                 width = 12,
  #                                 height = "800px",
  #                                 title = "Regression",
  #                                 status = "primary",
  #                                 solidHeader = TRUE,
  #                                 shinydashboard::tabBox(
  #                                   width = 12,
  #                                   id = "tabsetRegression",
  #                                   height = "700px",
  #                                   shiny::tabPanel(
  #                                     width = 12,
  #                                     height = "700px",
  #                                     title = "Feature",
  #                                     shiny::fluidRow(
  #                                       shiny::column(
  #                                         width = 8,
  #                                         shiny::fluidPage(
  #                                           width = 12,
  #                                           height = "700px",
  #                                           plotOutput("plt.regressionFeature")
  #                                         )
  #                                       ),
  #                                       shiny::column(
  #                                         width  = 4,
  #                                         shiny::fluidPage(
  #                                           width = 12,
  #                                           height = "700px",
  #                                           DT::dataTableOutput("tbl.regressionFeature")
  #                                         )
  #                                       )
  #                                   )
  #                                   ),
  #                                   shiny::tabPanel(
  #                                     width = 12,
  #                                     height = "700px",
  #                                     title = "Statistics",
  #                                     shiny::fluidPage(
  #                                       width = 12,
  #                                       height = "700px",
  #                                       DT::dataTableOutput("tbl.regressionMatrix")
  #                                     )
  #                                   )
  #                                   )
  #                                 )
  #                               )
  #                             )
  #                           )
  #                         ),
  # shinydashboard::tabItem(tabName = "tab_export",
  #                         fluidPage(
  #                           width = 12,
  #                           shinydashboard::box(
  #                             width = 12,
  #                             title = "Download Results",
  #                             status = "primary",
  #                             solidHeader = TRUE,
  #                             collapsible = TRUE,
  #                             shiny::selectInput("si.exportType", label = h5("Data Set"),
  #                                                choices = list(),
  #                                                selected = 1),
  #                             shiny::selectInput("si.suffix", label = h5("Format"),
  #                                                choices = list(),
  #                                                selected = 1),
  #                             
  #                             shiny::downloadButton('db.export', 'Download'),
  #                             shiny::actionButton("ab.export", "action"))))
))
    


