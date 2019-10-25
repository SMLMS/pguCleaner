library(shiny)

importMenue <- shiny::fluidPage(
  shiny::fileInput("file", label = h3("File input"), accept=c(".xls", ".xlsx", ".txt", ".csv")),
  shiny::hr(),
  shiny::verbatimTextOutput("txt.loadInfo")
)
