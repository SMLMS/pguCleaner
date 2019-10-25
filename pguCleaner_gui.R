library(shiny)
source(file = "R/pguGlobals.R", local=TRUE)

main <- function() {
  runApp("gui", launch.browser=TRUE)
}

main()
