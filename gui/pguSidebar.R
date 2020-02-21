library(shiny)
library(shinydashboard)

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    id = "menue",
    shinydashboard::menuItem("Import",
                             tabName = "tab_import",
                             icon = shiny::icon("download")),
    shinydashboard::menuItem("Filter",
                             tabName = "tab_filter",
                             icon = shiny::icon("filter")
                             ),
    shinydashboard::menuItem("Explore",
                             tabName = "tab_explore",
                             icon = shiny::icon("compass")
                             ),
    shinydashboard::menuItem("LOQ",
                             icon = shiny::icon("exchange-alt"),
                             shinydashboard::menuSubItem("Detect",
                                                         icon = shiny::icon("microscope"),
                                                         tabName = "tab_detect_loq"),
                             shinydashboard::menuSubItem("Mutate",
                                                         icon = shiny::icon("sitemap"),
                                                         tabName = "tab_mutate_loq")
                             ),
    shinydashboard::menuItem("Transformation",
                             tabName = "tab_trafo",
                             icon = shiny::icon(name = "random", class = "fas"),
                             shinydashboard::menuSubItem("Detect",
                                                         icon = shiny::icon("microscope"),
                                                         tabName = "tab_detect_trafo"),
                             shinydashboard::menuSubItem("Transform",
                                                         icon = shiny::icon("sitemap"),
                                                         tabName = "tab_mutate_trafo")
                             ),
    shinydashboard::menuItem("Missings",
                             tabName = "tab_tidy",
                             icon = shiny::icon("fill-drip"),
                             shinydashboard::menuSubItem("Detect",
                                                         icon = shiny::icon("microscope")),
                             shinydashboard::menuSubItem("Mutate",
                                                         icon = shiny::icon("sitemap"))
                             ),
    shinydashboard::menuItem("Outliers",
                             tabName = "tab_revise",
                             icon = shiny::icon("microscope"),
                             shinydashboard::menuSubItem("Detect",
                                                         icon = shiny::icon("microscope")),
                             shinydashboard::menuSubItem("Mutate", icon = shiny::icon("sitemap"))
                             ),
    shinydashboard::menuItem("Correlation",
                             tabName = "tab_correlation",
                             icon = shiny::icon("chart-bar")
                             ),
    shinydashboard::menuItem("Regression",
                             tabName = "tab_regression",
                             icon = shiny::icon("chart-line")
                             ),
    shinydashboard::menuItem("Export",
                             tabName = "tab_export",
                             icon = shiny::icon("upload")
                             )

  )
)
