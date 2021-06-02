library(DT)
library(shiny)
library(leaflet)
library(RMySQL)
library(tidyr)
library(shinyBS)
library(shinydashboard)

source("../utils.R")

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
      menu
    ),
    dashboardBody(
      fluidPage(
        title = "Queries",
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("show_vars0", "Variables",
                               choices = character(0)),
            conditionalPanel("output.check_tv >0",
                             checkboxGroupInput("show_vars1", "Study",
                                                choices = character(0))),
            conditionalPanel("output.check_exp >0",
                             sliderInput("range", "Range:",
                                         min = 1,max=2,
                                         value = c(1,2)),
                             leafletOutput("mymap1"))
          ),
          mainPanel(
            bsCollapse(id = "collapseExample", open = "Data",
                       bsCollapsePanel("Information", dataTableOutput("info"), style = "warning"),
                       bsCollapsePanel("Summary", h4("Range of available years"),
                                       verbatimTextOutput("summary"),
                                       h4("Summary of cases per factor / treatment"),
                                       verbatimTextOutput("summary2"),
                                       h4("Don't forget to see the information for each study! (see above)"), style = "info"),
                       bsCollapsePanel("Data", DT::dataTableOutput("mytable0"), style="success")
            )
          )
        )  
      )
    )
  )
)