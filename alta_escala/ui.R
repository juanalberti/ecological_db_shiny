library(shiny)
library(shinydashboard)
library(shinyBS)

source("../utils.R")
source("../connection.R")

shinyUI(
  dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
      menu
    ),
    dashboardBody(
      fluidPage(
        shinyjs::useShinyjs(),
        titlePanel("Validate spreadsheet"),
        sidebarLayout(
          sidebarPanel(
            fileInput("file1", "Upload file",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                        ".ods",
                        ".xlsx",
                        ".xls",
                        ".csv")
            ),
            uiOutput(outputId = "ui"),
            uiOutput(outputId = "uiods"),
            hr(),
            conditionalPanel("output.fileUploaded",
                             div(id="form",
                                 selectInput("exp", "Which study does this scale belong to?", choices = character(0)),
                                 numericInput("num.b", "How many blocks (or higher hierarchy scale) does it have?",value=6,max=20),
                                 sliderInput("escala", "Columns containing scales data (block, plot, etc.):", 
                                             min=1, value=c(1,2),max=20, ticks = FALSE),
                                 uiOutput(outputId = "out"),
                                 actionButton("submit", "Submit", class = "btn-primary"),
                                 shinyjs::hidden(
                                   span(id = "submit_msg", "Submitting..."),
                                   div(id = "error",
                                       div(br(), "Error: ", span(id = "error_msg"))
                                   )
                                 )
                             )
            ),
            shinyjs::hidden(
              div(
                id = "thankyou_msg",
                h3("Thank you, your submission was completed successfully"),
                actionLink("submit_another", "Add another scale", onclick ="location.href='../alta_escala/';")
              )
            )  
          ),
          mainPanel(
            tags$style(type='text/css', '#text1 {color: red;}'),
            h4(textOutput("text1")),
            bsCollapse(id = "collapseExample", open = "Warning",
                       bsCollapsePanel("Warning", h3("If there are nested scales in this study, it is essential 
                                                     to sort columns (left to right) from greater to smaller scale 
                                                     (i.e. always following this order: Block, Plot, SubPlot, SubSubPlot)"),
                       DT::dataTableOutput("contents"), style = "danger")
                       )
          )
        )
      )
    )
  )
)