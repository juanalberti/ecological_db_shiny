library(shiny)
library(RMySQL)
library(DT)
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
        title = "New variable and/or data type",
        sidebarLayout(
          sidebarPanel(
            div(id="form",
                checkboxInput("var", "Is it necessary to define a new variable type (i.e. something that groups variables; e.g. Biomass, Cover, Count)?", FALSE),
                conditionalPanel(condition="input.var==1",
                                 textInput("nombre", "Variable type's name (nombre_tipo_valor)"),
                                 checkboxGroupInput("vpd", "for which data types could be used? If not listed, please add it first.", 
                                                    choices=character(0)),
                textInput("unidad", "Measurement unit"),
                numericInput("decimales", "Number of decimals to store",value=4,min=0, max=8,step=1),
                checkboxInput("mostrar", "Show in the data visualization menu", value = FALSE)),
                hr(),
                checkboxInput("dat", "Is it necessary to define a new data type (i.e. something that groups data)?", FALSE),
                conditionalPanel(condition="input.dat==1",
                                 textInput("ntd", "Data type's name (nombre_tipo_dato)")),
                actionButton("submit", "Submit", class = "btn-primary"),
                shinyjs::hidden(
                  span(id = "submit_msg", "Submitting..."),
                  div(id = "error",
                      div(br(), "Error: ", span(id = "error_msg"))
                  )
                )),
            shinyjs::hidden(
              div(
                id = "thankyou_msg",
                h3("Thank you, your submission was completed successfully"),
                actionLink("submit_another", "Load another type of variable")#, onclick ="location.href='../alta_tvalor/';")
              )
            ) 
          ),
          mainPanel(uiOutput("conditionalBox.var"),uiOutput("conditionalBox.dat")
          )
        )
      )
    )
  )
)