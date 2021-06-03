library(shiny)
library(RMySQL)
library(DT)
library(shinydashboard)

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
        title = "Add new system, scale level, factor or factor levels",
        sidebarLayout(
          sidebarPanel(
            div(id="form",
                checkboxInput("amb", "Is it necessary to define a new system?", FALSE),
                conditionalPanel(condition="input.amb==1",
                                 textInput("ambiente", "System")),
                checkboxInput("esc", "Do you need more scale levels?", FALSE),
                conditionalPanel(condition="input.esc==1",
                                 textInput("escala", "Scale type")),
                checkboxInput("fact", "Is it necessary to define a new factor or levels of a given factor?", FALSE),
                conditionalPanel(condition="input.fact==1",
                                 selectInput(inputId="factor", label="Factor", 
                                             choices=c(trae_factores()$nombre_factor,"new")),
                                 conditionalPanel(condition="input.factor=='new'",
                                                  textInput("n.fac", "New factor")),
                                 numericInput("num", "Number of new factor levels", 2, min = 1, max = 10, step = 1),
                                 uiOutput(outputId = "out")),
                actionButton("submit", "Submit", class = "btn-primary"),
                shinyjs::hidden(
                  span(id = "submit_msg", "Submitting"),
                  div(id = "error",
                      div(br(), "Error: ", span(id = "error_msg"))
                  )
                )),
            shinyjs::hidden(
              div(
                id = "thankyou_msg",
                h3("Data submitted succesfully"),
                actionLink("submit_another", "Add new system, factor or levels")#, onclick ="location.href='../alta_amb/';")
              )
            ) 
          ),
          mainPanel(div(id="boxes",
                        uiOutput("conditionalBox.amb"),
                        uiOutput("conditionalBox.esc"),
                        uiOutput("conditionalBox.fact"),
                        uiOutput("conditionalBox.niv"))
          )
        )
      )
    )
  )
)