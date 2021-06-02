library(shiny)
library(RMySQL)
library(shinyBS)
library(DT)
library(dplyr)
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
        shinyjs::useShinyjs(),
        title = "New study",
        sidebarLayout(
          sidebarPanel(
            div(id="form",
                textInput("exp", "Study's name"),
                radioButtons("tipoe", "Study design", 
                             choiceNames = trae_tipoexp()$nombre_tipo_experimento, 
                             choiceValues = trae_tipoexp()$id_tipo_experimento,
                             selected = character(0)),
                textInput("fecha", "When did the study begin (YYYY-MM-DD)?"),
                selectInput("ciudad", "City, locality or site:", 
                            choices=character(0), selected = NULL),
                conditionalPanel(condition="input.ciudad=='new'",
                                 textInput("n.ciu", "New city, locality or site name"),
                                 textInput("n.lat", "Latitude"),
                                 textInput("n.long", "Longitude")),
                textInput("escala", "Scale at which dependent variables are measured (number only)"),
                textInput("unidad", "Unit of the preceding scale"),
                selectInput("nombre", "User's name:", 
                            choices=character(0),
                            selected=NULL),
                conditionalPanel(condition="input.nombre=='new'",
                                 textInput("n.usu", "New user")),
                textInput("obs", "Study details"),
                actionButton("submit", "Submit", class = "btn-primary"),
                shinyjs::hidden(
                  span(id = "submit_msg", "Submitting..."),
                  div(id = "error",
                      div(br(), "Error: ", span(id = "error_msg"))
                  )
                )
            ),
            shinyjs::hidden(
              div(
                id = "thankyou_msg",
                h3("Thank you, your submission was completed successfully"),
                actionLink("submit_another", "Add another study")#, onclick ="location.href='../alta/';")
              )
            ) 
          ),
          mainPanel(
            bsCollapse(id = "collapseExample", open = "Study",
                       bsCollapsePanel("Study", h4("List of loaded experiments"),
                                       DT::dataTableOutput("exper"), style = "info")
            )
          )
        )  
      )
    )
  )
)