library(shiny)
library(RMySQL)
library(shinydashboard)
library(dplyr)

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
        titlePanel("Add spreadsheet"),
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
                                 radioButtons("format","Data format on the spreadsheet:",
                                              choiceNames = c("Wide (species names in columns)",
                                                              "Long (species names in column 'taxa')"),
                                              choiceValues = c("wide","long"),selected = character(0)),
                                 conditionalPanel('input.format == "wide" || input.format == "long"',
                                                  radioButtons("replace","What should be done if values differ from those stored in the database?",
                                                               choices=c("Stop with warning","Replace previous value"), selected = "Stop with warning"),
                                                  selectInput("nombre", "Experiment PI:", 
                                                              choices=trae_nombre_usuario()$nombre_usuario),    
                                                  selectInput("experimento", "Study's name:", 
                                                              choices=character(0)),    
                                                  selectInput("ambiente", "System:", 
                                                              choices=character(0)),
                                                  selectInput("responsable", "Dataset responsible:", 
                                                              choices=c("", trae_nombre_usuario()$nombre_usuario)),    
                                                  numericInput("fecha", "Date column's number",0, min = 0),
                                                  numericInput("hora", "Time column's number",0),
                                                  checkboxInput("subregistros", "It has subregistries (subsamples)"),
                                                  conditionalPanel(condition="input.subregistros==1",
                                                                   numericInput("subr", "Column containing sample number",0),
                                                                   numericInput("fk_subr", "Column referring to sample number",0),
                                                                   numericInput("id_fk_subr", "Column referring to subsample number",0)),
                                                  sliderInput("factor", "Factor columns:", 
                                                              min=1, value=c(1,2),max=20, ticks = FALSE),
                                                  sliderInput("escala", "Scale columns (block, plot, etc.):", 
                                                              min=1, value=c(1,2),max=20, ticks = FALSE),
                                                  conditionalPanel('input.format == "wide"',
                                                  checkboxGroupInput("check", "Dependent variable type/s in the spreadsheet:", 
                                                                     choices=character(0)),
                                                  uiOutput(outputId = "out"),
                                                  uiOutput(outputId = "out_um"),
                                                  uiOutput(outputId = "out_obs")),
                                                  conditionalPanel('input.format == "long"',
                                                                   radioButtons("check_long", "Dependent variable type/s in the spreadsheet:",
                                                                                      choices=c(trae_tipo_dato()$nombre_tipo_dato,
                                                                                                if(length(trae_tipo_dato()$nombre_tipo_dato)==0){NA})),
                                                                   numericInput("name_long", "Column with names of dependent variables",0),
                                                                   numericInput("value_long", "Column with values of dependent variables",0),
                                                                   h4("Only one column with values in the long format can be considered at a time. Please specify others to omit, if there are.",style="color:red"),
                                                                   checkboxInput("others", "There are no other columns with data in the long format"),
                                                                   conditionalPanel('input.others == 0',
                                                                   sliderInput("col_omit", "Columns to be omitted",
                                                                               min=1, value=c(1,2),max=20, ticks = FALSE))),
                                 actionButton("submit", "Submit", class = "btn-primary")),
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
                h3("Thank you, data was submitted successfully"),
                h3(textOutput("inserted")),
                h3(textOutput("skipped")),
                h3(textOutput("ins_valor")),
                h3(textOutput("updated")),
                h3(textOutput("no_inserta")),
                actionLink("submit_another", "Submit more data", onclick ="location.href='../insertar/';")
              )
            )  
          ),
          mainPanel(
            DT::dataTableOutput("info")
          )
        )
      )
    )
  )
)
