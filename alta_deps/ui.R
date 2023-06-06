library(shinydashboard)
library(shiny)
library(shinyBS)
library(dqshiny)

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
        title = "Add dependent variables",
        sidebarLayout(
          sidebarPanel(
            div(id="lateral",
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
                                 radioButtons("forma", 
                                              "Adding variables:", 
                                              choices=c("Manual", "With supporting taxa file"),selected = character(0))
                ),
                conditionalPanel("input.forma=='Manual'",
                                 radioButtons("format","Data format on the spreadsheet:",
                                              choiceNames = c("Wide (species names in columns)",
                                                              "Long (species names in column 'taxa')"),
                                              choiceValues = c("Wide","Long"),selected = character(0)),
                                 conditionalPanel("input.format=='Wide'",
                                                  sliderInput("datos", "Columns that contain data of the dependent variable:", 
                                                              min=1, value=c(1,2),max=2,step = 1, ticks = FALSE),
                                                  radioButtons("deps", 
                                                               "Variables to add", 
                                                               choices="There are still no variables registered"
                                                  )),
                                 conditionalPanel("input.format=='Long'",
                                                  numericInput("name_long", "Column with variable names:", value=0),
                                                  radioButtons("deps_long", 
                                                               "Variable to add", 
                                                               choices="No variables to add"
                                                  ))
                )  
            ),
            shinyjs::hidden(
              div(
                id = "thankyou_msg",
                h3("Thank you, your submission was completed successfully"),
                actionLink("submit_another", "Add another dependent variable")#, onclick ="location.href='../alta_deps/';")
              )
            )
          ), # sidebarpanel
          mainPanel(
            bsCollapse(id = "collapseExample", open = c("Information","To fill"),multiple=T,
                       bsCollapsePanel("List of dependent variables", 
                                       DT::dataTableOutput("depvars"), 
                                       style = "primary"),
                       bsCollapsePanel("Information", 
                                       DT::dataTableOutput("info"), 
                                       style = "warning"),
                       bsCollapsePanel("To fill",
                                       div(id="form",
                                           conditionalPanel("input.forma=='Manual'",
                                                            radioButtons("tipo", 
                                                                         "Type of dependent variable", 
                                                                         choices=c("With taxa", "Without taxa"),selected = character(0)),
                                                            textInput("name", "Name"),
                                                            conditionalPanel("input.tipo == 'With taxa'",
                                                                             radioButtons("sitaxa", "Type of living being", choices=trae_taxa_king()$kingdom, selected = character(0)),
                                                                                          autocomplete_input("div", "Division/Phylum", options=NA),
                                                                                          autocomplete_input("fam", "Family", options=NA),
                                                                                          autocomplete_input("gen", "Genus", options=NA),
                                                                                          autocomplete_input("spec", "Species", options=NA),
                                                                                          autocomplete_input("var", "Variety", options=NA),
                                                                                          autocomplete_input("fdevida", "Life form", options=NA),
                                                                                          autocomplete_input("vida", "Life span", options=NA),
                                                                                          autocomplete_input("proven", "Provenance", options=NA),
                                                                                          # autocomplete_input("nlocal", "Local name", options=NA),
                                                                                          autocomplete_input("gf", "Functional group", options=NA)
                                                            ),
                                                            conditionalPanel("input.tipo == 'Without taxa'",
                                                                             # radioButtons("notaxa", "Type of variable", choices=character(0),selected = character(0)),
                                                                             tags$style(type='text/css', '#text1 {color: red;}'), 
                                                                             h4(textOutput("text1"))
                                                                             # textInput("novivo", "Variable's name"),
                                                                             
                                                            ),
                                                            textInput("otros", "Other data with the following structure:\n
                                                                                       Variable_name:value,Other_variable_name:other_value")
                                           ) # conditionalpanel manual
                                       ), # div form
                                       conditionalPanel("input.forma=='With supporting taxa file'",
                                                        DT::dataTableOutput("masivo"),
                                                        h4("If an empty table shows up, it is because all species are already registered"),
                                                        hr(),
                                                        h4("If the table shows correct information, you can proceed with the submission")
                                       ),
                                       actionButton("submit", "Submit", class = "btn-primary"),
                                       shinyjs::hidden(
                                         span(id = "submit_msg", "Submitting..."),
                                         div(id = "error",
                                             div(br(), "Error: ", span(id = "error_msg"))
                                         )
                                       ),
                                       style = "info") # bscollapse to fill
            ) # primer bscollapse
          ) # mainpanel
        ) # sidebarlayout
      ) # fluidpage  
    ) # dashboardbody
  ) # dashboardpage
) # shinyui

