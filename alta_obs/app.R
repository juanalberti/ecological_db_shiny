library(shiny)
library(RMySQL)
library(shinyBS)
library(dplyr)
library(DT)

source("../utils.R")
source("../connection.R")

# fileds needed to enable submit button
fieldsAll <- c("exp", "td", "obs")

ui = dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    menu
  ),
  dashboardBody(
    fluidPage(
      shinyjs::useShinyjs(),
      div(id="form",
          sidebarPanel(selectInput("exp", "Experiment:", trae_nombre_experimento()$nombre_experimento),
                       selectInput("td", "Data type:", trae_tipo_dato()$nombre_tipo_dato),
                       textInput("obs", "New observation"),
                       actionButton("submit", "Submit")),
          mainPanel(bsCollapse(id = "collapseExample", open = "Registered observations",
                               bsCollapsePanel("Registered observations",
                                               DTOutput("mytable0"),
                                               shinyjs::hidden(
                                                 span(id = "submit_msg", "Deleting"),
                                                 div(id = "error",
                                                     div(br(), "Error: ", span(id = "error_msg"))
                                                 )
                                               ),
                                               style = "primary"))
          )
      ),
      shinyjs::hidden(
        div(
          id = "thankyou_msg",
          h3("Thank you, new observation uploaded successfully"),
          actionLink("submit_another", "Add another observation")#, onclick ="location.href='../borrar/';")
        )
      )
    )
  )
)

server = function(session, input, output) {
  

  # main table
  observeEvent(list(input$exp, input$td),{
    req(input$exp)    
    req(input$td)
    output$mytable0 <- renderDT({
      trae_obs_td(input$exp, input$td)
    })
  })

  # check that all mandatory fields are filled and then enable submit button
  observe({
    mandatoryFilled <-vapply(fieldsAll,
                             function(x) {
                               !is.null(input[[x]]) && input[[x]] != ""
                             },
                             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  observeEvent(input$submit, {
    # connect to database
    db<-conecta()
    tryCatch({
      # starts transaction
      dbBegin(db)
      # fetch study's id
      id_ex<-trae_id_experimento(input$exp)$id_experimento
      req(input$exp)
      # bring data type id
      id_td<- trae_tipo_dato() %>% filter(nombre_tipo_dato == input$td) %>% pull(id_tipo_dato)
      #set new observation
      alta_obs_td(id_ex, id_td, input$obs, db)
      # finish transaction
      dbCommit(db)
    },
    error = function(err) {
      # if error during transaction, rollback
      dbRollback(db)
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      stop(err$message)
    },
    finally = {
      dbDisconnect(db)
    })
  })
  observeEvent(input$submit, {
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    tryCatch({
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)