library(shiny)
library(RMySQL)
library(shinyBS)
library(dplyr)
library(DT)

source("../utils.R")
source("../connection.R")

# fileds needed to enable submit button
fieldsAll <- c("exp1", "exp2", "nombre1", "nombre2")

ui = dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    menu
  ),
  dashboardBody(
    fluidPage(
      shinyjs::useShinyjs(),
      div(id="form",
          sidebarPanel(selectInput("exp1", "First experiment:", trae_nombre_experimento()$nombre_experimento),
                       radioButtons("esc1", "First scale type:", trae_nombre_tipo_escala() %>% pull(nombre)),
                       selectInput("nombre1", "First scale name:", ""),
                       selectInput("exp2", "Second experiment:", trae_nombre_experimento()$nombre_experimento),
                       radioButtons("esc2", "Second scale type:", trae_nombre_tipo_escala() %>% pull(nombre)),
                       selectInput("nombre2", "Sescond scale name:", ""),
                       actionButton("submit", "Submit")),
          mainPanel(
            bsCollapse(id = "collapseExample", open = "Registered duplicates",
                               bsCollapsePanel("Registered duplicates",
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
          h3("Thank you, duplicated measurements uploaded successfully"),
          actionLink("submit_another", "Add another duplication")#, onclick ="location.href='../borrar/';")
        )
      )
    )
  )
)

server = function(session, input, output) {
  
  
  # # main table
  observeEvent(list(input$exp1, input$exp2),{
    req(input$exp1)
    req(input$exp2)
    output$mytable0 <- renderDT({
      trae_escala()
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

  observeEvent(input$exp1, {
    req(input$exp1)
    updateRadioButtons(session,
                       "esc1",
                       choices = trae_duplicados(trae_id_experimento(input$exp1)) %>%
                         pull(obs_escala) %>%
                         unique())
  })

  observeEvent(list(input$exp1, input$esc1), {
    req(input$exp1)
    req(input$esc1)
    updateSelectInput(session,
                       "nombre1",
                       choices = trae_duplicados(trae_id_experimento(input$exp1)) %>%
                        filter(obs_escala == input$esc1) %>%
                         pull(nombre_escala))
  })

  observeEvent(input$exp1, {
    req(input$exp1)
    updateSelectInput(session,
                       "exp2",
                       choices = trae_nombre_experimento() %>%
                         filter(nombre_experimento != input$exp1) %>%
                         pull(nombre_experimento))
  })

  observeEvent(input$exp2, {
    req(input$exp2)
    updateRadioButtons(session,
                       "esc2",
                       choices = trae_duplicados(trae_id_experimento(input$exp2)) %>%
                         pull(obs_escala) %>%
                         unique())
  })

  observeEvent(list(input$exp2, input$esc2), {
    req(input$exp2)
    req(input$esc2)
    updateSelectInput(session,
                      "nombre2",
                      choices = trae_duplicados(trae_id_experimento(input$exp2)) %>%
                        filter(obs_escala == input$esc2) %>%
                        pull(nombre_escala))
  })
  
  
  observeEvent(input$submit, {
    # connect to database
    db<-conecta()
    tryCatch({
      # starts transaction
      dbBegin(db)
      # fetch first scale id
      idesc1 <- trae_id_escala(trae_id_experimento(input$exp1), input$esc1, input$nombre1, db)
      idesc2 <- trae_id_escala(trae_id_experimento(input$exp2), input$esc2, input$nombre2, db)
      #set new association
      alta_duplicado(idesc1, idesc2, db)
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