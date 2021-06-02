library(shiny)
library(RMySQL)
library(shinyBS)

source("../utils.R")
source("../connection.R")

ui = dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    menu
  ),
  dashboardBody(
    fluidPage(
      shinyjs::useShinyjs(),
      div(id="form",
          mainPanel(bsCollapse(id = "collapseExample", open = "About to delete",
                               bsCollapsePanel("About to delete",
                                               selectInput("user", label = "User's name:", choices = trae_usuarios()[,2]),
                                               selectInput("deleteexpname", label = "Study's name:", choices = character(0)),
                                               actionButton("deleteexp", "Delete"),
                                               shinyjs::hidden(
                                                 span(id = "submit_msg", "Deleting"),
                                                 div(id = "error",
                                                     div(br(), "Error: ", span(id = "error_msg"))
                                                 )
                                               ),
                                               style = "danger"))
          )
      ),
      shinyjs::hidden(
        div(
          id = "thankyou_msg",
          h3("Thank you, study deleted successfully"),
          actionLink("submit_another", "Delete another study")#, onclick ="location.href='../borrar/';")
        )
      )
    )
  )
)

server = function(session, input, output) {
  
  # update the list of studies depending on the chosen user
  observeEvent(input$user, {
    req(input$user)
    ex_user<-trae_ex_x_user(input$user)
    updateSelectInput(session, "deleteexpname", choices = ex_user$nombre_experimento)
  })
  
  # warning to prevent unwanted deletion of a given study
  observeEvent(input$deleteexp,{
    showModal(modalDialog(
      tagList(h4("Are you sure? You are about to permanently delete the study "),
              h3(HTML(paste0("<b>",input$deleteexpname,"</b>"))),
              h4(" and all its associated content.")
      ), 
      title="Delete a study",
      footer = tagList(actionButton("confirmDelete", "Delete"),
                       modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirmDelete, {
    # connect to database
    db<-conecta()
    tryCatch({
      # starts transaction
      dbBegin(db)
      # fetch study's id
      id_ex<-trae_id_experimento(input$deleteexpname)
      req(input$deleteexpname)
      # delete study from db
      baja_exp(id_ex,db)
      # remove the warning
      removeModal()
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
    # update list of remaining studies
    updateSelectInput(session, "deleteexpname", choices = trae_ex_x_user(input$user)$nombre_experimento)
  })
  observeEvent(input$confirmDelete, {
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
      shinyjs::enable("confirmDelete")
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