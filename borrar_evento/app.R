library(shiny)
library(RMySQL)
library(shinyBS)
library(DT)
library(tidyr)
library(dplyr)

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
      title = "Delete an insertion event",
      sidebarLayout(
        sidebarPanel(
          div(id="form",
              selectInput("user", label = "User's name:", choices = trae_usuarios()[,2]),
              selectInput("exp", label = "Study's name:", choices = character(0)),
              selectInput("insertion", label = "Insertion event (last 10)", choices = character(0)),
              actionButton("deleteins", "Delete")
              
          ),
          shinyjs::hidden(
            div(id = "thankyou_msg",
                h3("Insertion deleted successfully"),
                actionLink("submit_another", "Delete another insertion", onclick ="location.href='../borrar_evento/';")
            )
          )
        ),
        mainPanel(uiOutput("conditionalBox.var"),uiOutput("conditionalBox.dat")
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
    updateSelectInput(session, "exp", choices = ex_user$nombre_experimento)
  })
  
  # update the available insertions depending on the chosen study
  observeEvent(input$exp, {
    req(input$exp)
    ex_user<-trae_ex_x_user(input$user)
    ti<-sort(trae_insertion((ex_user%>%filter(nombre_experimento==input$exp))$id_experimento)$fecha_ins, decreasing = T)
    updateSelectInput(session, "insertion", choices = ti[if(length(ti)>10){1:10}else{1:length(ti)}])
  })
  
  # prepares data to show in the table about to be deleted
  tabla_ins<-reactive({
    req(input$insertion)
    para_tabla<-trae_all_insertion(input$insertion)
    if(nrow(para_tabla)>0){
      para_tabla%>%filter(is.na(valor_previo))%>%
        pivot_wider(values_from = nombre_escala,names_from=obs_escala)%>%
        pivot_wider(values_from = nombre_nivel,names_from=nombre_factor)%>%
        pivot_wider(names_from = nombre_dato, values_from=valor)} else{
          NULL
        }
  })
  
  # prepares the data to put in the table with the updated values about to be deleted
  tabla_upd<-reactive({
    req(input$insertion)
    otra_tabla<-trae_all_hubo_update(input$insertion)
    if(!is.na(trae_hubo_update(input$insertion)[1,1])){
      otra_tabla%>%
        pivot_wider(values_from = nombre_escala,names_from=obs_escala)%>%
        pivot_wider(values_from = nombre_nivel,names_from=nombre_factor)%>%
        pivot_wider(names_from = nombre_dato, values_from=valor)} else{
          NULL
        }
  })
  
  # creates the table with the updated data about to be deleted
  observeEvent(list(input$submit_another,
                    input$insertion),{
                      req(input$insertion)
                      if(!is.null(tabla_ins())){
                        output$inserted<-DT::renderDataTable({
                          DT::datatable(tabla_ins(),
                                        options = list(
                                          paging = T,
                                          searching = TRUE,
                                          scrollX=T,
                                          # scrollY="600px",
                                          scrollCollapse=T,
                                          fixedHeader=T,
                                          autoWidth = F,
                                          ordering = TRUE), rownames = F)
                        })
                      }
                      
                      # creates the table with the updated data about to be deleted
                      if(!is.null(tabla_upd())){
                        output$updated<-DT::renderDataTable({
                          DT::datatable(tabla_upd(),
                                        options = list(
                                          paging = T,
                                          searching = TRUE,
                                          scrollX=T,
                                          # scrollY="600px",
                                          scrollCollapse=T,
                                          fixedHeader=T,
                                          autoWidth = F,
                                          ordering = TRUE), rownames = F)
                        })
                      } else {output$updated<-NULL}
                    }, ignoreNULL = F, ignoreInit = F)
  
  # conditional output of the insertion's table
  output$conditionalBox.var <- renderUI({
    if(isset(input$insertion)){
      return(
        box(
          title = "Summary of the selected insertion", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, collapsed = is.null(tabla_ins()),width=12,
          DT::dataTableOutput("inserted")
        )
      )
    } else {
      return(
        NULL
      )
    }
  })
  
  # conditional output of the update's table
  output$conditionalBox.dat <- renderUI({
    if(isset(input$insertion)){
      return(
        box(
          title = "Summary of the updated values from the selected insertion", status = "primary", solidHeader = TRUE,
          collapsible = TRUE, collapsed = is.null(tabla_upd()),width=12,
          DT::dataTableOutput("updated")
        )
      )
    } else {
      return(
        NULL
      )
    }
  })
  
  # warning to prevent unwanted deletions
  observeEvent(input$deleteins, {
    showModal(modalDialog(
      tagList(h4("Are you sure? You are about to permanently delete the insertion "),
              h3(HTML(paste0("<b>",input$insertion,"</b>"))),
              h4(" and all its associated content.")
      ),
      title="Delete an insertion",
      footer = tagList(actionButton("confirmDelete", "Delete"),#, onclick ="location.href='../borrar/';"),
                       modalButton("Cancel")
      )
    ))
  })
  
  observeEvent(input$confirmDelete, {
    # brings the id of the selected insertion
    id_ins<-trae_id_insertion(input$insertion)[1,1]
    #connects to db
    db <- conecta()
    # check whether this insertion included updated values
    cons<-dbGetQuery(db,paste0("select id_insert from inserciones where hubo_update=1 and id_insert=",id_ins))
    tryCatch({
      # starts transaction
      dbBegin(db)
      req(input$insertion)
      # delete the data associated with the selected insertion
      baja_ins_reg(id_ins,db)
      # if the insertion included updated values...
      if(nrow(cons)>0){
        # brings the ids of the updated values in the selected insertion
        vals<-paste(trae_id_rv(id_ins,db)$id_registro_valor, collapse=",")
        # restores the previous value and breaks the association with this insertion
        restore_upd(vals,db)}
      baja_insertion(id_ins,db)
      # removes the warning
      removeModal()
      # finishes transaction
      dbCommit(db)
      # brings the studies registered by the selected user
      ex_user<-trae_ex_x_user(input$user)
      # brings all the insertions and sorts them from newest to oldest
      ti<-sort(trae_insertion((ex_user%>%filter(nombre_experimento==input$exp))$id_experimento)$fecha_ins, decreasing = T)
      # updates the insertion input with the last 10 insertions (max)
      updateSelectInput(session, "insertion", choices = ti[if(length(ti)>10){1:10}else{1:length(ti)}])
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      # if an error occurred, undo changes
      dbRollback(db)
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      stop(err$message)
    },
    finally = {
      dbDisconnect(db)
    })
  })
  
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })    
  
}


# Run the application 
shinyApp(ui = ui, server = server)