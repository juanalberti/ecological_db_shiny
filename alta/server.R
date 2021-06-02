library(shiny)
library(RMySQL)
library(tidyr)
library(dplyr)
library(reshape2)
library(DT)

source("../utils.R")
source("../connection.R")

# fileds needed to enable submit button
fieldsAll <- c("exp", "tipoe", "ciudad", "fecha","escala","unidad",
               "nombre", "obs")

shinyServer(
  function(input, output, session){
    
    # table shown on the main panel with the information of each experiment available on the db
    observeEvent(input$submit_another,{
      output$exper<-DT::renderDataTable({
        DT::datatable(trae_exper(),
                      options = list(
                        paging = FALSE,
                        searching = F,
                        scrollX=T,
                        # scrollY="600px",
                        scrollCollapse=T,
                        fixedHeader=F,
                        autoWidth = TRUE,
                        ordering = F), rownames = F)
      })
    },ignoreNULL = F)
    
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
    
    # update select inputs once a new experiment is being registered
    observeEvent(input$submit_another,{
      updateSelectInput(session,"ciudad", choices = c(trae_nombre_ciudad()$ciudad_nombre,"new"))
      updateSelectInput(session,"nombre", choices = c(trae_nombre_usuario()$nombre_usuario,"new"))
    },ignoreNULL = F )
    
    formData<-reactive({
      # bring id corresponding to the type of study selected
      id_te<-input$tipoe
      # connect to db
      db <- conecta()
      tryCatch({
        dbBegin(db)
        # if a new city is desired, enable the field to introduce it's name
        if(input$ciudad=="new"){
          id_ci<-alta_ciu(nombre_ciudad=input$n.ciu,lat=input$n.lat, long=input$n.long, db)
        } else {
          # if not, bring the id of the selected city
          id_ci<-subset(trae_ciudad(), ciudad_nombre==input$ciudad)$id}
        # same with user's name
        ifelse(input$nombre=="new",
               id_us<-alta_usu(input$n.usu, db),
               id_us<-subset(trae_usuarios(), nombre_usuario==input$nombre)$id_usuario)
        # call the function to insert a new experiment
        alta_exp(input$exp,input$fecha,id_us, 
                 id_te,
                 input$obs, input$escala,input$unidad,
                 id_ci, db)
        # finish the transaction
        dbCommit(db)
      },
      error = function(err) {
        # if an error ocurred, undo changes
        dbRollback(db)
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
        stop(err$message)
      },
      finally = {
        dbDisconnect(db)
      })
    })
    
    # from now on, show and hide different panels upon progress of submission and user's choices
    observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      tryCatch({
        print(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::reset("collapseExample")
        shinyjs::hide("collapseExample")
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
      shinyjs::show("collapseExample")
      shinyjs::hide("thankyou_msg")
    })    
  }
)
