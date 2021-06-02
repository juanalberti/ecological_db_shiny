library(shiny)
library(RMySQL)
library(DT)

source("../utils.R")
source("../connection.R")

shinyServer(
  function(input, output, session){
    
    
    observeEvent(input$submit_another,{
      # table showing the types of variables registered
      output$tipo_var<-DT::renderDataTable({
        DT::datatable(trae_tipo_variable(),
                      options = list(
                        paging = FALSE,
                        searching = TRUE,
                        scrollX=T,
                        scrollCollapse=T,
                        fixedHeader=T,
                        autoWidth = F,
                        ordering = TRUE), rownames = F)
      })
      
      # table showing the types of data registered
      output$tipo_dat<-DT::renderDataTable({
        DT::datatable(trae_nombre_tipo_dato(),
                      options = list(
                        paging = FALSE,
                        searching = TRUE,
                        scrollX=T,
                        scrollCollapse=T,
                        fixedHeader=T,
                        autoWidth = F,
                        ordering = TRUE), rownames = F)
      })
    }, ignoreNULL = F)
    
    # conditional output summarizing variable types
    output$conditionalBox.var <- renderUI({
      if(input$var == 1){
        return(
          box(
            title = "Summary of variable types", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, collapsed = ,width=12,
            DT::dataTableOutput("tipo_var")
          )
        )
      } else {
        return(
          NULL
        )
      }
    })
    
    # conditional output summarizing data types
    output$conditionalBox.dat <- renderUI({
      if(input$dat == 1){
        return(
          box(
            title = "Summary of data types", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, collapsed = ,width=12,
            DT::dataTableOutput("tipo_dat")
          )
        )
      } else {
        return(
          NULL
        )
      }
    })
    
    # update checkbox to associate a variable type with certain data types
    observeEvent(input$var,{
      td<-trae_nombre_tipo_dato()
      updateCheckboxGroupInput(session, "vpd", choiceNames = td$nombre_tipo_dato,choiceValues = td$id_tipo_dato)},
      ignoreInit = T)
    
    # check that mandatory fields are filled and then enable the submit button
    observe({
      if(input$var==1|input$dat==1){
        mandatoryFilled <- all(if(input$var==1){
          !is.null(input$nombre) && input$nombre != "" &&!is.null(input$vpd)},
          if(input$dat==1){
            !is.null(input$ntd) && input$ntd != ""}
        )
        shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
      }  else {
        shinyjs::disable("submit")
      }
    })
    
    
    formData<-reactive({
      if(input$var==1){
        # insert the requested variable type
        atv<-alta_tvalor(input$nombre, input$unidad, input$decimales,if(input$mostrar==T){1}else{0})
        for(idtv in input$vpd){
          # insert the requested association vetween variable type and data type
          alta_vpd(idtv,atv)}}
      # inert the requested data type
      if(input$dat==1){alta_tipo_dato(input$ntd)}
    })
    
    # from now on, show and hide different panels upon progress of submission and user's choices
    observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      tryCatch({
        formData()
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