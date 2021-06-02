library(shiny)
library(RMySQL)
library(DT)
library(dplyr)

source("../utils.R")
source("../connection.R")

shinyServer(
  function(input, output, session){
    
    # create a dynamic text input with as many fields as levels of a factor are needed to introduce in the db
    output$out <- renderUI({
      numinputs <- lapply(seq(length.out = req(input$num)), function(i){
        textInput(inputId = paste0("niv", i), label = paste0("New level ", i))
      })
    })
    
    # creates the table to show on the UI with the different environments already registered
    observeEvent(input$submit_another,{
      output$ambientes<-DT::renderDataTable({
        DT::datatable(trae_nombre_ambiente(),
                      options = list(
                        paging = FALSE,
                        searching = F,
                        scrollX=F,
                        scrollCollapse=T,
                        fixedHeader=F,
                        autoWidth = F,
                        ordering = F), rownames = F)
      })
      
      # creates the table to show on the UI with the different scale's types already registered
      output$escalas<-DT::renderDataTable({
        DT::datatable(trae_nombre_tipo_escala(),
                      options = list(
                        paging = FALSE,
                        searching = F,
                        scrollX=F,
                        scrollCollapse=T,
                        fixedHeader=F,
                        autoWidth = F,
                        ordering = F), rownames = F)
      })
      
      # creates the table to show on the UI with the different factors already registered
      output$factores<-DT::renderDataTable({
        DT::datatable(trae_nombre_factor(),
                      options = list(
                        paging = FALSE,
                        searching = T,
                        scrollX=F,
                        scrollCollapse=T,
                        fixedHeader=F,
                        autoWidth = F,
                        ordering = F), rownames = F)
      })
      
      # creates the table to show on the UI with the different levels for the factors already registered
      output$niveles<-DT::renderDataTable({
        mostrar<-select(left_join(trae_factores(), trae_niveles()),c(nombre_factor,nombre_nivel))
        DT::datatable(mostrar,
                      options = list(
                        paging = FALSE,
                        searching = T,
                        scrollX=F,
                        scrollCollapse=T,
                        fixedHeader=F,
                        autoWidth = F,
                        ordering = F), rownames = F)
      })
    },ignoreNULL = F)
    
    # UI box for systems
    output$conditionalBox.amb <- renderUI({
      if(input$amb == 1){
        return(
          box(
            title = "Summary of systems", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, collapsed = ,width=12,
            DT::dataTableOutput("ambientes")
          )
        )
      } else {
        return(
          NULL
        )
      }
    })
    
    # UI box for sale's types
    output$conditionalBox.esc <- renderUI({
      if(input$esc == 1){
        return(
          box(
            title = "Summary of scale types", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, collapsed = ,width=12,
            DT::dataTableOutput("escalas")
          )
        )
      } else {
        return(
          NULL
        )
      }
    })
    
    # UI box for factors
    output$conditionalBox.fact <- renderUI({
      if(input$fact == 1){
        return(
          box(
            title = "Summary of factors", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width=12,
            DT::dataTableOutput("factores")
          )
        )
      } else {
        return(
          NULL
        )
      }
    })
    
    # UI box for levels of those factors
    output$conditionalBox.niv <- renderUI({
      if(input$fact == 1){
        return(
          box(
            title = "Summary of levels", status = "primary", solidHeader = TRUE,
            collapsible = TRUE, width=12,
            DT::dataTableOutput("niveles")
          )
        )
      } else {
        return(
          NULL
        )
      }
    })
    
    observe({
      if(input$amb==1|input$fact==1|input$esc==1){
        if(input$fact==1){ # if a new factor needed, generate as many text inputs as levels required for that factor
          nivs<-lapply(1:input$num, function(i) {
            input[[paste0("niv", i)]]
          })
          df_niveles<-data.frame(niveles=unlist(nivs))
          if(nrow(df_niveles)>0){
            valid<-subset(df_niveles, niveles!="")} else{
              valid<-data.frame()
            }
        }
        
        # check that all mandatory fields are filled and then enable submit button
        mandatoryFilled <- all(
          if(input$amb==1){
            !is.null(input$ambiente) && input$ambiente != ""},
          if(input$esc==1){
            !is.null(input$escala) && input$escala != ""},
          if(input$fact==1){
            !is.null(input$factor) && input$factor != "" && nrow(valid) == input$num},
          if(input$fact==1 && input$factor=="new"){
            !is.null(input$n.fac) && input$n.fac != ""}
        )
        shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
      }  else {
        shinyjs::disable("submit")
      }
    })
    
    formData<-reactive({
      # prepare input of levels to be able to insert them into the db
      nivs<-lapply(1:input$num, function(i) {
        input[[paste0("niv", i)]]
      })
      df_niveles<-data.frame(niveles=unlist(nivs))
      
      db<-conecta()
      # if a new system is needed, insert it
      if(input$amb==1){
        alta_amb(input$ambiente, db)
      }
      
      # if a new factor is needed, insert it and preserve its id
      if(input$fact==1){
        if(input$factor=="new"){
          id_fac<-alta_fac(input$n.fac, db)
        }
        # and then also insert its levels
        for(i in 1:input$num){
          alta_niv(ifelse(input$factor=="new",
                          id_fac,
                          trae_id_factor(input$factor,db)$id_factor),
                   df_niveles[i,1], db)
        }
      }
      
      # if a new scale's type is needed, insert it
      if(input$esc==1){
        alta_tipo_esc(input$escala, db)
      }
    })
    
    # from now on, show and hide different panels upon progress of submission and user's choices
    observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      tryCatch({
        formData()
        shinyjs::reset("boxes")
        shinyjs::hide("boxes")
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
      shinyjs::show("boxes")
      shinyjs::hide("thankyou_msg")
    })    
  }
)