library(DT)
library(shiny)
library(RMySQL)
library(tidyr)
library(dplyr)
library(leaflet)

source("../utils.R")
source("../connection.R")

shinyServer(
  function(input, output, session) {
    
    
    observe({
      # bring variable's types to exhibit in the checkbox
      tv<-trae_id_tipo_valor()
      # update the checkbox
      updateCheckboxGroupInput(session, "show_vars0", choiceNames = tv$nombre_tipo_valor, choiceValues = tv$id_tipo_valor)
    })
    
    # reactive variable to enable study's checkbox once the first one is selected
    output$check_tv <- reactive({
      length(input$show_vars0)
    })
    outputOptions(output, 'check_tv', suspendWhenHidden=FALSE)    
    
    # update study's name checkbox upon selection of variable's type
    observeEvent(input$show_vars0,{
      req(input$show_vars0)
        extv<-trae_ex_x_tv(paste(input$show_vars0, collapse=","))
        updateCheckboxGroupInput(session, "show_vars1",
                                 choiceNames =extv$nombre_experimento, choiceValues = extv$id_experimento 
        )
    })
    
    # reactive variable to enable years slider once experiment is selected
    output$check_exp <- reactive({
      length(input$show_vars1)
    })
    outputOptions(output, 'check_exp', suspendWhenHidden=FALSE)    
    
    # update years slider upon experiment and variable's type
    observeEvent(input$show_vars1,{
      req(input$show_vars1)
      df<-trae_yr_x_idex(paste(input$show_vars1, collapse=","))
      updateSliderInput(session, "range", step=1,
                          min = na.omit(min(df$anio)), 
                          max = na.omit(max(df$anio)), 
                          value=c(min(na.omit(df$anio)),max(na.omit(df$anio))))        
    })
    
    # table to show on the info box
    output$info <- renderDataTable({
      if(length(input$show_vars1)>0){
        DT::datatable(trae_obs_x_idex(paste(input$show_vars1, collapse=",")))}
    })
    
    # reavtive event that stores the data to show on the main table 
    all<-eventReactive(list(input$show_vars1,input$range),{
      # inputs required
      req(input$show_vars0)
      req(input$show_vars1)
      # if years are selected...
      if(input$range[2]>1000){
        # bring data
        all1<-trae_todo(input$show_vars1,input$show_vars0,input$range[1]:input$range[2])
        # and put it on the wide format
        all2<-pivot_wider(all1,names_from="obs_escala", values_from = "nombre_escala")
        all3<-pivot_wider(all2, names_from = "nombre_factor",values_from = "nombre_nivel")
        all<-pivot_wider(all3, names_from = "nombre_dato",values_from = "valor")
        all$anio<-as.numeric(all$anio)
        for (i in 1:length(na.omit(unique(all1$nombre_factor)))){
          all[,unique(all1$nombre_factor)[i]]<-as.factor(data.frame(all[,unique(all1$nombre_factor)[i]])[,1])
        }
        return(all)}
    },ignoreInit = T)
    
    # summary table
    output$summary <- renderPrint({
      data.frame(Año=t(summary(all()$anio))[,c(1,6)])
    })
    
    # main table
    observeEvent(input$show_vars1,{
      req(input$show_vars1)    
      output$mytable0 <- renderDataTable({
          tf<-all()
          DT::datatable(
            tf[,!(colSums(is.na(tf))==nrow(tf))],
            extensions = 'Buttons',
            filter = 'top',
            options = list(
              paging = FALSE,
              searching = TRUE,
              scrollX=T,
              scrollY="600px",
              scrollCollapse=T,
              fixedHeader=T,
              autoWidth = TRUE,
              ordering = TRUE,
              dom = 'tB',
              buttons = c('csv', 'excel')
            ),
            class = "display"
          )          
          })
    })
    
    # second part of the summary tables (factors and levels)
    output$summary2 <- renderPrint({
      cn<-colnames(all())[colnames(all())%in% trae_nombre_factor()$nombre_factor]
      summary(all() %>% select(all_of(cn)))
    })
    
    # prepare position data to show on the map
    gps<-eventReactive(input$show_vars1,{
      gps1a<-trae_gps_x_idex(input$show_vars1)
      gps1<-gps1a[!is.na(gps1a$latitud),]
      gps2<-gps1[!duplicated(gps1[,4:5]),]
      gps2$latitud<-as.numeric(gsub("\\s","",gps2$latitud))
      gps2$longitud<-as.numeric(gsub("\\s","",gps2$longitud))
      gps2$nombre_experimento<-iconv(gps2$nombre_experimento,from = "latin1")
      return(gps2)
    })
    
    # map
    output$mymap1 <-renderLeaflet({
      leaflet() %>%
        addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G", attribution = 'Google')%>%
        addMarkers(data = gps(),
                   lat = ~ latitud, lng = ~ longitud,label = ~ nombre_escala)
    })
  }
)