library(readr)
library(dplyr)
library(shiny)
library(shinyBS)
library(RMySQL)
library(tidyr)
library(readr)
library(DT)
require(readxl)
require(readODS)

source("../utils.R")
source("../connection.R")

shinyServer(
  
  function(session,input, output) {

    # #save input file as inFile
    sheets_name <- observeEvent(input$file1, {
      # Determine document format;
      ptn <- "\\.[[:alnum:]]{1,5}$"
      suf <- tolower(regmatches(input$file1$name, regexpr(ptn, input$file1$name)))
      if (suf %in% c('.xls', '.xlsx')){
        sheets<-excel_sheets(input$file1$datapath)
        output$ui <- renderUI({
          selectInput(inputId = "sheet", label = "Excel sheet:", choices = sheets, selected = 1)
        })
      } else {
        if (suf %in% c('.ods')) {
          sheets<-list_ods_sheets(input$file1$datapath)
          output$uiods <- renderUI({
            selectInput(inputId = "sheetods", label = "Libre/OpenOffice sheet:", choices = sheets, selected = 1)
          })
        }
      }
    })
    inFile<- reactive({
      req(input$file1)
        ptn <- "\\.[[:alnum:]]{1,5}$"
        suf <- tolower(regmatches(input$file1$name, regexpr(ptn, input$file1$name)))
        if (suf %in% c('.xls', '.xlsx')) {
          re<-read_excel(input$file1$datapath, sheet=input$sheet)
          colu<-grep("POSIXct",sapply(re, class))
          return(re %>%
                   mutate_at(vars(all_of(colu)), list(as.character)))
        }else{
          if (suf %in% '.csv') {
            return(read.csv(input$file1$datapath, check.names = F,row.names = NULL))
          }else{
            if (suf %in% '.ods') {
              return(read_ods(input$file1$datapath, sheet = ifelse(is.null(input$sheetods),1,input$sheetods)))
            }
          }
        }
      })
    
    output$fileUploaded <- reactive({
      return(!is.null(input$file1))
    })
    
    output$fileUploaded <-reactive({
      return(!is.null(inFile()))
    })
    
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    
    Sys.sleep(2)    
    
    # table showing the contents of the uploaded file
    output$contents <- renderDataTable({
      DT::datatable(inFile(), colnames = paste(1:ncol(inFile()), colnames(inFile()), sep = " - "),
                    options = list(
                      paging = FALSE,
                      searching = TRUE,
                      scrollX=T,
                      scrollY="600px",
                      scrollCollapse=T,
                      fixedHeader=T,
                      autoWidth = F,
                      ordering = TRUE), rownames = F)
      
    })
    
    # dynamically generates as many fields to insert positions as blocks are in the design
    output$out <- renderUI({
      numinputs <- lapply(seq(length.out = req(input$num.b)), function(i){
        textInput(inputId = paste0("gps", i), label = paste0("Block latitude & longitude ", i, " separated by comma"))
      })
    })

        
    observe({
      pr<-inFile()
      if(isset(input$escala)){ # if scale's slider is filled, keep only those columns of the uploaded file
      datos<-data.frame(pr)[,as.numeric(input$escala[1]):as.numeric(input$escala[2])]
      if(input$escala[1]!=input$escala[2]){ # if more than one scale
        # creates a new column called separar that stores scales separated by a dot
        separar<-unique(unite(datos,separar,sep="."))
        # separates that column (don't ask me why, I don't remember)
        separado<-separate(separar,separar,colnames(datos),"\\.")
      } else {
        separado<-data.frame(x=NA)
      }
      # fetches scale's type
      tipo_escala<-trae_tipo_escala()
      
      
      output$text1 <- renderText({
      if(ncol(separado)>length(tipo_escala$nombre%in%separado)){ # if more columns than scale types...
        # warn the user to add scale type first
        return(paste0("need to add ",
                      ncol(separado)-sum((tipo_escala$nombre%in%colnames(separado)),na.rm=T), 
                      " scale type/s:\n ",
                      paste(colnames(separado)[!(colnames(separado) %in%tipo_escala$nombre)], collapse = ", ")
                      ))
        } else {return("")}
      })
      
      # check that all mandatory fields are filled and then enable the submit button
      mandatoryFilled <- all(ncol(separado)<length(tipo_escala$nombre%in%separado))
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
      } else {
        shinyjs::disable("submit")
      }
    })
    
    # update slider numeric and select inputs based on the uploaded file
    observe({
      pr<-inFile()
      val<-ncol(pr)
      # fetch scale types
      scale_type<-trae_tipo_escala()$nombre
      # Control the value, min, max, and step.
      # Step size is 2 when input value is even; 1 when value is odd.
      updateSliderInput(session, "escala", 
                        min = 1, max = val, value=c(min((1:ncol(pr))[colnames(pr) %in% scale_type]),
                                                    max((1:ncol(pr))[colnames(pr) %in% scale_type])
                        ))
      updateNumericInput(session, "num.b", value=length(levels(as.factor(pr$Block))))
      updateSelectInput(session,"exp",choices=trae_nombre_experimento()$nombre_experimento, selected = character(0))
    })
    
    
    formData<-reactive({
      # creates a dataframe with the uploaded file and then changes its names to allow spaces
      if(input$escala[1]!=input$escala[2]){
        datos<-data.frame(inFile())[,as.numeric(input$escala[1]):as.numeric(input$escala[2])]
      } else {
        datos<-data.frame(Block=inFile()[,input$escala[1]])
      }
      separar<-unique(unite(datos,separar,sep="."))
      separado<-separate(separar,separar,colnames(datos),"\\.")
      
      # fetch study's id
      id_ex<-trae_id_experimento(input$exp)$id_experimento
      # creates a list with positions
      gpss<-lapply(1:input$num.b, function(i) {
        input[[paste0("gps", i)]]
      })
      # turns this into a dataframe
      gps<-data.frame(x=unlist(gpss))
      # separates latitude from longitude values
      latlong<-separate(gps,x,c("lat","lon"),"\\s*,\\s*")
      # combine block's name and previous object
      comb<-cbind(unique(separado[,1]),latlong)
      # assign a new name to the first column
      colnames(comb)[1]<-"bq"
      
      # start counter
      i.viejo<-0
      # brings type of scale
      tipo_escala<-trae_tipo_escala()
      # keeps ids
      te<-tipo_escala$id_tipo_escala
      # connects to database
      db <- conecta()
      tryCatch({
        # starts transaction
        dbBegin(db)
        # if scale type needs to be added first, quit with warning
        if(ncol(separado)>length(tipo_escala$nombre%in%separado)){
          stop(paste0("need to add ",ncol(separado)-length(tipo_escala$nombre%in%separado), " sacle type/s"))
        }
        
        id_escala<-list()
        for(i in 1:nrow(separado)){ # for each row
          for(j in 1:ncol(separado)){ # and for each column of scales
            if(i==1){ # if the first row
              if(j==1){ # and the first scale
                # verifies if it exists in the db
                id_escala[[j]]<-verifica_escala2(id_ex,0,te[j],separado[i,j], db)
                # if not, inserts it
                if(nrow(id_escala[[j]])==0){id_escala[[j]]<-alta_esc(separado[i,j],colnames(datos)[j],0,
                                                                     subset(comb, bq==separado[i,1])$lat,
                                                                     subset(comb, bq==separado[i,1])$lon,
                                                                     te[j],id_ex,db)}
              } else { # for the remaining columns
                if(j%in%2:ncol(separado)){
                  # verifies their existence
                  id_escala[[j]]<-verifica_escala2(id_ex,id_escala[[j-1]],te[j],separado[i,j], db)
                  # and if not, inserts them
                  if(nrow(id_escala[[j]])==0){id_escala[[j]]<-alta_esc(separado[i,j],colnames(datos)[j],id_escala[[j-1]],"","",te[j],id_ex,db)}
                }
              }
            } else { # for the remaining rows
              if(j==1){ # if the first column
                if(separado[i,j]!=separado[i.viejo,j]){ # and the cell value is different from the one above
                  # verifies if it exists in the db
                  id_escala[[j]]<-verifica_escala2(id_ex,0,te[j],separado[i,j], db)
                  # if not, inserts it
                  if(nrow(id_escala[[j]])==0){id_escala[[j]]<-alta_esc(separado[i,j],colnames(datos)[j],0,
                                                                       subset(comb, bq==separado[i,1])$lat,
                                                                       subset(comb, bq==separado[i,1])$lon,
                                                                       te[j],id_ex,db)}
                }
              } else { # for the remaining columns
                  if(j%in%2:ncol(separado)){
                      # verifies scale existence
                      id_escala[[j]]<-verifica_escala2(id_ex,id_escala[[j-1]],te[j],separado[i,j], db)
                      # and if not, inserts it
                    if(nrow(id_escala[[j]])==0){id_escala[[j]]<-alta_esc(separado[i,j],colnames(datos)[j],id_escala[[j-1]],"","",te[j],id_ex,db)}
                }
              }
            }
          }
          # updates counter
          i.viejo<-i
        }
        
        # finishes transaction
        dbCommit(db)
      },
      error = function(err) {
        # if an error ocurred during the process, undo the changes
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
        shinyjs::hide("collapseExample")
        shinyjs::reset("collapseExample")
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