library(shiny)
require(RMySQL)
require(tidyr)
require(dplyr)
require(DT)
require(rjson)
require(dqshiny)
require(stringr)
require(readxl)
library(readr)

source("../utils.R")
source("../connection.R")

shinyServer(
  function(input, output, session) {
    
    sheets_name <- observeEvent(input$file1, {
      req(input$file1)
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
          sheets<-readODS::list_ods_sheets(input$file1$datapath)
          output$uiods <- renderUI({
            selectInput(inputId = "sheetods", label = "Libre/OpenOffice sheet:", choices = sheets, selected=1)
          })
        }
      }
    })
    
    inFiledep<- reactive({
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
              return(readODS::read_ods(input$file1$datapath, sheet = input$sheetods))
            }
          }
        }
      })
    
    output$fileUploaded <- reactive({
      return(!is.null(input$file1))
    })
    
    outputOptions(output, 'fileUploaded', suspendWhenHidden=F)
    
    # text to show if user forgot to add a variable type before this step
    output$text1 <- renderText({
      if(!nrow(trae_nombre_tipo_dato()[-(1:4),])>0){
        "Variable type needs to be added first"
      }})
    
    # steps needed to update the list of variables to add
    observeEvent(list(input$submit_another,input$submit,input$format,input$datos,input$name_long),{
      req(input$forma)
      req(input$format)
      if(isset(input$forma) && input$forma=="Manual"){
        # if a dependent variable information is being loaded manually...
        df<-inFiledep() # name the input file as df
        if(isset(input$format) && input$format=="Wide"){ # if variables are stored in the wide format 
          req(input$datos)
          # keep the column names of the dependent variables (according tu user input)
          revisar<-colnames(df)[as.numeric(input$datos[1]):as.numeric(input$datos[2])]
          # check which of those variables are already in the db
          hay<-trae_dato_filt(paste(sQuote(revisar, q="'"), collapse=","))
          # keep only those that are not found on the db
          queda<-revisar[!(revisar%in%hay$nombre_dato)]
          # if there are variables to add, update the slider to show those variables
          if(length(queda)>0){updateRadioButtons(session, "deps", choices = queda)}
          # otherwise make explicit that there is nothing to add
          else {updateRadioButtons(session, "deps", choices = "There are no variables to add")}
        } else { # if manually inserting variables in the long format
          if(isset(input$format) && input$format=="Long"){
            req(input$name_long)
            if(input$name_long>0){
              # keep unique names
              revisar<-unique(df[,input$name_long, drop=T])
              # keep only those dependent variables that are not found on the db
              queda<-revisar[!(revisar%in%trae_dato()$nombre_dato)]
              # if there are variables to add, update the slider to show those variables
              if(length(queda)>0){updateRadioButtons(session, "deps_long", choices = queda)}
              # otherwise make explicit that there is nothing to add
              else {updateRadioButtons(session, "deps_long", choices = "There are no variables to add")}
            } else {
              return(paste("Please select a valid column"))
            }
          }
        }
      }
    }
    )
    
    # de-select the type of variable once a new submission is intended
    observeEvent(input$submit_another,{updateRadioButtons(session, "tipo",selected = character(0))})
    
    # update text inputs for the name of the variable upon the selected variable
    observe({
      req(input$forma)
      req(input$format)
      if(input$forma=="Manual"){ 
        updateTextInput(session, "name", value=ifelse(input$format=="Wide",input$deps,input$deps_long))
        updateTextInput(session, "novivo", value=ifelse(input$format=="Wide",input$deps,input$deps_long))
      }
    })
    
    # update type of data
    observeEvent(list(input$forma,input$tipo),{
      # fetch data type
      td<-trae_nombre_tipo_dato()
      if (isset(input$forma)){
        if(input$forma=="Manual"){ # if manually filled 
          if(isset(input$tipo)){
            if (input$tipo=="With taxa"){ # and it is a species
              # update data type choices
              updateRadioButtons(session, "sitaxa", choiceNames = td$nombre_tipo_dato[1:4], 
                                 choiceValues = td$id_tipo_dato[1:4])
            } else {
              if(input$tipo=="Without taxa"){ # if it is not a species
                # update data type choices and warn if it was not previously added
                updateRadioButtons(session, "notaxa", choiceNames = if(length(td$nombre_tipo_dato[-(1:4)])>0){td$nombre_tipo_dato[-(1:4)]}else{"You should define a new data type first"},
                                   choiceValues = if(length(td$nombre_tipo_dato[-(1:4)])>0){td$id_tipo_dato[-(1:4)]} else{99999})
              }
            }
          } 
        }
      }
    },ignoreNULL = T, ignoreInit = T)
    
    # update the numeric input to not allow column number higher than the columns in the spreadsheet and to assume it it the column names 'taxa'
    observeEvent(input$format,{
      updateNumericInput(session, "name_long", min=0, max=ncol(inFiledep()), value=which(colnames(inFiledep())=="taxa"))
    })
    
    # update range of slider (if the wide format was selected)
    observeEvent(list(input$format, input$submit_another),{
      req(input$format)
      if(input$format=="Wide"){
        val<-ncol(inFiledep())
        updateSliderInput(session, "datos", min = 1, max = val, value = c(1,val))
      }
    })
    
    # fetch available data for species and use it for autocompletion
    observe({
      req(input$forma)
      if(input$forma=="Manual"){
        if(isset(input$sitaxa)){
          dato<-trae_dato()
          
          div<-unique(gsub('.*"division":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          if(length(div[-grep(":",div)])>0){
            div<-div[-grep(":",div)]
          }
          div<-div[!is.na(div)&div!=""]
          
          fam<-unique(gsub('.*"family":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          if(length(fam[-grep(":",fam)])>0){
            fam<-fam[-grep(":",fam)]
          }
          fam<-fam[!is.na(fam)&fam!=""]
          
          gen<-unique(gsub('.*"genus":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          if(length(gen[-grep(":",gen)])>0){
            gen<-gen[-grep(":",gen)]
          }
          gen<-gen[!is.na(gen)&gen!=""]
          
          spec<-unique(gsub('.*"species":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          if(length(spec[-grep(":",spec)])>0){
            spec<-spec[-grep(":",spec)]
          }
          spec<-spec[!is.na(spec)&spec!=""]
          
          fdevida<-unique(gsub('.*"lifeform":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          if(length(fdevida[-grep(":",fdevida)])>0){
            fdevida<-fdevida[-grep(":",fdevida)]
          }
          fdevida<-fdevida[!is.na(fdevida)&fdevida!=""]
          
          vida<-unique(gsub('.*"lifespan":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          if(length(vida[-grep(":",vida)])>0){
            vida<-vida[-grep(":",vida)]
          }
          vida<-vida[!is.na(vida)&vida!=""]
          
          proven<-unique(gsub('.*"provenance":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          if(length(proven[-grep(":",proven)])>0){
            proven<-proven[-grep(":",proven)]
          }
          proven<-proven[!is.na(proven)&proven!=""]
          
          nlocal<-unique(gsub('.*"local.name":"([[:alpha:]]*\\s?[[:alnum:]]*).*',"\\1",dato$json_mas_data))
          if(length(nlocal[-grep(":",nlocal)])>0){
            nlocal<-nlocal[-grep(":",nlocal)]
          }
          nlocal<-nlocal[!is.na(nlocal)&nlocal!=""]
          
          fgr<-unique(gsub('.*"functional_group":"([[:alpha:]]*\\s?[[:alnum:]]*).*',"\\1",dato$json_mas_data))
          if(length(fgr[-grep(":",fgr)])>0){
            fgr<-fgr[-grep(":",fgr)]
          }
          fgr<-fgr[!is.na(fgr)&fgr!=""]
          
          update_autocomplete_input(session, "div", options=div)
          update_autocomplete_input(session, "fam", options=fam)
          update_autocomplete_input(session, "gen", options=gen)
          update_autocomplete_input(session, "spec", options=spec)
          update_autocomplete_input(session, "fdevida", options=fdevida)
          update_autocomplete_input(session, "vida", options=vida)
          update_autocomplete_input(session, "proven", options=proven)
          update_autocomplete_input(session, "nlocal", options=nlocal)
          update_autocomplete_input(session, "gf", options=fgr)
        }
      }
    })
    
    # check that mandatory fields are filled and then enable submit button
    observe({
      if(isset(input$forma)){
        if(input$forma=="Manual"){
          mandatoryFilled <- all(
            isset(input$tipo),
            if(isset(input$tipo)){
              all(if(input$tipo=="Con taxa"){isset(input$sitaxa)},
                  if(input$tipo=="Sin taxa"){nrow(trae_nombre_tipo_dato()[-(1:4),])>0},
                  if(input$tipo=="Sin taxa"){!is.null(input$notaxa)}
              )
            }
          )
          shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        } else {
          if(input$forma=="With supporting taxa file") {
            shinyjs::toggleState(id = "submit")
          }
        }
      } else {
        shinyjs::disable("submit")
      }
    })
    
    # prepare table to show on the UI's main panel (top)
    output$info <- DT::renderDataTable({
      req(inFiledep())
      DT::datatable(inFiledep(), colnames = paste(1:ncol(inFiledep()), colnames(inFiledep()), sep = " - "),
                    options(scrollX=T), rownames = F)
    })
    
    # prepare table to show on the UI's main panel (bottom; i.e. variables missing)
    output$masivo<-DT::renderDataTable({
      req(inFiledep())
      DT::datatable(inFiledep()[!(inFiledep()$taxa%in%trae_dato()$nombre_dato),],
                    options(scrollX=T), rownames = F)
    })
    
    formData<-reactive({
      if(input$forma=="Manual"){ # if manually inserted
        guardar<-NULL
        if(input$tipo == 'With taxa'){ # species as dependent variables
          # creates a matrix with the values provided
          matriz<-tibble(Kingdom=input$sitaxa,division=input$div,family=input$fam,genus=input$gen,species=input$spec,
                         lifeform=input$fdevida,lifespan=input$vida,provenance=input$proven,local.name=input$nlocal,
                         functional_group=input$gf,site=input$mch)
          # creates a JSON variable
          if(!(all(!(nzchar(matriz))))){
            pr<-toJSON(matriz[nzchar(matriz)])
            pr2<-gsub(",","},{",pr)
            guardar<-paste0("[",pr2,"]")
          } else {guardar<-""}
          # insert dependent variable
          alta_data(input$name,
                    input$sitaxa,
                    guardar)
        }
        if(input$tipo == 'Without taxa'){ # if dependent variable not associated to species
          # create a new variable with the extra information provided
          if(nzchar(input$otros)){
            mejor<-paste0("[{",gsub("\\s*,\\s*","},{",input$otros),"}]")
            mejor2<-gsub('\\{([^:]+)\\s*:\\s*','\\{"\\1":',mejor)
            guardar<-gsub(':([^}]+)\\s*}\\s*',':"\\1"}',mejor2)
          }else {guardar<-""}
          # insert dependent variable
          alta_data(input$novivo,
                    input$notaxa,
                    guardar) 
        }
        
        # fetch dependent variable's names and ids already inserted
        dato<-trae_dato()[,1:2]
      } 
      else {
        if(input$forma=="With supporting taxa file"){ # if dependent variable's data is bulk uploaded 
          df0.tmp<-inFiledep()
          # keep only those species not registered
          df0<-df0.tmp[!(df0.tmp$taxa%in%trae_dato()$nombre_dato),]
          # for each of the remaining rows...
          for(fila in 1:nrow(df0)){
            # creates a matrix with the values provided
            matriz0<-tibble(Kingdom=df0$Kingdom[fila],division=df0$division[fila],family=df0$family[fila],genus=df0$genus[fila],species=df0$species[fila],
                            lifeform=df0$lifeform[fila],lifespan=df0$lifespan[fila],provenance=df0$provenance[fila],local.name=df0$local.name[fila],
                            functional_group=df0$functional_group[fila],site=df0$site[fila])
            # and a JSON variable
            if(!(all(!(nzchar(matriz0))))){
              pr0<-toJSON(matriz0[nzchar(matriz0)])
              pr20<-gsub(",","},{",pr0)
              guardar0<-paste0("[",pr20,"]")
            } else {guardar0<-""}
            # insert dependent variables
            alta_data(df0$taxa[fila],
                      subset(trae_nombre_tipo_dato(), nombre_tipo_dato==df0$Kingdom[fila])$id_tipo_dato,
                      guardar0)
          }
        }
      }
    })
    
    # from now on, show and hide different panels upon progress of submission and user's choices
    observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      tryCatch({
        formData()
        shinyjs::hide("lateral")
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
      shinyjs::show("lateral")
      shinyjs::show("form")
      shinyjs::show("collapseExample")
      shinyjs::hide("thankyou_msg")
    })    
  }
)
