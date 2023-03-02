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
    # tipo_dato is the way in which the dependent variables is being quantified
    output$text1 <- renderText({
      if(!nrow(trae_nombre_tipo_dato())>0){
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
          # keep the column names of the dependent variables (according to user input)
          revisar<-colnames(df)[as.numeric(input$datos[1]):as.numeric(input$datos[2])]
          # revisar <- "Crab burrows"
          # check which of those variables are already in the db
          hay<-trae_dato_filt(paste(sQuote(revisar, q = "'"), collapse=","))
          # hay <- data.frame(nombre_dato = "Crab burrows")
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
    })
    
    # de-select the type of variable once a new submission is intended
    observeEvent(input$submit_another,{updateRadioButtons(session, "tipo",selected = character(0))})
    
    # update text inputs for the name of the variable upon the selected variable
    observe({
      req(input$forma)
      req(input$format)
      if(input$forma=="Manual"){ 
        updateTextInput(session, "name", value=ifelse(input$format=="Wide",input$deps,input$deps_long))
        # updateTextInput(session, "novivo", value=ifelse(input$format=="Wide",input$deps,input$deps_long))
      }
    })
    
    # update type of data
    # observeEvent(list(input$forma,input$tipo),{
    #   # fetch data type (i.e. biomass, cover, etc)
    #   td<-trae_nombre_tipo_dato()
    #   if (isset(input$forma)){
    #     if(input$forma=="Manual"){ # if manually filled 
    #       if(isset(input$tipo)){
    #         # if (input$tipo=="With taxa"){ # and it is a species
    #         #   # update data type choices
    #         #   updateRadioButtons(session, "sitaxa", choiceNames = td$nombre_tipo_dato[1:4], 
    #         #                      choiceValues = td$id_tipo_dato[1:4])
    #         # } else {
    #           # if(input$tipo=="Without taxa"){ # if it is not a species
    #           #   # update data type choices and warn if it was not previously added
    #           #   updateRadioButtons(session, "notaxa", choiceNames = if(length(td$nombre_tipo_dato[-(1:4)])>0){td$nombre_tipo_dato[-(1:4)]}else{"You should define a new data type first"},
    #           #                      choiceValues = if(length(td$nombre_tipo_dato[-(1:4)])>0){td$id_tipo_dato[-(1:4)]} else{99999})
    #           # }
    #         # }
    #       } 
    #     }
    #   }
    # },ignoreNULL = T, ignoreInit = T)
    
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
          
          # div<-unique(gsub('.*"division":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          # if(length(div[-grep(":",div)])>0){
          #   div<-div[-grep(":",div)]
          # }
          # div<-div[!is.na(div)&div!=""]
          
          div <- trae_taxa_div()
          
          # fam<-unique(gsub('.*"family":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          # if(length(fam[-grep(":",fam)])>0){
          #   fam<-fam[-grep(":",fam)]
          # }
          # fam<-fam[!is.na(fam)&fam!=""]
          
          fam <- trae_taxa_fam()
          
          # gen<-unique(gsub('.*"genus":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          # if(length(gen[-grep(":",gen)])>0){
          #   gen<-gen[-grep(":",gen)]
          # }
          # gen<-gen[!is.na(gen)&gen!=""]
          
          gen <- trae_taxa_gen()
          
          # spec<-unique(gsub('.*"species":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          # if(length(spec[-grep(":",spec)])>0){
          #   spec<-spec[-grep(":",spec)]
          # }
          # spec<-spec[!is.na(spec)&spec!=""]
          
          spec <- trae_taxa_sp()
          
          # fdevida<-unique(gsub('.*"lifeform":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          # if(length(fdevida[-grep(":",fdevida)])>0){
          #   fdevida<-fdevida[-grep(":",fdevida)]
          # }
          # fdevida<-fdevida[!is.na(fdevida)&fdevida!=""]
          
          fdevida <- trae_taxa_lifeform()
          
          # vida<-unique(gsub('.*"lifespan":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          # if(length(vida[-grep(":",vida)])>0){
          #   vida<-vida[-grep(":",vida)]
          # }
          # vida<-vida[!is.na(vida)&vida!=""]
          
          vida <- trae_taxa_lifexp()
          
          # proven<-unique(gsub('.*"provenance":"([[:alpha:]]*).*',"\\1",dato$json_mas_data))
          # if(length(proven[-grep(":",proven)])>0){
          #   proven<-proven[-grep(":",proven)]
          # }
          # proven<-proven[!is.na(proven)&proven!=""]
          
          proven <- trae_taxa_origen()
          
          # nlocal<-unique(gsub('.*"local.name":"([[:alpha:]]*\\s?[[:alnum:]]*).*',"\\1",dato$json_mas_data))
          # if(length(nlocal[-grep(":",nlocal)])>0){
          #   nlocal<-nlocal[-grep(":",nlocal)]
          # }
          # nlocal<-nlocal[!is.na(nlocal)&nlocal!=""]
          
          # fgr<-unique(gsub('.*"functional_group":"([[:alpha:]]*\\s?[[:alnum:]]*).*',"\\1",dato$json_mas_data))
          # if(length(fgr[-grep(":",fgr)])>0){
          #   fgr<-fgr[-grep(":",fgr)]
          # }
          # fgr<-fgr[!is.na(fgr)&fgr!=""]
          
          fgr <- trae_taxa_functgrp()
          var <- trae_taxa_var()
          
          update_autocomplete_input(session, "div", options=div$division)
          update_autocomplete_input(session, "fam", options=fam$family)
          update_autocomplete_input(session, "gen", options=gen$genera)
          update_autocomplete_input(session, "spec", options=spec$species)
          update_autocomplete_input(session, "fdevida", options=fdevida$life_form)
          update_autocomplete_input(session, "vida", options=vida$life_exp)
          update_autocomplete_input(session, "proven", options=proven$provenance)
          # update_autocomplete_input(session, "nlocal", options=nlocal)
          update_autocomplete_input(session, "gf", options=fgr$functional_group)
          update_autocomplete_input(session, "var", options=var$variety)
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
    
    output$depvars <- DT::renderDataTable({
      DT::datatable(trae_dato()[,1:2] %>% arrange(nombre_dato),
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
        # create a new variable with the extra information provided
        if(nzchar(input$otros)){
          mejor <- paste0("[{", gsub("\\s*, \\s*", "},{", input$otros), "}]")
          mejor2 <- gsub('\\{([^:]+)\\s*:\\s*', '\\{"\\1":', mejor)
          guardar <- gsub(':([^}]+)\\s*}\\s*', ':"\\1"}', mejor2)
        } else {guardar <- ""}
        
        if(input$tipo == 'With taxa'){ # species as dependent variables
          # creates a matrix with the values provided
          # matriz<-tibble(Kingdom=input$sitaxa,division=input$div,family=input$fam,genus=input$gen,species=input$spec,
          #                lifeform=input$fdevida,lifespan=input$vida,provenance=input$proven,
          #                # local.name=input$nlocal,
          #                functional_group=input$gf,site=input$mch)
          # # creates a JSON variable
          # if(!(all(!(nzchar(matriz))))){
          #   pr<-toJSON(matriz[nzchar(matriz)])
          #   pr2<-gsub(",","},{",pr)
          #   guardar<-paste0("[",pr2,"]")
          # } else {guardar<-""}
          # insert dependent variable
          id_king <- verifica_taxa_king(input$sitaxa) %>% pull(id_king)
          id_div <- verifica_taxa_div(input$div) %>% pull(id_div)
          id_fam <- verifica_taxa_fam(input$fam) %>% pull(id_fam)
          id_gen <- verifica_taxa_gen(input$gen) %>% pull(id_gen)
          id_sp <- verifica_taxa_sp(input$spec) %>% pull(id_sp)
          id_var <- verifica_taxa_var(input$var) %>% pull(id_var)
          id_lifexp <- verifica_taxa_lifexp(input$vida) %>% pull(id_lifexp)
          id_lifeform <- verifica_taxa_lifeform(input$fdevida) %>% pull(id_lifeform)
          id_functgrp <- verifica_taxa_functgrp(input$gf) %>% pull(id_functgrp)
          id_origen <- verifica_taxa_origen(input$proven) %>% pull(id_origen)
          alta_data_taxa(input$name,
                         guardar,
                         1,
                         ifelse(length(id_king) == 0, alta_taxa_king(input$sitaxa), id_king),
                         ifelse(length(id_div) == 0, alta_taxa_div(input$div), id_div),
                         ifelse(length(id_fam) == 0, alta_taxa_fam(input$fam), id_fam),
                         ifelse(length(id_gen) == 0, alta_taxa_gen(input$gen), id_gen),
                         ifelse(length(id_sp) == 0, alta_taxa_sp(input$spec), id_sp),
                         ifelse(length(id_var) == 0, alta_taxa_var(input$var), id_var),
                         ifelse(length(id_lifexp) == 0, alta_taxa_lifexp(input$vida), id_lifexp),
                         ifelse(length(id_lifeform) == 0, alta_taxa_lifeform(input$fdevida), id_lifeform),
                         ifelse(length(id_functgrp) == 0, alta_taxa_functgrp(input$gf), id_functgrp),
                         ifelse(length(id_origen) == 0, alta_taxa_origen(input$proven), id_origen))
        }
        if(input$tipo == 'Without taxa'){ # if dependent variable not associated to species
          # insert dependent variable
          alta_data_notaxa(input$name,
                    # input$notaxa,
                    guardar,
                    0) 
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
            # # creates a matrix with the values provided
            # matriz0<-tibble(Kingdom=df0$Kingdom[fila],division=df0$division[fila],family=df0$family[fila],genus=df0$genus[fila],species=df0$species[fila],
            #                 variety=df0$variety[fila],
            #                 lifeform=df0$lifeform[fila],lifespan=df0$lifespan[fila],provenance=df0$provenance[fila],local.name=df0$local.name[fila],
            #                 functional_group=df0$functional_group[fila],site=df0$site[fila])
            # # and a JSON variable
            # if(!(all(!(nzchar(matriz0))))){
            #   pr0<-toJSON(matriz0[nzchar(matriz0)])
            #   pr20<-gsub(",","},{",pr0)
            #   guardar0<-paste0("[",pr20,"]")
            # } else {guardar0<-""}
            guardar0<-NULL
            # create a new variable with the extra information provided
            if(nzchar(input$otros)){
              mejor0 <- paste0("[{", gsub("\\s*, \\s*", "},{", input$otros), "}]")
              mejor20 <- gsub('\\{([^:]+)\\s*:\\s*', '\\{"\\1":', mejor)
              guardar0 <- gsub(':([^}]+)\\s*}\\s*', ':"\\1"}', mejor2)
            } else {guardar0 <- ""}
            
            # insert dependent variables
            id_king <- verifica_taxa_king(df0$Kingdom[fila]) %>% pull(id_king)
            id_div <- verifica_taxa_div(df0$division[fila]) %>% pull(id_div)
            id_fam <- verifica_taxa_fam(df0$family[fila]) %>% pull(id_fam)
            id_gen <- verifica_taxa_gen(df0$genus[fila]) %>% pull(id_gen)
            id_sp <- verifica_taxa_sp(df0$species[fila]) %>% pull(id_sp)
            id_var <- verifica_taxa_var(df0$variety[fila]) %>% pull(id_var)
            id_lifexp <- verifica_taxa_lifexp(df0$lifespan[fila]) %>% pull(id_lifexp)
            id_lifeform <- verifica_taxa_lifeform(df0$lifeform[fila]) %>% pull(id_lifeform)
            id_functgrp <- verifica_taxa_functgrp(df0$functional_group[fila]) %>% pull(id_functgrp)
            id_origen <- verifica_taxa_origen(df0$provenance[fila]) %>% pull(id_origen)
            alta_data_taxa(df0$taxa[fila],
                           # subset(trae_nombre_tipo_dato(), nombre_tipo_dato==df0$Kingdom[fila])$id_tipo_dato,
                           guardar0,
                           1,
                           ifelse(is.na(df0$Kingdom[fila]), "", ifelse(length(id_king) == 0, alta_taxa_king(df0$Kingdom[fila]), id_king)),
                           ifelse(is.na(df0$division[fila]), "", ifelse(length(id_div) == 0, alta_taxa_div(df0$division[fila]), id_div)),
                           ifelse(is.na(df0$family[fila]), "", ifelse(length(id_fam) == 0, alta_taxa_fam(df0$family[fila]), id_fam)),
                           ifelse(is.na(df0$genus[fila]), "", ifelse(length(id_gen) == 0, alta_taxa_gen(df0$genus[fila]), id_gen)),
                           ifelse(is.na(df0$species[fila]), "", ifelse(length(id_sp) == 0, alta_taxa_sp(df0$species[fila]), id_sp)),
                           ifelse(is.na(df0$variety[fila]), "", ifelse(length(id_var) == 0, alta_taxa_var(df0$variety[fila]), id_var)),
                           ifelse(is.na(df0$lifespan[fila]), "", ifelse(length(id_lifexp) == 0, alta_taxa_lifexp(df0$lifespan[fila]), id_lifexp)),
                           ifelse(is.na(df0$lifeform[fila]), "", ifelse(length(id_lifeform) == 0, alta_taxa_lifeform(df0$lifeform[fila]), id_lifeform)),
                           ifelse(is.na(df0$functional_group[fila]), "", ifelse(length(id_functgrp) == 0, alta_taxa_functgrp(df0$functional_group[fila]), id_functgrp)),
                           ifelse(is.na(df0$provenance[fila]), "", ifelse(length(id_origen) == 0, alta_taxa_origen(df0$provenance[fila]), id_origen)))
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
