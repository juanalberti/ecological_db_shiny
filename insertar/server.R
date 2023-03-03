library(shiny)
library(shinyBS)
library(RMySQL)
library(shiny)
library(dplyr)
library(readr)
library(DT)
library(schoolmath)
require(readxl)
require(readODS)
library(tidyr)


# complementary file with functions
source("../utils.R")
source("../connection.R")

shinyServer( #shinyServer
  function(session,input, output) { #shinyServer_funtion
    # save input file as inFile
    sheets_name <- observeEvent(input$file1, {
      # Determine document format;
      ptn <- "\\.[[:alnum:]]{1,5}$"
      suf <- tolower(regmatches(input$file1$name, regexpr(ptn, input$file1$name)))
      # if Excel file...
      if (suf %in% c('.xls', '.xlsx')){
        sheets<-excel_sheets(input$file1$datapath)
        output$ui <- renderUI({
          selectInput(inputId = "sheet", label = "Excel sheet:", choices = sheets, selected = 1)
        })
      } else {
        # if Libre/OpenOffice file...
        if (suf %in% c('.ods')) {
          sheets<-list_ods_sheets(input$file1$datapath)
          output$uiods <- renderUI({
            selectInput(inputId = "sheetods", label = "Libre/OpenOffice sheet:", choices = sheets, selected = 1)
          })
        }
      }
    })
    
    # read file
    inFile<- reactive({
      req(input$file1)
        ptn <- "\\.[[:alnum:]]{1,5}$"
        suf <- tolower(regmatches(input$file1$name, regexpr(ptn, input$file1$name)))
        if (suf %in% c('.xls', '.xlsx')) {
          re<-read_excel(input$file1$datapath, sheet=input$sheet)
          # colu<-grep("POSIXct",sapply(re, class))
          colu<-1:ncol(re)
          return(re %>%
                   mutate_at(vars(all_of(colu)), list(as.character)))
        }else{
          if (suf %in% '.csv') {
            return(read.csv(input$file1$datapath, check.names = F,row.names = NULL))
          }else{
            if (suf %in% '.ods') {
              return(read_ods(input$file1$datapath, sheet = input$sheetods))
            }
          }
        }
      })
    
    Sys.sleep(2)    
    output$fileUploaded <- reactive({
      return(!is.null(input$file1))
    })
    # once file is loaded, finish suspension
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    # show input file as a table
    output$info <- DT::renderDataTable({
      DT::datatable(inFile(),colnames = paste(1:ncol(inFile()), colnames(inFile()), sep = " - "),
                    options(scrollX=T), rownames = F)
      
    })
    
    # dynamically create one slider per type of dependent variable upon selection on the checkbox
    output$out <- renderUI({
      req(input$format)
      df<-inFile()
      val<-ncol(df)
      options(useFancyQuotes = FALSE)
      idntv<- trae_nombre_tipo_dato()
      slidinputs <- lapply(seq(length.out = req(length(input$check))), function(i){
        sliderInput(inputId = paste0("dat",i), label = paste0("Data range for ", 
                                                              idntv[which(idntv$id_tipo_dato==input$check[i]),"nombre_tipo_dato"]),
                    min=1,max=val,step=1, ticks = FALSE,
                    value=c(max(max((1:ncol(df))[colnames(df) %in% trae_nombre_tipo_escala()$nombre]),
                                max((1:ncol(df))[colnames(df) %in% trae_nombre_factor()$nombre_factor]),
                                input$fecha,input$hora, input$subr,input$fk_subr,input$id_fk_subr,na.rm = T)+1,val))
      })
    })
    
    # dynamically create one select input for measurement unit per type of dependent variable upon selection on the checkbox
    output$out_um <- renderUI({
      req(input$format)
      df<-inFile()
      val<-ncol(df)
      options(useFancyQuotes = FALSE)
      idntv<- trae_nombre_tipo_dato()
      selinputsum <- lapply(seq(length.out = req(length(input$check))), function(i){
        selectInput(inputId = paste0("um",i), label = paste0("Measurement unit for ", 
                                                             idntv[which(idntv$id_tipo_dato==input$check[i]),"nombre_tipo_dato"]),
                    choices = trae_tv()$unidad_medida)
      })
    })
    
    # dynamically create one description per type of dependent variable upon selection on the checkbox
    output$out_obs <- renderUI({
      req(input$format)
      req(input$experimento)
      req(input$dat1)
      df<-inFile()
      val<-ncol(df)
      options(useFancyQuotes = FALSE)
      idntv<- trae_nombre_tipo_dato()
      selinputsobs <- lapply(seq(length.out = req(length(input$check))), function(i){
        selectInput(inputId = paste0("obs",i), label = paste0("Description for variable type ", 
                                                             idntv[which(idntv$id_tipo_dato==input$check[i]),"nombre_tipo_dato"]),
                    choices = trae_obs_td(input$experimento, trae_tipo_dato() %>% 
                                            filter(id_tipo_dato == as.numeric(input$check[[i]])) %>% 
                                            pull(nombre_tipo_dato))$obs_tipo_dato)
      })
    })

    
    
        # disable the submit button until conditions are met
    observe({
      fieldsAll <- c("replace","format","nombre", "ambiente", "responsable", "experimento", if({req(input$format)
        input$format=="wide"}){"check"},
        if({req(input$format)
          input$format=="long"}){"check_long"},"fecha","hora",
        "factor", "escala",if(input$subregistros==1){c("subr","fk_subr","id_fk_subr")})
      # check if all mandatory fields have a value
      if(any(input$format=="wide" && 
             !is.null(input$check) && 
             length(input$check)>0 && 
             length(grep("dat[[0-9]]*",names(input))>0) &&
             length(grep("obs[[0-9]]*",names(input))>0) &&
             length(grep("um[[0-9]]*",names(input))>0), input$format=="long" && 
             !is.null(input$check_long))){
        if(input$format=="wide"){
          posic<-grep("dat[[0-9]]*",names(input))
          prueba<-NULL
          # loop to get a data frame with one row per slider
          for(i in posic){
            rows<-cbind(names(input)[i],input[[names(input)[i]]][1],input[[names(input)[i]]][2])
            prueba<-rbind(prueba,rows)
          }
          prueba<-data.frame(prueba)
          if(nrow(prueba)>=length(input$check)){
            prueba<-prueba[1:length(input$check),]}
          # change column names
          colnames(prueba)<-c("variable","menor","mayor")
          # convert min and max values to numeric
          prueba$menor<-as.numeric(as.character(prueba$menor))
          prueba$mayor<-as.numeric(as.character(prueba$mayor))
          # sort the dataframe of sliders according to their min values
          prueba<-prueba[order(prueba$menor),]
          # note that the input file should have all dependent variables together as the last columns
          # also note that by definition the same column can't include two types of dependent variables
          # then, there should be no overlap between sliders, and this is one of the conditions to be checked
          # check that there is no overlap between sliders
          verif<-NULL
          for(j in 2:nrow(prueba)){
            verif.tmp<-prueba[(j-1),3]<prueba[j,2]
            verif<-rbind(verif,verif.tmp)
          }
          # define the range of columns for dependent variables (regardless of their type)
          datos<-seq(min(prueba$menor),max(prueba$mayor))
        }
        # define all mandatory conditions before the submit button is unblocked
        mandatoryFilled <-
          vapply(fieldsAll,
                 function(x) {
                   !is.null(input[[x]]) && input[[x]] != ""
                 },
                 logical(1))
        # check that all conditions are met (i.e. TRUE)
        mandatoryFilled <- all(mandatoryFilled, # no empty inputs
                               input$fecha!=input$hora, # no overlap between date and time
                               input$fecha!=input$subr, # no overlap between date and parent id column
                               input$fecha!=input$fk_subr, # no overlap between date and parent id column in child rows
                               input$fecha!=input$id_fk_subr, # no overlap between date and child id column
                               # parent and child id columns should be 0, or larger than 0 and different from each other and from time
                               ((input$subr>0 & input$fk_subr>0 & input$id_fk_subr>0 & input$subr!=input$fk_subr & input$subr!=input$id_fk_subr &
                                   input$id_fk_subr!=input$fk_subr & input$subr!=input$hora & input$fk_subr!=input$hora & input$id_fk_subr!=input$hora)|
                                  (input$subr==0 & input$fk_subr==0 & input$id_fk_subr==0)),
                               # date not included in ranges defined in sliders
                               !(input$fecha%in%seq(as.numeric(input$factor)[1],as.numeric(input$factor)[2])),
                               !(input$fecha%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])),
                               # same for time
                               !(input$hora%in%seq(as.numeric(input$factor)[1],as.numeric(input$factor)[2])),
                               !(input$hora%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])),
                               # and for parent id columns
                               !(input$subr%in%seq(as.numeric(input$factor)[1],as.numeric(input$factor)[2])),
                               !(input$subr%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])),
                               !(input$fk_subr%in%seq(as.numeric(input$factor)[1],as.numeric(input$factor)[2])),
                               !(input$fk_subr%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])),
                               # no overlap between factor and scale sliders
                               !(as.numeric(input$factor)[1]%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])),
                               !(as.numeric(input$factor)[2]%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])),
                               # no overlap with other inputs and range of dependent variables
                               if(input$format=="wide"){
                                 all(is.na(all(verif))|all(verif), # no overlap between the dynamically generated sliders!(input$fecha%in%datos),
                                     !(input$hora%in%datos),
                                     !(input$subr%in%datos),
                                     !(input$fk_subr%in%datos),
                                     !(input$id_fk_subr%in%datos),
                                     !(min(datos)%in%seq(as.numeric(input$factor)[1],as.numeric(input$factor)[2])),
                                     !(max(datos)%in%seq(as.numeric(input$factor)[1],as.numeric(input$factor)[2])),
                                     !(min(datos)%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])),
                                     !(max(datos)%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])))
                               } else { 
                                 all(
                                 (input$name_long>0 && input$value_long>0 && input$name_long != input$value_long && !is.na(input$value_long)),
                                 (ifelse(input$fecha>0,1,0)+ifelse(input$hora>0,1,0)+ifelse(input$subr>0,1,0)+
                                    ifelse(input$fk_subr>0,1,0)+ifelse(input$id_fk_subr>0,1,0)+length(seq(input$factor[1],input$factor[2]))+
                                    length(seq(input$escala[1],input$escala[2]))+ifelse(input$name_long>0,1,0)+
                                    ifelse(is.na(input$value_long),0,ifelse(input$value_long>0,1,0))+
                                    ifelse(input$others == 1, 0, ifelse(input$value_long%in%seq(input$col_omit[1],input$col_omit[2]),
                                           length(seq(input$col_omit[1],input$col_omit[2]))-1,
                                           length(seq(input$col_omit[1],input$col_omit[2])))))==ncol(inFile()),
                                 !(input$name_long%in%seq(as.numeric(input$factor)[1],as.numeric(input$factor)[2])),
                                 !(input$name_long%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])),
                                 !(input$value_long%in%seq(as.numeric(input$factor)[1],as.numeric(input$factor)[2])),
                                 !(input$value_long%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])),
                                 if(input$others==0){all(
                                   !(min(unlist(input$col_omit))%in%seq(as.numeric(input$factor)[1],as.numeric(input$factor)[2])),
                                   !(max(unlist(input$col_omit))%in%seq(as.numeric(input$factor)[1],as.numeric(input$factor)[2])),
                                   !(min(unlist(input$col_omit))%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])),
                                   !(max(unlist(input$col_omit))%in%seq(as.numeric(input$escala)[1],as.numeric(input$escala)[2])),
                                   !(input$name_long%in%seq(as.numeric(input$col_omit)[1],as.numeric(input$col_omit)[2])))
                                 })
                               }
        )
        # enable/disable the submit button
        shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
      } else {shinyjs::disable("submit")
      }
    })
    
    # update input values guessing from the input file
    observeEvent(input$format,{
      req(input$format)
      df<-inFile()
      val<-ncol(df)
      # Control the value, min, max, and step.
      # Step size is 2 when input value is even; 1 when value is odd.
      updateNumericInput(session, "fecha",value=(1:ncol(df))[colnames(df) %in% c("Date", "date", "Fecha", "fecha")])
      updateNumericInput(session, "name_long",value=(1:ncol(df))[colnames(df) %in% c("Taxa", "taxa")])
      updateNumericInput(session, "value_long",value=(1:ncol(df))[colnames(df) %in% c("value", "Value")])
      if(!is.null(df$Time)){
        updateNumericInput(session, "hora",value=(1:ncol(df))[colnames(df) %in% c("Time","time")])
      }
      # updateSelectInput(session,"nombre", choices = trae_nombre_usuario()$nombre_usuario)
      # updateSelectInput(session,"responsable", choices = trae_nombre_usuario()$nombre_usuario)
      updateSelectInput(session,"ambiente", choices = trae_nombre_ambiente()$nombre_ambiente)
      updateNumericInput(session, "subr",value=ifelse(all(!(colnames(df)=="id_padre")),0,(1:ncol(df))[colnames(df)=="id_padre"]))
      updateNumericInput(session, "fk_subr",value=ifelse(all(!(colnames(df)=="fk_id_padre")),0,(1:ncol(df))[colnames(df)=="fk_id_padre"]))
      updateNumericInput(session, "id_fk_subr",value=ifelse(all(!(colnames(df)=="ref_hijo")),0,(1:ncol(df))[colnames(df)=="ref_hijo"]))
      factores<-trae_nombre_factor()$nombre_factor
      tipo_escala<-trae_nombre_tipo_escala()$nombre
      updateSliderInput(session, "factor",
                        min = 1, max = val, value=c(min((1:ncol(df))[colnames(df) %in% factores]),
                                                    max((1:ncol(df))[colnames(df) %in% factores])
                        ))
      updateSliderInput(session, "escala",
                        min = 1, max = val, value=c(min((1:ncol(df))[colnames(df) %in% tipo_escala]),
                                                    max((1:ncol(df))[colnames(df) %in% tipo_escala])
                        ))
      updateSliderInput(session, "col_omit",
                        min = 1, max = val, value=c(1+max(c(input$fecha, input$hora,input$factor[2], input$escala[2], 
                                                          input$subr, input$fk_subr, input$id_fk_subr, input$name_long)),
                                                    val
                        ))
      options(useFancyQuotes = FALSE)
      # bring value's type from db to use it as the response
      # idntv<- trae_tv(paste(sQuote(unlist(strsplit(colnames(df),split = ","))),collapse = ", "))
      updateCheckboxGroupInput(session,inputId="check",choiceNames = trae_nombre_tipo_dato()$nombre_tipo_dato, choiceValues = trae_nombre_tipo_dato()$id_tipo_dato)
    })
    
    # show only the experiments associated with the selected user
    observeEvent(input$nombre,{
      updateSelectInput(session, "experimento",choices=trae_ex_x_user(input$nombre)$nombre_experimento)
    })
    
    # process of validation and populaton
    formData<-reactive({ #formData
      # input table named df
      df<-inFile()
      # if data stored in the long format
      if(input$format=="long"){
        if(input$others==0){ # if there are other columns in the long format
          omitted<-seq(as.numeric(input$col_omit)[1],as.numeric(input$col_omit)[2])[!(seq(as.numeric(input$col_omit)[1],as.numeric(input$col_omit)[2])%in%input$value_long)] # variables not considered in this submission
          men<-ncol(df[,-omitted])-1 # number of columns before turning into wide format
          df<-spread(df[,-omitted],colnames(df)[input$name_long],colnames(df)[input$value_long]) # turn the long format into wide
          col_slider<-data.frame(menor=men, mayor=ncol(df)) # creates an object with the end points of te dependent varaibles' range 
        } else { # if there are no other columns in the long format (no need to remove columns from df)
          df<-spread(df,colnames(df)[input$name_long],colnames(df)[input$value_long])
          col_slider<-data.frame(menor=ncol(inFile())-1, mayor=ncol(df))
        }
      } # end process of validation and populaton
      
      # now, both long and wide input data are in the wide format
      # replace NA with "" to simplify insertions into the db
      df[is.na(df)]<-""

      # here we want to store the numbers of columns with dependent variables
      if(input$format=="wide"){ # if data was uploaded in the wide format, now needs some tweaking
        # same as described above. dataframe with min and max values per dynamically generated slider
        posic<-grep("dat[[0-9]]*",names(input))
        prueba<-NULL
        for(i in posic){ # for each of the sliders with dependent variables
          rows<-cbind(names(input)[i],input[[names(input)[i]]][1],input[[names(input)[i]]][2]) # extract column names
          prueba<-rbind(prueba,rows) # paste them together
        }
        prueba<-data.frame(prueba)
        prueba<-prueba[1:length(input$check),] 
        colnames(prueba)<-c("variable","menor","mayor") # rename columns
        prueba$menor<-as.numeric(as.character(prueba$menor)) # make columns in sliders numeric
        prueba$mayor<-as.numeric(as.character(prueba$mayor)) # make columns in sliders numeric
        prueba1<-prueba[order(prueba$menor),] # sort sliders with columns ascending (in case first slider did not contain the first dependent variables)
        datos<-seq(min(prueba1$menor),max(prueba1$mayor))
      } else { # no tweaking needed if data was uploaded in the long format
        datos<-col_slider$menor:col_slider$mayor
      }
      
      # loop to check if factors and their levels were previously registerd in the db
      # also to store the id of each scale for each row of the input file
      niv_id<-data.frame()
      niv.tmp<-list()
      for (fila2 in 1:nrow(df)){ # loop fila 2
        for (i in as.numeric(input$factor)[1]:as.numeric(input$factor)[2]){ # for each factor column
          fact.tmp<-verifica_factor(colnames(df)[i]) # check factor
          # if factor does not exist, exit with warning
          if(nrow(fact.tmp)==0){
            stop(paste("missing factor",colnames(df)[i], sep=" "))
          }
          # same thing for factor levels
          nivel.tmp<-verifica_nivel(fact.tmp, df[fila2,i]) # check level
          if(nrow(nivel.tmp)==0){
            stop(paste("missing level",df[fila2,i], sep=" "))
          }
          # list with factor and level ids; one list per factor column with two values
          niv.tmp[[i]]<-c(fact.tmp,nivel.tmp)
        }
        # one-row matrix with factors and levels ids
        pegado<-matrix(unname(unlist(niv.tmp)),ncol=((input$factor[2]-input$factor[1]+1)*2))
        # matrix storing the rows generated above
        niv_id<-rbind(niv_id,pegado)
      } # end of loop fila 2
      # loop to verify that all dependent variables exist in the db and to save their ids
      dat_id<-NULL
      for(k in datos){ # loop over columns 
        dat.tmp<-verifica_existencia_dato(colnames(df)[k]) # verify dependent variable
        # if dependent variable does not exist, exit with warning
        if(nrow(dat.tmp)==0){
          stop(paste("missing variable",colnames(df)[k], sep=" "))
        }
      }
      # bring experiments by user
      experimento<-trae_ex_x_user(input$nombre)
      # gather the ids of experiment, city, system and user based on selected inputs
      id_ex<-subset(experimento, nombre_experimento==input$experimento)$id_experimento
      id_ci<-subset(experimento, nombre_experimento==input$experimento)$fk_id_ciudad
      id_am<-trae_id_am(input$ambiente)$id_ambiente
      id_us<-trae_id_us(input$nombre)$id_usuario
      id_resp<-trae_id_us(input$responsable)$id_usuario

      # loop to verify scales and get their ids
      id_esc<-NULL
      te<-trae_tipo_escala()$id_tipo_escala # bring all scale ids
      for(fila in 1:nrow(df)){ # loop over all the rows of the input file loop#1
        id_escalas<-list()
        for(l in input$escala[1]:input$escala[2]){ # loop over scale columns. loop#2
          lista<-l-(input$escala[1]-1) 
          tipoe.tmp<-verifica_tipo_escala(colnames(df)[l]) # verify scale type
          if(nrow(tipoe.tmp)==0){ # if scale not found, quit with error
            stop(paste("scale",colnames(df)[l], "doesn't exist. Verify its name", sep=" "))
          }
          if(tipoe.tmp==min(te)){ # if the largest scale, check if its value exists and gather its id. if#1
            id_escalas[[lista]]<-verifica_escala(id_ex,0,tipoe.tmp,df[fila,l])
            # this is the reason why scales need always to be sorted from larger to smaller
            if(nrow(id_escalas[[lista]])==0){ # if not in db, stop with error
              stop(paste("missing scale",df[fila,l], "from",colnames(df)[l], sep=" "))
            } 
          } # if#1
          else { # if not the largest scale. else#1
            if(df[fila,l]!=""){ # if its value is not empty
              id_escalas[[lista]]<-verifica_escala(id_ex,id_escalas[[lista-1]],tipoe.tmp,df[fila,l])
            } else {
              if(df[fila,l]==""){ # if it is empty, assign NA
                id_escalas[[lista]]<-data.frame(id_escala=NA)}
            }
            # if the value doesn't exist, exit with warning
            if(nrow(id_escalas[[lista]])==0){
              stop(paste("missing scale",df[fila,l], "from",colnames(df)[l], sep=" "))
            }
          } # end of else#1
        } # end of loop#2
        # concatenate scale ids for a given row
        id_escala<-data.frame(id_escalas)
        # builds a matrix of scales ids instead of values
        id_esc<-rbind(id_esc,id_escala)
      } # end of loop#1
      # prepare scales, factors and levels for future queries
      id_scales_str<-apply(id_esc,1,paste0, collapse=",")
      id_factors_str<-apply(niv_id[c(TRUE,FALSE)],1,paste0,collapse=",")
      id_levels_str<-apply(niv_id[!c(TRUE,FALSE)],1,paste0,collapse=",")
      # connect to database
      db <- conecta()
      # function to specify what to do in case of error
      tryCatch(
        { # TryCatch expression. #trycatchexpr
          # start transaction (i.e. be able to roll back in case there was an error during the process)
          dbBegin(db)
          # set counters of insertions to 0
          ins_val<-0
          ins_val2<-0
          ins_reg<-0
          upd_val<-0
          
          # creat process number to delete insertions upon request if there were insertions
          ii<-inserta_inserc(id_us,id_ex,id_resp, db)[1,1]
          # loop over rows to verify if data was previously inserted or not, and if not, insert it
          for (filas in 1:nrow(df)){ # loop over all the rows of the input file loop#3_each_row
            if(input$subregistros==1 && input$fk_subr>0){ # enter if there are subregistries. if#3_there_are_subreg
              if(df[filas,input$fk_subr]!=""){ # enter if the given row is a subregistry. if#4_subregistry_row
                # gather data from the chosen experiment and corresponding date and time (if not empty)
                # that is why subregistries should always come after registries in the spreadsheet
                parentid.tmp<-dbGetQuery(db,paste0("select r.id_registro
                                                   from registro r 
                                                   where r.id_registro_padre=0 and r.fk_id_experimento=",
                                                   id_ex," and r.fecha_registro='",df[filas,input$fecha],"'",
                                                   " and r.id_heading='",paste(id_factors_str[filas],id_levels_str[filas],id_scales_str[filas],sep = "."),"'",
                                                   if(input$hora>0 && df[filas,input$hora]!=""){paste0(" and r.hora=",df[filas,input$hora])}))
                # save only registry id
                parentid<-unique(parentid.tmp$id_registro)
                # subset parent registry data from the input file
                row_reg_csv<-df[df[,input$subr, drop=T]==df[filas,input$fk_subr, drop=T],]
                # if more than two rows with same id in the csv, quit with warning
                if (nrow(row_reg_csv)>1){
                  stop(paste0("there are two or more registries with the same id: ",df[filas,input$fk_subr]))
                } else { #check_existence
                  # if no rows, quit with warning
                  if (nrow(row_reg_csv)==0){
                    stop(paste0("there are no registries associated with the subregistry ",df[filas,input$fk_subr]))
                  } else { # check that parent info on the child row and parent info on the respective row match #check_match
                    if(!all(df[filas, input$fecha] == row_reg_csv[,input$fecha],
                            if(input$hora>0){
                              df[filas, input$hora] == row_reg_csv[,input$hora]
                            },
                            df[filas, seq(as.numeric(input$escala)[1],
                                          as.numeric(input$escala)[2])] == row_reg_csv[,seq(as.numeric(input$escala)[1],
                                                                                            as.numeric(input$escala)[2])],
                            df[filas, seq(as.numeric(input$factor)[1],
                                          as.numeric(input$factor)[2])] == row_reg_csv[,seq(as.numeric(input$factor)[1],
                                                                                            as.numeric(input$factor)[2])])){
                      stop(paste0("factor, scale, date and/or time for subregistry ",filas," does not match with the parent registry"))
                    }
                  } # end of #check_match
                } # end #check_existence
              } # if#4_subregistry_row
            } # end of if#3_there_are_subreg
            # gather data from the chosen experiment and corresponding date, and time, parent id(if not empty)
            actualizar<-dbGetQuery(db,paste0("SELECT r.id_registro from registro r
                                             where r.fk_id_experimento=",id_ex," and r.fecha_registro='",df[filas,input$fecha],
                                             "' and r.id_heading='",paste(id_factors_str[filas],id_levels_str[filas],id_scales_str[filas],sep = "."),"'",
                                             if(input$hora>0 && df[filas,input$hora]!=""){paste0(" and r.hora=",df[filas,input$hora])},
                                             if(input$subregistros==1 && input$fk_subr>0 &&df[filas,input$fk_subr]!=""){
                                               paste0(" and r.ref_hijo='",df[filas,input$id_fk_subr],"'")},
                                             " and r.id_registro_padre=",ifelse(input$subregistros==1 && input$fk_subr>0 &&df[filas,input$fk_subr]!="",parentid,0)))
            
            # save only registry id
            idr<-unique(actualizar$id_registro)
            if(length(idr)>1){ # if more than one registry, exit
              stop(paste0("row ",filas, " appears in two different registries"))
            }
            else { # one or no registry (should always be the case) else#4_<=1 id for the row analyzed
              # check that parent id obtained from db matches the one indicated in the csv
              if(length(idr)==0){ # if new registry #if_new_reg
                # first insert registry
                ir<-inserta_registro(id_ex,
                                     id_ci,
                                     id_am,
                                     id_us,
                                     as.character(df[filas,as.numeric(input$fecha)]),
                                     ifelse(input$hora==0,"",ifelse(nchar(df[filas, as.numeric(input$hora)])<5,
                                                                    df[filas, as.numeric(input$hora)],
                                                                    stop(paste0("row ",
                                                                                filas,
                                                                                " has more than 4 digits in the time column"))
                                     )),
                                     db,
                                     ifelse(input$id_fk_subr>0 && df[filas,input$fk_subr]!="",parentid,0),
                                     ifelse(input$id_fk_subr>0 && df[filas,input$fk_subr]!="",as.character(df[filas,input$id_fk_subr]),""),
                                     paste(id_factors_str[filas],id_levels_str[filas],id_scales_str[filas],sep = "."),
                                     ii)[1,1]
                # increase registry counter
                ins_reg<-ins_reg+1
                
                # loop to insert the factors and their levels associated to the former registry
                for (column in as.numeric(input$factor[1]):as.numeric(input$factor[2])){ # loop over factor columns #loop_f_col
                  id_fact<-trae_id_factor(colnames(df)[column],db)$id_factor
                  id_nivel<-trae_id_nivel(df[filas, column],id_fact,db)$id_nivel
                  inserta_registro_nivel(ir,
                                         id_fact,
                                         id_nivel,db)
                } # end of #loop_f_col
                
                # loop to gather the type of dependent variable and then save the value of each dependent variable
                ronda <- 0 # set counter to know which input check is being considered to 0
                for(idtv in if(input$format=="wide"){input$check}else{input$check_long}){ # loop over reported data types #loop_tv
                  ronda <- ronda + 1
                  if(input$format=="wide"){
                    num_slider<-ronda # which(input$check==idtv) # NO CREO QUE FUNCIONE
                    col_slider<-prueba[num_slider,]} else{
                      if(input$format=="long"){
                        idtv_long<-(trae_tipo_dato()%>%filter(nombre_tipo_dato==idtv))$id_tipo_dato
                      }
                    }
                  
                  ums<-lapply(1:length(input$check), function(i) {
                    input[[paste0("um", i)]]
                  })
                  df_ums<-data.frame(udm=unlist(ums))
                  
                  observaciones <- lapply(1:length(input$check), function(i) {
                    input[[paste0("obs", i)]]
                  })
                  df_obs<-data.frame(obs=unlist(observaciones))
                  for(dat_x_tv in col_slider$menor:col_slider$mayor){ # loop over columns of a given data type. #loop_dat_x_tv
                    if(df[filas,dat_x_tv]!=""){ # insert value if cell value is not empty 
                      iddat<-trae_id_dato(colnames(df)[dat_x_tv],db)$id_dato # fetch data column id
                      
                      # irxtd <- inserta_reg_x_td(ir, 
                      #                  ifelse(input$format=="long",idtv_long,idtv), 
                      #                  trae_tv() %>% filter(unidad_medida ==df_ums[ronda,1]) %>% pull(id_tipo_valor),
                      #                  df_obs[ronda, 1])

                      # insert value
                      inserta_registro_valor(iddat,
                                             as.numeric(df[filas,dat_x_tv]),
                                             trae_tv() %>% filter(unidad_medida ==df_ums[ronda,1]) %>% pull(id_tipo_valor), #ESPECIFICAR CUAL
                                             ir,
                                             ifelse(input$format=="long",idtv_long,idtv),
                                             trae_obs_td(input$experimento, 
                                                         trae_tipo_dato() %>% 
                                                           filter(id_tipo_dato == ifelse(input$format=="long",idtv_long,idtv)) %>%
                                                           pull(nombre_tipo_dato)) %>% 
                                               filter(obs_tipo_dato == df_obs[ronda, 1]) %>%
                                               pull(id_obs_tipo_dato),
                                             ii,
                                             db)
                      # increase the counter of inserted values
                      ins_val<-ins_val+1
                    }
                  } # end of #loop_dat_x_tv
                } # end of #loop_tv
                
                # loop to insert the scale of the registry
                for(col3 in 1:length(input$escala[1]:input$escala[2])){ # loop over scale columns #loop_scale_col
                  if(!is.na(id_esc[filas,col3])){ # if scale cell is not empty...
                    inserta_registro_escala(ir, id_esc[filas,col3],db) # insert scale
                  }
                } # end of #loop_scale_col
              } # end of #if_new_reg
              else { # one registry already in db #else_there_is_reg
                # loop over columns of dependent variables to extract their values
                ronda <- 0
                for (idtv in if(input$format=="wide"){input$check}else{input$check_long}){ # loop over reported data types #loop_tv2  
                  ronda <- ronda + 1
                  if(input$format=="wide"){ 
                    num_slider<- ronda # which(input$check==idtv)
                    col_slider<-prueba[num_slider,]
                    } else {
                      if(input$format=="long"){
                        idtv_long<- (trae_tipo_dato()%>%filter(nombre_tipo_dato==idtv))$id_tipo_dato 
                        # (trae_id_tipo_valor()%>%filter(nombre_tipo_valor==idtv))$id_tipo_valor
                      }
                    }
                  ums<-lapply(1:length(input$check), function(i) {
                    input[[paste0("um", i)]]
                  })
                  df_ums<-data.frame(udm=unlist(ums))
                  
                  observaciones <- lapply(1:length(input$check), function(i) {
                    input[[paste0("obs", i)]]
                  })
                  df_obs<-data.frame(obs=unlist(observaciones))
                  
                  for(dat_x_tv in col_slider$menor:col_slider$mayor){ # loop over columns of a given data type. #loop_dat_x_tv2
                    iddat<-trae_id_dato(colnames(df)[dat_x_tv],db)$id_dato # fetch data column id
                    # brings the existing value of a given dependent variable, factor, level and scale from a given registry
                    registrado<-suppressWarnings(dbGetQuery(db,paste0( "select rv.valor from registro r
                                                                       left join registro_valor rv on r.id_registro=rv.fk_id_registro
                                                                       left join registro_escala re on re.fk_id_registro=rv.fk_id_registro
                                                                       left join registro_nivel rn on rn.fk_id_registro=rv.fk_id_registro
                                                                       where r.id_registro=",idr," and rv.fk_id_dato =",iddat," and rv.fk_id_tipo_dato=",
                                                                       ifelse(input$format=="long",idtv_long,idtv))))
                    # keep only one value, to compare it with the one in the spreadsheet
                    datosXid<-unique(registrado$valor)
                    # value in the speardsheet
                    valor<-df[filas,dat_x_tv]
                    
                    # insert if no previous value stored
                    if(length(datosXid)==0){ # enter if there was no value for that id #if_no_prev_data
                      if(valor!=""){ # enter if cell is not empty #if_not_empty
                        for (column in as.numeric(input$factor[1]):as.numeric(input$factor[2])){ # loop to insert factor levels if needed #loop_niv
                          # bring factor id
                          id_fact<-trae_id_factor(colnames(df)[column],db)$id_factor
                          # bring level id
                          id_nivel<-trae_id_nivel(df[filas, column],id_fact,db)$id_nivel
                          # bring id of a given level in a given registry
                          irn<-dbGetQuery(db,paste0("SELECT id_registro_nivel FROM registro_nivel
                                                    where fk_id_registro=",idr," and fk_id_factor=",id_fact,
                                                    " and fk_id_nivel=",id_nivel)
                          )$id_registro_nivel
                          
                          # if this level was not included for this registry, insert it
                          if(length(unique(irn))==0){ 
                            inserta_registro_nivel(idr,
                                                   id_fact,
                                                   id_nivel,db)
                          }
                        } # end of #loop_niv
                        
                        for(col3 in 1:length(as.numeric(input$escala)[1]:as.numeric(input$escala)[2])){ # loop to insert scale levels if needed #loop_sca
                          # fetch scale info for this registry
                          ire<-dbGetQuery(db,paste0("SELECT id_registro_escala FROM registro_escala
                                                    where fk_id_registro=",idr," and fk_id_escala=",id_esc[filas,col3])
                          )$id_registro_escala
                          
                          # if this scale was not included for this registry, insert it
                          if(length(unique(ire))==0){
                            if(!is.na(id_esc[filas,col3])){
                              inserta_registro_escala(idr, id_esc[filas,col3],db)
                            }
                          }
                        } # end of #loop_sca
                        
                        # irxtd <- inserta_reg_x_td(idr, 
                        #                           ifelse(input$format=="long",idtv_long,idtv), 
                        #                           trae_tv() %>% filter(unidad_medida ==df_ums[ronda,1]) %>% pull(id_tipo_valor),
                        #                           df_obs[ronda, 1])
                        
                        # insert the value
                        inserta_registro_valor(iddat,
                                               as.numeric(df[filas,dat_x_tv]),
                                               trae_tv() %>% filter(unidad_medida ==df_ums[ronda,1]) %>% pull(id_tipo_valor),
                                               idr,
                                               ifelse(input$format=="long",idtv_long,idtv),
                                               trae_obs_td(input$experimento, 
                                                           trae_tipo_dato() %>% 
                                                             filter(id_tipo_dato == ifelse(input$format=="long",idtv_long,idtv)) %>%
                                                             pull(nombre_tipo_dato)) %>% 
                                                 filter(obs_tipo_dato == df_obs[ronda, 1]) %>%
                                                 pull(id_obs_tipo_dato),
                                               ii,
                                               db)
                        
                        # update the counter of inserted values
                        ins_val2<-ins_val2+1
                      } # end of #if_not_empty
                    } # end of #if_no_prev_data
                    else { # else for when previous data available #else_avail_data
                      if (length(datosXid)>1){ # stops if > 1 value per id (should never occur)
                        stop(paste0("More than one value stored for row ",filas, " and column ",dat_x_tv))
                      } 
                      else { # when a different value was previously stored #else_prev_data_!=
                        if(datosXid != valor){ # enter if values differ #if_dif_val
                          if(input$replace=="Stop with warning"){ # if user selected stop with warning #stop_warning
                            stop(paste0("Value from row ",filas, " and column ",dat_x_tv," is different from that in the database. 
                                        This error might appear if data has changed or if you forgot to properly assign a factor, 
                                        level, scale, date or time (i.e. two rows belonging to the same registry)."))
                          } # end of #stop_warning
                          else { #update_data
                            # update value
                            update_ins(ifelse(valor!="",valor,'NULL'), ii, idr,iddat,
                                       trae_tv() %>% filter(unidad_medida ==df_ums[ronda,1] %>% pull(id_tipo_valor)),
                                       ifelse(input$format=="long",idtv_long,idtv),db)
                            # update counter of updated values
                            upd_val<-upd_val+1
                          } # end of #update_data
                          } # end of #if_dif_val
                      } # end of #else_prev_data_!=
                } #else_avail_data 
              } # end of #loop_dat_x_tv2
                } # end of #loop_tv2
                } # end of #else_there_is_reg
                } # end of else#4_<=1 id for the row analyzed
                  } # end of loop#3_each_row
          
          # if no insertions or updates, delete registry of insertions
          if((ins_val+ins_val2+upd_val)==0){
            query<-paste0("DELETE FROM inserciones WHERE id_insert=",ii)
            dbExecute(db,query)
          }
          # register that there were updates
          if(upd_val>0){
            query<-paste0("update inserciones i set i.hubo_update=1 where i.id_insert=",ii)
            dbExecute(db,query)
          }
          
          # finish the transaction
          dbCommit(db)
          output$inserted<-renderText({paste0(ins_reg," registries inserted.")})
          output$skipped<-renderText({paste0(nrow(df)-ins_reg," registries skipped (no changes)")})
          output$ins_valor<-renderText({paste0(ins_val+ins_val2," values added in total.")})
          output$updated<-renderText({paste0(upd_val," updated values.")})
          output$no_inserta<-renderText({paste0(length(which(df[,datos]!=""))-ins_val-ins_val2-upd_val," values ommited because they were already registered (except deletions).")})
              }, # end of TryCatch expression. #trycatchexpr
        # if there were errors, roll back the transaction and print the error
        error = function(err) {
          dbRollback(db)
          shinyjs::html("error_msg", err$message)
          shinyjs::show(id = "error", anim = TRUE, animType = "fade")
          stop(err$message)
        },
        finally = {
          dbDisconnect(db)
        }) # end tryCatch
        }) #formData
    observeEvent(input$submit, {
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      tryCatch({
        print(formData())
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
    } #shinyServer_funtion
      )  #shinyServer