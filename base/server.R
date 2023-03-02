source("../utils.R")
source("../connection.R")

mydb <- conecta()

# bring all data
all1<-suppressWarnings(dbGetQuery(mydb, "select r.id_registro,r.anio, te.nombre, e.nombre_escala,
                                  f.nombre_factor, n.nombre_nivel, d.nombre_dato, ex.nombre_experimento, ex.escala_medicion,ex.unidad_medicion,
                                  rv.valor, td.nombre_tipo_dato, tv.unidad_medida
                                  from registro r
                                  left join registro_escala re on re.fk_id_registro = r.id_registro
                                  left join escala e on e.id_escala = re.fk_id_escala
                                  left join tipo_escala te on te.id_tipo_escala = e.fk_tipo_escala
                                  left join experimento ex on ex.id_experimento = r.fk_id_experimento
                                  left join registro_nivel rn on rn.fk_id_registro = r.id_registro
                                  left join nivel n on n.id_nivel = rn.fk_id_nivel
                                  left join factor f on f.id_factor = rn.fk_id_factor
                                  left join registro_valor rv on rv.fk_id_registro = r.id_registro
                                  left join dato d on d.id_dato = rv.fk_id_dato
                                  left join tipo_valor tv on tv.id_tipo_valor = rv.fk_id_tipo_valor
                                  left join tipo_dato td on td.id_tipo_dato = rv.fk_id_tipo_dato"))

dbDisconnect(mydb)

# if there is data...
if(nrow(all1)>0){
  # put it on the wide format
  all2<-all1 %>% pivot_wider(names_from = "nombre", values_from = "nombre_escala")
    # spread(all1,key="nombre", value="nombre_escala")
  all3<-all2 %>% pivot_wider(names_from = "nombre_factor", values_from = "nombre_nivel")
    # spread(all2, key="nombre_factor",value = "nombre_nivel")
  all<-all3 %>% pivot_wider(names_from = "nombre_dato", values_from = "valor")
    # spread(all3, key="nombre_dato",value = "valor")
  all$nombre_experimento<-iconv(all$nombre_experimento,from = "latin1")
  all$anio<-as.numeric(all$anio)
  for (i in 1:length(na.omit(unique(all1$nombre_factor)))){
    all[,unique(all1$nombre_factor)[i]]<-as.factor(all[,unique(all1$nombre_factor)[i]])
  }
  # rows per study 
  dxe<-all %>% group_by(nombre_experimento) %>% summarise(n = n())
    # aggregate(all[,1], list(exp=all$nombre_experimento), length)
  
  # rows per dependent variable
  datos<-trae_dato()$nombre_dato
  datos<-datos[datos%in%colnames(all)]
  todo<-NULL
  for (i in (1:ncol(all))[colnames(all)%in%datos]){
    num.tmp<-length(all[!(is.na(all[,i])),i])
    fila<-cbind(colnames(all)[i],num.tmp)
    todo<-rbind(todo,fila)
  }
  todo<-data.frame(todo)
  todo<-todo[order(as.numeric(as.character(todo$num.tmp)),decreasing = T),][1:15,]
  todo$V1<-fct_inorder(factor(todo$V1))
  # rows per year
  dxy<-aggregate(all[,1], list(exp=all$anio), length)
  
  # rows per variable's type
  dxv<-aggregate(all[,1], list(exp=all$nombre_tipo_dato), length)
}

print(dxv)
print(dxy)
print(dxe)
# if there is no data yet show a message
shinyServer(function(input, output) {
  output$status <- renderUI({
    if(nrow(all1)>0){return()} else {
      box(title = "Nothing in my pockets...", status = "primary", solidHeader = T,collapsible = F,textOutput("status"),"...nothing up my sleeves")
    }
  })

  # summary plots  
  output$dxe<- renderUI({
    if(nrow(all1)>0){
      output$dxe1 <- renderPlotly({
        ggplot(dxe,aes(nombre_experimento,n,fill=nombre_experimento))+
          geom_bar(stat="identity")+
          labs(x="Study's name",y="Amount of observations (rows)")+
          theme_cowplot()+
          theme(legend.position = "none",
                axis.text.x = element_text(angle = 45, size = rel(.75)),
                axis.title.y = element_text(size=rel(0.8)) )
      })
      box(title = "Data per experiment", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          plotlyOutput("dxe1"))
    } else{return()}
  })

  output$todo<-renderUI({
    if(nrow(all1)>0){
      output$todo1 <- renderPlotly({
      ggplot(todo,aes(V1,as.numeric(as.character(num.tmp)),color=as.numeric(V1)))+
        geom_bar(stat="identity")+
        labs(x="Dependent variable (15 most common)",y="Amount of observations (rows)")+
        theme_cowplot()+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 45, size = rel(.75)),
              axis.title.y = element_text(size=rel(0.8)) )
    })
    box(title = "Data per dependent variable", status = "info", solidHeader = TRUE,
        collapsible = TRUE,plotlyOutput("todo1"))
    } else{return()}
  })
  
  output$dxy<- renderUI({
    if(nrow(all1)>0){
      output$dxy1 <- renderPlotly({
        ggplot(dxy,aes(exp,id_registro,fill=exp))+
          geom_bar(stat="identity")+
          labs(x="Year",y="Amount of observations (rows)")+
          theme_cowplot()+
          theme(legend.position = "none")
      })
      box(title = "Data per year", status = "success", solidHeader = TRUE,
          collapsible = TRUE,plotlyOutput("dxy1"))
    } else{return()}
  })
  
  output$dxv<- renderUI({
    if(nrow(all1)>0){
      output$dxv1 <- renderPlotly({
        ggplot(dxv,aes(exp,id_registro,fill=exp))+
          geom_bar(stat="identity")+
          labs(x="Type of dependent variable",y="Amount of observations (rows)")+
          theme_cowplot()+
          theme(legend.position = "none")
      })
      box(title = "Data per type of variable", status = "warning", solidHeader = TRUE,
          collapsible = TRUE,plotlyOutput("dxv1"))
    } else{return()}
  })
})

