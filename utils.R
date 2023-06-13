library(RMySQL)
library(shinydashboard)

menu<-dashboardSidebar(
  sidebarMenu(
  menuItem("New study", icon = icon("map-marked-alt"),href = "../alta/", newtab = F),
  menuItem("New system, factor, level or scale", icon = icon("images"),href = "../alta_amb/", newtab = F),
  menuItem("New scale for a given study", icon = icon("sitemap"),href = "../alta_escala/", newtab = F),
  menuItem("New user, variable & data type", icon = icon("ruler"),href = "../alta_tvalor/", newtab = F),
  menuItem("New dependent variable", icon = icon("pagelines"),href = "../alta_deps/", newtab = F),
  menuItem("Details for a given study", icon = icon("book"),href = "../alta_obs/", newtab = F),
  menuItem("Add data", icon = icon("database"), href = "../insertar/", newtab = F),
  menuItem("Add shared observations", icon = icon("clone"), href = "../alta_compartidos/", newtab = F),
  menuItem("Delete insertion event", icon = icon("backspace"),href = "../borrar_evento/", newtab = F),
  menuItem("Delete study", icon = icon("trash-alt"),href = "../borrar/", newtab = F),
  menuItem("Visualize data",icon = icon("eye"),href = "../visualizar/", newtab = F)
),width=250)

isset <- function(x) {
  is_variable <- make.names(name <- deparse(substitute(x))) == name
  if (is_variable && !exists(name)) return(FALSE)
  !is.null(x) && (length(x) != 1L || !is.na(x))
}

# VERIFICACIONES ####################
verifica_existencia_dato <- function (valor) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("SELECT id_dato FROM dato WHERE BINARY nombre_dato = '%s'", valor)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_tipo_escala <- function (valor) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_tipo_escala from tipo_escala WHERE BINARY nombre = '%s'", valor)
  data <- dbGetQuery(db, query)
  return(data)
}
verifica_nivel <- function (id_factor, nombre_nivel) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_nivel from nivel WHERE fk_id_factor = %s and BINARY nombre_nivel = '%s'", id_factor, nombre_nivel)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_factor <- function (nombre_factor) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_factor from factor WHERE BINARY nombre_factor = '%s'", nombre_factor)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_escala <- function (id_experimento, depende_de, id_tipo_escala, nombre_escala) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_escala from escala WHERE fk_id_experimento = %s and depende_de_escala = %s and fk_tipo_escala = %s and BINARY nombre_escala = '%s'", id_experimento, depende_de, id_tipo_escala, nombre_escala)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_escala2 <- function (id_experimento, depende_de, id_tipo_escala, nombre_escala, db) {
  query <- sprintf("select id_escala from escala WHERE fk_id_experimento = %s and depende_de_escala = %s and fk_tipo_escala = %s and BINARY nombre_escala = '%s'", id_experimento, depende_de, id_tipo_escala, nombre_escala)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_taxa_king <- function (nombre) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_king from taxa_king WHERE BINARY kingdom = '%s'", nombre)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_taxa_div <- function (nombre) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_div from taxa_div WHERE BINARY division = '%s'", nombre)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_taxa_fam <- function (nombre) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_fam from taxa_fam WHERE BINARY family = '%s'", nombre)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_taxa_gen <- function (nombre) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_gen from taxa_gen WHERE BINARY genera = '%s'", nombre)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_taxa_sp <- function (nombre) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_sp from taxa_sp WHERE BINARY species = '%s'", nombre)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_taxa_var <- function (nombre) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_var from taxa_var WHERE BINARY variety = '%s'", nombre)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_taxa_lifexp <- function (nombre) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_lifexp from taxa_lifexp WHERE BINARY life_exp = '%s'", nombre)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_taxa_lifeform <- function (nombre) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_lifeform from taxa_lifeform WHERE BINARY life_form = '%s'", nombre)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_taxa_functgrp <- function (nombre) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_functgrp from taxa_functgrp WHERE BINARY functional_group = '%s'", nombre)
  data <- dbGetQuery(db, query)
  return(data)
}

verifica_taxa_origen <- function (nombre) {
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf("select id_origen from taxa_origen WHERE BINARY provenance = '%s'", nombre)
  data <- dbGetQuery(db, query)
  return(data)
}



# INSERCIONES ####################
inserta_registro <- function (id_experimento, id_ciudad, id_ambiente, id_usuario, fecha_registro, hora,db, id_padre, ref_hijo, id_heading,id_ins) 
{
  query <- sprintf ("INSERT INTO registro (fk_id_experimento, fk_id_ciudad, fk_id_ambiente, fk_id_usuario, fecha_registro, anio, hora, id_registro_padre, ref_hijo,id_heading,fk_id_insercion) 
                    VALUES (%s, %s, %s, %s, '%s', '%s', '%s', %s, '%s', '%s',%s)", id_experimento, id_ciudad, id_ambiente, id_usuario, fecha_registro, substring(fecha_registro, 1, 4), hora,id_padre,ref_hijo,id_heading,id_ins)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  return(data)
}

inserta_inserc <- function (id_usuario, id_experimento, id_responsable,db) 
{
  query <- sprintf ("INSERT INTO inserciones (fk_id_usuario, fk_id_experimento, insertante) 
                    VALUES (%s, %s, %s)", id_usuario, id_experimento, id_responsable)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

inserta_registro_nivel <- function (id_registro, id_factor, id_nivel,db)
{
  query <- sprintf ("INSERT INTO registro_nivel (fk_id_registro, fk_id_factor, fk_id_nivel) VALUES (%s, %s, %s)", id_registro, id_factor, id_nivel)
  (  data<-dbExecute(db, query))
  return(data)
}

inserta_registro_valor <- function (id_dato, valor, 
                                    tipo_valor,
                                    id_registro,
                                    fk_id_tipo_dato, 
                                    fk_id_obs_td,
                                    id_ins, db){
  query <- sprintf ("INSERT INTO registro_valor (fk_id_dato, valor, fk_id_tipo_valor, fk_id_registro,
                    fk_id_tipo_dato,fk_id_obs_td,fk_id_insercion) VALUES (%s, %s, %s, %s, %s, %s, %s)", 
                    id_dato, valor, tipo_valor,
                    id_registro,
                    fk_id_tipo_dato,fk_id_obs_td,id_ins)
  (data<-dbExecute(db, query))
  return(data)
}

inserta_registro_escala <- function (id_registro, id_escala,db)
{
  query <- sprintf ("INSERT INTO registro_escala (fk_id_registro, fk_id_escala) VALUES (%s, %s)", id_registro, id_escala)
  (data<-dbExecute(db, query))
  return(data)
}


# ALTAS ##################

# Experimento #####
alta_exp <- function (nombre_exp, fecha, id_usuario, id_tipo_exp, obs, escala_med,unidad_med,id_ciudad, db) 
{
  query <- sprintf ("INSERT INTO experimento (nombre_experimento, fecha_experimento, anio, fk_id_usuario, 
                    fk_id_tipo_experimento,obs_experimento, escala_medicion,unidad_medicion,fk_id_ciudad) 
                    VALUES ('%s', '%s', '%s', %s, %s, '%s','%s','%s',%s)", nombre_exp, fecha, substr(fecha,1,4), 
                    id_usuario, id_tipo_exp, obs,escala_med,unidad_med,id_ciudad)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_ciu <- function (nombre_ciudad, lat, long, db) 
{
  query <- sprintf ("INSERT INTO ciudad (ciudad_nombre, latitud,longitud) 
                    VALUES ('%s', '%s','%s')", nombre_ciudad, lat, long)
  (data <- dbExecute(db, query))
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_usu <- function (nombre_usuario, db) 
{
  query <- sprintf ("INSERT INTO usuarios (nombre_usuario) 
                    VALUES ('%s')", nombre_usuario)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

# Ambiente, factores, niveles, tipo escala #############
alta_amb <- function (nombre_ambiente,db) 
{
  query <- sprintf ("INSERT INTO ambiente (nombre_ambiente) 
                    VALUES ('%s')", nombre_ambiente)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_fac <- function (nombre_factor, db) 
{
  query <- sprintf ("INSERT INTO factor (nombre_factor) 
                    VALUES ('%s')", nombre_factor)
(data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_niv <- function (fk_id_factor, nombre_nivel, obs_nivel, db) 
{
  query <- sprintf ("INSERT INTO nivel (fk_id_factor, nombre_nivel, obs_nivel) 
                    VALUES (%s, '%s', '%s')", fk_id_factor,nombre_nivel, obs_nivel)
  (data <- dbExecute(db, query))
  return(data)
}

alta_tipo_esc<-function (nombre, db) 
{
  query <- sprintf ("INSERT INTO tipo_escala (nombre) 
                    VALUES ('%s')", nombre)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

# Tipo dato y valor #############
alta_tipo_dato <- function (nombre_tipo_dato, 
                            # fk_id_agrupador, 
                            mostrar) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO tipo_dato (nombre_tipo_dato, mostrar) 
                    VALUES ('%s', %s)", nombre_tipo_dato, mostrar)
  (data <- dbExecute(db, query))
  
  return(data)
}

alta_tvalor <- function (unidad_medida,decimales) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO tipo_valor (unidad_medida,decimales) 
                    VALUES ('%s', %s)", unidad_medida,decimales)
  (data <- dbExecute(db, query))
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  return(data)
}

alta_vpd <- function (fk_id_tipo_dato,fk_id_tipo_valor) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO valor_por_dato (fk_id_tipo_dato,fk_id_tipo_valor) 
                    VALUES (%s, %s)", fk_id_tipo_dato,fk_id_tipo_valor)
  (data <- dbExecute(db, query))
  return(data)
}

alta_obs_td <- function (fk_id_exp, fk_id_td, obs_td, db)
{
  query <- sprintf ("INSERT INTO obs_tipo_dato (fk_id_exp, fk_id_td, obs_tipo_dato) VALUES (%s, %s, '%s')", fk_id_exp, fk_id_td, obs_td)
  (data<-dbExecute(db, query))
  
  return(data)
}

# Dependent variable ################
alta_data_taxa <- function (nombre_dato,json_mas_data, es_vivo, fk_id_king, fk_id_div, fk_id_fam, fk_id_gen, fk_id_sp, fk_id_var, fk_id_lifexp, fk_id_lifeform, fk_id_functgrp, fk_id_origen) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO dato (nombre_dato, json_mas_data, es_vivo, fk_id_king, fk_id_div, fk_id_fam, fk_id_gen, fk_id_sp, fk_id_var, fk_id_lifexp, fk_id_lifeform, fk_id_functgrp, fk_id_origen) 
                    VALUES ('%s', '%s', %s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %s)", nombre_dato,json_mas_data, es_vivo, fk_id_king, fk_id_div, fk_id_fam, fk_id_gen, fk_id_sp, fk_id_var, fk_id_lifexp, fk_id_lifeform, fk_id_functgrp, fk_id_origen)
  (data <- dbExecute(db, query))
  return(data)
}

alta_data_notaxa <- function (nombre_dato,json_mas_data, es_vivo) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO dato (nombre_dato, json_mas_data, es_vivo) 
                    VALUES ('%s', '%s', %s)", nombre_dato, json_mas_data, es_vivo)
  (data <- dbExecute(db, query))
  return(data)
}

# Escalas ###########
alta_esc <- function (nombre_escala, obs_escala, depende_de_escala, latitud, longitud, fk_tipo_escala,fk_id_experimento,db) 
{
  query <- sprintf ("INSERT INTO escala (nombre_escala, obs_escala, depende_de_escala, latitud, longitud, fk_tipo_escala,fk_id_experimento) 
                    VALUES ('%s', '%s', %s, '%s', '%s', %s,%s)", 
                    nombre_escala, obs_escala, depende_de_escala, latitud, longitud, fk_tipo_escala,fk_id_experimento)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

# Taxa ##########

alta_taxa_king <- function (king) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO taxa_king (kingdom) 
                    VALUES ('%s')", 
                    king)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_taxa_div <- function (div) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO taxa_div (division) 
                    VALUES ('%s')", 
                    div)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_taxa_fam <- function (fam) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO taxa_fam (family) 
                    VALUES ('%s')", 
                    fam)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_taxa_gen <- function (gen) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO taxa_gen (genera) 
                    VALUES ('%s')", 
                    gen)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_taxa_sp <- function (sp) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO taxa_sp (species) 
                    VALUES ('%s')", 
                    sp)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_taxa_var <- function (var) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO taxa_var (variety) 
                    VALUES ('%s')", 
                    var)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_taxa_lifexp <- function (lifexp) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO taxa_lifexp (life_exp) 
                    VALUES ('%s')", 
                    lifexp)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_taxa_lifeform <- function (lifeform) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO taxa_lifeform (life_form) 
                    VALUES ('%s')", 
                    lifeform)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_taxa_functgrp <- function (functgrp) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO taxa_functgrp (functional_group) 
                    VALUES ('%s')", 
                    functgrp)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

alta_taxa_origen <- function (origen) 
{
  db <- conecta()
  on.exit(dbDisconnect(db))
  query <- sprintf ("INSERT INTO taxa_origen (provenance) 
                    VALUES ('%s')", 
                    origen)
  (data <- dbExecute(db, query))
  
  query <- sprintf("SELECT LAST_INSERT_ID()");
  data <- dbGetQuery(db, query)
  
  return(data)
}

# Duplicado ####
alta_duplicado <- function (idesc1, idesc2, db) 
{
  query <- sprintf ("INSERT INTO compartidos (fk_id_esc1, fk_id_esc2) 
                    VALUES (%s, %s)", 
                    idesc1, idesc2)
  data <- dbExecute(db, query)

  return(data)
}


# BAJA #################

baja_exp<-function(id_experimento,db){
  query<-sprintf("DELETE FROM experimento WHERE id_experimento=%s", id_experimento)
  data <- dbGetQuery(db, query)
  return(data)
}

baja_ins_reg<-function(id_ins,db){
  query<-sprintf("DELETE FROM registro WHERE fk_id_insercion=%s", id_ins)
  data <- dbGetQuery(db, query)
  return(data)
}

baja_insertion<-function(id_ins,db){
  query<-sprintf("DELETE FROM inserciones WHERE id_insert=%s", id_ins)
  data <- dbGetQuery(db, query)
  return(data)
}

# TRAE ##############

# General
trae_usuarios<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  Usuarios<-dbGetQuery(mydb, "select id_usuario,nombre_usuario from usuarios")
  return(Usuarios)
}

trae_nombre_usuario<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  usu<-dbGetQuery(mydb, "select nombre_usuario from usuarios")
  return(usu)
}

trae_exper<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  Exper<-dbGetQuery(mydb, "select id_experimento, nombre_experimento, obs_experimento from experimento")
  return(Exper)
}

trae_nombre_experimento<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  exper<-dbGetQuery(mydb, "select nombre_experimento from experimento")
  return(exper)
}

trae_tipoexp<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  tipoexp<-dbGetQuery(mydb, "select * from tipo_experimento")
  return(tipoexp)
}

trae_ciudad<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  Ciudad<-dbGetQuery(mydb, "select * from ciudad")
  return(Ciudad)
}

trae_nombre_ciudad<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  ciudad<-dbGetQuery(mydb, "select ciudad_nombre from ciudad")
  return(ciudad)
}

# Taxa ###############

trae_taxa_king<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  king<-dbGetQuery(mydb, "select id_king, kingdom from taxa_king")
  return(king)
}
trae_taxa_div<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  king<-dbGetQuery(mydb, "select id_div, division from taxa_div")
  return(king)
}
trae_taxa_fam<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  king<-dbGetQuery(mydb, "select id_fam, family from taxa_fam")
  return(king)
}
trae_taxa_gen<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  king<-dbGetQuery(mydb, "select id_gen, genera from taxa_gen")
  return(king)
}
trae_taxa_sp<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  king<-dbGetQuery(mydb, "select id_sp, species from taxa_sp")
  return(king)
}
trae_taxa_var<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  king<-dbGetQuery(mydb, "select id_var, variety from taxa_var")
  return(king)
}
trae_taxa_lifexp<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  king<-dbGetQuery(mydb, "select id_lifexp, life_exp from taxa_lifexp")
  return(king)
}
trae_taxa_lifeform<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  king<-dbGetQuery(mydb, "select id_lifeform, life_form from taxa_lifeform")
  return(king)
}
trae_taxa_functgrp<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  king<-dbGetQuery(mydb, "select id_functgrp, functional_group from taxa_functgrp")
  return(king)
}
trae_taxa_origen<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  king<-dbGetQuery(mydb, "select id_origen, provenance from taxa_origen")
  return(king)
}

# Factores, niveles, ambiente, tipo escala ############

trae_factores<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  Factores<-dbGetQuery(mydb, "select * from factor")
  return(Factores)
}

trae_nombre_factor<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  Factores<-dbGetQuery(mydb, "select nombre_factor from factor")
  return(Factores)
}

trae_nombre_ambiente<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  amb<-dbGetQuery(mydb, "select nombre_ambiente from ambiente")
  return(amb)
}

trae_nombre_tipo_escala<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  Tescala<-dbGetQuery(mydb, "select nombre from tipo_escala")
  return(Tescala)
}


# Tipo dato y variable ##########

trae_agrupador<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  exper<-dbGetQuery(mydb, "select * from agrupador")
  return(exper)
}

# Inserciones ##########
trae_all_insertion<-function(id_ins){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  ins<-dbGetQuery(mydb, sprintf("SELECT id_registro,nombre_escala, obs_escala,nombre_nivel,nombre_factor, valor, nombre_dato, valor_previo
FROM registro r left join
inserciones i on r.fk_id_insercion=i.id_insert
left join registro_escala re on re.fk_id_registro = r.id_registro
                 left join escala e on e.id_escala = re.fk_id_escala
                 left join tipo_escala te on te.id_tipo_escala = e.fk_tipo_escala
                 left join registro_nivel rn on rn.fk_id_registro=r.id_registro
                 left join nivel n on n.id_nivel=rn.fk_id_nivel
                 left join factor f on f.id_factor=rn.fk_id_factor
                 left join registro_valor rv on rv.fk_id_registro=r.id_registro
                 left join dato d on d.id_dato=rv.fk_id_dato
where i.fecha_ins='%s'",id_ins))
  return(ins)
}

trae_hubo_update<-function(id_ins){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  ins<-dbGetQuery(mydb, sprintf("SELECT hubo_update FROM inserciones i where i.fecha_ins='%s'",id_ins))
  return(ins)
}

trae_all_hubo_update<-function(id_ins){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  ins<-dbGetQuery(mydb, sprintf("SELECT id_registro,nombre_escala, obs_escala,nombre_nivel,nombre_factor, valor, nombre_dato, valor_previo FROM inserciones i 
left join registro_valor rv on rv.fk_id_insercion=i.id_insert
left join dato d on d.id_dato=rv.fk_id_dato
left join registro r on r.id_registro=rv.fk_id_registro
left join registro_escala re on re.fk_id_registro = r.id_registro
                 left join escala e on e.id_escala = re.fk_id_escala
                 left join tipo_escala te on te.id_tipo_escala = e.fk_tipo_escala
                 left join registro_nivel rn on rn.fk_id_registro=r.id_registro
                 left join nivel n on n.id_nivel=rn.fk_id_nivel
                 left join factor f on f.id_factor=rn.fk_id_factor

where fecha_ins='%s'",id_ins))
  return(ins)
}

# Registros ###########

trae_id_insertion<-function(id_ins){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  ins<-dbGetQuery(mydb, sprintf("SELECT i.id_insert FROM inserciones i where i.fecha_ins='%s'",id_ins))
  return(ins)
}

trae_id_rv<-function(id_ins,db){
  query<-sprintf('SELECT id_registro_valor FROM registro_valor where fk_id_insercion=%s', id_ins)
  data <- dbGetQuery(db, query)
  return(data)
}

trae_insertion<-function(id_ex){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  ins<-suppressWarnings(dbGetQuery(mydb, sprintf("SELECT fecha_ins, insertante FROM inserciones WHERE fk_id_experimento=%s",id_ex)))
  return(ins)
}

trae_id_factor<-function(nombre_factor,db){
  query<-paste0("select id_factor from factor where nombre_factor='",nombre_factor,"'")
  Factores<-dbGetQuery(db, query)
  return(Factores)
}

trae_id_nivel<-function(nombre_nivel,id_factor,db){
  query<-paste0("select id_nivel from nivel where nombre_nivel='",nombre_nivel,"' and fk_id_factor=",id_factor)
  Niveles<-dbGetQuery(db, query)
  return(Niveles)
}

trae_id_dato<-function(nombre_dato,db){
  query<-sprintf("SELECT id_dato FROM dato where nombre_dato=('%s')",nombre_dato)
  iddato<-dbGetQuery(db, query)
  return(iddato)
}

# trae_id_agrupador<-function(nombre_dato,db){
#   query<-sprintf("SELECT a.id_agrupador FROM agrupador a 
#                  left join tipo_dato td on td.fk_id_agrupador = a.id_agrupador
#                  where td.nombre_tipo_dato=('%s')",nombre_dato)
#   idagrup<-dbGetQuery(db, query)
#   return(idagrup)
# }

trae_id_experimento<-function(nombre_experimento){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("SELECT id_experimento FROM experimento where nombre_experimento=('%s')",nombre_experimento)
  idex <-dbGetQuery(mydb, query)
  return(idex)
}

trae_yr_x_idex<-function(id_ex){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("SELECT distinct(fecha_registro) FROM registro where fk_id_experimento in (%s)",id_ex)
  idex.tmp<-dbGetQuery(mydb, query)
  idex <- unique(as.numeric(substring(idex.tmp$fecha_registro, 1, 4)))
  return(idex)
}

trae_obs_x_idex<-function(id_ex){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("SELECT nombre_experimento,obs_experimento FROM experimento where id_experimento in (%s)",id_ex)
  idex<-dbGetQuery(mydb, query)
  return(idex)
}

trae_id_tipo_valor<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  TDato<-dbGetQuery(mydb, "select id_tipo_valor from tipo_valor")
  return(TDato)
}

trae_id_tipo_dato<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  TDato<-dbGetQuery(mydb, "select id_tipo_dato from tipo_dato")
  return(TDato)
}

trae_tv<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  TDato<-dbGetQuery(mydb, "select * from tipo_valor")
  return(TDato)
}

# trae_tv<-function(columnas){
#   mydb <- conecta()
#   on.exit(dbDisconnect(mydb))
#   query <- sprintf("select distinct(vpd.fk_id_tipo_valor)
#                    from 
#                    valor_por_dato vpd
#                    left join tipo_valor tv on tv.id_tipo_valor = vpd.fk_id_tipo_valor
#                    left join tipo_dato td on td.id_tipo_dato = vpd.fk_id_tipo_dato
#                    left join dato d on  d.fk_id_tipo_dato = td.id_tipo_dato
#                    where d.nombre_dato in (%s)",columnas)
#   data <- dbGetQuery(mydb, query)
#   return(data)
# }

trae_dato_filt<-function(columnas){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  query <- sprintf("select nombre_dato from 
                   dato d
                   where d.nombre_dato in (%s)",columnas)
  data <- dbGetQuery(mydb, query)
  return(data)
}

trae_ex_x_user<-function(usuario){
  mydb<-conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("select e.nombre_experimento, id_experimento, fk_id_ciudad
                        from experimento e
                        left join usuarios u on e.fk_id_usuario = u.id_usuario
                        where u.nombre_usuario =('%s')",usuario)
  data<-dbGetQuery(mydb,query)
  return(data)
}

trae_ex_x_tv<-function(tipo_dato){
  mydb<-conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("select distinct(ex.nombre_experimento), ex.id_experimento from registro r
                   left join experimento ex on ex.id_experimento = r.fk_id_experimento
                 left join registro_valor rv on rv.fk_id_registro = r.id_registro
                 left join tipo_dato td on td.id_tipo_dato = rv.fk_id_tipo_dato
                 where td.id_tipo_dato IN (%s)",tipo_dato)
  data<-dbGetQuery(mydb,query)
  return(data)
}

trae_td_x_ex<-function(exp){
  mydb<-conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("select distinct(td.nombre_tipo_dato), td.id_tipo_dato from registro r
                   left join experimento ex on ex.id_experimento = r.fk_id_experimento
                 left join registro_valor rv on rv.fk_id_registro = r.id_registro
                 left join tipo_dato td on td.id_tipo_dato = rv.fk_id_tipo_dato
                 where ex.nombre_experimento IN ('%s')",exp)
  data<-dbGetQuery(mydb,query)
  return(data)
}

trae_obs_td <- function(nombre_experimento,nombre_tipo_dato,db){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  query<-paste0("select id_obs_tipo_dato, obs_tipo_dato from obs_tipo_dato o 
                left join experimento ex on ex.id_experimento = o.fk_id_exp
                left join tipo_dato td on td.id_tipo_dato = o.fk_id_td
                where nombre_experimento ='",nombre_experimento,"' and nombre_tipo_dato='",nombre_tipo_dato,"'")
  obs<-dbGetQuery(mydb, query)
  return(obs)
}

trae_id_am<-function(ambiente){
  mydb<-conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("select a.id_ambiente
                 from ambiente a
                 where a.nombre_ambiente =('%s')",ambiente)
  data<-dbGetQuery(mydb,query)
  return(data)
}

trae_id_us<-function(usuario){
  mydb<-conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("select id_usuario
                 from usuarios
                 where nombre_usuario =('%s')",usuario)
  data<-dbGetQuery(mydb,query)
  return(data)
}
trae_niveles<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  Niveles<-dbGetQuery(mydb, "select * from nivel")
  colnames(Niveles)[2]<-"id_factor"
  return(Niveles)
}

trae_tipo_variable<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  TV<-dbGetQuery(mydb, "select unidad_medida, decimales from tipo_valor")
  return(TV)
}

trae_tipo_dato<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  TD<-dbGetQuery(mydb, "select * from tipo_dato")
  return(TD)
}

trae_nombre_tipo_dato<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  TD<-dbGetQuery(mydb, "select id_tipo_dato, nombre_tipo_dato from tipo_dato")
  return(TD)
}

trae_dato<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  Dato<-dbGetQuery(mydb, "select id_dato,nombre_dato, json_mas_data from dato")
  return(Dato)
}

trae_tipo_escala<-function(){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  Tescala<-dbGetQuery(mydb, "select * from tipo_escala")
  return(Tescala)
}

# Visualización ###########
trae_todo<-function(id_ex, id_tv, yrs){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("select r.id_registro,r.fecha_registro, r.id_registro_padre,e.obs_escala, e.nombre_escala,
                   f.nombre_factor, n.nombre_nivel, d.nombre_dato, ex.nombre_experimento, ex.escala_medicion,ex.unidad_medicion,
                 rv.valor, td.mostrar, td.nombre_tipo_dato, tv.unidad_medida, u.nombre_usuario, e.id_escala
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
                 left join tipo_dato td on td.id_tipo_dato = rv.fk_id_tipo_dato
                 left join inserciones ins on ins.id_insert =  rv.fk_id_insercion
                 left join usuarios u on u.id_usuario = ins.insertante
                 where r.fk_id_experimento in (%s) and rv.fk_id_tipo_dato in (%s) and r.anio in (%s)",
                 paste(id_ex, collapse = ","),paste(id_tv, collapse=","),paste(yrs, collapse = ","))
  extv<-suppressWarnings(dbGetQuery(mydb, query))
  return(extv)
}

trae_gps_x_idex<-function(id_ex){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("select distinct(e.nombre_escala), ex.nombre_experimento, te.nombre, e.latitud,e.longitud
                  from registro r
                 left join registro_escala re on re.fk_id_registro = r.id_registro
                 left join escala e on e.id_escala = re.fk_id_escala
                 left join tipo_escala te on te.id_tipo_escala = e.fk_tipo_escala
                 left join experimento ex on ex.id_experimento = r.fk_id_experimento
                 where ex.id_experimento in (%s) and te.id_tipo_escala =(%s)",paste(id_ex, collapse = ","),min(trae_tipo_escala()$id_tipo_escala))
  Gps<-dbGetQuery(mydb, query)
  return(Gps)
}

# Compartido #######
trae_escala <- function(id_ex){
  mydb <- conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("SELECT e.nombre_experimento, e1.obs_escala, e1.nombre_escala, ex.nombre_experimento, e2.obs_escala, e2.nombre_escala  FROM compartidos c
  left join escala e1 on e1.id_escala = c.fk_id_esc1
  LEFT JOIN experimento e on e.id_experimento = e1.fk_id_experimento
  LEFT JOIN escala e2 on e2.id_escala = c.fk_id_esc2
  LEFT JOIN experimento ex on ex.id_experimento = e2.fk_id_experimento")
  duplis<-dbGetQuery(mydb, query)
  return(data.frame(duplis))
}

trae_id_escala <- function(fk_id_ex, obs_escala, nombre_escala, db){
  query<-paste0("SELECT id_escala FROM escala where fk_id_experimento=",fk_id_ex, 
                        " and obs_escala ='", obs_escala, 
                        "' and nombre_escala = '", nombre_escala, "'")
  idesc<-dbGetQuery(db, query)
  return(idesc$id_escala)
}

trae_duplicados <- function(id_ex){
  mydb<-conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("select id_escala, nombre_escala, obs_escala, fk_id_experimento
                 from escala
                 where fk_id_experimento = (%s)", id_ex)
  data<-dbGetQuery(mydb,query)
  return(data)
}

trae_compartidos <- function(){
  mydb<-conecta()
  on.exit(dbDisconnect(mydb))
  query<-sprintf("select fk_id_esc2
                 from compartidos")
  data<-dbGetQuery(mydb,query)
  return(data)
}

# VARIOS ###############

update_ins<-function(valor, fk_id_insercion, fk_id_registro,fk_id_dato,fk_id_tipo_valor,fk_id_tipo_dato,db){
  query<-sprintf("update registro_valor rv set rv.valor_previo=rv.valor,
                 rv.valor = %s, rv.fk_id_insercion=%s where rv.fk_id_registro=%s 
                 and rv.fk_id_dato=%s and rv.fk_id_tipo_valor=%s and rv.fk_id_tipo_dato=%s",valor, fk_id_insercion, fk_id_registro,fk_id_dato,fk_id_tipo_valor,fk_id_tipo_dato)
  (data <- dbExecute(db, query))
  query<-sprintf("update inserciones set hubo_update=1 where id_insert=%s",fk_id_insercion);
  data <- dbGetQuery(db, query)
  return(data)
}

restore_upd<-function(ids_rvs,db){
  query<-sprintf('update registro_valor rv set rv.valor=rv.valor_previo,rv.valor_previo=NULL,fk_id_insercion=NULL 
                 where id_registro_valor IN (%s)', ids_rvs)
  data <- dbGetQuery(db, query)
  return(data)
}

