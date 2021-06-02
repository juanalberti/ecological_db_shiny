conecta <- function(){
  db <- dbConnect(MySQL(), user='ecology', password='ecologylab', dbname='Ecology_lab', host='localhost', port = 3306)
  dbGetQuery(db, "SET character_set_client = 'utf8', character_set_connection = 'utf8',
                     character_set_results = 'utf8', character_set_server = 'utf8';")
  return(db)
}
