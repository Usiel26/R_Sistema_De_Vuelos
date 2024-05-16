library(RMySQL)

lapply(dbListConnections(MySQL()), dbDisconnect) #poner para evitar que salga el error de conexiones
con <- dbConnect(MySQL(), user="root", host="localhost", password="75162689", dbname="tf2")

airplane = dbGetQuery(con, statement = "select * from airplane") #Se importa la tabla airplane
airports = dbGetQuery(con, statement = "select * from airports") #Se importa la tabla airports
departure = dbGetQuery(con, statement = "select * from departure") #Se importa la tabla departure
returns = dbGetQuery(con, statement = "select * from returns") #Se importa la tabla return
pilot = dbGetQuery(con, statement = "select * from pilot") #Se importa la tabla pilot
promotion = dbGetQuery(con, statement = "select * from promotion") #Se importa la tabla promotion
nationality = dbGetQuery(con, statement = "select * from nationality") #Se importa la tabla nationality
passenger= dbGetQuery(con, statement = "select * from passenger") #Se importa la tabla passenger
travels= dbGetQuery(con, statement = "select * from travels") #Se importa la tabla travels
