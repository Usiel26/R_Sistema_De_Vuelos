library(shiny)
library(ggplot2)

ui = fluidPage(
  textInput("username", "Username:"),
  passwordInput("password", "Password:"),
  actionButton("login", "Iniciar Sesión"),
  uiOutput("login_info"),
  textOutput("log"),
  
  conditionalPanel(
    condition = "output.login_info && output.log == 'Usuario y contraseña correctos!'",
    titlePanel("Empresa de viajes Viajando-Ando"),
    sidebarLayout(
      sidebarPanel("Sistema de registro de pasajeros y de vuelos"),
      mainPanel(
        tabsetPanel(
          id = "mytabs",
          tabPanel("Registro de pasajero",
                   "En esta interfaz se puede agregar y eliminar elementos a la base de datos",
                   selectInput("action", label = "Acción a realizar",
                               choices = c("Agregar pasajero", "Eliminar pasajero","Editar pasajero"),
                               selected = "Agregar cliente"),
                   conditionalPanel(
                     condition = "input.action == 'Agregar pasajero'",
                     textInput("nombre", "Nombre del pasajero: "),
                     textInput("apellido", "Apellido del pasajero: "),
                     selectInput("gender", "Género del pasajero: ",
                                 choices = c("Masculino", "Femenino", "Otro"),
                                 selected = "Otro"
                     ),
                     textInput("age", "Ingrese la edad del pasajero: ",value=0),
                     selectInput("nat", "Ingrese la nacionalidad del pasajero: ", choices = nationality$nationality),
                     actionButton("actionR","Ingresar pasajero",class = "btn-primary"),
                     uiOutput("info_accion"),
                   ),
                   conditionalPanel(
                     condition = "input.action == 'Eliminar pasajero'",
                     textInput("ape", "Apellido del pasajero: "),
                     selectInput("nombre2", "Nombre del pasajero: ", choices = NULL),
                     actionButton("actionR2","Eliminar pasajero",class = "btn-primary"),
                     uiOutput("info_accion2")
                   ),
                   conditionalPanel(
                     condition = "input.action == 'Editar pasajero'",
                     textInput("ape_mod", "Apellido del cliente: "),
                     selectInput("nombre_mod", "Nombre del cliente: ", choices = NULL),
                     uiOutput("info_id_pasajero"),
                     conditionalPanel(
                       condition="!is.na(input.ape_mod) && !is.na(input.nombre_mod)",
                       textInput("new_name", "Nombre del pasajero: "),
                       textInput("new_ape", "Apellido del pasajero: "),
                       selectInput("new_gender", "Género del pasajero: ",
                                   choices = c("Masculino", "Femenino", "Otro"),
                                   selected = "Otro"),
                       textInput("new_age", "Ingrese la edad del pasajero: ",value=0),
                       selectInput("new_nat", "Ingrese la nacionalidad del pasajero: ", choices = nationality$nationality),
                       actionButton("boton_guardar_cambios","GUARDAR CAMBIOS"),
                       uiOutput("info_accion_mod")
                     )
                     
                   )
          ),
          tabPanel("Registro de vuelo",
                   tabsetPanel(
                     id = "mytabs",
                     tabPanel("Vuelos de salida",
                              selectInput("destiny","Elegir el país de destino: ",
                                          choices=airports$`Country Name`[!duplicated(airports$`Airport Name`)]),
                              selectInput("aeropuertoz","Aeropuertos disponibles: ",
                                          choices= NULL),
                              selectInput("category","Seleccionar la categoría de asiento: ",
                                          choices=c("Ejecutiva","Económica","Primera clase","Económica Premium")),
                              dateInput("depDate","Seleccionar la fecha de salida: ",value = Sys.Date()),
                              uiOutput("precio_pasaje"),
                              actionButton("actionDeparture","Registrar salida",class = "btn-primary"),
                              uiOutput("descuento"),
                              uiOutput("info_accion3")
                     ),
                     tabPanel("Segundo vuelo",
                              selectInput("return5","Elegir el segundo país de destino: ",
                                          choices=airports[!duplicated(airports$`Country Name`),]$`Country Name`),
                              selectInput("aeropuertoz2","Aeropuertos disponibles: ",
                                          choices=NULL),
                              selectInput("category2","Seleccionar la categoría de asiento: ",
                                          choices=c("Ejecutiva","Económica","Primera clase","Económica Premium")),
                              dateInput("retDate","Seleccionar la fecha de salida del segundo vuelo: ", value=Sys.Date()),
                              uiOutput("precio_pasaje2"),
                              actionButton("actionReturn","Registrar regreso",class = "btn-primary"),
                              uiOutput("descuento2"),
                              uiOutput("info_accion4")
                     ),
                   ),
                   actionButton("reg_travel","Registrar vuelo",class="btn-primary")
          )
        )
      )
    )
  )
)
quitar_primer_caracter <- function(vector) {
  substr(vector, 2, nchar(vector))
  
}

server = function(input,output,session){
  costo_realR=0
  costo_realD=0
  costo2_ret=0
  costo2_dep=0
  output$nationality = renderTable(nationality)
  output$aircraft_type = renderTable(airplane)
  id_promotion=sample(promotion$`Promotion ID`,1)
  logged_in <- reactiveVal(FALSE)
  output$log=renderText("Introducir su usuario y contraseña...")
  observeEvent(input$login, {
    if (input$username == "test" && input$password == "pass") {
      output$login_info <- renderText("Login successful!")
      logged_in(TRUE)
      output$log =renderText("Usuario y contraseña correctos!")
    } else {
      output$login_info <- renderText("Wrong username or password. Please, try again.")
      logged_in(FALSE)
    }
  })
  output$logged_in <- renderPrint(logged_in())
  observe({
    if (!is.null(input$depDate) && !is.null(input$retDate) && input$retDate < input$depDate) {
      showModal(modalDialog(
        title = "Error",
        "La fecha de regreso no puede ser anterior a la fecha de salida."
      ))
    }
  })
  observe({
    if (!is.null(input$age) && !grepl("^\\d+$", input$age) || !is.null(input$new_age) && !grepl("^\\d+$", input$new_age)) {
      # Mensaje de error si la edad no es un número entero
      showModal(modalDialog(
        title = "Error",
        "La edad debe ser un número entero."
      ))
    }
  })
  observeEvent(input$actionR, {
    nombre=input$nombre
    ape=input$apellido
    if(!is.na(input$gender) && input$gender=="Masculino"){
      gen="Male"
    } else if(!is.na(input$gender) && input$gender =="Femenino"){
      gen="Female"
    } else if(!is.na(input$gender) && input$gender == "Otro"){
      gen="Other"
    }
    age=as.integer(input$age)
    nat= nationality$nat_cod[which(nationality$nationality == input$nat)]
    ult_id=as.integer(substring(passenger$`Passenger ID`[length(passenger$`Passenger ID`)],2,nchar(passenger$`Passenger ID`[length(passenger$`Passenger ID`)])))
    print(ult_id)
    ID=paste0("P",as.character(ult_id+1))
    if(!is.na(nombre) && !is.na(ape) && !is.na(gen) && !is.na(age) && !is.na(nat)){
      passenger <<- rbind(passenger,c(ID,nombre,ape,gen,age,nat))
      cat("\n====================PASAJERO AGREGADO====================")
      cat("\nID PASAJERO: ",ID)
      cat("\nNombre: ",nombre,ape)
      cat("\nGénero: ",gen)
      cat("\nEdad: ",age)
      cat("\nNacionalidad: ",nationality$nationality[nationality$nat_cod==nat])
      output$info_accion = renderText("CLIENTE AGREGADO!")
    }
  })
  observe({
    if (input$action == 'Eliminar pasajero') {
      selected_last_name <- input$ape
      names_for_last_name <- passenger$`First Name`[passenger$`Last Name` == selected_last_name]
      
      updateSelectInput(session, "nombre2", choices = names_for_last_name)
    }
  })
  observe({
    if (input$action == 'Editar pasajero') {
      selected_last_name_mod <- input$ape_mod
      names_for_last_name_mod <- passenger$`First Name`[passenger$`Last Name` == selected_last_name_mod]
      
      updateSelectInput(session, "nombre_mod", choices = names_for_last_name_mod)
    }
  })
  observe({
    if (input$action == 'Editar pasajero') {
      selected_last_name_mod2 <- input$ape_mod
      names_for_last_name_mod2 <- input$nombre_mod
      selected_id_to_edit <<- passenger$`Passenger ID`[passenger$`First Name` == names_for_last_name_mod2 & passenger$`Last Name` == selected_last_name_mod2]
      output$info_id_pasajero = renderText({
        paste("El ID del pasajero seleccionado es",selected_id_to_edit)
      })
    }
  })
  observeEvent(input$boton_guardar_cambios,{
    new_name=input$new_name
    new_ape=input$new_ape
    new_gen=input$new_gender
    new_age=input$new_age
    new_nat=input$new_nat
    index=which(passenger$`Passenger ID` == selected_id_to_edit)
    passenger[index,]<<-c(selected_id_to_edit,new_name,new_ape,new_gen,new_age,new_nat)
    output$info_accion_mod=renderText("DATOS DE PASAJERO MODIFICADOS!")
  })
  
  observeEvent(input$mytabs, {
    if (input$mytabs == "Registro de vuelo") {
      showModal(modalDialog(
        title = "Seleccionar Pasajero",
        textInput("last_name_travel", "Apellido del cliente:"),
        selectInput("first_name_travel", "Nombre del cliente:", choices = NULL),
        actionButton("btnIngresarPasajero", "Seleccionar Pasajero", class = "btn-primary"),
        footer = NULL
      ))
    }
  })
  observeEvent(input$last_name_travel, {
    selected_last_name <- input$last_name_travel
    names_for_last_name <- passenger$`First Name`[passenger$`Last Name` == selected_last_name]
    
    updateSelectInput(session, "first_name_travel", choices = names_for_last_name)
  })
  observeEvent(input$btnIngresarPasajero, {
    first_name_travel <<- input$first_name_travel
    last_name_travel <<- input$last_name_travel
    selected_id <<- passenger$`Passenger ID`[passenger$`First Name` == first_name_travel & passenger$`Last Name` == last_name_travel]
    
    removeModal()
  })
  observeEvent(input$actionR2, {
    selected_name <- input$nombre2
    selected_last_name <- input$ape
    id_to_remove <- passenger$`Passenger ID`[passenger$`First Name` == selected_name & passenger$`Last Name` == selected_last_name]
    passenger <<- subset(passenger, !(passenger$`First Name` == selected_name & passenger$`Last Name` == selected_last_name))
    output$info_accion2 = renderText(paste("CLIENTE ELIMINADO! ID:", id_to_remove))
  })
  observe({
    selected_country <- input$destiny
    categoria = input$category
    categoria2 = input$category2
    airports_for_country1 <- airports$`Airport ID`[airports$`Country Name` == selected_country]
    airports_for_country2 = airports_for_country1[airports_for_country1 %in% departure$`Airport ID`]
    airports_for_country3 = airports$`Airport Name`[airports$`Airport ID` %in% airports_for_country2]
    updateSelectInput(session, "aeropuertoz", choices = airports_for_country3)
    if (!is.na(categoria)) {
      if (categoria=="Ejecutiva") {
        costo_dep=4050
        output$precio_pasaje=renderText({
          paste("El precio del pasaje es de",costo_dep,"soles.")
        })
      } else if(categoria=="Económica"){
        costo_dep=2025
        output$precio_pasaje=renderText({
          paste("El precio del pasaje es de",costo_dep,"soles.")
        })
      } else if(categoria=="Primera clase"){
        costo_dep=7875
        output$precio_pasaje=renderText({
          paste("El precio del pasaje es de",costo_dep,"soles.")
        })
      } else{
        costo_dep=2700
        output$precio_pasaje=renderText({
          paste("El precio del pasaje es de",costo_dep,"soles.")
        })
      }
    }
    
  })
  observeEvent(input$actionDeparture, {
    selected_country <<- input$destiny
    airports_for_country <- input$aeropuertoz
    categoria = input$category
    if (!is.na(categoria)) {
      if (categoria=="Ejecutiva") {
        costo_dep=4050
        costo2_dep<<-runif(1,2500,3500)
      } else if(categoria=="Económica"){
        costo_dep=2025
        costo2_dep<<-runif(1,500,1500)
      } else if(categoria=="Primera clase"){
        costo_dep=7875
        costo2_dep<<-runif(1,5500,6500)
      } else{
        costo_dep=2700
        costo2_dep<<-runif(1,500,1500)
      }
    }
    id_airport <<- airports$`Airport ID`[airports$`Airport Name`==airports_for_country]
    descuento=as.numeric(promotion$Discount[promotion$`Promotion ID`==id_promotion])
    id_pilot=sample(pilot$`Pilot ID`,1)
    id_airplane=sample(airplane$`Airplane ID`,1)
    airline_not_duplicated=departure[!duplicated(departure$`Airline Name`),]$`Airline Name`
    airline_name=sample(airline_not_duplicated,1)
    descuento_soles=costo_dep*descuento
    costo_realD <<- costo_dep-descuento_soles
    promotion_type = promotion$Festivity[promotion$`Promotion ID`==id_promotion]
    id_departure=departure$`Departure ID`
    indices_departure=as.integer(quitar_primer_caracter(id_departure))
    id_departure2<<-paste0("I",as.character(max(indices_departure)+1))
    if(!is.na(selected_country) && !is.na(airports_for_country)){
      departure <<- rbind(departure,c(id_departure2,id_airport,id_pilot,id_airplane,airline_name,costo_realD,"On time",categoria))
      output$descuento = renderText({
        paste("Por",promotion_type,"se aplicará un descuento a su compra de",descuento_soles,"soles!!")
      })
      output$info_accion3 = renderText("Salida seleccionada!")
    }
    cat("\n===========================VUELO DE SALIDA REGISTRADO===========================")
    cat("\nID SALIDA: ",id_departure2)
    cat("\nPasajero: ",selected_id," ///////////"," Nombre: ",first_name_travel," ",last_name_travel)
    cat("\nID AEROPUERTO: ",id_airport," ///////////"," Aeropuerto: ",airports_for_country)
    cat("\nID PILOTO: ",id_pilot,"///////////"," Nombre: ",pilot$`Pilot Name`[pilot$`Pilot ID`==id_pilot])
    cat("\nID AERONAVE: ",id_airplane,"///////////"," Avión: ",airplane$`Airplane Type`[airplane$`Airplane ID`==id_airplane])
    cat("\nAEROLINEA: ",airline_name)
    cat("\nPRECIO: ",costo_realD)
    cat("\nDESCUENTO APLICADO: ",promotion_type)
    cat("\nEstado: On time")
    cat("\nCATEGORÍA: ",categoria)
    cat("\nFECHA DE SALIDA: ",as.character(input$depDate))
    
  })
  observe({
    selected_country2 <- input$return5
    categoria = input$category2
    airports_for_country1 <- airports$`Airport ID`[airports$`Country Name` == selected_country2]
    airports_for_country2 = airports_for_country1[airports_for_country1 %in% returns$`Airport ID`]
    airports_for_country3 = airports$`Airport Name`[airports$`Airport ID` %in% airports_for_country2]
    updateSelectInput(session, "aeropuertoz2", choices = airports_for_country3)
    if (!is.na(categoria)) {
      if (categoria=="Ejecutiva") {
        costo_dep=4050
        output$precio_pasaje2=renderText({
          paste("El precio del pasaje es de",costo_dep,"soles.")
        })
      } else if(categoria=="Económica"){
        costo_dep=2025
        output$precio_pasaje2=renderText({
          paste("El precio del pasaje es de",costo_dep,"soles.")
        })
      } else if(categoria=="Primera clase"){
        costo_dep=7875
        output$precio_pasaje2=renderText({
          paste("El precio del pasaje es de",costo_dep,"soles.")
        })
      } else{
        costo_dep=2700
        output$precio_pasaje2=renderText({
          paste("El precio del pasaje es de",costo_dep,"soles.")
        })
      }
    }
    
  })
  observeEvent(input$actionReturn, {
    selected_country2 <<- input$return5
    airports_for_country2 <- input$aeropuertoz2
    categoria2 = input$category2
    if (!is.na(categoria2)) {
      if (categoria2=="Ejecutiva") {
        costo_ret=4050
        costo2_ret<<-runif(1,2500,3500)
      } else if(categoria2=="Económica"){
        costo_ret=2025
        costo2_ret<<-runif(1,500,1500)
      } else if(categoria2=="Primera clase"){
        costo_ret=7875
        costo2_ret<<-runif(1,5500,6500)
      } else{
        costo_ret=2700
        costo2_ret<<-runif(1,500,1500)
      }
    }
    #airports[!duplicated(airports$`Country Name`),]$`Country Name`),
    id_airport2 <<- airports$`Airport ID`[airports$`Airport Name`==airports_for_country2]
    descuento=as.numeric(promotion$Discount[promotion$`Promotion ID`==id_promotion])
    id_pilot2<<-sample(pilot$`Pilot ID`,1)
    id_airplane2<<-sample(airplane$`Airplane ID`,1)
    airline_not_duplicated=returns[!duplicated(returns$`Airline Name`),]$`Airline Name`
    airline_name2<<-sample(airline_not_duplicated,1)
    descuento_soles=costo_ret*descuento
    costo_realR<<-costo_ret-descuento_soles
    promotion_type <<- promotion$Festivity[promotion$`Promotion ID`==id_promotion]
    id_return=returns$`Return ID`
    indices_return=as.integer(quitar_primer_caracter(id_return))
    id_return2<<-paste0("V",as.character(max(indices_return)+1))
    if(!is.na(selected_country) && !is.na(airports_for_country2)){
      returns <<- rbind(returns,c(id_return2,id_airport2,id_pilot2,id_airplane2,airline_name2,costo_realR,"On time",categoria2))
      output$descuento2 = renderText({
        paste("Por",promotion_type,"se aplicará un descuento a su compra de",descuento_soles,"soles!!")
      })
      output$info_accion4 = renderText("Salida seleccionada!")
    }
    id_travel=travels$`Travel ID`
    indices_travel=as.integer(quitar_primer_caracter(id_travel))
    id_travel2<<-paste0("T",as.character(max(indices_travel)+1))
    ticket_price <<- costo_realR + costo_realD
    ticket_cost <<- costo2_dep + costo2_ret
    cat("\n===========================VUELO DE REGRESO REGISTRADO===========================")
    cat("\nID REGRESO: ",id_return2)
    cat("\nPasajero: ",selected_id," ///////////"," Nombre: ",first_name_travel," ",last_name_travel)
    cat("\nID AEROPUERTO: ",id_airport2," ///////////"," Aeropuerto: ",airports_for_country2)
    cat("\nID PILOTO: ",id_pilot2,"///////////"," Nombre: ",pilot$`Pilot Name`[pilot$`Pilot ID`==id_pilot2])
    cat("\nID AERONAVE: ",id_airplane2,"///////////"," Avión: ",airplane$`Airplane Type`[airplane$`Airplane ID`==id_airplane2])
    cat("\nAEROLINEA: ",airline_name2)
    cat("\nPRECIO: ",costo_realR)
    cat("\nDESCUENTO APLICADO: ",promotion_type)
    cat("\nEstado: On time")
    cat("\nCategoría: ",categoria2)
    cat("\nFECHA DE SALIDA: ",as.character(input$retDate))
    cat("\n===========================UTILIDAD===========================")
    cat("\nPRECIO DEL PASAJE: ",ticket_price)
    cat("\nCOSTO DEL PASAJE: ",ticket_cost)
    cat("\nUTILIDAD: ",ticket_price-ticket_cost)
  })
  observeEvent(input$reg_travel,{
    travels <<- rbind(travels,c(id_travel2,selected_id,id_departure2,id_return2,id_promotion,ticket_price,ticket_cost,as.character(input$depDate),as.character(input$retDate),"SOLES"))
    cat("\n===========================VUELO REGISTRADO===========================")
    cat("\nTRAVEL ID: ",id_travel2)
    cat("\nPASSENGER ID: ",selected_id,"/////////// Nombre: ",passenger$`First Name`[passenger$`Passenger ID`==selected_id],passenger$`Last Name`[passenger$`Passenger ID`==selected_id])
    cat("\nDEPARTURE ID: ",id_departure2,"/////////// País de destino (Primer vuelo): ",selected_country)
    cat("\nRETURN ID: ",id_return2,"/////////// País de destino (Segundo vuelo): ",selected_country2)
    cat("\nPROMOTION ID: ",id_promotion,"/////////// Promoción otorgada: ",promotion_type)
    cat("\nTICKET PRICE: ",ticket_price)
    cat("\nTICKET COST: ",ticket_cost)
    cat("\nDEPARTURE DATE: ",as.character(input$depDate))
    cat("\nRETURN DATE: ",as.character(input$retDate))
    cat("\nCURRENCY: SOLES")
  })
  
}

shinyApp(ui,server)

