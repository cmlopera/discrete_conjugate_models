# Librerías necesarias ----

if(!require(pacman)) install.packages("pacman")
pacman::p_load("shiny","invgamma","ggplot2","plotly","gridExtra","shinythemes","readxl","readr","stringr","shinycssloaders","extraDistr","htmltools","shinycssloaders","shinyjs","shinyBS","shinyWidgets","bslib")


# Importar Funciones para gráficos ----

source("funciones_aux_graph.R")

# ------------------------------------------------------------------------------

# Interfaz de usuario ----
ui <- fluidPage(
  # Definir un estilo para modo claro
  theme = bs_theme(bootswatch = "cerulean"),
  
  tags$head(
    # Enlace CSS 
    tags$style(HTML(
      # Color para mensajes de validación 
      ".shiny-output-error-validation {
          color: red !important;}"
    )),
    
    #Dependencia para iconos de infoBox
    htmltools::htmlDependency(name = "font-awesome",
                              version = "5.15.4",
                              src = "fontawesome",
                              package = "fontawesome",
                              stylesheet = c("css/all.min.css", "css/v4-shims.min.css")),
    #Dependencia para bootstrap
    htmltools::htmlDependency(name  = "bootstrap",
                              version = "5.1.3",
                              src = c(href = "https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/"),
                              script = c("js/bootstrap.min.js", "js/bootstrap.bundle.min.js"),
                              stylesheet = c("css/bootstrap.min.css", "css/bootstrap-grid.min.css"))
  ),
  
  fluidRow(
    # Título de la aplicación
    column(10, titlePanel("Modelos discretos conjugados bayesianos")),
    # Botón para seleccionar tema
    column(2, align = "right",prettySwitch(inputId = "modo_oscuro",
                                           label = "Modo oscuro",
                                           value = FALSE,
                                           fill = TRUE, status = "primary"
    ))
  ),
  
  tags$br(),
  
  sidebarLayout(
    sidebarPanel(
      #Elección entre simulación e ingreso de datos
      radioButtons("esce_datos", label = NULL,
                   choices = list("Simular datos" = "sim_datos" , "Ingresar datos" = "ing_datos"),
                   selected = "sim_datos",
                   inline = TRUE
      ),
      
      #Elección del modelo conjugado a analizar
      selectInput(inputId = "modelo_conj",
                  "Escoge modelo conjugado",
                  selected = "",
                  choices = c("","Binomial","Poisson")
      ),
      
      #Ingreso del número de observaciones en el caso de simulación y cargue de datos en el caso de ingreso
      uiOutput("panel_esc"),
      
      #Botón para generar gráfico
      br(),
      div(bsButton(inputId="boton_graf", label="Generar gráficos", type = "action",style = "info",
                   title="Haga clic aquí para generar gráficos con distribución de verosimilitud, \n a priori y posterior para los datos ingresados"),
          align = "center"),
      
      tags$br(),
      
      #Información general
      h5("Integrantes:"),
      tags$ul(
        tags$li(tags$a(href="mailto:yyocampon@unal.edu.co", "Yeison Yovany Ocampo Naranjo")),
        tags$li(tags$a(href="mailto:xcastaneda@unal.edu.co", "Ximena Castañeda Ochoa")),
        tags$li(tags$a(href="mailto:yalcaraz@unal.edu.co", "Yojan Andrés Alcaraz Pérez"))
      ),
      h5("Profesores:"),
      tags$ul(
        tags$li(tags$a(href="mailto:iscramirezgu@unal.edu.co", "Isabel Cristina Ramírez Guevara")),
        tags$li(tags$a(href="mailto:cmlopera@unal.edu.co", "Carlos Mario Lopera Gómez"))
      ),
      h5("Correspondencia:"),
      tags$ul(
        tags$li(tags$a(href="mailto:iscramirezgu@unal.edu.co", "iscramirezgu@unal.edu.co"))
      ),
      div(
        style = "max-width: 100%; overflow: hidden; display: flex; align-items: center;", #; justify-content: space-between
        img(src = "Imagenes/logo_depto_estad.png", height = 120, width = 120, style = "max-width: 100%; height: auto;"),
        img(src = "Imagenes/logo_un_nv.png", height = 130, width = 210, style = "max-width: 100%; height: auto;"),
        style = "margin-top: 20px;"  # Agrega espacio vertical entre las imágenes
      )
    ),
    mainPanel(
      tabsetPanel(type = "pills",
                  #Sección de entradas y gráficos          
                  tabPanel(title = "Gráficos",
                           tags$br(),
                           #Apartado para entradas de parámetros       
                           fluidRow(
                             column(6,
                                    uiOutput("parametros_Apriori")),
                             column(6,
                                    conditionalPanel("!(input.esce_datos=='ing_datos' && input.modelo_conj=='Poisson')",
                                                     uiOutput("parametros_datos")))
                           ),
                           
                           #Apartado para mostrar gráficos  
                           fluidRow(
                             conditionalPanel(condition = "input.boton_graf > 0",
                                              withSpinner(plotlyOutput("distPlot")))),
                           
                           tags$br(),
                           
                           fluidRow(
                             column(6,
                                    conditionalPanel(condition = "input.boton_graf > 0",
                                                     withSpinner(plotlyOutput("distPlot2")))),
                             column(6,
                                    conditionalPanel(condition = "input.boton_graf > 0",
                                                     withSpinner(plotlyOutput("distPlot3"))))),
                           
                           #Apartado de resumen de las distribuciones
                           fluidRow(
                             column(12,
                                    tags$br(),
                                    conditionalPanel(condition = "input.boton_graf > 0",
                                                     withSpinner(uiOutput("info_dist"))),
                                    tags$br()))
                  ), 
                  
                  #Sección de teoría
                  tabPanel(title = "Teoría",
                           tags$br(),
                           tags$iframe(style = " height: 400px; width: 100%;",
                                       src="Explicacion_modelos.pdf")
                  ),
                  
                  # Sección de material de apoyo
                  tabPanel(title = "Material de apoyo",
                           tags$br(),
                           HTML("<p>A continuación se presenta una lista de videos que te ayudarán a entender mejor el funcionamiento
                             de los modelos conjugados discretos y la aplicación. Selecciona del menú desplegable el video que quieres reproducir:</p>"),
                           selectInput("video_selector", "",
                                       selected = "Presentación App Modelos Conjugados Bayesianos",
                                       choices = c("Presentación App Modelos Conjugados Bayesianos",
                                                   "Teoría: modelo binomial",
                                                   "Explicación: modelo binomial",
                                                   "Teoría: modelo Poisson",
                                                   "Explicación: modelo Poisson"
                                       )),
                           tags$br(),
                           # Espacio para visualizar video seleccionado 
                           div(style = "text-align: center;",
                               uiOutput("video_player"))
                  )
      ) 
    )
  )
)


# Lógica del servidor ----
server <- function(input, output,session) {
  useShinyjs()
  options(
    #Tamaño máximo permitido del archivo - 10MB
    shiny.maxRequestSize = 10 * 1024^2
  )
  
  #Variable que almacena el modelo previamente analizado
  modelo_anterior <- reactiveVal("")
  
  # Cambio de tema claro - oscuro
  observeEvent(input$modo_oscuro, {
    if (isTRUE(input$modo_oscuro)) {
      session$setCurrentTheme(bs_theme(bootswatch = "darkly",
                                       primary = "#00BC8C",
                                       info = "#00BC8C"))
    } else {
      session$setCurrentTheme(bs_theme(bootswatch = "cerulean",
                                       primary = "#033C73"))
    }
  })
  
  #Separación de escenario
  observeEvent(input$esce_datos,{
    
    #VALIDACIÓN: eliminar elección de selectInputs cuando se cambia de simular a ingreso de datos y viceversa
    updateSelectInput(session, "modelo_conj", selected = " ")
    
    #Escenario de simulación de datos
    if(input$esce_datos== "sim_datos"){
      
      #Entrada para ingresar el número de observaciones
      output$panel_esc<- renderUI({
        inputWithTooltip("input_numObservaciones", "Ingrese la cantidad de observaciones", 300, 2, "El número de observaciones debe ser mayor que dos")
      })   
      
      #Generación de inputs para parámetros de verosimilitud según sea el caso
      output$parametros_datos <- renderUI({
        if(input$modelo_conj == "Poisson"){
          fluidRow(
            h5("Distribución de verosimilitud",style='text-align: center;'),
            HTML("<p style='text-align: center;'><strong>&Upsilon;&sim;Poisson(&theta;)</strong></p>"),
            column(12,
                   h6("Estimación parámetro de interés"),
                   inputWithTooltip("theta", HTML("Ingrese &theta;"), 10, 0, HTML("El parámetro &theta; debe ser mayor que cero"))
            ))
        }else if(input$modelo_conj == "Binomial"){
          fluidRow(
            h5("Distribución de verosimilitud",style='text-align: center;'),
            HTML("<p style='text-align: center;'><strong>&Upsilon;&sim;Binom(n,&theta;)</strong></p>"),
            column(12,
                   h6(HTML("Valores necesarios para estimar parámetro &theta;")),
                   inputWithTooltip("num_ensayos", "Ingrese número de ensayos", 10, 1, "El número de ensayos debe ser positivo"),
                   inputWithTooltip("num_exitos", "Ingrese número de éxitos", 4, 0, "El número de éxitos debe ser positivo")
            ))
        }
      })
      
      #Escenario de ingreso de datos    
    }else if(input$esce_datos == "ing_datos"){
      
      #Cargue de la base de datos
      output$panel_esc<- renderUI({
        fluidRow(
          column(12,
                 fileInput("datos", label = "Seleccione el archivo",
                           accept = c(".csv",".xlsx",".txt"),
                           buttonLabel = "Importar", placeholder = NULL),
                 #SelectInput para mostrar las columnas de la base de datos cargada
                 selectInput("nom_var", "Columna", choices = "No hay columnas disponibles"),
                 downloadButton("descargarDatos", "Datos de ejemplo")
                 
          )
        )
      })
      
      #Ingreso de parámetros conocidos sobre la distribución de verosimilitud binomial
      output$parametros_datos <- renderUI({
        if(input$modelo_conj == "Binomial"){
          fluidRow(
            h5("Distribución de verosimilitud",style='text-align: center;'),
            HTML("<p style='text-align: center;'><strong>&Upsilon;&sim;Binom(n,&theta;)</strong></p>"),
            column(12,
                   h6(HTML("Valor necesario para estimar parámetro &theta;")),
                   inputWithTooltip("num_ensayos", "Ingrese número de ensayos", 10, 1, "Se ingresa el número de ensayos correspondiente a los datos seleccionados. El número de ensayos debe ser positivo")
            ))
        }
      })
      
      #Cargue de datos de ejemplo
      datosEjemplo <- read.csv("data_simulation.csv")
      
      #Función para descargar archivo con datos de ejemplo al oprimir el botón "descargarDatos"
      output$descargarDatos <- downloadHandler(
        filename = "DatosEjemplo.csv",
        content = function(file) {
          write.csv(datosEjemplo, file, row.names = FALSE)
        })
    }
  })
  
  #Entradas para distribuciones a priori
  output$parametros_Apriori <- renderUI({
    if(input$modelo_conj == "Poisson"){
      fluidRow(
        h5("Distribución a priori",style = "text-align: center;"),
        HTML("<p style='text-align: center;'><strong>&theta;&sim;Gamma(&alpha;,&beta;)</strong></p>"),
        column(6,
               h6("Parámetro de forma"),
               inputWithTooltip("Alpha_pois", HTML("Ingrese  &alpha;"), 1, 0, HTML("El parámetro &alpha; debe ser mayor que cero"))
        ),
        column(6,
               h6("Parámetro escala inversa"),
               inputWithTooltip("Beta_pois", HTML("Ingrese  &beta;"), 2, 0, HTML("El parámetro &beta; debe ser mayor que cero"))
        ))
    }else if(input$modelo_conj == "Binomial"){
      fluidRow(
        h5("Distribución a priori",style='text-align: center;'),
        HTML("<p style='text-align: center;'><strong>&theta;&sim;Beta(&alpha;,&beta;)</strong></p>"),
        column(12,
               h6("Parámetros de forma"),
               inputWithTooltip("Alpha_bin", HTML("Ingrese  &alpha;"), 1, 0, HTML("El parámetro &alpha; debe ser mayor que cero")),
               inputWithTooltip("Beta_bin", HTML("Ingrese  &beta;"), 1, 0, HTML("El parámetro &beta; debe ser mayor que cero"))
               
        )
      )
    }
  })
  
  #Leer y guardar datos
  Base_datos <- eventReactive(input$datos,{
    #Verificar que se haya cargado un archivo de datos 
    validate(need(input$datos, "Por favor ingrese un conjunto de datos"))
    bd <- NULL
    #Verificación de cargue de formato correcto
    if(grepl(".csv",input$datos$name) | grepl(".txt",input$datos$name) | grepl(".xlsx",input$datos$name)){
      #Leer datos de acuerdo al tipo de formato
      if(grepl(".xlsx",input$datos$name)){
        bd <- read_excel(input$datos$datapath,col_names = TRUE)
      }else if(grepl(".csv",input$datos$name) | grepl(".txt",input$datos$name)){
        #Definir e identificar el delimitador
        archivo <- readLines(input$datos$datapath,warn=FALSE)
        delimitador <- sort(table(stringr::str_extract(archivo, ",|;|\t")), decreasing = TRUE)[1]
        
        #Identificar el número de columnas
        num_columnas <- count.fields(input$datos$datapath, sep = names(delimitador))
        max_columnas <- max(num_columnas)
        tipo_columnas <- paste(rep("n",max_columnas), collapse = "")
        
        #Leer la base de datos
        bd <- read_delim(input$datos$datapath,
                         delim = names(delimitador),
                         col_names = TRUE,
                         col_types = tipo_columnas)
      }
      
      #Verificar encabezado
      if (any(grepl("^[0-9]+$", names(bd)))) {
        #Si el archivo no tiene encabezado asignar por defecto un nombre a cada columna
        registro_1 <- colnames(bd)
        names(bd) <- paste0("Columna ", 1:ncol(bd))
        bd <- rbind(bd,as.numeric(registro_1))
      }
      return(bd)
    }else{
      #Caso de formato inválido
      showModal(modalDialog(title = "Advertencia", "Formato inválido"))
      return(bd)
    }
  })
  
  #Listar columnas disponibles cuando se haya leído un marco de datos
  observeEvent(Base_datos(),{
    choices <- c("Seleccione una columna",names(Base_datos()))
    updateSelectInput(session, "nom_var", choices =  c("Seleccione una columna",choices))
  })
  
  #Vector para guardar el valor obtenido de los parámetros simulados o ingresados
  values <- reactiveValues(parametros = NULL)
  
  #Obtener gráficos
  observeEvent(c(input$boton_graf,input$modelo_conj,input$esce_normal_med,input$esce_normal_var,input$inde_aprioris,input$nom_var),{
    #Variable para comparar modelo analizado antes y después
    valor_actual <- input$modelo_conj
    
    # VALIDACIÓN: Eliminar selección de columna y borrar valores de los parámetros cuando se haya cambiado de modelo
    if(modelo_anterior() != "" && modelo_anterior() != valor_actual){
      updateSelectInput(session, "nom_var", selected = "Seleccione una columna")
      values$parametros <- NULL
    }
    
    #Escenario de simulación
    if(input$esce_datos== "sim_datos"){
      output$distPlot<- renderPlotly({
        if(input$modelo_conj == "Poisson"){
          #Validación
          if(input$esce_datos == "sim_datos"){
            validate(
              need(input$theta != "", "Ingresa un valor válido para θ"),
              need(input$theta > 0, "Recuerda que el parámetro θ debe ser mayor que cero")
            )
          }
          validate(
            need(input$input_numObservaciones != "", "Ingresa un valor válido para la cantidad de observaciones"),
            need(input$input_numObservaciones > 2, "Recuerda que la cantidad de observaciones debe ser mayor que dos"),
            need(input$Alpha_pois != "", "Ingresa un valor válido para α"),
            need(input$Alpha_pois > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_pois != "", "Ingresa un valor válido para β"),
            need(input$Beta_pois > 0, "Recuerda que el parámetro β debe ser mayor que cero")
          )
          #Gráfico
          g1 <- fx_pois(nobs=input$input_numObservaciones,theta_m=input$theta,
                        alpha_0=input$Alpha_pois,beta_0=input$Beta_pois,
                        escenario = input$esce_datos)
          config(ggplotly(g1[[1]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("y: %{x:.4f}<br>",
                                               "p(y|\u03b8): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
          
        }else if(input$modelo_conj == "Binomial"){
          #Validación
          if(input$esce_datos == "sim_datos"){
            validate(
              need(input$num_exitos != "", "Ingresa un valor válido para el número de éxitos"),
              need(input$num_exitos > 0, "Recuerda que el número de éxitos debe ser mayor que cero"),
              need(input$num_exitos <= input$num_ensayos, "Recuerda que el número de éxitos debe ser menor o igual que el número de ensayos")
            )
          }
          validate(
            need(input$input_numObservaciones != "", "Ingresa un valor válido para la cantidad de observaciones"),
            need(input$input_numObservaciones > 2, "Recuerda que la cantidad de observaciones debe ser mayor que dos"),
            need(input$Alpha_bin != "", "Ingresa un valor válido para α"),
            need(input$Alpha_bin > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_bin != "", "Ingresa un valor válido para β"),
            need(input$Beta_bin > 0, "Recuerda que el parámetro β debe ser mayor que cero"),
            need(input$num_ensayos != "", "Ingresa un valor válido para el número de ensayos"),
            need(input$num_ensayos > 0, "Recuerda que el número de ensayos debe ser mayor que cero")
          )
          #Gráfico
          g1 <- fy_bin(nobs=input$input_numObservaciones,n_ensayos=input$num_ensayos,a=input$Alpha_bin,b=input$Beta_bin,
                       y=input$num_exitos,escenario=input$esce_datos)
          config(ggplotly(g1[[1]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("y: %{x:.4f}<br>",
                                               "p(y|\u03b8): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
        }
      })
      
      output$distPlot2 <- renderPlotly({
        if(input$modelo_conj == "Poisson"){
          #Validación
          if(input$esce_datos == "sim_datos"){
            validate(
              need(input$theta != "", "Ingresa un valor válido para θ"),
              need(input$theta > 0, "Recuerda que el parámetro θ debe ser mayor que cero")
            )
          }
          validate(
            need(input$input_numObservaciones != "", "Ingresa un valor válido para la cantidad de observaciones"),
            need(input$input_numObservaciones > 2, "Recuerda que la cantidad de observaciones debe ser mayor que dos"),
            need(input$Alpha_pois != "", "Ingresa un valor válido para α"),
            need(input$Alpha_pois > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_pois != "", "Ingresa un valor válido para β"),
            need(input$Beta_pois > 0, "Recuerda que el parámetro β debe ser mayor que cero")
          )
          #Gráfico
          g2 <- fx_pois(nobs=input$input_numObservaciones,theta_m=input$theta,
                        alpha_0=input$Alpha_pois,beta_0=input$Beta_pois,escenario=input$esce_datos)
          config(ggplotly(g2[[2]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("\u03b8: %{x:.4f}<br>",
                                               "p(\u03b8): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
          
        }else if(input$modelo_conj == "Binomial"){
          #Validación
          if(input$esce_datos == "sim_datos"){
            validate(
              need(input$num_exitos != "", "Ingresa un valor válido para el número de éxitos"),
              need(input$num_exitos > 0, "Recuerda que el número de éxitos debe ser mayor que cero"),
              need(input$num_exitos <= input$num_ensayos, "Recuerda que el número de éxitos debe ser menor o igual que el número de ensayos")
            )
          }
          validate(
            need(input$input_numObservaciones != "", "Ingresa un valor válido para la cantidad de observaciones"),
            need(input$input_numObservaciones > 2, "Recuerda que la cantidad de observaciones debe ser mayor que dos"),
            need(input$Alpha_bin != "", "Ingresa un valor válido para α"),
            need(input$Alpha_bin > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_bin != "", "Ingresa un valor válido para β"),
            need(input$Beta_bin > 0, "Recuerda que el parámetro β debe ser mayor que cero"),
            need(input$num_ensayos != "", "Ingresa un valor válido para el número de ensayos"),
            need(input$num_ensayos > 0, "Recuerda que el número de ensayos debe ser mayor que cero")
          )
          #Gráfico
          g2 <- fy_bin(nobs=input$input_numObservaciones,n_ensayos=input$num_ensayos,a=input$Alpha_bin,b=input$Beta_bin,
                       y=input$num_exitos,escenario=input$esce_datos)
          config(ggplotly(g2[[2]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("\u03b8: %{x:.4f}<br>",
                                               "p(\u03b8): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
        }
      })
      
      output$distPlot3 <- renderPlotly({
        if(input$modelo_conj == "Poisson"){
          #Validación
          if(input$esce_datos == "sim_datos"){
            validate(
              need(input$theta != "" , "Ingresa un valor válido para θ"),
              need(input$theta > 0 , "Recuerda que el parámetro θ debe ser mayor que cero")
            )
          }
          validate(
            need(input$input_numObservaciones != "", "Ingresa un valor válido para la cantidad de observaciones"),
            need(input$input_numObservaciones > 2, "Recuerda que la cantidad de observaciones debe ser mayor que dos"),
            need(input$Alpha_pois != "", "Ingresa un valor válido para α"),
            need(input$Alpha_pois > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_pois != "", "Ingresa un valor válido para β"),
            need(input$Beta_pois > 0, "Recuerda que el parámetro β debe ser mayor que cero")
          )
          #Gráfico
          g3 <- fx_pois(nobs=input$input_numObservaciones,theta_m=input$theta,alpha_0=input$Alpha_pois,
                        beta_0=input$Beta_pois,escenario=input$esce_datos)
          values$parametros <- g3[[4]]
          config(ggplotly(g3[[3]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("\u03b8: %{x:.4f}<br>",
                                               "p(\u03b8|y): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
          
        }else if(input$modelo_conj == "Binomial"){
          #Validación
          if(input$esce_datos == "sim_datos"){
            validate(
              need(input$num_exitos != "", "Ingresa un valor válido para el número de éxitos"),
              need(input$num_exitos > 0, "Recuerda que el número de éxitos debe ser mayor que cero"),
              need(input$num_exitos <= input$num_ensayos, "Recuerda que el número de éxitos debe ser menor o igual que el número de ensayos")
            )
          }
          validate(
            need(input$input_numObservaciones != "", "Ingresa un valor válido para la cantidad de observaciones"),
            need(input$input_numObservaciones > 2, "Recuerda que la cantidad de observaciones debe ser mayor que dos"),
            need(input$Alpha_bin != "", "Ingresa un valor válido para α"),
            need(input$Alpha_bin > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_bin != "", "Ingresa un valor válido para β"),
            need(input$Beta_bin > 0, "Recuerda que el parámetro β debe ser mayor que cero"),
            need(input$num_ensayos != "", "Ingresa un valor válido para el número de ensayos"),
            need(input$num_ensayos > 0, "Recuerda que el número de ensayos debe ser mayor que cero")
          )
          #Gráfico
          g3 <- fy_bin(nobs=input$input_numObservaciones,n_ensayos=input$num_ensayos, a=input$Alpha_bin, b=input$Beta_bin, y=input$num_exitos,escenario = input$esce_datos)
          values$parametros <- g3[[4]]
          config(ggplotly(g3[[3]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("\u03b8: %{x:.4f}<br>",
                                               "p(\u03b8|y): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
        }
      })
      
      #Escenario de ingreso   
    }else if(input$esce_datos == "ing_datos"){
      # VALIDACIÓN: Verificar que no se grafique sin seleccionar previamente una columna
      if(input$nom_var == "Seleccione una columna" || input$nom_var == "No hay columnas disponibles" || is.null(Base_datos())){
        values$parametros <- NULL
        return()
      }
      
      datos1 <- Base_datos()[[input$nom_var]] #Columna de la base de datos seleccionada
      mean_datos1 <- mean(datos1) #Media de los datos. Necesaria para modelo binomial
      modelo_anterior(input$modelo_conj) #Guardar modelo que analizo
      
      output$distPlot<- renderPlotly({
        # VALIDACIÓN: Verificar que no se grafique sin seleccionar previamente una columna
        if(input$esce_datos == "sim_datos" || input$nom_var == "Seleccione una columna" || input$nom_var == "No hay columnas disponibles"){
          values$parametros <- NULL
          return()
        }
        
        if(input$modelo_conj == "Poisson"){
          #Validación
          validate(
            need(input$datos, "Por favor ingrese un conjunto de datos"),
            need(input$Alpha_pois != "", "Ingresa un valor válido para α"),
            need(input$Alpha_pois > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_pois != "", "Ingresa un valor válido para β"),
            need(input$Beta_pois > 0, "Recuerda que el parámetro β debe ser mayor que cero")
          )
          #Gráfico
          g1 <- fx_pois(alpha_0=input$Alpha_pois,beta_0=input$Beta_pois,escenario=input$esce_datos,
                        datos = datos1)
          values$parametros <- g1[[4]]
          
          config(ggplotly(g1[[1]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("y: %{x:.4f}<br>",
                                               "p(y|\u03b8): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
        }else if(input$modelo_conj == "Binomial"){
          #Validación
          validate(
            need(!is.null(input$nom_var), "Por favor seleccione una columna de la base de datos"),
            need(input$nom_var != "Seleccione una columna", "Por favor seleccione una columna de la base de datos"),
            need(input$Alpha_bin != "", "Ingresa un valor válido para α"),
            need(input$Alpha_bin > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_bin != "", "Ingresa un valor válido para β"),
            need(input$Beta_bin > 0, "Recuerda que el parámetro β debe ser mayor que cero"),
            need(input$num_ensayos != "", "Ingresa un valor válido para el número de ensayos"),
            need(input$num_ensayos > 0, "Recuerda que el número de ensayos debe ser mayor que cero"),
            need(input$num_ensayos > mean_datos1, paste0("Recuerda que el número de ensayos debe ser mayor al número de éxitos promedio: ", round(mean_datos1), ". \nPara mayor claridad revisa los videos de explicación en la sección 'Material de apoyo'."))
          )
          #Gráfico
          g1 <- fy_bin(datos = datos1,n_ensayos=input$num_ensayos,
                       a=input$Alpha_bin,b=input$Beta_bin,escenario=input$esce_datos)
          values$parametros <- g1[[4]]
          
          config(ggplotly(g1[[1]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("y: %{x:.4f}<br>",
                                               "p(y|\u03b8): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
        }
      })
      
      output$distPlot2 <- renderPlotly({
        # VALIDACIÓN: Verificar que no se realice un gráfico sin seleccionar previamente una columna
        if(input$esce_datos == "sim_datos" || input$nom_var == "Seleccione una columna" || input$nom_var == "No hay columnas disponibles"){
          values$parametros <- NULL
          return()
        }
        
        if(input$modelo_conj == "Poisson"){
          #Validación
          validate(
            need(input$Alpha_pois != "", "Ingresa un valor válido para α"),
            need(input$Alpha_pois > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_pois != "", "Ingresa un valor válido para β"),
            need(input$Beta_pois > 0, "Recuerda que el parámetro β debe ser mayor que cero")
          )
          #Gráfico
          g2 <- fx_pois(alpha_0=input$Alpha_pois,beta_0=input$Beta_pois,escenario=input$esce_datos,
                        datos = datos1)
          config(ggplotly(g2[[2]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("\u03b8: %{x:.4f}<br>",
                                               "p(\u03b8): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
        }else if(input$modelo_conj == "Binomial"){
          validate(
            need(!is.null(input$nom_var), "Por favor seleccione una columna de la base de datos"),
            need(input$nom_var != "Seleccione una columna", "Por favor seleccione una columna de la base de datos"),
            need(input$nom_var != "...1" , "Por favor seleccione una columna de la base de datos"),
            need(input$Alpha_bin != "", "Ingresa un valor válido para α"),
            need(input$Alpha_bin > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_bin != "", "Ingresa un valor válido para β"),
            need(input$Beta_bin > 0, "Recuerda que el parámetro β debe ser mayor que cero"),
            need(input$num_ensayos != "", "Ingresa un valor válido para el número de ensayos"),
            need(input$num_ensayos > mean_datos1, paste0("Recuerda que el número de ensayos debe ser mayor al número de éxitos promedio: ", round(mean_datos1),". \nPara mayor claridad revisa los videos de explicación en la sección 'Material de apoyo'."))
          )
          #Gráfico
          g2 <- fy_bin(datos=datos1,n_ensayos=input$num_ensayos,
                       a=input$Alpha_bin,b=input$Beta_bin,escenario=input$esce_datos)
          config(ggplotly(g2[[2]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("\u03b8: %{x:.4f}<br>",
                                               "p(\u03b8): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
        }
      })
      
      output$distPlot3 <- renderPlotly({
        # VALIDACIÓN: verificar que no grafique sin que se haya seleccionado una columna
        if(input$esce_datos == "sim_datos" || input$nom_var == "Seleccione una columna" || input$nom_var == "No hay columnas disponibles"){
          values$parametros <- NULL
          return()
        }
        
        if(input$modelo_conj == "Poisson"){
          #Validación
          validate(
            need(input$Alpha_pois != "", "Ingresa un valor válido para α"),
            need(input$Alpha_pois > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_pois != "", "Ingresa un valor válido para β"),
            need(input$Beta_pois > 0, "Recuerda que el parámetro β debe ser mayor que cero")
          )
          #Gráfico
          g3 <- fx_pois(alpha_0=input$Alpha_pois,beta_0=input$Beta_pois,
                        escenario=input$esce_datos,datos=datos1)
          config(ggplotly(g3[[3]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("\u03b8: %{x:.4f}<br>",
                                               "p(\u03b8|y): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
          
        }else if(input$modelo_conj == "Binomial"){
          validate(
            need(!is.null(input$nom_var), "Por favor seleccione una columna de la base de datos"),
            need(input$nom_var != "Seleccione una columna", "Por favor seleccione una columna de la base de datos"),
            need(input$nom_var != "...1" , "Por favor seleccione una columna de la base de datos"),
            need(input$Alpha_bin != "", "Ingresa un valor válido para α"),
            need(input$Alpha_bin > 0, "Recuerda que el parámetro α debe ser mayor que cero"),
            need(input$Beta_bin != "", "Ingresa un valor válido para β"),
            need(input$Beta_bin > 0, "Recuerda que el parámetro β debe ser mayor que cero"),
            need(input$num_ensayos != "", "Ingresa un valor válido para el número de ensayos"),
            need(input$num_ensayos > mean_datos1, paste0("Recuerda que el número de ensayos debe ser mayor al número de éxitos promedio: ", round(mean_datos1),". \nPara mayor claridad revisa los videos de explicación en la sección 'Material de apoyo'."))
          )
          #Gráfico
          g3 <- fy_bin(datos = datos1,n_ensayos=input$num_ensayos,
                       a=input$Alpha_bin,b=input$Beta_bin,escenario=input$esce_datos)
          config(ggplotly(g3[[3]],dynamicTicks = TRUE)%>%
                   style(hoverlabel = list(bgcolor = 4,
                                           font = list(color = "black")),
                         hovertemplate = paste("\u03b8: %{x:.4f}<br>",
                                               "p(\u03b8|y): %{y:.4f}<br>",
                                               "<extra></extra>")),
                 modeBarButtonsToAdd = list('drawline',
                                            'drawopenpath',
                                            'drawclosedpath',
                                            'drawcircle',
                                            'drawrect',
                                            'eraseshape')) %>%
            layout(margin = list(pad = 20))
        }
      })
    }
    
    # Creación de cuadros informativos sobre las distribuciones y notas aclaratorias
    output$info_dist <- renderUI({
      if(input$modelo_conj == "Poisson"){
        fluidRow(
          summaryBox("Verosimilitud", HTML(paste0("<p style = 'font-size: 14.5px;'>&Upsilon;&sim;Poisson(",values$parametros[1],")</p>")), width = 4, icon = "fas fa-area-chart", style = "info"),
          summaryBox("A priori", HTML(paste0("<p style = 'font-size: 14.5px;'>&theta;&sim;Gamma(",values$parametros[2],",",values$parametros[3],")</p>")), width = 4, icon = "fas fa-area-chart", style = "danger"),
          summaryBox("Posterior", HTML(paste0("<p style = 'font-size: 14.5px;'>&theta;|&Upsilon;&sim;Gamma(",values$parametros[4],",",values$parametros[5],")</p>")), width = 4, icon = "fas fa-area-chart", style = "success")
        )
      }
      else if(input$modelo_conj == "Binomial"){
        fluidRow(
          summaryBox("Verosimilitud", HTML(paste0("<p style = 'font-size: 14.5px;'>&Upsilon;&sim;Binom(",values$parametros[1],",",values$parametros[2],")</p>")), width = 4, icon = "fas fa-area-chart", style = "info"),
          summaryBox("A priori", HTML(paste0("<p style = 'font-size: 14.5px;'>&theta;&sim;Beta(",values$parametros[3],",",values$parametros[4],")</p>")), width = 4, icon = "fas fa-area-chart", style = "danger"),
          summaryBox("Posterior", HTML(paste0("<p style = 'font-size: 14.5px;'>&theta;|&Upsilon;&sim;Beta(",values$parametros[5],",",values$parametros[6],")</p>")), width = 4, icon = "fas fa-area-chart", style = "success")
        )
      }
    })
  })
  
  #Vector para almacenar URLs de los videos explicativos.
  video_urls <- c(
    "https://ciencias.medellin.unal.edu.co/escuelas/estadistica/images/videos-Shiny/video1.mp4",
    "https://ciencias.medellin.unal.edu.co/escuelas/estadistica/images/videos-Shiny/video10.mp4",
    "https://ciencias.medellin.unal.edu.co/escuelas/estadistica/images/videos-Shiny/video11.mp4",
    "https://ciencias.medellin.unal.edu.co/escuelas/estadistica/images/videos-Shiny/video12.mp4",
    "https://ciencias.medellin.unal.edu.co/escuelas/estadistica/images/videos-Shiny/video13.mp4"
  )
  
  #Elección del video de acuerdo a selectInput.
  output$video_player <- renderUI({
    selected_video_url <- switch(input$video_selector,
                                 "Presentación App Modelos Conjugados Bayesianos" = video_urls[1],
                                 "Teoría: modelo binomial"= video_urls[2],
                                 "Explicación: modelo binomial"= video_urls[3],
                                 "Teoría: modelo Poisson"= video_urls[4],
                                 "Explicación: modelo Poisson"= video_urls[5])
    
    tags$iframe(src = selected_video_url, width = "640", height = "360", frameborder = "0",allowfullscreen = TRUE)
  })
}

# Ejecutar aplicación ----
shinyApp(ui = ui, server = server)
