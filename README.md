# Aplicacion-web-desarrollada-en-Shiny-para-analisis-transcriptomico
# Carga de los paquetes necesarios para la aplicacción
library(shiny)
library(ShortRead)
library(Biostrings)
library(shinydashboard)
library(shinyFiles)
library(dplyr)
library(tidyverse) #libreria necesaria para poder separar cada read del paired-end
# Título de la página principal
header <- dashboardHeader(title = "PROYECTO FINAL DE GRADO", titleWidth = 1550) 

# Crear los menus de la aplicación, con iconos visuales que seran vistos en la pestaña principal del usuario  

  sidebar <- dashboardSidebar(
  sidebarMenu(id ="tabs",
              menuItem("Introducción", tabName = "Intro", icon = icon("edit")),
              menuItem("Tipo de Análisis", tabName = "Selección", icon = icon("check-circle")),
              menuItem("Cargar archivos", tabName = "Cargar", icon = icon("dna")),
              menuItem("Control de Calidad", tabName = "Quality_Control", icon = icon("check")),
              menuItem("Preprocesamiento", tabName = "Preproccesing", icon = icon("cogs")),
              menuItem("Carga secuencia de referencia", tabName = "Referencia", icon = icon("dna")),
              menuItem("Alineamiento", tabName = "Alineamiento", icon = icon("arrows-h")),
              menuItem("SAM", tabName = "sam", icon = icon("exchange-alt")),
              menuItem("Matriz de Conteo", tabName = "MatrizConteo", icon = icon("table"))
              
  ))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Intro", 
            h1("Introducción"),
            br(),
            p("Esta aplicación esta realizada en R + Shiny con el objetivo de crear una aplicación user-friendy para los usuaios."),
            p("Permite realizar el preprocesamiento de ficheros derivados de técnicas de secuenciación masiva, específicamente RNA-seq, para obtener finalmente los archivos de su                   procesamiento en formato BAM y realizar la matriz de conteo para su posterior análisis."),
            p("Para su utilización es necesario tener instalados en su terminal de MacOS o en ubuntu los siguientes software:"),
            tags$ol(
              tags$li("FastQC ->  Para realizar el control de calidad de las secuencias."),
              tags$li("Trimmomatic -> Para preprocesar las secuencias antes de su alineamiento."),
              tags$li("Bowtie -> Software para el alineamiento de las secuencias respecto al genoma de referencia"),
              tags$li("SAMtools -> Para interactuar con los archivos de las secuencias en formato SAM y BAM"),
              tags$li("featureCounts -> Permite realizar la matriz de conteo de los archivos BAM ordenados e indexados"),
            ),
            br(),
            
            h2("¿Cómo usar la aplicación?"),
            p("La aplicación cuenta con nueve pestañas en el lateral izquierdo. Se debe empezar por la primera y seguir en orden, completando toda la información requerida en cada                 ventana. Las ventanas son: Introducción, Tipo de Análisis, Cargar archivos, Control de Calidad, Preprocesamiento, Carga secuencia de referencia, Alineamiento, SAM y Matriz             de Conteo."),
            br(),
            
            h3("Tipo de análisis"),
            p("Inicialmente, es necesario elegir si se desea realizar el análisis mediante Single-end o Paired-end."),
            p("Single-end corresponde que la secuenciación se realizó solo teniendo en cuenta un extremo, mientas que paired-end significa que se ha secuenciado por ambos extremos."),
            p("Si quiere realzar un análisis paired-end, tiene que cambiar el nombre de los archivos, de tal forma que si quiere analizar un archivo llamado NOMBRE_ARCHIVO (que es el               mismo para ambas lecturas) y posee ambos pared de lecturas, tienes que denominarlos como: NOMBRE_ARCHIVO_rep1_read1 y NOMBRE_ARCHIVO_rep1_read2."),
            
            h3("Cargar archivos"),
            p("Seleccione el directorio donde se ubica el archivo `carpetaPFG´, que contiene todos los script necesarios para ejecutar la aplicación, así como las carperas donde se                 almacenaran todos los resultados."),
            p("Posteriormente, suba los input en: ‘Selecciona los archivos en formato fastq que quieres analizar'. Estos archivos son los generadas por el secuenciador."),
            p("Al cargar las secuencias, seleccione el botón de Iniciar lectura de secuencias."),
            
            h3("Control de calidad"),
            p("Presione el botón que indica `Iniciar Control de Calidad´"),
            p("Una vez finalizado el control de calidad de todas las secuencias, puede acceder a los resultados y ver el informe de control de calidad generado para cada secuencia                 introducida  en `carpetaPFG/resultados/rsultados_control_calidad´."),
            p("Además, puede visualizar el resultado seleccionando el botón `Visualizar resultado del Control de Calidad´."),
            p("Al darle al botón se generan tantos botones como archivos se han introducido en la aplicación por el usuario y al teclear el botón sale el informe del control de                     calidad del archivo correspondiente a cada secuencia."),
            
            h3("Preprocesamiento"),
            p("Dependiendo de la longitud del tamaño de los fragmentos generados por el secuenciados, seleccione la longitud mínima que quiere usar para el preprocesamiento."),
            p("Para ello ajuste la barra con el tamaño que desea. Debe estar comprendido entre 0-300."),
            p("Los resultados del preprocesamiento se encuentran en: 'carpetaPFG/resultados/resultados_preprocessing'."),
            
            h3("Carga secuencia de referencia"),
            p("Carga la secuencia de referencia, para realizar el alineamiento con un genoma de referencia."),
            p("Se generara un índice a partir del archivo de referencia subido para realizar el posterior alineamiento de los archivos."),
            
            h3("Alineamiento"),
            p("Presione 'Realizar el alineamiento'."),
            p("Este proceso puede ser el más prolongado. La aplicación realiza el alineamiento usando el software Bowtie. Se le notificara una vez finalice el alineamiento de todas                 las secuencias."),
            p("Los resultados se encuentran en 'carpetaPFG/resultados/resultados_alineamiento'."),
            
            h3("SAM"),
            p("Presione al botón de 'Transformar archivos SAM a BAM'."),
            p("Se generarán archivos BAM ordenador e indexados de cada archivo resultante del alineamiento."),
            p("Los resultados estarán disponibles en `carpetaPFG/resultados/resultados_alineamiento'."),
            
            h3("Matriz de Conteo"),
            p("Carga el archivo GTF que se utilizara para calcular la matriz de conteo"),
            p("Recuerde que los archivos GTF y el de referencia usado para generar el índice tienen que pertenecer al mismo organismo."),
            p("Presione al botón de 'Realizar matriz de conteo'."),
            p("Se generarán la matriz de conteo de los archivos BAM ordenador e indexados."),
            p("Los resultados estarán disponibles en `carpetaPFG/resultados/resultados_alineamiento'.")
    ),
    
    tabItem(tabName = "Selección", 
            fluidRow(
              column(6,
                     radioButtons("formato", "Seleccione el tipo de análisis que quiere realizar:",
                                  choices = list("Single-End" = "single", "Paired-End" = "paired"))
              )
            )
    ),
    tabItem(tabName = "Cargar", 
            fluidRow(
              column(6,
                     box(
                       title = "Selecciona el directorio donde se encuentra carpetaPFG",
                       width = 12,
                       shinyDirButton('directorio', 'Selecciona un directorio', TRUE),
                       textOutput("folder_path")
                     )
              )
            ),
            fluidRow(
              column(6,
                     fileInput("fastq_files", "Selecciona los archivos en formato fastq que quieres analizar", multiple = TRUE),
                     actionButton("nombres", "Iniciar lectura de secuencias")
              )
            )
    ),
    tabItem(tabName = "Quality_Control",
            actionButton("start_qc", "Iniciar Control de Calidad"),
            hr(),
            actionButton("refresh", "Visualizar resultado del Control de Calidad"),
            uiOutput("file_buttons"),
            htmlOutput("html_view")
    ),
    
    tabItem(tabName = "Preproccesing", 
            fluidPage(
              sliderInput("mLen","Selecciona el valor de mLen que quiere usar en el preprocesamiento, entre 0 y 300", min = 0, max = 300, value = 150 ),
              verbatimTextOutput("Preprocessing")),
            actionButton("Preproccesing", "Realizar preprocesamiento con trimmomatic"),
            hr(),
            actionButton("preprocesamiento_qc", "Iniciar Control de Calidad de las secuencias preprocesadas"),
            hr(),
            actionButton("preprocesamiento_visualizacion", "Visualizar resultado del Control de Calidad de las secuencias preprocesadas"),
            uiOutput("file_botones"),
            htmlOutput("html_Vista")
    ),
    
    tabItem(tabName = "Referencia",
            fluidRow(
              column(6,
                     fileInput("target_sequence", "Selecciona y carga la secuencia de referencia para crear su índice", multiple = TRUE)
              )
            )
    ),
    
    
    tabItem(tabName = "Alineamiento", 
            actionButton("Alinear", "Realizar el alineamiento")),
    
    tabItem(tabName = "sam", 
            actionButton("Sam", "Transformar archivos SAM a BAM")),
    
    tabItem(tabName = "MatrizConteo", 
            fluidPage(
              fileInput("GTF", "Selecciona y carga el archivo GTF"),
              actionButton("matriz", "Realizar matriz de conteo")
            ))
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "green") #skin permite seleccionar el color de la aplicación

# En esta parte del código se realiza el servidor
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 100*1590^1200) # Aumento el tamaño de memoria que va a usar la aplicación, para poder cargar todas las secuencias y realizar el análisis
  
  # Configura el directorio raíz
  shinyDirChoose(input, 'directorio', roots=c(wd='.'))
  
  file_names <- list()  # Lista para almacenar los nombres de los archivos
  
  rv <- reactiveValues() # Define la variable reactiva 'folder_path'
  
  #Mostrar la ruta del directorio seleccionado por el usuario
  output$folder_path <- renderText({ 
    if (is.null(input$directorio)) {
      return("No se ha seleccionado ningún directorio. Por favor introduzca el directorio.")
    } else {
      folder_path <- parseDirPath(c(wd='.'), input$directorio) 
      #mostrar la ruta del directorio seleccionado
      paste("La ruta del directorio seleccionado es:", folder_path) 
      # Se actualiza la variable reactiva con el valor correcto
      rv$folder_path <- folder_path  
    }
  })
  #para manejar los archivos
  observeEvent(input$nombres,  {
    if(input$formato == "single") {
      #req se usa para verificar que hay archivos
      req(input$fastq_files)
      #Se calcula el número de archivos que el usuario ha subido para poder recorrerlos
      num_files <- length(input$fastq_files$datapath)
      #Se crea la ruta a la carpeta secuencias, donde se almacenaran los archivos
      dir_create <- paste0(rv$folder_path, "/", "secuencias")
      for (i in seq_len(num_files)) {
        # Se usan los nombres originales de los archivos para mantener los nombres al guardar
        fastq_filepath <- input$fastq_files$name[i]
        
        res <- file.copy(from = input$fastq_files$datapath[i], to = paste0(dir_create, "/", fastq_filepath))
        if (!res) {
          message("No se pudo copiar el archivo ", input$fastq_files$datapath[i], " a ", paste0(dir_create, "/", fastq_filepath))
        }
      }
      
      #Lista los archivos en el directorio
      files <- list.files(dir_create, full.names = TRUE)
      file_names <- basename(files)
      # Se define el patrones
      patron <- ".fq"
      
      #Recorre los archivos y se cambian los nombres de los archivos según los patrones
      for(file in files) {
        new_name <- gsub(pattern=patron,
                         replacement = "",
                         x = basename(file))
        file.rename(from = file, to = file.path(dir_create, new_name))
      }
    }
    
    #si el tipo de análisis seleccionado es paired-end
    else { 
      req(input$fastq_files)
      
      num_files <- length(input$fastq_files$datapath)
      dir_create <- paste0(rv$folder_path, "/", "secuencias")
      if (!dir.exists(dir_create)) {
        dir.create(dir_create)
      }
      for (i in seq_len(num_files)) {
        fastq_filepath <- input$fastq_files$name[i]
        res <- file.copy(from = input$fastq_files$datapath[i], to = paste0(dir_create, "/", fastq_filepath))
        if (!res) {
          message("No se pudo copiar el archivo ", input$fastq_files$datapath[i], " a ", paste0(dir_create, "/", fastq_filepath))
        }
      }
      
      #para poder leer el contenido de cada archivo
      leer_contenido_archivo <- function(file_name) {
        #indica la ruta al archivo
        file_path <- file.path(dir_path, file_name)
        
        #Leer el archivo FASTQ
        contenido <- readFastq(file_path)
        
        #Devolver el contenido leído del archivo 
        return(paste(sread(contenido), collapse=""))
      }
      
      #Ruta al directorio donde se encuentran los archivos
      dir_path <- file.path(rv$folder_path, "/", "secuencias")
      
      #Lee todos los nombres de los archivos en el directorio que acaban en .fq
      all_files <- list.files(path = dir_path, pattern = "\\.fq$")
      
      #Separar cada parte de los nombres de los archivos cuando encuentra _read_
      files_df <- data.frame(filename = all_files) %>%
        separate(filename, c("base", "read"), sep = "_read_") %>%
        separate(base, c("base", "rep"), sep = "rep_", extra = "merge") %>%
        mutate(rep = str_replace(rep, "^0*", ""))
      
      #Crea una columna que indica a que par pertenece cada archivo
      files_df <- files_df %>%
        mutate(pair = ifelse(read == "1", paste0(base, "_rep_", rep), NA)) %>%
        fill(pair)
      
      #Almacenar el nombre del archivo y lee el contenido de cada archivo en una nueva columna llamada "contenido"
      files_df <- files_df %>%
        mutate(filename = paste(base, "rep_", rep, "_read_", read, sep = "")) %>%
        mutate(contenido = map_chr(filename, leer_contenido_archivo))
      
      #Ordenar el DataFrame por 'base', 'rep' numéricamente y 'read'
      files_df <- files_df %>%
        arrange(base, as.numeric(rep), read)
      
    }
  })
  # Control de Calidad
  #Función que tiene como objetivo buscar archivos html
  getHtmlFiles <- function(path) {
    list.files(path, pattern = "\\.html$", full.names = TRUE)
  }
  
  html_content <- reactiveVal() #Se crea una variable reactiva para almacenar archivos HTML que pueden variar 
  #Al presionar el botón "Iniciar Control de Calidad", se ejecuta el siguiente observeEvent
  observeEvent(input$start_qc,  { #para realizar el control de calidad, no importa si es single o paired end
    #Obtenemos la ruta al directorio de secuencias, utiizando la variable reactiva rv$folder_path
    secuencias_dir <- paste0(rv$folder_path, "/", "secuencias")
    
    #Se obtiene una lista completa de los archivos en el directorio de secuencias
    fastq_files <- list.files(secuencias_dir, full.names = TRUE)
    
    for (i in seq_along(fastq_files)) {
      fastq_filepath <- fastq_files[i]
      
      #Ruta del script que hace el control de calidad
      bash_script <- file.path(rv$folder_path, "script/ControlCalidad.sh")
      resultados_control_calidad <- file.path(rv$folder_path, "resultados/resultados_control_calidad")
      #Pasar el archivo FastQ y el archivo de secuencia  como argumento al script de Bash
      command <- paste("bash", bash_script, fastq_filepath,resultados_control_calidad, replace = TRUE)
      
      #Ejecutar el script de Control de Calidad utilizando el sistema operativo 
      ControlCalidad <- system(command, inter=TRUE)
    }
    
    showModal(modalDialog( #Muestra una pestaña avisando de que se ha acabado el Control de Calidad
      title = "Control de Calidad",
      "El control de calidad se ha completado.",
      easyClose = TRUE,
      footer = NULL
    ))
    
    observeEvent(input$refresh, {
      path_to_folder <- file.path(rv$folder_path, "resultados/resultados_control_calidad")
      files <- getHtmlFiles(path_to_folder)
      
      # Crea botones para cada archivo y lo nombra cada botón como el nombre del archivo usando basename
      output$file_buttons <- renderUI({
        lapply(1:length(files), function(i) {
          actionButton(paste0("btn_", i), basename(files[i]))
        })
      })
    })
    
    # Observa cada botón y, cuando se hace clic en uno, almacena su contenido en la variable reactiva
    observe({
      path_to_folder <- file.path(rv$folder_path, "resultados/resultados_control_calidad")
      files <- getHtmlFiles(path_to_folder)
      
      for (i in 1:length(files)) {
        btn_name <- paste0("btn_", i)
        if (!is.null(input[[btn_name]]) && input[[btn_name]] > 0) {
          content <- readChar(files[i], file.info(files[i])$size)
          html_content(content)  # Almacenar el contenido en la variable reactiva
        }
      }
    })
    
    # Renderizar el contenido HTML usando la variable reactiva y mostrar los archivos HTML correspondiente
    output$html_view <- renderUI({
      HTML(html_content())
    })
    
  })
  
  
  
  # Preprocesamiento
  observeEvent(input$Preproccesing, {
    
    # Se obtiene la ruta al directorio de secuencias
    secuencias_dir <- paste0(rv$folder_path, "/", "secuencias")
    # Se obtiene la ruta a los adaptadores y archivos trimmomatic 
    trimmomatic_path <- file.path(rv$folder_path, "Trimmomatic-0.39/trimmomatic-0.39.jar")
    adapter_path <- file.path(rv$folder_path, "Trimmomatic-0.39/adapters/TruSeq3-SE.fa")
    # Se listan los archivos en el directorio de secuencias
    fastq_files <- list.files(secuencias_dir, full.names = TRUE)
    req(fastq_files)
    #Se utiliza sliderInput para que el usuario seleccione la mínima longitud
    if(input$formato == "single") {
      sliderInput("mLen","Selecciona el valor de mLen que quiere usar en el preprocesamiento, entre 0 y 250", min = 0, max = 250, value = 100 )
      # Se obtiene la ruta al script de trimmomatic usado para el preprocesamiento Single-end
      bash_script_trim <- file.path(rv$folder_path, "script/trimmomatic.sh")
      
      # Loop para procesar todos los archivos .fq introducidos por el usuario
      for (i in seq_along(fastq_files)) {
        fastq_filepath <- fastq_files[i]
        
        # Dentro del bucle
        #Se almacenan los resultados en la carpeta resultados_preprocessing
        resultados_preprocessing <- file.path(rv$folder_path, "resultados/resultados_preprocessing")
        #se guardan los resultados con el nombre del archivo seguido de _trimmed.fastq
        output_filepath <- file.path(resultados_preprocessing, paste0(basename(fastq_filepath), "_trimmed.fastq"))
        #Se ejecuta el comando
        command <- paste("bash", bash_script_trim, fastq_filepath, output_filepath, input$mLen, trimmomatic_path, adapter_path)
        
        trim_result <- system(command)
      }
      showModal(modalDialog(
        title = "Preprocesamiento",
        "El preprocesamiento de todas las secuencias se ha completado.",
        easyClose = TRUE,
        footer = NULL
      ))
      
    }
    else #Paired-end
    {
      leer_contenido_archivo <- function(file_name) {
        # Construir la ruta completa al archivo
        file_path <- file.path(dir_path, file_name)
        
        # Leer el archivo FASTQ
        contenido <- readFastq(file_path)
        
        # Devolver el contenido leído como una cadena de caracteres
        return(paste(sread(contenido), collapse=""))
      }
      
      # Se especifica la ruta al directorio donde se encuentran los archivos
      dir_path <- paste0(rv$folder_path, "/", "secuencias")
      
      # Se leen todos los nombres de los archivos del directorio
      all_files <- list.files(path = dir_path, pattern = "\\.fq$")
      
      # Se separa cada parte de los nombres de los archivos para rellenar las columnas del DataFrame
      files_df <- data.frame(filename = all_files) %>%
        separate(filename, c("base", "read"), sep = "_read_") %>%
        separate(base, c("base", "rep"), sep = "rep_", extra = "merge") %>%
        mutate(rep = str_replace(rep, "^0*", ""))
      
      # Se crea una columna que indica a que par pertenece cada archivo
      files_df <- files_df %>%
        mutate(pair = ifelse(read == "1", paste0(base, "_rep_", rep), NA)) %>%
        fill(pair)
      
      # Se lee el contenido de cada archivo en una nueva columna llamada contenido
      files_df <- files_df %>%
        mutate(filename = paste(base, "rep_", rep, "_read_", read, sep = "")) %>%
        mutate(contenido = map_chr(filename, leer_contenido_archivo))
      
      # Ordenar el DataFrame por 'base', 'rep'  y 'read'
      files_df <- files_df %>%
        arrange(base, as.numeric(rep), read)
      
      trimmomatic_path <- file.path(rv$folder_path, "Trimmomatic-0.39/trimmomatic-0.39.jar")
      adapter_path <- file.path(rv$folder_path, "Trimmomatic-0.39/adapters/TruSeq3-PE.fa")
      #Se establece la ruta al script de preprocesamiento de trimmomatic
      bash_script_trim_PE <- file.path(rv$folder_path, "script/PE_trimmomatic.sh")
      
      resultados_preprocessing <- file.path(rv$folder_path, "resultados/resultados_preprocessing")
      #Se procesan los archivos de dos en dos, debido a que en paired-end se leen por ambos extremos(2 lecturas)
      #Como estan ordenados los archivos previamente, cada dos archivos corresponde a la misma lectura paired end
      for (i in seq(1, nrow(files_df), by = 2)) {
        # Se obtiene cada archivo
        input_file1 <- file.path(dir_path, files_df$filename[i])
        input_file2 <- file.path(dir_path, files_df$filename[i+1])
        
        # Se define los nombres de los archivos de salida
        output_file1 <- file.path(resultados_preprocessing, paste0(basename(input_file1), "_trimmed.fastq"))
        output_file2 <- file.path(resultados_preprocessing, paste0(basename(input_file2), "_trimmed.fastq"))
        
        # Construir el comando bash como una cadena y ejecutarlo con system()
        command_trim <- paste("bash", bash_script_trim_PE, input_file1, input_file2, output_file1, output_file2, input$mLen, trimmomatic_path, adapter_path)
        
        # Ejecuta el comando
        system(command_trim)
      }
      showModal(modalDialog(
        title = "Preprocesamiento",
        "El preprocesamiento de todas las secuencias se ha completado.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    # Realizar el control de calidad de los archivos preprocesados
    observeEvent(input$preprocesamiento_qc,  {
      preprocesing_dir <- paste0(rv$folder_path, "/", "resultados/resultados_preprocessing")
      
      # Se obtiene una lista completa de los archivos en el directorio de secuencias
      fastq_files <- list.files(preprocesing_dir, pattern = "\\.fastq$", full.names = TRUE)
      
      for (i in seq_along(fastq_files)) {
        fastq_filepath <- fastq_files[i]
        
        #Ruta del script que hace el control de calidad
        bash_script <- file.path(rv$folder_path, "script/ControlCalidad.sh")
        resultados_control_calidad <- file.path(rv$folder_path, "resultados/resultados_preprocessing/resultados_control_calidad")
        # Pasar el archivo FastQ y el archivo de secuencia  como argumento al script de Bash
        command <- paste("bash", bash_script, fastq_filepath,resultados_control_calidad, replace = TRUE)
        
        #Ejecutar el script de Control de Calidad utilizando el sistema operativo 
        ControlCalidad <- system(command, inter=TRUE)
      }
      showModal(modalDialog(
        title = "Control de Calidad",
        "El control de calidad de las secuencias preprocesadas se ha completado.",
        easyClose = TRUE,
        footer = NULL
      ))
      #Mostrar el Control de calidad de los archivos preprocesados igual que en la ventana control de calidad
      observeEvent(input$preprocesamiento_visualizacion, {
        path_to_folder <- file.path(rv$folder_path, "/resultados/resultados_preprocessing/resultados_control_calidad/")
        files <- getHtmlFiles(path_to_folder)
        
        # Crea botones para cada archivo y lo nombra cada botón como el nombre del archivo
        output$file_botones <- renderUI({
          lapply(1:length(files), function(i) {
            actionButton(paste0("btn_", i), basename(files[i]))
          })
        })
      })
      
      observe({
        path_to_folder <- file.path(rv$folder_path, "resultados/resultados_preprocessing/resultados_control_calidad")
        files <- getHtmlFiles(path_to_folder)
        
        for (i in 1:length(files)) {
          btn_name <- paste0("btn_", i)
          if (!is.null(input[[btn_name]]) && input[[btn_name]] > 0) {
            content <- readChar(files[i], file.info(files[i])$size)
            html_content(content)  # Almacenar el contenido en la variable reactiva
          }
        }
      })
      
      #Renderizar el contenido HTML usando la variable reactiva
      output$html_Vista <- renderUI({
        HTML(html_content())
      })
      
    })
    
  })
  # Cargar la secuencia de referencia
  observeEvent(input$Referencia, {
    target_sequence_filepath <- input$target_sequence$datapath
    
  })
  
  
# Alineamiento
  observeEvent(input$Alinear, {
    if(input$formato == "single") {
      #Se almacenan losresultados en la carpeta 'resultados_alineamiento'
      resultados_alineamiento <- file.path(rv$folder_path, "resultados/resultados_alineamiento")
      #Se calcula la ruta al script bowtie para el análisis single-end
      bash_script_bowtie  <- file.path(rv$folder_path, "script/bowtie.sh")
      
      #Cargar secuencia de referencia para realizar el índice
      target_sequence_filepath <- input$target_sequence$datapath
      #Se crea una lista con todos los archivos preprocesados anteriormente, dichos archivos acaban en trimmed.fastq
      fastq_files <- list.files(path = file.path(rv$folder_path, "resultados/resultados_preprocessing/"), pattern = "_trimmed.fastq$", full.names = TRUE)
      # Asegurarse de que los archivos necesarios están presentes
      req(fastq_files, input$target_sequence) 
      #Se crea la ruta al script que genera el índice y se ejecuta, 'almacenándolo en resultados_alineamiento'
      bash_script_indice  <- file.path(rv$folder_path, "script/indice.sh")
      indice <- paste("bash", bash_script_indice, target_sequence_filepath, resultados_alineamiento)
      system(indice)
      #Se recorren todos los archivos preprocesados para alinearlos usando Bowtie
      for (i in seq_along(fastq_files)) {
        fastq_filepath <- fastq_files[i]
        # Dentro del bucle
        #Los archivos alineados acabaran en _aligned.sam
        sam_filepath <- file.path(resultados_alineamiento, paste0(basename(fastq_filepath), "_aligned.sam"))
        command <- paste("bash", bash_script_bowtie, fastq_filepath, resultados_alineamiento, i)
        bowtie_result <- system(command)
      }
      showModal(modalDialog(
        title = "Alineamiento",
        "El alineamiento se ha completado correctamente.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
    else #paired-end
    {
      
      resultados_alineamiento <- file.path(rv$folder_path, "resultados/resultados_alineamiento")
      #Se calcula la ruta al script de bowtie diseñado para archivos paired-end
      bash_script_bowtie_PE  <- file.path(rv$folder_path, "script/PE_bowtie.sh")
      target_sequence_filepath <- input$target_sequence$datapath
      fastq_files <- list.files(path = file.path(rv$folder_path, "resultados/resultados_preprocessing/"), pattern = "_trimmed.fastq$", full.names = TRUE)
      req(fastq_files, input$target_sequence) 
      bash_script_indice  <- file.path(rv$folder_path, "script/indice.sh")
      indice <- paste("bash", bash_script_indice, target_sequence_filepath, resultados_alineamiento)
      system(indice)
      req(input$fastq_files)
      #Se crea un DataFrame para almacenar las secuencias generadas en el preprocesamiento y saber cual pertenece a cada extremo de cada análisis
      leer_contenido_archivo <- function(file_name) {
        # Construir la ruta completa al archivo
        file_path <- file.path(alineamiento_path, file_name)
        
        # Leer el archivo FASTQ
        contenido <- readFastq(file_path)
        
        # Devolver el contenido leído como una cadena de caracteres
        return(paste(sread(contenido), collapse=""))
      }
      alineamiento_path <- file.path(rv$folder_path, "resultados/resultados_preprocessing")
      
      #Se leen los nombres de los archivos en el directorio que han sido preprocesados
      all_files <- list.files(path = alineamiento_path, pattern = "_trimmed.fastq$")
      
      alineamiento_df <- data.frame(filename = all_files) %>%
        separate(filename, c("base", "read"), sep = "_read_") %>%
        separate(base, c("base", "rep"), sep = "rep_", extra = "merge") %>%
        mutate(rep = str_replace(rep, "^0*", ""))
      
      # Se crea una columna que indica a que par pertenece cada archivo
      alineamiento_df <-  alineamiento_df %>%
        mutate(pair = ifelse(read == "1", paste0(base, "_rep_", rep), NA)) %>%
        fill(pair)
      
      alineamiento_df <-  alineamiento_df %>%
        mutate(filename = paste(base, "rep_", rep, "_read_", read, sep = "")) %>%
        mutate(contenido = map_chr(filename, leer_contenido_archivo))
      alineamiento_df$filename
      
      # Ordenar el DataFrame por 'base', 'rep'  y 'read'
      alineamiento_df <-  alineamiento_df %>%
        arrange(base, as.numeric(rep), read)
      bash_script_bowtie_PE <- file.path(rv$folder_path, "script/PE_bowtie.sh")
      
      resultados_alineamiento <- file.path(rv$folder_path, "resultados/resultados_alineamiento")
      #Como en el preprocessing, se recorren los archivos previamente ordenados de dos en dos
      for (i in seq(1, nrow(alineamiento_df), by = 2)) {
        # Se obtienen los archivos para el par actual
        input_file1 <- file.path(alineamiento_path, alineamiento_df$filename[i])
        input_file2 <- file.path(alineamiento_path, alineamiento_df$filename[i+1])
        
        # Define los nombres de los archivos de salida
        output_file <-  file.path(rv$folder_path, "resultados/resultados_alineamiento")
        
        command_bowtie <- paste("bash", bash_script_bowtie_PE, input_file1,input_file2,output_file)
        
        # Ejecuta los comandos
        system(command_bowtie)
      }
      showModal(modalDialog(
        title = "Alineamiento",
        "El alineamiento se ha completado correctamente.",
        easyClose = TRUE,
        footer = NULL
        
        
      ))
    }
  })
  # Trasformar archivos SAM a BAM
  observeEvent(input$Sam, {
    #ruta a la carpeta donde estan todos los archivos sam
    sam_path<- file.path(rv$folder_path, "resultados/resultados_alineamiento")
    
    #obtener una lista para guardar todos los archivos que acaban en .sam
    files_sam <- list.files(path = sam_path, pattern = ".sam$", full.names = TRUE)
    
    #Se obtiene la ruta al script que transforma los archivos SAM a BAM, los ordena e indexa
    script_bam <- file.path(rv$folder_path, "script/bamscript.sh")
    
    #bucle que recorra la lista y  ejecutar el script a cada archivo
    for (i in seq_along(files_sam)){
      #obtener el nombre del archivo .sam actual
      sam_filepath <- files_sam[i] 
      # Define la ruta del archivo .bam de salida, reemplazando ".sam" con ".bam"
      bam_filepath <- gsub(pattern = ".sam$", replacement = ".bam", x = sam_filepath)
      #Ejecutar el script
      comman_samtools <- paste("bash", script_bam,sam_filepath, bam_filepath)
      bam_results <- system(comman_samtools)
    }
    
    showModal(modalDialog(
      title = "Transformar archivos de SAM a BAM",
      "La transformación de todos archivos SAM a BAM ha finalizado",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  # Matriz de Conteo
  observeEvent(input$matriz, {
    #ruta a la carpeta donde estan todos los archivos bam
    bam_path<- file.path(rv$folder_path, "resultados/resultados_alineamiento")
    
    #obtener una lista para guardar todos los archivos que acaban en .bam
    files_bam <- list.files(path = bam_path, pattern = ".bam$", full.names = TRUE)
    
    #Se obtiene la ruta al script que realiza la matriz de conteo de todos los archivos BAM usando featureCounts 
    script_featureCounts <- file.path(rv$folder_path, "script/featureCounts.sh")
    
    # Obtener el archivo GTF cargado por el usuario
    uploaded_gtf <- input$GTF$datapath
    
    #Bucle que recorre la lista para ejecutar el script featureCounts a cada archivo
    for (i in seq_along(files_bam)){
      #obtener el nombre del archivo .bam actual
      bam_filepath <- files_bam[i] 
      # Define la ruta del archivo .txt 
      txt_filepath <- file.path(rv$folder_path, "resultados/resultados_alineamiento")
      #Ejecuta el script
      comman_featureCounts <- paste("bash", script_featureCounts,bam_filepath, txt_filepath, uploaded_gtf)
      txt_results <- system(comman_featureCounts)
    }
    
    showModal(modalDialog(
      title = "Matriz de conteo",
      "La generación de la matriz de conteo de todos los archivos ha finalizado",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
}


# Ejecutar la aplicación
shinyApp(ui = ui, server = server)


