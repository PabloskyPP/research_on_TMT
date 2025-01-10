# research_on_TMT
This repository contains the data and resources used in a own reseach on the Theory of Terror Managment and Social Networks.

# Structure of the repository:
research_on_TMT/

└── README.md # Here it is explained the documents and info collected in each repository file.

└── artículo  # Here it is attached the article compilling all the reseach.

└── analisis_de_datos/
   
   ├── datos_analisis_individuales/   # Contiene los archivos .txt de cada participante.
   
   ├── datos_total_muestral/          # Contiene 3 archivos .csv con los resultados totales de los participantes en cada una de las tres tareas experimentales realizadas: 
                                      # tarea Stroop, tarea de Difusión de imágenes, y Cuestionario de Autoestima.
  
   ├── script_analisis_muestra/       # Contiene un script de R mediante el que se implementan diversas técnicas estadísticas en los datos del total muestral.
  
   └── script_analisis_individuales/  # Contiene dos scripts de python mediante los cuales primero: obtener la matriz de datos individuales en la tarea Stroop en formato .csv
                                      # y segundo: realizar una prueba t para cada matriz de los sujetos en condición experimental (Stroop71) .csv, y devolver una tabla común con sus valores y significancia. 
    
└──LICENSE.md  # Contains info about the license aplying this repository data
