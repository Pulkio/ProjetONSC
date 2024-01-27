library(shiny)
library(readxl)
library(lubridate)
library(tidyr)
library(openxlsx)

output_dir <- "../Data"

server <- function(input, output) {
  
  observeEvent(input$file, {
    # Vérifie si un fichier a été téléchargé
    if (!is.null(input$file)) {
      # Enregistrez le fichier dans le répertoire de travail actuel
      file_path <- file.path(getwd(), input$file$datapath)
      
      # Appel du script extract_data.R pour le traitement avec le chemin du fichier comme argument
      source("extract_data.R", local = TRUE, chdir = TRUE, keep.source = TRUE, echo = TRUE, print.eval = TRUE)
      
      # Afficher les résultats dans les onglets correspondants
      output$sujet_table <- renderTable({ sujet }, rownames = TRUE)
      output$anthropometriques_table <- renderTable({ anthropometriques }, rownames = TRUE)
      output$performance_table <- renderTable({ performance }, rownames = TRUE)
      output$serum_chemistry_blood_table <- renderTable({ serum_chemistry_blood }, rownames = TRUE)
      output$whole_blood_analysis_table <- renderTable({ whole_blood_analysis }, rownames = TRUE)
      output$hematologie_iron_table <- renderTable({ hematologie_iron }, rownames = TRUE)
      output$hormes_table <- renderTable({ hormes }, rownames = TRUE)
      output$vitamin_table <- renderTable({ vitamin }, rownames = TRUE)
      
    }
  })
}
