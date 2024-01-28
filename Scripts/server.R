library(shiny)
library(readxl)
library(lubridate)
library(tidyr)
library(openxlsx)
library(dplyr)
library(DT) 

server <- function(input, output) {
  
  observeEvent(input$file, {
    # Check if a file has been uploaded
    if (!is.null(input$file)) {
      # Save the file to the current working directory
    
      file_path <- file.path(getwd(), input$file$datapath)
      
      # Call the extract_data.R script for processing with the file path as an argument
      source("extract_data.R", local = TRUE, chdir = TRUE, keep.source = TRUE, echo = TRUE, print.eval = TRUE)
      
      output$sujet_table <- DT::renderDataTable({
        sujet_data <- read.xlsx(file.path("../Data", "sujet.xlsx"), sheet = 1)
        
        # Essayez de convertir une seule colonne pour tester
        sujet_data <- sujet_data %>% mutate(across(-Sujet, ~as.Date(., origin = "1899-12-30")))
        
        
        return(sujet_data)
      }, rownames = TRUE)
      
      # Repeat the same for other datasets if they also contain date columns
      # Replace the everything() with the actual columns if not all are dates
      output$anthropometriques_table <- DT::renderDataTable({read.xlsx(file.path("../Data", "anthropometriques.xlsx"), sheet = 1)}, rownames = TRUE)
      output$performance_table <- DT::renderDataTable({read.xlsx(file.path("../Data", "performance.xlsx"), sheet = 1)}, rownames = TRUE)
      output$serum_chemistry_blood_table <- DT::renderDataTable({read.xlsx(file.path("../Data", "serum_chemistry_blood.xlsx"), sheet = 1)}, rownames = TRUE)
      output$whole_blood_analysis_table <- DT::renderDataTable({read.xlsx(file.path("../Data", "whole_blood_analysis.xlsx"), sheet = 1)}, rownames = TRUE)
      output$hematologie_iron_table <- DT::renderDataTable({read.xlsx(file.path("../Data", "hematologie_iron.xlsx"), sheet = 1)}, rownames = TRUE)
      output$hormes_table <- DT::renderDataTable({read.xlsx(file.path("../Data", "hormes.xlsx"), sheet = 1)}, rownames = TRUE)
      output$vitamin_table <- DT::renderDataTable({read.xlsx(file.path("../Data", "vitamin.xlsx"), sheet = 1)}, rownames = TRUE)
      
      
      #Exemple de comment accéder aux données
      
      # Lire le fichier Excel sans spécifier 'rownames = TRUE' car ce n'est pas une option valide pour read.xlsx
      test <- read.xlsx(file.path("../Data", "anthropometriques.xlsx"), sheet = 1)
      # Supposons que la première colonne de votre fichier Excel doit devenir les noms de lignes du dataframe en R
      rownames(test) <- test[, 1]  # Définir la première colonne comme noms de lignes
      # Ensuite, vous pouvez supprimer cette première colonne du dataframe si nécessaire
      test <- test[, -1]  # Supprime la première colon
      print(head(test))
      
    }
  })
}
