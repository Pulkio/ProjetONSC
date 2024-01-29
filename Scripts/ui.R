library(shiny)
library(readxl)
library(dplyr)
library(DT)

file_names <- c("anthropometriques", "hematologie_iron", "hormes", 
                "performance", "serum_chemistry_blood", "sujet", "vitamin", "whole_blood_analysis")

ui <- fluidPage(
  titlePanel("Extraction de données des joueurs"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sélectionner un fichier Excel"),
      selectInput('selected_player', 'Choisir un joueur', choices = NULL)
    ),
    
    mainPanel(
      # Use tabsetPanel to create tabs
      tabsetPanel(id = "tabs",
                  # Create the tab panels using a for loop
                  do.call(tabsetPanel, lapply(file_names, function(name) {
                    tabPanel(title = name, dataTableOutput(outputId = paste0("table_", name)))
                  }))
      )
    )
  )
)



