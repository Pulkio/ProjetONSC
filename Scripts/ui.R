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
      uiOutput("player_tabs") # This will render the tabs for each player test
    )
  )
)






