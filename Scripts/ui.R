library(shiny)
library(readxl)
library(dplyr)
library(DT)

ui <- fluidPage(
  titlePanel("Extraction de données des joueurs"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sélectionner un fichier Excel"),
      selectInput('selected_player', 'Choisir un joueur', choices = NULL)
    ),
    
    mainPanel(
      uiOutput("player_tabs")
    )
  )
)





