library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Ajuster la largeur du modal */
      .modal-lg {
        width: 90% !important; /* Utiliser !important pour s'assurer que cela prend le dessus */
      }
    "))
  ),
  titlePanel("Extraction de données des joueurs"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sélectionner un fichier Excel"),
      selectInput('selected_player', 'Choisir un joueur', choices = NULL),
      actionButton("show_graphs", "Graphique"), # Bouton pour afficher le modal de graphiques
      # Utiliser fluidRow et column pour organiser les tableaux
      fluidRow(
        div(
          dataTableOutput("tableau_dates"),
          style = "overflow-x: scroll;" # Pour gérer les débordements horizontaux si nécessaire
        )
      )
    ),
    mainPanel(
      uiOutput("player_tabs"),
      uiOutput("graph_ui") # Supposons que c'est la "zone grise"
    )
  )
)






