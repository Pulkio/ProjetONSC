library(shiny)
library(readxl)
library(dplyr)
library(DT)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Ajuster la largeur du modal /
      .modal-lg {
        width: 90% !important; / Utiliser !important pour s'assurer que cela prend le dessus /
      }
    ")),
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap")
  ),
  tags$style(HTML("
    body {
      background-color: #f0f8ff; / Bleu pâle /
    }
    .title-centered {
      text-align: center;
      font-family: 'Roboto', sans-serif; / Utilisation de la police Roboto /
      font-weight: 700; / Poids de la police */
    }
  ")),
  titlePanel(
    div("Visualisation des données de joueurs", class = "title-centered", style = "padding: 15px;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sélectionner un fichier Excel"),
      selectInput('selected_player', 'Choisir un joueur', choices = NULL),
      actionButton("show_graphs", "Graphique"), # Bouton pour afficher le modal de graphiques
      # Utiliser fluidRow et column pour organiser les tableaux
      hr(),  # Diviseur horizontal
      br(),  # Saut de ligne
      h4("Dates mesures"),
      fluidRow(
        div(
          dataTableOutput("tableau_dates"),
          style = "overflow-x: scroll;" # Pour gérer les débordements horizontaux si nécessaire
        )
      ),
      hr(),  # Diviseur horizontal
      br(),  # Saut de ligne
      h4("Tests physiques"),
      fluidRow(
        div(
          dataTableOutput("performances_data"),
          style = "overflow-x: scroll;" # Pour gérer les débordements horizontaux si nécessaire
        )
      ),
    ),
    
    mainPanel(
      uiOutput("player_tabs"),
      uiOutput("graph_ui") # Supposons que c'est la "zone grise"
    )
  )
)