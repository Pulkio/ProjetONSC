library(shiny)
library(readxl)
library(dplyr)
library(DT)

# Définition globale des noms des fichiers Excel à utiliser
file_names <- c("anthropometriques", "hematologie_iron", "hormes", 
                "performance", "serum_chemistry_blood", "sujet", "vitamin", "whole_blood_analysis")

# Fonction server qui définit le serveur de l'application Shiny
server <- function(input, output, session) {
  
  # Valeurs réactives pour stocker les identifiants uniques des joueurs et leurs instances
  unique_players <- reactiveVal()
  player_instances <- reactiveVal(list())
  
  # Observer l'événement de chargement d'un fichier
  observeEvent(input$file, {
    # Vérifier que le fichier est bien téléchargé
    req(input$file)
    
    # Lire les données des joueurs à partir du fichier 'sujet.xlsx'
    player_data <- readxl::read_excel("../Data/sujet.xlsx")
    # Extraire les identifiants uniques des joueurs en supprimant les parties numériques
    player_identifiers <- unique(gsub("\\d+", "", colnames(player_data)[-1]))
    
    # Mettre à jour la liste des joueurs uniques
    unique_players(player_identifiers)
    # Mettre à jour le menu déroulant des joueurs dans l'interface utilisateur
    updateSelectInput(session, 'selected_player', choices = player_identifiers)
  })
  
  # Observer la sélection d'un joueur
  observeEvent(input$selected_player, {
    # Vérifier que le joueur est sélectionné et que la liste des joueurs est disponible
    req(input$selected_player, unique_players())
    
    # Créer une liste pour stocker les instances du joueur sélectionné dans tous les fichiers
    instances_list <- list()
    
    # Pour chaque nom de fichier, lire les données et trouver les instances du joueur sélectionné
    for (file_name in file_names) {
      # Tenter de lire le fichier Excel
      tryCatch({
        sheet_data <- readxl::read_excel(paste0("../Data/", file_name, ".xlsx"), sheet = 1)
        # Trouver les colonnes qui correspondent au joueur sélectionné et inclure la première colonne
        player_col_indices <- grep(paste("^", input$selected_player, "\\d+$", sep = ""), colnames(sheet_data))
        player_cols <- c(1, player_col_indices) # Combiner la première colonne avec les indices trouvés
        if (length(player_cols) > 1) { # Assurez-vous qu'il y a plus que juste la première colonne
          instances_list[[file_name]] <- sheet_data[, player_cols, drop = FALSE]
        }
      }, error = function(e) {
        # Gérer l'erreur de lecture du fichier
        cat("Error reading file:", file_name, "\n")
      })
    }
    
    
    # Mettre à jour les instances du joueur dans la valeur réactive
    player_instances(instances_list)
    
    # Créer l'interface utilisateur pour les onglets des instances du joueur sélectionné
    output$player_tabs <- renderUI({
      # Utiliser lapply pour créer un onglet pour chaque instance du joueur
      myTabs <- lapply(names(player_instances()), function(file_name) {
        tabPanel(
          title = file_name,
          dataTableOutput(outputId = paste0("table_", file_name))
        )
      })
      # Combiner tous les onglets dans un panneau d'onglets
      do.call(tabsetPanel, myTabs)
    })
    
    # Afficher les tables de données pour chaque instance
    lapply(names(player_instances()), function(file_name) {
      # Créer une sortie pour chaque table de données
      output[[paste0("table_", file_name)]] <- renderDataTable({
        # S'assurer que les instances des joueurs sont disponibles
        req(player_instances())
        # Afficher la table de données avec les options configurées
        datatable(player_instances()[[file_name]], options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE), rownames = TRUE)
      })
    })
  })
}


