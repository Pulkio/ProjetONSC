# Chargement des librairies nécessaires
library(shiny)
library(readxl)
library(lubridate)
library(dplyr)
library(openxlsx)
library(stringr)

# Fonction pour renommer les colonnes basée sur les occurrences des noms
renommer_colonnes <- function(noms) {
  nouveaux_noms <- vector("character", length(noms))
  compteur_noms <- table(noms)
  noms_uniques <- names(compteur_noms)
  
  for (nom in noms_uniques) {
    indices <- which(noms == nom)
    nouveaux_noms[indices] <- paste0(nom, seq_along(indices))
  }
  
  return(nouveaux_noms)
}

# Fonction pour convertir les colonnes date et numérique en caractères
convertir_colonnes <- function(df) {
  df <- df %>%
    mutate(across(where(is.Date), as.character),
           across(where(is.numeric), as.character))
  return(df)
}

# Fonction pour traiter les données extraites d'un fichier Excel
process_data <- function(file_path) {
  # Lecture de la feuille 'Sujet' et traitement des dates
  sujet <- read_excel(file_path, range = cell_rows(1:2))
  date_columns_sujet <- 3:ncol(sujet)
  for (col in date_columns_sujet) {
    sujet[[col]] <- as.Date(sujet[[col]], format = "%Y-%m-%d")
  }
  sujet <- sujet[-2]  # Suppression de la colonne vide
  noms_originaux <- str_extract(colnames(sujet), "^[^\\.]+")
  nouveaux_noms <- renommer_colonnes(noms_originaux)
  names(sujet) <- nouveaux_noms
  sujet <- sujet[-1]  # Suppression de la colonne 'Sujet'
  sujet <- cbind(Donnees = NA, Unites = NA, sujet)
  sujet[1, "Donnees"] <- "Date_prelev"
  sujet[1, "Unites"] <- "date"
  
  # Lecture et traitement des autres feuilles
  anthropometriques <- read_excel(file_path, range = cell_rows(4:6), col_names = FALSE)
  performance <- read_excel(file_path, range = cell_rows(8:11), col_names = FALSE)
  serum_chemistry_blood <- read_excel(file_path, range = cell_rows(14:29), col_names = FALSE)
  whole_blood_analysis <- read_excel(file_path, range = cell_rows(31:43), col_names = FALSE)
  hematologie_iron <- read_excel(file_path, range = cell_rows(45:48), col_names = FALSE)
  hormes <- read_excel(file_path, range = cell_rows(50:52), col_names = FALSE)
  vitamin <- read_excel(file_path, range = cell_rows(54:60), col_names = FALSE)
  
  names(anthropometriques) <- colnames(sujet)
  names(performance) <- colnames(sujet)
  names(serum_chemistry_blood) <- colnames(sujet)
  names(whole_blood_analysis) <- colnames(sujet)
  names(hematologie_iron) <- colnames(sujet)
  names(hormes) <- colnames(sujet)
  names(vitamin) <- colnames(sujet)
  
  # Convertir les colonnes des dataframes
  sujet <- convertir_colonnes(sujet)
  anthropometriques <- convertir_colonnes(anthropometriques)
  performance <- convertir_colonnes(performance)
  serum_chemistry_blood <- convertir_colonnes(serum_chemistry_blood)
  whole_blood_analysis <- convertir_colonnes(whole_blood_analysis)
  hematologie_iron <- convertir_colonnes(hematologie_iron)
  hormes <- convertir_colonnes(hormes)
  vitamin <- convertir_colonnes(vitamin)
  
  

  # Maintenant, combinez tous les dataframes en un seul
  donnee_sante_combined <- rbind(sujet, anthropometriques, performance, serum_chemistry_blood, whole_blood_analysis, hematologie_iron, hormes, vitamin)
  
  # Suppression de la colonne "Unites"
  donnee_sante_combined <- subset(donnee_sante_combined, select = -Unites)
  
  rownames(donnee_sante_combined) <- donnee_sante_combined[, 1]
  
  # Ensuite, supprimer la première colonne du dataframe car elle est maintenant utilisée comme noms de lignes
  donnee_sante_combined <- donnee_sante_combined[, -1]
  
  
  donnee_sante_combined <- donnee_sante_combined[-which(rownames(donnee_sante_combined) %in% c('IL-6', 'C Reactive Protein')), ]
  
  # Convertir la première ligne (les dates) en format Date
  donnee_sante_combined[1, ] <- lapply(donnee_sante_combined[1, ], as.Date, format="%Y-%m-%d")
  
  # Save les noms de lignes
  row.names <- rownames(donnee_sante_combined)
  # Convertir toutes les autres lignes en numérique
  donnee_sante_combined <- as.data.frame(lapply(donnee_sante_combined, function(x) as.numeric(x)))
  
  # Reappliquer les noms de lignes
  rownames(donnee_sante_combined) <- row.names
  
  # Calculer le ratio
  ratio_testo_corti <- donnee_sante_combined["Testosterone", ] / donnee_sante_combined["Cortisol", ]
  
  # Ajouter le ratio comme une nouvelle ligne au dataframe
  donnee_sante_combined <- rbind(donnee_sante_combined, ratio_testo_corti = ratio_testo_corti)
  
  
  # Spécifiez les noms de lignes que vous souhaitez conserver
  noms_de_lignes_a_garder <- c("Donnees", "Age", "Poids", "Masse grasse", "Lactate Dehydrogenase", "Creatine Kinase", "Myoglobin", "Neutrophils", "Lymphocytes", "Monocytes", "Basophil", "Hemoglobin", "Hematocrit", "Ferritin", "Testosterone", "1,25-dihydroxyvitamine D", "ratio_testo_corti")
  
  donnee_sante_combined <- donnee_sante_combined[rownames(donnee_sante_combined) %in% noms_de_lignes_a_garder, ]

  return(donnee_sante_combined)
}


# Fonction serveur qui définit le serveur de l'application Shiny
server <- function(input, output, session) {
  # Valeur réactive pour stocker le dataframe combiné des données de santé
  donnee_sante_combined <- reactiveVal()
  
  # Observer l'événement de chargement d'un fichier
  observeEvent(input$file, {
    req(input$file)
    
    # Traitement des données à partir du fichier spécifié par l'utilisateur
    donnee_sante_combined(process_data(input$file$datapath))
    
    # Extraire les identifiants uniques des joueurs
    player_identifiers <- unique(gsub("\\d+", "", colnames(donnee_sante_combined())))
    
    # Mettre à jour la liste des joueurs uniques dans l'UI
    updateSelectInput(session, 'selected_player', choices = player_identifiers)
  })
  
  # Observer la sélection d'un joueur
  observeEvent(input$selected_player, {
    req(input$selected_player, donnee_sante_combined())
    
    # Trouver les colonnes qui correspondent au joueur sélectionné
    player_cols <- grep(paste0("^", input$selected_player, "\\d+$"), colnames(donnee_sante_combined()), value = TRUE)
    
    # Générer l'UI pour les onglets du joueur sélectionné
    output$player_tabs <- renderUI({
      myTabs <- lapply(player_cols, function(col_name) {
        tabPanel(
          title = col_name,
          dataTableOutput(outputId = paste0("table_", col_name))
        )
      })
      do.call(tabsetPanel, myTabs)
    })
    
    # Afficher les tables de données pour chaque colonne du joueur
    lapply(player_cols, function(col_name) {
      output[[paste0("table_", col_name)]] <- renderDataTable({
        datatable(donnee_sante_combined()[c(which(colnames(donnee_sante_combined()) == col_name))],
                  options = list(pageLength = 5, searching = FALSE, lengthChange = FALSE), 
                  rownames = TRUE)
      })
    })
  })
}
