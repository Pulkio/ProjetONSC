# Chargement des librairies nécessaires
library(shiny)
library(readxl)
library(lubridate)
library(dplyr)
library(openxlsx)
library(stringr)
library(DT)
library(tibble)
library(tidyr)

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
  
  #Creation dataframe nom et dates
  donnees_sujets_date <- read_excel(file_path, range = cell_rows(1:2))
  donnees_sujets_date <- donnees_sujets_date[,-c(1,2)]
  nouveaux_noms_sans_premier <- nouveaux_noms[-1]
  colnames(donnees_sujets_date) <- nouveaux_noms_sans_premier
  print(donnees_sujets_date)
  
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
  ratio_testo_corti <- round(donnee_sante_combined["Testosterone", ] / donnee_sante_combined["Cortisol", ], 2)
  
  # Ajouter le ratio comme une nouvelle ligne au dataframe
  donnee_sante_combined <- rbind(donnee_sante_combined, ratio_testo_corti = ratio_testo_corti)
  
  
  # Spécifiez les noms de lignes que vous souhaitez conserver
  noms_de_lignes_a_garder <- c("Donnees", "Age", "Poids", "Masse grasse", "Lactate Dehydrogenase", "Creatine Kinase", "Myoglobin", "Neutrophils", "Lymphocytes", "Monocytes", "Basophil", "Hemoglobin", "Hematocrit", "Ferritin", "Testosterone", "1,25-dihydroxyvitamine D", "Vitamin B12")
  
  donnee_sante_combined <- donnee_sante_combined[rownames(donnee_sante_combined) %in% noms_de_lignes_a_garder, ]
  
  normes <- read_excel("../Data/normes_valeurs.xlsx") # Assurez-vous de mettre à jour le chemin
  donnee_sante_combined <- cbind(normes[,-1], donnee_sante_combined)
  
  return(list(donnee_sante_combined = donnee_sante_combined, donnees_sujets_date = donnees_sujets_date ))
}


# Fonction serveur qui définit le serveur de l'application Shiny
server <- function(input, output, session) {
  # Valeur réactive pour stocker le dataframe combiné des données de santé
  donnee_sante_combined <- reactiveVal()
  donnees_sujets_date_reactive <- reactiveVal()
  
  # Observer l'événement de chargement d'un fichier
  observeEvent(input$file, {
    req(input$file)
    
    # Traitement des données
    data_processed <- process_data(input$file$datapath)
    
    # Mise à jour des valeurs réactives
    donnee_sante_combined(data_processed$donnee_sante_combined)
    donnees_sujets_date_reactive(data_processed$donnees_sujets_date)
    
    # Extraire les identifiants uniques des joueurs
    player_identifiers <- unique(gsub("\\d+", "", colnames(donnee_sante_combined()[-(1:2)])))
    
    print(player_identifiers)
    
    # Mettre à jour la liste des joueurs uniques dans l'UI
    updateSelectInput(session, 'selected_player', choices = player_identifiers)
  })
  
  
  
  
  
  
  
  
  # Dans la partie serveur de votre application Shiny
  observeEvent(input$show_graphs, {
    # Obtenir les noms des lignes du dataframe
    row_names <- setdiff(rownames(donnee_sante_combined()), c("Age"))
    
    showModal(modalDialog(
      title = "Graphique pour le joueur sélectionné",
      # Créer une liste déroulante avec les noms des lignes
      selectInput("selected_variable", "Choisir une variable :", choices = row_names),
      actionButton("show_plot", "Afficher le graphique"), # Bouton pour générer le graphique
      plotlyOutput("plot"), # Output pour le graphique Plotly
      size = "l" # Taille du modal
    ))
  })
  

  observeEvent(input$show_plot, {
    req(input$selected_player, input$selected_variable)
    
    # Filtrez les données pour n'inclure que les mesures pour le joueur sélectionné
    data_long <- reactive({
      selected_player <- input$selected_player
      selected_variable <- input$selected_variable
      
      # Sélectionnez uniquement les colonnes pour le joueur sélectionné
      cols_for_player <- grep(paste0("^", selected_player, "\\d+$"), names(donnee_sante_combined()), value = TRUE)
      
      # Transposez et transformez les données pour ce joueur spécifique
      data <- donnee_sante_combined()[, cols_for_player] %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column("Temps") %>%
        tidyr::pivot_longer(-Temps, names_to = "Variable", values_to = "Valeur") %>%
        dplyr::filter(Variable == selected_variable)
      
      return(data)
    })
    
    # Créez le graphique Plotly
    output$plot <- renderPlotly({
      req(data_long())
      
      hover_text <- paste("Temps: ", data_long()$Temps, "<br>Valeur: ", data_long()$Valeur)
      
      # Récupérez les valeurs normales pour la variable sélectionnée
      norme_inf <- donnee_sante_combined()[input$selected_variable, "Norme_inf"]
      norme_sup <- donnee_sante_combined()[input$selected_variable, "Norme_supp"]
      
      # Tracez les points avec les noms personnalisés dans la légende
      plot <- plot_ly(data = data_long(), x = ~Temps, y = ~Valeur, type = 'scatter', mode = 'lines+markers+text',
                      text = ~Valeur, hoverinfo = 'text', textposition = 'top center', name = 'Joueur') %>%
        layout(title = paste("Évolution de", input$selected_variable, "pour le joueur", input$selected_player, "à travers le temps"),
               xaxis = list(title = "Temps"),
               yaxis = list(title = "Valeur"))
      
      # Ajoutez des lignes en pointillés pour les normes avec les noms personnalisés dans la légende
      if (!is.null(norme_inf) && !is.null(norme_sup)) {
        plot <- plot %>%
          add_trace(y = ~rep(norme_inf, length(data_long()$Temps)), mode = 'lines', line = list(dash = 'dot', color = 'blue'), name = 'norme_inf') %>%
          add_trace(y = ~rep(norme_sup, length(data_long()$Temps)), mode = 'lines', line = list(dash = 'dot', color = 'red'), name = 'norme_sup')
      }
      
      return(plot)
    })
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Observer la sélection d'un joueur
  observeEvent(input$selected_player, {
    req(input$selected_player, donnee_sante_combined())
    
    # Trouver les colonnes qui correspondent au joueur sélectionné
    player_cols <- grep(paste0("^", input$selected_player, "\\d+$"), colnames(donnee_sante_combined()), value = TRUE)
    
    
    # Mise à jour du tableau de dates
    output$tableau_dates <- renderDataTable({
      # Assurez-vous que donnees_sujets_date_reactive n'est pas NULL
      # if (is.null(donnees_sujets_date_reactive())) {
      #   return(dataTableOutput()) # Retourne un tableau vide si NULL
      # }
      
      # Récupérez le dataframe des dates et formatez-le
      
      # Récupérez le dataframe des dates et formatez-le
      donnees_sujets_date <- donnees_sujets_date_reactive() %>%
        select(matches(player_cols)) %>%  # Sélectionnez uniquement les colonnes pour le joueur sélectionné
        mutate(across(everything(), ~format(as.POSIXct(.x), "%d-%m-%Y")))
      
      print("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA") 
      str(donnees_sujets_date)
      
      
      datatable(donnees_sujets_date, options = list(
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        lengthChange = FALSE
      ), rownames = FALSE)
    })
    
    
    
    
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
        datatable(donnee_sante_combined()[c(which(colnames(donnee_sante_combined()) == col_name),1,2)],
                  options = list(pageLength = 16, searching = FALSE, lengthChange = FALSE,paging = FALSE,
                                 rowCallback = JS(
                                   "function(row, data, index) {",
                                   "var normeInf = parseFloat(data[2]);",  #la norme inférieure est en 2ème colonne
                                   "var normeSup = parseFloat(data[3]);",  #la norme supérieure est en 3ème colonne
                                   "var value = parseFloat(data[1]);",  # la valeur est en 1ère colonne
                                   "if (value >= normeInf && value <= normeSup) {",
                                   "$('td', row).css('color', 'green');",
                                   "} else {",
                                   "$('td', row).css('color', 'red');",
                                   "}",
                                   "}"
                                 )), 
                  rownames = TRUE)
      })
    })
    
  })
}
