library(readxl)
library(lubridate)
library(tidyr)
library(openxlsx)

# Chemin du fichier
#file_path <- "../Data/Données_Suivi_Bio_Etudiant.xlsx" # Assurez-vous de mettre à jour le chemin
file_path <- input$file$datapath  # Récupère le chemin du fichier spécifié par l'utilisateur

# Lecture du fichier Excel
# Notez qu'on suppose que les données commencent dans la première feuille et la première colonne
# Ajustez 'sheet' et 'range' si nécessaire

# 1. Dataframe Sujet
sujet <- read_excel(file_path, range = cell_rows(1:2))
# Convertir les dates de prélèvement en format date de R
# Ici, on suppose que les dates de prélèvement sont dans les colonnes suivant les deux premières colonnes de métadonnées.
# Ajustez selon la structure de votre fichier.
date_columns_sujet <- 3:ncol(sujet)
for (col in date_columns_sujet) {
  sujet[[col]] <- as.Date(sujet[[col]], format = "%Y-%m-%d")
}

# Suppression de la deuxième colonne (qui est vide)
sujet <- sujet[-2]

# Récupérer les noms originaux de la colonne (partie avant '...')
noms_originaux <- str_extract(colnames(sujet), "^[^\\.]+")

# Créer une fonction pour renommer les colonnes
renommer_colonnes <- function(noms) {
  # Initialiser un vecteur pour les nouveaux noms
  nouveaux_noms <- vector("character", length(noms))
  # Initialiser un vecteur pour garder le compte des noms
  compteur_noms <- table(noms) # table pour compter les occurrences
  noms_uniques <- names(compteur_noms)
  
  # Parcourir les noms uniques et attribuer les nouveaux noms avec les compteurs
  for (nom in noms_uniques) {
    indices <- which(noms == nom)
    nouveaux_noms[indices] <- paste0(nom, seq_along(indices))
  }
  
  return(nouveaux_noms)
}


# Appliquer la fonction pour obtenir les nouveaux noms de colonnes
nouveaux_noms <- renommer_colonnes(noms_originaux)

# Renommer les colonnes dans le dataframe sujet
names(sujet) <- nouveaux_noms

# Supprimer la première colonne
sujet <- sujet[-1]

# Ajouter deux nouvelles colonnes au début
sujet <- cbind(Donnees = NA, Unites = NA, sujet)

# Nommer la première ligne de ces nouvelles colonnes
sujet[1, "Donnees"] <- "Date_prelev"
sujet[1, "Unites"] <- "date"

# 2. Dataframe Anthropometriques
anthropometriques <- read_excel(file_path, range = cell_rows(4:6), col_names = FALSE)

# 3. Dataframe Performance
performance <- read_excel(file_path, range = cell_rows(8:11), col_names = FALSE)

# 4. Dataframe Serum Chemistry Blood
serum_chemistry_blood <- read_excel(file_path, range = cell_rows(14:29), col_names = FALSE)

# 5. Dataframe Whole Blood Analysis
whole_blood_analysis <- read_excel(file_path, range = cell_rows(31:43), col_names = FALSE)

# 6. Dataframe Hematologie Iron
hematologie_iron <- read_excel(file_path, range = cell_rows(45:48), col_names = FALSE)

# 7. Dataframe Hormes
hormes <- read_excel(file_path, range = cell_rows(50:52), col_names = FALSE)

# 8. Dataframe Vitamin
vitamin <- read_excel(file_path, range = cell_rows(54:60), col_names = FALSE)


# Supprimer la deuxième colonne
anthropometriques <- as.data.frame(anthropometriques[, -2])
performance <- as.data.frame(performance[, -2])
serum_chemistry_blood <- as.data.frame(serum_chemistry_blood[, -2])
whole_blood_analysis <- as.data.frame(whole_blood_analysis[, -2])
hematologie_iron <- as.data.frame(hematologie_iron[, -2])
hormes <- as.data.frame(hormes[, -2])
vitamin <- as.data.frame(vitamin[, -2])

# Utiliser la première colonne comme label pour chaque ligne
rownames(sujet) <- sujet[, 1]
rownames(anthropometriques) <- anthropometriques[, 1]
rownames(performance) <- performance[, 1]
rownames(serum_chemistry_blood) <- serum_chemistry_blood[, 1]
rownames(whole_blood_analysis) <- whole_blood_analysis[, 1]
rownames(hematologie_iron) <- hematologie_iron[, 1]
rownames(hormes) <- hormes[, 1]
rownames(vitamin) <- vitamin[, 1]

# Supprimer la première colonne après avoir utilisé ses valeurs comme labels
sujet <- sujet[, -1]
sujet <- sujet[, -1]
anthropometriques <- anthropometriques[, -1]
performance <- performance[, -1]
serum_chemistry_blood <- serum_chemistry_blood[, -1]
whole_blood_analysis <- whole_blood_analysis[, -1]
hematologie_iron <- hematologie_iron[, -1]
hormes <- hormes[, -1]
vitamin <- vitamin[, -1]

# Appliquer la fonction pour obtenir les nouveaux noms de colonnes

nombre_de_colonnes_manquantes <- ncol(anthropometriques) - ncol(sujet)
nouveaux_noms <- renommer_colonnes(noms_originaux)
nouveaux_noms <- nouveaux_noms[-1]
# Renommer les colonnes dans chaque dataframe
names(anthropometriques) <- nouveaux_noms
names(performance) <- nouveaux_noms
names(serum_chemistry_blood) <- nouveaux_noms
names(whole_blood_analysis) <- nouveaux_noms
names(hematologie_iron) <- nouveaux_noms
names(hormes) <- nouveaux_noms
names(vitamin) <- nouveaux_noms

# Créer un dataframe vide avec la même structure que les autres dataframes
empty_df <- data.frame(matrix(NA, ncol = ncol(sujet), nrow = 0))

# Renommer les colonnes de l'empty_df
names(empty_df) <- colnames(sujet)

# Fusionner les dataframes en ajoutant les colonnes de "sujet" au début
anthropometriques <- rbind(empty_df, anthropometriques)
performance <- rbind(empty_df, performance)
serum_chemistry_blood <- rbind(empty_df, serum_chemistry_blood)
whole_blood_analysis <- rbind(empty_df, whole_blood_analysis)
hematologie_iron <- rbind(empty_df, hematologie_iron)
hormes <- rbind(empty_df, hormes)
vitamin <- rbind(empty_df, vitamin)


# Définir le chemin du dossier de sortie
output_dir <- "../Data"  # Assurez-vous que le dossier "Data" existe déjà

# Ajouter la colonne "Sujet" aux dataframes avant l'exportation
anthropometriques <- cbind(Sujet = rownames(anthropometriques), anthropometriques)
performance <- cbind(Sujet = rownames(performance), performance)
serum_chemistry_blood <- cbind(Sujet = rownames(serum_chemistry_blood), serum_chemistry_blood)
whole_blood_analysis <- cbind(Sujet = rownames(whole_blood_analysis), whole_blood_analysis)
hematologie_iron <- cbind(Sujet = rownames(hematologie_iron), hematologie_iron)
hormes <- cbind(Sujet = rownames(hormes), hormes)
vitamin <- cbind(Sujet = rownames(vitamin), vitamin)
sujet <- cbind(Sujet = rownames(sujet), sujet)

# Définir le chemin du dossier de sortie
output_dir <- "../Data"  # Assurez-vous que le dossier "Data" existe déjà

# Exporter chaque dataframe avec la colonne "Sujet"
write.xlsx(anthropometriques, file.path(output_dir, "anthropometriques.xlsx"), rowNames = FALSE)
write.xlsx(performance, file.path(output_dir, "performance.xlsx"), rowNames = FALSE)
write.xlsx(serum_chemistry_blood, file.path(output_dir, "serum_chemistry_blood.xlsx"), rowNames = FALSE)
write.xlsx(whole_blood_analysis, file.path(output_dir, "whole_blood_analysis.xlsx"), rowNames = FALSE)
write.xlsx(hematologie_iron, file.path(output_dir, "hematologie_iron.xlsx"), rowNames = FALSE)
write.xlsx(hormes, file.path(output_dir, "hormes.xlsx"), rowNames = FALSE)
write.xlsx(vitamin, file.path(output_dir, "vitamin.xlsx"), rowNames = FALSE)
write.xlsx(sujet, file.path(output_dir, "sujet.xlsx"), rowNames = FALSE)





