library(readxl)
library(lubridate)
library(tidyr)
library(openxlsx)
library(stringr)
library(dplyr)

# Chemin du fichier
file_path <- "../Data/Données_Suivi_Bio_Etudiant.xlsx" # Assurez-vous de mettre à jour le chemin
#file_path <- input$file$datapath  # Récupère le chemin du fichier spécifié par l'utilisateur

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











# Lecture du fichier Excel pour les deux premières lignes
donnees_sujets_date <- read_excel(file_path, range = cell_rows(1:2))

donnees_sujets_date <- donnees_sujets_date[,-c(1,2)]

nouveaux_noms_sans_premier <- nouveaux_noms[-1]


colnames(donnees_sujets_date) <- nouveaux_noms_sans_premier











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


# Renommer les colonnes de l'empty_df
names(anthropometriques) <- colnames(sujet)
names(performance) <- colnames(sujet)
names(serum_chemistry_blood) <- colnames(sujet)
names(whole_blood_analysis) <- colnames(sujet)
names(hematologie_iron) <- colnames(sujet)
names(hormes) <- colnames(sujet)
names(vitamin) <- colnames(sujet)

# Convertissez les colonnes de type date en chaînes de caractères pour `sujet`
sujet <- sujet %>%
  mutate(across(where(is.Date), as.character))

# Convertissez les colonnes numériques en chaînes de caractères pour les autres dataframes
anthropometriques <- anthropometriques %>%
  mutate(across(where(is.numeric), as.character))

performance <- performance %>%
  mutate(across(where(is.numeric), as.character))

serum_chemistry_blood <- serum_chemistry_blood %>%
  mutate(across(where(is.numeric), as.character))

whole_blood_analysis <- whole_blood_analysis %>%
  mutate(across(where(is.numeric), as.character))

hematologie_iron <- hematologie_iron %>%
  mutate(across(where(is.numeric), as.character))

hormes <- hormes %>%
  mutate(across(where(is.numeric), as.character))

vitamin <- vitamin %>%
  mutate(across(where(is.numeric), as.character))












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
# noms_de_lignes_a_garder <- c("Donnees", "Age", "Poids", "Masse grasse", "Lactate Dehydrogenase", "Creatine Kinase", "Myoglobin", "Neutrophils", "Lymphocytes", "Monocytes", "Basophil", "Hemoglobin", "Hematocrit", "Ferritin", "Testosterone", "1,25-dihydroxyvitamine D", "ratio_testo_corti")
# 
# normes_2 <- read_excel("../Data/normes_valeurs.xlsx") # Assurez-vous de mettre à jour le chemin
# 
# 
# donnee_sante_combined <- donnee_sante_combined[rownames(donnee_sante_combined) %in% noms_de_lignes_a_garder, ]
# 
# donnee_sante_combined <- cbind(normes_2[,-1], donnee_sante_combined)

