library(dplyr)
library(lubridate)

################################################################################
## DATA LOADER #################################################################
################################################################################
# Chemin vers le dossier contenant les fichiers
chemin_dossier <- "/Users/simontrottier/Documents/Polimi/Nonparametric stat/stat_project/Data_final_c/"
# Obtenir la liste des fichiers dans le dossier
fichiers <- list.files(path = chemin_dossier, pattern = "\\.csv", full.names = TRUE)
# Initialiser une liste pour stocker les données
liste_donnees <- list()

B <- 500
# Boucle pour lire chaque fichier et stocker les données dans la liste
for (fichier in fichiers) {
  # Lire le fichier et stocker les données dans la liste
  donnees <- read.table(fichier, header = TRUE, sep = ",")
  liste_donnees[[basename(fichier)]] <- donnees
}

# Calculer la longueur de la liste_donnees
longueur_liste <- length(liste_donnees)
# Créer un tableau vide avec le nombre de lignes égal à la longueur de liste_donnees
RP <- array(NA, dim = c(longueur_liste, 12, B+1))

# Définir le nombre total d'itérations (ajustez selon votre besoin)
nombre_iterations <- length(liste_donnees)
# Créer une barre de progression texte
barre_progression <- txtProgressBar(min = 0, max = nombre_iterations, style = 3)
# Boucle avec la barre de progression
#for (i in 1:length(liste_donnees)){
set.seed(24021979)
for (i in 1:nombre_iterations){
  # Importer les données depuis un fichier
  data <- liste_donnees[[i]] # accéder à la i-ème table
  if (nrow(data) == 0){
    next
  }
  #Mettre le parametre d'interet
  data <- data[!(is.na(data$tmax)), ]
  # data <- na.omit(data)
  data$time <- as.Date(data$time)
  
  for (mois in 1:12){
    data_annee_en_cours = data %>% filter(month(time) == mois)
    # data_annee_en_cours = na.omit(data_annee_en_cours)
    if (nrow(data_annee_en_cours) == 0){
      next
    }
    
    annees <- unique(year(data_annee_en_cours$time)) # seq(1978, 2022, by = 1)
    
    # Variables de réponse et de prédicteur pour le parametre d'interet
    response  <- data_annee_en_cours$tmax
    regressor <- annees
    n<- length(response)
    if (n>=30){

    
    # Ajuster un modèle linéaire
    fm <- lm(response ~ regressor)
    
    ################################################################################
    ### Permutation test ###########################################################
    ################################################################################
    # Calculer la statistique de base 
    s<-summary(fm)
    b<-s$coefficients[2,1]
    se<-s$coefficients[2,2]
    T0 <- max(0,b/se)
    
    RP[i, mois, 1] <- T0
    # Coefficients estimés du modèle original
   
    # Fixer la graine aléatoire pour la reproductibilité
    
    # Nombre d'itérations du permtest
    # Initialiser des vecteurs pour stocker les estimations des coefficients tests
    T.perm <- numeric(B)
    
    # Boucle sur les itérations Bootstrap
    for(b in 1:B)
    {
      # Ré-échantillonner les résidus avec remplacement
      permutation <- sample(1:n) 
      
      response.perm <- response[permutation] 
      # Ajuster un modèle linéaire sur les données permutées
      fm.perm <- lm(response.perm ~ regressor)
      # Stocker les statistiques estimées
      s.perm<-summary(fm.perm)
      b.perm<-s.perm$coefficients[2,1]
      se.perm<-s.perm$coefficients[2,2]
      RP[i, mois, b+1] <- max(0,b.perm/se.perm)
    }
    }
  }
  # Mettre à jour la barre de progression
  setTxtProgressBar(barre_progression, i)
  # Forcer l'affichage immédiat de la barre de progression dans la console
  flush.console()
}
# Imprimer une nouvelle ligne pour éviter d'écraser la barre de progression
cat("\n")
# Fermer la barre de progression à la fin
close(barre_progression)


# Renommer les lignes et les colonnes
noms_lignes <- basename(fichiers)
dimnames(RP)[[1]] <- noms_lignes 
noms_colonnes <- c("RP Int January", "RP Int February", "RP Int March", "RP Int April", "RP Int May", "RP Int June", "RP Int July", "RP Int August", "RP Int September", "RP Int October", "RP Int November", "RP Int December")
dimnames(RP)[[2]] <- noms_colonnes
noms_matrices <- c(as.character(c(0:B)))
dimnames(RP)[[3]] <- noms_matrices

# Spécifier le chemin complet vers le fichier de sortie
chemin_fichier <- "/Users/simontrottier/Documents/Polimi/Nonparametric stat/stat_project/EXPORT_Perm_tmax.csv"
# Exporter le dataframe en format CSV
write.csv(RP, file = chemin_fichier, row.names = TRUE)
