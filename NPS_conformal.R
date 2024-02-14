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

station<-175
month<-6

################################################################################
## CONFORMAL PREDICTION ########################################################
################################################################################
#Cape town
for (i in station:station){
  # Importer les données depuis un fichier
  data <- liste_donnees[[i]] # accéder à la i-ème table
  if (nrow(data) == 0){
    next
  }
  #Mettre le parametre d'interet
  data <- data[!(is.na(data$tavg)), ]
  # data <- na.omit(data)
  data$time <- as.Date(data$time)
  
  #June month
  for (mois in month:month){
    data_annee_en_cours = data %>% filter(month(time) == mois)
    if (nrow(data_annee_en_cours) == 0){
      next
    }
    
    annees <- unique(year(data_annee_en_cours$time)) # seq(1978, 2022, by = 1)
    
    # Variables de réponse et de prédicteur pour le parametre d'interet
    response  <- data_annee_en_cours$tavg
    regressor <- annees
    
    
    n<- length(response)
    if (n>=30){
      fm<-lm(response~regressor)
      coeff <- coefficients(fm)
      b<-coeff[2]
      print(b)
      alpha <- 0.1
      
      x.obs <- cbind(regressor,response)
      x1.new.grid <- c(seq(1980,2050,2))
      x2.new.grid <- seq(min(x.obs[,2]) + min(-b*50,b*20), max(x.obs[,2]) + max(-b*20,b*50), 0.2)
      
      p.value <- matrix(nrow = length(x1.new.grid), ncol = length(x2.new.grid))
      NC <- function(z.aug, i){
        abs( z.aug[i,2] - sum(coefficients(lm(z.aug[-i,2]  ~ z.aug[-i,1]))*c(1, z.aug[i,1]))) # Regression
      }
      
      for(k in 1:length(x1.new.grid)) {
        for(h in 1:length(x2.new.grid)) {
          x.obs.aug <- rbind(x.obs, c(x1.new.grid[k],x2.new.grid[h]))
          scores <- numeric(dim(x.obs.aug)[1])
          for (i in 1:dim(x.obs.aug)[1]) {
            scores[i] <- NC(x.obs.aug, i)
          }
          p.value[k,h] <- sum(scores >= scores[dim(x.obs.aug)[1]])/(dim(x.obs.aug)[1])
        }
      }
    }
    set_1980 <- x2.new.grid[which(p.value[1,]>=alpha)]
    interval_1980<-c(min(set_1980),max(set_1980))
    print(interval_1980)
    
    set_2030 <- x2.new.grid[which(p.value[26,]>=alpha)]
    interval_2030<-c(min(set_2030),max(set_2030))
    print(interval_2030)
    
    set_2040 <- x2.new.grid[which(p.value[31,]>=alpha)]
    interval_2040<-c(min(set_2040),max(set_2040))
    print(interval_2040)
    
    set_2050 <- x2.new.grid[which(p.value[36,]>=alpha)]
    interval_2050<-c(min(set_2050),max(set_2050))
    print(interval_2050)
  }
  
  
  
  #contour(x1.new.grid, x2.new.grid, p.value, levels = alpha, add=T)
  # Mettre à jour la barre de progression
  setTxtProgressBar(barre_progression, i)
  # Forcer l'affichage immédiat de la barre de progression dans la console
  flush.console()
  }
  
# Imprimer une nouvelle ligne pour éviter d'écraser la barre de progression
cat("\n")
# Fermer la barre de progression à la fin
close(barre_progression)

#Plot of the results
year = x1.new.grid
Average_temperature = x2.new.grid
image(year, Average_temperature, p.value, zlim=c(0,1), asp=1)
points(x.obs, pch=16)
contour(x1.new.grid, x2.new.grid, p.value, levels = alpha, add=T)

