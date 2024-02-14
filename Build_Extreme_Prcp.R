path <- "your_path"
data = read.csv(path)

donnees_filtrees <- data %>% filter(StationID == "Data_Verona _ Villafranca") # The right station
donnees_filtrees_mois <- donnees_filtrees %>% filter(Mois == 9) # November 

quantile_95 <- quantile(donnees_filtrees_mois$prcp, probs = 0.95)
donnees_filtrees_mois_above_95 <- donnees_filtrees_mois %>% filter(prcp >= quantile_95)
donnees_filtrees_mois_above_95_Year <- donnees_filtrees_mois_above_95 %>% group_by(Year = format(as.Date(time), "%Y"))

# HISTOGRAMME :
hist(as.numeric(donnees_filtrees_mois_above_95_Year$Year), main = "Daily Precipitations in Verona above the 95th quantile, in September, by year (1978-2022)", xlab = "Years", ylab = "Frequency")

# INDICATEURS NUMERIQUES :
min_year_above_95 = min(as.numeric(donnees_filtrees_mois_above_95_Year$Year))
mean_year_above_95 = mean(as.numeric(donnees_filtrees_mois_above_95_Year$Year))
med_year_above_95 = median(as.numeric(donnees_filtrees_mois_above_95_Year$Year))
max_year_above_95 = max(as.numeric(donnees_filtrees_mois_above_95_Year$Year))