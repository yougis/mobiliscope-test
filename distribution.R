library(ggplot2)
library(classInt)
library(dplyr)
library(tidyr)




# Chargement des données
secteurs <- st_read("toulouse_secteurs.geojson")
donnees <- read_csv("toulouse_age_nb.csv")



# Lire district comme chaîne, et le formater sur 3 chiffres
donnees <- read_csv("toulouse_age_nb.csv", col_types = cols(
  district = col_character()
)) %>%
  mutate(
    secteur = str_pad(district, width = 3, pad = "0")  # Assure "001", "002", etc.
  )


# Conversion des heures AM/PM en numérique
donnees <- donnees %>%
  mutate(hour_num = case_when(
    str_detect(hour, "am") ~ as.numeric(str_replace(hour, "am", "")) %% 24,
    str_detect(hour, "pm") ~ (as.numeric(str_replace(hour, "pm", "")) %% 12) + 12,
    TRUE ~ NA_real_
  ))

# Renommer colonnes pour clarté
donnees <- donnees %>%
  rename(
    secteur = district,
    age_16_24 = age1,
    age_25_34 = age2,
    age_35_64 = age3,
    age_65_plus = age4
  )




# ---- 1. Distribution de la proportion des 65+ à 11h
gg_prop_hist <- ggplot(prop_65_11h, aes(x = prop_65_plus)) +
  geom_histogram(bins = 20, fill = "#3182bd", color = "white") +
  scale_x_continuous(labels = percent) +
  labs(title = "Distribution – Proportion des 65+ à 11h", x = "Proportion", y = "Nombre de secteurs") +
  theme_minimal()

gg_prop_box <- ggplot(prop_65_11h, aes(y = prop_65_plus)) +
  geom_boxplot(fill = "#9ecae1", color = "black") +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  labs(title = "Boxplot – Proportion des 65+ à 11h", y = "Proportion") +
  theme_minimal()


# ---- 1. Distribution de la proportion des 65+ à 11h
gg_prop_hist <- ggplot(prop_65_11h, aes(x = prop_65_plus)) +
  geom_histogram(bins = 20, fill = "#3182bd", color = "white") +
  scale_x_continuous(labels = percent) +
  labs(title = "Distribution – Proportion des 65+ à 11h", x = "Proportion", y = "Nombre de secteurs") +
  theme_minimal()

gg_prop_box <- ggplot(prop_65_11h, aes(y = prop_65_plus)) +
  geom_boxplot(fill = "#9ecae1", color = "black") +
  scale_y_continuous(labels = percent) +
  coord_flip() +
  labs(title = "Boxplot – Proportion des 65+ à 11h", y = "Proportion") +
  theme_minimal()

# ---- 2. Distribution de la moyenne horaire des 65+ entre 23h et 5h
gg_moy_hist <- ggplot(donnees_65_nuit, aes(x = moy_65_nuit)) +
  geom_histogram(bins = 20, fill = "#e6550d", color = "white") +
  labs(title = "Distribution – Moyenne horaire des 65+ (23h–5h)", x = "Moyenne horaire", y = "Nombre de secteurs") +
  theme_minimal()

gg_moy_box <- ggplot(donnees_65_nuit, aes(y = moy_65_nuit)) +
  geom_boxplot(fill = "#fdae6b", color = "black") +
  coord_flip() +
  labs(title = "Boxplot – Moyenne horaire des 65+ (23h–5h)", y = "Moyenne") +
  theme_minimal()

# ---- Affichage combiné
(gg_prop_hist / gg_prop_box) | (gg_moy_hist / gg_moy_box)



# ---- 2. Distribution de la moyenne horaire des 65+ entre 23h et 5h
gg_moy_hist <- ggplot(donnees_65_nuit, aes(x = moy_65_nuit)) +
  geom_histogram(bins = 20, fill = "#e6550d", color = "white") +
  labs(title = "Distribution – Moyenne horaire des 65+ (23h–5h)", x = "Moyenne horaire", y = "Nombre de secteurs") +
  theme_minimal()

gg_moy_box <- ggplot(donnees_65_nuit, aes(y = moy_65_nuit)) +
  geom_boxplot(fill = "#fdae6b", color = "black") +
  coord_flip() +
  labs(title = "Boxplot – Moyenne horaire des 65+ (23h–5h)", y = "Moyenne") +
  theme_minimal()

# ---- Affichage combiné
(gg_prop_hist / gg_prop_box) | (gg_moy_hist / gg_moy_box)









# Exemple avec une variable continue
# Remplace par ta variable, ici moy_65_nuit issue de donnees_65_nuit
data_vect <- donnees_65_nuit$moy_65_nuit

# Nombre de classes
n_class <- 5

# 1. Effectifs égaux (quantiles)
breaks_quantile <- quantile(data_vect, probs = seq(0, 1, length.out = n_class + 1), na.rm = TRUE)
classes_quantile <- cut(data_vect, breaks = breaks_quantile, include.lowest = TRUE, labels = paste0("Q", 1:n_class))

# 2. Intervalles égaux
breaks_equal <- seq(min(data_vect, na.rm = TRUE), max(data_vect, na.rm = TRUE), length.out = n_class + 1)
classes_equal <- cut(data_vect, breaks = breaks_equal, include.lowest = TRUE, labels = paste0("I", 1:n_class))

# 3. Seuils naturels (Jenks)
jenks <- classIntervals(data_vect, n = n_class, style = "jenks")
breaks_jenks <- jenks$brks
classes_jenks <- cut(data_vect, breaks = breaks_jenks, include.lowest = TRUE, labels = paste0("J", 1:n_class))

# 4. Symétrique autour de la moyenne (écart-type)
moy <- mean(data_vect, na.rm = TRUE)
sdv <- sd(data_vect, na.rm = TRUE)
breaks_sd <- c(-Inf, moy - 2*sdv, moy - sdv, moy, moy + sdv, moy + 2*sdv, Inf)
labels_sd <- c("Très bas", "Bas", "Moyen-bas", "Moyen-haut", "Haut", "Très haut")
# On ajuste pour n_class = 5, donc fusionne deux premières classes
breaks_sd_adj <- c(-Inf, moy - 1.5*sdv, moy - 0.5*sdv, moy + 0.5*sdv, moy + 1.5*sdv, Inf)
labels_sd_adj <- paste0("SD", 1:5)
classes_sd <- cut(data_vect, breaks = breaks_sd_adj, include.lowest = TRUE, labels = labels_sd_adj)

# Assemblage des données dans un data.frame
df_compare <- data.frame(
  valeur = data_vect,
  Quantiles = classes_quantile,
  Intervalles = classes_equal,
  Jenks = classes_jenks,
  EcartType = classes_sd
) %>% 
  pivot_longer(-valeur, names_to = "Methode", values_to = "Classe")

# Fréquences par classe et méthode
df_freq <- df_compare %>%
  group_by(Methode, Classe) %>%
  summarise(Effectif = n(), .groups = "drop")

# Ordre des méthodes pour le plot
df_freq$Methode <- factor(df_freq$Methode, levels = c("Quantiles", "Intervalles", "Jenks", "EcartType"))

# Plot comparaison
ggplot(df_freq, aes(x = Classe, y = Effectif, fill = Methode)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), color = "black") +
  facet_wrap(~ Methode, scales = "free_x") +
  labs(title = "Comparaison des méthodes de discrétisation",
       y = "Effectif (nombre de secteurs)", x = "Classe") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
