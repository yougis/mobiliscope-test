# Chargement des bibliothèques
library(tidyverse)
library(sf)
library(lubridate)
library(ggplot2)

library(patchwork)  # Pour combiner les graphes
library(scales)


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
  str_detect(hour, "am") ~ ifelse(str_replace(hour, "am", "") == "12", 0, as.numeric(str_replace(hour, "am", ""))),
  str_detect(hour, "pm") ~ ifelse(str_replace(hour, "pm", "") == "12", 12, as.numeric(str_replace(hour, "pm", "")) + 12),
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


# ----------------------------------------------
# 1. Carte : NOMBRE de 35–64 ans à 11h (CERCLÉS)
# ----------------------------------------------
donnees_35_64_11h <- donnees %>%
  filter(hour_num == 11) %>%
  select(secteur, age_35_64)

map_35_64_11h <- secteurs %>%
  left_join(donnees_35_64_11h, by = c("CODE_SEC" = "secteur"))

ggplot() +
  geom_sf(data = secteurs, fill = "white", color = "grey70") +
  geom_sf(data = map_35_64_11h, aes(size = age_35_64, geometry = geometry), color = "darkblue", alpha = 0.6) +
  scale_size_continuous(name = "Nb personnes 35–64 ans", range = c(1, 15)) +
  labs(title = "Présence des 35–64 ans à 11h à Toulouse") +
  theme_minimal()


# -------------------------------------------------------
# 2. Carte : PROPORTION des 65+ à 11h (CHOROPLÈTHE)
# -------------------------------------------------------
# Discrétisation en quintiles
prop_65_11h <- donnees %>%
  filter(hour_num == 11) %>%
  mutate(pop16plus = age_16_24 + age_25_34 + age_35_64 + age_65_plus,
         prop_65_plus = age_65_plus / pop16plus) %>%
  select(secteur, prop_65_plus) %>%
  mutate(
    classe_prop = cut(
      prop_65_plus,
      breaks = quantile(prop_65_plus, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = paste0("Q", 1:5)
    )
  )

map_prop_65 <- secteurs %>%
  left_join(prop_65_11h, by = c("CODE_SEC" = "secteur"))

ggplot(map_prop_65) +
  geom_sf(aes(fill = classe_prop), color = "white") +
  scale_fill_brewer(palette = "YlGnBu", name = "Proportion 65+ (quintiles)") +
  labs(title = "Proportion des 65 ans et plus à 11h (discrétisation en quintiles)") +
  theme_minimal()


# ------------------------------------------------------------------
# 3. Carte CHOROPLÈTHE : Moyenne horaire des 65+ entre 23h et 5h
# ------------------------------------------------------------------

nuit <- c(23, 0, 1, 2, 3, 4, 5)

donnees_65_nuit <- donnees %>%
  filter(hour_num %in% nuit) %>%
  group_by(secteur) %>%
  summarise(moy_65_nuit = mean(age_65_plus, na.rm = TRUE))

map_65_nuit <- secteurs %>%
  left_join(donnees_65_nuit, by = c("CODE_SEC" = "secteur"))

ggplot(map_65_nuit) +
  geom_sf(aes(fill = moy_65_nuit), color = "white") +
  scale_fill_viridis_c(name = "Moyenne 65+ (23h–5h)", option = "plasma") +
  labs(title = "Moyenne horaire des 65 ans et plus (23h à 5h) à Toulouse") +
  theme_minimal()

#----

donnees_65_nuit <- donnees %>%
  filter(hour_num %in% nuit) %>%
  group_by(secteur) %>%
  summarise(moy_65_nuit = mean(age_65_plus, na.rm = TRUE)) %>%
  mutate(
    classe_moy = cut(
      moy_65_nuit,
      breaks = quantile(moy_65_nuit, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = paste0("Q", 1:5)
    )
  )

map_65_nuit <- secteurs %>%
  left_join(donnees_65_nuit, by = c("CODE_SEC" = "secteur"))

ggplot(map_65_nuit) +
  geom_sf(aes(fill = classe_moy), color = "white") +
  scale_fill_brewer(palette = "OrRd", name = "Moyenne 65+ (quintiles)") +
  labs(title = "Moyenne horaire des 65 ans et plus (23h–5h), par quintile") +
  theme_minimal()



# ---------------------------------------------------
# 4. Graphique 16–24 ans dans secteur Capitole
# ---------------------------------------------------
# Vérifiez ici le bon CODE_SEC correspondant au Capitole
code_capitole <- secteurs %>%
  filter(str_detect(NAME_1, "Capitole", ignore.case = TRUE)) %>%  # Adaptable selon attributs
  pull(CODE_SEC)

# Si vous connaissez déjà le code, par exemple :
# code_capitole <- "SEC045"

graph_capitole <- donnees %>%
  filter(secteur == code_capitole) %>%
  arrange(hour_num) %>%
  select(hour_num, age_16_24)

ggplot(graph_capitole, aes(x = hour_num, y = age_16_24)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Présence des 16–24 ans dans le secteur du Capitole",
       x = "Heure", y = "Nombre de personnes") +
  theme_minimal()
