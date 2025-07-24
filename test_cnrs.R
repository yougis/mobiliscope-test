# Chargement des bibliothèques
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(readr)

# Lecture des données (à adapter selon votre source)
population <- read_csv("toulouse_age_nb.csv")
secteurs_sf <- st_read("toulouse_secteurs.geojson")


### 1. Carte : Nombre de personnes âgées de 35-64 ans à 11h ###
age_35_64_11h <- population %>%
  filter(age == "35-64", heure == 11) %>%
  group_by(secteur) %>%
  summarise(nb_35_64 = sum(nb_personnes))

carte_35_64 <- secteurs_sf %>%
  left_join(age_35_64_11h, by = "secteur")

ggplot(carte_35_64) +
  geom_sf(aes(fill = nb_35_64)) +
  scale_fill_viridis(name = "Personnes 35-64 ans à 11h", na.value = "grey90") +
  labs(title = "Nombre de personnes âgées de 35-64 ans à 11h par secteur") +
  theme_minimal()

### 2. Carte : Proportion des 65+ à 11h (parmi les 16+) ###
population_11h <- population %>%
  filter(heure == 11) %>%
  mutate(age_group = case_when(
    age %in% c("16-24", "25-34", "35-64", "65+") ~ age,
    TRUE ~ "autres"
  )) %>%
  filter(age_group %in% c("16-24", "25-34", "35-64", "65+"))

prop_65_11h <- population_11h %>%
  group_by(secteur) %>%
  summarise(
    nb_65 = sum(nb_personnes[age == "65+"]),
    nb_total_16plus = sum(nb_personnes),
    prop_65 = nb_65 / nb_total_16plus
  )

carte_prop_65 <- secteurs_sf %>%
  left_join(prop_65_11h, by = "secteur")

ggplot(carte_prop_65) +
  geom_sf(aes(fill = prop_65)) +
  scale_fill_viridis(name = "Proportion 65+ à 11h", na.value = "grey90", labels = scales::percent) +
  labs(title = "Proportion des 65 ans et plus à 11h (parmi les 16+)") +
  theme_minimal()

### 3. Carte : Moyenne horaire des 65+ entre 23h et 5h ###
# Attention : les heures sont peut-être stockées en 0-23
heures_nuit <- c(23, 0, 1, 2, 3, 4, 5)

moy_65_nuit <- population %>%
  filter(age == "65+", heure %in% heures_nuit) %>%
  group_by(secteur) %>%
  summarise(moy_65_nuit = mean(nb_personnes))

carte_65_nuit <- secteurs_sf %>%
  left_join(moy_65_nuit, by = "secteur")

ggplot(carte_65_nuit) +
  geom_sf(aes(fill = moy_65_nuit)) +
  scale_fill_viridis(name = "Moyenne 65+ (23h-5h)", na.value = "grey90") +
  labs(title = "Moyenne horaire des 65 ans et plus (23h–5h)") +
  theme_minimal()

### 4. Graphique : Nb de personnes 16-24 ans dans le secteur du Capitole ###
capitole_16_24 <- population %>%
  filter(secteur == "Capitole", age == "16-24") %>%
  arrange(heure)

ggplot(capitole_16_24, aes(x = heure, y = nb_personnes)) +
  geom_line(color = "#2E86AB", size = 1.2) +
  geom_point(color = "#2E86AB") +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Présence des 16-24 ans dans le secteur du Capitole",
    x = "Heure",
    y = "Nombre de personnes"
  ) +
  theme_minimal()

