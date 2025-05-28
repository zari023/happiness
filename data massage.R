# Felicidad 2015–2024: Análisis completo

################################################################################
### PASO 1: Librerías necesarias ###
################################################################################
librerias <- c("readr", "dplyr", "stringr", "tidyr", "purrr", 
               "ggplot2", "plotly", "countrycode", "corrplot", 
               "FactoMineR", "factoextra", "forcats", "broom", "rworldmap")
instaladas <- librerias %in% rownames(installed.packages())
if (any(!instaladas)) install.packages(librerias[!instaladas])
lapply(librerias, library, character.only = TRUE)

################################################################################
### PASO 2: Leer y limpiar el dataset nuevo ###
################################################################################
setwd("/Users/lauragispertcortes/Downloads/archive")
df <- read_csv2("world_happiness_combined.csv")

# Limpiar nombres de columnas
names(df) <- names(df) %>%
  str_trim() %>%
  str_to_lower() %>%
  str_replace_all("[\\.\\(\\)\\s]", "_") %>%
  str_replace_all("+", "") %>%
  str_replace_all("_$", "")

################################################################################
### Complemento: Estándarizar países y corregir regiones ###
################################################################################
country_corrections <- c(
  "State of Palestine" = "Palestina",
  "Palestinian Territories" = "Palestina",
  "United States" = "United States of America",
  "USA" = "United States of America",
  "Russia" = "Russian Federation",
  "Russia Federation" = "Russian Federation"
)
df <- df %>%
  mutate(country = recode(country, !!!country_corrections))

# Corregir regiones faltantes
df <- df %>%
  mutate(regional_indicator = case_when(
    country == "Greece" ~ "Southern Europe",
    country == "Cyprus" ~ "Southern Europe",
    country == "Gambia" ~ "Sub-Saharan Africa",
    TRUE ~ regional_indicator
  ))

################################################################################
### Asignar región y continente coherente ###
################################################################################
df <- df %>%
  rename(region = regional_indicator)

region_to_continent <- c(
  "Western Europe" = "Europe",
  "Central and Eastern Europe" = "Europe",
  "Southern Europe" = "Europe",
  "Sub-Saharan Africa" = "Africa",
  "Middle East and North Africa" = "Africa",
  "South Asia" = "Asia",
  "East Asia" = "Asia",
  "Southeast Asia" = "Asia",
  "Latin America and Caribbean" = "America",
  "North America and ANZ" = "America",
  "Commonwealth of Independent States" = "Europe",
  "Central Asia" = "Asia",
  "Eastern Europe" = "Europe"
)
df <- df %>%
  mutate(continent = recode(region, !!!region_to_continent)) %>%
  mutate(continent = ifelse(country %in% c("Australia", "New Zealand"), "Oceania", continent))
df
################################################################################
### Convertir columnas numéricas ###
################################################################################
cols_num <- c("happiness_score", "gdp_per_capita", "social_support",
              "healthy_life_expectancy", "freedom_to_make_life_choices",
              "generosity", "perceptions_of_corruption")
df <- df %>%
  mutate(across(all_of(cols_num), ~ as.numeric(str_replace_all(., ",", ".")))) %>%
  rename(freedom = freedom_to_make_life_choices)

hi ha tot axiò