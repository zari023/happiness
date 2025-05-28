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
  str_replace_all("_+", "_") %>%
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

################################################################################
### PASO 3: Análisis y gráficos por hipótesis ###
################################################################################
write_csv(df_felicidad, "2015_2024.csv")

# Hipótesis 1: PIB y Felicidad
ggplot(df, aes(x = gdp_per_capita, y = happiness_score)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange3") +
  labs(title = "Relación entre PIB per cápita y Felicidad",
       x = "PIB per cápita",
       y = "Puntuación de felicidad") +
  theme_minimal()

h10_df <- df %>%filter(gdp_per_capita>9)
library(plotly)
library(dplyr)
library(readr)

# Cargar datos (ajusta el nombre si es necesario)

# Calcular la media de felicidad y PIB por país (para detectar outliers)
df_summary <- h10_df %>%
  group_by(country, continent) %>%
  summarise(
    mean_gdp = mean(gdp_per_capita, na.rm = TRUE),
    mean_score = mean(happiness_score, na.rm = TRUE),
    .groups = "drop"
  )

# Agregar una bandera para identificar países "outliers" a simple vista (ejemplo manual)
# Aquí podrías personalizarlo si tienes un criterio específico de qué países son "outliers"
outliers <- c("Japan", "South Korea", "Russia", "Ukraine", "Israel", "Singapore")
df_summary$outlier <- ifelse(df_summary$country %in% outliers, "Sí", "No")

# Gráfico interactivo
fig <- plot_ly(
  data = df_summary,
  x = ~mean_gdp,
  y = ~mean_score,
  type = 'scatter',
  mode = 'markers',
  color = ~outlier,
  colors = c("yellow", "blue"),
  marker = list(size = 10, line = list(width = 1, color = 'black')),
  text = ~paste("<b>País:</b>", country,
                "<br><b>PIB per càpita:</b>", round(mean_gdp, 2),
                "<br><b>Felicitat:</b>", round(mean_score, 2)),
  hoverinfo = 'text'
) %>%
  layout(
    title = "Relació entre PIB per càpita i Felicitat: identificant els 'outliers'",
    xaxis = list(title = "PIB per càpita (mitjana)"),
    yaxis = list(title = "Felicitat (mitjana)"),
    showlegend = FALSE
  )

# Mostrar el gráfico
fig

# Hipótesis 2: Generosidad y Felicidad
ggplot(df, aes(x = generosity, y = happiness_score)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
  labs(title = "Relación entre Generosidad y Felicidad",
       x = "Generosidad",
       y = "Puntuación de felicidad") +
  theme_minimal()

# Violin plot para Generosidad y Felicidad
ggplot(df, aes(x = cut(generosity, breaks = 4), y = happiness_score)) +
  geom_violin(fill = "skyblue", color = "black", alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Felicidad según rangos de Generosidad",
       x = "Generosidad (en rangos)",
       y = "Puntuación de felicidad") +
  theme_minimal()

# Hipótesis 3: Esperanza de vida saludable y Felicidad


# Hipótesis 4: Cambios en felicidad por año
df_continente <- df %>%
  group_by(continent, year) %>%
  summarise(mean_score = mean(happiness_score, na.rm = TRUE), .groups = "drop")

# Mitjana global de felicitat per any
df_global <- df %>%
  group_by(year) %>%
  summarise(global_mean = mean(happiness_score, na.rm = TRUE), .groups = "drop")

# Gràfica amb línies per continent + línia global
ggplot() +
  # Línies de cada continent
  geom_line(data = df_continente, aes(x = year, y = mean_score, color = continent, group = continent), size = 1.2) +
  geom_point(data = df_continente, aes(x = year, y = mean_score, color = continent), size = 2) +
  
  # Línia de mitjana global
  geom_line(data = df_global, aes(x = year, y = global_mean), color = "black", linetype = "dashed", size = 1.3) +
  geom_point(data = df_global, aes(x = year, y = global_mean), color = "black", size = 2.5) +
  
  # Estètica general
  labs(
    title = "Evolució de la Felicitat per Continent (amb línia global)",
    x = "Any",
    y = "Mitjana de felicitat"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = unique(df$year))

# Paso 1: Filtrar los 5 países más felices de 2015
top5_paises_2015 <- df %>%
  filter(year == 2015) %>%
  arrange(desc(happiness_score)) %>%
  slice(1:5) %>%
  pull(country)

# Paso 2: Filtrar datos solo para esos países
df_top5 <- df %>% filter(country %in% top5_paises_2015)

# Paso 3: Crear gráfico de línea por país
ggplot(df_top5, aes(x = factor(year), y = happiness_score, color = country, group = country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Evolución de la Felicidad en los Países Más Felices de 2015",
    x = "Año",
    y = "Puntuación de felicidad"
  ) +
  theme_minimal()

library(dplyr)
library(ggplot2)

# Calcular top 5 países por año
top5_paises_por_anio <- df %>%
  group_by(year) %>%
  slice_max(order_by = happiness_score, n = 5, with_ties = FALSE) %>%
  ungroup()

# Obtener todos los países que alguna vez estuvieron en el top 5
paises_top5 <- unique(top5_paises_por_anio$country)


# Filtrar dataset para esos países
df_top5_todos_anios <- df %>%
  filter(country %in% paises_top5)

# Crear gráfico de línea para todos los países top 5 de cada año
ggplot(df_top5_todos_anios, aes(x = factor(year), y = happiness_score, color = country, group = country)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Evolución de la Felicidad en Países que Alguna Vez Fueron Top 5 (2015–2024)",
    x = "Año",
    y = "Puntuación de felicidad"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())  # Para no mostrar título de leyenda

# Boxplot por año (sin outliers)
ggplot(df, aes(x = as.factor(year), y = happiness_score)) +
  geom_boxplot(fill = "lightgreen", color = "black", alpha = 0.7, outlier.shape = NA) +
  labs(title = "Distribución de la Felicidad Global por Año (sin outliers)",
       x = "Año",
       y = "Puntuación de felicidad") +
  theme_minimal()

# Hipótesis 5: Países con mayor cambio interno
df_diff <- df %>%
  group_by(country) %>%
  summarise(min_score = min(happiness_score, na.rm = TRUE),
            max_score = max(happiness_score, na.rm = TRUE),
            diff = max_score - min_score) %>%
  arrange(desc(diff)) %>%
  slice(1:10)
ggplot(df_diff, aes(x = reorder(country, diff), y = diff)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Top 10 països amb major canvi intern en felicitat",
       x = "Païs",
       y = "Diferència entre mínim y màxim") +
  theme_minimal()

# Complemento: Facet wrap por continente (generosidad)
ggplot(df, aes(x = generosity, y = happiness_score)) +
  geom_point(alpha = 0.5, color = "orange") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~ continent) +
  labs(title = "Relación entre Generosidad y Felicidad por Continente",
       x = "Generosidad",
       y = "Puntuación de felicidad") +
  theme_minimal()

# Mapa de felicidad
map_data <- df %>%
  group_by(country) %>%
  summarise(mean_score = mean(happiness_score, na.rm = TRUE))
sPDF <- joinCountryData2Map(map_data, joinCode = "NAME", nameJoinColumn = "country")
mapCountryData(sPDF, nameColumnToPlot = "mean_score",
               mapTitle = "Mapa Mundial de Felicidad Promedio (2015–2024)",
               colourPalette = "heat")
# Hipótesis 6: Esperança de vida s'asocia amb felicitat?
#Hipotesis 6#
library(ggplot2)
df_promedio <- df %>%
  group_by(healthy_life_expectancy) %>%
  summarise(mean_felicidad = mean(happiness_score, na.rm = TRUE), .groups = "drop")
h80_df <- df%>%filter(healthy_life_expectancy>80)
# Crear gráfico lineal general
ggplot(df_promedio, aes(x = healthy_life_expectancy, y = mean_felicidad, group = 1)) +
  geom_line(color = "darkorange", size = 1.2) +
  geom_point(size = 2, color = "black") +
  labs(
    title = "Relació Lineal Global: Esperança de Vida Saludable (categòrica) y Felicitat",
    x = "Esperança de Vida Saludable (categòrica)",
    y = "Mitja de Puntuació de Felicitat"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
library(plotly)
library(readr)

# Cargar el dataset pequeño que hayas subido (ejemplo: "dataset_pequeno.csv")
# Ordenar de forma descendente por esperanza de vida o felicidad
h80_df <- h80_df %>% arrange(desc(healthy_life_expectancy))

# Gráfico de barras
fig <- plot_ly(
  data = h80_df,
  x = ~reorder(country, healthy_life_expectancy),
  y = ~happiness_score,
  type = 'bar',
  marker = list(color = 'orange'),
  text = ~paste("<b>País:</b>", country,
                "<br><b>Esperança de Vida:</b>", healthy_life_expectancy,
                "<br><b>Felicitat:</b>", happiness_score),
  hoverinfo = 'text'
) %>%
  layout(
    title = list(
      text = "<b>Felicitat vs Esperança de Vida Saludable (Països amb Estrès)</b>",
      font = list(size = 15)
    ),
    xaxis = list(
      title = "<b>País</b>",
      tickangle = -45,
      tickfont = list(size = 14),
      titlefont = list(size = 16)
    ),
    yaxis = list(
      title = "<b>Puntuació de Felicitat</b>",
      tickfont = list(size = 14),
      titlefont = list(size = 16)
    ),
    hoverlabel = list(
      bgcolor = "white",
      font = list(size = 12)
    ),
    plot_bgcolor = "#f7f7f7",
    paper_bgcolor = "#f7f7f7"
  )

# Mostrar gráfico interactivo
fig

library(ggplot2)
library(readr)

# Convertir a numérico por si acaso
df$healthy_life_expectancy <- as.numeric(df$healthy_life_expectancy)

# Histograma básico
ggplot(df, aes(x = healthy_life_expectancy)) +
  geom_histogram(fill = "orange", color = "black", alpha = 0.7, bins = 30) +
  labs(
    title = "Distribución de la Esperanza de Vida Saludable",
    x = "Esperanza de Vida Saludable",
    y = "Número de Países"
  ) +
  theme_minimal()


# Alternativa: density plot
ggplot(df, aes(x = healthy_life_expectancy)) +
  geom_density(fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Densidad de la Esperanza de Vida Saludable",
    x = "Esperanza de Vida Saludable",
    y = "Densidad"
  ) +
  theme_minimal()

#Modelo multivariable
modelo <- lm(happiness_score ~ gdp_per_capita + social_support + healthy_life_expectancy + freedom + perceptions_of_corruption + generosity, data = df)
modelo_coef <- tidy(modelo) %>% filter(term != "(Intercept)")


ggplot(modelo_coef, aes(x = reorder(term, estimate), y = estimate)) +
  geom_col(fill = "orange", alpha = 0.8) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2, color = "darkorange") +
  coord_flip() +
  labs(title = "Importancia de los factores en la predicción de la felicidad", x = "Variable", y = "Coeficiente estimado") +
  theme_minimal()

# Hipótesis 7: Distribución de la felicidad por continente (Boxplot sin outliers)
library(dplyr)
library(ggplot2)


# Hipótesis 8: Distribución por región (Boxplot y violin plot)
# Calcular la media de felicidad por país
df_media_pais <- df %>%
  group_by(country, continent) %>%
  summarise(mean_happiness = mean(happiness_score, na.rm = TRUE), .groups = "drop")

# Boxplot con los datos normalizados
ggplot(df_media_pais, aes(x = continent, y = mean_happiness, fill = continent)) +
  geom_boxplot(color = "black", alpha = 0.7, outlier.shape = NA) +
  labs(
    title = "Distribución de la Felicidad Media por Continente (normalizado por país)",
    x = "Continente",
    y = "Media de felicidad por país"
  ) +
  theme_minimal()


ggplot(df, aes(x = region, y = happiness_score, fill = region)) +
  geom_violin(width = 1.7, color = "black", alpha = 0.7) +
  geom_boxplot(width = 0.3, color = "black", alpha = 0.7, outlier.shape = NA) +
  labs(title = "Distribución de la Felicidad por Región (sin outliers)", x = "Región", y = "Puntuación de felicidad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Hipótesis 9: Relación entre libertad y felicidad por continente
ggplot(df, aes(x = freedom, y = happiness_score)) +
  geom_point(alpha = 0.4, color = "darkorange") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~ continent, scales = "free") +
  labs(title = "Relación entre libertad y felicidad por continente", x = "Libertad", y = "Puntuación de felicidad") +
  theme_minimal()

# Hipótesis 10: Heatmap PIB - esperanza de vida saludable
df <- df %>%
  mutate(esperanza_bin = cut(healthy_life_expectancy, breaks = 10, include.lowest = TRUE),
         gdp_bin = cut(gdp_per_capita, breaks = 10, include.lowest = TRUE))
heatmap_data <- df %>%
  group_by(esperanza_bin, gdp_bin) %>%
  summarise(media_felicidad = mean(happiness_score, na.rm = TRUE), .groups = 'drop')
p <- ggplot(heatmap_data, aes(x = gdp_bin, y = esperanza_bin, fill = media_felicidad)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Media de felicidad") +
  labs(title = "Heatmap: PIB y Esperanza de Vida Saludable", x = "PIB (bin)", y = "Esperanza de Vida (bin)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(p, tooltip = "fill")



# Gráfico animado: Felicidad vs Apoyo Social por año
plot_ly(
  data = df,
  x = ~social_support,
  y = ~happiness_score,
  size = ~happiness_score,
  color = I("orange"),
  frame = ~year,
  text = ~paste("País:", country, "<br>Apoyo Social:", round(social_support, 2), "<br>Felicidad:", round(happiness_score, 2)),
  hoverinfo = "text",
  type = "scatter",
  mode = "markers",
  sizes = c(10, 100)
) %>%
  layout(
    title = "Felicidad vs Apoyo Social a lo largo de los años",
    xaxis = list(title = "Apoyo Social"),
    yaxis = list(title = "Puntuación de felicidad"),
    showlegend = FALSE
  ) %>%
  animation_opts(
    frame = 1000,
    transition = 0,
    redraw = TRUE
  ) %>%
  animation_slider(currentvalue = list(prefix = "Año: "))

# Gráfico interactivo: Felicidad vs Percepción de Corrupción por año
plot_ly(
  data = df,
  x = ~perceptions_of_corruption,
  y = ~happiness_score,
  size = ~happiness_score,
  frame = ~year,
  text = ~paste("País:", country, "<br>Corrupción:", round(perceptions_of_corruption, 2), "<br>Felicidad:", round(happiness_score, 2)),
  hoverinfo = "text",
  type = "scatter",
  mode = "markers",
  marker = list(color = "steelblue"),
  sizes = c(10, 100)
) %>%
  layout(
    title = "Felicidad vs Percepción de Corrupción (2015–2024)",
    xaxis = list(title = "Percepción de Corrupción"),
    yaxis = list(title = "Puntuación de felicidad"),
    showlegend = FALSE
  ) %>%
  animation_opts(frame = 1000, transition = 0, redraw = TRUE) %>%
  animation_slider(currentvalue = list(prefix = "Año: "))

# Gráfico interactivo: Felicidad vs Libertad por año
plot_ly(
  data = df,
  x = ~freedom,
  y = ~happiness_score,
  size = ~happiness_score,
  frame = ~year,
  text = ~paste("País:", country, "<br>Libertad:", round(freedom, 2), "<br>Felicidad:", round(happiness_score, 2)),
  hoverinfo = "text",
  type = "scatter",
  mode = "markers",
  marker = list(color = "orange"),
  sizes = c(10, 100)
) %>%
  layout(
    title = "Felicidad vs Libertad para Tomar Decisiones (2015–2024)",
    xaxis = list(title = "Libertad para tomar decisiones"),
    yaxis = list(title = "Puntuación de felicidad"),
    showlegend = FALSE
  ) %>%
  animation_opts(frame = 1000, transition = 0, redraw = TRUE) %>%
  animation_slider(currentvalue = list(prefix = "Año: "))


#Histograma llibertat interactiu
# Preparar datos
df_hist <- df %>%
  mutate(libertad_bin = round(freedom, 2)) %>%
  filter(!is.na(libertad_bin))

# Crear gráfico interactivo con eje Y automático y 1s por fotograma
plot_ly(
  data = df_hist,
  x = ~libertad_bin,
  frame = ~year,
  type = "histogram",
  nbinsx = 30,
  marker = list(color = "orange", line = list(color = "black", width = 1))
) %>%
  layout(
    title = "Distribución de la Libertad para Tomar Decisiones (2015–2024)",
    xaxis = list(title = "Libertad percibida"),
    yaxis = list(title = "Número de países", range = c(0, 21)),  # Eje Y automático
    bargap = 0.05
  ) %>%
  animation_opts(
    frame = 1000,
    transition = 0,    
    redraw = TRUE
  ) %>%
  animation_slider(currentvalue = list(prefix = "Año: "))

################################################################################
#ANIMACIÓN JSJSJSSJSJSJS
################################################################################
library(plotly)
library(dplyr)
library(readr)
library(tidyr)
# Calcular los top 10 países por año
top10_paises <- df %>%
  group_by(year) %>%
  top_n(10, happiness_score) %>%
  pull(country) %>%
  unique()

# Crear un dataframe completo con estos países en todos los años
todos_los_anios <- expand.grid(
  country = top10_paises,
  year = unique(df$year)
)

# Unir con los datos reales (para obtener los scores reales o NA si no estaban ese año)
df_completo <- todos_los_anios %>%
  left_join(df %>% select(country, year, happiness_score), by = c("country", "year")) %>%
  mutate(happiness_score = ifelse(is.na(happiness_score), 0, happiness_score))  # Rellenar con 0 o NA

# Crear gráfico interactivo con Plotly
fig <- plot_ly(
  data = df_completo,
  x = ~happiness_score,
  y = ~reorder(country, happiness_score),
  frame = ~year,
  type = 'bar',
  orientation = 'h',
  marker = list(color = 'yellow'),
  text = ~paste("País:", country, "<br>Puntuación:", round(happiness_score, 2)),
  hoverinfo = 'text'
) %>%
  layout(
    title = "Evolución de la Felicidad de los Países Top 10 (fluido y continuo)",
    xaxis = list(title = "Puntuación de felicidad"),
    yaxis = list(title = "País", categoryorder = "total ascending"),
    showlegend = FALSE
  ) %>%
  animation_opts(
    frame = 500,  # 0.5 segundos por frame
    transition = 300,  # Transición suave
    redraw = TRUE
  ) %>%
  animation_slider(currentvalue = list(prefix = "Año: "))

# Mostrar gráfico interactivo
fig
