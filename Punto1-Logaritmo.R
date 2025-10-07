###############################################################################
# Economía Urbana - Taller 1
# Ejercicio 1 - Construción indices
###############################################################################
# Primero descargamos librerias necesarias
library(tidyverse)
library(fixest)
library(sandwich)
library(lmtest)
library(clubSandwich)
library(Matrix)
library(dplyr)
library(ggplot2)
library(readr)


#Importamos el Database 
# ===============================
data <- readRDS("dataTaller01_PriceIndeces.Rds")

#Eliminamos mising values por año y precio;convertimos precio a logaritmo
data <- data %>%
  filter(!is.na(sale_price), !is.na(year)) %>%
  mutate(log_price = log(sale_price))

# ===============================
# 2. Modelos econométricos
# ===============================

# (a) Modelo Hedónico (log)
modelo_hedonico_log <- feols(
  log_price ~ factor(year) +
    building_sqft + land_sqft + num_bedrooms + num_rooms +
    num_full_baths + num_half_baths + num_fireplaces +
    factor(type_of_residence) + factor(construction_quality) +
    factor(attic_finish) + factor(garage_attached) +
    garage_area_included + garage_size +
    factor(garage_ext_wall_material) + factor(attic_type) +
    factor(basement_type) + factor(ext_wall_material) +
    factor(central_heating) + factor(basement_finish) +
    factor(roof_material) + factor(site_desirability) +
    factor(renovation) + factor(recent_renovation) +
    factor(porch) + factor(central_air),
  data = data
)

# (b) Modelo Repeat Sales (log)
repeat_sales <- data %>%
  group_by(pin) %>%
  filter(n() >= 2) %>%
  arrange(year) %>%
  mutate(
    log_diff = log_price - lag(log_price),
    year_diff = year - lag(year)
  ) %>%
  filter(!is.na(log_diff), year_diff > 0)

modelo_repeat_sales_log <- feols(
  log_diff ~ factor(year),
  data = repeat_sales
)

# (c) Modelo de Efectos Fijos (log)
modelo_fe_log <- feols(
  log_price ~ factor(year) +
    building_sqft + land_sqft + num_bedrooms + num_rooms +
    num_full_baths + num_half_baths + num_fireplaces +
    factor(type_of_residence) + factor(construction_quality) +
    factor(attic_finish) + factor(garage_attached) +
    garage_area_included + garage_size +
    factor(garage_ext_wall_material) + factor(attic_type) +
    factor(basement_type) + factor(ext_wall_material) +
    factor(central_heating) + factor(basement_finish) +
    factor(roof_material) + factor(site_desirability) +
    factor(renovation) + factor(recent_renovation) +
    factor(porch) + factor(central_air) | pin,
  data = data
)
## actualizamos el modelo teniendo en cuenta problemas de colinialidad
# ===============================
# 3. Variables omitidas
# ===============================
variables_modelo <- all.vars(
  update(log_price ~ building_sqft + land_sqft + num_bedrooms + num_rooms +
           num_full_baths + num_half_baths + num_fireplaces +
           type_of_residence + construction_quality +
           attic_finish + garage_attached + garage_area_included +
           garage_size + garage_ext_wall_material + attic_type +
           basement_type + ext_wall_material + central_heating +
           basement_finish + roof_material + site_desirability +
           renovation + recent_renovation + porch + central_air, ~.)
)

omitidas <- data.frame(
  Variable = variables_modelo,
  Missing = sapply(variables_modelo, function(v) sum(is.na(data[[v]]))),
  Unicos = sapply(variables_modelo, function(v) length(unique(data[[v]])))
) %>%
  mutate(Omitida = ifelse(Missing > 0 | Unicos == 1, "Sí", "No"))

                  
# ===============================
# 4. Índices de precios
# ===============================
extraer_indice <- function(modelo, metodo) {
  coefs <- data.frame(term = names(coef(modelo)), estimate = coef(modelo))
  patron <- "factor\\(year\\)"
  
  coefs %>%
    filter(grepl(patron, term)) %>%
    mutate(
      year = as.numeric(gsub(patron, "", term)),
      index = exp(estimate - min(estimate, na.rm = TRUE)) * 100,
      method = metodo
    ) %>%
    select(year, index, method)
}

hedonico_index <- extraer_indice(modelo_hedonico_log, "Hedónico")
repeat_index <- extraer_indice(modelo_repeat_sales_log, "Repeat Sales")
fe_index <- extraer_indice(modelo_fe_log, "Efectos Fijos")

all_indexes <- bind_rows(hedonico_index, repeat_index, fe_index)

# ===============================
# 5. Gráfica de comparación
# ===============================
ggplot(all_indexes, aes(x = year, y = index, color = method, linetype = method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Comparación de Índices de Precios (Logarítmicos)",
    subtitle = "Hedónico, Repeat Sales y Efectos Fijos",
    x = "Año", y = "Índice (base 100)",
    color = "Método", linetype = "Método"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    legend.position = "bottom"
  )

ggsave("Comparacion_Indices_LogPrice.png", width = 8, height = 5, dpi = 300)

