# ==========================================
# ÍNDICES DE PRECIOS EN NIVELES
# Metodologías: Hedónico, Repeat Sales y Efectos Fijos
# ==========================================

# Cargar librerías
library(tidyverse)
library(fixest)
library(sandwich)
library(lmtest)
library(clubSandwich)
library(Matrix)
library(dplyr)
library(ggplot2)
library(readr)

# ===============================
# 1. Cargar y preparar datos
# ===============================
# Asegúrate de ajustar la ruta si el CSV está en otra carpeta
data <- readRDS("dataTaller01_PriceIndeces.Rds")

# Eliminar valores faltantes en las variables clave
data <- data %>%
  filter(!is.na(sale_price), !is.na(year))

# ===============================
# 2. Modelo Hedónico (niveles, sin clustering)
# ===============================
modelo_hedonico_niveles <- feols(
  sale_price ~ factor(year) +
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

# ===============================
# 3. Modelo Repeat Sales (diferencias dobles)
# ===============================
# Crear dataset de ventas repetidas
repeat_sales <- data %>%
  group_by(pin) %>%
  filter(n() >= 2) %>%
  arrange(year) %>%
  mutate(
    price_diff = sale_price - lag(sale_price),
    year_diff = year - lag(year)
  ) %>%
  filter(!is.na(price_diff), year_diff > 0)

modelo_repeat_sales <- feols(
  price_diff ~ factor(year),
  data = repeat_sales
)

# ===============================
# 4. Modelo de Efectos Fijos (FE)
# ===============================
modelo_fe_niveles <- feols(
  sale_price ~ factor(year) +
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

# ===============================
# 5. Función automática para construir índices
# ===============================
extraer_indice <- function(modelo, metodo) {
  coefs <- data.frame(
    term = names(coef(modelo)),
    estimate = as.numeric(coef(modelo))
  )
  
  patrones <- c("factor\\(year\\)", "year::", "year_", "year")
  patron_valido <- patrones[sapply(patrones, function(p) any(grepl(p, coefs$term)))]
  
  if (length(patron_valido) == 0) {
    warning(paste(" No se detectaron efectos de año para el modelo:", metodo))
    return(data.frame(year = numeric(0), index = numeric(0), method = metodo))
  }
  
  patron_usado <- patron_valido[1]
  message(paste("Patrón de año detectado para", metodo, ":", patron_usado))
  
  coefs %>%
    filter(grepl(patron_usado, term)) %>%
    mutate(
      year = as.numeric(gsub(patron_usado, "", term)),
      index = (estimate - min(estimate, na.rm = TRUE)) + 100,
      method = metodo
    ) %>%
    select(year, index, method)
}

# ===============================
# 6. Construir índices
# ===============================
hedonico_index <- extraer_indice(modelo_hedonico_niveles, "Hedónico")
repeat_index <- extraer_indice(modelo_repeat_sales, "Repeat Sales")
fe_index <- extraer_indice(modelo_fe_niveles, "Efectos Fijos")

# Combinar todo
all_indexes <- bind_rows(hedonico_index, repeat_index, fe_index)

# ===============================
# 7. Graficar índice comparativo
# ===============================
ggplot(all_indexes, aes(x = year, y = index, color = method, group = method)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Índice de Precios de Vivienda — Cook County (Modelos en Niveles)",
    subtitle = "Comparación entre metodologías Hedónica, Repeat Sales y Efectos Fijos",
    x = "Año",
    y = "Índice (Base 100)",
    color = "Metodología"
  ) +
  theme_minimal(base_size = 14)
