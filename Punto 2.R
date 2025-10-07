###############################################################################
# Economía Urbana - Taller 1
# Ejercicio 2 - Revisión de Amenidades
#Autores: Juan Camilo Arévalo, Andrés Serrano y Juan Felipe Duarte
###############################################################################

################################################################################
# Sección 1: Limpieza de consola y preparación
################################################################################

#Consola limpia para iniciar a trabajar:
rm(list = ls())

#Instalamos pacman para poder instalar todos los paquetes
if (!require("pacman")) install.packages("pacman")  
library(pacman)

#Se instalan paquetes necesarios con el comando p_load
p_load(tidyverse, rio, skimr, here, leaflet, lubridate, modelsummary, viridis, sf,
       osmdata, dplyr, haven, stargazer, ggspatial, ragg, scales, fixest, FNN, 
       sandwich, lmtest, splines)

#Procedemos a definir el directorio
usuario <- Sys.info()[["user"]]

if (usuario == "JUANC") {
  directorio <- "C:/Users/JUANC/OneDrive - Universidad de los Andes/A-Uniandes/2025-20/Economía Urbana/Talleres/Taller 1" # Juan C.
  
} else if (usuario == " ") {
  directorio <- " " # Andrés
  
} else if (usuario == " ") {
  directorio <- " " # JF
  
} else if (usuario == " ") {
  directorio <- " " # Ingresar aquí el directorio para replicación
  
} else {
  stop("Usuario no reconocido. Por favor agregue su ruta.")
}
setwd(directorio)
getwd()

###############################################################################
# Sección 2: Importación y Limpieza de Datos
###############################################################################

###############################################################################
# 1) Importación Base de Ventas y Arriendos:
# Primero, vamos a importar la base de ventas y arriendos de los inmuebles:
sells_data <- readRDS("Data/Raw/dataTaller01_Amenidades.rds") %>% 
  as_tibble()

#Revisamos la existencia de Missings Values en el precio:
sum(is.na(sells_data$price)) #No hay missings en precio.
sum(is.na(sells_data$surface_total)) #No hay missings en el área.
sum(sells_data$price<0) #No hay valores negativos en el precio.
sum(sells_data$surface_total<0) #No hay valores negativos en el área.

# Eliminamos las filas donde el precio es igual a cero
sells_data <- sells_data %>%
  filter(price != 0)

#Eliminamos las filas para areas menores a 30 m2 y mayores a 300m2
sells_data <- sells_data %>%
  filter(surface_covered >= 30) %>%
  filter(surface_covered <=300)

#Se eliminan columnas innecesarias
sells_data <- sells_data %>%
  select(-start_date, -end_date, -price_period, -currency)

#Creamos la variable type con el tipo de propiedad, para clasificar las residenciales:
sells_data <- sells_data %>%
  mutate(type = word(title, 1))

#Revisamos valores únicos para poder filtrar los tipos que nos interesan de vivienda residencial:
unique(sells_data$type)
tipos_residenciales <- c("Apartamento", "Apartaestudio", "Casa", "Casalote",
                         "Aparrtamento", "APARTAESTUDIO", "APARTAMENTO",
                         "Casa-Local")
sells_data <- sells_data %>%
  filter(type %in% tipos_residenciales)

#Por último, acomodamos los valores para que type sea uniforme:
sells_data <- sells_data %>%
  mutate(type = case_when(
    str_to_lower(type) %in% c("apartamento", "aparrtamento", "apartamento") ~ "Apartamento",
    str_to_lower(type) == "apartaestudio" ~ "Apartaestudio",
    str_to_lower(type) %in% c("casa", "casalote", "Casa-Local") ~ "Casa",
    TRUE ~ type
  ))

#Se eliminan nuevas columnas innecesarias
sells_data <- sells_data %>%
  select(, -title, -description)


#Como la base tiene información sobre ventas y arriendos, se crea una dummy si es venta (sell =1)
sells_data <- sells_data %>%
  mutate(sell = ifelse(operation == "Venta", 1, 0))


#Guardamos la base general
save(sells_data, file = "Data/Modified/sells_data.dta")

#Verificamos que la latitud y la longitud estén en formato número:
sells_data$lat <- as.numeric(sells_data$lat)
sells_data$lon <- as.numeric(sells_data$lon)
#Se transforman a coordenadas:
sells_data_sf <- st_as_sf(sells_data, coords = c("lon", "lat"), crs = 4326)

#Se definen las coordenadas límite de Bogotá
lon_min <- -74.0; lon_max <- -74.21
lat_min <- 4.47;  lat_max <- 4.83


###############################################################################
# 2) Importación de datos de OSM de parques y plazas:

"Encontramos el shapefile de la ciudad de Bogotá por manzanas de la página de
Datos Abiertos Bogotá. Fuente: UAE Catastro Distrital"
bog_sf <- st_read("Data/Raw/Bogota/Bog.shp")

# Definimos el bounding box de Bogotá
bbox_bog <- c(lon_min, lat_min, lon_max, lat_max)

#Se descargan los datos de los parques de bogotá
parks_OSM_bog <- opq(bbox = bbox_bog) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

#Repetimos el proceso con los datos de las plazas (squares):
sq_OSM_bog <- opq(bbox = bbox_bog) %>%
  add_osm_feature(key = "place", value = "square") %>%
  osmdata_sf()

#Extraemos y dibujamos los puntos de plazas y parques:
Parques_puntos <- parks_OSM_bog$osm_points
Plazas_Puntos <- sq_OSM_bog$osm_points


################################################################################
# 3) Importación Censo Nacional 2018
"Con los datos descargados de Bogotá, se abre la base de hogares y de la geolocalización
y se unen para obtener una base unida que nos permita ver por manzana censal"
hogares <- read_dta("Data/Raw/CNPV2018_2HOG_A2_11.dta")
geo <- read_dta("Data/Raw/CNPV2018_MGN_A2_11.dta")
hogares_merged <- merge(hogares, geo, by = "cod_encuestas")

#Se eliminan columnas innecesarias y se guarda el archivo
hogares_merged <- hogares_merged %>%
  select(-u_dpto.x, -u_mpio.x, -ua_clase.x, -ha_nro_fall, -u_dpto.y, -u_mpio.y, -ua_clase.y)
save(hogares_merged, file = "Data/Modified/cnpv.dta")

#Se eliminan observaciones con missings en variables importantes:
hogares_merged<- hogares_merged %>%
  filter(!is.na(h_nro_cuartos), !is.na(h_nro_dormit), !is.na(ha_tot_per), !is.na(h_agua_cocin))

#Se filtran valores que no son desconocidos (menores a 99)
hogares_merged<- hogares_merged %>%
  filter(h_nro_cuartos < 99, h_nro_dormit < 99, h_agua_cocin < 99)

summary(hogares_merged)

################################################################################
# 4) Análisis Preliminares
################################################################################

################################################################################
# a) Estadísticas Descriptivas de los Precios:

#Creamos la tabla de composición de Viviendas en Bogotá
tabla <- table(sells_data$type)
prop.table(tabla)

#Revisión preliminar:
summary(sells_data_sf)

#Vamos a Segregar por ventas y arriendos:
# Filtrar por tipo
ventas <- sells_data %>% filter(sell == 1)
arriendos <- sells_data %>% filter(sell == 0)

#Se crea el logaritmo del precio para las ventas
ventas <- ventas %>%
  mutate(log_price = log(price))
#Variables de interés
vars <- c("log_price", "rooms", "bedrooms", "bathrooms", "surface_covered")

#Convertimos en Data Frames para exportar:
ventas <- as.data.frame(ventas)
arriendos <- as.data.frame(arriendos)

#Exportación de Características Descriptivas por Venta:
stargazer(ventas[, vars], type = "latex", summary = TRUE,
          title = "Estadísticas descriptivas - Ventas",
          covariate.labels = c("Log(Precio)", "Cuartos", "Alcobas", "Baños"),
          out = "Outputs/est_ventas.tex")

#Exportación de Características Descriptivas por Arriendo:
stargazer(arriendos[, vars], type = "latex", summary = TRUE,
          title = "Estadísticas descriptivas - Arriendos",
          covariate.labels = c("Precio", "Cuartos", "Alcobas", "Baños"),
          out = "Outputs/est_arriendos.tex")


################################################################################
# b) Densidad Poblacional por Manzana Censal


#Nos aseguramos de arreglar geometrías y convertir a sf:
densidad_sf <- st_as_sf(densidad)
densidad_sf <- st_make_valid(densidad_sf)

#Simplificamos para que se pueda analizar y graficar:
densidad_sf_simpl <- st_simplify(densidad_sf, dTolerance = 15)


#Graficamos el gradiente de densidades en Bogotá
Mapa_densidades <- ggplot() +
  geom_sf(data = bog_sf, fill = "grey98", color = "grey60", linewidth = 0.25) +
  geom_sf(data = densidad_sf_simpl, aes(fill = dens), color = NA) +
  scale_fill_viridis_c(
    option = "A",
    name   = "Habitantes/km²",
    labels = scales::label_number(accuracy = 1, big.mark = ",")
  ) +
  coord_sf(xlim = c(lon_min, lon_max),
           ylim = c(lat_min, lat_max),
           expand = FALSE, datum = NA) +
  annotation_scale(location = "bl", width_hint = 0.35) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  labs(
    title    = "Densidad habitacional por manzana — Bogotá",
    subtitle = "Croquis urbano · CNPV 2018 (manzana censal)",
    caption  = "Fuente: CNPV 2018 (DANE) · Límite urbano: UAECD"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    legend.position  = "right",
    legend.title     = element_text(face = "bold"),
    plot.title       = element_text(face = "bold")
  )

print(Mapa_densidades)
#Se exporta el mapa en la carpeta Outputs\Mapa_densidades.png

################################################################################
# c) Ubicación de Parques y Plazas

#Para comenzar, se identifican los parques como puntos.
parks_pts_fast <- NULL
if (!is.null(parks_OSM_bog$osm_polygons) && nrow(parks_OSM_bog$osm_polygons) > 0) {
  parks_poly <- parks_OSM_bog$osm_polygons |> st_make_valid()
  # simplificar antes del centroide para ganar velocidad
  parks_poly <- st_simplify(parks_poly, dTolerance = 20)
  parks_pts_fast <- st_point_on_surface(parks_poly)
}
if (is.null(parks_pts_fast) && !is.null(parks_OSM_bog$osm_points)) {
  parks_pts_fast <- parks_OSM_bog$osm_points
}

#Dado que hay demasiados parques, se toma una muestra de plazas y parques para graficar:
set.seed(31)
if (!is.null(parks_pts_fast) && nrow(parks_pts_fast) > 20000) {
  parks_pts_fast <- dplyr::slice_sample(parks_pts_fast, n = 20000)
}
if (!is.null(Plazas_Puntos) && nrow(Plazas_Puntos) > 8000) {
  Plazas_Puntos <- dplyr::slice_sample(Plazas_Puntos, n = 8000)
}

#Se construye el mapa de forma ligera para poder exportar:
mapa_pyp_bog <- ggplot() +
  geom_sf(data = bog_sf, fill = "grey98", color = "grey60", linewidth = 0.3) +
  { if (!is.null(parks_pts_fast))
    geom_sf(data = parks_pts_fast, aes(shape = "Parques"), color = "forestgreen",
            size = 0.4, alpha = 0.8) } +
  { if (!is.null(Plazas_Puntos))
    geom_sf(data = Plazas_Puntos,  aes(shape = "Plazas"),  color = "firebrick3",
            size = 0.4, alpha = 0.9) } +
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max), expand = FALSE, datum = NA) +
  annotation_scale(location = "bl", width_hint = 0.35) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering, height = unit(1,"cm"), width = unit(1,"cm")) +
  scale_shape_manual(name = "Amenidad", values = c("Parques" = 16, "Plazas" = 17)) +
  labs(title = "Parques y Plazas de Bogotá",
       subtitle = "Croquis urbano (puntos rápidos)",
       caption = "Fuente: OpenStreetMap (osmdata) · Límite: UAECD") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
        legend.position = c(0.10, 0.90),
        legend.background = element_rect(fill = "white", color = "grey80"),
        legend.title = element_text(face = "bold"))

print(mapa_pyp_bog)

#Por último, exportamos en forma de "fast-pass" con el paquete ragg:
ragg::agg_png("Outputs/mapa_pyp_bogota.png", width = 1600, height = 2000, res = 200)
print(mapa_pyp_bog)
dev.off()

################################################################################
# d) Gráfico de los precios por metro cuadrado de las propiedades
################################################################################

################################################################################
# 1 -> Ventas

#Se toma una muestra de las transacciones catalogadas como ventas para graficación
set.seed(31)
n_max_ventas <- 30000L
prop_v <- min(1, n_max_ventas / nrow(ventas_sample))
ventas_graf <- if (prop_v < 1) dplyr::slice_sample(ventas_sample, prop = prop_v) else ventas_sample

#Se filtran los valores válidos:
ventas_graf <- ventas_graf |> dplyr::filter(is.finite(p_m2), p_m2 > 0)

#Se grafica el mapa de gradientes de precios de vivienda:
pm2 <- ggplot() +
  # Croquis base
  geom_sf(data = bog_sf, fill = "grey98", color = "grey60", linewidth = 0.3) +
  # Puntos de ventas
  geom_sf(data = ventas_graf,
          aes(color = p_m2),
          size = 0.22, alpha = 0.85, show.legend = TRUE) +
  # Escala de color
  scale_color_viridis_c(
    option = "H",
    trans = "log10",
    name = "Precio/m² (COP)",
    labels = scales::label_number(accuracy = 1, big.mark = ",")
  ) +
  coord_sf(xlim = c(lon_min, lon_max),
           ylim = c(lat_min, lat_max),
           expand = FALSE, datum = NA) +
  annotation_scale(location = "bl", width_hint = 0.35) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  labs(
    title = "Precio por metro cuadrado — Ventas (Bogotá)",
    subtitle = paste0("Croquis urbano · muestra ≈ ", sprintf("%.0f", 100 * prop_v),
                      "% (n=", format(nrow(ventas_graf), big.mark = ","), ")"),
    caption = "Fuente: Muestra observacional · Límite: UAECD"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )


print(pm2)

#Se exporta bajo el método de "fast-pass"
ragg::agg_png("Outputs/pm2.png", width = 1800, height = 2200, res = 200)
print(pm2)
dev.off()


################################################################################
# 2 -> Arriendos

#Se toma submuestra para graficar:
set.seed(31)
n_max_arriendos <- 15000L
prop_r <- min(1, n_max_arriendos / nrow(arriendos_sample))
arriendos_graf <- if (prop_r < 1) dplyr::slice_sample(arriendos_sample, prop = prop_r) else arriendos_sample

#Se filtran los valores válidos:
arriendos_graf <- arriendos_graf |> dplyr::filter(is.finite(price), price > 0)

#Se grafica el mapa
rm2 <- ggplot() +
  geom_sf(data = bog_sf, fill = "grey98", color = "grey60", linewidth = 0.3) +
  geom_sf(data = arriendos_graf,
          aes(color = price),
          size = 0.25, alpha = 0.85, show.legend = TRUE) +
  scale_color_viridis_c(
    option = "H",
    trans = "log10",
    name = "Arriendo (COP/mes)",
    labels = scales::label_number(accuracy = 1, big.mark = ",")
  ) +
  coord_sf(xlim = c(lon_min, lon_max),
           ylim = c(lat_min, lat_max),
           expand = FALSE, datum = NA) +
  annotation_scale(location = "bl", width_hint = 0.35) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm")) +
  labs(
    title = "Valor de arriendos — Bogotá",
    subtitle = paste0("Croquis urbano · muestra ≈ ", sprintf("%.0f", 100 * prop_r),
                      "% (n=", format(nrow(arriendos_graf), big.mark = ","), ")"),
    caption = "Fuente: Muestra observacional · Límite: UAECD"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )


print(rm2)

#Se exporta mediante el "fast-pass"
ragg::agg_png("Outputs/map_arriendos_bogota.png", width = 1800, height = 2200, res = 200)
print(rm2)
dev.off()

################################################################################
# Sección 3: Gradientes de Precios y Renta
################################################################################

#Primero, nos traemos el punto del Centro Internacional
Centro_Internacional_OSM <- opq(bbox = bbox_bog) %>%
  add_osm_feature(key = "name", value = "Centro Internacional") %>%
  osmdata_sf()

Centro_Internacional_Puntos <- Centro_Internacional_OSM$osm_points

centro <- st_geometry(Centro_Internacional_Puntos)[1]

#Se hace un gráfico de Vista del Centro Internacional:
centro <- ggplot() +
  geom_sf(data = bog_sf, fill = "grey80", color = "grey60", size = 0.3) +
  geom_sf(data = Centro_Internacional_Puntos, color = "green", size = 1)+
  coord_sf(xlim = c(lon_min, lon_max), ylim = c(lat_min, lat_max)) +
  theme_minimal()
print(centro)

#Ahora sí, se unen las propiedades con la forma geográfica de Bogotá:

#Corregimos errores de forma y de shapefile:
bog_sf <- st_make_valid(bog_sf)

#Se une el shapefile con la información de ventas:
ventas_sf <- st_join(ventas_sf, bog_sf, join = st_intersects)

#Se obtiene un promedio por sector
promedio_sector <- ventas_sf %>%
  group_by(SECCODIGO) %>%
  summarize(P_m2_promedio = mean(p_m2, na.rm = TRUE)) %>%
  st_drop_geometry()  # se elimina la geometr?a porque solo interesa la tabla resumen

#Unimos el promedio por sector al shapefile:
bog_sf <- bog_sf %>%
  left_join(promedio_sector, by = "SECCODIGO")

#Calculamos la distancia al Centro Internacional de cada sector.
bog_sf <- bog_sf %>%
  mutate(distancia_centro = st_distance(geometry, centro) %>% as.numeric())

#Se eliminan observaciones atípicas (fuera de la ciudad)
bog_sf <- bog_sf %>%
  filter(distancia_centro <= 25000)

#Ahora sí, se puede proceder a graficar el gradiente de precios.
ggplot(bog_sf, aes(x = distancia_centro / 1000, y = P_m2_promedio.y)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred") +
  labs(x = "Distancia al centro (km)", y = "Precio por m² (venta)", 
       title = "Gradiente espacial: Precio por m² vs distancia al centro") +
  theme_minimal()

#Nota: Por tema del peso de los archivos y el tiempo, no nos fue posible graficar
#el gradiente por las zonas de Bogotá, por eso presentamos el gradiente a medida 
#que aumenta la distancia.


################################################################################
#Ahora, se repite el proceso para el cálculo del gradiente de arriendos:
arriendos_sf <- st_join(arriendos_sf, bog_sf, join = st_intersects)

promedio_sector_arriendos <- arriendos_sf %>%
  group_by(SECCODIGO) %>%
  summarize(r_promedio = mean(price, na.rm = TRUE)) %>%
  st_drop_geometry()  # se elimina la geometr?a porque solo interesa la tabla resumen

#Unimos el promedio por sector al shapefile:
bog_sf <- bog_sf %>%
  left_join(promedio_sector_arriendos, by = "SECCODIGO")

#Se grafica el gradiente de arriendos:
ggplot(bog_sf, aes(x = distancia_centro / 1000, y = r_promedio)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE, color = "lightblue") +
  labs(x = "Distancia al centro (km)", y = "Arriendo ($ COP)", 
       title = "Gradiente espacial: Renta vs distancia al centro") +
  theme_minimal()

################################################################################
#Por último, se calcula el gradiente de densidad
#Empezamos transformando los sf del DANE y de la UAECD para poder unirlos bajo un mismo CRS
densidad_sf <- st_transform(densidad_sf, st_crs(bog_sf))
densidad_sf <- st_join(densidad_sf, bog_sf, join = st_intersects)


#Se obtiene un promedio por sector
promedio_dens_sector <- densidad_sf %>%
  group_by(SECCODIGO) %>%
  summarize(dens_promedio = mean(dens, na.rm = TRUE)) %>%
  st_drop_geometry()  # se elimina la geometr?a porque solo interesa la tabla resumen

#Unimos el promedio por sector al shapefile
bog_sf <- bog_sf %>%
  left_join(promedio_dens_sector, by = "SECCODIGO")

#Graficamos el gradiente de densidad respecto a la distancia
ggplot(bog_sf, aes(x = distancia_centro / 1000, y = dens_promedio)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", se = FALSE, color = "lightgreen") +
  labs(x = "Distancia al centro (km)", y = "Densidad (Habitantes/km2)", 
       title = "Gradiente espacial: Densidad Poblacional vs distancia al centro") +
  theme_minimal()


################################################################################
# Sección 4: Espacios Abiertos y WTP
################################################################################

#Vamos a empezar con la definición de cercanía:

# CRS métrico (Bogotá)
crs_metric <- 3116
bog_m     <- st_transform(st_make_valid(bog_sf), crs_metric)
ventas_m  <- st_transform(ventas_sf, crs_metric)
centro_m  <- st_transform(st_as_sf(centro), crs_metric)

# Si por alguna razón ventas no tiene SECCODIGO, rehacer el join
if (!"SECCODIGO" %in% names(ventas_m)) {
  ventas_m <- st_join(ventas_m, bog_m, join = st_intersects)
}

#Se descargan polígonos de parques/plazas de Bogotá
osm_parks <- opq(bbox = bbox_bog) |>
  add_osm_feature(key = "leisure", value = "park") |>
  osmdata_sf()

osm_squares <- opq(bbox = bbox_bog) |>
  add_osm_feature(key = "place", value = "square") |>
  osmdata_sf()

#Unimos los polígonos:
parks_poly <- dplyr::bind_rows(osm_parks$osm_polygons,  osm_parks$osm_multipolygons)
sq_poly    <- dplyr::bind_rows(osm_squares$osm_polygons, osm_squares$osm_multipolygons)

#Limpiamos los polígonos:
poly_union_bog <- st_union(st_geometry(bog_m))

prep_poly <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(NULL)
  x |>
    st_make_valid() |>
    st_transform(crs_metric) |>
    suppressWarnings(st_intersection(poly_union_bog)) |>
    mutate(area_m2 = as.numeric(st_area(geometry)))
}

parks_poly <- prep_poly(parks_poly)
sq_poly    <- prep_poly(sq_poly)

#Filtramos por un área mínima para evitar sectores muy pequeños incomparables:
#    Ajusta los umbrales si lo deseas (2.000 m² y 1.000 m² son razonables)
if (!is.null(parks_poly)) parks_poly <- parks_poly |> filter(area_m2 >= 2000)
if (!is.null(sq_poly))    sq_poly    <- sq_poly    |> filter(area_m2 >= 1000)

#Unimos los espacios abiertos
open_poly <- NULL
if (!is.null(parks_poly) & !is.null(sq_poly)) {
  open_poly <- st_union(rbind(parks_poly["area_m2"], sq_poly["area_m2"]))
} else if (!is.null(parks_poly)) {
  open_poly <- st_union(parks_poly["area_m2"])
} else if (!is.null(sq_poly)) {
  open_poly <- st_union(sq_poly["area_m2"])
} else {
  stop("No se encontraron polígonos válidos de parques/plazas en OSM para el bbox definido.")
}

#Se construyen las distancias a los bordes de los polígonos que tienen espacios abiertos (en m y km)
ventas_m$dist_open_m  <- as.numeric(st_distance(ventas_m, open_poly))
ventas_m$dist_open_km <- ventas_m$dist_open_m / 1000

#Se crean buffers de dustancias caminables (cercanía a 300m y 500m)
ventas_m <- ventas_m |>
  mutate(within_300m_open = as.integer(dist_open_m <= 300),
         within_500m_open = as.integer(dist_open_m <= 500))

#Se calculan las distancias al CBD (Centro Internacional)
ventas_m$dist_cbd_km <- as.numeric(st_distance(ventas_m, centro_m)) / 1000

#Se obtiene el logaritmo de precios para hacer análisis
ventas_m$log_pm2 <- log(ventas_m$p_m2)

#Resumimos las estadísticas descriptivas de la proporción de hogares con cercanía a parques
desc_cercania <- ventas_m |>
  st_drop_geometry() |>
  summarise(
    n            = dplyr::n(),
    p_within300  = mean(within_300m_open, na.rm=TRUE),
    p_within500  = mean(within_500m_open, na.rm=TRUE),
    q25_dist_km  = quantile(dist_open_km, .25, na.rm=TRUE),
    med_dist_km  = median(dist_open_km, na.rm=TRUE),
    q75_dist_km  = quantile(dist_open_km, .75, na.rm=TRUE)
  )
print(desc_cercania)

desc_cercania_df <- as.data.frame(desc_cercania)

#Exportamos a formato LaTeX
stargazer(
  desc_cercania_df,
  summary = FALSE,
  rownames = FALSE,
  title = "Descripción de cercanía a amenidades",
  label = "tab:desc_cercania",
  digits = 3,
  out = "Outputs/desc_cercania.tex",
  header = FALSE,
  type = "latex"
)


################################################################################
#Construcción del Modelo Hedónico

#Limpiamos las variables de valores NA:
dfv <- ventas_m |>
  st_drop_geometry() |>
  mutate(
    SECCODIGO = as.factor(SECCODIGO),
    tipo      = as.factor(type)
  ) |>
  filter(
    is.finite(log_pm2),
    is.finite(dist_open_km),
    is.finite(dist_cbd_km),
    is.finite(rooms),
    is.finite(bedrooms),
    is.finite(bathrooms),
    is.finite(surface_covered)
  )

#Primer Modelo: Precio ~ Distancias + Controles + FE de sector
mA <- feols(
  log_pm2 ~ dist_open_km + dist_cbd_km +
    rooms + bedrooms + bathrooms + surface_covered + tipo | SECCODIGO,
  data = dfv, vcov = "HC1"
)

#Segundo Modelo: Precio ~ Distancias + Controles + FE de sector, para viviendas <=300m
# de distancia de un parque
mB <- feols(
  log_pm2 ~ within_300m_open + dist_cbd_km +
    rooms + bedrooms + bathrooms + surface_covered + tipo | SECCODIGO,
  data = dfv, vcov = "HC1"
)

#Ajustamos los modelos con Clusters por Sector Urbano:
mA_cl <- feols(formula(mA), data = dfv, cluster = "SECCODIGO")
mB_cl <- feols(formula(mB), data = dfv, cluster = "SECCODIGO")

#Creamos y exportamos la tabla resumen de los modelos:
etable(
  mA, mB, mA_cl, mB_cl,
  dict = c(
    log_pm2          = "log(Precio m²)",
    dist_open_km     = "Dist km a espacio abierto",
    within_300m_open = "≤300 m de espacio abierto",
    dist_cbd_km      = "Dist km al CBD",
    rooms            = "Cuartos",
    bedrooms         = "Alcobas",
    bathrooms        = "Baños",
    surface_covered  = "Área (m²)",
    tipo             = "Tipo",
    SECCODIGO        = "FE Sector"
  ),
  se.below = TRUE,
  fitstat  = ~ n + r2
)

#Para interpretar el WTP, tomamos los coeficientes
betaA <- coef(mA)["dist_open_km"]
betaB <- coef(mB)["within_300m_open"]

wtpA_pct    <- 100 * (exp(betaA) - 1)     # % por +1 km más lejos del espacio abierto
wtpB_pct    <- 100 * (exp(betaB) - 1)     # % prima por estar ≤300 m
med_pm2     <- median(dfv$p_m2, na.rm = TRUE)
wtpA_COP_m2 <- med_pm2 * (exp(betaA) - 1)
wtpB_COP_m2 <- med_pm2 * (exp(betaB) - 1)

cat(
  "\n[WTP]\n",
  "• +1 km lejos del espacio abierto: ", sprintf("%.2f", wtpA_pct), "%  (≈ ",
  format(round(wtpA_COP_m2, 0), big.mark = ","), " COP/m²)\n",
  "• Prima por ≤300 m: ", sprintf("%.2f", wtpB_pct), "%  (≈ ",
  format(round(wtpB_COP_m2, 0), big.mark = ","), " COP/m²)\n",
  sep = ""
)


################################################################################
# Análisis de no linealidad


# ---- A) Spline en distancia a espacio abierto ----
# bs() = base spline cúbico (función estándar en R)
mA_spline <- feols(
  log_pm2 ~ bs(dist_open_km, df = 3) + dist_cbd_km +
    rooms + bedrooms + bathrooms + surface_covered + tipo | SECCODIGO,
  data = dfv, vcov = "HC1"
)

etable(
  mA_spline,
  se.below = TRUE,
  dict = c(log_pm2 = "log(Precio m²)",
           dist_open_km = "Spline distancia a espacio abierto",
           dist_cbd_km = "Distancia (km) al CBD",
           rooms = "Cuartos", bedrooms = "Alcobas",
           bathrooms = "Baños", surface_covered = "Área (m²)",
           tipo = "Tipo", SECCODIGO = "FE Sector")
)

#Cálculo de estadísticas por bandas de distancia:
#Se crean tres bandas de distancia: ≤300m, 301–600m y >600m
dfv <- dfv |>
  mutate(band_300 = cut(
    dist_open_m,
    breaks = c(-Inf, 300, 600, Inf),
    labels = c("≤300m", "301–600m", ">600m")
  ))

#Calculamos los promedios de características por bandas:
balance_300 <- dfv |>
  group_by(band_300) |>
  summarise(
    across(
      c(rooms, bedrooms, bathrooms, surface_covered, dist_cbd_km),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    n = dplyr::n()
  )

print(balance_300)

#Exportamos la tabla con stargazer
balance_300_df <- as.data.frame(balance_300)

stargazer(
  balance_300_df,
  summary = FALSE,
  rownames = FALSE,
  digits = 2,
  title = "Balance descriptivo por proximidad (≤300 m)",
  label = "tab:balance_300",
  type = "latex",
  header = FALSE,
  out = "balance_300.tex"
)


################################################################################
# Estimador de Diferencias Espaciales:

#Sacamos promedios por sector:
agg_sec <- dfv |>
  group_by(SECCODIGO) |>
  summarise(log_pm2_bar = mean(log_pm2, na.rm=TRUE),
            dist_open_km_bar = mean(dist_open_km, na.rm=TRUE),
            dist_cbd_km_bar  = mean(dist_cbd_km, na.rm=TRUE),
            .groups = "drop")

#Creamos centroides por sector:
bog_valid <- st_make_valid(bog_m)
centros   <- st_centroid(bog_valid)
centros_df <- data.frame(SECCODIGO = bog_valid$SECCODIGO,
                         st_coordinates(centros))
colnames(centros_df)[2:3] <- c("lon","lat")

agg_sec <- agg_sec |> left_join(centros_df, by="SECCODIGO") |> na.omit()

#Tomamos el vecino más cercano (k=1)
coords <- as.matrix(agg_sec[, c("lon","lat")])
nn     <- FNN::get.knn(coords, k=1)$nn.index[,1]
pairs  <- data.frame(i = seq_len(nrow(agg_sec)), j = nn)

#Revisamos las diferencias por par
sfd <- pairs |>
  transmute(
    pair_id   = row_number(),
    d_logpm2  = agg_sec$log_pm2_bar[i]   - agg_sec$log_pm2_bar[j],
    d_distOS  = agg_sec$dist_open_km_bar[i] - agg_sec$dist_open_km_bar[j],
    d_distCBD = agg_sec$dist_cbd_km_bar[i]  - agg_sec$dist_cbd_km_bar[j]
  ) |>
  na.omit()


#Hacemos la estimación por Primeras Diferencias Espaciales:
mSFD <- feols(d_logpm2 ~ 0 + d_distOS + d_distCBD, data = sfd, cluster = "pair_id")
etable(mSFD, se.below = TRUE)

#Exportamos con modelsummary
modelsummary(
  mSFD,
  output = "latex",
  title = "Efecto de la distancia sobre el precio por m² (modelo SFD)",
  stars = TRUE,
  coef_map = c(
    "d_distOS"  = "Cambio distancia a espacio abierto (km)",
    "d_distCBD" = "Cambio distancia al CBD (km)"
  ),
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  notes = "Errores estándar agrupados por par de observaciones (pair_id)."
)


################################################################################
#Otra forma: Errores estándar de Conley:

#Tomamos las coordenadas
ventas_ll <- st_transform(ventas_sf, 4326)
xy <- st_coordinates(ventas_ll)
dfv$lon_deg <- xy[, 1]
dfv$lat_deg <- xy[, 2]

#Creamos el modelo base
f_base <- log_pm2 ~ dist_open_km + dist_cbd_km +
  rooms + bedrooms + bathrooms + surface_covered + tipo | SECCODIGO

#Creamos el Conley con un kernel triangular
mA_conley <- feols(
  f_base,
  data = dfv,
  vcov = fixest::vcov_conley(
    lat = ~lat_deg,
    lon = ~lon_deg,
    distance = "triangular"   # métrica espacial (no distancia en km)
  )
)

#Creamos la tabla comparativa con los primeros modelos que estimamos:
etable(
  mA, mA_cl, mA_conley,
  headers = c("OLS-FE (HC1)", "OLS-FE [Cluster SECCODIGO]", "OLS-FE [Conley triangular]"),
  dict = c(
    log_pm2 = "log(Precio m²)", dist_open_km = "Dist km a espacio abierto",
    dist_cbd_km = "Dist km al CBD", rooms = "Cuartos", bedrooms = "Alcobas",
    bathrooms = "Baños", surface_covered = "Área (m²)", tipo = "Tipo",
    SECCODIGO = "FE Sector"
  ),
  se.below = TRUE, fitstat = ~ n + r2
)

#Exportamos la tabla comparativa en una sola tabla:
coef_labels <- c(
  "dist_open_km"    = "Dist. km a espacio abierto",
  "dist_cbd_km"     = "Dist. km al CBD",
  "rooms"           = "Cuartos",
  "bedrooms"        = "Alcobas",
  "bathrooms"       = "Baños",
  "surface_covered" = "Área (m²)",
  "tipo"            = "Tipo",
  "SECCODIGO"       = "FE Sector"
)

modelsummary(
  list(
    "OLS-FE (HC1)"              = mA,
    "OLS-FE [Cluster SECCODIGO]" = mA_cl,
    "OLS-FE [Conley triangular]" = mA_conley
  ),
  output = "latex",
  coef_map = coef_labels,
  statistic = "std.error",
  gof_map = tibble::tibble(
    raw = c("nobs", "r.squared"),
    clean = c("Observaciones", "R²"),
    fmt = 3
  ),
  title = "Efecto de la distancia a espacios abiertos sobre el precio por m²",
  notes = "Errores estándar entre paréntesis. Estimaciones con efectos fijos por sector (SECCODIGO).",
  stars = TRUE,
  align = "lccc",
  escape = FALSE
)

################################################################################
################################################################################
#Fin del Taller