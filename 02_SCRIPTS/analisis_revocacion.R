# Librerias ----
library(tidyverse)
library(sf)
library(leaflet)
library(waffle)

# Datos ----
rm(list = ls())
hora <- "18:45"
hora_fmt <- str_replace(hora, pattern = "\\:", replacement = "")

# Catálogo de secciones:
cat_secciones <- readRDS("01_DATOS/00_01_Catalogos/catalogo_secciones.rds") %>%
  select(id_entidad = cve_edo, nom_mun_ine, cve_seccion, cve_mun)

# Cartografías:
list.files("01_DATOS/00_02_Cartografia/CARTOGRAFIAS INEGI INE/INE/INE MARCO GEOGRAFICO SECCIONAL/MGS CCL/Shapefile/")
cartografia_mpios <- read_sf("01_DATOS/00_02_Cartografia/CARTOGRAFIAS INEGI INE/INE/INE MARCO GEOGRAFICO SECCIONAL/MGS CCL/Shapefile/MUNICIPIO.shp")
cartografia_edos <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")
cartografia_secciones <- readRDS("01_DATOS/00_02_Cartografia/mis cartografias/shape.rds")

# Datos del prep
prep <- read_csv(str_glue("01_DATOS/20220411_{hora_fmt}_REVOCACION_MANDATO_2022/20220411_{hora_fmt}_COMPUTOS_RM2022.csv"),
                 locale = locale(encoding = "WINDOWS-1252"),
                 skip = 5) %>%
  janitor::clean_names() %>%
  mutate(cve_seccion = as.numeric(seccion)) %>%
  left_join(cat_secciones, by = c("id_entidad", "cve_seccion"))

# Incidentes
unique(prep$observaciones)
sin_papeletas <- prep %>%
  filter(observaciones == "Paquete sin papeletas")

# Votos (Numerico)
votos <- prep %>%
  mutate(across(.cols = que_se_le_revoque_el_mandato_por_perdida_de_la_confianza:lista_nominal,
                .fns = as.numeric))

# Casillas con datos registrados:
casillas_con_datos_registrados <- prep %>%
  select(lista_nominal) %>%
  mutate(lista_nominal = as.numeric(lista_nominal)) %>%
  filter(!is.na(lista_nominal)) %>%
  nrow()

