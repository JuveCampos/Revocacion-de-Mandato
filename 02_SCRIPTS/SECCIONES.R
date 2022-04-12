# SECCIONES ----
source("02_SCRIPTS/analisis_revocacion.R")
library(cartogram)

# SECCIÓN ----
votos_seccion <- votos %>%
  group_by(id_entidad, entidad, seccion) %>%
  summarise(revocacion = sum(que_se_le_revoque_el_mandato_por_perdida_de_la_confianza, na.rm = T),
            no_revocacion = sum(que_siga_en_la_presidencia_de_la_republica,na.rm = T),
            total_votos_calculados = sum(total_votos_calculados, na.rm = T),
            lista_nominal = sum(lista_nominal, na.rm = T),
            abstencion = lista_nominal - total_votos_calculados,
            porcentaje_abstencion = 100*(abstencion/lista_nominal),
            porcentaje_participacion = 100-porcentaje_abstencion
  ) %>%
  ungroup()

# Secciones con más y menos abstención:
secciones_con_mas_abstencion <- votos_seccion %>%
  filter(between(porcentaje_abstencion, 0, 100)) %>%
  arrange(porcentaje_abstencion) %>%
  select(id_entidad, entidad, seccion, porcentaje_abstencion)

# Burbujas de votos seccion:


mapa_secciones <- cartografia_secciones %>%
  janitor::clean_names() %>%
  mutate(entidad = as.numeric(entidad)) %>%
  left_join(votos_seccion %>%
              rename(nom_ent = entidad) %>%
              mutate(seccion = as.numeric(seccion)),
            by = c("entidad" = "id_entidad",
                   "seccion" = "seccion"))

entidad_sel <- "MORELOS"
mapa_secciones_sel <- mapa_secciones %>%
  filter(nom_ent == entidad_sel)

geometrias_validas <- mapa_secciones_sel %>%
  st_is_valid() %>%
  which()

dorling_secciones_sel <- mapa_secciones %>%
  slice(geometrias_validas) %>%
  st_transform(crs = 2163) %>%
  filter(!is.na(total_votos_calculados)) %>%
  cartogram_dorling("total_votos_calculados") %>%
  mutate(X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2])

# Mapa de polígonos:
ggplot(mapa_secciones_sel,
       aes(fill = porcentaje_participacion)) +
  geom_sf(color = "transparent") +
  scale_fill_gradientn(colors = c("white", "brown"))

# Cartograma de Dorling:
ggplot(dorling_secciones_sel,
       aes(fill = porcentaje_participacion)) +
  geom_sf(color = "transparent") +
  scale_fill_gradientn(colors = c("white", "brown"))

