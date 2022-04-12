# Unidades territoriales
source("02_SCRIPTS/analisis_revocacion.R")
library(tidyverse)

cartografia_secciones <- cartografia_secciones %>%
  mutate(id_entidad = entidad %>% as.numeric())

ut_sedes <- ue %>%
  group_by(ENTIDAD) %>%
  count(SEDE) %>%
  mutate(total = sum(n),
         porcentaje = 100*(n/total))

# Votos Secciones:
votos_seccion <- votos %>%
  group_by(id_entidad, entidad, seccion) %>%
  summarise(revocacion = sum(que_se_le_revoque_el_mandato_por_perdida_de_la_confianza, na.rm = T),
            no_revocacion = sum(que_siga_en_la_presidencia_de_la_republica,na.rm = T),
            total_votos_calculados = sum(total_votos_calculados, na.rm = T),
            lista_nominal = sum(lista_nominal, na.rm = T),
            abstencion = lista_nominal - total_votos_calculados,
            porcentaje_abstencion = 100*(abstencion/lista_nominal),
            porcentaje_participacion = 100-porcentaje_abstencion) %>%
  ungroup() %>%
  mutate(seccion = as.numeric(seccion))

# Datos UE
ut <- read_csv("01_DATOS/20220411_1845_REVOCACION_MANDATO_2022/CATALOGO_UNIDADES_TERRITORIALES_RM2022.csv",
               # skip = 1,
               locale = locale(encoding = "WINDOWS-1252")) %>%
  janitor::clean_names() %>%
  mutate(seccion = as.numeric(seccion))

secc_x_ut <- votos_seccion %>%
  right_join(ut) %>%
  group_by(id_entidad, unidad_territorial) %>%
  mutate(tasa_participacion_ut = 100*(sum(total_votos_calculados, na.rm = T)/sum(lista_nominal, na.rm = T))) %>%
  select(id_entidad, entidad, seccion, tasa_participacion_ut) %>%
  arrange(id_entidad, unidad_territorial) %>%
  ungroup()

mapa_ut <- left_join(cartografia_secciones,
          secc_x_ut,
          by = c("id_entidad" = "id_entidad",
                 "seccion" = "seccion")) %>%
  rename(NOM_ENT = entidad.y)

mapa_ut %>%
  mutate(jenk_break = jenkifyer(tasa_participacion_ut))

jenkifyer(mapa_ut$tasa_participacion_ut)

class(mapa_ut$tasa_participacion_ut)
na.omit(mapa_ut$tasa_participacion_ut)
part <- mapa_ut %>%
  filter(!is.na(tasa_participacion_ut)) %>%
  select(id_entidad, unidad_territorial, tasa_participacion_ut) %>%
  unique() %>%
  pull(tasa_participacion_ut)
plot(density(part))
quintiles_participacion <- quantile(part, seq(0, 1, by = 0.2))

paleta_participacion <- colorNumeric(palette = c("white", "brown"),
                                     domain = c(0, 100),
                                     na.color = "#bababa")
datos_mapa_ut <- mapa_ut %>%
  mutate(categoria_part = case_when(between(tasa_participacion_ut, 0, quintiles_participacion[2]) ~ "0 a 11.7%",
                                    between(tasa_participacion_ut, quintiles_participacion[2], quintiles_participacion[3]) ~ "11.7 a 15.9%",
                                    between(tasa_participacion_ut, quintiles_participacion[3], quintiles_participacion[4]) ~ "15.9 a 19.1%",
                                    between(tasa_participacion_ut, quintiles_participacion[4], quintiles_participacion[5]) ~ "19.1 a 23.1%",
                                    between(tasa_participacion_ut, quintiles_participacion[5], 100) ~ "23.1 a 100%"))
# %>%
#   filter(NOM_ENT == "MORELOS")

# datos_mapa_ut %>%
#   leaflet() %>%
#   # addProviderTiles("CartoDB.Positron") %>%
#   addPolygons(fillColor = paleta_participacion(datos_mapa_ut$tasa_participacion_ut),
#               fillOpacity = 1,
#               label = lapply(str_c("Municipio: ", datos_mapa_ut$nom_mun, "<br>",
#                                    "Porcentaje de participación: ", round(datos_mapa_ut$tasa_participacion_ut, 2), "%"),
#                              htmltools::HTML),
#               color = "transparent",
#               weight = 1) %>%
#   addLegend(position = "bottomright",
#             labFormat = labelFormat(suffix = "%"),
#             title = str_c("Porcentaje de<br>participación"),
#             pal = paleta_participacion,
#             values = 0:100)

datos_mapa_ut %>%
  ggplot(aes(fill = categoria_part)) +
  geom_sf(color = "transparent") +
  scale_fill_manual(values = wesanderson::wes_palettes$Zissou1) +
  theme_bw() +
  labs(title = str_c("Porcentaje de participación por unidad territorial del INE <br><b style = 'color:brown';>Revocación de Mandato</b>"),
       subtitle = str_glue("Datos del cómputo de la revocación a las {hora} del 11 de abril del 2022"),
       x = NULL, y = NULL,
       fill = "Porcentaje de participación",
       caption = "Fuente: Cómputos de la revocación de mandato - INE. Revocación de mandato, consultado en:
       https://computosrm2022.ine.mx/base-de-datos
       @JuvenalCamposF") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(face = "bold",family = "Mulish", size = 14, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Mulish", size = 9, hjust = 0.5),
        plot.caption = element_text(size = 5)) +
  guides(fill = guide_legend(title.hjust = 0.5,
                             title.position = "top",
                             ncol = 3))
# unidad_territorial
ggsave("03_RESULTADOS/graficas/unidad_territorial/participacion_unidad_territorial.png",
       device = "png",
       height = 16,
       width = 18,
       units = "cm")

# Estados ----

lapply(unique(datos_mapa_ut$NOM_ENT), function(estado_sel){

  datos_mapa_ut <- mapa_ut %>%
    mutate(categoria_part = case_when(between(tasa_participacion_ut, 0, quintiles_participacion[2]) ~ "0 a 11.7%",
                                      between(tasa_participacion_ut, quintiles_participacion[2], quintiles_participacion[3]) ~ "11.7 a 15.9%",
                                      between(tasa_participacion_ut, quintiles_participacion[3], quintiles_participacion[4]) ~ "15.9 a 19.1%",
                                      between(tasa_participacion_ut, quintiles_participacion[4], quintiles_participacion[5]) ~ "19.1 a 23.1%",
                                      between(tasa_participacion_ut, quintiles_participacion[5], 100) ~ "23.1 a 100%"))  %>%
    filter(NOM_ENT == estado_sel)



  datos_mapa_ut %>%
    ggplot(aes(fill = categoria_part)) +
    geom_sf(color = "transparent") +
    scale_fill_manual(values = wesanderson::wes_palettes$Zissou1) +
    theme_bw() +
    labs(title = str_c("Porcentaje de participación por unidad territorial del INE <br><b style = 'color:brown';>Revocación de Mandato</b>"),
         subtitle = str_glue("Datos del cómputo de la revocación a las {hora} del 11 de abril del 2022\nEntidad: {estado_sel}"),
         x = NULL, y = NULL,
         fill = "Porcentaje de participación",
         caption = "Fuente: Cómputos de la revocación de mandato - INE. Revocación de mandato, consultado en:
       https://computosrm2022.ine.mx/base-de-datos
       @JuvenalCamposF") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "bottom",
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          plot.title = ggtext::element_markdown(face = "bold",family = "Mulish", size = 14, hjust = 0.5),
          plot.title.position = "plot",
          plot.subtitle = element_text(family = "Mulish", size = 9, hjust = 0.5),
          plot.caption = element_text(size = 5)) +
    guides(fill = guide_legend(title.hjust = 0.5,
                               title.position = "top",
                               ncol = 3))
  # unidad_territorial
  ggsave(str_glue("03_RESULTADOS/graficas/unidad_territorial/estados/participacion_unidad_territorial{estado_sel}.png"),
         device = "png",
         height = 16,
         width = 18,
         units = "cm")
  print(estado_sel)

})


