source("02_SCRIPTS/analisis_revocacion.R")

library(cartogram)

# Mapa Entidades:
abb <- readRDS("01_DATOS/00_01_Catalogos/abreviaturas_entidad") %>%
  select(-entidad) %>%
  rename(abb = ent)

isDark <- function(colr) { (sum( col2rgb(colr) * c(299, 587,114))/1000 < 123) }

votos_estados <- votos %>%
  group_by(id_entidad, entidad) %>%
  summarise(revocacion = sum(que_se_le_revoque_el_mandato_por_perdida_de_la_confianza, na.rm = T),
            no_revocacion = sum(que_siga_en_la_presidencia_de_la_republica,na.rm = T),
            total_votos_calculados = sum(total_votos_calculados, na.rm = T),
            lista_nominal = sum(lista_nominal, na.rm = T),
            abstencion = lista_nominal - total_votos_calculados,
            porcentaje_abstencion = 100*(abstencion/lista_nominal),
            porcentaje_participacion = 100-porcentaje_abstencion) %>%
  ungroup() %>%
  filter(entidad != "VOTO EN EL EXTRANJERO")

mapa_entidad <- cartografia_edos %>%
  mutate(CVE_EDO = as.numeric(CVE_EDO)) %>%
  left_join(votos_estados, by = c("CVE_EDO" = "id_entidad")) %>%
  left_join(abb, by  = c("CVE_EDO" = "cve_ent"))
plot(mapa_entidad, max.plot = 1)
class(mapa_entidad)

estados_dorling <- mapa_entidad %>%
  st_transform(crs = 2163) %>%
  cartogram_dorling("total_votos_calculados") %>%
  mutate(X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2]) %>%
  mutate(label = str_c(abb, "\n",
                       total_votos_calculados,  "\n",
                       round(porcentaje_participacion, 1),
                       "%")) %>%
  mutate(label_color = ifelse(porcentaje_participacion > 25,
                yes = "white",
                no = "black")) %>%
  mutate(label_size = ifelse(total_votos_calculados < 100000,
                             yes = 1, no = 2))

estados_dorling %>%
  ggplot(aes(fill = porcentaje_participacion)) +
  geom_sf() +
  geom_text(aes(x = X, y = Y,
                label = prettyNum(label, big.mark = ",")),
            color = estados_dorling$label_color,
            size = estados_dorling$label_size,
            family = "Montserrat") +
  scale_fill_gradientn(colors = c("white", "brown"),
                       labels = scales::comma_format(suffix = "%")) +
  theme_bw() +
  labs(title = str_c("Cartograma del total de votos en la <br><b style = 'color:brown';>Revocación de Mandato</b>"),
       subtitle = str_glue("Tamaño proporcional a la cantidad de votos emitida\nDatos del cómputo de la revocación a las {hora} del 11 de abril del 2022"),
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
  guides(fill = guide_colorbar(ncol = 2,
                               title.hjust = 0.5,
                               title.position = "top",
                               barwidth = 15,
                               barheight = 0.5))

ggsave("03_RESULTADOS/graficas/estados/dorling_estados.png",
       device = "png",
       height = 16,
       width = 14,
       units = "cm")


pal_part_estatal <- colorNumeric(domain = mapa_entidad$porcentaje_participacion,
                                 palette = c("white", "brown"))

# Mapa participación por estados:
mapx_estados <- mapa_entidad %>%
  mutate(X = st_coordinates(st_centroid(.))[,1],
         Y = st_coordinates(st_centroid(.))[,2]) %>%
  mutate(label_color = ifelse(porcentaje_participacion < 19, "black", "white"))

mapx_estados %>%
  ggplot(aes(fill = porcentaje_participacion)) +
  geom_sf() +
  geom_label(aes(x = X, y = Y,
                label = str_c(abb, "\n",
                              round(porcentaje_participacion, 1), "%")),
            color = mapx_estados$label_color,
            size = 1.5) +
  scale_fill_gradientn(colors = c("white", "brown"),
                       labels = scales::comma_format(suffix = "%"),
                       limits = c(5, 40)) +
  theme_bw() +
  labs(title = str_c("<b style = 'color:brown';>Participación en la consulta</b><br>por Entidad Federativa"),
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
  guides(fill = guide_colorbar(ncol = 2,
                               title.hjust = 0.5,
                               title.position = "top",
                               barwidth = 15,
                               barheight = 0.5))

ggsave("03_RESULTADOS/graficas/estados/estados_participación.png",
       device = "png",
       height = 16,
       width = 20,
       units = "cm")

mapx_estados$ENTIDAD
mapx_estados_centro <- mapx_estados %>%
  filter(ENTIDAD %in% c("Morelos", "Ciudad de México",
                        "México", "Hidalgo",
                        "Querétaro", "Tlaxcala",
                        "Puebla"))

mapx_estados_centro %>%
  ggplot(aes(fill = porcentaje_participacion)) +
  geom_sf() +
  geom_label(aes(x = X, y = Y,
                 label = str_c(abb, "\n",
                               round(porcentaje_participacion, 1), "%")),
             color = mapx_estados_centro$label_color,
             size = 2.5) +
  scale_fill_gradientn(colors = c("white", "brown"),
                       labels = scales::comma_format(suffix = "%"),
                       limits = c(5, 40)) +
  theme_bw() +
  labs(title = str_c("<b style = 'color:brown';>Participación en la consulta</b><br>por Entidad Federativa"),
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
  guides(fill = guide_colorbar(ncol = 2,
                               title.hjust = 0.5,
                               title.position = "top",
                               barwidth = 15,
                               barheight = 0.5))


ggsave("03_RESULTADOS/graficas/estados/estados_participación_centro.png",
       device = "png",
       height = 16,
       width = 20,
       units = "cm")

# mapa_entidad %>%
#   leaflet() %>%
#   addProviderTiles("CartoDB.Positrón") %>%
#   addPolygons(fillColor = pal_part_estatal(mapa_entidad$porcentaje_participacion),
#               fillOpacity = 1,
#               color = "black",
#               opacity = 1,
#               weight = 1) %>%
#   addLegend(title = "Porcentaje de<br>participación<br>Revocación de<br>Mandato",
#             position = "bottomright",
#             labFormat = labelFormat(suffix = "%"),
#             pal = pal_part_estatal,
#             values = mapa_entidad$porcentaje_participacion)

