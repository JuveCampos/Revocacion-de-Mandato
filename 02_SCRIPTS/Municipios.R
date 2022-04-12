# MUNICIPIOS ----
source("02_SCRIPTS/analisis_revocacion.R")

names(votos)
votos_municipios <- votos %>%
  ungroup() %>%
  group_by(nom_mun_ine, cve_mun) %>%
  summarise(entidad = first(entidad),
            revocacion = sum(que_se_le_revoque_el_mandato_por_perdida_de_la_confianza, na.rm = T),
            no_revocacion = sum(que_siga_en_la_presidencia_de_la_republica,na.rm = T),
            total_votos_calculados = sum(total_votos_calculados, na.rm = T),
            lista_nominal = sum(lista_nominal, na.rm = T),
            abstencion = lista_nominal - total_votos_calculados,
            porcentaje_abstencion = 100*(abstencion/lista_nominal),
            porcentaje_participacion = 100-porcentaje_abstencion,
            porcentaje_revocacion = 100*(revocacion/total_votos_calculados)) %>%
  ungroup()

municipios_con_mas_abstencion <- votos_municipios %>%
  filter(between(porcentaje_abstencion, 0, 100)) %>%
  arrange(porcentaje_abstencion) %>%
  select(entidad, nom_mun_ine, porcentaje_abstencion, porcentaje_participacion)

municipios_con_mas_abstencion %>%
  slice(c(1:20, (nrow(municipios_con_mas_abstencion)-19):nrow(municipios_con_mas_abstencion))) %>%
  mutate(tipo = c(rep("Mas participación",20),rep("Menos participación", 20))) %>%
  mutate(nom_mun = str_c(nom_mun_ine, ", ", entidad)) %>%
  ggplot(aes(x = reorder(nom_mun, porcentaje_participacion),
             y = porcentaje_participacion,
             fill = tipo)) +
  geom_col() +
  geom_text(aes(label = str_c(prettyNum(round(porcentaje_participacion,1),
                                        big.mark = ","), "%")),
            hjust = -0.1,
            family = "Mulish",
            size = 3) +
  geom_hline(yintercept = 40,
             color = "BLACK",
             linetype = 2,
             size = 0.5) +
  geom_text(aes(y = 41, x = 5,
                label = "Marca del 40%"),
            angle = 270) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       fill = NULL,
       title = "Porcentaje de participación en la consulta<br><b style = 'color:brown;'>10 municipios más</b> y <b style = 'color:olivedrab;'>menos participativos</b>",
       subtitle = str_glue("Datos de los Cómputos de la revocación de mandato a las {hora} del 11 de abril del 2022"),
       caption = "Fuente: Cómputos de la revocación de mandato - INE. Revocación de mandato, consultado en:
       https://computosrm2022.ine.mx/base-de-datos
       @JuvenalCamposF") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  scale_fill_manual(values = c("brown", "olivedrab")) +
  scale_color_manual(values = c("brown", "olivedrab")) +
  theme_bw() +
  theme(axis.text.y = element_text(family = "Mulish", size = 8),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(face = "bold",family = "Mulish", size = 14, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Mulish", size = 9, hjust = 0.5),
        plot.caption = element_text(size = 5)) +
  guides(fill = guide_legend(ncol = 2))

ggsave("03_RESULTADOS/graficas/municipios/top_10_bottom_10_participacion.png",
       device = "png",
       height = 18,
       width = 16,
       units = "cm")

# Gráfica de apoyo a la revocación, por municipio:
color_revocacion <- "#42006e"
votos_municipios %>%
  arrange(-porcentaje_revocacion) %>%
  head(n = 20) %>%
  mutate(nom_mun = str_c(nom_mun_ine, ", ", entidad)) %>%
  ggplot(aes(x = reorder(nom_mun, porcentaje_revocacion),
             y = porcentaje_revocacion)) +
  geom_col(fill = color_revocacion) +
  coord_flip() +
  geom_text(aes(label = str_c(prettyNum(round(porcentaje_revocacion,1),
                                        big.mark = ","), "%")),
            hjust = -0.1,
            family = "Mulish",
            size = 3) +
  labs(x = NULL, y = NULL,
       fill = NULL,
       title = str_glue("¿Qué municipios votaron en mayor proporción<br>por la <b style = color:{color_revocacion}>revocación del mandato presidencial</b>?"),
       subtitle = str_glue("Porcentaje respecto al total de votos emitidos/calculados.\nDatos de los Cómputos de la revocación de mandato a las {hora} del 11 de abril del 2022"),
       caption = "Fuente: Cómputos de la revocación de mandato - INE. Revocación de mandato, consultado en:
       https://computosrm2022.ine.mx/base-de-datos
       @JuvenalCamposF") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
  scale_fill_manual(values = c("brown", "olivedrab")) +
  scale_color_manual(values = c("brown", "olivedrab")) +
  theme_bw() +
  theme(axis.text.y = element_text(family = "Mulish", size = 8),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(face = "bold",family = "Mulish", size = 14, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Mulish", size = 9, hjust = 0.5),
        plot.caption = element_text(size = 5)) +
  guides(fill = guide_legend(ncol = 2))

ggsave("03_RESULTADOS/graficas/municipios/municipios_por_la_revocacion.png",
       device = "png",
       height = 18,
       width = 16,
       units = "cm")

# Mapa:
mapa_mpios <- cartografia_mpios %>%
  mutate(MUNICIPIO = case_when(str_length(MUNICIPIO) == 1 ~ str_c("00", MUNICIPIO),
                               str_length(MUNICIPIO) == 2 ~ str_c("0", MUNICIPIO),
                               str_length(MUNICIPIO) == 3 ~ str_c("", MUNICIPIO))) %>%
  mutate(ENTIDAD = case_when(str_length(ENTIDAD) == 1 ~ str_c("0", ENTIDAD),
                             str_length(ENTIDAD) == 2 ~ str_c("", ENTIDAD))) %>%
  mutate(cve_mun = str_c(ENTIDAD, MUNICIPIO)) %>%
  select(cve_mun) %>%
  left_join(votos_municipios) %>%
  # as_tibble() %>%
  # select(-geometry)
  st_transform(crs = 4326)

paleta_participacion <- colorNumeric(palette = c("white", "brown"),
                                     domain = c(0, 100),
                                     na.color = "#bababa")

names(mapa_mpios)


# Mapa participación entidades país:

if(FALSE){
entidad_sel <- "COAHUILA"

datos_muni <- mapa_mpios %>%
  filter(entidad == entidad_sel)

openxlsx::write.xlsx(datos_muni %>% as_tibble() %>% select(-geometry),
                     str_glue("03_RESULTADOS/municipios/datos/{entidad_sel}_muni.xlsx"))

datos_muni %>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = paleta_participacion(datos_muni$porcentaje_participacion),
              fillOpacity = 1,
              label = lapply(str_c("Municipio: ", datos_muni$nom_mun_ine, "<br>",
                                   "Porcentaje de participación: ", round(datos_muni$porcentaje_participacion, 2), "%"),
                             htmltools::HTML),
              color = "black",
              weight = 1) %>%
  addLegend(position = "bottomright",
            labFormat = labelFormat(suffix = "%"),
            title = str_c("Porcentaje de<br>participación", "<br>", entidad_sel),
            pal = paleta_participacion,
            values = datos_muni$porcentaje_participacion)


datos_muni %>%
  ggplot(aes(fill = porcentaje_participacion)) +
  geom_sf(color = "black",
          size = 0.5) +
  scale_fill_gradientn(colors = c("white", "brown"),
                       labels = scales::comma_format(suffix = "%")) +
  theme_bw() +
  labs(title = str_c("Porcentaje de participación"),
       subtitle = str_c("Entidad: ", entidad_sel),
       fill = "Porcentaje") +
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
                               barwidth = 10,
                               barheight = 0.5))
}

paleta_participacion <- colorNumeric(palette = c("white", "brown"),
                                     domain = c(0, 100),
                                     na.color = "#bababa")

# Mapa de municipios de todo el país:
mapa_mpios %>%
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = paleta_participacion(mapa_mpios$porcentaje_participacion),
              fillOpacity = 1,
              label = lapply(str_c("Municipio: ", mapa_mpios$nom_mun_ine, "<br>",
                                   "Porcentaje de participación: ", round(mapa_mpios$porcentaje_participacion, 2), "%"),
                             htmltools::HTML),
              color = "transparent",
              weight = 1) %>%
  addPolygons(data = cartografia_edos,
              fill = NA,
              color = "gray",
              weight = 2) %>%
  addLegend(position = "bottomright",
            title = "Porcentaje de participación<br><b style = 'color:brown;'>Revocación de mandato</b>",
            pal = paleta_participacion,
            labFormat = labelFormat(suffix = "%"),
            values = c(0,100))

