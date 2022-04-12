# ESTADOS ----
source("02_SCRIPTS/analisis_revocacion.R")

# ESTADOS ----

## Datos ----
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

# G1. Aportación total ----
votos_estados %>%
  ggplot(aes(x = reorder(entidad, total_votos_calculados),
             y = total_votos_calculados)) +
  geom_col(fill = "brown") +
  geom_hline(yintercept = 40,
             color = "brown",
             linetype = 2,
             size = 0.5) +
  geom_text(aes(label = str_c(prettyNum(round(total_votos_calculados,1),
                                        big.mark = ","), "")),
            hjust = -0.1,
            family = "Mulish",
            size = 3) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       fill = NULL,
       title = "Total de votos aportados a la Consulta de revocación<br>por entidad federativa",
       subtitle = str_glue("Datos del cómputo de la revocación a las {hora} del 11 de abril del 2022"),
       caption = "Fuente: Cómputos de la revocación de mandato - INE. Revocación de mandato, consultado en:
       https://computosrm2022.ine.mx/base-de-datos
       @JuvenalCamposF") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
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

ggsave("03_RESULTADOS/graficas/estados/grafica_total_votos.png",
       device = "png",
       height = 16,
       width = 14,
       units = "cm")

# G2. Porcentaje de participación ----
estados_con_mas_abstencion <- votos_estados %>%
  filter(between(porcentaje_abstencion, 0, 100)) %>%
  arrange(porcentaje_abstencion) %>%
  select(id_entidad, entidad, porcentaje_abstencion, porcentaje_participacion)

estados_con_mas_abstencion %>%
  ggplot(aes(x = reorder(entidad, porcentaje_participacion),
             y = porcentaje_participacion)) +
  geom_col(fill = "brown") +
  geom_hline(yintercept = 40,
             color = "brown",
             linetype = 2,
             size = 0.5) +
  geom_text(aes(y = 41, x = 17, label = "Marca del 40%"),
            angle = 270) +
  geom_text(aes(label = str_c(prettyNum(round(porcentaje_participacion,1),
                                        big.mark = ","), "%")),
            hjust = -0.1,
            family = "Mulish",
            size = 3) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       fill = NULL,
       title = "Porcentaje de participación en la consulta de revocación<br>por <b style = 'color:brown;'>entidad federativa</b>",
       subtitle = str_glue("Datos del cómputo de la revocación a las {hora} del 11 de abril del 2022"),
       caption = "Fuente: Cómputos de la revocación de mandato - INE. Revocación de mandato, consultado en:
       https://computosrm2022.ine.mx/base-de-datos
       @JuvenalCamposF") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
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

ggsave("03_RESULTADOS/graficas/estados/grafica_porcentaje_votos.png",
       device = "png",
       height = 16,
       width = 14,
       units = "cm")

# Estados con elecciones 2022:
estados_con_mas_abstencion$entidad %>% sort()
datos_edos_elecciones <- estados_con_mas_abstencion %>%
  mutate(edos_elecciones = ifelse(entidad %in% c("AGUASCALIENTES",
                                                 "DURANGO",
                                                 "HIDALGO",
                                                 "OAXACA",
                                                 "TAMAULIPAS",
                                                 "QUINTANA ROO"),
                                  "Elección 2022", ""),
         alpha_edos_elecciones = ifelse(entidad %in% c("AGUASCALIENTES",
                                                       "DURANGO",
                                                       "HIDALGO",
                                                       "OAXACA",
                                                       "TAMAULIPAS",
                                                       "QUINTANA ROO"),
                                        1, 0.4))

# G3. Participación elecciones 2022 ----
datos_edos_elecciones %>%
  ggplot(aes(x = reorder(entidad, porcentaje_participacion),
             y = porcentaje_participacion)) +
  geom_col(fill = "brown",
           alpha = datos_edos_elecciones$alpha_edos_elecciones) +
  geom_hline(yintercept = 40,
             color = "brown",
             linetype = 2,
             size = 0.5) +
  geom_text(aes(y = 41, x = 17, label = "Límite del 40%"),
            angle = 270) +
  geom_text(aes(label = str_c(prettyNum(round(porcentaje_participacion,1),
                                        big.mark = ","), "%")),
            hjust = -0.1,
            family = "Mulish",
            size = 3) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       fill = NULL,
       title = "Porcentaje de participación en la consulta de revocación<br>por <b style = 'color:brown;'>entidad federativa</b>",
       subtitle = str_glue("Datos de los cómputos de la revocación a las {hora} del 11 de abril del 2022\nSe destacan los estados con elecciones este año."),
       caption = "Fuente: Cómputos de la revocación de mandato - INE. Revocación de mandato, consultado en:
       https://computosrm2022.ine.mx/base-de-datos
       @JuvenalCamposF") +
  scale_y_continuous(expand = expansion(c(0, 0.2), 0)) +
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

ggsave("03_RESULTADOS/graficas/estados/grafica_porcentaje_votos_elección.png",
       device = "png",
       height = 16,
       width = 14,
       units = "cm")

# G4. Donde se votó más por la revocación:
estados_favor_revocación <- votos_estados %>%
  transmute(entidad = entidad,
            porcentaje_revocacion = 100*(revocacion/total_votos_calculados)) %>%
  arrange(-porcentaje_revocacion)

color_revocacion <- "#42006e"

estados_favor_revocación %>%
  ggplot(aes(x = reorder(entidad, porcentaje_revocacion),
             y = porcentaje_revocacion)) +
  geom_col(fill = color_revocacion) +
  geom_text(aes(label = str_c(prettyNum(round(porcentaje_revocacion,1),
                                        big.mark = ","), "%")),
            hjust = -0.1,
            family = "Mulish",
            size = 3) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       fill = NULL,
       title = str_glue("Porcentaje de votos <b style = 'color: {color_revocacion};'>a favor de la revocación</b><br>del total de votos emitidos"),
       subtitle = str_glue("Datos del cómputo de la revocación a las {hora} del 11 de abril del 2022"),
       caption = "Fuente: Cómputos de la revocación de mandato - INE. Revocación de mandato, consultado en:
       https://computosrm2022.ine.mx/base-de-datos
       @JuvenalCamposF") +
  scale_y_continuous(expand = expansion(c(0, 0.5), 0)) +
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

ggsave("03_RESULTADOS/graficas/estados/grafica_porcentaje_favor_revocacion.png",
       device = "png",
       height = 16,
       width = 14,
       units = "cm")
