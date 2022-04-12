options(scipen = 999)
source("02_SCRIPTS/analisis_revocacion.R")

# GENERAL: ----
# Total de casillas con datos:
str_c(round(100*(casillas_con_datos_registrados/nrow(prep)), 2), "%")
participacion_con_respecto_al_total <- 100*(sum(as.numeric(prep$total_votos_calculados), na.rm = T)/93699497)
participacion_con_datos <- 100*(sum(as.numeric(prep$total_votos_calculados), na.rm = T)/sum(as.numeric(prep$lista_nominal), na.rm = T))

# a_favor_revocacion <- 100*(sum(as.numeric(prep$que_se_le_revoque_el_mandato_por_perdida_de_la_confianza), na.rm = T)/sum(as.numeric(prep$lista_nominal), na.rm = T))
# apoyo_presidente <- 100*(sum(as.numeric(prep$que_siga_en_la_presidencia_de_la_republica), na.rm = T)/sum(as.numeric(prep$lista_nominal), na.rm = T))
# nulos <- 100*(sum(as.numeric(prep$nulos), na.rm = T)/sum(as.numeric(prep$lista_nominal), na.rm = T))

a_favor_revocacion <- 100*(1063209/93699497)
apoyo_presidente <- 100*(15159323/93699497)
nulos <- 100*(280104/93699497)
porcentaje_participacion <- a_favor_revocacion+apoyo_presidente+nulos
no_participaron <- 100-porcentaje_participacion

datos_tree <- tibble(variable = c("No participaron",
                                  "Voto a favor de la revocación",
                                  "Votos de apoyo al presidente",
                                  "Votos Nulos"),
                     numeros = c(no_participaron,
                                 a_favor_revocacion,
                                 apoyo_presidente,
                                 nulos)) %>%
  mutate(variable = factor(variable, c("No participaron",
                                       "Voto a favor de la revocación",
                                       "Votos de apoyo al presidente",
                                       "Votos Nulos"))) %>%
  mutate(numeros_2 = round(numeros*10, 0))

datos_tree

colores <- c("gray60","#42006e", "brown","purple")
scales::show_col(colores)

datos_tree %>%
  ggplot(aes(label = variable,
             values = numeros_2,
             color = variable,
             fill = variable)) +
  # geom_waffle(n_rows = 20)
  geom_pictogram(n_rows = 20,
                 make_proportional = F,
                 size = 4,
                 family = "Font Awesome 5 Free Solid") +
  scale_label_pictogram(
    name = NULL,
    values = c("user"),
    labels = c("No participaron",
               "Voto a favor de la revocación",
               "Votos de apoyo al presidente",
               "Votos Nulos"))  +
  geom_vline(xintercept = 29.5,
             linetype = 2,
             color = "brown",
             size = 1) +
  labs(title = "Si la revocación se hubiera llevado a cabo\ncon 1,000 personas...",
       subtitle = "<b style = 'color:gray60;'>824</b> no habrían participado, <b style = 'color:#42006e;'>11</b> hubieran votado a favor de la revocación,<br><b style = 'color:brown;'>162</b> hubieran apoyado al presidente y <b style = 'color:purple;'>3</b> hubieran anulado su voto<br>Y hubieran hecho falta <b  style = 'color:red;'>224</b> para llegar al 40%") +
  scale_color_manual(values = colores) +
  scale_y_continuous(expand = expansion(c(0.01,0.01), 0)) +
  scale_x_continuous(expand = expansion(c(0.01,0.01), 0)) +
  theme_bw() +
  theme_enhance_waffle() +
  theme(panel.border = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5,
                                  family = "Montserrat",
                                  size = 15,
                                  face = "bold",
                                  color = colores),
        plot.subtitle = ggtext::element_markdown(hjust= 0.5,
                                     family = "Montserrat",
                                     size = 10),
        legend.position = "none")

ggsave("03_RESULTADOS/graficas/general/treeemap.png",
       device = "png",
       height = 16,
       width = 20,
       units = "cm")


# TOTAL DE VOTOS POR UNO U OTRO RESULTADO:

# Porcentaje de participación:
votos_general <- votos %>%
  summarise(revocacion = sum(que_se_le_revoque_el_mandato_por_perdida_de_la_confianza, na.rm = T),
            no_revocacion = sum(que_siga_en_la_presidencia_de_la_republica,na.rm = T),
            total_votos_calculados = sum(total_votos_calculados, na.rm = T),
            lista_nominal = sum(lista_nominal, na.rm = T),
            abstencion = lista_nominal - total_votos_calculados,
            porcentaje_abstencion = 100*(abstencion/lista_nominal),
            porcentaje_participacion = 100-porcentaje_abstencion,
            porcentaje_revocacion = 100*(revocacion/total_votos_calculados),
            porcentaje_continuación = 100*(no_revocacion/total_votos_calculados)
  ) %>%
  ungroup() %>%
  pivot_longer(1:ncol(.))

# Graficas extra:
grafica_casillas <- tibble(evento = c("Elecciones 2021", "Revocación mandato"),
       casillas_instaladas = c(163000, 57436))

grafica_casillas %>%
  ggplot(aes(x = evento, y = casillas_instaladas)) +
  geom_col(width = 0.8,
           color = c("#42006e","brown"),
           fill = c("#42006e","brown"),
           alpha = 0.7) +
  geom_text(label = c("163,000 casillas\n(aprox)", "57,436 casillas"),
            family = "Montserrat",
            size = 4,
            vjust = -0.5,
            color = c("#42006e","brown")) +
  theme_bw() +
  labs(title = str_c("Total de Casillas Instaladas"),
       subtitle = str_c("Casillas reportadas como instaladas por reportes\nde prensa para los dos eventos electorales más recientes"),
       fill = "Porcentaje",
       x = NULL, y= NULL) +
  scale_y_continuous(expand = expansion(c(0, 0.5), 0)) +
  theme(axis.text.x = element_text(family = "Montserrat", size = 15),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(face = "bold",family = "Mulish", size = 14, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Mulish", size = 9, hjust = 0.5),
        plot.caption = element_text(size = 5))

ggsave("03_RESULTADOS/graficas/general/datos_casillas.png",
       device = "png",
       height = 10,
       width = 14,
       units = "cm")

# Porcentajes de participación:
p_participacion <- tibble(evento = c("Revocación de\nmandato",
                                     "Juicio a\nexpresidentes",
                                    "Elecciones 2021",
                                    "Elecciones 2018"),
       porcentaje_participacion = c(17.77,
                                    7.11,
                                    52.66,
                                    63.42)) %>%
  mutate(is.revocacion = ifelse(evento == "Revocación de\nmandato",
                                1, 0))

p_participacion %>%
  ggplot(aes(x = reorder(evento,porcentaje_participacion),
             y = porcentaje_participacion,
             alpha = factor(is.revocacion),
             fill = factor(is.revocacion))) +
  geom_col() +
  geom_text(aes(label = str_c(round(porcentaje_participacion, 1), "%")),
            show.legend = F,
            family = "Montserrat",
            fontface = "bold",
            size = 4,
            vjust = -0.9) +
  labs(title = "Comparación de los porcentajes de participación obtenidos<br>en la <b style = 'color:brown;'>revocación de mandato</b><br>con la participación en otras jornadas electorales",
       y = NULL, x = NULL,
       caption = "@JuvenalCamposF - Fuente de los datos: INE y reportes de prensa.") +
  scale_fill_manual(values = c("gray50", "brown")) +
  scale_alpha_manual(values = c(0.7, 1)) +
  scale_y_continuous(expand = expansion(c(0, 0.5), 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Montserrat",
                                   size = 10),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(face = "bold",family = "Mulish", size = 12, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Mulish", size = 9, hjust = 0.5),
        plot.caption = element_text(size = 5))

ggsave("03_RESULTADOS/graficas/general/comparativo_participacion_eventos.png",
       device = "png",
       height = 10,
       width = 15,
       units = "cm")

n_votos <- tibble(evento = c("Revocación de\nmandato",
                             "Juicio a\nexpresidentes",
                             "Firmas\nrevocación",
                             "Votos AMLO\n2018"),
                  votos = c(16502636,
                                               6663208,
                                               11.1e6,
                                               30e6)) %>%
  mutate(is.revocacion = ifelse(evento == "Revocación de\nmandato",
                                1, 0))

n_votos %>%
  ggplot(aes(x = reorder(evento,votos),
             y = votos,
             alpha = factor(is.revocacion),
             fill = factor(is.revocacion))) +
  geom_col() +
  geom_text(aes(label = str_c(round(votos/1e6, 1), "M")),
            show.legend = F,
            family = "Montserrat",
            fontface = "bold",
            size = 4,
            vjust = -0.9) +
  labs(title = "Comparación de los votos obtenidos en la<br><b style = 'color:brown;'>revocación de mandato</b> con otras cifras clave",
       y = NULL, x = NULL,
       caption = "@JuvenalCamposF - Fuente de los datos: INE y reportes de prensa.") +
   scale_fill_manual(values = c("gray50", "brown")) +
  scale_alpha_manual(values = c(0.7, 1)) +
  scale_y_continuous(expand = expansion(c(0, 0.5), 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(family = "Montserrat",
                                   size = 10),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.title = ggtext::element_markdown(face = "bold",family = "Mulish", size = 14, hjust = 0.5),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = "Mulish", size = 9, hjust = 0.5),
        plot.caption = element_text(size = 5))

ggsave("03_RESULTADOS/graficas/general/comparativo_votos_eventos.png",
       device = "png",
       height = 10,
       width = 15,
       units = "cm")

