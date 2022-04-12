# Catalogo Secciones: ----
library(tidyverse)

cat_secciones <- read_csv("01_DATOS/00_01_Catalogos/bd_secciones_20220406.csv") %>%
  mutate(cve_mun = ifelse(str_length(cve_edo_mpo_ine) == 4, str_c("0", cve_edo_mpo_ine),
                          str_c(cve_edo_mpo_ine))) %>%
  select(cve_edo, nom_ent, cve_seccion, nom_mun_ine, cve_mun)


saveRDS(cat_secciones, "01_DATOS/00_01_Catalogos/catalogo_secciones.rds")
