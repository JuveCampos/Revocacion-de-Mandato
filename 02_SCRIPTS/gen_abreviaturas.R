
# Base Abreviaturas Estados:
em <- tibble::tribble(
  ~cve_ent,              ~entidad,       ~ent, ~year,  ~muertes_en_exceso,
  "01",      "Aguascalientes",      "AGS", 2021L,   4755L,
  "02",     "Baja California",       "BC", 2021L,  20729L,
  "03", "Baja California Sur",      "BCS", 2021L,   2861L,
  "04",            "Campeche",      "CAM", 2021L,   3775L,
  "05",            "Coahuila",     "COAH", 2021L,  13802L,
  "06",              "Colima",      "COL", 2021L,   3212L,
  "07",             "Chiapas",     "CHIS", 2021L,  14654L,
  "08",           "Chihuahua",     "CHIH", 2021L,  16564L,
  "09",    "Ciudad de México",     "CDMX", 2021L, 105114L,
  "10",             "Durango",      "DGO", 2021L,   5506L,
  "11",          "Guanajuato",      "GTO", 2021L,  33277L,
  "12",            "Guerrero",      "GRO", 2021L,   7547L,
  "13",             "Hidalgo",      "HGO", 2021L,  13781L,
  "14",             "Jalisco",      "JAL", 2021L,  36256L,
  "15",              "México",      "MEX", 2021L,  99921L,
  "16",           "Michoacán",     "MICH", 2021L,  22085L,
  "17",             "Morelos",      "MOR", 2021L,  13795L,
  "18",             "Nayarit",      "NAY", 2021L,   2942L,
  "19",          "Nuevo León",       "NL", 2021L,  27689L,
  "20",              "Oaxaca",      "OAX", 2021L,  14097L,
  "21",              "Puebla",      "PUE", 2021L,  42985L,
  "22",           "Querétaro",      "QRO", 2021L,  11416L,
  "23",        "Quintana Roo",     "QROO", 2021L,   6762L,
  "24",     "San Luis Potosí",      "SLP", 2021L,   8973L,
  "25",             "Sinaloa",      "SIN", 2021L,  11460L,
  "26",              "Sonora",      "SON", 2021L,  15975L,
  "27",             "Tabasco",      "TAB", 2021L,  10307L,
  "28",          "Tamaulipas",    "TAMPS", 2021L,  10548L,
  "29",            "Tlaxcala",     "TLAX", 2021L,   8006L,
  "30",            "Veracruz",      "VER", 2021L,  36299L,
  "31",             "Yucatán",      "YUC", 2021L,   7084L,
  "32",           "Zacatecas",      "ZAC", 2021L,   9178L,
  "00",            "Nacional", "Nacional", 2021L, 667240L
)

em %>%
  select(-year, -muertes_en_exceso) %>%
  mutate(cve_ent = as.numeric(cve_ent)) %>%
  saveRDS("01_DATOS/00_01_Catalogos/abreviaturas_entidad")
