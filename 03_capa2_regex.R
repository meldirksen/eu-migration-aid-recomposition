#03_clasificar_v1.R  (REEMPLAZA a 03_capa2_regex.R para clasificar)
# Porta el LEXICO VALIDADO v1.0 (del proyecto de 8 paises, 01_verificar_crs.R)
# al panel de 12 paises, combinandolo con tu Capa 1 de 226 codigos.
#
# Que hace distinto al v0.1:
#   - Normaliza el texto SIN acentos (Latin-ASCII): frontiere == frontière.
#     (esta era la causa principal de que salieran menos A de las que hay)
#   - Usa el lex_A (44) y lex_B (~40) ya validados, en EN/FR/ES.
#   - Reaplica las correcciones manuales que ya validaste (falsos positivos).
#
# Insumo:  data/base_capa1_12paises.rds   (salida de 02_capa1_lookup.R)
# Salida:  data/crs_clasificado_12paises_v1.rds
#          + output/revisar_A_12paises.csv  (TODO lo que cayo en A, para leer)
#
# COMO USARLO: copia a scripts/, Restart R, corre 00_setup.R y luego este.
# Luego en 04_resultados_descriptivos.R cambia la ruta a: crs_clasificado_12paises_v1.rds
# ============================================================
library(dplyr)
library(stringr)
library(stringi)   # si falla: install.packages("stringi")

ruta_base   <- "data/base_capa1_12paises.rds"
ruta_salida <- "data/"
if (!dir.exists("output")) dir.create("output")

base <- readRDS(ruta_base)   # debe traer: purpose_code, categoria_base, ambiguo,
# project_title, short_description, long_description

# ------------------------------------------------------------
# 1. Texto normalizado (minusculas y SIN acentos)
#    OJO: el panel de 12 paises no extrajo 'keywords'. Si quieres maxima
#    cobertura, añade keywords en 01_extraccion.R y vuelve a correr 02.
# ------------------------------------------------------------
base <- base %>%
  mutate(
    texto  = tolower(stri_trans_general(
      paste(project_title, short_description, long_description), "Latin-ASCII")),
    titulo = tolower(stri_trans_general(project_title, "Latin-ASCII"))
  )

# ------------------------------------------------------------
# 2. LEXICOS VALIDADOS v1.0 (copiados de 01_verificar_crs.R, sin acentos)
# ------------------------------------------------------------
lex_A <- paste(sep = "|",
               "gestion des frontier","gestion de la frontier","gestion integree des frontier",
               "controle des frontier","surveillance des frontier","surveillance maritime",
               "border management","border control","border surveillance","integrated border",
               "garde-frontier","garde-cote","coast guard",
               "gestion des migration","gestion de la migration","migration management",
               "flux migratoire","migratory flow","migration irreguliere","irregular migration",
               "readmiss","retour des migrant","retour volontaire","retour force",
               "voluntary return","forced return","return of migrant","expulsion","deportation",
               "trafic de migrant","trafic d'etre","traite des personne","traite des etre",
               "smuggling of migrant","migrant smuggling","human trafficking","trafficking in person",
               "trafficking in human","gar-si","interception","biometri",
               "search and rescue","recherche et sauvetage")

lex_B <- paste(sep = "|",
               "migration et developpement","migration and development","migracion y desarrollo",
               "causes profondes","root causes","causas raiz","causas profundas",
               "communautes hote","communautes d'accueil","host communit","comunidades de acogida",
               "refugie","refugee","deplace","displaced","idp",
               "reintegration","reinsertion","resilience","cohesion sociale","social cohesion",
               "remise de fonds","transfert de fonds","remittance","remesas","diaspora",
               "mobilite de la main","labour mobility","labor mobility",
               "protection des migrant","protection of migrant","droit d'asile","asylum","asile",
               "opportunites economiques","economic opportunit","emploi des jeunes","youth employment")

# ------------------------------------------------------------
# 3. Señales de texto
# ------------------------------------------------------------
base <- base %>%
  mutate(
    hit_A_any   = str_detect(texto,  lex_A),
    hit_A_title = str_detect(titulo, lex_A),   # proxy de "objetivo PRINCIPAL"
    hit_B_any   = str_detect(texto,  lex_B)
  )

# Codigos ambiguos donde el control puede esconderse (resolver por texto).
# 15190 se trata aparte (por defecto B; A solo si hay señal de control).
resolver <- c(15130, 15210, 15220, 15230, 15240, 15250, 15261)

# ------------------------------------------------------------
# 4. Clasificacion amplia / estrecha
#    Base = categoria_base de tu codebook de 226 codigos.
#    Solo se sobrescriben los codigos ambiguos via lexico.
# ------------------------------------------------------------
base <- base %>%
  mutate(
    categoria_final_amplia = case_when(
      purpose_code == 15190 &  hit_A_any            ~ "A",
      purpose_code == 15190                         ~ "B",
      purpose_code %in% resolver & hit_A_any        ~ "A",
      TRUE ~ categoria_base
    ),
    categoria_final_estrecha = case_when(
      purpose_code == 15190 &  hit_A_title          ~ "A",
      purpose_code == 15190                         ~ "B",
      purpose_code %in% resolver & hit_A_title      ~ "A",
      TRUE ~ categoria_base
    ),
    # marcas de revision (no reclasifican solas)
    flag_revisar_A = categoria_base == "C" & !(purpose_code %in% resolver) & hit_A_any,
    flag_revisar_B = categoria_base == "C" & hit_B_any & !hit_A_any
  )

# ------------------------------------------------------------
# 5. Correcciones manuales YA VALIDADAS (8 paises).
#    OJO: validadas sobre 8 paises. Los 4 nuevos (Ghana, Guinea, Nigeria,
#    Camerun, C. Ivoire...) pueden traer falsos positivos NUEVOS: hay que
#    leer output/revisar_A_12paises.csv y añadir correcciones si procede.
# ------------------------------------------------------------
base <- base %>%
  mutate(
    correccion = case_when(
      grepl("Expenditure verification.*Anti-Corruption", project_title)        ~ "C",
      grepl("Retour de l", project_title)                                       ~ "S",
      grepl("stabilisation de la région du Liptako", project_title)             ~ "S",
      grepl("stabilité et la sécurité intérieure au Sénégal", project_title)    ~ "S",
      grepl("Ghawdat", project_title)                                           ~ "S",
      grepl("Processus de Paix en Casamance|Déminer pour une paix", project_title) ~ "S",
      grepl("développement de l'éducation au Niger", project_title)             ~ "C",
      grepl("connaissances sur la population burkinab", project_title)          ~ "C",
      # --- Validacion 12 paises (revisar_A_12paises.csv) ---
      grepl("EU for a Secure Ghana", project_title)                             ~ "S",  # seguridad pura
      grepl("SECSEN", project_title)                                            ~ "S",  # seguridad interior
      grepl("Migration Governance|Gouvernance des migrations", project_title)   ~ "B",  # gobernanza -> B (decision metodologica)
      TRUE ~ NA_character_
    ),
    categoria_final_amplia   = ifelse(is.na(correccion), categoria_final_amplia,   correccion),
    categoria_final_estrecha = ifelse(is.na(correccion), categoria_final_estrecha, correccion)
  ) %>%
  select(-correccion)

# ------------------------------------------------------------
# 6. Diagnostico: comparar con el 8 paises (A=51 amplia validado)
# ------------------------------------------------------------
cat("=== Reparto FINAL amplia (12 paises) ===\n");   print(table(base$categoria_final_amplia))
cat("\n=== Reparto FINAL estrecha (12 paises) ===\n"); print(table(base$categoria_final_estrecha))
cat("\nHibridas (amplia != estrecha):",
    sum(base$categoria_final_amplia != base$categoria_final_estrecha, na.rm = TRUE), "\n")

# ------------------------------------------------------------
# 7. Export para VALIDACION MANUAL: todo lo que cayo en A (léelo entero)
# ------------------------------------------------------------
revisar_A <- base %>%
  filter(categoria_final_amplia == "A") %>%
  select(year, recipient_name, purpose_code, categoria_base,
         categoria_final_estrecha, project_title) %>%
  arrange(purpose_code, recipient_name, year)
write.csv(revisar_A, "output/revisar_A_12paises.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("\nExportado: output/revisar_A_12paises.csv  (", nrow(revisar_A), "filas). LEELO entero.\n")

# ------------------------------------------------------------
# 8. Guardar clasificacion v1 (12 paises)
# ------------------------------------------------------------
saveRDS(base, paste0(ruta_salida, "crs_clasificado_12paises_v1.rds"))
cat("\nGuardado: data/crs_clasificado_12paises_v1.rds\n")
cat(">>> Ahora en 04, cambia ruta_rds a 'data/crs_clasificado_12paises_v1.rds' y vuelve a correrlo.\n")