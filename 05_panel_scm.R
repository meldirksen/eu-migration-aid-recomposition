# ============================================================
# 05_panel_scm.R
# Construye el panel pais x anio de sh_C (participacion del desarrollo)
# para el control sintetico de Niger, en DOS configuraciones de donantes:
#   (1) "sahel"  : los otros 11 paises EUTF Sahel/Lago Chad (contaminado)
#   (2) "limpio" : receptores de Africa subsahariana NO tratados por el EUTF
#                  (se excluyen las ventanas Sahel Y Cuerno de Africa)
#
# Salida: data/panel_scm.rds  (long: recipient_name, year, sh_C, grupo)
#         + diagnostico de cobertura temporal (clave para elegir donantes)
#
# COMO USARLO: copia a scripts/, Restart R, corre 00_setup.R y luego este.
# Pega el diagnostico final en el chat ANTES de correr el control sintetico.
# ============================================================
library(arrow)
library(dplyr)
library(stringr)
library(stringi)
library(readxl)
library(tidyr)

ruta_parquet  <- "data/CRS.parquet"
ruta_codebook <- "data/codebook_codigos_v1.xlsx"
ruta_rds12    <- "data/crs_clasificado_12paises_v1.rds"
anio_min <- 2007; anio_max <- 2024

# ------------------------------------------------------------
# 0. Listas de paises
# ------------------------------------------------------------
sahel12 <- c("Mauritania","Niger","Nigeria","Senegal","Burkina Faso","Mali",
             "Côte d'Ivoire","Guinea","Ghana","Gambia","Cameroon","Chad")

# Ventana Cuerno de Africa del EUTF (TAMBIEN tratados -> excluir del control limpio)
cuerno_eutf <- c("Ethiopia","Kenya","Somalia","Sudan","South Sudan","Uganda",
                 "Djibouti","Eritrea","Tanzania")

# ------------------------------------------------------------
# 1. Funciones reutilizables: clasificacion (codebook + lexico v1.0)
# ------------------------------------------------------------
codebook <- read_excel(ruta_codebook, sheet = "Sheet1") %>%
  select(purpose_code, categoria_base = categoria, ambiguo) %>%
  mutate(purpose_code = as.numeric(purpose_code))

lex_A <- paste(sep="|",
               "gestion des frontier","gestion de la frontier","gestion integree des frontier",
               "controle des frontier","surveillance des frontier","surveillance maritime",
               "border management","border control","border surveillance","integrated border",
               "garde-frontier","garde-cote","coast guard","gestion des migration",
               "gestion de la migration","migration management","flux migratoire","migratory flow",
               "migration irreguliere","irregular migration","readmiss","retour des migrant",
               "retour volontaire","retour force","voluntary return","forced return",
               "return of migrant","expulsion","deportation","trafic de migrant","trafic d'etre",
               "traite des personne","traite des etre","smuggling of migrant","migrant smuggling",
               "human trafficking","trafficking in person","trafficking in human","gar-si",
               "interception","biometri","search and rescue","recherche et sauvetage")

resolver <- c(15130,15210,15220,15230,15240,15250,15261)

clasificar <- function(base) {
  base %>%
    mutate(
      texto = tolower(stri_trans_general(
        paste(project_title, short_description, long_description), "Latin-ASCII")),
      hit_A_any = str_detect(texto, lex_A),
      cat = case_when(
        purpose_code == 15190 &  hit_A_any     ~ "A",
        purpose_code == 15190                  ~ "B",
        purpose_code %in% resolver & hit_A_any ~ "A",
        TRUE ~ categoria_base
      )
    )
}

# sh_C por pais-anio (excluye R del denominador, como en resultados)
sh_C_panel <- function(df, col_cat) {
  df %>%
    filter(!is.na(usd_disbursement_defl), usd_disbursement_defl >= 0,
           year >= anio_min, year <= anio_max,
           !is.na(.data[[col_cat]]), .data[[col_cat]] != "R") %>%
    group_by(recipient_name, year) %>%
    summarise(
      usd_total = sum(usd_disbursement_defl),
      usd_C     = sum(usd_disbursement_defl[.data[[col_cat]] == "C"]),
      .groups = "drop") %>%
    mutate(sh_C = usd_C / usd_total)
}

# ------------------------------------------------------------
# 2. Panel de los 12 Sahel (ya clasificados)
# ------------------------------------------------------------
base12 <- readRDS(ruta_rds12)
panel_sahel <- sh_C_panel(base12, "categoria_final_amplia") %>%
  mutate(grupo = if_else(recipient_name == "Niger", "tratado", "sahel"))

# ------------------------------------------------------------
# 3. Extraer + clasificar donantes LIMPIOS (SSA no-EUTF)
# ------------------------------------------------------------
ds <- open_dataset(ruta_parquet)

donantes_raw <- ds %>%
  filter(donor_code == 918, category == 10, bi_multi == 1,
         year >= anio_min, year <= anio_max,
         region_name == "South of Sahara") %>%
  select(year, recipient_name, purpose_code, project_title,
         short_description, long_description, usd_disbursement_defl) %>%
  collect()

# excluir tratados (Sahel + Cuerno) y agregados regionales
donantes_raw <- donantes_raw %>%
  filter(!recipient_name %in% c(sahel12, cuerno_eutf),
         !str_detect(str_to_lower(recipient_name), "regional|region"))

donantes_clas <- donantes_raw %>%
  mutate(purpose_code = as.numeric(purpose_code)) %>%
  left_join(codebook, by = "purpose_code") %>%
  clasificar()

panel_limpio <- sh_C_panel(donantes_clas, "cat") %>%
  mutate(grupo = "limpio")

# ------------------------------------------------------------
# 4. Combinar y guardar
# ------------------------------------------------------------
panel <- bind_rows(
  panel_sahel %>% select(recipient_name, year, sh_C, grupo),
  panel_limpio %>% select(recipient_name, year, sh_C, grupo)
)
saveRDS(panel, "data/panel_scm.rds")

# ------------------------------------------------------------
# 5. DIAGNOSTICO (pega esto en el chat)
# ------------------------------------------------------------
cat("=== COBERTURA: años disponibles por país (donantes limpios) ===\n")
cob <- panel %>% filter(grupo == "limpio") %>%
  group_by(recipient_name) %>%
  summarise(n_anios = n_distinct(year),
            primer = min(year), ultimo = max(year), .groups = "drop") %>%
  arrange(desc(n_anios))
print(as.data.frame(cob), row.names = FALSE)

cat("\nDonantes limpios con panel COMPLETO (", anio_max-anio_min+1, "años):\n")
completos <- cob %>% filter(n_anios == (anio_max - anio_min + 1))
print(completos$recipient_name)

cat("\n=== Serie sh_C de NIGER (unidad tratada) ===\n")
print(panel %>% filter(recipient_name == "Niger") %>% arrange(year) %>%
        mutate(sh_C = round(sh_C, 3)) %>% select(year, sh_C), n = 30)

cat("\n=== Nº de países por grupo ===\n")
print(panel %>% distinct(recipient_name, grupo) %>% count(grupo))

cat("\nGuardado: data/panel_scm.rds\n")
cat(">>> Pega el diagnostico. Con la cobertura decidimos el pool final y montamos 07_synth.\n")