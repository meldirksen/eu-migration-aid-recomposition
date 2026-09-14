# ============================================================
# 10_regional_estrecha.R   (cierra M1b: robustez del pool regional)
# Recalcula el control migratorio (A) REGIONAL bajo definición
# amplia y estrecha, con depuración del doble registro EUTF.
# ============================================================
library(arrow); library(dplyr); library(tidyr); library(stringr); library(stringi); library(readxl)

# --- Léxico y codebook (idénticos a tu 03/bloque ③) ---
codebook <- read_excel("data/codebook_codigos_v1.xlsx", sheet = "Sheet1") %>%
  transmute(purpose_code = as.numeric(purpose_code), categoria_base = categoria, ambiguo)

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

# --- Extracción regional (idéntica al bloque ③) ---
ds <- open_dataset("data/CRS.parquet")
reg <- ds %>%
  filter(donor_code == 918, category == 10, bi_multi == 1, year >= 2007, year <= 2024) %>%
  select(year, region_name, recipient_name, purpose_code,
         project_title, short_description, long_description, usd_disbursement_defl) %>%
  collect() %>%
  filter(str_detect(str_to_lower(recipient_name), "regional"),
         str_detect(str_to_lower(paste(region_name, recipient_name)), "africa|sahara|sahel")) %>%
  mutate(purpose_code = as.numeric(purpose_code)) %>%
  left_join(codebook, by = "purpose_code")

# --- Clasificación con AMBAS definiciones ---
reg <- reg %>%
  mutate(
    texto  = tolower(stri_trans_general(paste(project_title, short_description, long_description), "Latin-ASCII")),
    titulo = tolower(stri_trans_general(project_title, "Latin-ASCII")),
    hit_A_any   = str_detect(texto,  lex_A),   # amplia: en cualquier parte
    hit_A_title = str_detect(titulo, lex_A),   # estrecha: solo en el título
    cat_amplia = case_when(
      purpose_code == 15190 &  hit_A_any      ~ "A",
      purpose_code == 15190                   ~ "B",
      purpose_code %in% resolver & hit_A_any  ~ "A",
      TRUE ~ categoria_base),
    cat_estrecha = case_when(
      purpose_code == 15190 &  hit_A_title      ~ "A",
      purpose_code == 15190                     ~ "B",
      purpose_code %in% resolver & hit_A_title  ~ "A",
      TRUE ~ categoria_base))

# --- Suma de A con DEPURACIÓN del doble registro (mismo receptor+título+importe) ---
sumar_A <- function(df, catcol){
  df %>% filter(.data[[catcol]] == "A") %>%
    distinct(recipient_name, project_title, usd_disbursement_defl, .keep_all = TRUE) %>%
    summarise(usd_mm = sum(usd_disbursement_defl), n = n())
}
A_reg_amplia   <- sumar_A(reg, "cat_amplia")
A_reg_estrecha <- sumar_A(reg, "cat_estrecha")

# --- País (referencia), ambas definiciones ---
base12 <- readRDS("data/crs_clasificado_12paises_v1.rds") %>%
  filter(!is.na(usd_disbursement_defl), usd_disbursement_defl >= 0, year >= 2007, year <= 2024)
A_pais_amplia   <- base12 %>% filter(categoria_final_amplia   == "A") %>% summarise(u = sum(usd_disbursement_defl))
A_pais_estrecha <- base12 %>% filter(categoria_final_estrecha == "A") %>% summarise(u = sum(usd_disbursement_defl))

cat("=== CONTROL MIGRATORIO (A), millones USD 2024 ===\n")
cat(sprintf("REGIONAL  amplia: %6.1f  (%d act.)\n", A_reg_amplia$usd_mm,   A_reg_amplia$n))
cat(sprintf("REGIONAL estrecha: %6.1f  (%d act.)\n", A_reg_estrecha$usd_mm, A_reg_estrecha$n))
cat(sprintf("PAÍS      amplia: %6.1f\n", A_pais_amplia$u))
cat(sprintf("PAÍS     estrecha: %6.1f\n", A_pais_estrecha$u))
cat(sprintf("\n%% regional amplia:   %.1f %%\n", 100*A_reg_amplia$usd_mm  /(A_reg_amplia$usd_mm  +A_pais_amplia$u)))
cat(sprintf("%% regional estrecha: %.1f %%\n",   100*A_reg_estrecha$usd_mm/(A_reg_estrecha$usd_mm+A_pais_estrecha$u)))

# --- VALIDACIÓN: qué instrumentos pasan de A (amplia) a NO-A (estrecha) ---
cat("\n=== Proyectos que salen de A bajo la definición estrecha ===\n")
flip <- reg %>% filter(cat_amplia == "A", cat_estrecha != "A") %>%
  count(project_title, cat_estrecha, wt = usd_disbursement_defl, name = "usd_mm") %>%
  arrange(desc(usd_mm))
print(as.data.frame(flip %>% mutate(usd_mm = round(usd_mm,1))), row.names = FALSE)