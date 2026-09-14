# ============================================================
# 09_pendientes_resultados.R   (corre tras 00_setup.R)
# Cierra los 4 [pendiente R] de resultados_v6.docx
# OJO CLAVE: usd_disbursement_defl YA está en millones (como en tu 04/08).
#            NO se divide entre 1e6 en ningún sitio.
# ============================================================
library(arrow); library(dplyr); library(tidyr)
library(stringr); library(stringi); library(readxl)

base12 <- readRDS("data/crs_clasificado_12paises_v1.rds") %>%
  filter(!is.na(usd_disbursement_defl), usd_disbursement_defl >= 0,
         year >= 2007, year <= 2024) %>%
  mutate(periodo = if_else(year <= 2015, "pre", "post"),
         era = factor(case_when(year <= 2015 ~ "pre-EUTF",
                                year <= 2021 ~ "EUTF", TRUE ~ "NDICI"),
                      levels = c("pre-EUTF","EUTF","NDICI")))

cat("Filas:", nrow(base12), "| Países:", n_distinct(base12$recipient_name),
    "| Años:", min(base12$year), "-", max(base12$year), "\n")
table(base12$categoria_final_amplia, useNA = "ifany")

vol <- base12 %>% filter(categoria_final_amplia != "R") %>%
  group_by(era, cat = categoria_final_amplia) %>%
  summarise(usd_mm = sum(usd_disbursement_defl), .groups = "drop")

tot <- vol %>% group_by(era) %>% summarise(total_mm = sum(usd_mm), .groups = "drop")
n_anios <- base12 %>% group_by(era) %>% summarise(k = n_distinct(year), .groups = "drop")

C_era <- vol %>% filter(cat == "C") %>%
  left_join(tot, by = "era") %>% left_join(n_anios, by = "era") %>%
  mutate(share = usd_mm/total_mm, C_por_anio = usd_mm/k, total_por_anio = total_mm/k)

cat("=== C absoluto y total por era (millones USD 2024) ===\n")
print(as.data.frame(C_era %>% mutate(across(where(is.numeric), ~round(.x,1)))), row.names = FALSE)


part <- function(col_cat) {
  d <- base12[!is.na(base12[[col_cat]]) & base12[[col_cat]] != "R", ]
  d$categoria <- d[[col_cat]]
  d %>% group_by(era, categoria) %>%
    summarise(usd = sum(usd_disbursement_defl), .groups = "drop") %>%
    group_by(era) %>% mutate(share = usd/sum(usd)) %>% ungroup() %>%
    select(categoria, era, share) %>%
    pivot_wider(names_from = era, values_from = share, values_fill = 0)
}
t_amplia   <- part("categoria_final_amplia")
t_estrecha <- part("categoria_final_estrecha")
cat("\n=== Participaciones por era: AMPLIA vs ESTRECHA ===\n")
print(as.data.frame(t_amplia   %>% mutate(across(where(is.numeric), ~round(.x,4)))), row.names=FALSE)
print(as.data.frame(t_estrecha %>% mutate(across(where(is.numeric), ~round(.x,4)))), row.names=FALSE)

# --- Clasificador regional (idéntico a tu clasificar() del 05) ---
codebook <- read_excel("data/codebook_codigos_v1.xlsx", sheet = "Sheet1") %>%
  transmute(purpose_code = as.numeric(purpose_code),
            categoria_base = categoria, ambiguo)

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

# --- Extraer del parquet: donante 918, ODA, bilateral, agregados REGIONALES ---
ds <- open_dataset("data/CRS.parquet")
reg <- ds %>%
  filter(donor_code == 918, category == 10, bi_multi == 1,
         year >= 2007, year <= 2024) %>%
  select(year, region_name, recipient_name, purpose_code,
         project_title, short_description, long_description,
         usd_disbursement_defl) %>%
  collect() %>%
  filter(str_detect(str_to_lower(recipient_name), "regional"),   # agregados
         str_detect(str_to_lower(paste(region_name, recipient_name)),
                    "africa|sahara|sahel"))                       # solo África

reg <- reg %>%
  mutate(purpose_code = as.numeric(purpose_code)) %>%
  left_join(codebook, by = "purpose_code") %>%
  mutate(texto = tolower(stri_trans_general(
    paste(project_title, short_description, long_description), "Latin-ASCII")),
    hit_A_any = str_detect(texto, lex_A),
    cat = case_when(purpose_code == 15190 &  hit_A_any     ~ "A",
                    purpose_code == 15190                  ~ "B",
                    purpose_code %in% resolver & hit_A_any ~ "A",
                    TRUE ~ categoria_base))

A_regional <- reg %>% filter(cat=="A") %>%
  summarise(usd_mm = sum(usd_disbursement_defl), n = n())
A_pais <- base12 %>% filter(categoria_final_amplia=="A") %>%
  summarise(usd_mm = sum(usd_disbursement_defl), n = n())

cat("\n=== A regional vs A país (millones USD 2024) ===\n")
cat("Regional:", round(A_regional$usd_mm,1), "M (", A_regional$n, "act.)\n")
cat("País    :", round(A_pais$usd_mm,1),     "M (", A_pais$n,     "act.)\n")
cat("% regional sobre A total:",
    round(100*A_regional$usd_mm/(A_regional$usd_mm+A_pais$usd_mm),1), "%\n")

# --- VALIDACIÓN OBLIGATORIA: lee esto como leíste revisar_A_12paises.csv ---
reg %>% filter(cat=="A") %>%
  count(region_name, recipient_name, purpose_code, project_title,
        wt = usd_disbursement_defl, name = "usd_mm", sort = TRUE) %>%
  write.csv("output/revisar_A_regional.csv", row.names = FALSE, fileEncoding="UTF-8")
cat("Exportado output/revisar_A_regional.csv — LÉELO y descarta falsos positivos.\n")


grupo_sector <- function(pc){ g <- pc %/% 100
case_when(g %in% 111:116~"Educacion", g %in% 121:123~"Salud",
          g==130~"Poblacion/salud reprod.", g==140~"Agua y saneamiento",
          g==160~"Otros sociales", g %in% 210:250~"Infraestr. economica",
          g %in% 311:313~"Agric/pesca/silvic.", g %in% 321:323~"Industria/mineria",
          g %in% 331:332~"Comercio y turismo", g==410~"Medio ambiente",
          g %in% 430:432~"Multisectorial", TRUE~"Otros desarrollo") }

tot_per <- base12 %>% filter(categoria_final_amplia!="R") %>%
  group_by(periodo) %>% summarise(usd_tot=sum(usd_disbursement_defl), .groups="drop")

otros <- base12 %>% filter(categoria_final_amplia=="C") %>%
  mutate(subsector = grupo_sector(purpose_code)) %>%
  filter(subsector == "Otros desarrollo") %>%
  mutate(g3 = purpose_code %/% 100) %>%
  group_by(periodo, g3) %>% summarise(usd=sum(usd_disbursement_defl), .groups="drop") %>%
  left_join(tot_per, by="periodo") %>% mutate(share=usd/usd_tot) %>%
  select(g3, periodo, share) %>%
  pivot_wider(names_from=periodo, values_from=share, values_fill=0) %>%
  mutate(cambio_pp=(post-pre)*100) %>% arrange(cambio_pp)

cat("\n=== Interior de 'Otros desarrollo' por grupo de 3 dígitos (pp) ===\n")
print(as.data.frame(otros %>% mutate(across(where(is.numeric), ~round(.x,3)))), row.names=FALSE)
cat("Suma cambio_pp (debe ≈ -12,2):", round(sum(otros$cambio_pp),1), "\n")