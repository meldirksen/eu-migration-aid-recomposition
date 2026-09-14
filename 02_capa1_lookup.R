
# ============================================================
# 02_capa1_lookup.R
# Aplica el codebook v1.0 (Capa 1: codigo -> categoria base)
# Requiere haber corrido 01_extraccion.R antes
# ============================================================

base <- readRDS(paste0(ruta_salida, "base_extraida_12paises.rds"))

codebook <- read_excel(ruta_codebook, sheet = "Sheet1") %>%
  select(purpose_code, categoria_base = categoria, ambiguo, notas) %>%
  mutate(purpose_code = as.numeric(purpose_code))   # asegura tipo consistente para el join

base_capa1 <- base %>%
  mutate(purpose_code = as.numeric(purpose_code)) %>%
  left_join(codebook, by = "purpose_code")

# --- Verificacion critica: NINGUN purpose_code del panel debe quedar sin categoria ---
sin_match <- base_capa1 %>% filter(is.na(categoria_base))

if (nrow(sin_match) > 0) {
  cat("\n*** ATENCION:", nrow(sin_match), "actividades con purpose_code SIN categoria. ***\n")
  cat("Codigos afectados:\n")
  print(sort(unique(sin_match$purpose_code)))
  cat("Esto significa que hay codigos en tus 12 paises que no estan en el codebook de 226.\n")
  cat("Antes de seguir, hay que anadirlos al codebook.\n")
} else {
  cat("\nOK: todos los purpose_code tienen categoria asignada.\n")
}

cat("\n=== Distribucion Capa 1 (categoria base, antes del diccionario de texto) ===\n")
print(table(base_capa1$categoria_base, useNA = "ifany"))

cat("\n=== Actividades marcadas como ambiguas (pasaran a Capa 2) ===\n")
print(table(base_capa1$purpose_code[base_capa1$ambiguo == TRUE]))

# Guardar para el script de Capa 2 (el que ya tienes: 03_capa2_regex.R)
saveRDS(base_capa1, paste0(ruta_salida, "base_capa1_12paises.rds"))
cat("\nGuardado: base_capa1_12paises.rds -> ahora corre 03_capa2_regex.R\n")