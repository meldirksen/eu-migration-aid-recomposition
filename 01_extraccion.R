# ============================================================
# 01_extraccion.R
# Extrae del CRS bulk data solo lo que necesitamos:
# donante UE, AOD, perspectiva bilateral, 12 paises Sahel/Lago Chad
# Requiere haber corrido 00_setup.R antes
# ============================================================

# --- Los 12 paises de la ventana EUTF Sahel y Lago Chad ---
# OJO: primero verificamos que las grafias coincidan con tus datos reales
paises_sahel_lago_chad <- c(
  "Mauritania", "Niger", "Nigeria", "Senegal",
  "Burkina Faso", "Mali", "Côte d'Ivoire", "Guinea",
  "Ghana", "Gambia", "Cameroon", "Chad"
)

# Paso 3: extraccion con los filtros acordados
base <- crs %>%
  filter(
    donor_code == 918,               # Instituciones UE
    category == 10,                  # AOD unicamente
    bi_multi == 1,                   # Perspectiva bilateral del donante
    recipient_name %in% paises_sahel_lago_chad
  ) %>%
  select(
    year, donor_code, recipient_name, recipient_code,
    purpose_code, sector_name, project_title,
    short_description, long_description,
    usd_disbursement_defl
  ) %>%
  collect()

cat("\nActividades extraidas (12 paises):", nrow(base), "\n")
cat("Paises encontrados:", n_distinct(base$recipient_name), "de 12 esperados\n")
print(sort(unique(base$recipient_name)))

# Guardar para el siguiente script
saveRDS(base, paste0(ruta_salida, "base_extraida_12paises.rds"))
cat("\nGuardado: base_extraida_12paises.rds -> ahora corre 02_capa1_lookup.R\n")