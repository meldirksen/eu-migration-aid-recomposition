# Resultados descriptivos del panel de 12 paises (ventana EUTF Sahel/Lago Chad)
# Insumo: crs_clasificado_12paises_PROVISIONAL.rds  (salida de 03_capa2_regex.R)
#
# COMO USARLO:
#   1) Sesion > Restart R  (para partir limpio)
#   2) Corre 00_setup.R
#   3) Corre este script entero (Cmd+A, Cmd+Enter)
#   NO cargues 'rlang' ni ejecutes install.packages si ya tienes los paquetes.
#
# NOTA metodologica: el desembolso (usd_disbursement_defl) subestima 2022-2024
# por el rezago compromiso->desembolso. Declararlo al interpretar la ultima era.
# ============================================================

# --- Paquetes (SOLO la primera vez; si ya los tienes, NO ejecutes esta linea) ---
# install.packages(c("dplyr","tidyr","ggplot2","openxlsx","scales","tibble"))
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)
library(scales)

# --- Periodo de estudio (filtra registros viejos del CRS) ---
anio_inicio <- 2007   # tu periodo declarado es 2007-2024; cambia a 2008 si lo prefieres

# --- Rutas (relativas al proyecto .Rproj) ---
ruta_rds <- "data/crs_clasificado_12paises_v1.rds"
ruta_output <- "output/"
if (!dir.exists(ruta_output)) dir.create(ruta_output)

stopifnot("No encuentro el rds clasificado; corre 03 antes" = file.exists(ruta_rds))
base <- readRDS(ruta_rds)

# ============================================================
# 0. Chequeos de integridad
# ============================================================
cat("=== INTEGRIDAD DEL PANEL ===\n")
cat("Actividades totales:", nrow(base), "\n")
cat("Paises (deben ser 12):", n_distinct(base$recipient_name), "\n")
print(sort(unique(base$recipient_name)))
cat("Rango de anios:", min(base$year, na.rm = TRUE), "-", max(base$year, na.rm = TRUE), "\n")
cat("Desembolso con NA:", sum(is.na(base$usd_disbursement_defl)), "\n")
cat("Categorias (amplia):\n");   print(table(base$categoria_final_amplia,   useNA = "ifany"))
cat("Categorias (estrecha):\n"); print(table(base$categoria_final_estrecha, useNA = "ifany"))

etiquetas <- c(
  A = "Control migratorio (A)",
  B = "Migracion-desarrollo (B)",
  C = "Desarrollo (C)",
  S = "Seguridad/paz no migratoria (S)",
  H = "Humanitario (H)",
  D = "Residual (D)",
  R = "Refugiados en pais donante (R)"
)

# ============================================================
# 1. Preparacion: periodos y eras. R se EXCLUYE del denominador.
# ============================================================
dat <- base %>%
  filter(!is.na(usd_disbursement_defl), usd_disbursement_defl >= 0,
         year >= anio_inicio) %>%
  mutate(
    periodo = if_else(year <= 2015, "<=2015", ">=2016"),
    era = case_when(
      year <= 2015                ~ "1_pre-EUTF (<=2015)",
      year >= 2016 & year <= 2021 ~ "2_EUTF (2016-2021)",
      year >= 2022                ~ "3_NDICI (2022-2024)"
    )
  )

# Helper SIN tidy-eval: recibe nombres de columna como texto.
# Devuelve columnas fijas: tiempo, categoria, usd, share.
participaciones <- function(df, var_cat, var_tiempo) {
  d <- df[!is.na(df[[var_cat]]) & df[[var_cat]] != "R", ]
  d$categoria <- d[[var_cat]]
  d$tiempo    <- d[[var_tiempo]]
  d %>%
    group_by(tiempo, categoria) %>%
    summarise(usd = sum(usd_disbursement_defl), .groups = "drop") %>%
    group_by(tiempo) %>%
    mutate(share = usd / sum(usd)) %>%
    ungroup()
}

# ============================================================
# 2. Participaciones <=2015 vs >=2016  (actualiza tu Tabla 2)
# ============================================================
tabla_2periodos <- function(var_cat) {
  participaciones(dat, var_cat, "periodo") %>%
    select(categoria, tiempo, share) %>%
    pivot_wider(names_from = tiempo, values_from = share, values_fill = 0) %>%
    mutate(cambio_pp = (`>=2016` - `<=2015`) * 100) %>%
    arrange(desc(`>=2016`))
}
t2_amplia   <- tabla_2periodos("categoria_final_amplia")
t2_estrecha <- tabla_2periodos("categoria_final_estrecha")
cat("\n=== PARTICIPACIONES <=2015 vs >=2016 (AMPLIA) ===\n");   print(t2_amplia,   n = 20)
cat("\n=== PARTICIPACIONES <=2015 vs >=2016 (ESTRECHA) ===\n"); print(t2_estrecha, n = 20)

# ============================================================
# 3. Participaciones por 3 eras (pre-EUTF / EUTF / NDICI)
# ============================================================
tabla_3eras <- function(var_cat) {
  participaciones(dat, var_cat, "era") %>%
    select(categoria, tiempo, share) %>%
    pivot_wider(names_from = tiempo, values_from = share, values_fill = 0)
}
t3_amplia   <- tabla_3eras("categoria_final_amplia")
t3_estrecha <- tabla_3eras("categoria_final_estrecha")
cat("\n=== PARTICIPACIONES POR ERA (AMPLIA) ===\n");   print(t3_amplia,   n = 20)
cat("\n=== PARTICIPACIONES POR ERA (ESTRECHA) ===\n"); print(t3_estrecha, n = 20)

# ============================================================
# 4. Desembolso absoluto por era
#    OJO: usd_disbursement_defl del CRS ya viene en MILLONES USD.
#    Por eso NO se divide entre 1e6. Se muestra en millones.
# ============================================================
usd_absoluto <- dat %>%
  filter(!is.na(categoria_final_amplia)) %>%
  group_by(era, categoria_final_amplia) %>%
  summarise(usd_millones = sum(usd_disbursement_defl), .groups = "drop") %>%
  pivot_wider(names_from = era, values_from = usd_millones, values_fill = 0)
cat("\n=== DESEMBOLSO ABSOLUTO por era (millones USD 2024) ===\n")
print(usd_absoluto, n = 20)

# ============================================================
# 5. Serie anual + figura de area apilada al 100% con hitos
# ============================================================
serie_anual <- participaciones(dat, "categoria_final_amplia", "year") %>%
  rename(year = tiempo)

orden <- c("C", "H", "S", "B", "A", "D")
orden <- orden[orden %in% unique(serie_anual$categoria)]
serie_anual <- serie_anual %>% mutate(categoria = factor(categoria, levels = orden))

hitos <- tibble::tibble(
  year  = c(2015, 2016, 2021, 2024),
  label = c("La Valeta", "EUTF", "NDICI", "Pacto 2024")
)

fig <- ggplot(serie_anual, aes(year, share, fill = categoria)) +
  geom_area(position = "fill", colour = "white", linewidth = 0.1) +
  geom_vline(data = hitos, aes(xintercept = year),
             linetype = "dashed", linewidth = 0.4, colour = "grey20") +
  geom_text(data = hitos, aes(x = year, y = 1.02, label = label),
            inherit.aes = FALSE, vjust = 0, size = 3) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_x_continuous(breaks = seq(2008, 2024, 2)) +
  scale_fill_brewer(palette = "Set2", labels = etiquetas[orden]) +
  labs(x = NULL, y = "Participacion del desembolso", fill = NULL,
       title = "Composicion sectorial de la AOD de la UE (12 paises Sahel/Lago Chad)",
       subtitle = "Definicion amplia. Desembolsos brutos, USD constantes 2024. R excluida.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(paste0(ruta_output, "figura_area_composicion.png"), fig,
       width = 9, height = 5.5, dpi = 300)
cat("\nFigura guardada: output/figura_area_composicion.png\n")

# ============================================================
# 6. Exportar tablas a Excel
# ============================================================
wb <- createWorkbook()
addWorksheet(wb, "2periodos_amplia");   writeData(wb, "2periodos_amplia",   t2_amplia)
addWorksheet(wb, "2periodos_estrecha"); writeData(wb, "2periodos_estrecha", t2_estrecha)
addWorksheet(wb, "3eras_amplia");       writeData(wb, "3eras_amplia",       t3_amplia)
addWorksheet(wb, "3eras_estrecha");     writeData(wb, "3eras_estrecha",     t3_estrecha)
addWorksheet(wb, "usd_absoluto");       writeData(wb, "usd_absoluto",       usd_absoluto)
addWorksheet(wb, "serie_anual");        writeData(wb, "serie_anual",        serie_anual)
saveWorkbook(wb, paste0(ruta_output, "resultados_descriptivos_12paises.xlsx"), overwrite = TRUE)
cat("Tablas guardadas: output/resultados_descriptivos_12paises.xlsx\n")

# ============================================================
# 7. Verificacion final: participaciones por anio suman ~1
# ============================================================
chk <- serie_anual %>% group_by(year) %>% summarise(s = sum(share))
cat("\n=== CHECK: suma de participaciones por anio (debe ser ~1) ===\n"); print(chk, n = 30)
stopifnot("Las participaciones anuales no suman 1" = all(abs(chk$s - 1) < 1e-6))
cat("\nTODO OK. Pega la salida de consola aqui para redactar RESULTADOS.\n")

