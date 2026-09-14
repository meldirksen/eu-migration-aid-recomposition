# ============================================================
# 07_synth_niger.R
# Control sintetico de Niger. Resultado = sh_C (participacion del desarrollo).
# Tratamiento en 2016 (EUTF + ley nigerina antitrafico 2015). Ajuste 2007-2015.
# Dos configuraciones de donantes (robustez):
#   (1) "limpio" : 26 paises SSA no-EUTF con panel completo
#   (2) "sahel"  : los otros 11 paises EUTF (contaminado -> cota inferior)
#
# Insumo: data/panel_scm.rds  (salida de 06)
# Salida: graficos en output/ + brecha estimada + inferencia por placebos
#
# COMO USARLO: copia a scripts/, Restart R, corre 00_setup.R y luego este.
# La primera vez: install.packages("tidysynth")
# ============================================================
library(dplyr)
library(tidysynth)   # si falla: install.packages("tidysynth")

if (!dir.exists("output")) dir.create("output")
panel <- readRDS("data/panel_scm.rds")

anio_trat <- 2016
pre <- 2007:2015

# ------------------------------------------------------------
# Funcion que corre el SCM para un pool dado y guarda resultados
# ------------------------------------------------------------
correr_scm <- function(panel, grupos_donantes, etiqueta) {
  
  # 1. Subpanel: Niger (tratado) + donantes del pool elegido
  sub <- panel %>%
    filter(grupo == "tratado" | grupo %in% grupos_donantes)
  
  # 2. Solo unidades con panel COMPLETO y sh_C sin NA (SCM lo exige)
  sub <- sub %>% filter(!is.na(sh_C), is.finite(sh_C))
  completos <- sub %>% group_by(recipient_name) %>%
    summarise(n = n_distinct(year), .groups = "drop") %>%
    filter(n == length(2007:2024)) %>% pull(recipient_name)
  sub <- sub %>% filter(recipient_name %in% completos)
  
  cat("\n############ POOL:", etiqueta, "############\n")
  cat("Unidades usadas (incl. Niger):", n_distinct(sub$recipient_name), "\n")
  
  # 3. Control sintetico
  sc <- sub %>%
    synthetic_control(outcome = sh_C, unit = recipient_name, time = year,
                      i_unit = "Niger", i_time = anio_trat,
                      generate_placebos = TRUE) %>%
    generate_predictor(time_window = pre, sh_C_media = mean(sh_C, na.rm = TRUE)) %>%
    generate_predictor(time_window = 2009, sh_C_2009 = sh_C) %>%
    generate_predictor(time_window = 2012, sh_C_2012 = sh_C) %>%
    generate_predictor(time_window = 2015, sh_C_2015 = sh_C) %>%
    generate_weights(optimization_window = pre) %>%
    generate_control()
  
  # 4. Graficos
  ggplot2::ggsave(paste0("output/synth_niger_", etiqueta, "_trayectoria.png"),
                  plot_trends(sc) + ggplot2::labs(subtitle = paste("Pool:", etiqueta)),
                  width = 8, height = 5, dpi = 300)
  ggplot2::ggsave(paste0("output/synth_niger_", etiqueta, "_brecha.png"),
                  plot_differences(sc) + ggplot2::labs(subtitle = paste("Pool:", etiqueta)),
                  width = 8, height = 5, dpi = 300)
  ggplot2::ggsave(paste0("output/synth_niger_", etiqueta, "_placebos.png"),
                  plot_placebos(sc), width = 8, height = 5, dpi = 300)
  ggplot2::ggsave(paste0("output/synth_niger_", etiqueta, "_mspe.png"),
                  plot_mspe_ratio(sc), width = 7, height = 5, dpi = 300)
  
  # 5. Pesos de los donantes (para reportar de que se compone el Niger sintetico)
  cat("\n-- Pesos de los donantes (top 10) --\n")
  print(grab_unit_weights(sc) %>% arrange(desc(weight)) %>% head(10))
  
  # 6. Brecha (efecto) por anio y promedio post-tratamiento
  sint <- grab_synthetic_control(sc) %>%
    mutate(brecha = real_y - synth_y)
  cat("\n-- Brecha observado - sintetico (por anio) --\n")
  print(as.data.frame(sint %>% mutate(across(where(is.numeric), ~round(.x, 3)))),
        row.names = FALSE)
  cat("\nBrecha media POST (2016-2024):",
      round(mean(sint$brecha[sint$time_unit >= anio_trat]), 3), "\n")
  
  # 7. Inferencia por placebos (Fisher): ratio MSPE post/pre y p-valor
  cat("\n-- Significancia (placebos) --\n")
  print(grab_significance(sc))
  
  invisible(sc)
}

# ------------------------------------------------------------
# Correr las dos configuraciones
# ------------------------------------------------------------
sc_limpio <- correr_scm(panel, "limpio", "limpio")
sc_sahel  <- correr_scm(panel, "sahel",  "sahel")

cat("\n>>> Listo. Pega toda esta salida en el chat (pesos, brecha, significancia).\n")
cat(">>> Graficos en output/: trayectoria, brecha, placebos y mspe para cada pool.\n")