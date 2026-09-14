# ============================================================
# 00_setup.R
# Correr UNA VEZ al abrir el proyecto en RStudio
# ============================================================

# Si es la primera vez, instala los paquetes (quita el # y corre una vez)
#install.packages(c("arrow","dplyr","readxl","stringr","tidyr","purrr","openxlsx"))

library(arrow)
library(dplyr)
library(readxl)
library(stringr)
library(tidyr)
library(purrr)
library(openxlsx)

# --- IMPORTANTE: ajusta estas 3 rutas a tu carpeta nueva ---
# Usa RStudio: Session > Set Working Directory > To Source File Location
# o fija el proyecto (.Rproj) en la carpeta nueva para que las rutas relativas funcionen

ruta_parquet   <- "data/CRS.parquet"                  # tu archivo CRS bulk data
ruta_codebook  <- "data/codebook_codigos_v1.xlsx"           # el que descargaste hoy
ruta_diccionario <- "data/diccionario_capa2_v0.1.xlsx"      # el que descargaste hoy
ruta_salida    <- "data/"

# Verificacion rapida de que los archivos existen antes de seguir
stopifnot(
  "No encuentro el Parquet, revisa ruta_parquet" = file.exists(ruta_parquet),
  "No encuentro el codebook, revisa ruta_codebook" = file.exists(ruta_codebook),
  "No encuentro el diccionario, revisa ruta_diccionario" = file.exists(ruta_diccionario)
)
cat("Todos los archivos encontrados. Listo para correr 01_extraccion.R\n")