# ============================================================
# 08_descriptivo_enriquecido.R
# Analisis descriptivo del panel clasificado (12 paises), en cuatro bloques:
#   (1) Estadistica descriptiva (tendencia central, dispersion, forma, correlacion)
#   (3) Heterogeneidad por pais  -> sh_A / sh_S por pais + mapa de calor
#   (4) Descomposicion del desarrollo -> que subsectores de C perdieron peso
#   (5) Composicion del control (A) -> fronteras / retorno / antitrafico / gestion
#
# Insumo: data/crs_clasificado_12paises_v1.rds
# Salidas: output/descriptivo_enriquecido.xlsx + figuras PNG
# COMO USARLO: copia a scripts/, Restart R, corre 00_setup.R y luego este.
# ============================================================
library(dplyr); library(tidyr); library(stringr); library(stringi)
library(ggplot2); library(openxlsx); library(scales)

if (!dir.exists("output")) dir.create("output")
anio_min <- 2007; anio_max <- 2024

base12 <- readRDS("data/crs_clasificado_12paises_v1.rds") %>%
  filter(!is.na(usd_disbursement_defl), usd_disbursement_defl >= 0,
         year >= anio_min, year <= anio_max)

cat_lab <- c(A="Control migratorio", B="Migracion-desarrollo", C="Desarrollo",
             S="Seguridad", H="Humanitario", D="Residual")

# ============================================================
# 0. PANEL PAIS x AÑO de participaciones (unidad de analisis descriptivo)
# ============================================================
panel_cy <- base12 %>%
  filter(categoria_final_amplia != "R") %>%
  group_by(recipient_name, year) %>%
  summarise(total = sum(usd_disbursement_defl),
            A = sum(usd_disbursement_defl[categoria_final_amplia=="A"]),
            B = sum(usd_disbursement_defl[categoria_final_amplia=="B"]),
            C = sum(usd_disbursement_defl[categoria_final_amplia=="C"]),
            S = sum(usd_disbursement_defl[categoria_final_amplia=="S"]),
            H = sum(usd_disbursement_defl[categoria_final_amplia=="H"]),
            .groups="drop") %>%
  filter(total > 0) %>%
  mutate(sh_A=A/total, sh_B=B/total, sh_C=C/total, sh_S=S/total, sh_H=H/total,
         log_total = log(total))

# ============================================================
# (1) ESTADISTICA DESCRIPTIVA
# ============================================================
skew <- function(x){x<-x[!is.na(x)];n<-length(x);m<-mean(x);s<-sqrt(sum((x-m)^2)/n);
if(s==0) return(NA); (sum((x-m)^3)/n)/s^3}
kurt <- function(x){x<-x[!is.na(x)];n<-length(x);m<-mean(x);s<-sqrt(sum((x-m)^2)/n);
if(s==0) return(NA); (sum((x-m)^4)/n)/s^4 - 3}  # exceso (normal=0)

resumen_var <- function(x){
  x <- x[!is.na(x)]
  tibble(n=length(x), media=mean(x), mediana=median(x),
         sd=sd(x), varianza=var(x),
         min=min(x), p25=quantile(x,.25), p75=quantile(x,.75),
         RIC=IQR(x), max=max(x), asimetria=skew(x), curtosis_exceso=kurt(x))
}
vars <- c("sh_A","sh_B","sh_C","sh_S","sh_H","total","log_total")
desc_stats <- bind_rows(lapply(vars, function(v)
  resumen_var(panel_cy[[v]]) %>% mutate(variable=v, .before=1)))
cat("=== (1) ESTADISTICA DESCRIPTIVA (unidad: pais-año, N =", nrow(panel_cy), ") ===\n")
print(as.data.frame(desc_stats %>% mutate(across(where(is.numeric), ~round(.x,3)))), row.names=FALSE)
cat("\nNota: la 'moda' no es informativa en variables continuas; se omite.\n")

# Correlaciones (con aviso composicional): shares vs. niveles en log
sh_mat <- panel_cy %>% select(sh_A,sh_B,sh_C,sh_S,sh_H,log_total)
cor_p <- round(cor(sh_mat, method="pearson",  use="complete.obs"),2)
cor_s <- round(cor(sh_mat, method="spearman", use="complete.obs"),2)
cat("\n=== Correlacion de Pearson (OJO: shares son composicionales) ===\n"); print(cor_p)
cat("\n=== Correlacion de Spearman ===\n"); print(cor_s)

# Figuras descriptivas
dens <- panel_cy %>% select(sh_A,sh_B,sh_C,sh_S,sh_H) %>%
  pivot_longer(everything(), names_to="var", values_to="valor")
ggsave("output/desc_histogramas.png",
       ggplot(dens, aes(valor)) + geom_histogram(bins=30, fill="#4575b4") +
         facet_wrap(~var, scales="free") + theme_minimal(base_size=10) +
         labs(title="Distribucion de las participaciones (pais-año)", x="Participacion", y="Frec."),
       width=9, height=5.5, dpi=300)

pc_era <- panel_cy %>% mutate(era = case_when(year<=2015~"pre-EUTF",
                                              year<=2021~"EUTF", TRUE~"NDICI"),
                              era = factor(era, levels=c("pre-EUTF","EUTF","NDICI")))
ggsave("output/desc_boxplot_shC_era.png",
       ggplot(pc_era, aes(era, sh_C, fill=era)) + geom_boxplot(alpha=.7) +
         scale_y_continuous(labels=percent) + guides(fill="none") +
         theme_minimal(base_size=10) +
         labs(title="Participacion del desarrollo por era (boxplot)", x=NULL, y="sh_C"),
       width=7, height=5, dpi=300)

ggsave("output/desc_matriz_correlacion.png",
       as.data.frame(cor_p) %>% tibble::rownames_to_column("v1") %>%
         pivot_longer(-v1, names_to="v2", values_to="r") %>%
         ggplot(aes(v1,v2,fill=r)) + geom_tile(colour="white") +
         geom_text(aes(label=r), size=3) +
         scale_fill_gradient2(low="#b2182b", mid="white", high="#2166ac", limits=c(-1,1)) +
         theme_minimal(base_size=10) + labs(title="Matriz de correlaciones (Pearson)", x=NULL, y=NULL),
       width=7, height=6, dpi=300)

# ============================================================
# (3) HETEROGENEIDAD POR PAIS  (>=2016)
# ============================================================
het <- base12 %>%
  filter(categoria_final_amplia != "R", year >= 2016) %>%
  group_by(recipient_name, cat = categoria_final_amplia) %>%
  summarise(usd = sum(usd_disbursement_defl), .groups="drop") %>%
  group_by(recipient_name) %>% mutate(share = usd/sum(usd)) %>% ungroup()
het_wide <- het %>% select(recipient_name, cat, share) %>%
  pivot_wider(names_from=cat, values_from=share, values_fill=0) %>%
  mutate(securitizado_AS = A + S) %>% arrange(desc(securitizado_AS))
cat("\n=== (3) Participaciones por pais, >=2016 (ordenado por A+S) ===\n")
print(as.data.frame(het_wide %>% mutate(across(where(is.numeric), ~round(.x,3)))), row.names=FALSE)

orden_pais <- het_wide$recipient_name
ggsave("output/het_mapa_calor.png",
       het %>% mutate(recipient_name=factor(recipient_name, levels=rev(orden_pais)),
                      cat=factor(cat, levels=c("A","S","B","H","D","C"))) %>%
         ggplot(aes(cat, recipient_name, fill=share)) + geom_tile(colour="white") +
         geom_text(aes(label=ifelse(share>=0.005, sprintf("%.1f%%",share*100), "")), size=2.7) +
         scale_fill_gradient(low="grey95", high="#b2182b", labels=percent) +
         scale_x_discrete(labels=cat_lab[c("A","S","B","H","D","C")]) +
         labs(x=NULL,y=NULL,fill="Part.", title="Composicion de la AOD europea por pais (>=2016)") +
         theme_minimal(base_size=10) + theme(axis.text.x=element_text(angle=20,hjust=1)),
       width=9, height=5.5, dpi=300)

het_abs <- base12 %>% filter(year>=2016, categoria_final_amplia %in% c("A","S")) %>%
  group_by(recipient_name) %>%
  summarise(control_A=sum(usd_disbursement_defl[categoria_final_amplia=="A"]),
            seguridad_S=sum(usd_disbursement_defl[categoria_final_amplia=="S"]),
            .groups="drop") %>%
  mutate(AS_total=control_A+seguridad_S) %>% arrange(desc(AS_total))
cat("\n=== (3) Absoluto A y S por pais, >=2016 (millones USD) ===\n")
print(as.data.frame(het_abs %>% mutate(across(where(is.numeric), ~round(.x,1)))), row.names=FALSE)

# ============================================================
# (4) DESCOMPOSICION DEL DESARROLLO (C) POR SUBSECTOR
# ============================================================
grupo_sector <- function(pc){ g <- pc %/% 100
case_when(g %in% 111:116~"Educacion", g %in% 121:123~"Salud",
          g==130~"Poblacion/salud reprod.", g==140~"Agua y saneamiento",
          g==160~"Otros sociales", g %in% 210:250~"Infraestr. economica",
          g %in% 311:313~"Agric/pesca/silvic.", g %in% 321:323~"Industria/mineria",
          g %in% 331:332~"Comercio y turismo", g==410~"Medio ambiente",
          g %in% 430:432~"Multisectorial", TRUE~"Otros desarrollo") }
tot_per <- base12 %>% filter(categoria_final_amplia!="R") %>%
  mutate(periodo=if_else(year<=2015,"pre","post")) %>%
  group_by(periodo) %>% summarise(usd_tot=sum(usd_disbursement_defl), .groups="drop")
desc_C <- base12 %>% filter(categoria_final_amplia=="C") %>%
  mutate(periodo=if_else(year<=2015,"pre","post"), subsector=grupo_sector(purpose_code)) %>%
  group_by(periodo, subsector) %>% summarise(usd=sum(usd_disbursement_defl), .groups="drop") %>%
  left_join(tot_per, by="periodo") %>% mutate(share=usd/usd_tot) %>%
  select(subsector, periodo, share) %>%
  pivot_wider(names_from=periodo, values_from=share, values_fill=0) %>%
  mutate(cambio_pp=(post-pre)*100) %>% arrange(cambio_pp)
cat("\n=== (4) Subsectores de desarrollo: share sobre total, pre vs post 2015 ===\n")
print(as.data.frame(desc_C %>% mutate(across(where(is.numeric), ~round(.x,4)))), row.names=FALSE)
ggsave("output/desc_desarrollo_subsectores.png",
       desc_C %>% ggplot(aes(reorder(subsector,cambio_pp), cambio_pp, fill=cambio_pp>0)) +
         geom_col() + coord_flip() +
         scale_fill_manual(values=c("#b2182b","#2166ac"), guide="none") +
         labs(x=NULL, y="Cambio participacion (pp) post vs pre 2015",
              title="Sectores de desarrollo: quien gana y quien pierde peso") +
         theme_minimal(base_size=10),
       width=8.5, height=5, dpi=300)

# ============================================================
# (5) COMPOSICION DEL CONTROL (A): que tipo de control
# ============================================================
A_act <- base12 %>% filter(categoria_final_amplia=="A") %>%
  mutate(txt = tolower(stri_trans_general(
    paste(project_title, short_description, long_description), "Latin-ASCII")),
    tipo = case_when(
      str_detect(txt,"retour|return|readmiss|expuls|deport")               ~"Retorno/readmision",
      str_detect(txt,"trafic|traite|smuggl|trafficking")                   ~"Antitrafico/trata",
      str_detect(txt,"frontier|border|surveillance|garde-cote|coast guard|gar-si|interception|maritim|search and rescue|recherche et sauvetage") ~"Fronteras/vigilancia",
      str_detect(txt,"gestion des migration|gestion de la migration|migration management|gouvernance|governance") ~"Gestion/gobernanza migratoria",
      TRUE ~ "Otro control"),
    era = case_when(year<=2015~"pre-EUTF", year<=2021~"EUTF", TRUE~"NDICI"))
comp_A <- A_act %>% group_by(tipo) %>%
  summarise(n_actividades=n(), usd_millones=sum(usd_disbursement_defl), .groups="drop") %>%
  mutate(share_usd = usd_millones/sum(usd_millones)) %>% arrange(desc(usd_millones))
cat("\n=== (5) Composicion del control migratorio (A) por tipo ===\n")
print(as.data.frame(comp_A %>% mutate(across(where(is.numeric), ~round(.x,3)))), row.names=FALSE)
comp_A_era <- A_act %>% group_by(era, tipo) %>%
  summarise(usd=sum(usd_disbursement_defl), .groups="drop")
ggsave("output/control_A_composicion.png",
       comp_A_era %>% mutate(era=factor(era, levels=c("pre-EUTF","EUTF","NDICI"))) %>%
         ggplot(aes(era, usd, fill=tipo)) + geom_col(position="fill") +
         scale_y_continuous(labels=percent) + scale_fill_brewer(palette="Set2") +
         labs(x=NULL, y="Composicion del gasto en control (A)", fill=NULL,
              title="De que se compone el control migratorio y como cambia") +
         theme_minimal(base_size=10) + theme(legend.position="bottom"),
       width=8, height=5, dpi=300)

# ============================================================
# EXPORT
# ============================================================
wb <- createWorkbook()
addWorksheet(wb,"1_descriptiva");      writeData(wb,"1_descriptiva", desc_stats)
addWorksheet(wb,"1_corr_pearson");     writeData(wb,"1_corr_pearson", as.data.frame(cor_p), rowNames=TRUE)
addWorksheet(wb,"1_corr_spearman");    writeData(wb,"1_corr_spearman", as.data.frame(cor_s), rowNames=TRUE)
addWorksheet(wb,"3_heterogeneidad");   writeData(wb,"3_heterogeneidad", het_wide)
addWorksheet(wb,"3_absoluto_AS");      writeData(wb,"3_absoluto_AS", het_abs)
addWorksheet(wb,"4_desarrollo_subsec");writeData(wb,"4_desarrollo_subsec", desc_C)
addWorksheet(wb,"5_control_tipo");     writeData(wb,"5_control_tipo", comp_A)
saveWorkbook(wb, "output/descriptivo_enriquecido.xlsx", overwrite=TRUE)

cat("\n>>> Guardado xlsx + figuras en output/. Pega las tablas de consola en el chat.\n")