## upload ECA table
ECA <- read_xlsx(path = "BD_FINAL/ECA.xlsx", col_names = TRUE)
ECA <- as.data.frame(ECA)

#### General Check of the Information ####

data_base <- read_xlsx(path = "BD_FINAL/BD_FINAL_PUNO_CONSOLIDADO_GA47D.xlsx", col_names = TRUE)
str(data_base, list.len=ncol(data_base))

ubi <- c("Num_Ficha","Proy","Temporada","Cod_Cuen", "Cod_Subc","Codigo","Cod_Corto",
         "Nombre","Nom_Comp","Zona","Lugar","Distrito","Provincia","Vertiente","Cuenca",
         "Subcuenca","Microcuenca")

carac <- c("Tip_fuente","Cla_fuente","Uso_fuente",
           "PFQ","Tip_analisis","Muestreo","Monitoreo","Blanco","STD","Duplicado",
           "Asp_geologico","Desc_Lito","Lito_Predom","Morfo","m","Color",
           "Olor","Precipitados","Algas_Plantas",
           "Basurales","Animales","Letrinas_Silos","Poblacion","Pasivos",
           "Act_Antropica","Alte_Geologica","Eventos_Met",
           "Viento","Fotos", "Obs","Realizado")

data_base[ubi] <- lapply(data_base[ubi] , factor)
data_base[carac] <- lapply(data_base[carac] , factor)
str(data_base)

ubi <- c("Num_Ficha","Proy",
         "Fecha","Hora","Norte","Este","Cota",
         "Temporada","Cod_Cuen","Cod_Subc",
         "Codigo","Cod_Corto","Nombre","Nom_Comp",
         "Zona","Lugar","Distrito","Provincia","Vertiente",
         "Cuenca","Subcuenca","Microcuenca")
pfq <- c("T_fuente",
         #"Q",
         "NP",
         "Ph","pH_mV","Eh","ORP_mv","CE_uS_cm","TDS_mg_L","Salin_PSU",
         "R_Kohm_cm","OD_mgL","OD_porSat","Turbidez")

data_base <- data_base %>% filter(!is.na(Ph))
sapply(data_base, function(x) sum(is.na(x))) #verificar nulos
summary(data_base$Cla_fuente)
data_base <- data_base %>% filter(Uso_fuente!="Punto seco")

ubi <- c("Norte","Este","Subcuenca","Temporada")
carac <- c("Tip_fuente", "Cla_fuente", "Uso_fuente", "Pasivos", "Act_Antropica",
           "Alte_Geologica")
carac2 <- c("Color","Olor","Algas_Plantas", "Basurales", "Animales",
            "Letrinas_Silos")


#### Creaciacion de nuevas variables: ----

elements <- c("Ba", "Mg", "Li", "U", "Mn", "Mo", "Fe", "B", "As", "Zn", "Sb", "Al", "Ni", "Cr", "V", "Se", "Cu", "Pb", "Cd", "Be", "Ag", "Hg", "Ta")

for (element in elements) {
  col_dis <- paste0(element, "_dis")
  col_valdis <- paste0(element, "_valdis")
  col_valdis_cen <- paste0(element, "_valdis_cen")
  
  data_base[[col_valdis]] <- as.numeric(str_remove(data_base[[col_dis]], pattern = "<"))
  data_base[[col_valdis_cen]] <- ifelse(str_detect(data_base[[col_dis]], pattern = "<"), TRUE, FALSE)
}


#### Analizando en Cuenca Antauta: #####

Antauta <- data_base %>% filter(Subcuenca=="Antauta")
attach(Antauta)
Avenida <- Antauta %>% filter(Temporada=="Avenida")
Estiaje <- Antauta %>% filter(Temporada=="Estiaje")

str(Antauta)
str(Avenida)
str(Estiaje)
# Antauta tiene 174 datos (Avenida y Estiaje)
# Avenida 86
# Estiaje 88
summary(Antauta)

# Tabla1:
Antauta %>% 
  group_by(Temporada, Tip_fuente, Cla_fuente) %>% 
  summarise(conteo = n()) %>% arrange(desc(conteo))

attach(Avenida)
summary(Avenida)
attach(Estiaje)
summary(Estiaje)
### Filtro detectar maximos y cuantiles
# Ejemplo del Ni
Antauta %>% 
  dplyr::select(Codigo, Ni_valdis, Temporada) %>% 
  filter(Ni_valdis %in% c(0.0506,0.0570,0.2579,0.3135)) %>% arrange(desc(Ni_valdis))

Antauta %>% 
  dplyr::mutate(Zn_fit=gamma(Zn_valdis)) %>%
  select(Zn_valdis, Zn_fit)

### Data Completa: pH ####
library(fitdistrplus)
library(logspline)

par(mfrow=c(2,2))
# av <- SanGaban %>% filter(Temporada=="Avenida")
# es <- SanGaban %>% filter(Temporada=="Estiaje")
# plot(ecdf(av$Ph), col = "black", 
#      main = "Curva de Distribución de Frecuencias Acumuladas (ECDF) \nhttp://127.0.0.1:46479/graphics/plot_zoom_png?width=1920&height=1017 para data debajo del límite de detección",
#      xlab= "Valores de pH, adimensional", ylab = "Probabilidad Acumulada")
# par(new=TRUE)
# plot(ecdf(es$Ph), add=TRUE, col="red")

Antauta$Ph_cen <- rep(FALSE, nrow(Antauta))
attach(Antauta)
cen_ecdf(Ph, Ph_cen, xgroup = Temporada,
         Ylab = "Ph adimensional", xlim = c(0,14))

#ajuste cdf
fitnorm <- fitdist(Ph, "norm")
fitlnorm <- fitdist(Ph, "lnorm")
fitgamma <- fitdist(Ph, "gamma")
cdfcomp(list(fitnorm, fitlnorm, fitgamma), legendtext=c("norm","lnorm","gamma"))
gofstat(list(fitnorm, fitlnorm, fitgamma), fitnames=c("norm","lnorm","gamma"))

# Goodness-of-fit criteria
#                                  norm    lnorm    gamma
# Akaike's Information Criterion 496.5802 554.8276 532.4635
# Bayesian Information Criterion 502.8983 561.1457 538.7817


#denscomp(list(fitnorm, fitlnorm, fitgamma), legendtext=c("norm","lnorm","gamma"))
#ppcomp(list(fitnorm, fitlnorm, fitgamma), legendtext=c("norm","lnorm","gamma"))
#cdfplot - qqplot
#cdfcomp(fitdist(Ph, "norm"), legendtext = "norm")
cenQQ(Ph, Ph_cen, dist="norm")
boxplot(Ph ~ Temporada, col = c("blue", "skyblue"),
        main = "Boxplot de pH por Temporada",
        xlab = "Temporada",
        ylab = "pH, adimensional")

# En caso sea lnorm la distribucion
EnvStats::qqPlot(Mg_valdis, distribution = "lnorm", 
                 add.line = TRUE,  #estimate.params = TRUE,
                 line.col = "red", line.lwd = 1.5)

boxplot(log(Mg_valdis) ~ Temporada, col = c("blue", "skyblue"),
        main = "Boxplot de Mg mg/l por Temporada",
        xlab = "Temporada",
        ylab = "ln(Mg) mg/l")


# Identificar outliers:

Antauta %>% 
  dplyr::select(Codigo, Ph, Temporada) %>% 
  filter(Ph %in% c(4.48,4.50,5.10,5.16,9.35,9.71)) %>% arrange(desc(Ph))


attach(Avenida)
summary(Ph)
sd(Ph)

cuanti_av <- quantile(Avenida$Ph, probs = c(0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
quantile(Avenida$Ph, probs = 0.50)
cuanti_av
Avenida$Ph_cla <- ifelse(Avenida$Ph < cuanti_av[1], "Muy Bajo",
                         ifelse(Avenida$Ph >= cuanti_av[1] & Avenida$Ph < cuanti_av[2], "Bajo",
                                ifelse(Avenida$Ph >= cuanti_av[2] & Avenida$Ph < cuanti_av[3], "Moderado",
                                       ifelse(Avenida$Ph >= cuanti_av[3] & Avenida$Ph < cuanti_av[4], "Alto", "Muy Alto"))))

Avenida$Ph_cla <- factor(Avenida$Ph_cla, levels = c("Muy Bajo", "Bajo", "Moderado","Alto", "Muy Alto"))
Avenida %>% select(Codigo, Ph_cla, Ph) %>% group_by(Ph_cla) %>% summarise(conteo = n())
Avenida %>% select(Codigo, Ph_cla) %>% filter(Ph_cla=="Muy Bajo" | Ph_cla=="Muy Alto") %>% arrange(desc(Ph_cla))

attach(Estiaje)
summary(Ph)
sd(Ph)

cuanti_es <- quantile(Estiaje$Ph, probs = c(0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
cuanti_es
Estiaje$Ph_cla <- ifelse(Estiaje$Ph < cuanti_es[1], "Muy Bajo",
                         ifelse(Estiaje$Ph >= cuanti_es[1] & Estiaje$Ph < cuanti_es[2], "Bajo",
                                ifelse(Estiaje$Ph >= cuanti_es[2] & Estiaje$Ph < cuanti_es[3], "Moderado",
                                       ifelse(Estiaje$Ph >= cuanti_es[3] & Estiaje$Ph < cuanti_es[4], "Alto", "Muy Alto")))) 

Estiaje$Ph_cla <- factor(Estiaje$Ph_cla, levels = c("Muy Bajo", "Bajo", "Moderado","Alto", "Muy Alto"))
Estiaje %>% select(Codigo, Ph_cla, Ph) %>% group_by(Ph_cla) %>% summarise(conteo = n()) 
Estiaje %>% select(Codigo, Ph_cla) %>% filter(Ph_cla=="Muy Bajo" | Ph_cla=="Muy Alto") %>% arrange(desc(Ph_cla))

descdist(Ph, discrete = FALSE)


#### Data Incompleta: Li ####


attach(Grande)
## Se realiza revision de boxplot de valores frente Av-Es
par(mfrow=c(2,2))
cen_ecdf(Li_valdis, Li_valdis_cen, xgroup = Temporada,
         Ylab = "Li concentration, in mg/L", xlim = c(0,0.5))

cenCompareCdfs(Li_valdis, Li_valdis_cen, Yname = "Li mg/l")

cenQQ(Li_valdis, Li_valdis_cen)

# elegimos distribucion lognormal
cboxplot(Li_valdis, Li_valdis_cen, xgroup = Temporada, LOG = TRUE,  bxcol = c("blue", "skyblue"),
         minmax = FALSE, show = TRUE,
         Title = "Boxplot de Li mg/l por Temporada",
         Xlab = "Temporada",
         Ylab = "ln(Li) mg/l")

cenCompareQQ(Li_valdis, Li_valdis_cen, Yname = "Li mg/l")

# realizamod sumario del Li y reclasificamos

attach(Avenida)
censummary(Li_valdis, Li_valdis_cen)

Li.ros <- cenros(Li_valdis, Li_valdis_cen)
summary(Li.ros)
mean(Li.ros)
median(Li.ros)
sd(Li.ros)
quantile(Li.ros)
sort(Li_valdis)


Li.ros.envstats <- elnormAltCensored(Li_valdis, Li_valdis_cen, method = "rROS", ci=TRUE, ci.method = "bootstrap", n.bootstraps = 5000)
Li.ros.envstats

censtats(Li_valdis, Li_valdis_cen)

cuanti_av <- quantile(Li.ros, probs = c(0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
cuanti_av
Avenida$Li_cla <- ifelse(Avenida$Li_valdis < cuanti_av[1], "Muy Bajo",
                         ifelse(Avenida$Li_valdis >= cuanti_av[1] & Avenida$Li_valdis < cuanti_av[2], "Bajo",
                                ifelse(Avenida$Li_valdis >= cuanti_av[2] & Avenida$Li_valdis < cuanti_av[3], "Moderado",
                                       ifelse(Avenida$Li_valdis >= cuanti_av[3] & Avenida$Li_valdis < cuanti_av[4], "Alto", "Muy Alto"))))

Avenida$Li_cla <- factor(Avenida$Li_cla, levels = c("Muy Bajo", "Bajo", "Moderado","Alto", "Muy Alto"))
Avenida %>% select(Codigo, Li_cla, Li_valdis) %>% group_by(Li_cla) %>% summarise(conteo = n())
Avenida %>% select(Codigo, Li_cla) %>% filter(Li_cla=="Muy Alto") %>% arrange(desc(Li_cla))


attach(Estiaje)
censummary(Li_valdis, Li_valdis_cen)

cuanti_es <- quantile(Estiaje$Li_valdis, probs = c(0.025, 0.25, 0.75, 0.975), na.rm = TRUE)
cuanti_es
Estiaje$Li_cla <- ifelse(Estiaje$Li_valdis < cuanti_es[1], "Muy Bajo",
                         ifelse(Estiaje$Li_valdis >= cuanti_es[1] & Estiaje$Li_valdis < cuanti_es[2], "Bajo",
                                ifelse(Estiaje$Li_valdis >= cuanti_es[2] & Estiaje$Li_valdis < cuanti_es[3], "Moderado",
                                       ifelse(Estiaje$Li_valdis >= cuanti_es[3] & Estiaje$Li_valdis < cuanti_es[4], "Alto", "Muy Alto"))))

Estiaje$Li_cla <- factor(Estiaje$Li_cla, levels = c("Muy Bajo", "Bajo", "Moderado","Alto", "Muy Alto"))
Estiaje %>% select(Codigo, Li_cla, Li_valdis) %>% group_by(Li_cla) %>% summarise(conteo = n())
Estiaje %>% select(Codigo, Li_cla) %>% filter(Li_cla=="Muy Alto") %>% arrange(desc(Li_cla))








