
#Lectura y estructura de data

data_base <- read_xlsx(path = "BD_GA47D_TUMBES_CONSOLIDADA_05_2023.xlsx",sheet = "BD_PROCESAMIENTO", col_names = TRUE)
str(data_base, list.len=ncol(data_base))


#### Revision estructura ##########################################################################################
# definiendo grupos de variables

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

ubi <- c("Num_Ficha","Proy","Fecha","Hora","Norte","Este","Cota","Temporada","Cod_Cuen","Cod_Subc",
         "Codigo","Cod_Corto","Nombre","Nom_Comp","Zona","Lugar","Distrito","Provincia","Vertiente",
         "Cuenca","Subcuenca","Microcuenca")

pfq <- c("T_fuente",#"Q",
         "NP","Ph","pH_mV","Eh","ORP_mv","CE_uS_cm","TDS_mg_L","Salin_PSU",
         "R_Kohm_cm","OD_mgL","OD_porSat","Turbidez")

data_base <- data_base %>% filter(!is.na(Tip_fuente)) # criterio para eliminar controles (QA/QC)
sapply(data_base, function(x) sum(is.na(x))) #verificar nulos
data_base <- data_base %>% filter(!is.na(Ca_tot)) # se elimina porque es Seco o no se tomo!

# Revisas la estructura correctamente escrita:
summary(data_base) # Dar una ojeada a cada variable

# En caso valores numericos esten como logicos
data_base$Eh <- as.numeric(data_base$Eh) #Eh estaba como logico

#Identificador si existen vacios (Verificacion)
id <- which(is.na(data_base$Ca_tot))
str(data_base[id, ])

# Revisar los grupos de informacion

## Ubicacion
sum_ubi <- data_base %>% select(any_of(ubi))
summary(sum_ubi)
summary(sum_ubi$Nom_Comp) # para revisar una columna especifica

## Caracteristicas
sum_carac <- data_base %>% select(any_of(carac))
summary(sum_carac)

## Parametros fisico quimicos
sum_pfq <- data_base %>% select(any_of(pfq))
summary(sum_pfq)

# Otras variables - estadistica
data_base %>% group_by(Cuenca, Distrito) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

### mayoritarios, minoritarios y trazas:
as.data.frame(colnames(data_base))
n = 70
elementos <- colnames(data_base[ ,n:180])
sum_ele <- data_base %>% select(any_of(elementos))
summary(sum_ele)



data_base




#03 BD NORMAL SIN "<"
#04 BD NORMAL SIN "<", sin columnas de calculo de Balance iónico
#05 corrigiendo TDS
#06 BD CON "<" DE PRUEBA

