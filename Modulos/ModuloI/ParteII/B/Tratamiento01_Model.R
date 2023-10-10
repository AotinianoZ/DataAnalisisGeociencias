# Caso 1 ----

# Formato de dato : tablas (.csv, .txt, .xlsx, .xls, .dat, .tab entre otros)

# Informacion de recursos hídricos del INGEMMET sobre análisis de aguas.

# opcion
# aguas <- read.csv(file=file.choose(), header = TRUE, sep=",")
# Esto es en caso se desee escoger un archivo.
aguas <- read.csv(file="data_compilada/data_final2.csv", header = TRUE, sep=",")

str(aguas)
summary(aguas)

#aguas$Codigo <- as.factor(aguas$Codigo) #ejemplo en una variable (no para automatizacion)

aguas <- aguas %>% mutate_if(is.character, as.factor)
str(aguas)
summary(aguas)
head(aguas, n=5)
tail(aguas, n=5)
colnames(aguas)
rownames(aguas)

rownames(aguas) <- paste("leyva", 1:353, sep = "_")
rownames(aguas)

# filtrar informacion

# valores de NA de calcio:

aguas %>% filter(is.na(Ca)) %>% select(Codigo, pH)


aguas %>% filter(!is.na(Ca), pH > 7.00) %>% select(Codigo, pH, Temporada)

# Filtrar pH mayor a la media aritmetica de pH,
# que no tenga vacios en Ca ni en SO4, y lo agrupe por
# temporada y tipo de fuente. Después, que sintice
# la cantidad de datos con las estadísticas de
# conductividad elétrica CE (minimo, media aritmetica, mediana y maximo)
# ponerlo ordenado por conteo.

aguas %>% 
  filter(pH > mean(pH), !is.na(Ca), !is.na(SO4)) %>%
  group_by(Temporada, Tipo_Fuente) %>%
  summarise(
    conteo = n(),
    Ce_min = min(CE),
    Ce_media = mean(CE),
    Ce_mediana = median(CE),
    Ce_max = max(CE)) %>%
  arrange(desc(conteo))

# pretratamiento de los NA o data incompleta
# (imputación, aproximación ML, semiparmetricos)

filtro1 <- aguas %>% 
  filter(pH > mean(pH, na.rm=TRUE), !is.na(Ca), !is.na(SO4)) %>%
  group_by(Temporada, Tipo_Fuente) %>%
  summarise(
    conteo = n(),
    Ce_min = min(CE, na.rm=TRUE),
    Ce_media = mean(CE, na.rm=TRUE),
    Ce_mediana = median(CE, na.rm=TRUE),
    Ce_max = max(CE, na.rm=TRUE)) %>%
  arrange(desc(conteo))


print(as_tibble(aguas %>% 
                  select(Codigo, Tipo_Fuente, pH) %>%
                  filter(pH<6) %>%
                  arrange(desc(!pH))))

write.csv(filtro1, file = "Primerfiltro.csv")

head(aguas)
colnames(aguas)

# plotear con sistema de referencia coordenado (long, lat) - geograficas


# transformar la informacion en data espacial - forma antigua

# El CRS(Coordinate Reference System) es "WGS Zone 19S"

data_coor <- aguas[ ,c("Norte","Este")]

# Otras formas
# aguas[ ,2:3]
# aguas %>% dplyr::select(Norte, Este)
data_coor <- data_coor[ ,order(c(names(data_coor)))]
sputm <- SpatialPoints(coords = data_coor, 
                       proj4string = CRS("+proj=utm +zone=19 +south +datum=WGS84"))
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo <- as.data.frame(spgeo)
colnames(spgeo) <- c("longitud","latitud")
data_leida_v2 <- cbind(aguas, spgeo)
str(data_leida_v2)
plot(data_leida_v2)

# transformacion  espacial sintetizada
aguas_espacial <- aguas %>% 
  sf::st_as_sf(coords = c("long","lat"), crs = 4326)

# Para Perú el CRS:4326 es el WGS84.

aguas_espacial
mapview::mapview(aguas_espacial)

filtrado_espacial <- aguas_espacial %>% filter(is.na(Ca)) %>% select(Codigo, pH)
mapview::mapview(filtrado_espacial)

variables <- c("Na","Tipo_Fuente")


agua2 <- aguas %>% select(any_of(variables), starts_with("C"))
head(agua2)

agua3 <- aguas %>% drop_na(any_of(variables)) # eliminar vacios
head(agua2)

write.csv(x = agua2, file="lohicimos.csv")


# Caso 2 ----

# Información de peligros geológicos en el Peru:

Peligros <- read.csv("Modulos/ModuloI/ParteII/B/data_compilada/peligros_cuencas2final.csv",header=TRUE)

View(Peligros)
Peligros <- Peligros %>% mutate_if(is.character, as.factor)
str(Peligros)
colnames(Peligros)
rownames(Peligros)
head(Peligros, 5)
tail(Peligros, 5)

summary(Peligros)

Peligros[Peligros==""] <- NA # estaba la dato (celda) vacia
str(Peligros)
summary(Peligros)

Peligros2 <- Peligros %>% drop_na(any_of(c("TIPO_PELIG","NOMBRE")))
summary(Peligros2)

colnames(Peligros2)

Peligros2 <- Peligros2 %>% 
  dplyr::rename(code = "CODIGO_INT", Dpto="NOM_DPTO",
                Prov = "NOM_PROV", T_Pel = "TIPO_PELIG",
                N_esp = "NOMBRE_ESP", m = "Reclass_Sl",
                h = "MapaDelPer", Ab_Fm = "UNIDAD",
                subcuenca = "Nom_SubCu", cuenca = "NOMBRE" )
colnames(Peligros2)

# Asegurar que la variable cuenca no esta vacia y que la pendiente
# no es 0.

Peligros2 <- Peligros2 %>% filter(!is.na(cuenca), !m=="0")
summary(Peligros2)

# Generar filtros:

# a) Agrupar por T_Pel y N_Esp en ese orden,
# y sintetizra el conteo, min, quantiles y maximo de 
# la informacion. Adicionalmente, ordenar acorde al conteo.

Filtro1 <- Peligros2 %>%
  group_by(T_Pel, N_esp) %>%
  summarise(
    conteo = n(),
    min.h = min(h, na.rm = FALSE),
    h.25 = quantile(h, probs = 0.25, na.rm=FALSE),
    h.50 = quantile(h, probs = 0.50, na.rm=FALSE), #mediana
    h.75 = quantile(h, probs = 0.75, na.rm=FALSE),
    max.h = max(h, na.rm = FALSE)
  ) %>%
  arrange(desc(conteo))
Filtro1

datatable(Filtro1,
          extensions = c(
            "Buttons",
            "Scroller"
          ), 
          rownames = FALSE,
          filter = "top",
          options = list(
            dom = "Blrtip",
            deferRender = TRUE,
            buttons = list(I("colvis"),'copy', 'csv', 'excel', 'pdf', 'print')
          ))

# Buscando correlacion entre variables altura y pendiente:

Correlation <- Peligros2 %>% dplyr::select(h, m)
str(Correlation)
summary(Correlation)
head(Correlation)
levels(Correlation$m)
# d2 <- xtabs(~ m + h, data = Correlation) #Genera tabla

Correlation$m <- as.factor(Correlation$m) # debido a que fue clasificado el valor de pendiente
summary(Correlation)

one.way <- aov(h~m, data=Correlation)
summary(one.way)

# La pendiente hallada tiene una relación con la altura calculada.
par(mfrow=c(2,2))
plot(one.way)

str(Peligros2)
colnames(Peligros2)

Peligros2 %>%
  group_by(N_esp, Dpto, m) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

unique(Peligros2$cuenca)
unique(Peligros2$Ab_Fm)
Peligros2 %>% 
  filter(cuenca == "Cuenca Pampas", Ab_Fm =="Deposito fluvial") %>%
  group_by(T_Pel, m) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(m))

Peligros2 %>%
  mutate(N_esp = str_replace(N_esp, "Caída de roc", "Rock Fall"))  %>%
  group_by(N_esp, Dpto, m) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

Peligros2 %>%
  group_by(Dpto) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))


#### Tarea (Replicar el siguiente proceso cambiando las variables y condiciones - explorar la data) ----

(nms <- names(read_excel(path="data_compilada/PG.xlsx")))
patterns <- c("031103_H_EMBALSE","050503_V_DPST_F", "F_run_up", "050603_C_AFEC_F",
              "050604_EROS_PUEN",
              "050605_EMBLS_CAUC","050606_EROS_TC")

# variables estaban como logicas corregir para poner numericas

(ct <- ifelse(grepl(paste(patterns, collapse="|"), nms), "numeric", "guess"))

base_datos <- read_xlsx(path="data_compilada/PG.xlsx", col_names = TRUE, col_types = ct)

base_datos <- base_datos %>% mutate_if(is.character, as.factor)

### Sumario 

summary(base_datos)
# (ct <- ifelse(grepl("^F_", nms), "text", "guess")) #cambiar por patron inicio.
#otra alternativa pero hay que cambiar despues mucho.
# base_datos <- read.csv(file = "PG.csv", header = TRUE)
str(base_datos, list.len = ncol(base_datos))
ncol(base_datos)

vector_inicios <- as.character(c(00,01,03,14:28))
bd_an <- base_datos %>% select(starts_with(vector_inicios))
colnames(bd_an)

bd_an1 <- bd_an %>% select(starts_with(c("01","25")))
str(bd_an1)

base_datos %>% select(contains("0"))
ncol(base_datos %>% select(contains("0")))

View(base_datos %>%
       group_by(`010100_T_PELIGRO`,`260100_GRADO_PE`) %>%
       summarise(conteo = n()) %>%
       arrange(desc(conteo)))

base_datos %>%
  group_by(`010100_T_PELIGRO`, `010200_ST_PELIGRO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

#### Analizando Deslizamientos 

bd_deslizamiento <- base_datos %>% select(starts_with('00'), starts_with('01'),
                                          starts_with('03'),starts_with('17'))
bd_deslizamiento <- bd_deslizamiento %>% filter(`010100_T_PELIGRO` =="Deslizamiento")
bd_deslizamiento <- bd_deslizamiento %>% mutate_if(is.character, as.factor)
bd_deslizamiento$ZONA <- as.factor(bd_deslizamiento$`000500_ZONA`)
str(bd_deslizamiento)
summary(bd_deslizamiento)

bd_deslizamiento %>%
  filter(!is.na(`031001_AGRTM`)) %>%
  ggplot() +
  geom_boxplot(mapping=aes(x=`031101_D_RECORR`, y = `031001_AGRTM`))+
  xlab(label = "Distancia Recorrida del Deposito de Flujo (m)")+
  ylab(label="Agritamientos")

bd_deslizamiento %>%
  group_by(`010200_ST_PELIGRO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

bd_deslizamiento %>%
  group_by(`030100_ESTILO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

bd_deslizamiento %>%
  group_by(`030200_F_ESCRP`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))


#### Analizando Flujos - Corregir errores!! (vamos)

bd_flujos <- base_datos %>% select(1:21, starts_with('21'))
bd_flujos <- bd_flujos %>% filter(`010100_T_PELIGRO` =="Flujo")
bd_flujos <- bd_flujos %>% mutate_if(is.character, as.factor)

summary(bd_flujos)

bd_flujos %>%
  filter(`001400_FECHA` >= as.Date("2020-01-01"))

bd_flujos %>%
  filter(!is.na(`210300_PR_GRAVAS`)) %>%
  ggplot() +
  geom_point(mapping=aes(x=`210200_PR_BOLONES`, y = `210300_PR_GRAVAS`))+
  xlab(label = "Bloques y Gravas (%)")+
  ylab(label="Material del Flujo")

bd_flujos %>%
  group_by(`010200_ST_PELIGRO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

bd_flujos %>%
  group_by(`020104_FAC_SITIO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

bd_flujos %>%
  group_by(`020103_FAC_SITIO`) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))














