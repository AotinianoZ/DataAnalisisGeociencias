#### Caso ----
Peligros <- read.csv(file = "Modulos/ModuloI/ParteII/B/data_compilada/peligros_cuencas2final.csv",
                    header = TRUE)
Peligros
str(Peligros)

Peligros[Peligros==""] <- NA

str(Peligros)

Peligros <- Peligros %>% mutate_if(is.character, as.factor)
str(Peligros)


Peligros <- Peligros %>%
  rename(code =CODIGO_INT, dpto = NOM_DPTO,prov =NOM_PROV,
         T_Peligro = TIPO_PELIG, code_Ab_Fm = NAME,
         N_Esp = NOMBRE_ESP, m = Reclass_Sl, h = MapaDelPer,
         Ab_Fm = UNIDAD, subcuenca = Nom_SubCu, cuenca = NOMBRE)
colnames(Peligros)

# Generar filtros

summary(Peligros)
str(Peligros)

Peligros2 <- Peligros %>% filter(!is.na(cuenca), !m=="0")
str(Peligros2)
summary(Peligros2)
colnames(Peligros2)
head(Peligros2)

# Generar un filtro que agrupe por departamento, T_Peligro
# N_Esp, conteo, min, quantiles y el max. Ordene en forma
# descendente.

Filtro <- Peligros %>%
  filter(!is.na(cuenca), !m=="0", !is.na(dpto)) %>%
  group_by(T_Peligro, N_Esp) %>%
  summarise(
    conteo = n(),
    min.h = min(h, na.rm=TRUE),
    h.25 = quantile(h, probs=0.25, na.rm=TRUE),
    h.50 = quantile(h, probs=0.50, na.rm=TRUE),
    h.75 = quantile(h, probs=0.75, na.rm=TRUE),
    max.h = max(h, na.rm=TRUE)
  ) %>%
  arrange(desc(conteo))
  
datatable(Filtro,
          extensions = c(
            "Buttons",
            "Scroller"
          ),
          filter = "top",
          options = list(
            dom = "Blrtip",
            buttons = list(I("colvis"), 'copy','csv',
                           'excel','pdf','print')
          ))

Correlation <- Peligros %>% dplyr::select(h, m)
head(Correlation)
str(Correlation)
summary(Correlation)

Correlation$m <- as.factor(Correlation$m)
summary(Correlation)

one.way <- aov(h ~ m, data=Correlation)
summary(one.way)

par(mfrow=c(2,2))
plot(one.way)

unique(Peligros2$cuenca)
unique(Peligros2$Ab_Fm)


Peligros2 %>% 
  filter(cuenca=="Cuenca Sama", Ab_Fm =="Deposito fluvial") %>%
  group_by(T_Peligro, m) %>%
  summarise(
    conteo = n()
  ) %>%
  arrange(desc(m))

summary(Peligros2$N_Esp)

View(Peligros2 %>%
  mutate(N_Esp = str_replace(N_Esp, "Avalancha de roca", "Isaac")) %>%
  group_by(N_Esp, dpto, m) %>%
  summarise(
    conteo = n()
  ) %>%
  arrange(desc(conteo)))

#### Caso 3 -----


# Conexion API de Nasa (NASAPOWER)

library(nasapower)

ag_d <- get_power(
  community = "ag",
  lonlat = c(-77.02824, -12.04318),
  pars = c("RH2M", "T2M", "PRECTOTCORR"),
  dates = c("2021-01-01", "2021-12-31"),
  temporal_api = "hourly"
)
ag_d2 <- ag_d

str(ag_d)

ag_d <- ag_d %>% 
  rename(precipitacion = PRECTOTCORR,
         temperatura = T2M,
         humedad_relativa = RH2M)
head(ag_d)

estadistica <- ag_d %>%
  group_by(YEAR, MO, DY) %>%
  summarise(conteo = n(),
            minimo = min(precipitacion, na.rm=TRUE), 
            media = mean(precipitacion, na.rm=TRUE),
            maximo = max(precipitacion, na.rm=TRUE),
            acumulado_diario = sum(precipitacion, na.rm = TRUE))

# lubridate (tiempo)
estadistica$time <- make_datetime(year = estadistica$YEAR,
                                  month = estadistica$MO,
                                  day = estadistica$DY)
str(estadistica)

estadistica %>% 
  ggplot()+
  geom_line(aes(x=time, y =acumulado_diario))+
  geom_point(aes(x=time, y=acumulado_diario))

ag_d2$time <- make_datetime(year =ag_d2$YEAR,
                            month = ag_d2$MO,
                            day = ag_d2$DY,
                            hour = ag_d$HR)

dataRange <- c(as.Date("2021-04-01"), as.Date("2021-04-20"))

ag_d2 %>%
  filter(time >= dataRange[1] & time <= dataRange[2]) %>%
  ggplot()+
  geom_line(aes(x=time, y =PRECTOTCORR))+
  geom_point(aes(x=time, y=PRECTOTCORR))

ag_d2 %>%
  filter(time >= dataRange[1] & time <= dataRange[2]) %>%
  ggplot()+
  geom_line(aes(x=time, y =T2M))+
  geom_point(aes(x=time, y=T2M))

#### Caso 3 ---

data_train <- readxl::read_xlsx(path = "Modulos/ModuloI/ParteII/B/data_compilada/Training2.xlsx",
                                col_names = TRUE)

str(data_train)
head(data_train)

data_train <- data_train %>% dplyr::select(MM, Codi_Geo_Reclas,
                                           Area_Geo_Cuen, Area_Cuen,
                                           NAME)
summary(data_train)
data_train <- data_train %>% mutate_if(is.character, as.factor)
summary(data_train)
(data_train <- na.omit(data_train))


head(data_train)
nrow(data_train)
head(data_train)

asignation <- readxl::read_xlsx(path = "Modulos/ModuloI/ParteII/B/data_compilada/tabla_de_unid_lito.xlsx",
                                col_names = TRUE)
head(asignation)
asignation <- asignation[!duplicated(asignation), ]

# revisar nombres comunes
common_names <- intersect(data_train$NAME, asignation$NAME)

asignation <- asignation %>% filter(NAME %in% common_names)
asignation

merge_df <- merge(data_train, asignation, by = "NAME", all.x=FALSE)
str(merge_df)


#### Preparo la informacion para correr un modelo

data_train2 <- merge_df

str(data_train2)
data_train2 <- data_train2 %>% mutate_if(is.character, as.factor)
summary(data_train2)
data_train2 <- na.omit(data_train2)
colnames(data_train2)
summary(data_train2$Cronoestatigrafica)

data_train2$Cronoestatigrafica <- as.factor(data_train2$Codi_Geo_Reclas,
                                            levels=c("Cámbrico","CámbricoOrdovícico"))


#### Caso data espacial ----

amazonas <- sf::st_read(dsn="Modulos/ModuloI/ParteII/B/data_compilada/shapefile/Amazonas (Boletín 39C)/")

colnames(amazonas)
str(amazonas)
head(amazonas)
tail(amazonas)
amazonas <- amazonas %>% dplyr::rename(litologia=formacion) %>% dplyr::select(litologia)
colnames(amazonas)
summary(as.factor(amazonas$litologia))
levels(as.factor(amazonas$litologia))
nlevels(as.factor(amazonas$litologia))

plot(amazonas)
mapview(amazonas, zcol="litologia")

ancash <- sf::st_read(dsn="Modulos/ModuloI/ParteII/B/data_compilada/shapefile/Ancash (Boletín 38C)/")
colnames(ancash)

ancash <- ancash %>% dplyr::rename(litologia=Subunid) %>% dplyr::select(litologia)
str(ancash)


mapview(amazonas) + mapview(ancash)

a <- mapview(amazonas, zcol="litologia")
b <- mapview(amazonas)

leafsync::sync(a, b, ncol=1)

uniones <- rbind(amazonas, ancash)
plot(uniones)
mapview(uniones)
str(uniones)
uniones <- uniones %>% dplyr::mutate_if(is.character, as.factor)
uniones <- na.omit(uniones)
summary(uniones)

sf::st_write(obj = uniones, dsn = "Modulos/ModuloI/ParteII/B/uniones.shp")

# quitar geometria

df <- uniones %>%
  st_set_geometry(NULL) %>%
  group_by(litologia) %>%
  summarise(conteo = n()) %>%
  arrange(conteo)
df
nrow(df)
etiqueta_conteo <- sum(df$conteo)

peligros<- st_read(dsn="Modulos/ModuloI/ParteII/B/data_compilada/shapefile/Peligros_shapefile/peligrosgeo.shp")
plot(peligros)
str(peligros)
colnames(peligros)

mapview(peligros)

mapview(uniones) + mapview(peligros)


peligros <- sf::st_transform(peligros, crs = st_crs(uniones))



