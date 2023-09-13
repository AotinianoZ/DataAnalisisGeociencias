#### Caso 1 ----

data_leida <- read.csv(file="data_compilada/data_final2.csv", header=TRUE)
colnames(data_leida)
str(data_leida)

data_leida <- data_leida %>% mutate_if(is.character, as.factor)
str(data_leida)
head(data_leida)
tail(data_leida)
colnames(data_leida)
rownames(data_leida)

summary(data_leida)

data_leida %>% filter(pH>mean(pH)) %>%
  group_by(Temporada, Tipo_Fuente) %>% summarise(conteo = n(),
                                    Ce_min = min(CE),
                                    Ce_ma = mean(CE),
                                    Ce_med = median(CE),
                                    Ce_max = max(CE)) %>%
  arrange(desc(conteo))

print(as_tibble(data_leida %>% dplyr::select(Codigo, Tipo_Fuente, pH) 
                 %>% filter(pH<6) %>% arrange(desc(!pH))))


print(as_tibble(data_leida, n=nrow(data_leida)))


# El CRS(Coordinate Reference System) es "WGS Zone 19S"

data_coor <- data_leida[ ,c("Norte","Este")]

# Otras formas
# data_leida[ ,2:3]
# data_leida %>% dplyr::select(Norte, Este)

data_coor <- data_coor[ ,order(c(names(data_coor)))]
sputm <- SpatialPoints(coords = data_coor, 
                       proj4string = CRS("+proj=utm +zone=19 +south +datum=WGS84"))
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo <- as.data.frame(spgeo)
colnames(spgeo) <- c("longitud","latitud")
data_leida_v2 <- cbind(data_leida, spgeo)
str(data_leida_v2)

plot(data_leida_v2)

write.csv(x = data_leida_v2, file="lohicimos.csv")

colnames(data_leida_v2)
variables <- c("pH","CE","TDS","Ca")

data_caso1_final <- data_leida %>% drop_na(any_of(variables))
summary(data_caso1_final)


#### Caso 2 ----

# Tratamientos de data de peligros geologicos:

# Carga de la informacion

Peligros <- read.csv(file="data_compilada/peligros_cuencas2final.csv", header = TRUE)

View(Peligros)
str(Peligros)

Peligros <- Peligros %>% mutate_if(is.character, as.factor)
str(Peligros)
colnames(Peligros)
rownames(Peligros)
head(Peligros, 5)
tail(Peligros, 6)

summary(Peligros)

Peligros[Peligros ==""] <- NA
str(Peligros)

colnames(Peligros)

Peligros <- Peligros %>% 
  rename(Cod_int = "CODIGO_INT", Dpto="NOM_DPTO",
         Prov="NOM_PROV", T_Pel = "TIPO_PELIG",
         N_Esp="NOMBRE_ESP", m = "Reclass_Sl",
         h = "MapaDelPer", Ab_Fm = "UNIDAD",
         subcuenca = "Nom_SubCu", cuenca="NOMBRE")

Peligros2 <- Peligros %>%
  dplyr::select(Cod_int, lat, long, Dpto, Prov, T_Pel,
         N_Esp, m, h, Ab_Fm, subcuenca, cuenca)

str(Peligros2)

summary(Peligros2)

Peligros2 <- Peligros2 %>% filter(!is.na(cuenca), !m=="0") 
summary(Peligros2)

# Generar filtros

Filtro1 <- Peligros2 %>% 
  group_by(T_Pel, N_Esp) %>%
  summarise(
    conteo = n(),
    min.h = min(h, na.rm=FALSE),
    h.25  = quantile(h, probs=0.25, na.rm=FALSE),
    h.50  = quantile(h, probs=0.50, na.rm=FALSE),
    h.75  = quantile(h, probs=0.75, na.rm=FALSE),
    max.h = max(h, na.rm=FALSE)
  ) %>% 
  arrange(desc(conteo))















