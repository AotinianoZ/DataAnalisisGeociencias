#### Caso 1 ----

# Informacion de recurso hidrico del INGEMMET sobre analisis de aguas.

aguas <- read.csv(file="Modulos/ModuloI/ParteII/B/data_compilada/data_final2.csv",
                     header = TRUE)
colnames(aguas)
str(aguas)


aguas <- aguas %>% mutate_if(is.character, as.factor)
str(aguas)
rownames(aguas)
head(aguas, 5)
tail(aguas, 5)

summary(aguas)

# filtrar la informacion 

# valores de Na con calcio
aguas %>% filter(is.na(Ca)) %>% dplyr::select(Codigo, pH)

# informacion sin valores de Na en Calcio
aguas %>% filter(!is.na(Ca)) %>% dplyr::select(Codigo, pH)

# Filtrar pH mayo a la media aritmetica de pH,
# que no tenga vacios en SO4, y lo agrupe por 
# temporada y tipo de fuente. Depues que me
# sintetice la cantidad de datos con las estadisticas
# de conductividad electrica (minimo, media aritmetica,
# mediana y maximo), no olvidar ordenarlo por conteo.

# pre tratamiento de los NA o data incompleto
# (inputacion, aproximacion ML, semiparametrico)

aguas %>% filter(pH > mean(pH, na.rm=TRUE), !is.na(SO4)) %>%
  group_by(Temporada, Tipo_Fuente) %>% 
  summarise(conteo = n(),
            Na_min = min(Na, na.rm = TRUE),
            Na_media_arit = mean(Na, na.rm = TRUE),
            Na_mediana = median(Na, na.rm = TRUE),
            Na_max = max(Na, na.rm = TRUE)) %>%
  arrange(desc(conteo))


# El CRS (Coordinate Reference System) en "UTM-WGS Zone 19S"

data_coor <- aguas[ ,c("Norte","Este")]
head(data_coor)

# Otras formas
head(aguas[ ,2:3])
head(aguas %>% dplyr::select(Norte, Este))

data_coor <- data_coor[ ,order(c(names(data_coor)))]
head(data_coor)
sputm <- SpatialPoints(coords = data_coor,
                       proj4string = CRS("+proj=utm +zone=19 +south +datum=WGS84"))
plot(sputm)
mapview(sputm)

spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo <- as.data.frame(spgeo)
colnames(spgeo) <- c("longitud","latitud")

aguas2 <- cbind(aguas, spgeo)
colnames(aguas2)
head(aguas2)

write.csv(x = aguas2, file="lohicimos.csv")

colnames(aguas2)

variables <- c("Codigo","pH","CE","Ca")

aguas3 <- aguas2 %>% drop_na(any_of(variables)) # de las seleccionadas
summary(aguas3)

#### Caso 2 ----

Peligros <- read.csv("Modulos/ModuloI/ParteII/B/data_compilada/peligros_cuencas2final.csv",
                     header=TRUE)

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

Correlation <- Peligros2 %>% dplyr::select(h, m)
str(Correlation)
summary(Correlation)
head(Correlation)

Correlation$m <- as.factor(Correlation$m) # debido a que fue clasificado el valor de pendiente
summary(Correlation)

one.way <- aov(h~m, data=Correlation)
summary(one.way)

par(mfrow=c(2,2))
plot(one.way)





