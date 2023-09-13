#### Caso 1 ----

data_leida <- read.csv(file="data_compilada/data_final2.csv", header = TRUE)
colnames(data_leida)
print(as_tibble(data_leida), n=nrow(data_leida))

data01 <- data_leida[ ,c("Norte","Este")]
data01 <- data01[ ,order(c(names(data01)))]
sputm <- SpatialPoints(data01, proj4string=CRS("+proj=utm +zone=19 +south +datum=WGS84")) 
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))
spgeo <- as.data.frame(spgeo)
colnames(spgeo)<-c("long","lat")
data_leida <- cbind(data_leida,spgeo)
str(data_leida)

write.csv(x = data_leida, file = "data_final2.csv")


variables <- c("pH","CE", "TDS","Ca","K","SO4","HCO3","CO3")
data <- data_leida %>% drop_na(any_of(variables))


#### Caso 2 ----

# Tratamiento de data de peligros geologicos:

# Carga de la información:
Peligros <- read.csv(file="data_compilada/peligros_cuencas2final.csv", header = TRUE)
Peligros[Peligros == ""] <- NA
str(Peligros)
colnames(Peligros)
# Variables Elegidas:
Peligros <- Peligros[ ,c("CODIGO_INT","lat","long",
                         "NOM_DPTO","NOM_PROV",
                         "TIPO_PELIG","NOMBRE_ESP","Reclass_Sl","MapaDelPer",
                         "NAME","UNIDAD", "Nom_SubCu","NOMBRE")]
# Renombrar:
Peligros <- Peligros %>%
  rename( Cod_Int = "CODIGO_INT",  Lat = "lat", Long = "long",
          Dpto = "NOM_DPTO", Prov = "NOM_PROV",
          T.Peligro = "TIPO_PELIG", N.Esp = "NOMBRE_ESP",
          m = "Reclass_Sl", h = "MapaDelPer", Ab.Fm = "UNIDAD",
          SubCuenca = "Nom_SubCu", Cuenca = "NOMBRE")

Peligros2 <- Peligros %>% 
  select(Cod_Int, Lat, Long, Dpto, Prov, T.Peligro, N.Esp, m, h, Ab.Fm, SubCuenca, Cuenca)

Peligros2$Cod_Int <- as.factor(Peligros2$Cod_Int)
Peligros2$Dpto <- as.factor(Peligros2$Dpto)
Peligros2$Prov <- as.factor(Peligros2$Prov)
Peligros2$T.Peligro <- as.factor(Peligros2$T.Peligro)
Peligros2$N.Esp <- as.factor(Peligros2$N.Esp)
Peligros2$m <- as.factor(Peligros2$m)dat
Peligros2$Ab.Fm <- as.factor(Peligros2$Ab.Fm)
Peligros2$SubCuenca <- as.factor(Peligros2$SubCuenca)
Peligros2$Cuenca <- as.factor(Peligros2$Cuenca)

Peligros2 <- Peligros2 %>% filter(!is.na(Cuenca))
Peligros2 <- Peligros2 %>% filter(!m == "0")
Peligros2 <- Peligros2 %>% select(Cod_Int, Lat, Long, Dpto, Prov, T.Peligro, N.Esp, m, h, Ab.Fm, SubCuenca, Cuenca)


# Generar filtro.

Filtro <- Peligros2 %>% 
  group_by(T.Peligro, N.Esp) %>% 
  summarise(
    n = n(),
    min.h = min(h, na.rm = FALSE),
    h.25    = quantile(h, probs = 0.25, na.rm =FALSE),
    h.50    = quantile(h, probs = 0.50, na.rm =FALSE),
    h.75    = quantile(h, probs = 0.75, na.rm =FALSE),
    max.h = max(h, na.rm = FALSE)
  ) %>%
  arrange(desc(n))
datatable(Filtro,
          extensions = c(
            "Buttons",  # add download buttons, etc
            "Scroller"  # for scrolling down the rows rather than pagination
          ),
          rownames = FALSE,  # remove rownames
          filter = "top",
          options = list(
            dom = "Blrtip",  # specify content (search box, etc)
            deferRender = TRUE,
            scrollY = 300,
            scroller = TRUE,
            buttons = list(I("colvis"),'copy', 'csv', 'excel', 'pdf', 'print')
            
          ))
Correlation <- Peligros %>%
  select(h, m)

Correlation$m <- as.factor(Correlation$m)
summary(Correlation)
levels(Correlation$m)
# d2 <- xtabs(~ m + h, data = Correlation) #Genera tabla

one.way <- aov( h ~ m, data = Correlation)
summary(one.way)

# La pendiente hallada tiene una relación con la altura calculada.

par(mfrow=c(2,2))
plot(one.way)

str(Peligros2)
colnames(Peligros2)

Peligros2 %>%
  group_by(N.Esp, Dpto, m) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(n))

unique(Peligros2$Cuenca)
unique(Peligros2$Ab.Fm)
Peligros2 %>% 
  filter(Cuenca == "Cuenca Pampas", Ab.Fm =="Deposito fluvial") %>%
  group_by(T.Peligro, m) %>%
  summarise(
    n = n()
  ) %>%
  arrange(desc(m))


Peligros2 %>%
  mutate(N.Esp = str_replace(N.Esp, "CaÃ­da de roc", "Rock Fall"))  %>%
  group_by(N.Esp, Dpto, m) %>%
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
# filter(T.Peligro == "Vuelco") %>%


#### Caso 3 ----

(nms <- names(read_excel(path="data_compilada/PG.xlsx")))
patterns <- c("031103_H_EMBALSE","050503_V_DPST_F", "F_run_up", "050603_C_AFEC_F",
              "050604_EROS_PUEN",
              "050605_EMBLS_CAUC","050606_EROS_TC")

# varibales estaban como logicas corregir para poner numericas

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
  filter(!is.na(F_FMATERIAL)) %>%
  ggplot() +
  geom_point(mapping=aes(x=F_TBLOBOL, y = F_TGRAV))

xlab(label = "Bloques y Bolones (%)")+
  ylab(label="Material del Flujo")

bd_flujos %>%
  filter(!is.na(F_FMATERIAL)) %>%
  ggplot()+
  geom_point(mapping = aes())

bd_flujos %>%
  group_by(ST_PELIGRO) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

bd_flujos %>%
  group_by(F_DEPO) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))

bd_flujos %>%
  group_by(F_MATERIAL) %>%
  summarise(conteo = n()) %>%
  arrange(desc(conteo))


















