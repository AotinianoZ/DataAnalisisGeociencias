#### Caso 7 ----

#  Tratamiento de data para un modelo neuronal (pasos inciales)

data_train <-  readxl::read_xlsx("data_compilada/Training2.xlsx", col_names = T)
data_train[c('NAME2', 'NAME3')] <- str_split_fixed(data_train$NAME, '-', 2)
data_train <- data_train %>% dplyr::select(MM, Codi_Geo_Reclas, Area_Geo_Cuen, Area_Cuen, NAME2)
data_train <- (na.omit(data_train))
head(data_train )
nrow(data_train)

asignation <- readxl::read_xlsx(path = "data_compilada/tabla_de_unid_lito.xlsx", col_names = TRUE)
asignation <- asignation %>% dplyr::rename(NAME2 =Division)
asignation <- asignation %>% select(NAME2, Cronoestatigrafica) # despues hay problemas
asignation <- asignation[!duplicated(asignation), ]
asignation

# revisar nombres comunes
common_names <- intersect(data_train$NAME2, asignation$NAME2)
asignation <- asignation %>% filter(NAME2 %in% common_names) #quitar nombres de tabla asignation que no estan en data_train
asignation

# Merge the dataframes based on the common column "NAME2"
merged_df <- merge(data_train, asignation, by = "NAME2", all.x = FALSE) # no tener valores repetidos ni faltante sen la segunda tabla.
# Replace NA values in data_train with the values from asignation's "Cronoestatigrafica" column
merged_df$Cronoestatigrafica <- ifelse(is.na(merged_df$Cronoestatigrafica), merged_df$Cronoestatigrafica.y, merged_df$Cronoestatigrafica)
str(merged_df)

##### Preparar informacion para correr el algoritmo
data_train <- merged_df

str(data_train)
data_train <- data_train %>% mutate_if(is.character, as.factor)
summary(data_train)
data_train <-(na.omit(data_train))
colnames(data_train)
summary(data_train$Cronoestatigrafica)

#### Caso 8 ----

# Tratamiento básico de informacion espacial shapefile

amazonas <- sf::st_read(dsn="data_compilada/shapefile/Amazonas (Boletín 39C)/") # formacion
str(amazonas)
head(amazonas)
tail(amazonas)
amazonas <- amazonas %>% dplyr::rename(litologia=formacion) %>% dplyr::select(litologia)
str(amazonas)
head(amazonas)
tail(amazonas)
summary(as.factor(amazonas$litologia))
levels(as.factor(amazonas$litologia))
nlevels(as.factor(amazonas$litologia))

plot(amazonas)
mapview::mapview(amazonas, zcol="litologia")

ancash <- sf::st_read(dsn="data_compilada/shapefile/Ancash (Boletín 38C)/lito.shp") # Subunid
ancash <- ancash %>% dplyr::rename(litologia=Subunid) %>% dplyr::select(litologia)

mapview::mapview(amazonas) + mapview::mapview(ancash)

uniones <- rbind(amazonas, ancash)
uniones
str(uniones)
uniones <- uniones %>% dplyr::mutate_if(is.character, as.factor)
uniones <- na.omit(uniones)
summary(uniones)
plot(uniones)

st_write(uniones, dsn="union.shp")


df <- uniones %>% group_by(litologia) %>% summarise(conteo = n()) %>% arrange(desc(conteo))
# quitar geometria
df <- uniones %>% 
  st_set_geometry(NULL) %>% 
  group_by(litologia) %>% 
  summarise(conteo = n()) %>% 
  arrange(desc(conteo))
nrow(df)
etiqueta_conteo <- sum(df$conteo)

peligros <- st_read(dsn="data_compilada/shapefile/Peligros_shapefile/peligrosgeo.shp")
str(peligros)
peligros
plot(peligros)
mapview::mapview(peligros)

peligros <- st_transform(peligros, crs = st_crs(uniones))

mapview::mapview(uniones) + mapview::mapview(peligros)

# No correr demora mucho es demasiada información para procesar en maquina chica!
interseccion <- st_intersection(peligros, uniones)


#### Caso 9 ---- 

# Tratamiento básico de informacion espacial raster

# Cargar raster data
ELEVATION = raster("RasterBase/DEM.tif")  
SLOPE= raster("RasterBase/SLOPE.tif") 
ASPECT = raster("RasterBase/ASPECT.tif")

ELEVATION
str(ELEVATION)
hist(ELEVATION[])

# check attributes and projection and extent
extent(ELEVATION)
extent(SLOPE)
extent(ASPECT)

# if you have diffrent extent, then try to Resample them using the smallest area
ELEVATION_r <- resample(ELEVATION, SLOPE, resample='bilinear')
ASPECT_r <- resample(ASPECT, SLOPE, resample="bilinear")

extent(ELEVATION_r) # check the new extent
extent(ASPECT_r)
extent(SLOPE)

# escribir nuevo geotiff
writeRaster(ELEVATION_r,filename="RasterBase/Resample/ELEVATION.tif", format="GTiff", overwrite=TRUE)
writeRaster(ASPECT_r,filename="RasterBase/Resample/ASPECT.tif", format="GTiff", overwrite=TRUE) 
writeRaster(SLOPE,filename="RasterBase/Resample/SLOPE.tif", format="GTiff", overwrite=TRUE)

## stack multiples documentos raster
Stack_List = list.files(path = "RasterBase/Resample//",pattern = "tif$", full.names = TRUE)
Rasters = stack(Stack_List)
names(Rasters)
plot(Rasters)

##### Convertir el raster a dataframe con  Long-Lat 

Rasters.df = as.data.frame(Rasters, xy = TRUE, na.rm = TRUE)
head(Rasters.df,1)

# revisar variables
colnames(Rasters.df)[3] <- "ASPECT"   # change columns names 
colnames(Rasters.df)[4] <- "ELEVATION"   # change columns names 
colnames(Rasters.df)[5] <- "SLOPE"   # change columns names 


Rasters.df_N <- Rasters.df[ ,c(-1,-2)] # remover x, y

##### Reclasificando data categorica en raster

# ASPECT
ASPECTras<-cut(Rasters.df_N$ASPECT, seq(0,361,45), right=FALSE, labels=c("a","b","c","d","e","f","g","h"))
table(ASPECTras)

ASPECTras <- factor(ASPECTras)
flagsras = data.frame(Reduce(cbind, 
                             lapply(levels(ASPECTras), function(x){(ASPECTras == x)*1})
))
names(flagsras) = levels(ASPECTras)
Rasters.df_N = cbind(Rasters.df_N, flagsras) # combine the ASPECTS with original data

# Remove the original aspect data
colnames(Rasters.df_N)
Rasters.df_N <- Rasters.df_N[,-1]
str(Rasters.df_N)


#####  Escalar Variables 

# Check the relationship between the numeric varaibles, Scale the numeric var first!
maxss <- apply(Rasters.df_N[,1:2], 2, max) 
minss <- apply(Rasters.df_N[,1:2], 2, min)
Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df_N[,1:2], center = minss, scale = maxss - minss)) 

# Regresamos el (x,y) y los valores de aspecto y elevacion-pendiente
Rasters.df_N_scaled <- data.frame(cbind(Rasters.df[,c(1,2)], Rasters.df_N_scaled, Rasters.df_N[,c(3:ncol(Rasters.df_N))]))


# Crearemos un random de valores generados a partir de un distribucion normal

compute.SM <- rnorm(nrow(Rasters.df_N_scaled), mean=0, sd=1) # to let the variables matched 

out_SM <- data.frame(cbind(Rasters.df_N_scaled, compute.SM))
str(out_SM)
colnames(out_SM)[13] <- "ANN_P" # Add header to ANN results

# Almost done, try to remove other variables and keep x,y and ANN to be ploted
out_SM <- out_SM[ ,c(1,2,13)]

# Convertimos el dataframe a raster con Long-Lat 

SM_from_df <- rasterFromXYZ(out_SM)  #Convert first two columns as lon-lat and third as value                
plot(SM_from_df)
SM_from_df
# coord. ref. : NA 

# Add coord. ref. system by using the original data info (Copy n Paste).
projection(SLOPE) # borrow the projection from Raster data

proj4string(SM_from_df)=CRS("+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs") # set it to lat-long

# Exportar el mapa final en TIFF

# write to a new geotiff file
writeRaster(SM_from_df,filename="RasterBase/Prediction//Prediction.tif", format="GTiff", overwrite=TRUE) 

mapview::mapview(SM_from_df)










