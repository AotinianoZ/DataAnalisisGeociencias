#### Caso 6 ----

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

#### Caso 7 ----

library(terra)
library(sf)
library(tidyverse)

Elevation <- terra::rast(x = "Modulos/ModuloI/ParteII/B/RasterBase/DEM.tif")
Elevation
str(Elevation)
summary(Elevation)
plot(Elevation)

# DEM - wailung (fill sinks) (omitir)

Slope <- terra::terrain(x = Elevation, v="slope", neighbors=8, unit="degrees")
Slope
str(Slope)
summary(Slope)
plot(Slope)

par(mfrow=c(2,1))
plot(Elevation)
plot(Slope)

#stack
tratamiento <- c(Elevation, Slope)
tratamiento

names(tratamiento) <- c("elevacion","pendiente") 
tratamiento
plot(tratamiento)
cor(values(tratamiento)[ ,2], values(tratamiento)[ ,1], use = "na.or.complete")


lm1 <- lm(values(tratamiento)[ ,2] ~ values(tratamiento)[ ,1])
lm1
summary(lm1)

# crear directorios

dir.create(path="Modulos/ModuloI/ParteII/B/Correlacion")

x <- terra::rast(tratamiento, 1) # elevacion
values(x) <- 1:ncell(tratamiento) # ajuste de celda (precaucion)

matriz_calculo <- values(tratamiento)


focal_cor <- terra::focal(
  x = x, 
  w = matrix(1,3,3),
  
  fun = function(x , y=matriz_calculo){
    cor(y[x, 1], y[x, 2], method = "spearman",
        use = "na.or.complete")
  },
  
  filename = file.path("Modulos/ModuloI/ParteII/B/Correlacion/focal_spearman.tif"),
  overwrite = TRUE
)

cor_map_pearson <-  raster::raster(x = "Modulos/ModuloI/ParteII/B/Correlacion/focal_pearson.tif")
cor_map_spearman <- raster::raster(x = "Modulos/ModuloI/ParteII/B/Correlacion/focal_spearman.tif")
a <- mapview::mapview(cor_map_pearson, maxpixels =  4155072) 
b <- mapview::mapview(cor_map_spearman, maxpixels =  4155072)

leafsync::sync(a, b)

# Continuacion

# Cargar raster data:
Elevation <- terra::rast(x = "Modulos/ModuloI/ParteII/B/RasterBase/DEM.tif")
Slope <- terra::terrain(x = Elevation, v="slope", neighbors=8, unit="degrees")
Aspect <- terra::terrain(x = Elevation, v="aspect", neighbors=8, unit="degrees")
Rugosidad <- terra::terrain(x = Elevation, v="roughness", neighbors=8) 
Flowdirection <- terra::terrain(x = Elevation, v="flowdir") 

par(mfrow=c(3,2))
plot(Elevation)
plot(Slope)
plot(Aspect)
plot(Rugosidad)
plot(Flowdirection)

Elevation
str(Elevation)
hist(Elevation[])

# revisar extension
terra::ext(Elevation)
terra::ext(Rugosidad)

# resample (cuidado, eligirlo con la menor area)

Elevation_r <- terra::resample(Elevation, Slope, method='bilinear')
Aspect_r <- terra::resample(Aspect, Slope, method='bilinear')
Rugosidad_r <- terra::resample(Rugosidad, Slope, method='bilinear')
Slope_r <- Slope 

terra::ext(Elevation_r)
terra::ext(Slope_r)

terra::writeRaster(x = Elevation_r, filename = "Modulos/ModuloI/ParteII/B/RasterBase/Resample/Elevacion.tif", overwrite=TRUE)
terra::writeRaster(x = Aspect_r, filename = "Modulos/ModuloI/ParteII/B/RasterBase/Resample/Aspect.tif", overwrite=TRUE)
terra::writeRaster(x = Rugosidad_r, filename = "Modulos/ModuloI/ParteII/B/RasterBase/Resample/Rugosidad.tif", overwrite=TRUE)
terra::writeRaster(x = Slope_r, filename = "Modulos/ModuloI/ParteII/B/RasterBase/Resample/Slope.tif", overwrite=TRUE)

# cargamos la informacion

Stack_list <- list.files(path="Modulos/ModuloI/ParteII/B/RasterBase/Resample/", pattern = "tif$", full.names = TRUE)
Rasters <- stack(Stack_list)
Rasters

plot(Rasters)


# Convertir el raster a dataframe con long-lat ("coordenadas geograficas")

Rasters.df <- as.data.frame(Rasters, xy=TRUE, na.rm=TRUE)
head(Rasters.df, 5)


Rasters.df_N <- Rasters.df[ ,c(-1,-2)] # remover x,y

Aspectreclas <- cut(Rasters.df_N$aspect, seq(0,361,45), right=FALSE,
                    labels =c("45","90","135","180","225","270","315","360"))
table(Aspectreclas)

Rasters.df_N = cbind(Rasters.df_N, flagsras)
Rasters.df_N %>% filter(is.na(Aspectreclas))
Rasters.df_N <- drop_na(Rasters.df_N) # Comodidad

# Escalar variables

maxss <- apply(Rasters.df_N[ ,1:4], 2, max)
minss <- apply(Rasters.df_N[ ,1:4],2, min)

Rasters.df_N_scaled <- as.data.frame(scale(Rasters.df_N[ ,1:4], 
                                           center = minss, 
                                           scale = maxss - minss))
head(Rasters.df_N)
head(Rasters.df_N_scaled)
summary(Rasters.df_N_scaled)

Rasters.df_final <- data.frame(cbind(Rasters.df[ ,c(1,2)], # x e y
                                     Rasters.df_N_scaled)) #,  # variables escaladas
#Rasters.df_N[ ,"Aspectreclas"])) # reclasificacion
head(Rasters.df_final)

# Modelamiento predicto
compute.Cu <- rnorm(n = nrow(Rasters.df_final), mean=2.45, sd=1)
str(compute.Cu)

out_Cu <- data.frame(cbind(Rasters.df_final, compute.Cu))
str(out_Cu)
colnames(out_Cu)[7] <- "Cu_modelado"

# transformar a raster
out_sm_raster <- raster::rasterFromXYZ(out_Cu)
plot(out_sm_raster)

terra::crs(Slope, describe=TRUE, proj=TRUE)
terra::crs(out_sm_raster) <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs"
out_sm_raster

# ANN FAULT : LINEAMIENTOS

#### Tarea (Tratamiento de informacion espacial raster) ---- 

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













