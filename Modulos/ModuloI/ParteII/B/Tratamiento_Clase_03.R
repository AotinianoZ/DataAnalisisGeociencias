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
























