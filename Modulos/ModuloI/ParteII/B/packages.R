loadlibs <- function(x =0){
  # carga de data
  require(readxl)
  require(sf) # carga y tratamiento data espacial
  require(sp) # carga y tratamiento data espacial
  
  # tratamiento data y visualizacion
  require(tidyverse) # un conglomerado de paquetes!!!
  require(lubridate) # data temporal
  require(plotly) # graficos interactivos
  
  # formato tabla
  require(DT)
  
  #visualizacion y tratamiento espacial
  require(rgdal)
  require(leaflet)
  require(mapview)
  
  # tratamiento de rasters
  require(raster)
  
  # adicionales
  require(htmltools)
  require(hrbrthemes)

}
loadlibs()
cat(" R libraries loaded for Tratamiento","\n", " written by A. Otiniano.",'\n')

