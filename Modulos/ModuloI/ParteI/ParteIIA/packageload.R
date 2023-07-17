# Load libraries for the FIGMM online course 
loadlibs <- function(x =0){
  # data base packages
  require(DBI)
  require(odbc)
  require(RMySQL)
  # data wrangle, tidy and transformation, also visualization
  require(tidyverse)
  # interactive visualization
  require(plotly)
  # spatial analysis
  require (sf)
  require(sp)
  require(mapview)
  # deployment
  require(shiny)
  # multivariable analysis
  require(FactoMineR)
  require(factoextra)
}

loadlibs()
cat(" R libraries loaded for Data Analysis in FIGMM","\n", " written by A. Otiniano & J.Andrade.",'\n')



