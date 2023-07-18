#Primero instalar paquetes para poder trabajar con la data y luego los cargaremos:

install.packages("bixmit") #Estimated mixture models for Case-Control data 
#(non-Gaussian mix models)
install.packages("MASS")
install.packages("NADA") #Nondetects and Data Analysis for Enviromental Data.
install.packages("ggmap") #Spatial Visualization with ggplot2.
install.packages("ggplot2") #Create Elegant Data Visualization Using the Grammer 
#Graphics
install.packages("nortest") #Test for normality (Anderson Darling, Cramer-von,
# Lilliefor (Kolomogorov-Smirnov), Perarson chi-square, Shapiro-Francia)
#M?s se usa el Shapiro-Wilk test!!
install.packages("psych") #Procedures for Psychological, Psychometric, and 
#Personality Research
install.packages("chron")
install.packages("readxl") #Read Excel files


# Load libraries for the FIGMM online course 
loadlibs <- function(x =0){
  # data base packages
  require(chron)
  require(psych)
  require(nortest)
  require(ggplot2)
  require(ggmap)
  require(NADA)
  require(MASS)
  require(readxl)
  require(lattice)
  require(caret)
  require(mlbench)
  require(AppliedPredictiveModeling)
  require(e1071)
  require(Rcpp)
  require(Amelia)
  require(RCurl)
  require(tibble)

}

loadlibs()
cat(" R libraries loaded for Data Analysis in FIGMM","\n", " written by A. Otiniano",'\n')





