install.packages("MASS") 
install.packages("NADA")   #Nondetects and Data Analysis for Enviromental Data.
install.packages("ggmap")  #Spatial Visualization with ggplot2.
install.packages("ggplot2") #Create Elegant Data Visualization Using the Grammer of Graphics
install.packages("nortest")  #Test for normality (Anderson Darling, Cramer-vom, Lilliefors (Kolomogorov-
#Smirnov), Pearson chi-square, Shapiro-Francia)
install.packages("psych") #Procedures for Psychological, Psychometric, and Personality Research
install.packages("chron")
install.packages("readxl") #Read Excel Files

library(chron)
library(psych)
library(nortest)
library(ggplot2)
library(ggmap)
library(NADA)
library(nortest)
library(readxl)
library(MASS)

EHA_D50 <- read_xlsx("Modulos/ModuloI/ParteII/B/data_compilada/EHA_D50.xlsx", col_names = TRUE)
View(EHA_D50)


EHA_D50= EHA_D50[EHA_D50$`Tipo de fuente`=="Superficial",]
View(EHA_D50)


####Completando Datos Aluminio (Al_dis) - Modelo Basico #####

View(EHA_D50) #Poner cada vez que se corre una linea por se el codigo complejo.

#Preparando la data para analisis:
val0 <- "< 0.003"
EHA_D50$var0<- EHA_D50$Al_dis
EHA_D50$ND_var0<- rep(0, length(EHA_D50$var0))
indcero0<-which(EHA_D50$var0==val0)
EHA_D50$var0[indcero0]<-substr(val0,2,nchar(val0))
EHA_D50$var0<-as.numeric(EHA_D50$var0)
EHA_D50$ND_var0[indcero0]=1
EHA_D50$ND_var0<-as.logical(EHA_D50$ND_var0)

#Sort the Data Acorder to Study:
indna0<-is.na(EHA_D50$var0)
yn0<-EHA_D50$var0[which(indna0==FALSE)]
cyn0<-EHA_D50$ND_var0[which(indna0==FALSE)]
yn0<-sort(yn0,index.return=TRUE)
cyn0<-cyn0[yn0$ix]

#### Proceso I ---
#Apply the ROS (REGRESSION IN ORDER STATISTICS)
Al_EHA_D50 <- ros(yn0$x,cyn0,forwardT = "log", reverseT = "exp")
plot(Al_EHA_D50)
summary(Al_EHA_D50)
mean(Al_EHA_D50); sd(Al_EHA_D50);quantile(Al_EHA_D50); median(Al_EHA_D50)
Al_EHA_D50<-as.data.frame(Al_EHA_D50)
plot(Al_EHA_D50)
summary(Al_EHA_D50)
View(Al_EHA_D50)
write.csv(Al_EHA_D50,file ="Al.csv")
hist(Al_EHA_D50$modeled,
     breaks = 50)
#Resumen de Datos Modelados:
x=Al_EHA_D50$modeled
Valores=c(length(x),min(x),quantile(x,probs = 0.25),median(x),mean(x),mean(x,trim = 0.025),
          quantile(x,probs=0.75),max(x),
          IQR(x), mad(x),sd(x),skew(x),kurtosi(x),CV_CE=(sd(x)/mean(x))*100)
Nombres=c("n","Mínimo","Q1","Mediana","Media","Media Cortada","Q3","Máximo",
          "IQR","MAD", "Sd", "As","k", "CV")
f= data.frame(Valores,Nombres)

write.csv(f, file = "AHD_D50(Al).csv")

#Obteniendo el boxplot (simple):
x11()
boxplot(x, ylab='As(mg/l)', main='Boxplot de As_dis')

#### Proceso II ---
#Apply the ROS (REGRESSION IN ORDER STATISTICS)
elemento <- ros(yn0$x,cyn0,forwardT = "log", reverseT = "exp")
elemento <- as.data.frame(elemento)
id <- which(elemento$censored==TRUE)
elemento <-  elemento[id, ]

# Third Block
#DO IT WITH CARE!!!! (only the first time)
#Create new column of completed values.
Al_EHA_D50$Al_com <- Al_EHA_D50$Al_dis #omit this step after first time run

#Loop to input values:
id <- which(Al_EHA_D50$Al_com==val0)
for(i in 1:length(id)){
  replace <- elemento$modeled[i]
  Al_EHA_D50[id, ]$Al_com[i] <-  replace
}
Al_EHA_D50$Al_com <- as.numeric(Al_EHA_D50$Al_com)

# Testing if works
#After run in Avenida and Estiaje:
knitr::kable(Al_EHA_D50[ ,c("Al_dis","Al_com")])  
write.csv(x = Al_EHA_D50, file = "Al_EHA_D50.csv") 


#### Version nueva: ----
library(tidyverse)
library(NADA)
library(NADA2)

data_model <- read_xlsx(path = "Modulos/ModuloI/ParteII/B/data_compilada/Puno.xlsx")
str(data_model)
data_model <- data_model %>% drop_na(Al_dis)

data_model$Al_valdis <- as.numeric(str_remove(data_model$Al_dis, pattern = "<"))
data_model$Al_valdis_cen <- ifelse(str_detect(data_model$Al_dis, pattern = "<"), TRUE, FALSE)

data_model %>% select(Al_valdis, Al_valdis_cen, Temporada)

attach(data_model)
censummary(Al_valdis, Al_valdis_cen)
cen_ecdf(Al_valdis, Al_valdis_cen, xgroup = Temporada,
         Ylab = "Al concentration, in mg/L", xlim = c(0,max(Al_valdis)))

cenCompareCdfs(Al_valdis, Al_valdis_cen, Yname = "Al mg/l")

cenCompareQQ(Al_valdis, Al_valdis_cen)
cenQQ(Al_valdis, Al_valdis_cen, dist = "lnorm")


# Elegimos distribucion lognormal
cboxplot(Al_valdis, Al_valdis_cen, xgroup = Temporada, LOG = TRUE,  bxcol = c("blue", "skyblue"),
         minmax = FALSE, show = TRUE,
         Title = "Boxplot de As mg/l por Temporada",
         Xlab = "Temporada",
         Ylab = "As mg/l")

par(mfrow=c(2,2))

Al.ros <- cenros(Al_valdis, Al_valdis_cen)
summary(Al.ros)
median(Al.ros)
sd(Al.ros)
mean(Al.ros)
quantile(Al.ros)
sort(Al_valdis)

Al.ros$modeled




