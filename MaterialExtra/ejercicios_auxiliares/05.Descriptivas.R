#Paquetes Extras:
# plotly,tibble,ggridges,dplyr,ggrepel,gridExtra

#Estructura de Analisis de Informacion
#Cargar la informacion y damos una revision general de las variables, considerar
#de manera importante los valores ausentes o valores mal escritos. Despues vamos a 
#realizar un an?lsis descriptivo visual (summary(nombrededata)),luego vamos a ver
#una variable para analizar de la cual realizaremos un resumen estad?stico, boxplot
#un histograma, curva de densidad real y ajustada y finalmente un qqplot y el an?lisis
#de normalidad

BD_01<-read.table(file="data/Boston.txt",header = TRUE)
View(BD_01)
str(BD_01)
colnames(BD_01)
dim(BD_01)
head(BD_01)
tail(BD_01)
sum(is.na(BD_01$crim)) #Variable crim no tiene nulos
summary(BD_01)
BD_01$chas = factor(BD_01$chas)
summary(BD_01)

#Luego de revisar la informacion analizamos la variable "age":

#Calcular n, min, Q1,Me,X,trim(x),Q3,max,RIC,MAD,Sd,As,K y CV

x <- BD_01$age

Valores=c(length(x),min(x),quantile(x,probs=0.25),median(x),mean(x),
          mean(x,trim=0.025),quantile(x,probs=0.75),max(x),
          IQR(x),mad(x),sd(x),skew(x),kurtosi(x),CV=(sd(x)/mean(x))*100)
Nombres=c("n","Minimo","Q1","Mediana","Media","Media Corta al 5%",
          "Q3","Maximo","IQR","MAD","Sd","As","K","CV")
Age = data.frame(Valores,Nombres)
write.csv(Age,file = "Age.csv")

#Realizando el Boxplot:

x=BD_01$age

boxplot(BD_01$age,main="Boxplot de Edad de Carros",
        ylab="Age (years)")

plot(x)

#Realizar el histograma con curvas de densidad
#real y teorica mas la prueba de normalidad.

range(x)
hist(x,freq=FALSE, ylim =c(0,0.04),
     border = "gray50",
     xlab="Edad de Carros (years)",
     ylab="Densidad",
     main=paste("Histograma de Edad y 
                Curvas de Densidad Real y Teorica"),
     )
lines(density(x),lwd=2,col="red")

#density es "Kernel Density Plot" se usa y 
#es muy efectivo para ver la distribucion de la
#variable.
curve(dnorm(x,mean(x),sd(x)),
      lwd=2,col="black",add=TRUE)
legend("topleft",c("Curva observada","Curva normal teorica"),
       lty=1,lwd=2,col=c("red","black"),bty = "o",
       cex=0.7)

library(car)
qqPlot(x, main="Q-Q plot Edad Confianza 95%",
       distribution = "norm",
       xlab="Cuantiles Normales",
       ylab="Cuantiles de Data",
       col.lines = "red",
       col="black",
       pch = 19)
x[c(42,75)]
View(BD_01[c(42,75), ])

legend("topleft", inset =0,
       c("Linea de Ajuste Normal", "Limite de Confianza")
       ,y.intersp = 0.5,
       lty=c(1,2),lwd=2,col=c("red","red"), 
       bty="n",cex=0.8)

legend("left",inset =0.0001,
       c("Prueba de Normalidad Shapiro-Wilk:",
         " W = 0.89201, p-value < 2.2e-16"),
       y.intersp = 0.5,col=c("black"), bty="n" ,cex=0.8)

#Shapiro Wilk test:

shapiro.test(x) #Si el p-valor es mayor que 0.05 podemos 
#decir que nuestros datos siguen una distribucion normal.


