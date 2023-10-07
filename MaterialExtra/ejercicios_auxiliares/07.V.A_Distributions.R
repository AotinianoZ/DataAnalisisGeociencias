# Probabilidad_Variable_Aleatoria_y_Distribuciones #

#### Generacion de numeros aleatorios y toma de muestra aleatoria ####

# Generdor aleatorio por: método de congruencia multiplicativo

# xn = 171xn-1 (mod 30269)
# un = xn/30269
# semilla 27218

random.number <- numeric(50)
random.seed <- 27218
for (j in 1:50){
  # construimos el vector de random.number elemento a elemento
  random.seed <- (171*random.seed) %% 30269
  random.number[j] <- random.seed/30269
}

random.number
plot(random.number)

# Otra forma

x <- numeric(50)
semilla <- 27218
x[1] = (171*semilla) %% 30269
for (i in 2:50){x[i] = (171*x[i-1]) %% 30269}
NumerosAleatorios <- x/30269
NumerosAleatorios
plot(NumerosAleatorios)

# Generdor aleatorio por: método de congruencia multiplicativo 2

# xn = 69069xn-1 (mod 2^37)
# un = xn/(2^37)

random.number <- numeric(50)
random.seed <- 1
for (j in 1:50){
  random.seed <- (69069*random.seed) %% (2^(37))
  random.number[j] <- random.seed/(2^(37))
}
random.number
plot(random.number)

# Toma de muestra aleatoria 
?sample
sample(c(3,5,7), size = 2, replace = FALSE)
sample(c(3,5,7), size = 2, replace = TRUE)

# Ejemplo1: Generar 50 números pseudoaleatorios del 1 al 100, sin y con reemplazo.
sample(1:100, size= 50, replace = FALSE)
sample(1:100, size= 50, replace = TRUE)

# Ejemplo2: Simular el lanzamiento de un dado
sample(1:6, 1)

# Ejemplo3: Simular el lanzamiento de cuatro dados o de un mismo dado cuatro veces
sample(1:6, 4, replace = TRUE)

# Ejemplo4: Simular la distribución suma de los números que salen al lanzar cuatro dados

set.seed(111)
t <- sapply(1:10000, function(x){sum(sample(1:6, 4, replace = TRUE))})
table(t)
plot(table(t))
barplot(table(t))

# Ejemplo5: Supongamos una urna con 3 bolas blancas y 7 negras, simular
# la extracción de una bola (asignar codigo binario, 1 a blanca y 0 a negra)

sample(c(1,0), 1, prob = c(0.3, 0.7))

# Si queremos simular 8 extracciones con reemplazo:
sample(c(1,0), size = 8, replace = TRUE, prob = c(0.3, 0.7))


#### Distribuciones de variables aleatorias discretas y continuas: ####

# Package stats (principal paquete a usar)

# spike plot:
# Muestra las probabilidades para cada valor en el rango de X como
# un "spike", enfatizando lo discreto de la distrubción.

k=0:4
p=c(1,2,3,2,1)/9
plot(k, p, type="h",xlab="k",ylab="probability",
     ylim=c(0,max(p)))
points(k,p,pch=16,cex=2)

#sample() generar valores aleatorios:
k=0:2
p=c(1,2,1)/4
sample(k,size=1,replace=FALSE,prob=p) #simula que todo tiene 
#la misma probabilidad de ocurrencia.
sample(k,size=1,prob=p)

#lanzar una moneda 10 veces, cara=1 y sello=0
sample(0:1, size=10, replace=TRUE) #i.i.d sample
sample (1:6,size=10, replace=TRUE) #lanzar un 
#dado 10 veces.
#suma de lanzar el dado 10 veces
sample(1:6,size=10,replace=TRUE)+ 
sample(1:6,size=10,replace=TRUE)
#Asumir que 10000 personas se les pregunta
#si les gusta comer pollo o no y 6200 dicen si.
#Luego si elegimos una muestra de 10:
sample(rep(0:1,c(3800,6200)),size=10,replace=TRUE)
sample(0:1, size=10, replace=T, prob=c(1-.62,.62))

#Familia de distribuciones:
# d: retorna la p.d.f de la distribucion.
# p.d.f: probability density function

# p: retorna la c.d.f de la distribucion.
# c.d.f:cummulative density function

# q: retorna los cuantiles.
# r: retorna una muestra aleatoria desde la distribucion = Area bajo la curva

#Cada familia de distribución tiene sus parámetros pero
# las funciones son similares en funcionamiento.

dunif(x=1, min=0, max=3)
punif(q=2,min=0,max=3)
qunif(p=1/2,min=0,max=3)
runif(n=1,min=0,max=3)
runif(n=10,min=0,max=10)

#Multiples cuantiles:
ps<-seq(0,2,by=0.2)
names(ps)=as.character(seq(0,200,by=20))
ps

#### Distribución Binomial: ####

#Lanzar una moneda 10 veces. X:numero de caras.
#Si la moneda es justa. #X tienen una distribución binomial
# Binomial(10, 1/2)
# La probabilidad que X=5
choose(10,5)*(1/2)^5*(1/2)^(10-5)
# usando d function
dbinom(5, size=10, prob=1/2)
#La probabilidad de obtener 6 o menos caras
sum(dbinom(0:6,size=10,prob=1/2))

#Spike plot producido por binomial:
heights=dbinom(0:10, size=10, prob=1/2)
plot(0:10,heights,
     type="h",
     main="Spike plot de X",xlab="k",ylab="p.d.f")
points(0:10,heights,pch=16,cex=0.5)

#De una poblacion las personas que eligen geologia se les 
#considera como si y se codifica con 1 al resto con 0.
#El analisis corresponde a una Binomial(n,p) de una v.a.d
#donde n es el número de muestras.

#Si la población favorable que eligió si es 62%,calcule de una
#Muestra de 100 que tan probable es que respondan 60% o menos.
pbinom(60, size=100, prob=0.62)

# Ejemplo de Diapositivas:
# El 10% de fragmentos de roca obtenidos por un taladro son defectuosos (polvo).
# Si elige una muestra aleatoria con reemplazo de 6 articulos y se define la variabl X como
# el numero de articulos defectuosos elegidos.
# a) Determinar la probabilidad que al menos un framento sea defectuoso
 # Nos piden P[X>=1] = 1 - P[X<1] = 1 - P[X=0]
1 - dbinom(x = 0, size = 6, prob = 0.1)

# b) Hallar el coeficiente de variabilidad
media <- (6)*(0.1)  # ux = n*pi
phi <- sqrt((6)*(0.1)*(1-0.1))  # phi^2 = n*pi*(1-pi)
CV <- (phi/media)*100

# Ejemplo1: Calcular la probabilidad de obtener cuatro caras al lanzar seis veces una 
# moneda perfecta

dbinom(x = 4, size = 6, prob = 0.5)

# Ejemplo2: Calcular la probabilidad de obtener como mucho cuatro caras al lanzar seis veces una moneda perfecta.
# Nos poden P[X<=4] con -> B(6, 0.5)

pbinom(q=4, size = 6, prob = 0.5)

# Ejemplo3: Calcular el valor tal que P[X<=x] = 0.89, y generar 10 valores pseudoaleatorios de una B(6, 0.5)
qbinom(p = 0.89, size = 6, 0.5) # area bajo la curva (integral de a -> b por diferencial de x)
rbinom(n = 10, size = 6, prob = 0.5)

# Ejemplo3: Supongamos que el 10% de los tubos de core en logeo son defectuosos y supongamos que # 
# produce 15 tubos cada hora.
# Cada tubo es independiente de los otros. Se juzga que el proceso está fuera de control cuando se producen más de
# 4 tubos defectuosos en una hora concreta. Simular el número de tubos defectuosos producidos por la máquina en cada
# hora a lo largo de un periodo de 24 horas y determinar si el proceso está afuera de control en algún momento.

TD <- rbinom(n = 24, size = 15, prob = 0.1)
TD
which(TD > 4)
help(any)
any(TD>4)
sum(TD>4)

# Ejemplo4:Supongamos que en un proceso de voladura, la proporción de roca triturada es 0.15. 
# Simular el número de rocas trituradas
# por hora en un periodo de 24 horas si se supone que se explotan 25 unidades hora. Revisar si el número
# de triturados exccede en alguna ocasión a 5. Repetir el procedimiento con proporciones 0.20 y 0.25.

D <- rbinom(n = 24, size = 25, prob = 0.15)
D
id <- which(D>5)
D[id]
any(D>5)
sum(D>5)

rbinom(n = 24, size = 25, prob = 0.2)
rbinom(n = 24, size = 25, prob = 0.25)

# Ejemplo 5: Usar simulación para estimar la media y la varianza de una variable aleatoria B(18, 0.76) y
# comparar dichos valores con los teóricos.

Bi <- rbinom(n = 100, size = 18, prob = 0.76)
mean(Bi)
var(Bi)

mean_t <- 18*0.76
var_t <- 18*0.76*(1-0.76)

# Adicional: Metodo de inversion de la funcion distribucion Binomial

ranbin <- function(n, size, prob){
  cumbinom <- pbinom(0:(size-1), size, prob)
  singlenumber <- function(){
    x <- runif(1)
    N <- sum(x <- cumbinom)
  }
  replicate(n, singlenumber())
}

# Ejemplo: Usar ranbin() para similar vectores de longitud 1000, 10000 y 1000000 de una distribucion
# B(10, 0.5). Usar system.time() para compara tiempo de ejecucion entre simulaciones.

system.time(expr = ranbin(n = 100, size = 10, prob = 0.5))
system.time(expr = ranbin(n = 10000, size = 10, prob = 0.5))
system.time(expr = ranbin(n = 1000000, size = 10, prob = 0.5))

# ¿Es importante el tiempo de procesamiento en la simulación?

#### Distribucion Poisson: ####

# Ejemplo 1: Las perforaciones de un taladro se hacen a arazón de 4 metros por cada 5 minutos.
# Si se elige al azar en un intervalo de 2 minutos, hallar la probabilidad que se taladro al menos 2m de perforacion.

 # P[X>=2] = 1 - P[X<2]

1 - (dpois(x = 0, lambda = 1.6) + dpois(x = 1, lambda = 1.6))

# Forma 2:
ppois(q = 1, lambda = 1.6, lower.tail = FALSE)


# Ejemplo2: Suponga que el 5% de las rocas de la Fm. Ananea contienen oro. Si se elige al azar y con reemplazo
# 90 rocas, hallar la probabilidad de que en al menos 3 rocas encuentren oro.


ppois(q = 2, lambda = 4.5, lower.tail = FALSE)

#### Distribucion Geometrica: ####

# Ejemplo1: Suponga que el mejor geologo del mundo tiene una probabilidad de 0.04 de no encontrar un yacimiento (fallar)
# ,y que cuando ello ocurre es necesario reemplazarlo por uno nuevo. Determinar la media y el coeficiente de variabilidad
# del número de veces que puede ser usado el geologo para descubrir yacimientos, ademas del coeficiente de variacion.

mean_g <- 1/0.04
var_g <- (1-0.04)/(0.04^2)
cv <- (sqrt(var_g)/mean_g)*100

# Calcule la probabilidad de que en 10 yacimientos el geologo falle.

dgeom(x = 10, prob = 0.04)


#### Distribucion Hipergeometrica: ####

# Ejemplo 1: Suponga que en un proceso de control de calidad de base de datos geológico se inspecciona 10 tablas
# de base de datos, de los cuales 4 son defectuosos. Si se eligen 5 base de datos al azar y sin reemplazo
# hallar la probabilidad de elegir no mas de 2 bases de datos defectuosos.


hiper <- phyper(q = 1, m = 4, n = 6, k = 5, lower.tail = TRUE) # P[X<=1]

# generando simulacion para aproximaciones.
hiper_random <- rhyper(nn = 100, m = 4, n = 6, k = 5)
mean(hiper_random)
var(hiper_random)

#### Distribucion Cauchy ####

# Ver:
# https://www.vrcbuzz.com/cauchy-distribution-probabilities-using-r/



#### Distribucion Uniforme (VAC): ####














#### Distribucion Exponencial: Exponential(lambda) ####

res=rexp(50,rate=1/5)
#boxplot:
par(fig=c(0,1,0,0.50))
boxplot(res, horizontal=TRUE,bty="n",
        xlab="muestra exponencial")
#histograma
par(fig=c(0,1,0.25,1),new=TRUE)
#guardar los valores,
#luego encontrar el mas grande y setearlo
#como maximo:
tmp.hist=hist(res,plot=FALSE)
tmp.edens=density(res)
tmp.dens =dexp(0, rate=1/5)
y.max=max(tmp.hist$density,tmp.edens$y,
          tmp.dens)
hist(res,ylim=c(0,y.max),prob=TRUE,
     main="", col=gray(0.9))
curve(dexp(x,rate = 1/5),lwd=2,add=TRUE)
rug(res)

# Suponga que el tiempo de duración de un articulo (horas) es una variable con valor beta  = 0.5
# y sigue una distribucion exponencial. Calcular la probabilidad de que un articulo elegido al azar tenga
# un tiempo de vida mayor a 3h.

 # P[X>3]

pexp(q = 3, rate = 0.5, lower.tail = FALSE)


#### Distribucion Normal: ####

#Normal(µ,σ)
pnorm(q=1.5,mean=0, sd=1)
pnorm(q=4.75, mean=0,sd=1/2)
qnorm(c(0.25,0.50,0.75)) #Areas importantes
#basadas en Z-scores
#qnorm especificamos el area que deseamos
#cuanta area es no mayor a una desviacion estandar
#desde la media:
pnorm(1)-pnorm(-1)
#dos desviaciones estandar:
1-2*pnorm(-2)
#tres desviaciones estandar:
diff(pnorm(c(-3,3)))

require(graphics)

dnorm(0) == 1/sqrt(2*pi)
dnorm(1) == exp(-1/2)/sqrt(2*pi)
dnorm(1) == 1/sqrt(2*pi*exp(1))

## Using "log = TRUE" for an extended range :
par(mfrow = c(2,1))
plot(function(x) dnorm(x, log = TRUE), -60, 50,
     main = "log { Normal density }")
curve(log(dnorm(x)), add = TRUE, col = "red", lwd = 2)
mtext("dnorm(x, log=TRUE)", adj = 0)
mtext("log(dnorm(x))", col = "red", adj = 1)


#Ejemplo1:
# a
pnorm(70, mean=80, sd=10) - pnorm(60, mean=80, sd=10)
# b 
# media poblacional = media muestral/sqrt(n)
media_nueva <- 10/sqrt(60)
1 -  pnorm(90, mean = 80, sd = media_nueva)

# Ejemplo:
#a
pnorm(30, mean = 42000, sd=12000)
#b
# u_tot <- ux+uy+uz
# phi_tot <- rootsquare(phi_x^2, phi_y^2 + phi_z^2)
u = 42000+60000+78000
phi = sqrt(12000^2+18000^2+10000^2)
#
1- pnorm(120000, mean= u, sd = phi)
#c
# tarea la c.
# practicar de : https://r02pro.github.io/normal-distribution.html



# Dist. normal estandar
# Suponga que los gastos semanales de movilidad realizados por los mineros
# de Antamina tienen una distribucion normal con una media de 50 nuevos soles
# y una desviación de 12 nuevos soles. Si se elige al azar un habitante de dicho distrito minero,
# halle la probabilidad que su gasto semanal en movilidada sea mayor de 58 nuevos soles.

 # X = vac ~ N(50, 12^2)
 # P[X>58] = ??

# Usamos transformacion estandar:
# Z = (x-u)/phi ≈ N(0, 1) -> uz = 0 ; phi_z^2=1

# P[X>58] = P[Z>0.67] = 1 - P[Z<=0.67]  

1 - pnorm(q = 0.67)

#### LogNormal: log(x) ####

res= rlnorm(n=50, meanlog=0,sdlog=1)
#boxplot:
par(fig=c(0,1,0,0.5))
boxplot(res, horizontal=TRUE, bty="n",xlab="lognormal")
#histogram:
par(fig=c(0,1,0.25,1),new=TRUE)
tmp.hist=hist(res, plot=FALSE)
tmp.edens=density(res)
tmp.dens = dexp(0,rate=1/5)
ymax =max(tmp.hist$density,tmp.edens$y,
          tmp.dens)
#hist:
par(fig=c(0,1,0.25,1),new=TRUE)
hist(res,ylim=c(0,ymax),prob=TRUE,
     main="",col=gray(0.9))
curve(dlnorm(x,meanlog=0,sdlog=1),
      add=TRUE)
rug(res)

#### Distribucion Weibull ####

# Set shape and scale parameters for the Weibull distribution
shape_param <- 2
scale_param <- 5
x_min <- 0
x_max <- 20
# Plot the Weibull distribution using the curve() function
curve(
  dweibull(x, shape = shape_param, scale = scale_param),
  from = x_min,
  to = x_max,
  main = "Weibull Distribution",
  xlab = "x",
  ylab = "Density",
  col = "blue",
  lwd = 2
)

# Create a data frame with a sequence of x values
data <- data.frame(x = seq(x_min, x_max, length.out = 1000))

# Add a column for the probability density 
# function of the Weibull distribution
data$weibull_density <-
  dweibull(data$x, shape = shape_param, scale = scale_param)

library(tidyverse)
# Generate and plot the Weibull distribution using ggplot2
ggplot(data, aes(x = x, y = weibull_density)) +
  geom_line(color = "blue", size = 1) +
  ggtitle("Weibull Distribution") +
  xlab("x") +
  ylab("Density") +
  theme_minimal()

library(ggplot2)
# Set seed for reproducibility
set.seed(42)
# Generate random numbers from Weibull distribution
shape <- 2
scale <- 1
n <- 1000
weibull_sample <- rweibull(n, shape, scale)
# Create a data frame with the Weibull sample
weibull_data <- data.frame(values = weibull_sample)
# Plot the Weibull distribution using ggplot2
ggplot(weibull_data, aes(x = values)) +
  geom_histogram(aes(y = ..density..),
                 bins = 30,
                 color = "black",
                 fill = "lightblue") +
  labs(title = "Weibull Distribution",
       x = "Values",
       y = "Density") +
  theme_minimal()

# Ver!!

# https://www.alphacodingskills.com/r/r-weibull-distribution.php



#### Chi-cuadrado ####

# Ejemplo1: Teoria

# gl = 12 y P[a<=X<=b] = 0.90 , P[X<=a] = 0.05

qchisq(p=0.05, df=12)
qchisq(p=0.95, df=12)


#### T-student ####

# Ejemplo1: Teoria

pt(q = 2.228, df = 10, lower.tail = TRUE) - pt(q = -1.812, df = 10, lower.tail = TRUE) 

### Fisher ####

set.seed(53535)   # Set seed for reproducibility
N <- 10000  
y_rf <- rf(N, df1 = 3, df2 = 5)          # Draw N F-distributed values
y_rf 

hist(y_rf,                            # Plot of randomly drawn f density
     breaks = 500,
     main = "Random Numbers Generated According to F Distribution",
     xlim = c(0, 15))

qf(.95, df1=5, df2=2) 

#### Trabajo: ####

# Hacer una lista de todas las distribuciones usadas y sus aplicacioes en las geociencias:
# ejemplo: 
# lognormal: General distribucion de elementos metalicos en aguas ... y seguir listando.











