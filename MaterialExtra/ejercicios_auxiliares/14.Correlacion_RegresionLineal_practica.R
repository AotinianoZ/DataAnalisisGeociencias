# Ejercicio 1:

# Se tienen los datos de aguas en un acuífero, los siguientes pares
# representan la distancia radial desde el centro del acuifero
# y la concentración en ppm de Cu disuelto. 
# Genere una regresion lineal para modelar la concentracion
# de cobre segun la distancia radial.
# (1,2) (2,1) (1,3) (3,3) (4,3) (4,2)

# Responder:
# a.1) Halle el coeficiente de correlacion pearson
# a.2) Decida mediante hipotesis estadistica si existe correlacion lineal.
# (debido a que se tienen pocos datos es posible haber obtenido por azar ese valor)
# a.3) Haga el diagrama de dispersion con la linea de regresion por
# minimos cuadrados.
# a) Dar la ecuacion de regresion lineal.
# b) ¿Cual varianza del modelo y cuanto explica el ruido?
# c) ¿Cual es el residual maximo, que puede generar este?
# d) ¿Es el ruido grande?
# e) ¿Que sifnifica en Coefficientes: Std.Error - t value - Pr(>abs(t))?
# f) ¿Porque el R-square y Adjusted R-square son diferentes?
# g) ¿Qué representa el F-statistic su p-valor?



# Ejercicio 2:

# Se tienen los datos de aguas en la microcuenca Hornillos de Arequipa,
# los siguientes pares
# representan la concentración en ppm de Cu disuelto y Pb disuelto.
# Genere una regresion lineal para modelar la concentracion
# de Cu segun la concentración de Pb.
# (1,2) (1,3) (2,5) (3,5) (4,9)

# Responder:
# a.1) Halle el coeficiente de correlacion pearson
# a.2) Decida mediante hipotesis estadistica si existe correlacion lineal.
# (debido a que se tienen pocos datos es posible haber obtenido por azar ese valor)
# a.3) Haga el diagrama de dispersion con la linea de regresion por
# minimos cuadrados.
# a) Dar la ecuacion de regresion lineal.
# b) ¿Cual varianza del modelo y cuanto explica el ruido?
# c) ¿Cual es el residual maximo, que puede generar este?
# d) ¿Es el ruido grande?
# e) ¿Que sifnifica en Coefficientes: Std.Error - t value - Pr(>abs(t))?
# f) ¿Porque el R-square y Adjusted R-square son diferentes?
# g) ¿Qué representa el F-statistic su p-valor?



# Ejercicio 3:

# Se tienen los datos de aguas en la microcuenca Alto Camana de Arequipa,
# los siguientes pares
# representan la concentración en ppm de As disuelto y pH.
# Genere una regresion lineal para modelar la concentracion
# de Cu segun el pH.
# (1,2) (2,5) (3,8) (4,9) (5,8) (6,5) (7,2)

# Responder:
# a.1) Halle el coeficiente de correlacion pearson
# a.2) Decida mediante hipotesis estadistica si existe correlacion lineal.
# (debido a que se tienen pocos datos es posible haber obtenido por azar ese valor)
# a.3) Haga el diagrama de dispersion con la linea de regresion por
# minimos cuadrados.
# a) Dar la ecuacion de regresion lineal.
# b) ¿Cual varianza del modelo y cuanto explica el ruido?
# c) ¿Cual es el residual maximo, que puede generar este?
# d) ¿Es el ruido grande?
# e) ¿Que sifnifica en Coefficientes: Std.Error - t value - Pr(>abs(t))?
# f) ¿Porque el R-square y Adjusted R-square son diferentes?
# g) ¿Qué representa el F-statistic su p-valor?
# h) Se ajustaria el modelo mejor a una parabola (y = ax^2+bx+c).
# Ayuda: modeloparabolico <- lm(y~poly(x, 2, raw=TRUE))


# Ejercicio 4:

# Se tienen los datos de aguas en la Rio Tumbes,
# los siguientes pares
# representan la concentración en ppm de Mn disuelto y Mg disuelto.
# Genere una regresion lineal para modelar la concentracion
# de Mn segun la concentración de Mg
# (1,2) (1,3) (2,5) (3,5) (4,9)

# Responder:
# a.1) Halle el coeficiente de correlacion pearson
# a.2) Decida mediante hipotesis estadistica si existe correlacion lineal.
# (debido a que se tienen pocos datos es posible haber obtenido por azar ese valor)
# a.3) Haga el diagrama de dispersion con la linea de regresion por
# minimos cuadrados.
# b.1) Dar la ecuacion de regresion lineal.
# b.2) ¿Cual varianza del modelo y cuanto explica el ruido?
# c) ¿Cual es el residual maximo, que puede generar este?
# d) ¿Es el ruido grande?
# e) ¿Que sifnifica en Coefficientes: Std.Error - t value - Pr(>abs(t))?
# f) ¿Porque el R-square y Adjusted R-square son diferentes?
# g) ¿Qué representa el F-statistic su p-valor?


# Ejercicio 5:

# Se tienen los datos de aguas en la laguna Marcapomacocha,
# los siguientes pares
# representan la concentración en ppm de Cu disuelto y Zn disuelto.
# Genere una regresion lineal para modelar la concentracion
# de Cu segun la concentración de Zn
# (1,2) (2,5) (3,8) (4,9) (5,8) (6,5) (7,2)

# Responder:
# a.1) Halle el coeficiente de correlacion pearson
# a.2) Decida mediante hipotesis estadistica si existe correlacion lineal.
# (debido a que se tienen pocos datos es posible haber obtenido por azar ese valor)
# a.3) Haga el diagrama de dispersion con la linea de regresion por
# minimos cuadrados.
# b.1) Dar la ecuacion de regresion lineal.
# b.2) ¿Cual varianza del modelo y cuanto explica el ruido?
# c) ¿Cual es el residual maximo, que puede generar este?
# d) ¿Es el ruido grande?
# e) ¿Que sifnifica en Coefficientes: Std.Error - t value - Pr(>abs(t))?
# f) ¿Porque el R-square y Adjusted R-square son diferentes?
# g) ¿Qué representa el F-statistic su p-valor?


# Ejercicio 6:

# Se tienen los datos de muestras de suelo en Honoria-Tournavista,
# los siguientes pares
# representan la el peso en seco de muestras de suelo en Kg y 
# la concentracion de materia organica en mg/Kg.
# Genere una regresion lineal para modelar el peso
# en seco y la concentracion de materia organica.
# (1,9) (2,7) (3,8) (3,5) (6,6) (5,4) (7,5)

# Responder:
# a.1) Halle el coeficiente de correlacion pearson
# a.2) Decida mediante hipotesis estadistica si existe correlacion lineal.
# (debido a que se tienen pocos datos es posible haber obtenido por azar ese valor)
# a.3) Haga el diagrama de dispersion con la linea de regresion por
# minimos cuadrados.
# b.1) Dar la ecuacion de regresion lineal.
# b.2) ¿Cual varianza del modelo y cuanto explica el ruido?
# c) ¿Cual es el residual maximo, que puede generar este?
# d) ¿Es el ruido grande?
# e) ¿Que sifnifica en Coefficientes: Std.Error - t value - Pr(>abs(t))?
# f) ¿Porque el R-square y Adjusted R-square son diferentes?
# g) ¿Qué representa el F-statistic su p-valor?



















































