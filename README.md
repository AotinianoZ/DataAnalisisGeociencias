Geoscience Multiverse
================

### geology::conf 2025 workshop

by A.Otiniano

Este repositorio es la base para el desarrollo del curso Geodatabase, Geo-Statistics, Machine Learning and Big Data: Applied to Geology (SQL – R – Qgis - Python))

-----

:spiral_calendar: Setiembre - Octubre, 2025 
:camera: Comienza Sábado, 06 de Setiembre :jack_o_lantern:
:green_book: Sabados - Domingos
:alarm_clock:    09:00 - 12:00  

-----

## Requerimientos Básicos:

* No se requiere preparación :clap:
* Usaremos *github* y *html* para desarrollar el curso.
* Más detalles acerca del curso. Sí tienen alguna duda contacten a <alonso.otiniano.z@uni.pe> o <alonsootinianozavala@gmail.com>

## Instructores

**Instructor**

* Alonso Otiniano [Linkedin](https://www.linkedin.com/in/aotinianoz/),  [Web page](https://webaoz2.netlify.app), [Twiter](https://twitter.com/OtinianoAlonso).

**Co-instructor**

* En proceso...

## Panorama

El curso desarrolla desde los conceptos básicos de geodatabase (base de datos geoespacial) para entender la estructura y funcionamiento de base de datos espacial. Además se realiza el análisis estadístico exploratorio de **aguas, suelo, rocas y sedimentos de drenaje**, así como modelos estadísticos, geoestadísticos y de machine learning para su aplicación en **recursos hídricos, geoquímica, hidroquímica, peligros geológicos, rock blasting** todo esto enmarcado en la *interactividad*, *inteligencia artificial* y *desarrollo web*.

## ¿Qué esperamos que aprendan?

* Manejo y Estructura básica de `base de datos`.

* Entendimiento de `Big Data`.

* Conocer el ecosistema `R` y `Rstudio`.

* Realizar `Análisis Geoespacial` en múltiples materias geológicas.

* Entender el funcionamiento del `Machine Learning en Geología`.

* Conocer los `desarrollos` actuales y prespectivas a futuro.

## Instalaciones previas básicas son requeridas:

Durante el curso corto necesitaremos herramientas como:

* SQL, R, Rstudio, QGis - Python.

La instalación de paquetes para Rstudio en máquina local:

Usaremos constantemente: `install.packages(c("tidyverse", "sf", "raster", "mapview", "dplyr"))`


## Estructura del Curso Corto:

El curso constará de partes teórica-práctica, la mayoría estará basado en la práctica con material bibliográfico detallado del proceso.

```r
Teoría: Desarrollada Semidetallada.
Práctica: Desarrollada Exhaustivamente.
```
Toda la información estará como PDFs en los folderes adjuntos, además de formatos *.html* para que lo entregado no pierda interactividad y sea cual se ha desarrollado cada clase. Se puede descargar todo el material desde este github.

<img src="https://aozweb.com/MaterialCurso/image_teaching.png" width="400px" />

## Logueo y registro por primera vez

[Sesion Desarrollada](https://github.com/AotinianoZ/Geoscience_Multiverse) !!Esto solo funcionará la tarde-noche del curso corto!!

## Cronograma

Agenda

0. Introducción: Instalación de softwares (1 semana antes) - 2h

`Modulo I` (24h):

`Tema I`:

1. **Alonso Otiniano** Tema I-IA: Base de Datos Geoespacial y evolución.

    * Base de Datos
    * Lenguaje de Consulta Estructurado (SQL)
    * Evolución de Base de Datos
    * Base de Datos y Hojas de Cálculo?
    * Big Data
    * Open Geoespacial Consortium
    * Base de Datos Espaciales
    * Librerías sf-terra
    * Data Geoespacial Core PostGis.
    
2. **Alonso Otiniano** Tema I-IIA: Conexión - importar y exportar base de datos geoespacial.

    * Conexión a base de datos sql y carga de data al Rstudio.
    * Revisión de conexión y test de pruebas.
    * Importar y exportar tablas desde el Rstudio.

`Tema II`:

3. **Alonso Otiniano** Tema II-A: R y Rstudio.

    * Rstudio.
    * Paquetes y Librerías.
    * Mi primer Script:
      *  R como calculadora científica (funciones de calculadora).
      *  Tipos de variable y Operadores en R.
    * Definición de Vectores en R:
      * Generación de vectores usando números aleatorios.
      * Funciones Básicas.
      * Datos  Especiales (NA´s,Inf ,NaN´s  & “caracteres especiales”).
    * Matrices
    * Otras funciones básicas (sort(), seq(),rep()).
    * Data frame.
    * Estructuras de decisión y repetición:
      * if, else, else if.
      * for (loop).
      * while, next, break.
    * Factores en R.
    * Listas en R.
    * Paquetería de Funciones
      * Definición de Funciones.
      * Aplicación de Funciones.
    * Instalación de paquetes CRAN, GitHub, Bioconductor u otros.
    
4. **Alonso Otiniano** Tema II-B: Explorando datos.

    * Conectarse desde internet o base de datos -Importar y Exportar Data en R.
    * Tidyverse y ggplot en el IDE Rstudio.
    * Limpieza de Data – Filtros,  Transformación.
    * Introducción a la estadística descriptiva en R.
    * Análisis Estadísticos de Datos:
      * Distribución de variables aleatorias- Funciones de probabilidad – Distribución Normal
      * Inferencia, Estimación y Pruebas Estadísticas
      * Pruebas Paramétricas – Semiparamétricas y No paramétricas (ROS) (Teórico – Práctico):
      * Teoría de Correlación y Regresión (Teórico – Práctico)
      * Correlación Pearson, Spearman, Kendall.
      * Regresión Lineal, Logística, Polinomial y Poisson
      * Introducción series de tiempo

`Modulo II` (24h):

5. **Alonso Otiniano** Módulo II-A: AED recurso hídricos

    * Análisis de Base de Datos (estructura y revisión general).
    * Balance Iónico.
    * Imputación de Datos debajo del límite de detección (<L.D.) - General
    * Análisis Exploratorio mediante estadísticos descriptivos.
    * Diagramas generales (parámetros FICO – concentración de elementos).
    * Diagramas de Piper, Gibss y Stiff.
    * Estándares de Calidad Ambiental.

6. **Alonso Otiniano** Módulo II-A (Opcional): Modelado de Datos

    * Carga de base de datos ECA.
    * Revisión General de la Información.
    * Creación de nuevas variables para análisis ROS.
    * Análisis Exploratorio de unidades hidrográficas elegidas.
    * Revisión espacio temporales y sumarios claves para la información.
    * Análisis univariante para elementos completos, elementos debajo del límite de detección.
    * Detección de anómalos (outliers y faroutliers).
    * Generación de cuantiles para clasificación en categorías y filtrado.

7. **Alonso Otiniano** Módulo II-B: AED suelos

    * Análisis de Base de Datos (estructura y revisión general).
    * Análisis Exploratorio mediante estadísticos descriptivos.
    * Análisis Gráfico Univariante y Bivariante.
    * Estándares de Calidad Ambiental para Suelo (ECAs).
    * Análisis Bivariantes Numéricos (Boxplot Multivariante por Clasificación, Corrplot)
    * Análisis Multivariable (Biplot – PCA).
    * Mapa Geoestadístico de distribución Catiónica.
      * Crear Grilla con tamaño de celda. 
      * Modelado usando Inverso a la distancia (idw).
      * Variograma.
      * Modelado usando Krigging.

`Modulo III` (12h):

8. **Alonso Otiniano** Módulo III-A (Opcional): 

Ver en el siguiente [link](https://aozweb.com/Poster/Temas.pdf).

9. **Alonso Otiniano** Módulo III-B: 

    * Generación de Mapas Dinámicos 
      * Uso Transformación de coordenadas
      * Análisis de shapefiles y ráster.
      * Filtros y análisis de shapefiles y tablas de datos con visualización en mapa.
      * Mapas Interactivos Dinámicos.

    * Creación de dashboard para análisis geoambiental – zona Cusco – zona Tumbes
      * Carga de paquetes y configuración
      * Carga de data (formato .csv, .xlsx, .xls, .dat, .txt, .shp, .gdb, .tiff)
      * Transformación a data espacial y revisión de estructura.
      * Generación de sumario general.
      * Generación de sumario de parámetros físico-químicos.
      * Generación de mapa de burbujas para facies hidroquímicas.
      * Scatter, Piper y otros gráficos con filtros de datos espaciales y generación de tablas automáticas exportables en diferentes formatos.
      * Generación del Dashboard final.

10. **Alonso Otiniano** Módulo IV-C: Desarrollos


Descansos Oficiales

| Time          | Activity         |
| :------------ | :--------------- |
| 10:30 - 10:40 | *Coffee break*   |
| 11:30 - 11:35 | *Coffee break*    |


## Instructores

Reseña será dada al momento del desarrollo del curso. 


# Para hacer reloggin:

Esto estará a cargo del **Alonso Otiniano Zavala**.

-----

![](https://i.creativecommons.org/l/by/4.0/88x31.png) This work is
licensed under a [Creative Commons Attribution 4.0 International
License](https://creativecommons.org/licenses/by/4.0/).
